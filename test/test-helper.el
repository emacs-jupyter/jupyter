;;; test-helper.el --- Helpers for jupyter-test.el -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 15 Nov 2018
;; Version: 0.6.0

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

;;; Code:

(require 'zmq)
(require 'jupyter-client)
(require 'jupyter-repl)
(require 'jupyter-org-client)
(require 'jupyter-kernel-manager)
(require 'cl-lib)
(require 'ert)

(defvar jupyter-test-with-new-client nil
  "Whether the global client for a kernel should be used for tests.
Let bind to a non-nil value around a call to
`jupyter-with-kernel-client' or `jupyter-with-kernel-repl' to
start a new kernel REPL instead of re-using one.")

;;; `jupyter-echo-client'

(defclass jupyter-echo-client (jupyter-kernel-client)
  ((messages))
  :documentation "A client that echo's any messages sent back to
the channel the message was sent on. No communication is actually
done with a kernel. Every sent message on a channel is just
directly sent back to the handler method. The message flow when
handling a message is always

- status: busy
- reply message
- status: idle")

(cl-defmethod initialize-instance ((client jupyter-echo-client) &rest _slots)
  (cl-call-next-method)
  (oset client messages (make-ring 10))
  (oset client channels
        (list :hb (jupyter-hb-channel)
              :shell (list :alive-p nil :endpoint "foo://bar")
              :stdin (list :alive-p nil :endpoint "foo://bar")
              :iopub (list :alive-p nil :endpoint "foo://bar"))))

(cl-defmethod jupyter-send ((client jupyter-echo-client)
                            channel
                            type
                            message
                            &optional _flags)
  (let ((req (jupyter-request :id (jupyter-new-uuid))))
    (if (string-match "request" (symbol-name type))
        (setq type (intern (replace-match "reply" nil nil (symbol-name type))))
      (error "Not a request message type (%s)" type))
    ;; Message flow
    ;; - status: busy
    ;; - reply message
    ;; - status: idle
    ;;
    ;; Needed internally by a `jupyter-kernel-client', this is mainly handled
    ;; by the eventloop.
    (puthash (jupyter-request-id req) req (oref client requests))
    ;; Simulate a delay
    (run-at-time
     0.001 nil
     (lambda ()
       (jupyter-handle-message
        client :iopub (jupyter-test-message req :status (list :execution_state "busy")))
       (jupyter-handle-message client channel (jupyter-test-message req type message))
       (jupyter-handle-message
        client :iopub (jupyter-test-message req :status (list :execution_state "idle")))))
    req))

(cl-defmethod jupyter-handle-message ((client jupyter-echo-client) _channel msg)
  (ring-insert+extend (oref client messages) msg 'grow)
  (cl-call-next-method))

;;; Macros

(cl-defmacro jupyter-ert-info ((message-form &key ((:prefix prefix-form) "Info: "))
                               &body body)
  "Identical to `ert-info', but clear the REPL buffer before running BODY.
In a REPL buffer, the contents are erased and an input prompt is
inserted.

If the `current-buffer' is not a REPL, this is identical to
`ert-info'."
  (declare (debug ((form &rest [sexp form]) body))
           (indent 1))
  `(ert-info (,message-form :prefix (quote ,prefix-form))
     ;; Clear the REPL buffer before each new test section, but do this only if
     ;; the current client is a REPL client
     (when (and jupyter-current-client
                (object-of-class-p jupyter-current-client
                                   'jupyter-repl-client)
                (eq (current-buffer)
                    (oref jupyter-current-client buffer)))
       (let ((inhibit-read-only t))
         (erase-buffer)
         (jupyter-test-repl-ret-sync)))
     ,@body))

(defmacro jupyter-with-echo-client (client &rest body)
  (declare (indent 1) (debug (symbolp &rest form)))
  `(let ((,client (jupyter-echo-client)))
     ,@body))

(defvar jupyter-test-global-clients nil)

(defvar jupyter-test-global-repls nil)

(defmacro jupyter-with-kernel-client (kernel client &rest body)
  "Start a new KERNEL client, bind it to CLIENT, evaluate BODY.
This only starts a single global client unless the variable
`jupyter-test-with-new-client' is non-nil."
  (declare (indent 2) (debug (stringp symbolp &rest form)))
  (let ((manager (make-symbol "--manager"))
        (real-kernel (make-symbol "--real-kernel"))
        (global (make-symbol "--global")))
    `(progn
       (jupyter-error-if-no-kernelspec ,kernel)
       (let* ((,real-kernel (caar (jupyter-find-kernelspecs ,kernel)))
              (,global (alist-get ,real-kernel jupyter-test-global-clients)))
         (cl-destructuring-bind (,manager ,client)
             (if (and ,global (not jupyter-test-with-new-client))
                 ,global
               (let ((manager-client
                      (jupyter-start-new-kernel ,kernel)))
                 (prog1 manager-client
                   (unless (or ,global jupyter-test-with-new-client)
                     (setf (alist-get ,real-kernel jupyter-test-global-clients)
                           manager-client)))))
           ,@body)))))

(defmacro jupyter-with-python-client (client &rest body)
  "Start a new Python kernel, bind it to CLIENT, evaluate BODY."
  (declare (indent 1) (debug (symbolp &rest form)))
  `(jupyter-with-kernel-client "python" ,client
     ,@body))

(defmacro jupyter-with-kernel-repl (kernel client &rest body)
  "Start a new KERNEL REPL, bind the client to CLIENT, evaluate BODY.
Delete the REPL buffer after running BODY."
  (declare (indent 2) (debug (stringp symbolp &rest form)))
  (let ((real-kernel (make-symbol "--real-kernel"))
        (global (make-symbol "--global"))
        (cleanup-after (make-symbol "--cleanup-after")))
    `(progn
       (jupyter-error-if-no-kernelspec ,kernel)
       (let* ((,real-kernel (caar (jupyter-find-kernelspecs ,kernel)))
              (,global (alist-get ,real-kernel jupyter-test-global-repls))
              (,cleanup-after jupyter-test-with-new-client)
              (,client (if (and ,global (not jupyter-test-with-new-client))
                           ,global
                         (let ((client
                                (jupyter-run-repl ,kernel)))
                           (prog1 client
                             (unless (or ,global jupyter-test-with-new-client)
                               (setf (alist-get ,real-kernel jupyter-test-global-repls)
                                     client)))))))
         (unwind-protect
             (jupyter-with-repl-buffer ,client
               (progn ,@body))
           (cl-letf (((symbol-function 'yes-or-no-p)
                      (lambda (_prompt) t))
                     ((symbol-function 'y-or-n-p)
                      (lambda (_prompt) t)))
             (when ,cleanup-after
               (kill-buffer (oref ,client buffer)))))))))

(defmacro jupyter-with-python-repl (client &rest body)
  "Start a new Python REPL and run BODY.
CLIENT is bound to the Python REPL. Delete the REPL buffer after
running BODY."
  (declare (indent 1) (debug (symbolp &rest form)))
  `(jupyter-with-kernel-repl "python" ,client
     ,@body))

;;; Functions

(defun jupyter-test-ipython-kernel-version (spec)
  "Return the IPython kernel version string corresponding to SPEC.
Assumes that SPEC is a kernelspec for a Python kernel and
extracts the IPython kernel's semver."
  (let* ((cmd (aref (plist-get spec :argv) 0))
         (process-environment
          (append
           (cl-loop for (key val) on (plist-get spec :env) by #'cddr
                    collect (concat (substring (symbol-name key) 1) "=" val))
           process-environment))
         (version (shell-command-to-string
                   (concat cmd " -c 'import ipykernel; \
print(\"{}.{}.{}\".format(*ipykernel.version_info[:3]))'"))))
    (string-trim version)))

(defun jupyter-error-if-no-kernelspec (kernel)
  (prog1 kernel
    (unless (car (jupyter-find-kernelspecs
                  (regexp-quote kernel)))
      (error "Kernel not found (%s)" kernel))))

(defun jupyter-test-message (req type content)
  "Return a bare bones message plist for REQ.
TYPE is the message type of the returned message. CONTENT is the
message contents."
  (list :msg_id (jupyter-new-uuid)
        :msg_type type
        :parent_header (list :msg_id (jupyter-request-id req))
        :content content))

(defun jupyter-test-wait-until-idle-repl (client)
  "Wait until the execution state of a REPL CLIENT is idle."
  (while (not (equal (jupyter-execution-state client) "idle"))
    (sleep-for 0.01)))

(defun jupyter-test-repl-ret-sync ()
  "A synchronous version of `jupyter-repl-ret'."
  (jupyter-repl-ret)
  ;; Account for the multiple idle -> busy cycles that occurs from
  ;; `jupyter-repl-ret'
  (sleep-for 0.2)
  (jupyter-test-wait-until-idle-repl
   jupyter-current-client))

(defun jupyter-test-conn-info-plist ()
  "Return a connection info plist suitable for testing."
  (let* ((ports (cl-loop
                 with sock = (zmq-socket (zmq-current-context) zmq-PUB)
                 for c in '(:shell :hb :iopub :stdin :control)
                 collect c and
                 collect (zmq-bind-to-random-port sock "tcp://127.0.0.1")
                 finally (zmq-close sock))))
    `(:shell_port
      ,(plist-get ports :shell)
      :key  "8671b7e4-5656e6c9d24edfce81916780"
      :hb_port
      ,(plist-get ports :hb)
      :kernel_name "python"
      :control_port
      ,(plist-get ports :control)
      :signature_scheme "hmac-sha256"
      :ip "127.0.0.1"
      :stdin_port
      ,(plist-get ports :stdin)
      :transport "tcp"
      :iopub_port
      ,(plist-get ports :iopub))))

;;; test-helper.el ends here
