;;; test-helper.el --- Helpers for jupyter-test.el -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 15 Nov 2018

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
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
(require 'jupyter-zmq-channel-ioloop)
(require 'jupyter-channel-ioloop-comm)
(require 'jupyter-org-client)
(require 'jupyter-kernel-process-manager)
(require 'org-element)
(require 'subr-x)
(require 'cl-lib)
(require 'ert)

(declare-function jupyter-servers "jupyter-server")

;; Increase timeouts when testing for consistency. I think what is going on is
;; that communication with subprocesses gets slowed down when many processes
;; are being open and closed? The kernel processes are cached so they are
;; re-used for the most part except for tests that explicitly start and stop a
;; process. Increasing these timeouts seemed to do the trick.
(when (or (getenv "APPVEYOR") (getenv "TRAVIS"))
  (setq jupyter-long-timeout 120
        jupyter-default-timeout 60))

(when (> emacs-major-version 26)
  (defalias 'ert--print-backtrace #'backtrace-to-string))

(defvar jupyter-test-with-new-client nil
  "Whether the global client for a kernel should be used for tests.
Let bind to a non-nil value around a call to
`jupyter-test-with-kernel-client' or `jupyter-test-with-kernel-repl' to
start a new kernel REPL instead of re-using one.")

(defvar jupyter-test-temporary-directory-name "jupyter")

(defvar jupyter-test-temporary-directory
  (make-temp-file jupyter-test-temporary-directory-name 'directory)
  "The directory where temporary processes/files will start or be written to.")

;; tmp directory for TRAMP
(make-directory (expand-file-name "tmp" jupyter-test-temporary-directory))
;; Ensure we don't overwrite the default cookie file
(setq url-cookie-file (let ((temporary-file-directory jupyter-test-temporary-directory))
                        (make-temp-file "jupyter-cookie")))

(message "system-configuration %s" system-configuration)

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

(cl-defmethod initialize-instance ((client jupyter-echo-client) &optional _slots)
  (cl-call-next-method)
  (oset client messages (make-ring 10))
  (oset client kcomm (jupyter-channel-ioloop-comm
                      :ioloop-class 'jupyter-zmq-channel-ioloop))
  (with-slots (kcomm) client
    (oset kcomm hb (jupyter-hb-channel))
    (oset kcomm channels
          (list :stdin (make-jupyter-proxy-channel)
                :shell (make-jupyter-proxy-channel)
                :iopub (make-jupyter-proxy-channel)))))

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

;;; `jupyter-mock-comm-layer'

(defclass jupyter-mock-comm-layer (jupyter-comm-layer
                                   jupyter-comm-autostop)
  ((alive :initform nil)))

(cl-defmethod jupyter-comm-alive-p ((comm jupyter-mock-comm-layer))
  (oref comm alive))

(cl-defmethod jupyter-comm-start ((comm jupyter-mock-comm-layer))
  (unless (oref comm alive)
    (oset comm alive 0))
  (cl-incf (oref comm alive)))

(cl-defmethod jupyter-comm-stop ((comm jupyter-mock-comm-layer))
  (cl-decf (oref comm alive))
  (when (zerop (oref comm alive))
    (oset comm alive nil)))

(cl-defstruct jupyter-mock-comm-obj event)

(cl-defmethod jupyter-event-handler ((obj jupyter-mock-comm-obj) event)
  (setf (jupyter-mock-comm-obj-event obj) event))

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

(defmacro jupyter-test-at-temporary-directory (&rest body)
  (declare (debug (&rest form)))
  `(let ((default-directory jupyter-test-temporary-directory)
         (temporary-file-directory jupyter-test-temporary-directory)
         (tramp-cache-data (make-hash-table :test #'equal)))
     (let ((port (jupyter-test-ensure-notebook-server)))
       (dolist (method '("jpys" "jpy"))
         (setf
          (alist-get 'tramp-default-port
                     (alist-get method tramp-methods nil nil #'equal))
          (list port))))
     ,@body))

(defmacro jupyter-with-echo-client (client &rest body)
  (declare (indent 1) (debug (symbolp &rest form)))
  `(let ((,client (jupyter-echo-client)))
     ,@body))

(defvar jupyter-test-global-clients nil)

(defvar jupyter-test-global-repls nil)

(defmacro jupyter-test-with-client-cache (client-fun saved-sym kernel client &rest body)
  (declare (indent 4) (debug (functionp symbolp stringp symbolp &rest form)))
  (let ((spec (make-symbol "spec"))
        (saved (make-symbol "saved")))
    `(progn
       ;; If a kernel has died, e.g. being shutdown, remove it.
       (cl-loop
        for saved in (copy-sequence ,saved-sym)
        for client = (cdr saved)
        when (and client (slot-boundp client 'manager)
                  (not (jupyter-kernel-alive-p (oref client manager))))
        do (jupyter-stop-channels client)
        (cl-callf2 delq saved ,saved-sym))
       (let* ((,spec (progn (jupyter-error-if-no-kernelspec ,kernel)
                            (car (jupyter-find-kernelspecs ,kernel))))
              (,saved (cdr (assoc (car ,spec) ,saved-sym)))
              (,client (if (and ,saved (not jupyter-test-with-new-client))
                           ,saved
                         ;; Want a fresh kernel, so shutdown the cached one
                         (when ,saved
                           (if (slot-boundp ,saved 'manager)
                               (jupyter-shutdown-kernel (oref ,saved manager))
                             (jupyter-send-shutdown-request ,saved))
                           (jupyter-stop-channels ,saved))
                         (let ((client (,client-fun (car ,spec))))
                           (prog1 client
                             (let ((el (cons (car ,spec) client)))
                               (push el ,saved-sym)))))))
         ;; See the note about increasing timeouts during CI testing at the top
         ;; of jupyter-test.el
         (accept-process-output nil 1)
         ,@body))))

(defmacro jupyter-test-with-kernel-client (kernel client &rest body)
  "Start a new KERNEL client, bind it to CLIENT, evaluate BODY.
This only starts a single global client unless the variable
`jupyter-test-with-new-client' is non-nil."
  (declare (indent 2) (debug (stringp symbolp &rest form)))
  `(jupyter-test-with-client-cache
       (lambda (name) (cadr (jupyter-start-new-kernel name)))
       jupyter-test-global-clients ,kernel ,client
     (unwind-protect
         (progn ,@body)
       (when jupyter-test-with-new-client
         (jupyter-shutdown-kernel (oref client manager))))))

(defmacro jupyter-test-with-python-client (client &rest body)
  "Start a new Python kernel, bind it to CLIENT, evaluate BODY."
  (declare (indent 1) (debug (symbolp &rest form)))
  `(jupyter-test-with-kernel-client "python" ,client
     ,@body))

(defmacro jupyter-test-with-kernel-repl (kernel client &rest body)
  "Start a new KERNEL REPL, bind the client to CLIENT, evaluate BODY.

If `jupyter-test-with-new-client' is nil, any previously started
REPLs available will be re-used without starting a new one and no
cleanup of the REPL is done after evaluating BODY.

When `jupyter-test-with-new-client' is non-nil, a fresh REPL is
started and the REPL deleted after evaluating BODY."
  (declare (indent 2) (debug (stringp symbolp &rest form)))
  `(jupyter-test-with-client-cache
       jupyter-run-repl jupyter-test-global-repls ,kernel ,client
     (unwind-protect
         (jupyter-with-repl-buffer ,client
           (progn ,@body))
       (cl-letf (((symbol-function 'yes-or-no-p)
                  (lambda (_prompt) t))
                 ((symbol-function 'y-or-n-p)
                  (lambda (_prompt) t))
                 (jupyter-default-timeout 5))
         (when jupyter-test-with-new-client
           (kill-buffer (oref ,client buffer)))))))

(defmacro jupyter-test-with-python-repl (client &rest body)
  "Start a new Python REPL and run BODY.
CLIENT is bound to the Python REPL. Delete the REPL buffer after
running BODY."
  (declare (indent 1) (debug (symbolp &rest form)))
  `(jupyter-test-with-kernel-repl "python" ,client
     ,@body))

(defun jupyter-test-ioloop-eval-event (ioloop event)
  (eval
   `(progn
      ,@(oref ioloop setup)
      ,(jupyter-ioloop--event-dispatcher ioloop event))))

(defmacro jupyter-test-channel-ioloop (ioloop &rest body)
  (declare (indent 1))
  (let ((var (car ioloop))
        (val (cadr ioloop)))
    (with-temp-buffer
      `(let* ((,var ,val)
              (standard-output (current-buffer))
              (jupyter-channel-ioloop-channels nil)
              (jupyter-channel-ioloop-session nil)
              ;; Needed so that `jupyter-ioloop-environment-p' passes
              (jupyter-ioloop-stdin t)
              (jupyter-ioloop-poller (zmq-poller)))
         (unwind-protect
             (progn ,@body)
           (zmq-poller-destroy jupyter-ioloop-poller)
           (jupyter-ioloop-stop ,var))))))

(defmacro jupyter-test-rest-api-request (bodyform &rest check-forms)
  "Replace the body of `url-retrieve*' with CHECK-FORMS, evaluate BODYFORM.
For `url-retrieve', the callback will be called with a nil status."
  (declare (indent 1))
  `(progn
     (defvar url-request-data)
     (defvar url-request-method)
     (defvar url-request-extra-headers)
     (defvar url-http-end-of-headers)
     (defvar url-http-response-status)
     (defvar url-http-content-type)
     (let (url-request-data
           url-request-method
           url-request-extra-headers
           url-http-end-of-headers
           url-http-content-type
           (url-http-response-status 200)
           (fun (lambda (url &rest _)
                  (setq url-http-end-of-headers (point-min))
                  ,@check-forms
                  (current-buffer))))
       (with-temp-buffer
         (cl-letf (((symbol-function #'url-retrieve-synchronously) fun)
                   ((symbol-function #'url-retrieve)
                    (lambda (url cb &optional cbargs &rest _)
                      (prog1
                          (funcall fun url)
                        (apply cb nil cbargs)))))
           ,bodyform)))))

(defmacro jupyter-test-rest-api-with-notebook (client &rest body)
  (declare (indent 1))
  `(let* ((url-cookie-storage nil)
          (url-cookie-secure-storage nil)
          (host (format "localhost:%s" (jupyter-test-ensure-notebook-server)))
          (,client (jupyter-rest-client :url (format "http://%s" host))))
     ,@body))

(defmacro jupyter-test-with-notebook (server &rest body)
  (declare (indent 1))
  `(let* ((host (format "localhost:%s" (jupyter-test-ensure-notebook-server)))
          (url (format "http://%s" host))
          (,server (or (jupyter-find-server url)
                       (jupyter-server :url url))))
     ,@body))

(defmacro jupyter-test-with-server-kernel (server name kernel &rest body)
  (declare (indent 3))
  (let ((id (make-symbol "id")))
    `(let ((,kernel (jupyter-server-kernel
                     :server server
                     :spec (jupyter-guess-kernelspec
                            ,name (jupyter-server-kernelspecs ,server)))))
       (let ((,id (jupyter-start-kernel kernel)))
         (unwind-protect
             (progn ,@body)
           (jupyter-api-shutdown-kernel ,server ,id))))))

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
         (version
          (with-temp-buffer
            (call-process cmd nil t nil
                          "-c" "import ipykernel; \
print(\"{}.{}.{}\".format(*ipykernel.version_info[:3]))")
            (buffer-string))))
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
        ;; Add a dummy execution count since it is handled specially in
        ;; `jupyter-handle-message' to update the state of the client.
        :content (append content (list :execution_count 0))))

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
  (let* ((ports
          (cl-loop
           with ports = (jupyter-available-local-ports 5)
           for c in '(:shell :hb :iopub :stdin :control)
           collect c and collect (pop ports))))
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

(defun jupyter-test-text-has-property (prop val &optional positions)
  "Ensure PROP has VAL for text at POSITIONS.
It is an error if any text not at POSITIONS has PROP. A nil value
of POSITIONS means that all text from `point-min' to `point-max'
should have PROP with VAL."
  (cl-loop
   for i from (point-min) to (point-max)
   if (or (null positions) (memq i positions))
   do (should (equal (get-text-property i prop) val))
   else do (should-not (get-text-property i prop))))

(defun jupyter-test-kill-buffer (buffer)
  "Kill BUFFER, defaulting to yes for all `kill-buffer-query-functions'."
  (cl-letf (((symbol-function 'yes-or-no-p)
             (lambda (_prompt) t))
            ((symbol-function 'y-or-n-p)
             (lambda (_prompt) t)))
    (kill-buffer buffer)))

;;; `org-mode'

(defvar org-babel-load-languages)
(defvar org-confirm-babel-evaluate)

(defvar jupyter-org-test-session nil
  "Name of the session for testing Jupyter source blocks.")

(defvar jupyter-org-test-buffer nil
  "`org-mode' buffer for testing Jupyter source blocks.")

(defun jupyter-org-test-setup ()
  (unless jupyter-org-test-session
    (require 'org)
    (setq org-babel-load-languages
          '((python . t)
            (jupyter . t)))
    (setq org-confirm-babel-evaluate nil)
    (setq jupyter-org-test-session (make-temp-name "ob-jupyter-test"))
    (setq jupyter-org-test-buffer (get-buffer-create "ob-jupyter-test"))
    (org-babel-do-load-languages
     'org-babel-load-languages
     org-babel-load-languages)
    (with-current-buffer jupyter-org-test-buffer
      (org-mode)
      (insert
       "#+BEGIN_SRC jupyter-python " ":session " jupyter-org-test-session "\n"
       "#+END_SRC")
      (setq jupyter-current-client
            (with-current-buffer (org-babel-initiate-session)
              jupyter-current-client))))
  (with-current-buffer jupyter-org-test-buffer
    (erase-buffer)))

(defmacro jupyter-org-test (&rest body)
  (declare (debug (body)))
  `(progn
     (jupyter-org-test-setup)
     (with-current-buffer jupyter-org-test-buffer
       ,@body)))

(defmacro jupyter-org-test-src-block (block expected-result &rest args)
  "Test source code BLOCK.
EXPECTED-RESULT is a string that the source block's results
should match. ARGS is a plist of header arguments to be set for
the source code block. For example if ARGS is (:results \"raw\")
then the source code block will begin like

    #+BEGIN_SRC jupyter-python :results raw ...

Note if ARGS contains a key, regexp, then if regexp is non-nil,
EXPECTED-RESULT is a regular expression to match against the
results instead of an equality match."
  (let (regexp)
    (setq args
          (cl-loop for (arg val) on args by #'cddr
                   if (eq arg :regexp) do (setq regexp val)
                   else collect (cons arg val)))
    `(jupyter-org-test
      (jupyter-org-test-src-block-1 ,block ,expected-result ,regexp ',args))))

(defun jupyter-org-test-make-block (code args)
  (let ((arg-str (mapconcat
                  (lambda (x)
                    (cl-destructuring-bind (name . val) x
                      (concat (symbol-name name) " " (format "%s" val))))
                  args " ")))
    (concat
     "#+BEGIN_SRC jupyter-python " arg-str " :session "
     jupyter-org-test-session "\n"
     code "\n"
     "#+END_SRC")))

(defun jupyter-org-test-src-block-1 (code test-result &optional regexp args)
  (let ((src-block (jupyter-org-test-make-block code args)))
    (insert src-block)
    (let* ((info (org-babel-get-src-block-info)))
      (save-window-excursion
        (org-babel-execute-src-block nil info))
      (org-with-point-at (org-babel-where-is-src-block-result nil info)
        (when (equal (alist-get :async args) "yes")
          (jupyter-wait-until-idle
           (jupyter-last-sent-request jupyter-current-client)))
        (let ((element (org-element-context)))
          ;; Handle empty results with just a RESULTS keyword
          ;;
          ;; #+RESULTS:
          (if (eq (org-element-type element) 'keyword) ""
            (let ((result (buffer-substring-no-properties
                           (jupyter-org-element-begin-after-affiliated element)
                           (org-element-property :end element))))
              (if regexp (should (string-match-p test-result result))
                (message "\

Testing src-block:
%s

Expected result:
\"%s\"

Result:
\"%s\"

"
                         src-block test-result result)
                (should (eq (compare-strings
                             result nil nil test-result nil nil
                             'ignore-case)
                            t))))))))))

;;; Notebook server

(defvar jupyter-test-notebook nil
  "A cons cell (PROC . PORT).
PROC is the notebook process and PORT is the port it is connected
to.")

(defun jupyter-test-ensure-notebook-server (&optional authentication)
  "Ensure there is a notebook process available.
Return the port it was started on. The starting directory of the
process will be in the `jupyter-test-temporary-directory'.

If AUTHENTICATION is nil, start a notebook server without any
authentication. If AUTHENTICATION is t start with token
authentication. Finally, if AUTHENTICATION is a string it should
be the hashed password to use for authentication to the server,
see the documentation on the --NotebookApp.password argument."
  (if (process-live-p (car jupyter-test-notebook))
      (cdr jupyter-test-notebook)
    (unless noninteractive
      (error "This should only be called in batch mode"))
    (message "Starting up notebook process for tests")
    (let ((port (car (jupyter-available-local-ports 1))))
      (prog1 port
        (let ((default-directory jupyter-test-temporary-directory)
              (buffer (generate-new-buffer "*jupyter-notebook*"))
              (args (append
                     (list "notebook" "--no-browser" "--debug"
                           (format "--NotebookApp.port=%s" port))
                     (cond
                      ((eq authentication t)
                       (list))
                      ((stringp authentication)
                       (list
                        "--NotebookApp.token=''"
                        (format "--NotebookApp.password='%s'"
                                authentication)))
                      (t
                       (list
                        "--NotebookApp.token=''"
                        "--NotebookApp.password=''"))))))
          (setq jupyter-test-notebook
                (cons (apply #'start-process
                             "jupyter-notebook" buffer "jupyter" args)
                      port))
          (sleep-for 5))))))

;;; Cleanup

(when (or (getenv "APPVEYOR") (getenv "TRAVIS"))
  (add-hook 'kill-emacs-hook
            (lambda ()
              (ignore-errors
                (message "%s" (with-current-buffer
                                  (process-buffer (car jupyter-test-notebook))
                                (buffer-string)))))))

(defvar jupyter-test-zmq-sockets (make-hash-table :weakness 'key))

(advice-add 'zmq-socket
            :around (lambda (&rest args)
                      (let ((sock (apply args)))
                        (prog1 sock
                          (puthash sock t jupyter-test-zmq-sockets)))))

;; Do lots of cleanup to avoid core dumps on Travis due to epoll reconnect
;; attempts.
(add-hook
 'kill-emacs-hook
 (lambda ()
   (ignore-errors (delete-directory jupyter-test-temporary-directory))
   (ignore-errors (delete-process (car jupyter-test-notebook)))
   (cl-loop for server in (jupyter-servers)
            do (ignore-errors (jupyter-comm-stop server)))
   (cl-loop
    for client in (jupyter-clients)
    do (ignore-errors (jupyter-stop-channels client))
    (when (oref client manager)
      (ignore-errors (jupyter-shutdown-kernel (oref client manager)))))
   (cl-loop
    for sock being the hash-keys of jupyter-test-zmq-sockets do
    (ignore-errors
      (zmq-set-option sock zmq-LINGER 0)
      (zmq-close sock)))
   (ignore-errors (zmq-context-terminate (zmq-current-context)))))

;;; test-helper.el ends here
