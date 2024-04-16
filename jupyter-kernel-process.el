;;; jupyter-kernel-process.el --- Jupyter kernels as Emacs processes -*- lexical-binding: t -*-

;; Copyright (C) 2020-2024 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 25 Apr 2020

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

;; Jupyter kernels as Emacs processes.

;;; Code:

(require 'jupyter-kernel)
(require 'jupyter-monads)

(defgroup jupyter-kernel-process nil
  "Jupyter kernels as Emacs processes"
  :group 'jupyter)

(declare-function jupyter-ioloop-start "jupyter-ioloop")
(declare-function jupyter-ioloop-stop "jupyter-ioloop")
(declare-function jupyter-send "jupyter-ioloop")
(declare-function jupyter-ioloop-alive-p "jupyter-ioloop")
(declare-function jupyter-channel-ioloop-set-session "jupyter-channel-ioloop")
(declare-function ansi-color-apply "ansi-color")
(declare-function jupyter-hb-pause "jupyter-zmq-channel")

(defvar jupyter--kernel-processes '()
  "The list of kernel processes launched.
Elements look like (PROCESS CONN-FILE) where PROCESS is a kernel
process and CONN-FILE the associated connection file.

Cleaning up the list removes elements whose PROCESS is no longer
live.  When removing an element, CONN-FILE will be deleted and
PROCESS's buffer killed.

The list is periodically cleaned up when a new process is
launched.

Also, just before Emacs exits any connection files that still
exist are deleted.")

;;; Kernel definition

(cl-defstruct (jupyter-kernel-process
               (:include jupyter-kernel))
  connect-p)

(cl-defmethod jupyter-process ((kernel jupyter-kernel-process))
  "Return the process of KERNEL.
Return nil if KERNEL does not have an associated process."
  (car (cl-find-if (lambda (x) (and (processp (car x))
                               (eq (process-get (car x) :kernel) kernel)))
                   jupyter--kernel-processes)))

(cl-defmethod jupyter-alive-p ((kernel jupyter-kernel-process))
  (let ((process (jupyter-process kernel)))
    (and (process-live-p process)
         (cl-call-next-method))))

(defun jupyter-kernel-process (&rest args)
  "Return a `jupyter-kernel-process' initialized with ARGS."
  (apply #'make-jupyter-kernel-process args))

(cl-defmethod jupyter-kernel :extra "process" (&rest args)
  "Return a kernel as an Emacs process.
If ARGS contains a :spec key with a value being a
`jupyter-kernelspec', a `jupyter-kernel-process' initialized from
it will be returned.  The value can also be a string, in which
case it is considered the name of a kernelspec to use.

If ARGS contains a :conn-info key, a `jupyter-kernel-process'
with a session initialized from its value, either the name of a
connection file to read or a connection property list itself (see
`jupyter-read-connection'), will be returned.  The remaining ARGS
will be used to initialize the returned kernel.

Call the next method if ARGS does not contain a :spec or
:conn-info key."
  (if (plist-get args :server) (cl-call-next-method)
    (let ((spec (plist-get args :spec))
          (conn-info (plist-get args :conn-info)))
      (cond
       ((and spec (not conn-info))
        (when (stringp spec)
          (plist-put args :spec
                     (or (jupyter-guess-kernelspec spec)
                         (error "No kernelspec matching name (%s)" spec))))
        (cl-check-type (plist-get args :spec) jupyter-kernelspec)
        (apply #'jupyter-kernel-process args))
       (conn-info
        (apply #'jupyter-kernel-process
               :session (if (stringp conn-info)
                            (jupyter-connection-file-to-session conn-info)
                          conn-info)
               (cl-loop
                for (k v) on args by #'cddr
                unless (eq k :conn-info) collect k and collect v)))
       (t
        (cl-call-next-method))))))

;;; Client connection

(cl-defmethod jupyter-zmq-io ((kernel jupyter-kernel-process))
  (unless (jupyter-kernel-process-connect-p kernel)
    (jupyter-launch kernel))
  (let ((channels '(:shell :iopub :stdin :control))
        session ch-group hb kernel-io ioloop shutdown)
    (cl-macrolet ((continue-after
                   (cond on-timeout)
                   `(jupyter-with-timeout
                        (nil jupyter-default-timeout ,on-timeout)
                      ,cond)))
      (cl-labels ((set-session
                   ()
                   (or (setq session (jupyter-kernel-session kernel))
                       (error "A session is needed to connect to a kernel's I/O")))
                  (set-ch-group
                   ()
                   (let ((endpoints (jupyter-session-endpoints (set-session))))
                     (setq ch-group
                           (cl-loop
                            for ch in channels
                            collect ch
                            collect (list :endpoint (plist-get endpoints ch)
                                          :alive-p nil)))))
                  (ch-put
                   (ch prop value)
                   (plist-put (plist-get ch-group ch) prop value))
                  (ch-get
                   (ch prop)
                   (plist-get (plist-get ch-group ch) prop))
                  (ch-alive-p
                   (ch)
                   (and ioloop (jupyter-ioloop-alive-p ioloop)
                        (ch-get ch :alive-p)))
                  (ch-start
                   (ch)
                   (unless (ch-alive-p ch)
                     (jupyter-send ioloop 'start-channel ch
                                   (ch-get ch :endpoint))
                     (continue-after
                      (ch-alive-p ch)
                      (error "Channel failed to start: %s" ch))))
                  (ch-stop
                   (ch)
                   (when (ch-alive-p ch)
                     (jupyter-send ioloop 'stop-channel ch)
                     (continue-after
                      (not (ch-alive-p ch))
                      (error "Channel failed to stop: %s" ch))))
                  (start
                   ()
                   (unless ioloop
                     (require 'jupyter-zmq-channel-ioloop)
                     (setq ioloop (make-instance 'jupyter-zmq-channel-ioloop))
                     (jupyter-channel-ioloop-set-session ioloop session))
                   (unless (jupyter-ioloop-alive-p ioloop)
                     (jupyter-ioloop-start
                      ioloop
                      (lambda (event)
                        (pcase (car event)
                          ((and 'start-channel (let ch (cadr event)))
                           (ch-put ch :alive-p t))
                          ((and 'stop-channel (let ch (cadr event)))
                           (ch-put ch :alive-p nil))
                          ;; TODO: Get rid of this
                          ('sent nil)
                          (_
                           (jupyter-run-with-io kernel-io
                             (jupyter-publish event))))))
                     (condition-case err
                         (cl-loop
                          for ch in channels
                          do (ch-start ch))
                       (error
                        (jupyter-ioloop-stop ioloop)
                        (signal (car err) (cdr err)))))
                   ioloop)
                  (stop
                   ()
                   (when hb
                     (jupyter-hb-pause hb)
                     (setq hb nil))
                   (when ioloop
                     (when (jupyter-ioloop-alive-p ioloop)
                       (jupyter-ioloop-stop ioloop))
                     (setq ioloop nil))))
        (set-ch-group)
        (setq kernel-io
              ;; TODO: (jupyter-publisher :name "Session I/O" :fn ...)
              ;;
              ;; so that on error in a subscriber, the name can be
              ;; displayed to know where to look.  This requires a
              ;; `jupyter-publisher' struct type.
              (jupyter-publisher
                (lambda (content)
                  (if shutdown
                      (error "Kernel I/O no longer available: %s"
                             (cl-prin1-to-string session))
                    (pcase (car content)
                      ;; ('message channel idents . msg)
                      ('message
                       (pop content)
                       ;; Set the channel key of the message property list
                       (plist-put
                        (cddr content) :channel
                        (substring (symbol-name (car content)) 1))
                       (jupyter-content (cddr content)))
                      ('send
                       ;; Set the channel argument to a keyword so its
                       ;; recognized by the ioloop
                       (setq content
                             (cons (car content)
                                   (cons (intern (concat ":" (cadr content)))
                                         (cddr content))))
                       (apply #'jupyter-send (start) content))
                      ('hb
                       (unless hb
                         (setq hb
                               (let ((endpoints (set-session)))
                                 (make-instance
                                  'jupyter-hb-channel
                                  :session session
                                  :endpoint (plist-get endpoints :hb)))))
                       (jupyter-run-with-io (cadr content)
                         (jupyter-publish hb)))
                      (_ (error "Unhandled I/O: %s" content)))))))
        (list kernel-io
              (jupyter-subscriber
                (lambda (action)
                  (pcase action
                    ('interrupt
                     (jupyter-interrupt kernel))
                    ('shutdown
                     (jupyter-shutdown kernel)
                     (stop)
                     (setq shutdown t))
                    ('restart
                     (setq shutdown nil)
                     (jupyter-restart kernel)
                     (stop)
                     (set-ch-group)
                     (start))
                    (`(action ,fn)
                     (funcall fn kernel))))))))))

(cl-defmethod jupyter-io ((kernel jupyter-kernel-process))
  "Return an I/O connection to KERNEL's session."
  (jupyter-zmq-io kernel))

;;; Kernel management

(defun jupyter--gc-kernel-processes ()
  (setq jupyter--kernel-processes
        (cl-loop for (p conn-file) in jupyter--kernel-processes
                 if (process-live-p p) collect (list p conn-file)
                 else do (delete-process p)
                 (when (file-exists-p conn-file)
                   (delete-file conn-file))
                 and when (buffer-live-p (process-buffer p))
                 do (kill-buffer (process-buffer p)))))

(defun jupyter-delete-connection-files ()
  "Delete all connection files created by Emacs."
  ;; Ensure Emacs can be killed on error
  (ignore-errors
    (cl-loop for (_ conn-file) in jupyter--kernel-processes
             do (when (file-exists-p conn-file)
                  (delete-file conn-file)))))

(add-hook 'kill-emacs-hook #'jupyter-delete-connection-files)

(defun jupyter--start-kernel-process (name kernelspec conn-file)
  (let* ((process-name (format "jupyter-kernel-%s" name))
         (buffer-name (format " *jupyter-kernel[%s]*" name))
         (process-environment
          (append (jupyter-process-environment kernelspec)
                  process-environment))
         (args (jupyter-kernel-argv kernelspec conn-file))
         (atime (nth 4 (file-attributes conn-file)))
         (process (apply #'start-file-process process-name
                         (generate-new-buffer buffer-name)
                         (car args) (cdr args))))
    (set-process-query-on-exit-flag process jupyter--debug)
    ;; Wait until the connection file has been read before returning.
    ;; This is to give the kernel a chance to setup before sending it
    ;; messages.
    ;;
    ;; TODO: Replace with a check of the heartbeat channel.
    (jupyter-with-timeout
        ((format "Starting %s kernel process..." name)
         jupyter-long-timeout
         (unless (process-live-p process)
           (error "Kernel process exited:\n%s"
                  (with-current-buffer (process-buffer process)
                    (ansi-color-apply (buffer-string))))))
      ;; Windows systems may not have good time resolution when retrieving
      ;; the last access time of a file so we don't bother with checking that
      ;; the kernel has read the connection file and leave it to the
      ;; downstream initialization to ensure that we can communicate with a
      ;; kernel.
      (or (memq system-type '(ms-dos windows-nt cygwin))
          (let ((attribs (file-attributes conn-file)))
            ;; `file-attributes' can potentially return nil, in this case
            ;; just assume it has read the connection file so that we can
            ;; know for sure it is not connected if it fails to respond to
            ;; any messages we send it.
            (or (null attribs)
                (not (equal atime (nth 4 attribs)))))))
    (jupyter--gc-kernel-processes)
    (push (list process conn-file) jupyter--kernel-processes)
    process))

(cl-defmethod jupyter-launch :before ((kernel jupyter-kernel-process))
  "Ensure KERNEL has a non-nil SESSION slot.
A `jupyter-session' with random port numbers for the channels and
a newly generated message signing key will be set as the value of
KERNEL's SESSION slot if it is nil."
  (pcase-let (((cl-struct jupyter-kernel-process session) kernel))
    (unless session
      (setf (jupyter-kernel-session kernel) (jupyter-session-with-random-ports))
      ;; This is here for stability when running the tests.  Sometimes
      ;; the kernel ports are not set up fast enough due to the hack
      ;; done in `jupyter-session-with-random-ports'.  The effect
      ;; seems to be messages that are sent but never received by the
      ;; kernel.
      (sit-for 0.2))))

(cl-defmethod jupyter-launch ((kernel jupyter-kernel-process))
  "Start KERNEL's process.
Do nothing if KERNEL's process is already live.

The process arguments are constructed from KERNEL's SPEC.  The
connection file passed as argument to the process is first
written to file, its contents are generated from KERNEL's SESSION
slot.

See also https://jupyter-client.readthedocs.io/en/stable/kernels.html#kernel-specs"
  (let ((process (jupyter-process kernel)))
    (unless (process-live-p process)
      (pcase-let (((cl-struct jupyter-kernel-process spec session) kernel))
        (let ((conn-file (jupyter-write-connection-file session)))
          (setq process (jupyter--start-kernel-process
                         (jupyter-kernel-name kernel) spec
                         conn-file))
          ;; Make local tunnels to the remote ports when connecting to
          ;; remote kernels. Update the session object to reflect
          ;; these changes.
          (when (file-remote-p conn-file)
            (setf (jupyter-kernel-session kernel)
                  (let ((conn-info (jupyter-tunnel-connection conn-file)))
                    (jupyter-session
                     :conn-info conn-info
                     :key (plist-get conn-info :key)))))))
      (setf (process-get process :kernel) kernel)
      (setf (process-sentinel process)
            (lambda (process _)
              (pcase (process-status process)
                ('signal
                 (let ((kernel (process-get process :kernel)))
                   (when kernel
                     (warn "Kernel died unexpectedly")
                     (jupyter-shutdown kernel)))))))))
  (cl-call-next-method))

(cl-defmethod jupyter-shutdown ((kernel jupyter-kernel-process))
  "Shutdown KERNEL by killing its process unconditionally."
  (let ((process (jupyter-process kernel)))
    (when process
      (setf (process-get process :kernel) nil)
      (delete-process process))
    (cl-call-next-method)))

(cl-defmethod jupyter-restart ((_kernel jupyter-kernel-process))
  (cl-call-next-method))

(cl-defmethod jupyter-interrupt ((kernel jupyter-kernel-process))
  "Interrupt KERNEL's process.
The process can be interrupted when the interrupt mode of
KERNEL's spec. is \"signal\" or not specified.

See also https://jupyter-client.readthedocs.io/en/stable/kernels.html#kernel-specs"
  (pcase-let* ((process (jupyter-process kernel))
               ((cl-struct jupyter-kernel-process spec) kernel)
               ((cl-struct jupyter-kernelspec plist) spec)
               (imode (plist-get plist :interrupt_mode)))
    (cond
     ((or (null imode) (string= imode "signal"))
      (when (process-live-p process)
        (interrupt-process process t)))
     ((string= imode "message")
      (error "Send an interrupt_request using a client"))
     (t (cl-call-next-method)))))

(provide 'jupyter-kernel-process)

;;; jupyter-kernel-process.el ends here


