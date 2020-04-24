;;; jupyter-kernel-process.el --- Jupyter kernels as Emacs processes -*- lexical-binding: t -*-

;; Copyright (C) 2020 Nathaniel Nicandro

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

(defgroup jupyter-kernel-process nil
  "Jupyter kernels as Emacs processes"
  :group 'jupyter)

(declare-function jupyter-channel-ioloop-set-session "jupyter-channel-ioloop")

;;; Kernel definition

(cl-defstruct (jupyter-kernel-process
               (:include jupyter-kernel))
  (process nil
           :type (or null process)
           :documentation "A kernel process."))

(cl-defmethod jupyter-alive-p ((kernel jupyter-kernel-process))
  (pcase-let (((cl-struct jupyter-kernel-process process) kernel))
    (and (process-live-p process)
         (cl-call-next-method))))

(defun jupyter-kernel-process (&rest args)
  "Return a representation of a kernel process.
Return a `jupyter-kernel-process' with ARGS being the slot values
used for initialization."
  (let ((kernel (apply #'make-jupyter-kernel-process args))
        (session (pcase (plist-get args :session)
                   ((and (pred stringp) (pred file-exists-p) `,conn-file)
                    (let ((conn-info (jupyter-read-connection conn-file)))
                      (jupyter-session
                       :conn-info conn-info
                       :key (plist-get conn-info :key))))
                   ((and (pred jupyter-session-p) `,session)
                    session)
                   (_
                    (prog1 (jupyter-session-with-random-ports)
                      ;; This is here for stability when running
                      ;; the tests.  Sometimes the kernel ports are
                      ;; not set up fast enough due to the hack
                      ;; done in
                      ;; `jupyter-session-with-random-ports'.  The
                      ;; effect seems to be messages that are sent
                      ;; but never received by the kernel.
                      (sit-for 0.2)))))
        (spec (pcase (plist-get args :spec)
                ((and (pred stringp) `,name)
                 (or (jupyter-guess-kernelspec name)
                     (error "No kernelspec matching name (%s)" name)))
                (`,spec spec))))
    (setf (jupyter-kernel-session kernel) session)
    (setf (jupyter-kernel-spec kernel) spec)
    kernel))

;;; Client connection

(cl-defstruct jupyter--proxy-channel endpoint alive-p)

(defun jupyter--make-channel-group (session)
  (let ((endpoints (jupyter-session-endpoints session)))
    (append
     (list 'channel-group t
           :hb (make-instance
                'jupyter-hb-channel
                :session session
                :endpoint (plist-get endpoints :hb)))
     (cl-loop
      for channel in '(:control :shell :iopub :stdin)
      collect channel
      collect (make-jupyter--proxy-channel
               :endpoint (plist-get endpoints channel)
               :alive-p nil)))))

(defun jupyter--channel-alive-p (ioloop chgroup channel)
  (if (eq channel :hb)
      (let ((hb (plist-get chgroup channel)))
        (and hb (jupyter-channel-alive-p hb)))
    (and ioloop (jupyter-ioloop-alive-p ioloop)
         (jupyter--proxy-channel-alive-p
          (plist-get chgroup channel)))))

(defun jupyter--start-channel (ioloop chgroup channel)
  (unless (jupyter--channel-alive-p ioloop chgroup channel)
    (if (eq channel :hb) (jupyter-start-channel (plist-get chgroup channel))
      (let ((endpoint (jupyter--proxy-channel-endpoint
                       (plist-get chgroup channel))))
        (jupyter-send ioloop 'start-channel channel endpoint)
        ;; Verify that the channel starts
        (jupyter-with-timeout
            (nil jupyter-default-timeout
                 (error "Channel not started in ioloop subprocess (%s)" channel))
          (jupyter--channel-alive-p ioloop chgroup channel))))))

(defun jupyter--stop-channel (ioloop chgroup channel)
  (when (jupyter--channel-alive-p ioloop chgroup channel)
    (if (eq channel :hb) (jupyter-stop-channel (plist-get chgroup channel))
      (jupyter-send ioloop 'stop-channel channel)
      ;; Verify that the channel stops
      (jupyter-with-timeout
          (nil jupyter-default-timeout
               (error "Channel not stopped in ioloop subprocess (%s)" channel))
        (not (jupyter--channel-alive-p ioloop chgroup channel))))))

(cl-defmethod jupyter-connection ((kernel jupyter-kernel-process) (handler function))
  (require 'jupyter-zmq-channel-ioloop)
  (let* ((session (jupyter-kernel-session kernel))
         (channels '(:hb :shell :iopub :stdin))
         (chgroup (jupyter--make-channel-group session))
         (ioloop (make-instance 'jupyter-zmq-channel-ioloop)))
    (jupyter-channel-ioloop-set-session ioloop session)
    ;; session and ioloop are in the context of the connection and are
    ;; thus not accessible outside of it, therefore no other parts of
    ;; Emacs-Jupyter have to consider them.
    (make-jupyter-connection
     :hb (plist-get chgroup :hb)
     :id (lambda ()
           (format "session=%s" (truncate-string-to-width
                                 (jupyter-session-id session)
                                 9 nil nil "…")))
     :start (lambda (&optional channel)
              (unless (jupyter-ioloop-alive-p ioloop)
                (jupyter-ioloop-start
                 ioloop (lambda (event)
                          (pcase (car event)
                            ;; These channel events are from
                            ;; `jupyter-channel-ioloop'
                            ('start-channel
                             (setf (jupyter--proxy-channel-alive-p
                                    (plist-get chgroup (cadr event)))
                                   t))
                            ('stop-channel
                             (setf (jupyter--proxy-channel-alive-p
                                    (plist-get chgroup (cadr event)))
                                   nil))
                            (_
                             (funcall handler event))))))
              (if channel (jupyter--start-channel ioloop chgroup channel)
                (cl-loop
                 for channel in channels
                 do (jupyter--start-channel ioloop chgroup channel))))
     :stop (lambda (&optional channel)
             (if channel (jupyter--stop-channel ioloop chgroup channel)
               (cl-loop
                for channel in channels
                do (jupyter--stop-channel ioloop chgroup channel))
               (jupyter-ioloop-stop ioloop))
             (jupyter-ioloop-stop ioloop))
     :send (lambda (&rest event)
             (apply #'jupyter-send ioloop event))
     :alive-p (lambda (&optional channel)
                (if channel (jupyter--channel-alive-p ioloop chgroup channel)
                  (cl-loop
                   for channel in channels
                   thereis (jupyter--channel-alive-p ioloop chgroup channel)))))))

;;; Kernel management

(defvar jupyter--kernel-processes '()
  "The list of kernel processes launched.
Elements look like (PROCESS CONN-FILE) where PROCESS is a kernel
process and CONN-FILE the associated connection file.

This list is periodically cleaned up when a new process is
launched and when Emacs exits.")

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

(defun jupyter--kernel-died-process-sentinel (kernel)
  "Return a sentinel function calling KERNEL's `jupyter-kernel-died' method.
The method will be called when the process exits or receives a
fatal signal."
  (let ((ref (jupyter-weak-ref kernel)))
    (lambda (process _)
      (when-let (kernel (and (memq (process-status process) '(exit signal))
                             (jupyter-weak-ref-resolve ref)))
        (jupyter-kernel-died kernel)))))

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
  (pcase-let (((cl-struct jupyter-kernel-process process spec session) kernel))
    (unless (process-live-p process)
      (setq process
            (setf (jupyter-kernel-process-process kernel)
                  (jupyter--start-kernel-process
                   (jupyter-kernel-name kernel) spec
                   (jupyter-write-connection-file session))))
      (setf (process-sentinel process)
            ;; TODO: Have the sentinel function do something like
            ;; notify clients.  It should also handle auto-restarting
            ;; if that is wanted.
            (jupyter--kernel-died-process-sentinel kernel))
      (setf (jupyter-kernel-process-process kernel) process)))
  (cl-call-next-method))

;; TODO: Add restart argument
(cl-defmethod jupyter-shutdown ((kernel jupyter-kernel-process))
  "Shutdown KERNEL by killing its process unconditionally."
  (pcase-let (((cl-struct jupyter-kernel-process process) kernel))
    (when (process-live-p process)
      ;; The `process-sentinel' is ignored when shutting down because
      ;; killing the process when explicitly shutting it down is not
      ;; an unexpected exit.
      (setf (process-sentinel process) #'ignore)
      (kill-process process))
    (cl-call-next-method)))

(cl-defmethod jupyter-interrupt ((kernel jupyter-kernel-process))
  "Interrupt KERNEL's process.
The process can be interrupted when the interrupt mode of
KERNEL's SPEC is \"signal\" or not specified, otherwise the
KERNEL is interrupted by sending an :interrupt-request on
KERNEL's control channel.

See also https://jupyter-client.readthedocs.io/en/stable/kernels.html#kernel-specs"
  (pcase-let* (((cl-struct jupyter-kernel-process process spec) kernel)
               ((cl-struct jupyter-kernelspec plist) spec)
               (imode (plist-get plist :interrupt_mode)))
    (if (or (null imode) (string= imode "signal"))
        (interrupt-process process t)
      (cl-call-next-method))))

(provide 'jupyter-kernel-process)

;;; jupyter-kernel-process.el ends here

