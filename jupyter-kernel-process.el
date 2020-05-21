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

(defvar jupyter--kernel-processes '()
  "The list of kernel processes launched.
Elements look like (PROCESS CONN-FILE) where PROCESS is a kernel
process and CONN-FILE the associated connection file.

Cleaning up the list removes elements whose PROCESS is no longer
live.  When removing, CONN-FILE will be deleted and PROCESS's
buffer killed.  The list is periodically cleaned up when a new
process is launched.  Also, any connection files that still exist
before Emacs exits are deleted.")

;;; Kernel definition

(cl-defstruct (jupyter-kernel-process
               (:include jupyter-kernel)))

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
  "Return a representation of a kernel based on an Emacs process.
If ARGS contains a :spec key, return a `jupyter-kernel-process'
initialized using ARGS.  If the value is the name of a
kernelspec, the returned kernel's spec slot will be set to the
corresponding `jupyter-kernelspec'.  The session of the returned
kernel will be initialized with the return value of
`jupyter-session-with-random-ports'.

Call the next method if ARGS does not contain :spec."
  (let ((spec (plist-get args :spec)))
    (if (not spec) (cl-call-next-method)
      (when (stringp spec)
        (plist-put args :spec
                   (or (jupyter-guess-kernelspec spec)
                       (error "No kernelspec matching name (%s)" spec))))
      (apply #'jupyter-kernel-process args))))

;;; Client connection

(defun jupyter-kernel-process-io (kernel)
  (let ((session (jupyter-kernel-session kernel)))
    (let* ((channels '(:shell :iopub :stdin))
           (ch-group (let ((endpoints (jupyter-session-endpoints session)))
                       (cl-loop
                        for ch in channels
                        collect ch
                        collect (list :endpoint (plist-get endpoints ch)
                                      :alive-p nil))))
           (hb nil)
           (discarded nil)
           (kernel-io nil)
           (ioloop nil))
      (cl-macrolet ((continue-after
                     (cond on-timeout)
                     `(jupyter-with-timeout
                          (nil jupyter-default-timeout ,on-timeout)
                        ,cond)))
        (cl-labels ((ch-put
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
                        (error "Channel not started: %s" ch))))
                    (ch-stop
                     (ch)
                     (when (ch-alive-p ch)
                       (jupyter-send ioloop 'stop-channel ch)
                       (continue-after
                        (not (ch-alive-p ch))
                        (error "Channel not stopped: %s" ch))))
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
                            ('sent (message "SENT: %s" (cdr event)))
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
                     (and ioloop
                          (jupyter-ioloop-alive-p ioloop)
                          (jupyter-ioloop-stop ioloop))))
          (setq kernel-io
                (jupyter-publisher
                  (lambda (content)
                    (if discarded
                        (error "Kernel I/O no longer available")
                      (pcase (car content)
                        ;; ('message channel idents . msg)
                        ('message
                         (pop content)
                         ;; TODO: Get rid of this
                         (plist-put
                          (cddr content) :channel
                          (substring (symbol-name (car content)) 1))
                         (jupyter-content (cddr content)))
                        ('send (apply #'jupyter-send (start) content))
                        ('hb
                         (unless hb
                           (setq hb
                                 (let ((endpoints (jupyter-session-endpoints session)))
                                   (make-instance
                                    'jupyter-hb-channel
                                    :session session
                                    :endpoint (plist-get endpoints :hb)))))
                         (jupyter-run-with-io (cadr content)
                           (jupyter-publish hb)))
                        (_
                         (error "Unhandled I/O: %s" content)))))))
          (jupyter-return-delayed
            (list kernel-io
                  (lambda ()
                    (and hb (jupyter-hb-pause hb))
                    (stop)
                    (setq hb nil ioloop nil discarded t)))))))))

(cl-defmethod jupyter-io ((kernel jupyter-kernel-process))
  "Return a connection to KERNEL's session."
  (jupyter-mlet* ((io (jupyter-kernel-process-io kernel)))
    io))

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
        (setq process (jupyter--start-kernel-process
                       (jupyter-kernel-name kernel) spec
                       (jupyter-write-connection-file session))))
      (setf (process-get process :kernel) kernel)
      (setf (process-sentinel process)
            (lambda (process _)
              (pcase (process-status process)
                ('signal
                 (jupyter-kernel-died (process-get process :kernel))))))))
  (cl-call-next-method))

;; TODO: Add restart argument
(cl-defmethod jupyter-shutdown ((kernel jupyter-kernel-process))
  "Shutdown KERNEL by killing its process unconditionally."
  (let ((process (jupyter-process kernel)))
    (when process
      (delete-process process)
      (setf (process-get process :kernel) nil))
    (cl-call-next-method)))

(cl-defmethod jupyter-interrupt ((kernel jupyter-kernel-process))
  "Interrupt KERNEL's process.
The process can be interrupted when the interrupt mode of
KERNEL's SPEC is \"signal\" or not specified, otherwise the
KERNEL is interrupted by sending an :interrupt-request on
KERNEL's control channel.

See also https://jupyter-client.readthedocs.io/en/stable/kernels.html#kernel-specs"
  (pcase-let* ((process (jupyter-process kernel))
               ((cl-struct jupyter-kernel-process spec) kernel)
               ((cl-struct jupyter-kernelspec plist) spec)
               (imode (plist-get plist :interrupt_mode)))
    (if (or (null imode) (string= imode "signal"))
        (when process
          (interrupt-process process t))
      (cl-call-next-method))))

(provide 'jupyter-kernel-process)

;;; jupyter-kernel-process.el ends here


