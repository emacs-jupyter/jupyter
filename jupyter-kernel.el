;;; jupyter-kernel.el --- Kernels -*- lexical-binding: t -*-

;; Copyright (C) 2020 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 21 Apr 2020

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

;; Working with Jupyter kernels.  This file contains the functions
;; used to control the lifetime of a kernel and how clients can
;; connect to launched kernels.

;;; Code:

(require 'jupyter-base)
(require 'jupyter-monads)
(require 'jupyter-kernelspec)

(defgroup jupyter-kernel nil
  "Kernels"
  :group 'jupyter)

;;; Kernel definition

(cl-defstruct jupyter-kernel
  "A Jupyter kernel."
  (spec (make-jupyter-kernelspec)
        :type jupyter-kernelspec
        :documentation "The kernelspec of this kernel.")
  ;; FIXME: Remove this slot, used by `jupyter-widget-client'.
  (session nil :type jupyter-session))

(cl-defmethod jupyter-io :around ((kernel jupyter-kernel))
  (jupyter-do-launch kernel)
  (jupyter-mlet* ((value (cl-call-next-method)))
    (pcase-let ((`(,io ,discard-io) value))
      ;; Discard the I/O connection on shutdown
      (jupyter-run-with-io io
        (jupyter-subscribe
          (jupyter-subscriber
            (lambda (msg)
              (when (and (eq (jupyter-message-parent-type msg)
                             :shutdown-request)
                         (or (jupyter-message-status-idle-p msg)
                             (eq (jupyter-message-type msg)
                                 :shutdown-reply)))
                (funcall discard-io)
                ;; Cleanup the Emacs representation of a kernel.
                (jupyter-do-shutdown kernel)
                (jupyter-unsubscribe))))))
      (jupyter-return-delayed io))))

(cl-defmethod jupyter-alive-p ((kernel jupyter-kernel))
  "Return non-nil if KERNEL has been launched."
  (and (jupyter-kernel-session kernel) t))

(cl-defmethod cl-print-object ((kernel jupyter-kernel) stream)
  (princ (format "#<jupyter-kernel %s%s>"
                 (jupyter-kernelspec-name (jupyter-kernel-spec kernel))
                 (if (jupyter-alive-p kernel)
                     (concat " " (truncate-string-to-width
                                  (jupyter-session-id (jupyter-kernel-session kernel))
                                  9 nil nil "…"))
                   ""))
         stream))

(cl-defgeneric jupyter-kernel (&rest args)
  "Return a kernel constructed from ARGS.
ARGS are keyword arguments used to initialize the returned
kernel.

The default implementation will return a `jupyter-kernel' with a
session initialized from the value of :conn-info in ARGS, either
the name of a connection file to read or itself a connection
property list (see `jupyter-read-connection').  A client can
connect to the returned kernel using `jupyter-client'.

This method can be extended with extra primary methods for the
purposes of handling different forms of ARGS that do not just
need the default behavior."
  (let ((conn-info (plist-get args :conn-info)))
    (cond
     (conn-info
      (when (stringp conn-info)
        (setq conn-info (jupyter-read-connection conn-info)))
      (apply #'make-jupyter-kernel
             :session (jupyter-session
                       :conn-info conn-info
                       :key (plist-get conn-info :key))
             (cl-loop
              for (k v) on args by #'cddr
              unless (eq k :conn-info) collect k and collect v)))
     (t
      (cl-call-next-method)))))

;;; Kernel management

(cl-defgeneric jupyter-do-launch ((kernel jupyter-kernel))
  "Launch KERNEL."
  (cl-assert (jupyter-alive-p kernel)))

(cl-defmethod jupyter-do-launch :before ((kernel jupyter-kernel))
  "Notify that the kernel launched."
  (message "Launching %s kernel..." (jupyter-kernel-name kernel)))

(cl-defmethod jupyter-do-launch :after ((kernel jupyter-kernel))
  "Notify that the kernel launched."
  (message "Launching %s kernel...done" (jupyter-kernel-name kernel)))

(cl-defgeneric jupyter-do-shutdown ((kernel jupyter-kernel))
  "Shutdown KERNEL.
Once a kernel has been shutdown it has no more connected clients
and the process it represents no longer exists.

The default implementation of this method disconnects all
connected clients of KERNEL and sets KERNEL's session slot to
nil."
  (setf (jupyter-kernel-session kernel) nil))

(cl-defmethod jupyter-do-shutdown :before ((kernel jupyter-kernel))
  "Notify that the kernel will be shutdown."
  (message "%s kernel shutdown..." (jupyter-kernel-name kernel)))

(cl-defmethod jupyter-do-shutdown :after ((kernel jupyter-kernel))
  "Notify that the kernel launched."
  (message "%s kernel shutdown...done" (jupyter-kernel-name kernel)))

(defun jupyter-restart (kernel)
  "Shutdown then re-launch KERNEL."
  (jupyter-do-shutdown kernel)
  (jupyter-do-launch kernel))

(cl-defgeneric jupyter-do-interrupt ((kernel jupyter-kernel))
  "Interrupt KERNEL.

The default implementation sends an interrupt request on KERNEL's
control channel if the KERNEL's spec. has an :interrupt_mode
equal to \"message\"."
  (pcase-let* (((cl-struct jupyter-kernel spec session) kernel)
               ((cl-struct jupyter-kernelspec plist) spec))
    (when (string= (plist-get plist :interrupt_mode) "message")
      (let ((channel
             (make-instance
              'jupyter-zmq-channel
              :type :control
              :session session
              :endpoint (plist-get (jupyter-session-endpoints session) :control))))
        ;; TODO: `with-live-jupyter-channel'
        (jupyter-start channel)
        (jupyter-send channel :interrupt-request '())
        (jupyter-with-timeout
            ((format "Interrupting %s kernel"
                     (jupyter-kernel-name kernel))
             jupyter-default-timeout
             (message "No interrupt reply from kernel (%s)"
                      (jupyter-kernel-name kernel)))
          (jupyter-recv channel 'dont-wait))
        (jupyter-stop channel)))))

(provide 'jupyter-kernel)

;;; jupyter-kernel.el ends here
