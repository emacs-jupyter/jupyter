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
(require 'jupyter-kernelspec)

(declare-function jupyter-disconnect "jupyter-connection")

(defgroup jupyter-kernel nil
  "Kernels"
  :group 'jupyter)

;;; Kernel definition

(cl-defstruct jupyter-kernel
  "A Jupyter kernel."
  (spec (make-jupyter-kernelspec)
        :type jupyter-kernelspec
        :documentation "The kernelspec of this kernel.")
  ;; TODO: Remove require cycle so that I can have
  ;; `jupyter-connection' be the type and `make-jupyter-connection'
  ;; the default value.
  (connection nil
              :documentation "Kernel communication.")
  ;; FIXME: Remove this slot, used by `jupyter-widget-client'.
  (session nil :type jupyter-session)
  (clients nil
           :type (list-of jupyter-kernel-client)
           :documentation "List of clients able to receive messages."))

(cl-defmethod jupyter-alive-p ((kernel jupyter-kernel))
  "Return non-nil if KERNEL has been launched."
  (and (jupyter-kernel-session kernel) t))

(defun jupyter-kernel (&rest args)
  "Return a kernel constructed from ARGS.
ARGS are keyword arguments."
  (if (plist-get args :conn-info)
      (make-jupyter-kernel
       :session (let ((conn-info
                       (if (stringp (plist-get args :conn-info))
                           (jupyter-read-connection
                            (plist-get args :conn-file))
                         (plist-get args :conn-info))))
                  (jupyter-session
                   :conn-info conn-info
                   :key (plist-get conn-info :key))))
    (error "Implement")))

;;; Kernel management

(cl-defgeneric jupyter-launch ((kernel jupyter-kernel))
  "Launch KERNEL."
  (cl-assert (jupyter-alive-p kernel)))

(cl-defmethod jupyter-launch :before ((kernel jupyter-kernel))
  "Notify that the kernel launched."
  (message "Launching %s kernel..." (jupyter-kernel-name kernel)))

(cl-defmethod jupyter-launch :after ((kernel jupyter-kernel))
  "Notify that the kernel launched."
  (message "Launching %s kernel...done" (jupyter-kernel-name kernel)))

(cl-defgeneric jupyter-shutdown ((kernel jupyter-kernel))
  "Shutdown KERNEL.
Once a kernel has been shutdown it has no more connected clients
and the process it represents no longer exists.

The default implementation of this method disconnects all
connected clients of KERNEL and sets KERNEL's session slot to
nil."
  (cl-loop
   for client in (jupyter-kernel-clients kernel)
   do (jupyter-disconnect client kernel))
  (setf (jupyter-kernel-session kernel) nil))

(cl-defmethod jupyter-shutdown :before ((kernel jupyter-kernel))
  "Notify that the kernel launched."
  (message "%s kernel shutdown..." (jupyter-kernel-name kernel)))

(cl-defmethod jupyter-shutdown :after ((kernel jupyter-kernel))
  "Notify that the kernel launched."
  (message "%s kernel shutdown...done" (jupyter-kernel-name kernel)))

(defun jupyter-restart (kernel)
  "Shutdown then re-launch KERNEL."
  (jupyter-shutdown kernel)
  (jupyter-launch kernel))

(cl-defgeneric jupyter-interrupt ((kernel jupyter-kernel))
  "Interrupt KERNEL.

The default implementation of this method sends an interrupt
request on KERNEL's control channel if its kernelspec has an
:interrupt_mode of \"message\"."
  (pcase-let* (((cl-struct jupyter-kernel spec session) kernel)
               ((cl-struct jupyter-kernelspec plist) spec))
    (when (string= (plist-get plist :interrupt_mode) "message")
      (let ((channel
             (make-instance
              'jupyter-zmq-channel
              :type :control
              :session session
              :endpoint (cl-destructuring-bind (&key transport ip control_port
                                                     &allow-other-keys)
                            (jupyter-session-conn-info session)
                          (format "%s://%s:%d" transport ip control_port)))))
        ;; TODO: `with-live-jupyter-channel'
        (jupyter-start-channel channel)
        (jupyter-send channel :interrupt-request
                      (jupyter-message-interrupt-request))
        (jupyter-with-timeout
            ((format "Interrupting %s kernel"
                     (jupyter-kernel-name kernel))
             jupyter-default-timeout
             (message "No interrupt reply from kernel (%s)"
                      (jupyter-kernel-name kernel)))
          (jupyter-recv channel 'dont-wait))
        (jupyter-stop-channel channel)))))

(provide 'jupyter-kernel)

;;; jupyter-kernel.el ends here
