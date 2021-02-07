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
      ;; TODO: A `jupyter-io' method that handles this case.
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

(defun jupyter-kernel-name (kernel)
  (jupyter-kernelspec-name
   (jupyter-kernel-spec kernel)))

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
  (setf (jupyter-kernel-session kernel) nil))

(cl-defmethod jupyter-shutdown :before ((kernel jupyter-kernel))
  "Notify that the kernel will be shutdown."
  (message "%s kernel shutdown..." (jupyter-kernel-name kernel)))

(cl-defmethod jupyter-shutdown :after ((kernel jupyter-kernel))
  "Notify that the kernel launched."
  (message "%s kernel shutdown...done" (jupyter-kernel-name kernel)))

(cl-defgeneric jupyter-restart ((kernel jupyter-kernel))
  "Restart KERNEL.

The default implementation shuts down and then re-launches
KERNEL."
  (jupyter-shutdown kernel)
  (jupyter-launch kernel))

(cl-defgeneric jupyter-interrupt ((kernel jupyter-kernel))
  "Interrupt KERNEL."
  (ignore))

(provide 'jupyter-kernel)

;;; jupyter-kernel.el ends here
