;;; jupyter-kernel-manager.el --- Jupyter kernel manager -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 08 Jan 2018

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

;; Define interfaces for managing kernels.

;;; Code:

(require 'jupyter-base)
(require 'jupyter-env)
(require 'jupyter-messages)
(require 'jupyter-client)
(require 'jupyter-kernelspec)
(require 'jupyter-channel)
(eval-when-compile (require 'subr-x))

(declare-function ansi-color-apply "ansi-color" (string))

(defgroup jupyter-kernel-manager nil
  "Jupyter kernel manager"
  :group 'jupyter)

;;; `jupyter-kernel'

(defclass jupyter--kernel ()
  ((spec
    :type jupyter-kernelspec
    :initarg :spec
    :documentation "The kernelspec for this kernel.
SPEC is in the same format as one of the elements returned by
`jupyter-find-kernelspecs'.")
   (session
    :type jupyter-session
    :initarg :session
    :documentation "The session used for communicating with the kernel."))
  :abstract t
  :documentation "Partial representation of a Jupyter kernel.

Contains the kernelspec associated with the kernel and the
`jupyter-session' object used for communicating with the kernel
when it is alive.

Sub-classes must call `cl-next-method-method' in their
implementation of `jupyter-kill-kernel'.

A convenience method, `jupyter-kernel-name', is provided to
access the name of the kernelspec.")

(cl-defmethod jupyter-kernel-alive-p (obj)
  "Return non-nil if KERNEL is alive."
  (if (and (eieio-object-p obj)
           (slot-exists-p obj 'kernel))
      (jupyter-kernel-alive-p (oref obj kernel))
    (jupyter-alive-p obj)))

(cl-defmethod jupyter-start-kernel (kernel &rest _args)
  "Start KERNEL."
  (jupyter-launch kernel))

(cl-defgeneric jupyter-kill-kernel (kernel)
  "Stop KERNEL."
  (jupyter-shutdown kernel))

(cl-defgeneric jupyter-kernel-died (kernel)
  "Called when a KERNEL dies unexpectedly.")

(cl-defmethod jupyter-start-kernel :around (kernel &rest _args)
  "Call the next method unless KERNEL is already alive."
  (unless (jupyter-kernel-alive-p kernel)
    (cl-call-next-method)))

(cl-defmethod jupyter-kill-kernel :around (kernel)
  "Call the next method only when KERNEL is alive."
  (when (jupyter-kernel-alive-p kernel)
    (cl-call-next-method)))

(cl-defmethod jupyter-kernel-died (_kernel)
  "Do nothing if KERNEL died."
  (ignore))

(cl-defmethod jupyter-kill-kernel (_kernel)
  (ignore))

(cl-defmethod jupyter-kernel-name ((kernel jupyter--kernel))
  "Return the name of KERNEL."
  (jupyter-kernelspec-name (jupyter-kernel-spec kernel)))

(cl-defmethod jupyter-kernel-name ((kernel jupyter-kernel))
  "Return the name of KERNEL."
  (jupyter-kernelspec-name (jupyter-kernel-spec kernel)))

;;; `jupyter-kernel-manager'

(defvar jupyter--kernel-managers nil)

(defclass jupyter-kernel-manager (jupyter-instance-tracker)
  ((tracking-symbol :initform 'jupyter--kernel-managers)
   (kernel
    :type jupyter-kernel
    :initarg :kernel
    :documentation "The kernel that is being managed."))
  :abstract t)

(defun jupyter-kernel-managers ()
  "Return a list of all `jupyter-kernel-manager' objects."
  (jupyter-all-objects 'jupyter--kernel-managers))

(cl-defgeneric jupyter-make-client ((manager jupyter-kernel-manager) class &rest slots)
  "Make a new client from CLASS connected to MANAGER's kernel.
SLOTS are the slots used to initialize the client with.")

(cl-defmethod jupyter-make-client :before (_manager class &rest _slots)
  "Signal an error if CLASS is not a subclass of `jupyter-kernel-client'."
  (unless (child-of-class-p class 'jupyter-kernel-client)
    (signal 'wrong-type-argument (list '(subclass jupyter-kernel-client) class))))

(cl-defmethod jupyter-make-client (manager class &rest _slots)
  "Return an instance of CLASS using SLOTS.
CLASS is a subclass of `jupyter-kernel-client'.  Return the
instance already connected to MANAGER's kernel."
  (let ((client (jupyter-client (oref manager kernel) class)))
    (oset client manager manager)
    client))

(cl-defmethod jupyter-start-kernel ((manager jupyter-kernel-manager) &rest _args)
  "Start MANAGER's kernel."
  (jupyter-launch (oref manager kernel)))

;; FIXME: Specify the arguments.  See the method implementation for
;; `jupyter-rest-api' why this isn't done.
(cl-defgeneric jupyter-shutdown-kernel ((manager jupyter-kernel-manager) &rest _args)
  "Shutdown MANAGER's kernel or restart it if RESTART is non-nil.
Wait until TIMEOUT before forcibly shutting down the kernel."
  (jupyter-shutdown (oref manager kernel)))

(cl-defgeneric jupyter-interrupt-kernel ((manager jupyter-kernel-manager) &rest _args)
  "Interrupt MANAGER's kernel."
  (jupyter-interrupt (oref manager kernel)))

(cl-defmethod jupyter-kernel-alive-p ((manager jupyter-kernel-manager))
  "Is MANGER's kernel alive?"
  (and (slot-boundp manager 'kernel)
       (jupyter-alive-p (oref manager kernel))))

(provide 'jupyter-kernel-manager)

;;; jupyter-kernel-manager.el ends here
