;;; jupyter-connection.el --- Linking kernels and clients -*- lexical-binding: t -*-

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

;; Define the `jupyter-connect' function which does the work of
;; connecting to a kernel's channels in Emacs and ensuring
;; communication between a client and a kernel it would like to
;; connect to works.
;;
;; The auxiliary method `jupyter-connection' needs to be extended to
;; define how the above procedure works.

;;; Code:

(require 'jupyter-kernel)
(require 'jupyter-client)

(defgroup jupyter-connection nil
  "Linking kernels and clients"
  :group 'jupyter)

(defconst jupyter-connection-uninitialized
  (lambda (&rest _)
    (error "Connection not initialized"))
  "Default function used for some slots of a `jupyter-connection'.")

(cl-defstruct jupyter-connection
  "Represents an Emacs based connection to a kernel.
START, STOP, SEND, and ALIVE-P are one argument functions that
take a `jupyter-kernel' to: ensure that communication with a
kernel has started, stop any communication with a kernel, send a
message to a kernel, and determine if communication with a kernel
is possible."
  hb
  (id jupyter-connection-uninitialized
      :type function
      :read-only t)
  (start jupyter-connection-uninitialized
         :type function
         :read-only t)
  (stop jupyter-connection-uninitialized
        :type function
        :read-only t)
  (send jupyter-connection-uninitialized
        :type function
        :read-only t)
  (alive-p #'ignore
           :type function
           :read-only t))

(cl-defmethod jupyter-alive-p ((conn jupyter-connection) &optional channel)
  "Return non-nil if CONN is live."
  (funcall (jupyter-connection-alive-p conn) channel))

(defun jupyter-start (conn &optional channel)
  (funcall (jupyter-connection-start conn) channel))

(defun jupyter-stop (conn &optional channel)
  (funcall (jupyter-connection-stop conn) channel))

(defun jupyter-conn-id (conn)
  (funcall (jupyter-connection-id conn)))

(cl-defmethod jupyter-send ((conn jupyter-connection) &rest msg)
  (unless (funcall (jupyter-connection-alive-p conn))
    (jupyter-start conn))
  (apply (jupyter-connection-send conn) msg))

;;;; `jupyter-connection' and `jupyter-connect'

(cl-defgeneric jupyter-connection (kernel handler)
  "Return a `jupyter-connection' to KERNEL.
When messages are received from KERNEL they are passed to
HANDLER.")

(cl-defmethod jupyter-connection ((kernel jupyter-kernel) (handler function))
  (cond
   ((and (jupyter-kernel-session kernel)
         (require 'jupyter-kernel-process nil t))
    (jupyter-connection
     (jupyter-kernel-process :session (jupyter-kernel-session kernel))
     handler))
   (t (cl-call-next-method))))

(cl-defgeneric jupyter-connect (client kernel)
  "Connect CLIENT to KERNEL's channels, return CLIENT.

Once a client has been connected to a kernel, or a kernel to a
client, messages can be passed between the two.

If CLIENT is already connected to another kernel, it is
disconnected before connecting to KERNEL.")

(cl-defmethod jupyter-connect ((client jupyter-kernel-client) (kernel jupyter-kernel))
  (unless (jupyter-alive-p kernel)
    (jupyter-launch kernel))
  (when (null (jupyter-kernel-connection kernel))
    (setf (jupyter-kernel-connection kernel)
          (jupyter-connection
           kernel (lambda (event)
                    (cl-loop
                     for c in (jupyter-kernel-clients kernel)
                     do (if (eq (car event) 'sent)
                            (when jupyter--debug
                              (jupyter--show-event event))
                          (cl-destructuring-bind (_ channel _idents . msg) event
                            (jupyter-handle-message c channel msg))))))))
  (when (slot-boundp client 'kernel)
    (jupyter-disconnect client (oref client kernel)))
  (unless (jupyter-alive-p (jupyter-kernel-connection kernel))
    (jupyter-start (jupyter-kernel-connection kernel)))
  ;; Make the connection.  This involves setting the kernel slot of
  ;; CLIENT (which the functions `jupyter-(dis)?connect' are the sole
  ;; modifiers) to KERNEL and ensuring KERNEL calls CLIENT's message
  ;; handler, see the above function.
  (cl-pushnew client (jupyter-kernel-clients kernel))
  (oset client kernel kernel)
  ;; FIXME: This is here because `jupyter-widget-client' uses
  ;; `jupyter-find-client-for-session'.
  (oset client session (jupyter-kernel-session kernel))
  client)

(cl-defmethod jupyter-connect ((kernel jupyter-kernel) (client jupyter-kernel-client))
  (jupyter-connect client kernel))

;; TODO: Re-implement comm-autostop for a jupyter-kernel-process by extending this method
(cl-defgeneric jupyter-disconnect ((client jupyter-kernel-client) (kernel jupyter-kernel))
  "Disconnect CLIENT from KERNEL's channels.
CLIENT will no longer be able to communicate with KERNEL after it
has been disconnected."
  (when (jupyter-connected-p client kernel)
    (cl-callf2 delq client (jupyter-kernel-clients (oref client kernel)))
    (slot-makeunbound client 'kernel)
    (slot-makeunbound client 'session)))

(cl-defmethod jupyter-disconnect ((kernel jupyter-kernel) (client jupyter-kernel-client))
  (jupyter-disconnect client kernel))

(cl-defgeneric jupyter-connected-p ((kernel jupyter-kernel) (client jupyter-kernel-client))
  "Return non-nil if KERNEL and CLIENT are connected.
If they are connected, messages can be communicated between
them."
  (and (jupyter-alive-p kernel)
       (jupyter-alive-p (jupyter-kernel-connection kernel))
       (slot-boundp client 'kernel)
       (eq (oref client kernel) kernel)
       (progn
         (cl-assert (memq client (jupyter-kernel-clients kernel)))
         t)))

(cl-defmethod jupyter-connected-p ((client jupyter-kernel-client))
  (jupyter-connected-p (oref client kernel) client))

(cl-defmethod jupyter-connected-p ((client jupyter-kernel-client) (kernel jupyter-kernel))
  (jupyter-connected-p kernel client))

(provide 'jupyter-connection)

;;; jupyter-connection.el ends here
