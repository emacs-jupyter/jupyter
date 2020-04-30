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

;; This file defines the `jupyter-connection' method which takes a
;; kernel and a client and returns an instance of the
;; `jupyter-connection' struct type, representing the set of IO
;; actions and state transformations required to establish
;; communication between a kernel and a client.

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

(cl-defmethod jupyter-start ((conn jupyter-connection) &optional channel)
  (funcall (jupyter-connection-start conn) channel))

(cl-defmethod jupyter-stop ((conn jupyter-connection) &optional channel)
  (funcall (jupyter-connection-stop conn) channel))

(defun jupyter-conn-id (conn)
  (funcall (jupyter-connection-id conn)))

(cl-defmethod jupyter-send ((conn jupyter-connection) &rest msg)
  (unless (funcall (jupyter-connection-alive-p conn))
    (jupyter-start conn))
  (apply (jupyter-connection-send conn) msg))

;;;; `jupyter-connection'

(cl-defgeneric jupyter-connection (kernel handler)
  "Return a `jupyter-connection' to KERNEL.
When messages are received from KERNEL they are passed to
HANDLER.")

(declare-function jupyter-kernel-process "ext:jupyter-kernel-process")

(cl-defmethod jupyter-connection ((kernel jupyter-kernel) (handler function))
  (cond
   ((and (jupyter-kernel-session kernel)
         (require 'jupyter-kernel-process nil t))
    (jupyter-connection
     (jupyter-kernel-process :session (jupyter-kernel-session kernel))
     handler))
   (t (cl-call-next-method))))

(defvar jupyter--kernel-connections (make-hash-table :weakness 'key))

(defun jupyter-kernel-connection (kernel)
  "Return the `jupyter-connection' associated with KERNEL."
  (or (gethash kernel jupyter--kernel-connections)
      (puthash
       kernel (jupyter-connection
               kernel
               (lambda (event)
                 (cl-loop
                  for client in (jupyter-kernel-clients kernel) do
                  (if (eq (car event) 'sent)
                      (when jupyter--debug
                        (jupyter--show-event event))
                    (cl-destructuring-bind (_ channel _idents . msg) event
                      (jupyter-handle-message client channel msg))))))
       jupyter--kernel-connections)))

;; TODO: Reason about any reference cycles I'm creating.  When will
;; the connection be garbage collected if the client holds a reference
;; to the connection?
(cl-defmethod jupyter-connection ((kernel jupyter-kernel) (client jupyter-kernel-client))
  (let ((kcomm (jupyter--kernel-connection kernel))
        (conn nil)
        (connect (lambda ()
                   (cl-pushnew client (jupyter-kernel-clients kernel))
                   (oset client conn conn)
                   ;; FIXME: This is here because `jupyter-widget-client' uses
                   ;; `jupyter-find-client-for-session'.
                   (oset client session (jupyter-kernel-session kernel))))
        (disconnect (lambda ()
                      (cl-callf2 delq client (jupyter-kernel-clients kernel))
                      (slot-makeunbound client 'conn)
                      (slot-makeunbound client 'session)))
        (alive-p (lambda (&rest _)
                   (and (jupyter-alive-p kcomm)
                        (slot-boundp client 'conn) (eq conn (oref client conn))
                        (memq client (jupyter-kernel-clients kernel))))))
    (setq conn
          (make-jupyter-connection
           :id (jupyter-connection-id kcomm)
           :hb (jupyter-connection-hb kcomm)
           :start
           (lambda (&rest _)
             (unless (jupyter-alive-p kernel)
               (jupyter-launch kernel))
             (unless (jupyter-alive-p kcomm)
               (jupyter-start kcomm))
             (funcall connect))
           ;; TODO: Re-implement comm-autostop
           :stop
           (lambda (&rest _)
             (funcall disconnect))
           :send (jupyter-connection-send kcomm)
           :alive-p alive-p))))
(cl-defmethod jupyter-connection ((client jupyter-kernel-client) (kernel jupyter-kernel))
  (jupyter-connection kernel client))

(provide 'jupyter-connection)

;;; jupyter-connection.el ends here
