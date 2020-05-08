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

;; This file defines the `jupyter-connect' function used to connect a
;; client to a kernel.  
;;
;;
;; Before a client and kernel can be connected, a connection to a
;; kernel must be made to get at the 

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

(defvar jupyter-connections (make-hash-table :weakness 'key)
  "A cache for Jupyter kernel connections.
The keys are objects like kernels, clients, and other auxiliary
objects.  The values are lists whose elements are related in some
way to connecting clients to kernels in the current Emacs
session.

This table is used by `jupyter-io' to store the connection lists
returned by `jupyter-connection'.  It is also used by
`jupyter-connect' to keep track of the kernel a client is
connected to.")

;; FIXME: Remove the channel argument
(cl-defmethod jupyter-alive-p ((io function) &optional _channel)
  "Return non-nil if CONN is live."
  (jupyter-send io 'alive-p))

(cl-defmethod jupyter-start ((io function) &optional _channel)
  (jupyter-send io 'start))

(cl-defmethod jupyter-stop ((io function) &optional _channel)
  (jupyter-send io 'stop))

;;;; `jupyter-connection'

(defmacro jupyter-run-handlers (place event)
  "Evaluate the list of functions in PLACE using ARGS.
Set PLACE to those functions that returned non-nil."
  `(setf ,place
         (let ((handlers ,place)
               (event ,event)
               (new-handlers '()))
           (while handlers
             (let ((h (pop handlers)))
               (when (funcall h event)
                 (push h new-handlers))))
           new-handlers)))

(cl-defgeneric jupyter-connection (thing)
  "Establish a connection to THING.
Return a list (IO ...), where IO is a function used to perform
I/O actions on THING, the rest of the list is ignored.

IO takes arguments like (TYPE ARGS...) where TYPE is a symbol
describing the type of I/O action/query that ARGS are parameters
to.

This method should not be called directly, use `jupyter-io'.

At a minimum, IO should accept to the following sets of I/O
actions/queries:

  * Connection lifetime

    - ('start) :: Enable I/O.
    - ('stop) :: Disable I/O.
    - ('alive-p &optional CHANNEL) :: Return non-nil if I/O
      connection is live.  If CHANNEL is specified and the I/O
      connection resolves individual channels, return non-nil if
      CHANNEL is live.

  * Message sending/receiving

    - ('send ARGS...) :: Send ARGS as a message to THING.

    - ('add-handler FN) :: Add FN, a function, as a handler of
      messages received from THING.

    - ('remove-handler FN) :: Remove FN as a handler.

    'send and 'add-handler are also responsible for enabling the
    connection if it has not been already.

  * Heartbeat channel [1]

    - ('hb) :: Return a `jupyter-hb-channel' or nil if the
      connection does not support one.

\[1] https://jupyter-client.readthedocs.io/en/stable/messaging.html#heartbeat-for-kernels")

(declare-function jupyter-kernel-process "ext:jupyter-kernel-process")

(cl-defmethod jupyter-connection ((kernel jupyter-kernel))
  "Return a default connection list for KERNEL."
  (list
   (lambda (&rest args)
     (pcase (car args)
       ((or 'start 'stop 'alive-p))
       ((or 'message 'add-handler 'remove-handler) (error "Not implemented"))
       ('hb nil)
       (_ (error "Unhandled IO: %s" args))))))

(defun jupyter-io (thing)
  "Return a function that can be used to perform I/O with THING.
The function takes arguments like (TYPE ARGS...) where TYPE is a
symbol describing the type of I/O action/query that ARGS are
parameters to.

See `jupyter-connection' for the I/O actions/queries THING implements."
  (pcase-let ((`(,io . ,_)
               (or (gethash thing jupyter-connections)
                   (puthash thing (jupyter-connection thing)
                            jupyter-connections))))
    io))

(cl-defmethod jupyter-send ((io function) &rest args)
  "Send ARGS on IO."
  (apply io args))

(cl-defmethod jupyter-send ((kernel jupyter-kernel) &rest args)
  "Send a message to KERNEL.
Any associated messages will be received by KERNEL's handlers.
See `jupyter-connection' for how a handler can be added."
  (apply (jupyter-io kernel) 'message args))

(cl-defmethod jupyter-connect ((client jupyter-kernel-client) (kernel jupyter-kernel))
  "Connect CLIENT to KERNEL.
When CLIENT has been connected to KERNEL, it handles messages
KERNEL sends to its clients.  Return an I/O function that can be
used to send messages to KERNEL.

The connection exists as long as CLIENT exists or until CLIENT is
connected to a different kernel.  The connection of a client can
be removed manually with `jupyter-disconnect'.

See `jupyter-connection' for more info. on the I/O function."
  (jupyter-disconnect client)
  ;; Make a weak ref. to CLIENT so that remhandler, below, will be
  ;; called if CLIENT is not disconnected before being garbage
  ;; collected.
  (let* ((ref (jupyter-weak-ref client))
         (handler (lambda (event)
                    ;; FIXME: Handle the `sent' event, but not here.
                    ;; How can I get rid of it?
                    (pcase (car event)
                      ((and 'message (let `(,channel . ,msg) (cdr event)))
                       (when-let* ((client (jupyter-weak-ref-resolve ref)))
                         (jupyter-handle-message client channel msg))))))
         (io (jupyter-io kernel)))
    (jupyter-send io 'add-handler handler)
    (puthash client
             (let ((remhandler
                    (lambda ()
                      (jupyter-send io 'remove-handler handler))))
               ;; A reference to KERNEL is kept to allow disconnecting
               ;; all clients of KERNEL in `jupyter-disconnect'.
               ;;
               ;; The finalizer is here to remove the handler if
               ;; client is not disconnected before being garbage
               ;; collected.
               (list kernel remhandler (make-finalizer remhandler)))
             jupyter-connections)
    io))

(cl-defmethod jupyter-disconnect ((client jupyter-kernel-client))
  "Disconnect CLIENT from its kernel, if any."
  (pcase (gethash client jupyter-connections)
    (`(,_ ,remhandler . ,_)
     (funcall remhandler)))
  (remhash client jupyter-connections))

(cl-defmethod jupyter-kernel ((client jupyter-kernel-client))
  "Return the kernel CLIENT is connected to.
Return nil if CLIENT is not connected to any kernel."
  (let ((kernel (car (gethash client jupyter-connections))))
    kernel))

(cl-defmethod jupyter-clients ((kernel jupyter-kernel))
  "Return a list of clients KERNEL is connected to."
  (let ((clients '()))
    (maphash (lambda (k v)
               (when (and (eieio-object-p k)
                          (object-of-class-p k 'jupyter-kernel-client)
                          (memq kernel v))
                 (push k clients)))
             jupyter-connections)
    clients))

(cl-defmethod jupyter-disconnect ((kernel jupyter-kernel))
  "Disconnect all clients of KERNEL."
  (cl-loop for client in (jupyter-clients kernel)
           do (jupyter-disconnect client)))

(cl-defmethod jupyter-shutdown :extra "IO" ((kernel jupyter-kernel))
  "Stop KERNEL's I/O connections."
  (jupyter-disconnect kernel)
  (jupyter-stop (jupyter-io kernel))
  (cl-call-next-method))

(provide 'jupyter-connection)

;;; jupyter-connection.el ends here
