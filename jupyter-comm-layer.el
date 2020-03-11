;;; jupyter-comm-layer.el --- Kernel communication layer -*- lexical-binding: t -*-

;; Copyright (C) 2019 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 06 Apr 2019

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

;; Communication with a kernel can happen in various ways, e.g. through zmq
;; sockets, a websocket, and potentially others.
;;
;; The purpose of this file is to implement a kernel communication layer to
;; abstract away how a client communicates with the kernel it is connected to.
;;
;; A specific kernel communication layer (kcomm for short) is implemented by
;; extending the methods: `jupyter-comm-start', `jupyter-comm-stop',
;; `jupyter-comm-alive-p',`jupyter-event-handler', `jupyter-send', and possibly
;; `jupyter-initialize-connection'.
;;
;; A client registers with the kcomm by calling `jupyter-connect-client' and
;; de-registers with `jupyter-disconnect-client'.  The communication layer deals
;; with "events" which are just lists with an identifying symbol as the head
;; element.  Events that occur on the communication layer meant for clients,
;; e.g. a message received by a kernel or notification that a message was sent
;; to a kernel, will be broadcast to all registered clients.  Every client
;; wanting to receive such events must extend the method
;; `jupyter-event-handler' using the head method specializer.
;;
;; An event is sent to the kernel using `jupyter-send'.  So that sending an
;; event to the communication layer would look like
;;
;;     (jupyter-send kcomm 'send channel-type msg-type msg msg-id)
;;
;; The possible events that can be handled by a client is dependent on the
;; communication layer, but a `jupyter-kernel-client' implements handlers for a
;; `message' event (a kernel message) and a `sent' event (a notification that a
;; message was sent to a kernel).

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'jupyter-base)
(require 'jupyter-messages)

(defgroup jupyter-comm-layer nil
  "Kernel communication layer"
  :group 'jupyter)

(defclass jupyter-comm-layer ()
  ((clients :type list :initform nil))
  :abstract t)

(defmacro jupyter-comm-client-loop (comm client &rest body)
  "Loop over COMM's clients, binding each to CLIENT before evaluating BODY."
  (declare (indent 2))
  (let ((clients (make-symbol "clients")))
    `(let ((,clients (oref ,comm clients)))
       (while ,clients
         (when-let* ((,client (jupyter-weak-ref-resolve (pop ,clients))))
           ,@body)))))

;;; `jupyter-comm-layer'

(cl-defgeneric jupyter-comm-start ((comm jupyter-comm-layer) &rest _ignore)
  "Start communication on COMM.")

(cl-defgeneric jupyter-comm-stop ((comm jupyter-comm-layer) &rest _ignore)
  "Stop communication on COMM.")

(cl-defgeneric jupyter-comm-alive-p ((comm jupyter-comm-layer))
  "Return non-nil if communication has started on COMM.")

(cl-defgeneric jupyter-connect-client ((comm jupyter-comm-layer) obj)
  "Register OBJ to receive events from COMM.
By default, on the first OBJ connected, `jupyter-comm-start' is
called if needed.  This means that a call to
`jupyter-initialize-connection' should precede a call to
`jupyter-connect-client'.")

(cl-defgeneric jupyter-disconnect-client ((comm jupyter-comm-layer) obj)
  "De-register OBJ from receiving events from COMM.
By default, on the last OBJ removed, `jupyter-comm-stop' is
called if needed.")

(cl-defgeneric jupyter-comm-id ((comm jupyter-comm-layer))
  "Return an identification string for COMM.
Can be used to identify this communication channel.  For example,
used in `jupyter-repl-scratch-buffer' to name the scratch
buffer.")

(cl-defgeneric jupyter-event-handler (_obj _event)
  "Handle EVENT using OBJ."
  nil)

(cl-defmethod jupyter-send ((_comm jupyter-comm-layer) &rest _event)
  "Send EVENT to the underlying kernel using COMM."
  (error "Subclasses need to override this method"))

(cl-defgeneric jupyter-initialize-connection ((comm jupyter-comm-layer) &rest _ignore)
  "Initialize communication on COMM.")

(cl-defmethod jupyter-initialize-connection ((comm jupyter-comm-layer) &rest _ignore)
  "Raise an error if COMM is already alive."
  (when (jupyter-comm-alive-p comm)
    (error "Can't initialize a live comm")))

;; TODO: Figure out a better interface for these channel methods or just make
;; them unnecessary.  The design of `jupyter-comm-layer' only deals with
;; "events" and the channel abstraction is an implementation detail that
;; shouldn't be visible to the client.

(cl-defgeneric jupyter-channels-running-p ((comm jupyter-comm-layer))
  "Are any channels of CLIENT running?")

(cl-defmethod jupyter-channel-alive-p ((_comm jupyter-comm-layer) _channel)
  (error "Need to implement"))

(cl-defmethod jupyter-connect-client ((comm jupyter-comm-layer) obj)
  (unless (cl-loop for ref in (oref comm clients)
                   thereis (eq (jupyter-weak-ref-resolve ref) obj))
    (push (jupyter-weak-ref obj) (oref comm clients)))
  ;; Remove any garbage collected clients
  (oset comm clients
        (cl-remove-if-not #'jupyter-weak-ref-resolve
                          (oref comm clients)))
  (unless (jupyter-comm-alive-p comm)
    (jupyter-comm-start comm)))

(cl-defmethod jupyter-disconnect-client ((comm jupyter-comm-layer) obj)
  (oset comm clients
        (cl-remove-if (lambda (ref)
                        (let ((deref (jupyter-weak-ref-resolve ref)))
                          (or (eq deref obj) (null deref))))
                      (oref comm clients))))

(cl-defmethod jupyter-event-handler ((comm jupyter-comm-layer) event)
  "Broadcast EVENT to all clients registered to receive them on COMM."
  ;; TODO: Dynamically cleanup list of garbage collected clients when looping
  ;; over it.
  (jupyter-comm-client-loop comm client
    (run-at-time 0 nil #'jupyter-event-handler client event)))

;;; `jupyter-comm-autostop'

(defclass jupyter-comm-autostop ()
  ()
  :abstract t
  :documentation "Stop the comm when the last client disconnects.")

(cl-defmethod jupyter-disconnect-client :after ((comm jupyter-comm-autostop) _client)
  "Stop COMM when there are no clients."
  (when (and (jupyter-comm-alive-p comm)
             (zerop (length (oref comm clients))))
    (jupyter-comm-stop comm)))

;;; `jupyter-hb-comm'
;; If the communication layer can talk to a heartbeat channel, then it should
;; add this class as a parent class.

(defclass jupyter-hb-comm ()
  ((hb :type jupyter-hb-channel))
  :abstract t)

(cl-defmethod jupyter-hb-beating-p ((comm jupyter-hb-comm))
  (jupyter-hb-beating-p (oref comm hb)))

(cl-defmethod jupyter-hb-pause ((comm jupyter-hb-comm))
  (jupyter-hb-pause (oref comm hb)))

(cl-defmethod jupyter-hb-unpause ((comm jupyter-hb-comm))
  (jupyter-hb-unpause (oref comm hb)))

(provide 'jupyter-comm-layer)

;;; jupyter-comm-layer.el ends here
