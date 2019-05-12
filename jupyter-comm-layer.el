;;; jupyter-comm-layer.el --- Kernel communication layer -*- lexical-binding: t -*-

;; Copyright (C) 2019 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 06 Apr 2019
;; Version: 0.8.0

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
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
;; de-registers with `jupyter-disconnect-client'. The communication layer deals
;; with "events" which are just lists with an identifying symbol as the head
;; element. Events that occur on the communication layer meant for clients,
;; e.g. a message received by a kernel or notification that a message was sent
;; to a kernel, will be broadcast to all registered clients. Every client
;; wanting to receive such events must extend the method
;; `jupyter-event-handler' using the head method specializer.
;;
;; An event is sent to the kernel using `jupyter-send'. So that sending an
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
(require 'jupyter-channel-ioloop)
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
called if needed. This means that a call to
`jupyter-initialize-connection' should precede a call to
`jupyter-connect-client'.")

(cl-defgeneric jupyter-disconnect-client ((comm jupyter-comm-layer) obj)
  "De-register OBJ from receiving events from COMM.
By default, on the last OBJ removed, `jupyter-comm-stop' is
called if needed.")

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
;; them unnecessary. The design of `jupyter-comm-layer' only deals with
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

;;; `jupyter-channel-comm'
;; A communication layer using `jupyter-sync-channel' objects for communicating
;; with a kernel. This communication layer is mainly meant for speed comparison
;; with the `jupyter-channel-ioloop-comm' layer. It implements communication in
;; the current Emacs instance and comparing it with the
;; `jupyter-channel-ioloop-comm' shows how much of a slow down there is when
;; all the processing of messages happens in the current Emacs instance.
;;
;; Running the test suit using `jupyter-channel-comm' vs
;; `jupyter-channel-ioloop-comm' shows, very roughly, around a 2x speed up
;; using `jupyter-channel-ioloop-comm'.

;; FIXME: This is needed since the `jupyter-kernel-client' and
;; `jupyter-channel-ioloop' use keywords whereas you can only access slots
;; using symbols.
(defsubst jupyter-comm--channel (c)
  (cl-case c
    (:hb 'hb)
    (:iopub 'iopub)
    (:shell 'shell)
    (:stdin 'stdin)))

(defclass jupyter-sync-channel-comm (jupyter-comm-layer
                                     jupyter-hb-comm
                                     jupyter-comm-autostop)
  ((session :type jupyter-session)
   (iopub :type jupyter-sync-channel)
   (shell :type jupyter-sync-channel)
   (stdin :type jupyter-sync-channel)
   (thread)))

(cl-defmethod initialize-instance ((_comm jupyter-sync-channel-comm) &optional _slots)
  (unless (functionp 'make-thread)
    (error "Need threading support"))
  (cl-call-next-method))

(defun jupyter-sync-channel-comm--check (comm)
  (condition-case err
      (cl-loop
       for channel-type in '(:iopub :shell :stdin)
       for channel = (slot-value comm (jupyter-comm--channel channel-type))
       for msg = (and (jupyter-channel-alive-p channel)
                      (with-slots (session socket) channel
                        (condition-case nil
                            (jupyter-recv session socket zmq-DONTWAIT)
                          ((zmq-EINTR zmq-EAGAIN) nil))))
       when msg do (jupyter-event-handler
                    comm (cl-list* 'message channel-type msg)))
    (error
     (thread-signal (car (all-threads)) (car err)
                    (cons 'jupyter-sync-channel-comm--check (cdr err)))
     (signal (car err) (cdr err)))))

(cl-defmethod jupyter-comm-start ((comm jupyter-sync-channel-comm))
  (cl-loop
   for channel in '(hb shell iopub stdin)
   do (jupyter-start-channel (slot-value comm channel)))
  (oset comm thread
        (make-thread
         (let ((comm-ref (jupyter-weak-ref comm)))
           (lambda ()
             (while (when-let* ((comm (jupyter-weak-ref-resolve comm-ref)))
                      (prog1 comm
                        (jupyter-sync-channel-comm--check comm)))
               (thread-yield)
               (thread-yield)))))))

(cl-defmethod jupyter-comm-stop ((comm jupyter-sync-channel-comm))
  (when (and (slot-boundp comm 'thread)
             (thread-alive-p (oref comm thread)))
    (thread-signal (oref comm thread) 'quit nil)
    (slot-makeunbound comm 'thread))
  (cl-loop
   for channel in '(hb shell iopub stdin)
   do (jupyter-stop-channel (slot-value comm channel))))

(cl-defmethod jupyter-comm-alive-p ((comm jupyter-sync-channel-comm))
  (jupyter-channels-running-p comm))

(cl-defmethod jupyter-channel-alive-p ((comm jupyter-sync-channel-comm) channel)
  (and (slot-boundp comm (jupyter-comm--channel channel))
       (jupyter-channel-alive-p (slot-value comm (jupyter-comm--channel channel)))))

(cl-defmethod jupyter-channels-running-p ((comm jupyter-sync-channel-comm))
  (cl-loop
   for channel in '(:shell :iopub :stdin :hb)
   thereis (jupyter-channel-alive-p comm channel)))

;;;; Channel start/stop methods

(cl-defmethod jupyter-stop-channel ((comm jupyter-sync-channel-comm) channel)
  (when (jupyter-channel-alive-p comm channel)
    (jupyter-stop-channel
     (slot-value comm (jupyter-comm--channel channel)))))

(cl-defmethod jupyter-start-channel ((comm jupyter-sync-channel-comm) channel)
  (unless (jupyter-channel-alive-p comm channel)
    (jupyter-start-channel
     (slot-value comm (jupyter-comm--channel channel)))))

(cl-defmethod jupyter-initialize-connection ((comm jupyter-sync-channel-comm)
                                             (session jupyter-session))
  (cl-call-next-method)
  (let ((endpoints (jupyter-session-endpoints session)))
    (oset comm session (copy-sequence session))
    (oset comm hb (make-instance
                   'jupyter-hb-channel
                   :session (oref comm session)
                   :endpoint (plist-get endpoints :hb)))
    (cl-loop
     for channel in `(:stdin :shell :iopub)
     do (setf (slot-value comm (jupyter-comm--channel channel))
              (jupyter-sync-channel
               :type channel
               :session (oref comm session)
               :endpoint (plist-get endpoints channel))))))

(cl-defmethod jupyter-send ((comm jupyter-sync-channel-comm)
                            _ channel-type msg-type msg msg-id)
  (let ((channel (slot-value comm (jupyter-comm--channel channel-type))))
    ;; Run the event handler on the next command loop since the expectation is
    ;; the client is that sending is asynchronous. There may be some code that
    ;; makes assumptions based on this.
    (run-at-time
     0 nil (lambda (id)
             (jupyter-event-handler comm (list 'sent channel-type id)))
     (jupyter-send channel msg-type msg msg-id))))

;;; `jupyter-ioloop-comm'

(defclass jupyter-ioloop-comm (jupyter-comm-layer)
  ((ioloop :type jupyter-ioloop))
  :abstract t)

;; Fall back method that catches IOLoop events that have not been handled by
;; the communication layer already.
(cl-defmethod jupyter-ioloop-handler ((_ioloop jupyter-ioloop)
                                      (comm jupyter-ioloop-comm)
                                      event)
  (unless (memq (car event) '(start quit))
    (jupyter-event-handler comm event)))

(cl-defmethod jupyter-send ((comm jupyter-ioloop-comm) &rest event)
  (apply #'jupyter-send (oref comm ioloop) event))

(cl-defmethod jupyter-comm-start ((comm jupyter-ioloop-comm))
  (with-slots (ioloop) comm
    (unless (jupyter-ioloop-alive-p ioloop)
      (jupyter-ioloop-start ioloop comm))))

(cl-defmethod jupyter-comm-stop ((comm jupyter-ioloop-comm))
  (with-slots (ioloop) comm
    (when (jupyter-ioloop-alive-p ioloop)
      (jupyter-ioloop-stop ioloop))))

(cl-defmethod jupyter-comm-alive-p ((comm jupyter-ioloop-comm))
  (and (slot-boundp comm 'ioloop)
       (jupyter-ioloop-alive-p (oref comm ioloop))))

;;; `jupyter-channel-ioloop-comm'

(cl-defstruct jupyter-proxy-channel endpoint alive-p)

(defclass jupyter-channel-ioloop-comm (jupyter-ioloop-comm
                                       jupyter-hb-comm
                                       jupyter-comm-autostop)
  ((session :type jupyter-session)
   (iopub :type jupyter-proxy-channel)
   (shell :type jupyter-proxy-channel)
   (stdin :type jupyter-proxy-channel)))

(cl-defmethod initialize-instance ((comm jupyter-channel-ioloop-comm) &optional _slots)
  (cl-call-next-method)
  (oset comm ioloop (jupyter-channel-ioloop)))

(cl-defmethod jupyter-initialize-connection ((comm jupyter-channel-ioloop-comm)
                                             (session jupyter-session))
  (cl-call-next-method)
  (let ((endpoints (jupyter-session-endpoints session)))
    (oset comm session (copy-sequence session))
    (oset comm hb (make-instance
                   'jupyter-hb-channel
                   :session (oref comm session)
                   :endpoint (plist-get endpoints :hb)))
    (cl-loop
     for channel in '(:stdin :shell :iopub)
     do (setf (slot-value comm (jupyter-comm--channel channel))
              (make-jupyter-proxy-channel
               :endpoint (plist-get endpoints channel)
               :alive-p nil)))))

(cl-defmethod jupyter-comm-start ((comm jupyter-channel-ioloop-comm))
  (with-slots (ioloop session) comm
    (unless (jupyter-ioloop-alive-p ioloop)
      (jupyter-ioloop-start ioloop session comm))
    (cl-loop
     for channel in '(:hb :shell :iopub :stdin)
     do (jupyter-start-channel comm channel))))

(cl-defmethod jupyter-comm-stop ((comm jupyter-channel-ioloop-comm))
  (cl-loop
   for channel in '(:hb :shell :iopub :stdin)
   do (jupyter-stop-channel comm channel))
  (cl-call-next-method))

;;;; `jupyter-channel-ioloop' events

(cl-defmethod jupyter-ioloop-handler ((_ioloop jupyter-channel-ioloop)
                                      (comm jupyter-channel-ioloop-comm)
                                      (event (head stop-channel)))
  (setf (jupyter-proxy-channel-alive-p
         (slot-value comm (jupyter-comm--channel (cadr event))))
        nil))

(cl-defmethod jupyter-ioloop-handler ((_ioloop jupyter-channel-ioloop)
                                      (comm jupyter-channel-ioloop-comm)
                                      (event (head start-channel)))
  (setf (jupyter-proxy-channel-alive-p
         (slot-value comm (jupyter-comm--channel (cadr event))))
        t))

;;;; Channel querying methods

(cl-defmethod jupyter-channel-alive-p ((comm jupyter-channel-ioloop-comm) channel)
  (if (eq channel :hb) (jupyter-channel-alive-p (oref comm hb))
    (with-slots (ioloop) comm
      (and ioloop (jupyter-ioloop-alive-p ioloop)
           (jupyter-proxy-channel-alive-p
            (slot-value comm (jupyter-comm--channel channel)))))))

(cl-defmethod jupyter-channels-running-p ((comm jupyter-channel-ioloop-comm))
  "Are any channels of CLIENT running?"
  (cl-loop
   for channel in '(:shell :iopub :stdin :hb)
   thereis (jupyter-channel-alive-p comm channel)))

;;;; Channel start/stop methods

(cl-defmethod jupyter-stop-channel ((comm jupyter-channel-ioloop-comm) channel)
  (when (jupyter-channel-alive-p comm channel)
    (if (eq channel :hb) (jupyter-stop-channel (oref comm hb))
      (with-slots (ioloop) comm
        (jupyter-send ioloop 'stop-channel channel)
        ;; Verify that the channel stops
        (or (jupyter-ioloop-wait-until ioloop 'stop-channel
              (lambda (_) (not (jupyter-channel-alive-p comm channel))))
            (error "Channel not stopped in ioloop subprocess (%s)" channel))))))

(cl-defmethod jupyter-start-channel ((comm jupyter-channel-ioloop-comm) channel)
  (unless (jupyter-channel-alive-p comm channel)
    (if (eq channel :hb) (jupyter-start-channel (oref comm hb))
      (let ((endpoint (jupyter-proxy-channel-endpoint
                       (slot-value comm (jupyter-comm--channel channel)))))
        (with-slots (ioloop) comm
          (jupyter-send ioloop 'start-channel channel endpoint)
          ;; Verify that the channel starts
          (or (jupyter-ioloop-wait-until ioloop 'start-channel
                (lambda (_) (jupyter-channel-alive-p comm channel)))
              (error "Channel not started in ioloop subprocess (%s)" channel)))))))

(provide 'jupyter-comm-layer)

;;; jupyter-comm-layer.el ends here
