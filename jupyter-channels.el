;;; jupyter-channels.el --- Jupyter channels -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 08 Jan 2018
;; Version: 0.8.0

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

;; Implements synchronous channel types. Each channel is essentially a wrapper
;; around a `zmq-socket' constrained to a socket type by the type of the
;; channel and with an associated `zmq-IDENTITY' obtained from the
;; `jupyter-session' that must be associated with the channel. A heartbeat
;; channel is distinct from the other channels in that it is implemented using
;; a timer which periodically pings the kernel depending on how its configured.
;; In order for communication to occur on the other channels, one of
;; `jupyter-send' or `jupyter-recv' must be called after starting the channel
;; with `jupyter-start-channel'.
;;
;; Also implemented is a `jupyter-comm-layer' using `jupyter-sync-channel' comm
;; objects (`jupyter-sync-channel-comm') defined in this file. It is more of a
;; reference implementation to show how it could be done and required that
;; Emacs was built with threading support enabled.

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'jupyter-base)
(require 'jupyter-comm-layer)
(require 'zmq)
(require 'ring)

(defgroup jupyter-channels nil
  "Jupyter channels"
  :group 'jupyter)

(defcustom jupyter-hb-max-failures 5
  "Number of heartbeat failures until the kernel is considered unreachable.
A ping is sent to the kernel on a heartbeat channel and waits
until `time-to-dead' seconds to see if the kernel sent a ping
back. If the kernel doesn't send a ping back after
`jupyter-hb-max-failures', the callback associated with the
heartbeat channel is called. See `jupyter-hb-on-kernel-dead'."
  :type 'integer
  :group 'jupyter-channels)

(defconst jupyter-socket-types
  (list :hb zmq-REQ
        :shell zmq-DEALER
        :iopub zmq-SUB
        :stdin zmq-DEALER
        :control zmq-DEALER)
  "The socket types for the various channels used by `jupyter'.")

;;; Basic channel types

(defclass jupyter-channel ()
  ((type
    :type keyword
    :initarg :type
    :documentation "The type of this channel. Should be one of
the keys in `jupyter-socket-types'.")
   (session
    :type jupyter-session
    :initarg :session
    :documentation "The session object used to sign and
send/receive messages.")
   (endpoint
    :type string
    :initarg :endpoint
    :documentation "The endpoint this channel is connected to.
 Typical endpoints look like \"tcp://127.0.0.1:5555\"."))
  :abstract t)

(defclass jupyter-sync-channel (jupyter-channel)
  ((socket
    :type (or null zmq-socket)
    :initform nil
    :documentation "The socket used for communicating with the kernel.")))

(cl-defgeneric jupyter-start-channel ((channel jupyter-channel) &key identity)
  "Start a Jupyter CHANNEL using IDENTITY as the routing ID.
If CHANNEL is already alive, do nothing.")

(defun jupyter-connect-endpoint (type endpoint &optional identity)
  "Create socket with TYPE and connect to ENDPOINT.
If IDENTITY is non-nil, it will be set as the ROUTING-ID of the
socket. Return the created socket."
  (let ((sock (zmq-socket (zmq-current-context) type)))
    (prog1 sock
      (zmq-socket-set sock zmq-LINGER 1000)
      (when identity
        (zmq-socket-set sock zmq-ROUTING-ID identity))
      (zmq-connect sock endpoint))))

(defun jupyter-connect-channel (ctype endpoint &optional identity)
  "Create a socket based on a Jupyter channel type.
CTYPE is one of the symbols `:hb', `:stdin', `:shell',
`:control', or `:iopub' and represents the type of channel to
connect to ENDPOINT. If IDENTITY is non-nil, it will be set as
the ROUTING-ID of the socket. Return the created socket."
  (let ((sock-type (plist-get jupyter-socket-types ctype)))
    (unless sock-type
      (error "Invalid channel type (%s)" ctype))
    (jupyter-connect-endpoint sock-type endpoint identity)))

(cl-defmethod jupyter-start-channel ((channel jupyter-sync-channel)
                                     &key (identity (jupyter-session-id
                                                     (oref channel session))))
  (unless (jupyter-channel-alive-p channel)
    (let ((socket (jupyter-connect-channel
                   (oref channel type) (oref channel endpoint) identity)))
      (oset channel socket socket)
      (cl-case (oref channel type)
        (:iopub
         (zmq-socket-set socket zmq-SUBSCRIBE ""))))))

(cl-defgeneric jupyter-stop-channel ((channel jupyter-channel))
  "Stop a Jupyter CHANNEL.
If CHANNEL is already stopped, do nothing.")

(cl-defmethod jupyter-stop-channel ((channel jupyter-sync-channel))
  (when (jupyter-channel-alive-p channel)
    (zmq-socket-set (oref channel socket) zmq-LINGER 0)
    (zmq-close (oref channel socket))
    (oset channel socket nil)))

(cl-defgeneric jupyter-channel-alive-p ((channel jupyter-channel))
  "Determine if a CHANNEL is alive.")

(cl-defmethod jupyter-channel-alive-p ((channel jupyter-sync-channel))
  (not (null (oref channel socket))))

;;; Sending/receiving

(cl-defmethod jupyter-send ((session jupyter-session)
                            socket
                            type
                            message
                            &optional
                            msg-id
                            flags)
  "For SESSION, send a message on SOCKET.
TYPE is message type of MESSAGE, one of the keys in
`jupyter-message-types'. MESSAGE is the message content.
Optionally supply a MSG-ID to the message, if this is nil a new
message ID will be generated. FLAGS has the same meaning as in
`zmq-send'. Return the message ID of the sent message."
  (declare (indent 1))
  (cl-destructuring-bind (id . msg)
      (jupyter-encode-message session type
        :msg-id msg-id :content message)
    (prog1 id
      (zmq-send-multipart socket msg flags))))

(cl-defmethod jupyter-recv ((session jupyter-session) socket &optional flags)
  "For SESSION, receive a message on SOCKET with FLAGS.
FLAGS is passed to SOCKET according to `zmq-recv'. Return a cons cell

    (IDENTS . MSG)

where IDENTS are the ZMQ identities associated with MSG and MSG
is the message property list whose fields can be accessed through
calls to `jupyter-message-content', `jupyter-message-parent-id',
and other such functions."
  (let ((msg (zmq-recv-multipart socket flags)))
    (when msg
      (cl-destructuring-bind (idents . parts)
          (jupyter--split-identities msg)
        (cons idents (jupyter-decode-message session parts))))))

(cl-defmethod jupyter-send ((channel jupyter-sync-channel) type message &optional msg-id)
  (jupyter-send (oref channel session) (oref channel socket) type message msg-id))

(cl-defmethod jupyter-recv ((channel jupyter-sync-channel))
  (jupyter-recv (oref channel session) (oref channel socket)))

;;; Heartbeat channel

(defclass jupyter-hb-channel (jupyter-sync-channel)
  ((type
    :type keyword
    :initform :hb
    :documentation "The type of this channel is `:hb'.")
   (time-to-dead
    :type number
    :initform 1
    :documentation "The time in seconds to wait for a response
from the kernel until the connection is assumed to be dead. Note
that this slot only takes effect when starting the channel.")
   (dead-cb
    :type function
    :initform #'ignore
    :documentation "A callback function that takes 0 arguments
and is called when the kernel has not responded for
\(* `jupyter-hb-max-failures' `time-to-dead'\) seconds.")
   (beating
    :type (or boolean symbol)
    :initform t
    :documentation "A flag variable indicating that the heartbeat
channel is communicating with the kernel.")
   (paused
    :type boolean
    :initform t
    :documentation "A flag variable indicating that the heartbeat
channel is paused and not communicating with the kernel. To
pause the heartbeat channel use `jupyter-hb-pause', to unpause
use `jupyter-hb-unpause'."))
  :documentation "A base class for heartbeat channels.")

(cl-defmethod jupyter-channel-alive-p ((channel jupyter-hb-channel))
  "Return non-nil if CHANNEL is alive."
  (zmq-socket-p (oref channel socket)))

(defun jupyter-hb--pingable-p (channel)
  (and (not (oref channel paused))
       (jupyter-channel-alive-p channel)))

(cl-defmethod jupyter-hb-beating-p ((channel jupyter-hb-channel))
  "Return non-nil if CHANNEL is reachable."
  (and (jupyter-hb--pingable-p channel)
       (oref channel beating)))

(cl-defmethod jupyter-hb-pause ((channel jupyter-hb-channel))
  "Pause checking for heartbeat events on CHANNEL."
  (oset channel paused t))

(cl-defmethod jupyter-hb-unpause ((channel jupyter-hb-channel))
  "Un-pause checking for heatbeat events on CHANNEL."
  (when (oref channel paused)
    (if (jupyter-channel-alive-p channel)
        ;; Consume a pending message from the kernel if there is one. We send a
        ;; ping and then schedule a timer which fires TIME-TO-DEAD seconds
        ;; later to receive the ping back from the kernel and start the process
        ;; all over again. If the channel is paused before TIME-TO-DEAD
        ;; seconds, there may still be a ping from the kernel waiting.
        (ignore-errors (zmq-recv (oref channel socket) zmq-DONTWAIT))
      (jupyter-start-channel channel))
    (oset channel paused nil)
    (jupyter-hb--send-ping channel)))

(cl-defmethod jupyter-hb-on-kernel-dead ((channel jupyter-hb-channel) fun)
  "When the kernel connected to CHANNEL dies, call FUN.
A kernel is considered dead when CHANNEL does not receive a
response after \(* `jupyter-hb-max-failures' `time-to-dead'\)
seconds has elapsed without the kernel sending a ping back."
  (declare (indent 1))
  (oset channel dead-cb fun))

(defun jupyter-hb--send-ping (channel &optional failed-count)
  (when (jupyter-hb--pingable-p channel)
    (condition-case nil
        (zmq-send (oref channel socket) "ping")
      ;; FIXME: Should be a part of `jupyter-hb--pingable-p'
      (zmq-ENOTSOCK
       (jupyter-hb-pause channel)
       (oset channel socket nil)))
    (run-with-timer
     (oref channel time-to-dead) nil
     (lambda (channel-ref)
       (when-let* ((channel (jupyter-weak-ref-resolve channel-ref))
                   (sock (and (jupyter-hb--pingable-p channel)
                              (oref channel socket))))
         (oset channel beating
               (condition-case nil
                   (and (zmq-recv sock zmq-DONTWAIT) t)
                 ((zmq-EINTR zmq-EAGAIN) nil)))
         (if (oref channel beating)
             (jupyter-hb--send-ping channel)
           ;; Reset the socket
           (jupyter-stop-channel channel)
           (jupyter-start-channel channel)
           (or failed-count (setq failed-count 0))
           (if (< failed-count jupyter-hb-max-failures)
               (jupyter-hb--send-ping channel (1+ failed-count))
             (oset channel paused t)
             (when (functionp (oref channel dead-cb))
               (funcall (oref channel dead-cb)))))))
     (jupyter-weak-ref channel))))

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

(cl-defmethod jupyter-comm-id ((comm jupyter-sync-channel-comm))
  (format "session=%s" (truncate-string-to-width
                        (jupyter-session-id (oref comm session))
                        9 nil nil "…")))

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

(provide 'jupyter-channels)

;;; jupyter-channels.el ends here
