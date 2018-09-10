;;; jupyter-channels.el --- Jupyter channels -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 08 Jan 2018
;; Version: 0.0.1

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

;;; Code:

(require 'jupyter-base)
(require 'ring)

(defgroup jupyter-channels nil
  "Jupyter channels"
  :group 'jupyter)

;;; Basic channel types

(defclass jupyter-channel ()
  ((type
    :type keyword
    :initarg :type
    :documentation "The type of this channel. Should be one of
 the keys in `jupyter-channel-socket-types'.")
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

(defclass jupyter-async-channel (jupyter-channel)
  ((ioloop
    :type (or null process)
    :initform nil
    :documentation "The process responsible for sending and
receiving messages on this channel.")
   (recv-queue
    :type ring
    :initform (make-ring 10))
   (status
    :type symbol
    :initform 'stopped)))

(cl-defgeneric jupyter-start-channel ((channel jupyter-channel) &key identity)
  "Start a Jupyter CHANNEL using IDENTITY as the routing ID.")

(cl-defmethod jupyter-start-channel ((channel jupyter-async-channel)
                                     &key (identity (jupyter-session-id
                                                     (oref channel session))))
  ;; TODO: Define a mechanism to attach a callback for each type of command in
  ;; an IOLoop so that the IOLoop filter is not responsible for setting the
  ;; status slot of a channel.
  (unless (jupyter-channel-alive-p channel)
    (zmq-subprocess-send (oref channel ioloop)
      (list 'start-channel (oref channel type) (oref channel endpoint) identity))
    (with-timeout (0.5 (error "Channel not started in ioloop subprocess"))
      (while (not (jupyter-channel-alive-p channel))
        (accept-process-output (oref channel ioloop) 0.1 nil 0)))))

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
  "Stop a Jupyter CHANNEL.")

(cl-defmethod jupyter-stop-channel ((channel jupyter-sync-channel))
  (when (jupyter-channel-alive-p channel)
    (zmq-socket-set (oref channel socket) zmq-LINGER 0)
    (zmq-close (oref channel socket))
    (oset channel socket nil)))

(cl-defmethod jupyter-stop-channel ((channel jupyter-async-channel))
  (when (jupyter-channel-alive-p channel)
    (zmq-subprocess-send (oref channel ioloop)
      (list 'stop-channel (oref channel type)))
    (with-timeout (0.5 (warn "Channel not stopped in ioloop subprocess"))
      (while (jupyter-channel-alive-p channel)
        (accept-process-output (oref channel ioloop) 0.1 nil 0)))))

(cl-defgeneric jupyter-get-message ((channel jupyter-channel) &rest _args)
  "Receive a message on CHANNEL.")

(cl-defmethod jupyter-get-message ((channel jupyter-sync-channel))
  "Block until a message is received on CHANNEL.
Return the received message."
  (cl-destructuring-bind (_idents . msg)
      (jupyter-recv channel)
    msg))

(cl-defmethod jupyter-get-message ((channel jupyter-async-channel) &optional timeout)
  "Get a message from CHANNEL's recv-queue.
If no message is available, return nil. Otherwise return the
oldest message in CHANNEL's recv-queue. If TIMEOUT is non-nil,
wait until TIMEOUT for a message."
  (let ((idents-msg (jupyter-recv channel timeout)))
    (when idents-msg
      (cl-destructuring-bind (_idents . msg)
          idents-msg
        msg))))

(cl-defmethod jupyter-send ((channel jupyter-async-channel) type message &optional msg-id)
  (zmq-subprocess-send (oref channel ioloop)
    (list 'send (oref channel type) type message msg-id)))

(cl-defmethod jupyter-send ((channel jupyter-sync-channel) type message &optional msg-id)
  (jupyter-send (oref channel session) (oref channel socket) type message msg-id))

(cl-defmethod jupyter-recv ((channel jupyter-sync-channel))
  (jupyter-recv (oref channel session) (oref channel socket)))

(cl-defmethod jupyter-recv ((channel jupyter-async-channel) &optional timeout)
  (let ((ring (oref channel recv-queue)))
    (when timeout
      (with-timeout (timeout
                     (error "Message not received on channel within timeout"))
        (while (ring-empty-p ring)
          (sleep-for 0.01))))
    (unless (ring-empty-p ring)
      (ring-remove ring))))

(cl-defgeneric jupyter-queue-message ((channel jupyter-async-channel) msg)
  "Queue MSG in CHANNEL's recv-queue.
MSG is a cons pair (IDENTS . MSG) which will be added to the
recv-queue slot of CHANNEL. To receive a message from the channel
call `jupyter-get-message'.")

(cl-defmethod jupyter-queue-message ((channel jupyter-async-channel) msg)
  "Queue MSG in CHANNEL's recv-queue."
  (let ((ring (oref channel recv-queue)))
    (ring-insert+extend ring msg 'grow)))

(cl-defgeneric jupyter-channel-alive-p ((channel jupyter-channel))
  "Determine if a CHANNEL is alive.")

(cl-defmethod jupyter-channel-alive-p ((channel jupyter-sync-channel))
  (not (null (oref channel socket))))

(cl-defmethod jupyter-channel-alive-p ((channel jupyter-async-channel))
  (and (process-live-p (oref channel ioloop))
       (not (eq (oref channel status) 'stopped))))

;;; Heartbeat channel

(defclass jupyter-hb-channel (jupyter-sync-channel)
  ((type
    :type keyword
    :initform :hb
    :documentation "The type of this channel is `:hb'.")
   (time-to-dead
    :type integer
    :initform 1
    :documentation "The time in seconds to wait for a response
from the kernel until the connection is assumed to be dead. Note
that this slot only takes effect when starting the channel.")
   (kernel-died-cb
    :type function
    :initform #'ignore
    :documentation "A callback function that takes 0 arguments
and is called when the kernel has not responded for 5
`time-to-dead' periods.")
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

(cl-defmethod jupyter-hb-beating-p ((channel jupyter-hb-channel))
  "Return non-nil if CHANNEL is reachable."
  (and (jupyter-channel-alive-p channel)
       (not (oref channel paused))
       (oref channel beating)))

(cl-defmethod jupyter-hb-pause ((channel jupyter-hb-channel))
  "Pause checking for heartbeat events on CHANNEL."
  (oset channel paused t))

(cl-defmethod jupyter-hb-unpause ((channel jupyter-hb-channel))
  "Unpause checking for heatbeat events on CHANNEL."
  (when (oref channel paused)
    (if (zmq-socket-p (oref channel socket))
        (jupyter-hb--send-ping channel)
      (jupyter-start-channel channel))))

(cl-defmethod jupyter-stop-channel ((channel jupyter-hb-channel))
  "Stop the heartbeat CHANNEL.
Stop the timer of the heartbeat channel."
  (when (jupyter-channel-alive-p channel)
    (oset channel paused t)
    (zmq-close (oref channel socket))
    (oset channel socket nil)))

(defun jupyter-hb-on-kernel-dead (hb-channel fun)
  "When the kernel connected to HB-CHANNEL dies call FUN.
A kernel is considered dead when the HB-CHANNEL does not receive
a response after 5 `time-to-dead' periods."
  (declare (indent 1))
  (cl-check-type hb-channel jupyter-hb-channel)
  (oset hb-channel kernel-died-cb fun))

(defun jupyter-hb--send-ping (channel &optional counter)
  (unless (oref channel paused)
    (let ((sock (oref channel socket)))
      (when (zmq-socket-p sock)
        (zmq-send sock "ping")
        (run-with-timer
         (oref channel time-to-dead) nil
         (lambda ()
           (unless (oset channel beating
                         (condition-case nil
                             (and (zmq-recv sock zmq-NOBLOCK) t)
                           ((zmq-EINTR zmq-EAGAIN) nil)))
             (let ((identity (zmq-socket-get sock zmq-IDENTITY)))
               (zmq-close sock)
               (oset channel socket
                     (jupyter-connect-channel
                      :hb (oref channel endpoint) identity)))
             (when (and (integerp counter) (> counter 5))
               (oset channel paused t)
               (when (oref channel kernel-died-cb)
                 (funcall (oref channel kernel-died-cb)))))
           (jupyter-hb--send-ping
            channel
            (unless (oref channel beating)
              (or (integerp counter) (setq counter 0))
              (1+ counter)))))))))

(cl-defmethod jupyter-start-channel ((channel jupyter-hb-channel) &key identity)
  "Start a heartbeat CHANNEL.
IDENTITY has the same meaning as in `jupyter-connect-channel'. A
heartbeat channel is handled specially in that it is implemented
with a timer in the current Emacs session. Starting a heartbeat
channel, starts the timer."
  (unless (jupyter-channel-alive-p channel)
    (oset channel socket (jupyter-connect-channel
                          :hb (oref channel endpoint) identity))
    (jupyter-hb--send-ping channel)))

(provide 'jupyter-channels)

;;; jupyter-channels.el ends here
