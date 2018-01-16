;;; jupyter-channels.el --- Jupyter channels -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 08 Jan 2018
;; Version: 0.0.1
;; X-URL: https://github.com/nathan/jupyter-channels

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

;; TODO: The `jupyter-channel' methods need work. `jupyter-kernel-client'
;; actually only uses a `jupyter-channel' to store received messages in the
;; recv-queue slot, to get the endpoint information for sockets created in a
;; client's ioloop subprocess, and to dispatch to message handlers using
;; `jupyter-handle-message'.
;;
;; The start and stop channel methods actually start and stop a channel's
;; socket in the current Emacs instance. What they should do is start and stop
;; a channel in a client's ioloop subprocess. A client's ioloop is available to
;; a channel since any channels initialized through
;; `jupyter-initialize-connection' have their parent-instance slot (from
;; `jupyter-connection') set to the client. So what can be done in the start
;; and stop methods is to check to see if the parent-instance slot is a
;; `jupyter-kernel-client' and if so, send its ioloop a command using
;; `zmq-subprocess-send'.
;;
;; TODO: `jupyter-channel' classes might not even need to be implemented in
;; reality. You could just as easily implement functions called on a client to
;; implement channels. Then the client can hold the recv-queue for each channel
;; and any channel information. This would even be better because then
;; internally to the client you can distinguish between a blocking client and
;; on that uses the ioloop subprocess. If the ioloop subprocess is nil, then
;; the client is blocking.
;;
;; You can do something like
;;
;;    (jupyter-get-message client :iopub)
;;
;; To get a message from the IOPub recv-queue or directly from a `jupyter-recv'
;; call based on if the client is blocking or not.

;;; Code:

(require 'jupyter-connection)
(require 'ring)

(defgroup jupyter-channels nil
  "Jupyter channels"
  :group 'communication)

;;; Basic channel types

(defclass jupyter-channel (jupyter-connection)
  ((type
    :type keyword
    :initarg :type
    :documentation "The type of this channel. Should be one of
 the keys in `jupyter-channel-socket-types', excluding `:hb'
 which corresponds to the heartbeat channel and is handled
 differently than the other channels. See `jupyter-hb-channel'.")
   (endpoint
    :type string
    :initarg :endpoint
    :documentation "The endpoint this channel is connected to.
 Typical endpoints look like \"tcp://127.0.0.1:5555\".")
   (socket
    :type (or null zmq-socket)
    :initform nil
    :documentation "The socket this channel uses to communicate
 with the kernel.")
   (recv-queue
    :type ring
    :initform (make-ring 10)
    :documentation "A queue of messages received on this channel
 that are waiting to be processed."))
  :abstract t
  :documentation "A base class for channels used by `jupyter'.")

(defclass jupyter-iopub-channel (jupyter-channel)
  ((type :initform :iopub))
  :documentation "A base class for iopub channels.")

(defclass jupyter-stdin-channel (jupyter-channel)
  ((type :initform :stdin))
  :documentation "A base class for stdin channels.")

(defclass jupyter-shell-channel (jupyter-channel)
  ((type :initform :shell))
  :documentation "A base class for shell channels.")

(defclass jupyter-control-channel (jupyter-channel)
  ((type :initform :control))
  :documentation "A base class for control channels.")

(cl-defmethod jupyter-start-channel ((channel jupyter-channel) &key identity)
  "Start a CHANNEL.
If IDENTITY is non-nil, it is used as the ROUTING_ID of the
underlying channel's socket."
  (unless (jupyter-channel-alive-p channel)
    (let ((sock (jupyter-connect-channel
                 (oref channel type) (oref channel endpoint) identity)))
      (oset channel socket sock))))

(cl-defmethod jupyter-start-channel ((channel jupyter-iopub-channel) &key _identity)
  "Start an iopub CHANNEL subscribed to all messages.
If IDENTITY is non-nil, it is used as the ROUTING_ID of the
underlying channel's socket."
  (when (cl-call-next-method)
    (zmq-socket-set (oref channel socket) zmq-SUBSCRIBE "")))

(cl-defmethod jupyter-stop-channel ((channel jupyter-channel))
  "Stop a CHANNEL.
The underlying socket's LINGER property is set to 0, the socket
is closed, the channel's socket property is set to nil, and any
pending messages in the channels recv-queue are removed. Note
that `jupyter-channel-alive-p' on the CHANNEL will return nil
after a call to this function."
  (when (jupyter-channel-alive-p channel)
    (let ((sock (oref channel socket)))
      (zmq-socket-set sock zmq-LINGER 0)
      (zmq-close sock)
      (cl-loop
       with ring = (oref channel recv-queue)
       repeat (ring-length ring) do (ring-remove ring))
      (oset channel socket nil))))

(cl-defmethod jupyter-channel-alive-p ((channel jupyter-channel))
  "Return non-nil if CHANNEL is alive.
A channel is alive if its socket property is bound to a
`zmq-socket'."
  (and (slot-boundp channel 'socket)
       (not (null (oref channel socket)))))

(cl-defmethod jupyter-queue-message ((channel jupyter-channel) msg)
  "Add a message to a CHANNEL's recieve queue.
MSG is a cons pair (IDENTS . MSG) which will be added to the
recv-queue slot of CHANNEL. To receive a message from the channel
call `jupyter-get-message'."
  (let ((ring (oref channel recv-queue)))
    (ring-insert+extend ring msg 'grow)))

(cl-defmethod jupyter-get-message ((channel jupyter-channel))
  (when (jupyter-messages-available-p channel)
    (cl-destructuring-bind (_idents . msg)
        (ring-remove (oref channel recv-queue))
      msg)))

(cl-defmethod jupyter-messages-available-p ((channel jupyter-channel))
  (not (ring-empty-p (oref channel recv-queue))))

;;; Heartbeat channel

(defclass jupyter-hb-channel (jupyter-connection)
  ((type
    :type keyword
    :initform :hb
    :documentation "The type of this channel is `:hb'.")
   (endpoint
    :type string
    :initarg :endpoint
    :documentation "The endpoint this channel is connected to.
 Typical endpoints look like \"tcp://127.0.0.1:5555\".")
   (socket
    :type (or null zmq-socket)
    :initform nil
    :documentation "The socket used for communicating with the kernel.")
   (time-to-dead
    :type integer
    :initform 1
    :documentation "The time in seconds to wait for a response
 from the kernel until the connection is assumed to be dead. Note
 that this slot only takes effect when starting the channel.")
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
 use `jupyter-hb-unpause'.")
   (timer
    :type (or null timer)
    :initform nil
    :documentation "The timer which sends and receives heartbeat
 messages from the kernel."))
  :documentation "A base class for heartbeat channels.")

(cl-defmethod jupyter-channel-alive-p ((channel jupyter-hb-channel))
  "Return non-nil if CHANNEL is alive."
  (and (oref channel timer) (memq (oref channel timer) timer-list)))

(cl-defmethod jupyter-hb-beating-p ((channel jupyter-hb-channel))
  "Return non-nil if the kernel associated with CHANNEL is still
connected."
  (unless (jupyter-channel-alive-p channel)
    (error "Heartbeat channel not alive"))
  (oref channel beating))

(cl-defmethod jupyter-hb-pause ((channel jupyter-hb-channel))
  "Pause checking for heartbeat events on CHANNEL."
  (unless (jupyter-channel-alive-p channel)
    (error "Heartbeat channel not alive"))
  (oset channel paused t))

(cl-defmethod jupyter-hb-unpause ((channel jupyter-hb-channel))
  "Unpause checking for heatbeat events on CHANNEL."
  (unless (jupyter-channel-alive-p channel)
    (error "Heartbeat channel not alive"))
  (oset channel paused nil))

(cl-defmethod jupyter-stop-channel ((channel jupyter-hb-channel))
  "Stop the heartbeat CHANNEL."
  (when (jupyter-channel-alive-p channel)
    (cancel-timer (oref channel timer))
    (zmq-socket-set (oref channel socket) zmq-LINGER 0)
    (zmq-close (oref channel socket))
    (oset channel socket nil)
    (oset channel timer nil)))

(cl-defmethod jupyter-start-channel ((channel jupyter-hb-channel) &key identity)
  (unless (jupyter-channel-alive-p channel)
    (oset channel socket (jupyter-connect-channel
                          :hb (oref channel endpoint) identity))
    (oset channel timer
          (run-with-timer
           0 (oref channel time-to-dead)
           (let ((sent nil))
             (lambda (channel)
               (let ((sock (oref channel socket)))
                 (when sent
                   (setq sent nil)
                   (if (condition-case nil
                           (zmq-recv sock zmq-NOBLOCK)
                         ((zmq-EINTR zmq-EAGAIN) nil))
                       (oset channel beating t)
                     (oset channel beating nil)
                     (zmq-socket-set sock zmq-LINGER 0)
                     (zmq-close sock)
                     (setq sock (jupyter-connect-channel
                                 :hb (oref channel endpoint) identity))
                     (oset channel socket sock)))
                 (unless (oref channel paused)
                   (zmq-send sock "ping")
                   (setq sent t)))))
           channel))))

(provide 'jupyter-channels)

;;; jupyter-channels.el ends here
