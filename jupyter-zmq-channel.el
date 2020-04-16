;;; jupyter-zmq-channel.el --- A Jupyter channel implementation using ZMQ sockets -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 27 Jun 2019

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

;; Implements synchronous channel types using ZMQ sockets.  Each channel is
;; essentially a wrapper around a `zmq-socket' constrained to a socket type by
;; the type of the channel and with an associated `zmq-IDENTITY' obtained from
;; the `jupyter-session' that must be associated with the channel.  A heartbeat
;; channel is distinct from the other channels in that it is implemented using
;; a timer which periodically pings the kernel depending on how its configured.
;; In order for communication to occur on the other channels, one of
;; `jupyter-send' or `jupyter-recv' must be called after starting the channel
;; with `jupyter-start-channel'.

;;; Code:

(require 'jupyter-messages)
(require 'zmq)
(require 'jupyter-channel)
(eval-when-compile (require 'subr-x))

(declare-function jupyter-ioloop-poller-remove "jupyter-ioloop")
(declare-function jupyter-ioloop-poller-add "jupyter-ioloop")

(defconst jupyter-socket-types
  (list :hb zmq-REQ
        :shell zmq-DEALER
        :iopub zmq-SUB
        :stdin zmq-DEALER
        :control zmq-DEALER)
  "The socket types for the various channels used by `jupyter'.")

(defclass jupyter-zmq-channel (jupyter-channel)
  ((socket
    :type (or null zmq-socket)
    :initform nil
    :documentation "The socket used for communicating with the kernel.")))

(defun jupyter-connect-endpoint (type endpoint &optional identity)
  "Create socket with TYPE and connect to ENDPOINT.
If IDENTITY is non-nil, it will be set as the ROUTING-ID of the
socket.  Return the created socket."
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
connect to ENDPOINT.  If IDENTITY is non-nil, it will be set as
the ROUTING-ID of the socket.  Return the created socket."
  (let ((sock-type (plist-get jupyter-socket-types ctype)))
    (unless sock-type
      (error "Invalid channel type (%s)" ctype))
    (jupyter-connect-endpoint sock-type endpoint identity)))

(cl-defmethod jupyter-start-channel ((channel jupyter-zmq-channel)
                                     &key (identity (jupyter-session-id
                                                     (oref channel session))))
  (unless (jupyter-channel-alive-p channel)
    (let ((socket (jupyter-connect-channel
                   (oref channel type) (oref channel endpoint) identity)))
      (oset channel socket socket)
      (cl-case (oref channel type)
        (:iopub
         (zmq-socket-set socket zmq-SUBSCRIBE ""))))
    (when (and (functionp 'jupyter-ioloop-environment-p)
               (jupyter-ioloop-environment-p))
      (jupyter-ioloop-poller-add (oref channel socket) zmq-POLLIN))))

(cl-defmethod jupyter-stop-channel ((channel jupyter-zmq-channel))
  (when (jupyter-channel-alive-p channel)
    (when (and (functionp 'jupyter-ioloop-environment-p)
               (jupyter-ioloop-environment-p))
      (jupyter-ioloop-poller-remove (oref channel socket)))
    (with-slots (socket) channel
      (zmq-disconnect socket (zmq-socket-get socket zmq-LAST-ENDPOINT)))
    (oset channel socket nil)))

(cl-defmethod jupyter-channel-alive-p ((channel jupyter-zmq-channel))
  (not (null (oref channel socket))))

(cl-defmethod jupyter-send ((channel jupyter-zmq-channel) type message &optional msg-id)
  "Send a message on a ZMQ based Jupyter channel.
CHANNEL is the channel to send MESSAGE on.  TYPE is a Jupyter
message type, like :kernel-info-request.  Return the message ID
of the sent message."
  (cl-destructuring-bind (id . msg)
      (jupyter-encode-message (oref channel session) type
        :msg-id msg-id
        :content message)
    (prog1 id
      (zmq-send-multipart (oref channel socket) msg))))

(cl-defmethod jupyter-recv ((channel jupyter-zmq-channel) &optional dont-wait)
  "Receive a message on CHANNEL.
Return a cons cell (IDENTS . MSG) where IDENTS are the ZMQ
message identities, as a list, and MSG is the received message.

If DONT-WAIT is non-nil, return immediately without waiting for a
message if one isn't already available."
  (condition-case nil
      (let ((session (oref channel session))
            (msg (zmq-recv-multipart (oref channel socket)
                                     (and dont-wait zmq-DONTWAIT))))
        (when msg
          (cl-destructuring-bind (idents . parts)
              (jupyter--split-identities msg)
            (cons idents (jupyter-decode-message session parts)))))
    (zmq-EAGAIN nil)))

;;; Heartbeat channel

(defvar jupyter-hb-max-failures 3
  "Number of heartbeat failures until the kernel is considered unreachable.
A ping is sent to the kernel on a heartbeat channel and waits
until `time-to-dead' seconds to see if the kernel sent a ping
back.  If the kernel doesn't send a ping back after
`jupyter-hb-max-failures', the callback associated with the
heartbeat channel is called.  See `jupyter-hb-on-kernel-dead'.")

(defclass jupyter-hb-channel (jupyter-zmq-channel)
  ((type
    :type keyword
    :initform :hb
    :documentation "The type of this channel is `:hb'.")
   (time-to-dead
    :type number
    :initform 10
    :documentation "The time in seconds to wait for a response
from the kernel until the connection is assumed to be dead.  Note
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
channel is paused and not communicating with the kernel.  To
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
        ;; Consume a pending message from the kernel if there is one.  We send a
        ;; ping and then schedule a timer which fires TIME-TO-DEAD seconds
        ;; later to receive the ping back from the kernel and start the process
        ;; all over again.  If the channel is paused before TIME-TO-DEAD
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
        (progn
          (zmq-send (oref channel socket) "ping")
          (run-with-timer
           (oref channel time-to-dead) nil
           (lambda ()
             (when-let* ((sock (and (jupyter-hb--pingable-p channel)
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
                     (funcall (oref channel dead-cb)))))))))
      ;; FIXME: Should be a part of `jupyter-hb--pingable-p'
      (zmq-ENOTSOCK
       (jupyter-hb-pause channel)
       (oset channel socket nil)))))

(provide 'jupyter-zmq-channel)

;;; jupyter-zmq-channel.el ends here
