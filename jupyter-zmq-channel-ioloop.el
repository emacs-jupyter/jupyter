;;; jupyter-zmq-channel-ioloop.el --- IOLoop functions for Jupyter channels -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 08 Nov 2018

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

;; A `jupyter-channel-ioloop' using `jupyter-zmq-channel' to send and receive
;; messages.  Whenever a message is received on a channel an event that looks
;; like the following will be sent back to the parent process
;;
;;     (message CHANNEL-TYPE IDENTS . MSG)
;;
;; where CHANNEL-TYPE is the channel on which the message was received (one of
;; `jupyter-socket-types'), IDENTS are ZMQ identities, typically ignored, and
;; MSG is the message plist.

;;; Code:

(require 'jupyter-base)
(require 'jupyter-channel-ioloop)
(require 'jupyter-zmq-channel)

(defclass jupyter-zmq-channel-ioloop (jupyter-channel-ioloop)
  ()
  :documentation "A `jupyter-ioloop' configured for Jupyter channels.")

(cl-defmethod initialize-instance ((ioloop jupyter-zmq-channel-ioloop) &optional _slots)
  (cl-call-next-method)
  (jupyter-ioloop-add-setup ioloop
    (require 'jupyter-zmq-channel-ioloop)
    (push 'jupyter-zmq-channel-ioloop--recv-messages jupyter-ioloop-post-hook)
    (cl-loop
     for channel in '(:shell :stdin :iopub)
     unless (object-assoc channel :type jupyter-channel-ioloop-channels)
     do (push (jupyter-zmq-channel
               :session jupyter-channel-ioloop-session
               :type channel)
              jupyter-channel-ioloop-channels))))

(defun jupyter-zmq-channel-ioloop--recv-messages (events)
  "Print the received messages described in EVENTS.
EVENTS is a list of socket events as returned by
`zmq-poller-wait-all'.  If any of the sockets in EVENTS matches
one of the sockets in `jupyter-channel-ioloop-channels', receive a
message on the channel and print a list with the form

    (message CHANNEL-TYPE . MSG...)

to stdout.  CHANNEL-TYPE is the channel on which MSG was received,
either :shell, :stdin, or :iopub.  MSG is a list as returned by
`jupyter-recv'."
  (let (messages)
    (dolist (channel jupyter-channel-ioloop-channels)
      (with-slots (type socket) channel
        (when (zmq-assoc socket events)
          (push (cons type (jupyter-recv channel)) messages))))
    (when messages
      ;; Send messages
      (mapc (lambda (msg) (prin1 (cons 'message msg))) (nreverse messages))
      (zmq-flush 'stdout))))

(provide 'jupyter-zmq-channel-ioloop)

;;; jupyter-zmq-channel-ioloop.el ends here
