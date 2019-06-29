;;; jupyter-channel-ioloop.el --- IOLoop functions for Jupyter channels -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 08 Nov 2018
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

;; Functions which define new events to be added to an IOLoop for working with
;; Jupyter channels. Adds the functions
;; `jupyter-channel-ioloop-add-start-channel-event',
;; `jupyter-channel-ioloop-add-stop-channel-event', and
;; `jupyter-channel-ioloop-add-send-event' to add the start-channel,
;; stop-channel, and send events to a `jupyter-ioloop'. In addition defines the
;; new type `jupyter-channel-ioloop' which takes care of setting up a
;; `jupyter-ioloop' for communicating with channels so that one only needs to
;; do the following to start a pre-configured subprocess:
;;
;; (let ((ioloop (jupyter-channel-ioloop)))
;;    (jupyter-start-ioloop ioloop ...)
;;    ...
;;    (jupyter-send ioloop 'start-channel ...)
;;    ...)

;;; Code:

(require 'jupyter-base)
(require 'jupyter-ioloop)
(require 'jupyter-zmq-channel)

;;; Variables used in the ioloop

;; Meant to be used in the subprocess
(defvar jupyter-ioloop-channels nil
  "A list of synchronous channels in an ioloop controlling Jupyter channels.")

(defvar jupyter-ioloop-session nil
  "The `jupyter-session' used when initializing Jupyter channels.")

;;; `jupyter-channel' ioloop arg

(jupyter-ioloop-add-arg-type jupyter-channel
  (lambda (arg)
    `(or (object-assoc ,arg :type jupyter-ioloop-channels)
         (error "Channel not alive (%s)" ,arg))))

;;; `jupyter-channel-ioloop'

(defclass jupyter-channel-ioloop (jupyter-ioloop)
  ()
  :documentation "A `jupyter-ioloop' configured for Jupyter channels.")

(cl-defmethod initialize-instance ((ioloop jupyter-channel-ioloop) &optional _slots)
  (cl-call-next-method)
  (jupyter-ioloop-add-setup ioloop
    (require 'jupyter-channel-ioloop)
    (push 'jupyter-channel-ioloop-recv-messages jupyter-ioloop-post-hook))
  (jupyter-channel-ioloop-add-send-event ioloop)
  (jupyter-channel-ioloop-add-start-channel-event ioloop)
  (jupyter-channel-ioloop-add-stop-channel-event ioloop)
  (jupyter-ioloop-add-teardown ioloop
    (mapc #'jupyter-stop-channel jupyter-ioloop-channels)))

;;;; Starting the ioloop

(defun jupyter-channel-ioloop--set-session (ioloop session)
  "In the IOLOOP, set SESSION as the `jupyter-ioloop-session'.
Add a form to IOLOOP's setup that sets the variable
`jupyter-ioloop-session' to a `jupyter-session' based on
SESSION's id and key. Remove any top level form in the setup that
sets `jupyter-ioloop-session' via `setq' before doing so."
  (cl-callf (lambda (setup)
              (cons `(setq jupyter-ioloop-session
                           (jupyter-session
                            :id ,(jupyter-session-id session)
                            :key ,(jupyter-session-key session)))
                    (cl-remove-if
                     (lambda (f) (and (eq (car f) 'setq)
                                 (eq (cadr f) 'jupyter-ioloop-session)))
                     setup)))
      (oref ioloop setup)))

(cl-defmethod jupyter-ioloop-start ((ioloop jupyter-channel-ioloop)
                                    (session jupyter-session)
                                    obj &key buffer)
  "Start IOLOOP, using SESSION to set the `jupyter-ioloop-session'.
Add setup forms to IOLOOP that will initialize the
`jupyter-ioloop-session' variable to a `jupyter-session' based on
SESSION's id and key. Also add `jupyter-ioloop-recv-messages' to
`jupyter-ioloop-post-hook'. In addition add the events send,
start-channel, and stop-channel that the parent Emacs process can
send to the IOLOOP. See `jupyter-channel-ioloop-add-send-event',
`jupyter-channel-ioloop-add-start-channel-event', and
`jupyter-ioloop-add-stop-channel-event'.

After doing the above initialization, start the IOLOOP. OBJ and
BUFFER have the same meaning as in the method definition for
`jupyter-ioloop'.

By default, no channels will be alive. A start-channel event will
have to be sent to the ioloop with the appropriate arguments to
start a channel."
  (jupyter-channel-ioloop--set-session ioloop session)
  (cl-call-next-method ioloop obj :buffer buffer))

;;;; Receiving messages in the ioloop

(defun jupyter-channel-ioloop-recv-messages (events)
  "Print the received messages described in EVENTS.
EVENTS is a list of socket events as returned by
`zmq-poller-wait-all'. If any of the sockets in EVENTS matches
one of the sockets in `jupyter-ioloop-channels', receive a
message on the channel and print a list with the form

    (message CHANNEL-TYPE . MSG...)

to stdout. CHANNEL-TYPE is the channel on which MSG was received,
either :shell, :stdin, or :iopub. MSG is a list as returned by
`jupyter-recv'."
  (let (messages)
    (dolist (channel jupyter-ioloop-channels)
      (with-slots (type socket) channel
        (when (zmq-assoc socket events)
          (push (cons type (jupyter-recv channel)) messages))))
    (when messages
      ;; Send messages
      (mapc (lambda (msg) (prin1 (cons 'message msg))) (nreverse messages))
      (zmq-flush 'stdout))))

;;;; Channel events

(defun jupyter-channel-ioloop-add-start-channel-event (ioloop)
  "Add a start-channel event handler to IOLOOP.
The event fires when the IOLOOP receives a list with the form

    (start-channel CHANNEL-TYPE ENDPOINT)

and shall stop any existing channel with CHANNEL-TYPE and start a
new channel with CHANNEL-TYPE connected to ENDPOINT. The
underlying socket IDENTITY is derived from
`jupyter-ioloop-session' in the IOLOOP environment. The channel
will be added to the variable `jupyter-ioloop-channels' in the
IOLOOP environment.

The handler also takes care of removing/adding the channel's
socket from/to `jupyter-ioloop-poller' in the IOLOOP environment.

A list with the form

    (start-channel CHANNEL-TYPE)

is returned to the parent process."
  (jupyter-ioloop-add-event ioloop start-channel (type endpoint)
    (cl-assert (memq type jupyter-socket-types))
    (let ((channel (object-assoc type :type jupyter-ioloop-channels)))
      (unless channel
        (setq channel (jupyter-zmq-channel
                       :session jupyter-ioloop-session
                       :type type :endpoint endpoint))
        (push channel jupyter-ioloop-channels))
      ;; Stop the channel if it is already alive
      (when (jupyter-channel-alive-p channel)
        (jupyter-ioloop-poller-remove (oref channel socket))
        (jupyter-stop-channel channel))
      ;; Start the channel, add it to the poller
      (oset channel endpoint endpoint)
      (jupyter-start-channel channel :identity (jupyter-session-id jupyter-ioloop-session))
      (jupyter-ioloop-poller-add (oref channel socket) zmq-POLLIN)
      (list 'start-channel type))))

(defun jupyter-channel-ioloop-add-stop-channel-event (ioloop)
  "Add a stop-channel event handler to IOLOOP.
The event fires when the IOLOOP receives a list with the form

    (stop-channel CHANNEL-TYPE)

If a channel with CHANNEL-TYPE exists and is alive, it is stopped
and remove from `jupyter-ioloop-poller'.

A list with the form

    (stop-channel CHANNEL-TYPE)

is returned to the parent process."
  (jupyter-ioloop-add-event ioloop stop-channel (type)
    (cl-assert (memq type jupyter-socket-types))
    (let ((channel (object-assoc type :type jupyter-ioloop-channels)))
      (when (and channel (jupyter-channel-alive-p channel))
        (jupyter-ioloop-poller-remove (oref channel socket))
        (jupyter-stop-channel channel))
      (list 'stop-channel type))))

(defun jupyter-channel-ioloop-add-send-event (ioloop)
  "Add a send event handler to IOLOOP.
The event fires when the IOLOOP receives a list with the form

    (send CHANNEL-TYPE MSG-TYPE MSG MSG-ID)

and calls (jupyter-send CHANNEL MSG-TYPE MSG MSG-ID) using the
channel corresponding to CHANNEL-TYPE in the IOLOOP environment.

A list with the form

    (sent CHANNEL-TYPE MSG-ID)

is returned to the parent process."
  (jupyter-ioloop-add-event
      ioloop send ((channel jupyter-channel) msg-type msg msg-id)
    (list 'sent (oref channel type)
          (jupyter-send channel msg-type msg msg-id))))

(provide 'jupyter-channel-ioloop)

;;; jupyter-channel-ioloop.el ends here
