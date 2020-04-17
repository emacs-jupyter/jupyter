;;; jupyter-channel-ioloop.el --- Abstract class to communicate with a jupyter-channel in a subprocess -*- lexical-binding: t -*-

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

;; Define a `jupyter-ioloop' that can be sent events to start, stop, or send a
;; message on a set of `jupyter-channel' objects.  For example to start a
;; `jupyter-channel' in the subprocess environment you would do something like
;;
;;     (jupyter-send ioloop 'start-channel TYPE ENDPOINT)
;;
;; where TYPE and ENDPOINT have the same meaning as in `jupyter-channel'.
;;
;; Note by default, no channels are available in the subprocess environment.
;; You initialize channels by setting the `jupyter-channel-ioloop-channels'
;; variable in the subprocess environment, e.g. using
;; `jupyter-ioloop-add-setup', before starting the `jupyter-ioloop'.
;;
;; When you call `jupyter-ioloop-start' a `jupyter-session' object needs to
;; passed as the second argument with whatever object you would like to receive
;; events as the third.  The `jupyter-session-id' will be used as the value of
;; the :identity key in the call to `jupyter-start-channel' when starting a
;; channel.
;;
;; Each event sent to the subprocess will send back a corresponding
;; confirmation event, the three events that can be sent and their
;; corresponding confirmation events are:
;;
;;     (start-channel TYPE ENDPOINT) -> (start-channel TYPE)
;;     (stop-channel TYPE) -> (stop-channel TYPE)
;;     (send TYPE MSG-TYPE MSG MSG-ID) -> (sent MSG-ID)
;;
;; For the send event, the MSG-TYPE, MSG, and MSG-ID have the same meaning as
;; the TYPE, MSG, and MSG-ID arguments of the `jupyter-send' method of a
;; `jupyter-channel'.
;;
;; Ex.
;;
;; (let ((ioloop (jupyter-channel-ioloop))
;;       (session (jupyter-session :id ...)))
;;    (jupyter-start-ioloop ioloop session ...)
;;    ...
;;    (jupyter-send ioloop 'start-channel ...)
;;    ...)

;;; Code:

(require 'jupyter-ioloop)

(defvar jupyter-channel-ioloop-session nil
  "The `jupyter-session' used when initializing Jupyter channels.")

(defvar jupyter-channel-ioloop-channels nil
  "A list of synchronous channels in an ioloop controlling Jupyter channels.")

(jupyter-ioloop-add-arg-type jupyter-channel
  (lambda (arg)
    `(or (object-assoc ,arg :type jupyter-channel-ioloop-channels)
         (error "Channel not alive (%s)" ,arg))))

(defclass jupyter-channel-ioloop (jupyter-ioloop)
  ()
  :abstract t)

(cl-defmethod initialize-instance ((ioloop jupyter-channel-ioloop) &optional _slots)
  (cl-call-next-method)
  (jupyter-ioloop-add-setup ioloop
    (require 'jupyter-channel-ioloop))
  (jupyter-channel-ioloop-add-start-channel-event ioloop)
  (jupyter-channel-ioloop-add-stop-channel-event ioloop)
  (jupyter-channel-ioloop-add-send-event ioloop)
  (jupyter-ioloop-add-teardown ioloop
    (mapc #'jupyter-stop-channel jupyter-channel-ioloop-channels)))

(defun jupyter-channel-ioloop-set-session (ioloop session)
  "In the IOLOOP, set SESSION as the `jupyter-channel-ioloop-session'.
Add a form to IOLOOP's setup that sets the variable
`jupyter-channel-ioloop-session' to a `jupyter-session' based on
SESSION's id and key.  Remove any top level form in the setup that
sets `jupyter-channel-ioloop-session' via `setq' before doing so."
  (cl-callf (lambda (setup)
              (cons `(setq jupyter-channel-ioloop-session
                           (jupyter-session
                            :id ,(jupyter-session-id session)
                            :key ,(jupyter-session-key session)))
                    (cl-remove-if
                     (lambda (f) (and (eq (car f) 'setq)
                                 (eq (cadr f) 'jupyter-channel-ioloop-session)))
                     setup)))
      (oref ioloop setup)))

;;; Channel events

(defun jupyter-channel-ioloop-add-start-channel-event (ioloop)
  "Add a start-channel event handler to IOLOOP.
The event fires when the IOLOOP receives a list with the form

    (start-channel CHANNEL-TYPE ENDPOINT)

and shall stop any existing channel with CHANNEL-TYPE and start a
new channel with CHANNEL-TYPE connected to ENDPOINT.  The
underlying socket IDENTITY is derived from
`jupyter-channel-ioloop-session' in the IOLOOP environment.  The
channel will be added to the variable
`jupyter-channel-ioloop-channels' in the IOLOOP environment.

Note, before sending this event to IOLOOP, the corresponding
channel needs to be available in the
`jupyer-channel-ioloop-channels' variable.  You can initialize
this variable in the setup form of IOLOOP.

A list with the form

    (start-channel CHANNEL-TYPE)

is returned to the parent process."
  (jupyter-ioloop-add-event
      ioloop start-channel ((channel jupyter-channel) endpoint)
    ;; Stop the channel if it is already alive
    (when (jupyter-channel-alive-p channel)
      (jupyter-stop-channel channel))
    ;; Start the channel
    (oset channel endpoint endpoint)
    (let ((identity (jupyter-session-id jupyter-channel-ioloop-session)))
      (jupyter-start-channel channel :identity identity))
    (list 'start-channel (oref channel type))))

(defun jupyter-channel-ioloop-add-stop-channel-event (ioloop)
  "Add a stop-channel event handler to IOLOOP.
The event fires when the IOLOOP receives a list with the form

    (stop-channel CHANNEL-TYPE)

If a channel with CHANNEL-TYPE exists and is alive, it is stopped.

A list with the form

    (stop-channel CHANNEL-TYPE)

is returned to the parent process."
  (jupyter-ioloop-add-event ioloop stop-channel (type)
    (let ((channel (object-assoc type :type jupyter-channel-ioloop-channels)))
      (when (and channel (jupyter-channel-alive-p channel))
        (jupyter-stop-channel channel))
      (list 'stop-channel type))))

(defun jupyter-channel-ioloop-add-send-event (ioloop)
  "Add a send event handler to IOLOOP.
The event fires when the IOLOOP receives a list of the form

    (send CHANNEL-TYPE MSG-TYPE MSG MSG-ID)

and calls (jupyter-send CHANNEL MSG-TYPE MSG MSG-ID) using the
channel corresponding to CHANNEL-TYPE in the IOLOOP environment.

A list of the form

    (sent CHANNEL-TYPE MSG-ID)

is returned to the parent process."
  (jupyter-ioloop-add-event
      ioloop send ((channel jupyter-channel) msg-type msg msg-id)
    (list 'sent (oref channel type)
          (jupyter-send channel msg-type msg msg-id))))

(provide 'jupyter-channel-ioloop)

;;; jupyter-channel-ioloop.el ends here
