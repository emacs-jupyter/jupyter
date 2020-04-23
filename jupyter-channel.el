;;; jupyter-channel.el --- Jupyter channel interface -*- lexical-binding: t -*-

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

;; Defines the `jupyter-channel' interface.

;;; Code:

(require 'eieio)

(defclass jupyter-channel ()
  ((type
    :type keyword
    :initarg :type
    :documentation "The type of this channel.")
   (session
    :type jupyter-session
    :initarg :session
    :documentation "The session object used to sign and send/receive messages.")
   (endpoint
    :type string
    :initarg :endpoint
    :documentation "The endpoint this channel is connected to.
 Typical endpoints look like \"tcp://127.0.0.1:5555\"."))
  :abstract t)

(cl-defgeneric jupyter-start-channel ((channel jupyter-channel) &key identity)
  "Start a Jupyter CHANNEL using IDENTITY as the routing ID.
If CHANNEL is already alive, do nothing.")

(cl-defgeneric jupyter-stop-channel ((channel jupyter-channel))
  "Stop a Jupyter CHANNEL.
If CHANNEL is already stopped, do nothing.")

(cl-defgeneric jupyter-channel-alive-p ((channel jupyter-channel))
  "Return non-nil if a CHANNEL is alive.")

(cl-defgeneric jupyter-send (channel type message &optional msg-id)
  "On CHANNEL send MESSAGE which has message TYPE and optionally a MSG-ID.")

(cl-defgeneric jupyter-recv (channel &optional dont-wait)
  "Receive a message on CHANNEL.
If DONT-WAIT is non-nil, return nil immediately if there is no
message available to receive.")

(provide 'jupyter-channel)

;;; jupyter-channel.el ends here
