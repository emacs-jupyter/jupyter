;;; jupyter-base.el --- Core definitions for Jupyter -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 06 Jan 2018
;; Version: 0.0.1
;; Keywords: jupyter literate-programming

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

;; This file holds the core requires, variables, and type definitions necessary
;; for jupyter.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'json)
(require 'zmq)
(require 'hmac-def)
(require 'jupyter-kernelspec)

(defcustom jupyter-include-other-output nil
  "Whether or not to handle IOPub messages from other clients.
A Jupyter client can receive messages from other clients
connected to the same kernel on the IOPub channel. You can choose
to ignore these messages by setting
`jupyter-include-other-output' to nil. If
`jupyter-include-other-output' is non-nil, then any messages that
are not associated with a request from a client are sent to the
client's handler methods with a nil value for the request
argument. To change the value of this variable for a particular
client use `jupyter-set'."
  :group 'jupyter
  :type 'boolean)

(defcustom jupyter-iopub-message-hook nil
  "Hook run with one argument, a message received on the IOPub channel.
Do not add to this hook variable directly, use
`jupyter-add-hook'."
  :group 'jupyter
  :type 'hook)
(put 'jupyter-iopub-message-hook 'permanent-local t)

(defcustom jupyter-shell-message-hook nil
  "Hook run with one argument, a message received on the SHELL channel.
Do not add to this hook variable directly, use
`jupyter-add-hook'."
  :group 'jupyter
  :type 'hook)
(put 'jupyter-shell-message-hook 'permanent-local t)

(defcustom jupyter-stdin-message-hook nil
  "Hook run with one argument, a message received on the STDIN channel.
Do not add to this hook variable directly, use
`jupyter-add-hook'."
  :group 'jupyter
  :type 'hook)
(put 'jupyter-stdin-message-hook 'permanent-local t)

(defcustom jupyter-runtime-directory (string-trim-right
                                      (shell-command-to-string
                                       "jupyter --runtime-dir"))
  "The Jupyter runtime directory.
When a new kernel is started through `jupyter-start-kernel', this
directory is where kernel connection files are written to."
  :group 'jupyter
  :type 'string)

(defconst jupyter-protocol-version "5.3"
  "The jupyter protocol version that is implemented.")

(defconst jupyter-socket-types
  (list :hb zmq-REQ
        :shell zmq-DEALER
        :iopub zmq-SUB
        :stdin zmq-DEALER
        :control zmq-DEALER)
  "The socket types for the various channels used by `jupyter'.")

(defconst jupyter-message-types
  (list :execute-result "execute_result"
        :execute-request "execute_request"
        :execute-reply "execute_reply"
        :inspect-request "inspect_request"
        :inspect-reply "inspect_reply"
        :complete-request "complete_request"
        :complete-reply "complete_reply"
        :history-request "history_request"
        :history-reply "history_reply"
        :is-complete-request "is_complete_request"
        :is-complete-reply "is_complete_reply"
        :comm-info-request "comm_info_request"
        :comm-info-reply "comm_info_reply"
        :kernel-info-request "kernel_info_request"
        :kernel-info-reply "kernel_info_reply"
        :shutdown-request "shutdown_request"
        :shutdown-reply "shutdown_reply"
        :interupt-request "interrupt_request"
        :interrupt-reply "interrupt_reply"
        :stream "stream"
        :display-data "display_data"
        :update-display-data "update_display_data"
        :execute-input "execute_input"
        :error "error"
        :status "status"
        :clear-output "clear_output"
        :input-reply "input_reply")
  "A plist mapping keywords to Jupyter message type strings.
The plist values are the message types either sent or received
from the kernel.")

;; https://tools.ietf.org/html/rfc4868
(defun sha256 (object)
  (secure-hash 'sha256 object nil nil t))

(define-hmac-function hmac-sha256 sha256 64 32)

;; TODO: Better UUID randomness, `cl-random' seeds the random state with the
;; current time but only to second resolution.
(defun jupyter-new-uuid ()
  "Make a version 4 UUID."
  (format "%04x%04x-%04x-%04x-%04x-%06x%06x"
          (cl-random 65536)
          (cl-random 65536)
          (cl-random 65536)
          ;; https://tools.ietf.org/html/rfc4122
          (let ((r (cl-random 65536)))
            (if (= (byteorder) ?l)
                ;; ?l = little-endian
                (logior (logand r 4095) 16384)
              ;; big-endian
              (logior (logand r 65295) 64)))
          (let ((r (cl-random 65536)))
            (if (= (byteorder) ?l)
                (logior (logand r 49151) 32768)
              (logior (logand r 65471) 128)))
          (cl-random 16777216)
          (cl-random 16777216)))

;;; Session object definition

(cl-defstruct (jupyter-session
               (:constructor nil)
               (:constructor
                jupyter-session
                (&key (id (jupyter-new-uuid))
                      (key nil))))
  "A `jupyter-session' holds the information needed to
authenticate messages. Each `jupyter-session' should have a
unique ID which is used as the `zmq-ROUTING-ID' for every
`jupyter-channel' socket that utilizes the session object. The
KEY of a `jupyter-session' is used for message signing. Message
signing is not done if the KEY of a `jupyter-session' is empty."
  (id nil :read-only t)
  (key nil :read-only t))

;;; Request object definition

(cl-defstruct jupyter-request
  "A `jupyter-request' encapsulates the current status of a
request to a kernel. A `jupyter-request' consists of the
following fields:

- -ID :: A UUID to match a `jupyter-request' to the received
        messages of a kernel. Note that this is an internal
        field, to access the ID of a request use
        `jupyter-request-id'. Note that `jupyter-request-id'
        blocks until there is a guarantee that the request was
        sent off to the kernel.

- TIME :: The time at which the request was made.

- IDLE-RECEIVED-P :: A flag variable that is set to t when a
                    `jupyter-kernel-client' has received the
                    status: idle message for the request.

- LAST-MESSAGE-TIME :: The last time a message was received for
                       the request.

- INHIBITED-HANDLERS :: A list of handler message types to
                        prevent the running of that particular
                        handler. If set to t, disable all
                        handlers for this request. Note this
                        should not be set directly, dynamically
                        bind `jupyter-inhibit-handlers' before
                        making the request.

- CALLBACKS :: An alist mapping message types to their
               corresponding callbacks. This alist is modified
               through calls to `jupyter-add-callback' on the request."
  (-id)
  (time (current-time))
  (idle-received-p nil)
  (last-message-time nil)
  (inhibited-handlers nil)
  (callbacks))

(defun jupyter-request-id (req)
  "Get the message ID for REQ."
  (or (jupyter-request--id req)
      (with-timeout (0.5 (error "Request not processed"))
        (while (null (jupyter-request--id req))
          (sleep-for 0 10))
        (jupyter-request--id req))))

(provide 'jupyter-base)

;;; jupyter-base.el ends here
