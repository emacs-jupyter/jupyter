;;; jupyter-server-ioloop.el --- Kernel server communication -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 03 Apr 2019

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

;; A `jupyter-server-ioloop' launches websocket connections in order to
;; communicate with a kernel server via the Jupyter messaging protocol.  You can
;; tell the ioloop to establish a websocket connection to a particular kernel
;; by sending a connect-channels event with the websocket URL and kernel ID.
;;
;;     (jupyter-send ioloop 'connect-channels "id")
;;
;; A connect-channels event will be emitted back to the parent process with the
;; ID of the kernel in response.
;;
;; To stop a websocket connection, a disconnect-channels event can be sent,
;; passing the kernel ID.
;;
;;     (jupyter-send ioloop 'disconnect-channels "id")
;;
;; A disconnect-channels event will also be emitted back to the parent process
;; with the ID of the kernel.
;;
;; Finally, a `jupyter-server-ioloop' behaves as a `jupyter-channel-ioloop'
;; when sent a `send' event.  That is it will emit a `sent' event after every
;; `send' and when a message is received from the kernel will emit a `message'
;; event.  When sending a `send' event, the format is the same as a
;; `jupyter-channel-ioloop' except that the kernel ID must be first argument.
;;
;;     (jupyter-send ioloop 'send "id" ...)
;;
;; Similarly, when the parent process receives a `message' or `sent' event, the
;; first argument will be the kernel ID
;;
;;     (message "id" ...) or (sent "id" ...)

;;; Code:

(require 'jupyter-ioloop)
(require 'jupyter-messages)
(require 'jupyter-rest-api)
(require 'websocket)

(defvar jupyter-server-recvd-messages nil)
(defvar jupyter-server-timeout nil)
(defvar jupyter-server-connected-kernels nil)
(defvar jupyter-server-rest-client nil)

(defclass jupyter-server-ioloop (jupyter-ioloop)
  ;; TODO: Clean this up by removing the need for these and just setting these
  ;; values in `jupyter-ioloop-start' similar to the `jupyter-channel-ioloop'.
  ((url :type string :initarg :url)
   (ws-url
    :type string
    :initarg :ws-url
    :documentation "The URL to connect websockets to.")
   (ws-headers
    :type (list-of cons)
    :initform nil
    :initarg :ws-headers
    :documentation "Headers that will be passed to the websocket connections.
Has the same format as `url-request-extra-headers'."))
  :documentation "A `jupyter-ioloop' configured for communication using websockets.

A websocket can be opened by sending the connect-channels event
with the websocket url and the kernel-id of the kernel to connect
to, e.g.

    \(jupyter-send ioloop 'connect-channels \"kernel-id\")

Also implemented is the send event which takes the same arguments
as the send event of a `jupyter-channel-ioloop' except the
kernel-id must be the first element, e.g.

    \(jupyter-send ioloop 'send \"kernel-id\" ...)

Events that are emitted to the parent process are the message
event, also the same as the event in `jupyter-channel-ioloop'
except with a kernel-id as the first element.  And a
disconnected-channels event that occurs whenever a websocket is
closed, the event has the kernel-id of the associated with the
websocket.")

(cl-defmethod initialize-instance ((ioloop jupyter-server-ioloop) &optional _slots)
  (cl-call-next-method)
  (cl-callf append (oref ioloop setup)
    `((jupyter-api-with-subprocess-setup
       (require 'jupyter-server-ioloop)
       (push 'jupyter-server-ioloop--recv-messages jupyter-ioloop-pre-hook)
       ;; Waiting is done using `accept-process-output' instead of
       ;; `zmq-poller-wait-all' since the latter doesn't allow Emacs to process
       ;; websocket events.
       (setq jupyter-server-timeout (/ jupyter-ioloop-timeout 4)
             jupyter-ioloop-timeout (* 3 (/ jupyter-ioloop-timeout 4)))
       (setq jupyter-server-rest-client (jupyter-rest-client
                                         :url ,(oref ioloop url)
                                         :ws-url ,(oref ioloop ws-url)
                                         :auth (quote ,(oref ioloop ws-headers)))))))
  (jupyter-server-ioloop-add-send-event ioloop)
  (jupyter-server-ioloop-add-connect-channels-event ioloop)
  (jupyter-server-ioloop-add-disconnect-channels-event ioloop))

;;; Receiving messages on a websocket

;; Added to `jupyter-ioloop-pre-hook'
(defun jupyter-server-ioloop--recv-messages ()
  (accept-process-output nil (/ jupyter-server-timeout 1000.0))
  (when jupyter-server-recvd-messages
    (mapc (lambda (msg) (prin1 (cons 'message msg)))
       (nreverse jupyter-server-recvd-messages))
    (setq jupyter-server-recvd-messages nil)
    (zmq-flush 'stdout)))

(defun jupyter-server-ioloop--on-message (ws frame)
  (cl-case (websocket-frame-opcode frame)
    ((text binary)
     (condition-case err
         (let* ((msg (jupyter-read-plist-from-string
                      (websocket-frame-payload frame)))
                (channel (intern (concat ":" (plist-get msg :channel))))
                (msg-type (jupyter-message-type-as-keyword
                           (jupyter-message-type msg)))
                (parent-header (plist-get msg :parent_header)))
           ;; Convert into keyword since that is what is expected
           (plist-put msg :msg_type msg-type)
           (plist-put parent-header :msg_type msg-type)
           (push (cons (plist-get (websocket-client-data ws) :id)
                       ;; NOTE: The nil is the identity field expected by a
                       ;; `jupyter-channel-ioloop', it is mimicked here.
                       (cons channel (cons nil msg)))
                 jupyter-server-recvd-messages))
       (error
        (zmq-prin1 (cons 'error (list (car err)
                                      (format "%S" (cdr err))))))))
    (t (zmq-prin1 (cons 'error (format "Unhandled websocket frame %s"
                                       (websocket-frame-opcode frame)))))))

(defun jupyter-server-ioloop--on-error (_ws type error)
  (zmq-prin1 (cons 'error (list 'websocket-error type
                                (format "%S" (cdr error))))))

(defun jupyter-server-ioloop--disconnect (ws)
  (websocket-close ws)
  (cl-callf2 delq ws jupyter-server-connected-kernels))

(defun jupyter-server-ioloop--connect (kernel-id)
  (let ((ws (jupyter-api-get-kernel-ws
             jupyter-server-rest-client kernel-id
             :on-error #'jupyter-server-ioloop--on-error
             :on-message #'jupyter-server-ioloop--on-message)))
    (push ws jupyter-server-connected-kernels)))

(defun jupyter-server-ioloop--kernel-ws (kernel-id)
  (cl-find-if
   (lambda (ws) (equal kernel-id (plist-get (websocket-client-data ws) :id)))
   jupyter-server-connected-kernels))

;;; IOLoop events

(defun jupyter-server-ioloop-add-send-event (ioloop)
  (jupyter-ioloop-add-event
      ioloop send (kernel-id channel msg-type msg msg-id)
    (let ((ws (jupyter-server-ioloop--kernel-ws kernel-id)))
      (unless ws
        (error "Kernel with ID (%s) not connected" kernel-id))
      (websocket-send-text
       ws (jupyter-encode-raw-message
              (plist-get (websocket-client-data ws) :session) msg-type
            :channel (substring (symbol-name channel) 1)
            :msg-id msg-id
            :content msg))
      (jupyter-server-ioloop--recv-messages)
      (list 'sent kernel-id channel msg-id))))

(defun jupyter-server-ioloop-add-connect-channels-event (ioloop)
  (jupyter-ioloop-add-event ioloop connect-channels (kernel-id)
    (let ((ws (jupyter-server-ioloop--kernel-ws kernel-id)))
      (unless ws
        ;; NOTE: Authentication of the client happens in the parent process or
        ;; through the Authorization header set in the :auth slot of the client.
        ;; In the case of the parent process doing the authentication, cookies
        ;; are written to `url-cookie-file' and read from this subprocess by the
        ;; websocket code.
        (url-cookie-parse-file)
        (jupyter-server-ioloop--connect kernel-id)))
    ;; Ensure any pending messages are handled, since usually we synchronize on
    ;; connect-channels events, we want this event to be the
    ;; `jupyter-ioloop-last-event' so the waiting loop in the parent process
    ;; can capture it.
    (jupyter-server-ioloop--recv-messages)
    (list 'connect-channels kernel-id)))

(defun jupyter-server-ioloop-add-disconnect-channels-event (ioloop)
  (jupyter-ioloop-add-event ioloop disconnect-channels (kernel-id)
    (let ((ws (jupyter-server-ioloop--kernel-ws kernel-id)))
      ;; See the note at the end of
      ;; `jupyter-server-ioloop-add-connect-channels-event'
      (jupyter-server-ioloop--recv-messages)
      (when ws
        (jupyter-server-ioloop--disconnect ws))
      (list 'disconnect-channels kernel-id))))

(provide 'jupyter-server-ioloop)

;;; jupyter-server-ioloop.el ends here
