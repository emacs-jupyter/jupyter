;;; jupyter-widget-client.el --- Widget support -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 21 May 2018

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

;; Use an external browser to interact with Jupyter widgets.
;;
;; A `jupyter-kernel-client' does not come with any widget support by default,
;; the purpose of the `jupyter-widget-client' class is to provide such support.
;; This is done by opening an external browser and serving it the necessary
;; resources to display widgets using the `simple-httpd' package.  Emacs then
;; acts as an intermediary for the widget comm messages sent between the
;; browser and the kernel, communicating with the kernel through `zmq' and with
;; the browser through `websocket'.
;;
;; To add widget support to a client, subclass `jupyter-widget-client'.

;;; Code:

(require 'simple-httpd)
(require 'websocket)
(require 'jupyter-client)

(defvar jupyter-widgets-initialized nil
  "A client local variable that is non-nil if a browser for widgets is opened.")

(defvar jupyter-widgets-server nil
  "The `websocket-server' redirecting kernel messages.")

(defvar jupyter-widgets-port 8090
  "The port that `jupyter-widgets-server' listens on.")

(defvar jupyter-widgets-supported-targets '("jupyter.widget")
  "A list of the supported widget target names.")

(defvar jupyter-widgets-url-format
  "http://127.0.0.1:%d/jupyter/widgets?username=%s&clientId=%s&port=%d"
  "Format of the URL that will be visited to display widgets.")

(defclass jupyter-widget-client (jupyter-kernel-client)
  ((widget-sock
    :type (or null websocket)
    :initform nil
    :documentation "The `websocket' connected to the browser
displaying the widgets for this client.")
   (widget-state
    :type string
    :initform "null"
    :documentation "The JSON encode string representing the
widget state.  When a browser displaying the widgets of the client
is closed, the state of the widgets is sent back to Emacs so that
the state can be recovred when a new browser is opened.")
   (widget-messages
    :type list
    :initform nil
    :documentation "A list of pending messages to send to the
widget socket."))
  :abstract t)

;;; Websocket handlers

(defsubst jupyter-widgets--send-deferred (client)
  (cl-loop for msg in (nreverse (oref client widget-messages))
           do (websocket-send-text (oref client widget-sock) msg))
  (oset client widget-messages nil))

(defun jupyter-widgets-on-message (ws frame)
  "When websocket, WS, receives a message FRAME, handle it.
Send the contents of the message FRAME to the kernel and register
callbacks."
  (cl-assert (eq (websocket-frame-opcode frame) 'text))
  (let* ((msg (jupyter-read-plist-from-string
               (websocket-frame-payload frame)))
         (client (jupyter-find-client-for-session
                  (jupyter-message-session msg))))
    (cl-assert client)
    (unless (equal ws (oref client widget-sock))
      ;; TODO: Handle multiple clients and sending widget state to new clients
      (oset client widget-sock ws))
    (pcase (jupyter-message-type msg)
      ("connect"
       (jupyter-widgets--send-deferred client))
      (_
       ;; Any other message the browser sends is meant for the kernel so do the
       ;; redirection and setup the callbacks
       (let* ((msg-id (jupyter-message-id msg))
              (msg-type (jupyter-message-type-as-keyword
                         (jupyter-message-type msg)))
              (channel (pcase (plist-get msg :channel)
                         ("shell" :shell)
                         ("iopub" :iopub)
                         ("stdin" :stdin)
                         (_ (error "Invalid channel"))))
              (content (jupyter-message-content msg))
              (jupyter-inhibit-handlers
               ;; Only let the browser handle these messages
               (if (memq msg-type '(:comm-info-request))
                   '(:comm-msg :status :comm-info-reply)
                 '(:comm-msg)))
              (req (jupyter-send client channel msg-type content msg-id)))
         (jupyter-add-callback req
           '(:comm-open :comm-close :comm-info-reply :comm-msg :status)
           (apply-partially #'jupyter-widgets-send-message client)))))))

(defun jupyter-widgets-on-close (ws)
  "Uninitialize the client whose widget-sock is WS."
  (cl-loop
   for client in jupyter--clients
   when (and (object-of-class-p client 'jupyter-widget-client)
             (equal ws (oref client widget-sock)))
   do (oset client widget-sock nil)
   (jupyter-set client 'jupyter-widgets-initialized nil)))

;;; Working with comm messages

(defun jupyter-widgets-normalize-comm-msg (msg)
  "Ensure that a comm MSG's fields are not ambiguous before encoding.
For example, for fields that are supposed to be arrays, ensure
that they will be encoded as such.  In addition, add fields
required by the JupyterLab widget manager."
  (prog1 msg
    (when (memq (jupyter-message-type msg)
                '(:comm-open :comm-close :comm-msg))
      (let ((buffers (plist-member msg :buffers)))
        (if (null buffers) (plist-put msg :buffers [])
          (when (eq (cadr buffers) nil)
            (setcar (cdr buffers) [])))
        (unless (equal (cadr buffers) [])
          (setq buffers (cadr buffers))
          (while (car buffers)
            (setcar buffers
                    (base64-encode-string
                     (encode-coding-string (car buffers) 'utf-8-auto t) t))
            (setq buffers (cdr buffers))))
        ;; Needed by WidgetManager
        (unless (jupyter-message-metadata msg)
          (plist-put msg :metadata '(:version "2.0")))))))

(cl-defmethod jupyter-widgets-send-message ((client jupyter-widget-client) msg)
  "Send a MSG to CLIENT's `widget-sock' `websocket'."
  (setq msg (jupyter-widgets-normalize-comm-msg msg))
  (let ((msg-type (jupyter-message-type msg)))
    (plist-put msg :channel
               (cond
                ((memq msg-type '(:status :comm-msg :comm-close :comm-open))
                 :iopub)
                ((memq msg-type '(:comm-info-reply))
                 :shell)))
    (push (jupyter--encode msg) (oref client widget-messages))
    (when (websocket-openp (oref client widget-sock))
      (jupyter-widgets--send-deferred client))))

;;; Displaying widgets in the browser
;; NOTE: The "display_model" and "clear_display" messages below are not true
;; Jupyter messages, but are only used for communication between the browser
;; and Emacs.

(cl-defmethod jupyter-widgets-display-model ((client jupyter-widget-client) model-id)
  "Display the model with MODEL-ID for the kernel CLIENT is connected to."
  ;; (jupyter-widgets-clear-display client)
  (jupyter-widgets-send-message
   client (list :msg_type "display_model"
                :content (list :model_id model-id))))

(cl-defmethod jupyter-widgets-clear-display ((client jupyter-widget-client))
  "Clear the models being displayed for CLIENT."
  (jupyter-widgets-send-message client (list :msg_type "clear_display")))

;;; `jupyter-kernel-client' methods

(defun jupyter-widgets-start-websocket-server ()
  "Start the `jupyter-widgets-server' if necessary."
  (unless (process-live-p jupyter-widgets-server)
    (setq jupyter-widgets-server
          (websocket-server
           jupyter-widgets-port
           :host 'local
           :on-message #'jupyter-widgets-on-message
           :on-close #'jupyter-widgets-on-close))))

(defun jupyter-widgets--initialize-client (client)
  (unless (jupyter-get client 'jupyter-widgets-initialized)
    (jupyter-set client 'jupyter-widgets-initialized t)
    (unless (get-process "httpd")
      (httpd-start))
    (browse-url
     (format jupyter-widgets-url-format
             httpd-port
             user-login-name
             (jupyter-session-id (oref client session))
             jupyter-widgets-port))))

(cl-defmethod jupyter-handle-comm-open ((client jupyter-widget-client) _req msg)
  (jupyter-with-message-content msg (target-name)
    (when (member target-name jupyter-widgets-supported-targets)
      (jupyter-widgets-start-websocket-server)
      (jupyter-widgets--initialize-client client)
      (jupyter-widgets-send-message client msg)))
  (cl-call-next-method))

(cl-defmethod jupyter-handle-comm-close ((client jupyter-widget-client) _req msg)
  (jupyter-widgets-send-message client msg)
  (cl-call-next-method))

(cl-defmethod jupyter-handle-comm-msg ((client jupyter-widget-client) _req msg)
  (jupyter-widgets-send-message client msg)
  (cl-call-next-method))

;;; `httpd' interface

(defun httpd/jupyter (proc path _query &rest _args)
  "Serve the javascript required for Jupyter widget support.
PROC is the httpd process and PATH is the requested resource
path.  Currently no resources are accessible at any PATH other
than the root, which will serve the necessary Javascript to
load."
  (let ((split-path (split-string (substring path 1) "/")))
    (if (= (length split-path) 1)
        (with-httpd-buffer proc "text/javascript; charset=UTF-8"
          (insert-file-contents
           (expand-file-name "js/built/index.built.js" jupyter-root)))
      (error "Not found"))))

(defun httpd/jupyter/widgets/built (proc path _query &rest _args)
  "Serve the resources required by the widgets in the browser.
PROC is the httpd process and PATH is the requested resource
path.  Currently this will only serve a file from the js/built
directory if it has one of the extensions woff, woff2, ttf, svg,
or eot.  These are used by Jupyter."
  (let* ((split-path (split-string (substring path 1) "/"))
         (file (car (last split-path)))
         (mime (pcase (file-name-extension file)
                 ((or "woff" "woff2")
                  "application/font-woff")
                 ("ttf"
                  "application/octet-stream")
                 ("svg"
                  "image/svg+xml")
                 ("eot"
                  "application/vnd.ms-fontobject"))))
    (unless mime
      (error "Unsupported file type"))
    (setq file (expand-file-name (concat "js/built/" file) jupyter-root))
    ;; TODO: Fix this, when loading the files through httpd, font awesome
    ;; doesnt work
    (when (file-exists-p file)
      (error "File nonexistent (%s)" (file-name-nondirectory file)))
    (with-temp-buffer
      (insert-file-contents file)
      (httpd-send-header proc mime 200
                         :Access-Control-Allow-Origin "*"))))

;; TODO: Since the path when we instantiate widgets is jupyter/widgets, all
;; files that are trying to be loaded locally in the javascript will be
;; referenced to this path.  If we encounter a javascript file requesting to be
;; loaded we can automatically search the jupyter --paths for notebook
;; extension modules matching it.
(defun httpd/jupyter/widgets (proc &rest _args)
  "Serve the HTML page to display widgets.
PROC is the httpd process."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "widget.html" jupyter-root))
    (httpd-send-header
     proc "text/html; charset=UTF-8" 200
     :Access-Control-Allow-Origin "*")))

(provide 'jupyter-widget-client)

;;; jupyter-widget-client.el ends here
