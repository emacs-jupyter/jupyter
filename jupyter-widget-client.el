;;; jupyter-widget-client.el --- Widget support -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 21 May 2018
;; Version: 0.0.1
;; Keywords:

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

;;

;;; Code:

(require 'simple-httpd)
(require 'websocket)
(require 'jupyter-client)

(defvar jupyter-widgets-initialized nil)

(defvar jupyter-widgets-server nil
  "The `websocket-server' redirecting kernel messages.")

(defvar jupyter-widgets-port 8090
  "The port that `jupyter-widgets-server' listens on.")

(defclass jupyter-widget-client (jupyter-kernel-client)
  ((widget-proc
    :initform nil
    :documentation "The process currently requesting a message.")
   (widget-state
    :type string
    :initform "null"
    :documentation "The JSON encode string representing the
    widget state. When a browser displaying the widgets of the client
    is closed, the state of the widgets is sent back to Emacs so that
    the state can be recovred when a new browser is opened.")
   (widget-messages
    :type list
    :initform nil
    :documentation "A list of messages to send to the widget process."))
  :abstract t)

(defun jupyter-on-message (ws frame)
  (cl-assert (eq (websocket-frame-opcode frame) 'text))
  (let* ((msg (jupyter-read-plist-from-string
               (websocket-frame-payload frame)))
         (client (jupyter-find-client-for-session
                  (jupyter-message-session msg))))
    (cl-assert client)
    (unless (equal ws (oref client widget-proc))
      ;; TODO: Handle multiple clients and sending
      ;; widget state to new clients
      (oset client widget-proc ws))
    ;; The widget client sends a connect message so that Emacs knows
    ;; which websocket to use, do not do any processing when we
    ;; received this message.
    (when (equal (jupyter-message-type msg) "connect")
      (cl-loop for msg in (nreverse (oref client widget-messages))
               do (websocket-send-text ws msg))
      (oset client widget-messages nil))
    (unless (equal (jupyter-message-type msg) "connect")
      (let* ((msg-id (jupyter-message-id msg))
             (msg-type (jupyter-message-type-as-keyword
                        (jupyter-message-type msg)))
             (channel (pcase (plist-get msg :channel)
                        ("shell" (oref client shell-channel))
                        ("iopub" (oref client iopub-channel))
                        ("stdin" (oref client stdin-channel))))
             (content (jupyter-message-content msg))
             (jupyter-inhibit-handlers
              ;; Only let the browser handle thee
              ;; messages
              (append '(:comm-msg)
                      (when (memq msg-type '(:comm-info-request))
                        '(:status :comm-info-reply))))
             (req (jupyter-send client channel msg-type content msg-id)))
        (jupyter-add-callback req
          '(:comm-open :comm-close :comm-info-reply :comm-msg :status)
          (apply-partially #'jupyter-widgets-send-message client))))))

(cl-defmethod initialize-instance ((client jupyter-widget-client) &rest _args)
  (unless (process-live-p jupyter-widgets-server)
    (setq jupyter-widgets-server
          (websocket-server
           jupyter-widgets-port
           :host 'local
           :on-message #'jupyter-on-message
           :on-close
           (lambda (ws)
             (cl-loop
              for client in jupyter--clients
              when (and (obj-of-class-p client 'jupyter-widget-client)
                        (equal ws (oref client widget-proc)))
              do (oset client widget-proc nil)
              (jupyter-set client 'jupyter-widgets-initialized nil))))))
  (cl-call-next-method))

(defun jupyter-widgets-sanitize-comm-msg (msg)
  "Ensure that a comm MSG's fields are not ambiguous before encoding.
For example, for fields that are supposed to be arrays, ensure
that they will be encoded as such. In addition, add fields
required by the JupyterLab widget manager."
  (prog1 msg
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
      (unless (plist-get msg :metadata)
        (plist-put msg :metadata '(:version "2.0"))))))

(cl-defmethod jupyter-widgets-send-message ((client jupyter-widget-client) msg)
  "Send a MSG to CLIENT's `widget-proc' `websocket'."
  (when (memq (jupyter-message-type msg)
              '(:comm-open :comm-close :comm-msg))
    (jupyter-widgets-sanitize-comm-msg msg))
  (let ((msg-type (jupyter-message-type msg)))
    ;; FIXME: The :date field is an emacs time object, i.e. a 4 element list,
    ;; convert to an actual time.
    ;; We don't have a channel field, but KernelFutureHandler.handleMsg
    ;; of jupyterlab requires it
    (plist-put msg :channel
               (cond
                ((memq msg-type '(:status :comm-msg :comm-close :comm-open))
                 :iopub)
                ((memq msg-type '(:comm-info-reply))
                 :shell)))
    ;; TODO: Do not let this grow without bound
    (push (jupyter--encode msg) (oref client widget-messages))
    (when (websocket-openp (oref client widget-proc))
      (cl-loop for msg in (nreverse (oref client widget-messages))
               do (websocket-send-text
                   (oref client widget-proc) msg))
      (oset client widget-messages nil))))

(cl-defmethod jupyter-widgets-display-model ((client jupyter-widget-client) model-id)
  "Display the model with MODEL-ID for the kernel CLIENT is connected to."
  ;; NOTE: This is a message specific for this purpose and not really a
  ;; Jupyter message
  ;; (jupyter-widgets-clear-display client)
  (jupyter-widgets-send-message
   client (list :msg_type "display_model"
                :content (list :model_id model-id))))

(cl-defmethod jupyter-widgets-clear-display ((client jupyter-widget-client))
  "Clear the models being displayed for CLIENT."
  ;; NOTE: This is a message specific for this purpose and not really a
  ;; Jupyter message
  (jupyter-widgets-send-message client (list :msg_type "clear_display")))

(defun httpd/jupyter (proc path query &rest _args)
  (let ((split-path (split-string (substring path 1) "/")))
    (if (= (length split-path) 1)
        (with-httpd-buffer proc "text/javascript; charset=UTF-8"
          (insert-file-contents
           (expand-file-name "js/built/index.built.js" jupyter-root)))
      (error "Not found"))))

(defun httpd/jupyter/widgets/built (proc path query &rest _args)
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
;; referenced to this path. If we encounter a javascript file requesting to be
;; loaded we can automatically search the jupyter --paths for notebook
;; extension modules matching it.
(defun httpd/jupyter/widgets (proc &rest _args)
  (with-temp-buffer
    (insert-file-contents (expand-file-name "widget.html" jupyter-root))
    (httpd-send-header
     proc "text/html; charset=UTF-8" 200
     :Access-Control-Allow-Origin "*")))

(cl-defmethod jupyter-handle-comm-open ((client jupyter-widget-client)
                                        req
                                        _id
                                        _target-name
                                        _target-module
                                        _data)
  (let ((msg (jupyter-request-last-message req)))
    (when (member (jupyter-message-get msg :target_name)
                  '("jupyter.widget"))
      (unless (jupyter-get client 'jupyter-widgets-initialized)
        (jupyter-set client 'jupyter-widgets-initialized t)
        (unless (get-process "httpd")
          (httpd-start))
        (browse-url
         (format "http://127.0.0.1:%d/jupyter/widgets?username=%s&clientId=%s&port=%d"
                 httpd-port
                 user-login-name
                 (jupyter-session-id (oref client session))
                 jupyter-widgets-port)))
      (jupyter-widgets-send-message client msg)))
  (cl-call-next-method))

(cl-defmethod jupyter-handle-comm-close ((client jupyter-widget-client)
                                         req
                                         _id
                                         _data)
  (jupyter-widgets-send-message client (jupyter-request-last-message req))
  (cl-call-next-method))

(cl-defmethod jupyter-handle-comm-msg ((client jupyter-widget-client)
                                       req
                                       _id
                                       _data)
  (jupyter-widgets-send-message client (jupyter-request-last-message req))
  (cl-call-next-method))

(provide 'jupyter-widget-client)

;;; jupyter-widget-client.el ends here
