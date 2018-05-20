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

(require 'skewer-mode)
(require 'jupyter-client)

(defvar jupyter-widgets-initialized nil)

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

(defun jupyter-widgets-sanitize-comm-msg (msg)
  "Ensure that a comm MSG's fields are not ambiguous before encoding.
For example, for fields that are supposed to be arrays, ensure
that they will be encoded as such. In addition, add fields required
by the JupyterLab widget manager."
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

(cl-defmethod jupyter-widgets-process-one-message ((client jupyter-widget-client))
  "Process one message in CLIENT's `widget-messages' slot.
A message is only processed if CLIENT's `widget-proc' slot is a
live process requesting a message. Schedule to process another
message if one is available."
  (when (process-live-p (oref client widget-proc))
    (let ((msg (pop (oref client widget-messages))))
      (when msg
        (with-temp-buffer
          (let ((proc (prog1 (oref client widget-proc)
                        (oset client widget-proc nil))))
            ;; FIXME: This is a bottleneck. Maybe only partially decode a
            ;; message instead of the full message.
            (insert (jupyter--encode msg))
            (httpd-send-header
             proc "text/json; charset=UTF-8" 200
             :Access-Control-Allow-Origin "*")))
        (run-at-time 0.001 nil #'jupyter-widgets-process-one-message client)))))

(cl-defmethod jupyter-widgets-queue-message ((client jupyter-widget-client) msg)
  "Queue a MSG to be processed by CLIENT's `widget-proc' at a later time."
  (let* ((msg (jupyter-widgets-sanitize-comm-msg msg))
         (msg-type (jupyter-message-type msg)))
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
    (oset client widget-messages
          (nconc (oref client widget-messages) (list msg)))))

(cl-defmethod jupyter-widgets-display-model ((client jupyter-widget-client) model-id)
  "Display the model with MODEL-ID for the kernel CLIENT is connected to."
  ;; NOTE: This is a message specific for this purpose and not really a
  ;; Jupyter message
  ;; (jupyter-widgets-clear-display client)
  (jupyter-widgets-queue-message
   client (list :msg_type "display_model"
                :content (list :model_id model-id)))
  (jupyter-widgets-process-one-message client))

(cl-defmethod jupyter-widgets-clear-display ((client jupyter-widget-client))
  "Clear the models being displayed for CLIENT."
  ;; NOTE: This is a message specific for this purpose and not really a
  ;; Jupyter message
  (jupyter-widgets-queue-message
   client (list :msg_type "clear_display")))

(defservlet jupyter "text/javascript; charset=UTF-8" ()
  (insert-file-contents
   (expand-file-name "js/built/index.built.js" jupyter-root)))

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
(defservlet* jupyter/widgets/:client-id "text/html; charset=UTF-8" ()
  (let ((client (jupyter-find-client-for-session client-id)))
    (if (not client)
        (error "No client found for ID (%s)" client-id)
      (insert-file-contents (expand-file-name "widget.html" jupyter-root))
      (dolist (key-replace `(("{username}" . ,user-login-name)
                             ("{client-id}" . ,client-id)
                             ("{widget-state}" . ,(oref client widget-state))))
        (goto-char (point-min))
        (while (search-forward (car key-replace) nil t)
          (replace-match (cdr key-replace)))))))

(defservlet* jupyter/widgets/send/:client-id "text/json; charset=UTF-8" ()
  "Send a message to the kernel whose session ID is CLIENT-ID.
The message is sent using the `jupyter-widget-client' whose
session ID is CLIENT-ID. Queue all messages generated by the sent
message to be sent back to `jupyter-widget-client's WIDGET-PROC."
  ;; TODO: How to avoid this step if it isn't needed?
  (let* ((msg
          (condition-case nil
              (jupyter-read-plist-from-string
               (cadr (assoc "Content" httpd-request)))
            (error (message (cadr (assoc "Content" httpd-request))))))
         (client (jupyter-find-client-for-session client-id))
         (channel (pcase (plist-get msg :channel)
                    ("shell" (oref client shell-channel))
                    ("iopub" (oref client iopub-channel))
                    ("stdin" (oref client stdin-channel))))
         (msg-type (jupyter-message-type-as-keyword
                    (jupyter-message-type msg)))
         (jupyter-inhibit-handlers
          ;; Only let the browser handle thee messages
          (when (memq msg-type '(:comm-info-request))
            '(:status :comm-info-reply))))
    ;; TODO: Remove the need for this special case
    (when (memq msg-type '(:comm-open :comm-close :comm-msg))
      (jupyter-widgets-sanitize-comm-msg msg))
    (let ((req (jupyter-send
                client channel msg-type (jupyter-message-content msg))))
      (jupyter-add-callback req
        t (lambda (msg)
            (jupyter-widgets-queue-message client msg)
            (jupyter-widgets-process-one-message client)))
      ;; FIXME: Bottleneck here since `jupyter-request-id' is a synchronizing
      ;; function.
      (insert (concat "{\"id\":\"" (jupyter-request-id req) "\"}")))))

(defun httpd/jupyter/widgets/recv (proc _path query &rest _args)
  "Queue the widget client PROC asking to receive a message.
QUERY should contain the key \"clientId\", which is the session
ID of the `jupyter-widget-client' which will provide messages to
PROC. If a message is already available, provide it to PROC."
  (let* ((client-id (or (cadr (assoc "clientId" query))
                        (error "No clientId specified")))
         (client (jupyter-find-client-for-session client-id)))
    (oset client widget-proc proc)
    (jupyter-widgets-process-one-message client)))

(defservlet* jupyter/widgets/state/:client-id "text/plain" ()
  "Save the state of a `jupyter-widget-client' whose session ID is CLIENT-ID."
  (let ((client (jupyter-find-client-for-session client-id)))
    (oset client widget-state (cadr (assoc "Content" httpd-request)))
    ;; The state is sent when the browser closes
    (jupyter-set client 'jupyter-widgets-initialized nil)
    (oset client widget-messages nil)))

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
        (unless (get-process "httpd")
          (httpd-start))
        (jupyter-set client 'jupyter-widgets-initialized t)
        (browse-url
         (format "http://127.0.0.1:%d/jupyter/widgets/%s"
                 httpd-port (jupyter-session-id (oref client session)))))
      (jupyter-widgets-queue-message client msg)
      (jupyter-widgets-process-one-message client)))
  (cl-call-next-method))

(cl-defmethod jupyter-handle-comm-close ((client jupyter-widget-client)
                                         req
                                         _id
                                         _data)
  (jupyter-widgets-queue-message client (jupyter-request-last-message req))
  (jupyter-widgets-process-one-message client)
  (cl-call-next-method))

(cl-defmethod jupyter-handle-comm-msg ((client jupyter-widget-client)
                                       req
                                       _id
                                       _data)
  (jupyter-widgets-queue-message client (jupyter-request-last-message req))
  (jupyter-widgets-process-one-message client)
  (cl-call-next-method))

(provide 'jupyter-widget-client)

;;; jupyter-widget-client.el ends here
