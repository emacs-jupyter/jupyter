;;; jupyter-messages.el --- Jupyter messages -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 08 Jan 2018
;; Version: 0.0.1
;; X-URL: https://github.com/nathan/jupyter-messages

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

(require 'jupyter-base)
(require 'jupyter-channels)

(defgroup jupyter-messages nil
  "Jupyter messages"
  :group 'communication)

(defconst jupyter-message-delimiter "<IDS|MSG>"
  "The message delimiter required in the jupyter messaging
protocol.")

(defconst jupyter--false :json-false
  "The symbol used to disambiguate nil from boolean false.")

;;; Signing messages

(defun jupyter--sign-message (session parts)
  (if (> (length (jupyter-session-key session)) 0)
      (cl-loop
       for b across (hmac-sha256 (mapconcat #'identity parts "")
                                 (jupyter-session-key session))
       concat (format "%02x" b))
    ""))

(defun jupyter--split-identities (parts)
  "Extract the identities from a list of message PARTS."
  (let ((idents nil))
    (if (catch 'found-delim
          (while (car parts)
            (when (string= (car parts) jupyter-message-delimiter)
              (setq parts (cdr parts)
                    idents (nreverse idents))
              (throw 'found-delim t))
            (setq idents (cons (car parts) idents)
                  parts (cdr parts))))
        (cons idents parts)
      (error "Message delimiter not in message list"))))

(defun jupyter--message-header (session msg-type)
  (list
   :msg_id (jupyter-new-uuid)
   :msg_type msg-type
   :version jupyter-protocol-version
   :username user-login-name
   :session (jupyter-session-id session)
   :date (format-time-string "%FT%T.%6N%z" (current-time))))

;;; Encode/decoding messages

(defun jupyter--encode-object (object)
  ;; Fix encoding recursive objects so that nil will get turned into "{}"
  (cl-letf (((symbol-function 'json-encode-keyword)
             (lambda (keyword)
               (cond ((eq keyword t)          "true")
                     ((eq keyword json-false) "false")
                     ((eq keyword json-null)  "{}")))))
    (encode-coding-string (json-encode-plist object) 'utf-8)))

(defun jupyter--decode-string (str)
  (let ((json-object-type 'plist)
        (json-array-type 'list)
        (json-false nil))
    (json-read-from-string (decode-coding-string str 'utf-8))))

(defun jupyter--decode-time (str)
  (let* ((time (date-to-time str)))
    (when (string-match "T.+\\(\\(?:\\.\\|,\\)[0-9]+\\)" str)
      (setq time (list (car time)
                       (cadr time)
                       (ceiling (* 1000000 (string-to-number
                                            (match-string 1 str))))
                       0)))
    time))

(cl-defun jupyter--encode-message (session
                                   type
                                   &key idents
                                   content
                                   parent-header
                                   metadata
                                   buffers)
  (declare (indent 2))
  (cl-check-type session jupyter-session)
  (cl-check-type metadata json-plist)
  (cl-check-type content json-plist)
  (cl-check-type buffers list)
  (let* ((header (jupyter--message-header session type))
         (msg-id (plist-get header :msg_id))
         (parts (mapcar #'jupyter--encode-object (list header
                                                  parent-header
                                                  metadata
                                                  content))))
    (cons msg-id
          (append
           (when idents (if (stringp idents) (list idents) idents))
           (list jupyter-message-delimiter
                 (jupyter--sign-message session parts))
           parts
           buffers))))

(defun jupyter--decode-message (session parts)
  (when (< (length parts) 5)
    (error "Malformed message. Minimum length of parts is 5"))
  (when (jupyter-session-key session)
    (let ((signature (car parts)))
      (when (seq-empty-p signature)
        (error "Unsigned message"))
      ;; TODO: digest_history
      ;; https://github.com/jupyter/jupyter_client/blob/7a0278af7c1652ac32356d6f00ae29d24d78e61c/jupyter_client/session.py#L915
      (unless (string= (jupyter--sign-message session (seq-subseq parts 1 5))
                       signature)
        (error "Invalid signature: %s" signature))))
  (cl-destructuring-bind
      (header parent-header metadata content &optional buffers)
      (cdr parts)
    (let ((header (jupyter--decode-string header))
          (parent-header (jupyter--decode-string parent-header)))
      ;; Decode dates to time objects as returned by `current-time'
      (mapc (lambda (plist)
           (let ((date (plist-get plist :date)))
             (when date
               (plist-put
                plist :date (jupyter--decode-time date)))))
         (list header parent-header))
      (list
       :header header
       :msg_id (plist-get header :msg_id)
       :msg_type (plist-get header :msg_type)
       :parent_header parent-header
       :metadata (jupyter--decode-string metadata)
       :content (jupyter--decode-string content)
       :buffers buffers))))

;;; Sending/receiving

(cl-defmethod jupyter-send ((session jupyter-session)
                            socket
                            type
                            message
                            &optional flags)
  (declare (indent 1))
  (cl-destructuring-bind (msg-id . msg)
      (jupyter--encode-message session type :content message)
    ;; TODO: Check for EAGAIN and reschedule the message for sending
    (zmq-send-multipart socket msg flags)
    msg-id))

(cl-defmethod jupyter-recv ((session jupyter-session) socket &optional flags)
  (let ((msg (zmq-recv-multipart socket flags)))
    (when msg
      (cl-destructuring-bind (idents . parts)
          (jupyter--split-identities msg)
        (cons idents (jupyter--decode-message session parts))))))

;;; Control messages

(cl-defun jupyter-message-interrupt-request ()
  (list))

;;; stdin messages

(cl-defun jupyter-message-input-reply (&key value)
  (cl-check-type value string)
  (list :value value))

;;; shell messages

(cl-defun jupyter-message-kernel-info-request ()
  (list))

(cl-defun jupyter-message-execute-request (&key
                                           code
                                           (silent nil)
                                           (store-history t)
                                           (user-expressions nil)
                                           (allow-stdin t)
                                           (stop-on-error nil))
  (cl-check-type code string)
  (cl-check-type user-expressions json-plist)
  (list :code code :silent (if silent t jupyter--false)
        :store_history (if store-history t jupyter--false)
        :user_expressions user-expressions
        :allow_stdin (if allow-stdin t jupyter--false)
        :stop_on_error (if stop-on-error t jupyter--false)))

(cl-defun jupyter-message-inspect-request (&key code pos detail)
  (setq detail (or detail 0))
  (unless (member detail '(0 1))
    (error "Detail can only be 0 or 1 (%s)" detail))
  (when (markerp pos)
    (setq pos (marker-position pos)))
  (cl-check-type code string)
  (cl-check-type pos integer)
  (list :code code :cursor_pos pos :detail_level detail))

(cl-defun jupyter-message-complete-request (&key code pos)
  (when (markerp pos)
    (setq pos (marker-position pos)))
  (cl-check-type code string)
  (cl-check-type pos integer)
  (list :code code :cursor_pos pos))

(cl-defun jupyter-message-history-request (&key
                                           output
                                           raw
                                           hist-access-type
                                           session
                                           start
                                           stop
                                           n
                                           pattern
                                           unique)
  (unless (member hist-access-type '("range" "tail" "search"))
    (error "History access type can only be one of (range, tail, search)"))
  (append
   (list :output (if output t jupyter--false) :raw (if raw t jupyter--false)
         :hist_access_type hist-access-type)
   (cond
    ((equal hist-access-type "range")
     (cl-check-type session integer)
     (cl-check-type start integer)
     (cl-check-type stop integer)
     (list :session session :start start :stop stop))
    ((equal hist-access-type "tail")
     (cl-check-type n integer)
     (list :n n))
    ((equal hist-access-type "search")
     (cl-check-type pattern string)
     (cl-check-type n integer)
     (list :pattern pattern :unique (if unique t jupyter--false) :n n)))))

(cl-defun jupyter-message-is-complete-request (&key code)
  (cl-check-type code string)
  (list :code code))

(cl-defun jupyter-message-comm-info-request (&key target-name)
  (when target-name
    (cl-check-type target-name string)
    (list :target_name target-name)))

(cl-defun jupyter-message-shutdown-request (&key restart)
  (list :restart (if restart t jupyter--false)))

;;; Convenience functions

(defsubst jupyter-message-id (msg)
  "Get the ID of MSG."
  (plist-get msg :msg_id))

(defsubst jupyter-message-parent-id (msg)
  "Get the parent ID of MSG."
  (jupyter-message-id (plist-get msg :parent_header)))

(defsubst jupyter-message-type (msg)
  "Get the MSG type."
  (plist-get msg :msg_type))

(defsubst jupyter-message-content (msg)
  "Get the MSG contents."
  (plist-get msg :content))

(defsubst jupyter-message-time (msg)
  "Get the MSG time.
The returned time has the same form as returned by
`current-time'."
  (plist-get (plist-get msg :header) :date))

(defsubst jupyter-message-get (msg key)
  "Get the value of KEY in the `jupyter-message-content' of MSG."
  (plist-get (jupyter-message-content msg) key))

(defsubst jupyter-message-data (msg mimetype)
  "Get one of the MIMETYPE's in MSG's data.
If MSG does not have a data key in its contents or the MIMETYPE
does not exist in MSG's data, return nil. Otherwise return the
value of the MIMETYPE in MSG's data."
  (plist-get (jupyter-message-get msg :data) mimetype))

(defun jupyter-message-status-idle-p (msg)
  (and (equal (jupyter-message-type msg) "status")
       (equal (jupyter-message-get msg :execution_state) "idle")))

(provide 'jupyter-messages)

;;; jupyter-messages.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
