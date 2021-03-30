;;; jupyter-messages.el --- Jupyter messages -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 08 Jan 2018

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

;; Routines to sign, encode, decode, send, and receive Jupyter messages.
;; Messages are represented as property lists, the contents of a message should
;; never be accessed directly since decoding of a message's contents is done on
;; demand.  You access the message contents through `jupyter-message-content',
;; `jupyter-message-header', `jupyter-message-metadata', etc.
;;
;; There are convenience macros: `jupyter-with-message-content' and
;; `jupyter-with-message-data'.
;;
;; There are many convenience functions: `jupyter-message-data',
;; `jupyter-message-get', `jupyter-message-type',
;; `jupyter-message-status-idle-p', etc.
;;
;; See the "Convenience functions and macros" section.

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'jupyter-base)
(require 'hmac-def)
(require 'json)

(defgroup jupyter-messages nil
  "Jupyter messages"
  :group 'jupyter)

(defconst jupyter-message-delimiter "<IDS|MSG>"
  "The message delimiter required in the jupyter messaging protocol.")

(defconst jupyter--false :json-false
  "The symbol used to disambiguate nil from boolean false.")

(defconst jupyter--empty-dict (make-hash-table :size 1)
  "An empty hash table to disambiguate nil during encoding.
Message parts that are nil, but should be encoded into an empty
dictionary are set to this value so that they are encoded as
dictionaries.")

;;; UUID

(defun jupyter-new-uuid ()
  "Return a version 4 UUID."
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

;;; Signing messages

(defun jupyter-sha256 (object)
  "Return the SHA256 hash of OBJECT."
  (secure-hash 'sha256 object nil nil t))

(define-hmac-function jupyter-hmac-sha256 jupyter-sha256 64 32)

(cl-defun jupyter-sign-message (session parts &optional (signer #'jupyter-hmac-sha256))
  "Use SESSION to sign message PARTS.
Return the signature of PARTS.  PARTS should be in the order of a
valid Jupyter message, see `jupyter-decode-message'.  SIGNER is
the message signing function and should take two arguments, the
text to sign and the key used for signing.  The default value
signs messages using `jupyter-hmac-sha256'."
  (if (> (length (jupyter-session-key session)) 0)
      (cl-loop
       ;; NOTE: Encoding to a unibyte representation due to an "Attempt to
       ;; change byte length of a string" error.
       with key = (encode-coding-string
                   (jupyter-session-key session) 'utf-8 t)
       with parts = (encode-coding-string
                     (cl-loop for part in parts concat part)
                     'utf-8 t)
       for byte across (funcall signer parts key)
       concat (format "%02x" byte))
    ""))

(defun jupyter--split-identities (parts)
  "Extract the identities from a list of message PARTS.
Return a cons cell (IDENTS . REST-PARTS)."
  (or (cl-loop
       for (part . rest-parts) on parts by #'cdr
       if (equal part jupyter-message-delimiter)
       return (cons idents rest-parts)
       else collect part into idents)
      (error "Message delimiter not in message list")))

(defun jupyter--message-header (session msg-type msg-id)
  "Return a message header.
The `:session' key of the header will have its value set to
SESSION's ID, and its `:msg_type' will be set to MSG-TYPE.  MSG-ID
will be set to the value of the `:msg_id' key.  The other fields
of the returned plist are `:version', `:username', and `:date'.
They are all set to appropriate default values."
  (list
   :msg_id msg-id
   :msg_type msg-type
   :version jupyter-protocol-version
   :username user-login-name
   :session (jupyter-session-id session)
   :date (format-time-string "%FT%T.%6N%z" (current-time))))

;;; Encode/decoding messages

(defun jupyter--encode (part)
  "Encode PART into a JSON string.
Take into account `jupyter-message-type' keywords by replacing
them with their appropriate message type strings according to the
Jupyter messaging spec.  After encoding into a JSON
representation, return the UTF-8 encoded string.

If PART is a string, return the UTF-8 encoded string without
encoding into JSON first.

If PART is a list whose first element is the symbol,
`message-part', then return the second element of the list if it
is non-nil.  If it is nil, then set the list's second element to
the result of calling `jupyter--encode' on the third element and
return the result."
  (let ((json-recursive-encoder-sym (if (fboundp 'json--print) 'json--print 'json-encode)))
    (unwind-protect
        (progn
          (add-function :around (symbol-function json-recursive-encoder-sym) #'jupyter--json-encode-preproc)
          (encode-coding-string
           (cond
            ((stringp part) part)
            (t (json-encode part)))
           'utf-8 t))
      (remove-function (symbol-function json-recursive-encoder-sym) #'jupyter--json-encode-preproc))))

(defun jupyter--json-encode-preproc (old-json-recursive-encoder object)
  (let (msg-type)
    (cl-flet ((json-encode
               (object)
               (jupyter--json-encode-preproc old-json-recursive-encoder object)))
     (cond
      ((eq (car-safe object) 'message-part)
       (cl-destructuring-bind (_ encoded-rep decoded-rep) object
         (or encoded-rep (setf (nth 1 object) (json-encode decoded-rep)))))
      ((and (keywordp object)
            (setf msg-type (plist-get jupyter-message-types object)))
       (json-encode msg-type))
      ((and (listp object)
            (= (length object) 4)
            (cl-every #'integerp object))
       (jupyter-encode-time object))
      (t (funcall old-json-recursive-encoder object))))))

(defun jupyter--decode (part)
  "Decode a message PART.

If PART is a list whose first element is the symbol,
`message-part', then return the third element of the list if it
is non-nil.  If it is nil, then set the list's third element to
the result of calling `jupyter--decode' on the second element and
return the result.

Otherwise, if PART is a string decode it using UTF-8 encoding and
read it as a JSON string.  If it is not valid JSON, return the
decoded string."
  (if (eq (car-safe part) 'message-part)
      (cl-destructuring-bind (_ encoded-rep decoded-rep) part
        (or decoded-rep (setf (nth 2 part) (jupyter--decode encoded-rep))))
    (let* ((json-object-type 'plist)
           (str (decode-coding-string part 'utf-8))
           (val (condition-case nil
                    (json-read-from-string str)
                  ;; If it can't be read as JSON, assume its just a regular
                  ;; string
                  (json-unknown-keyword str))))
      (prog1 val
        (when-let* ((msg-type (and (listp val)
                                   (plist-get val :msg_type))))
          (plist-put
           val :msg_type (jupyter-message-type-as-keyword msg-type)))))))

(defun jupyter-decode-time (str)
  "Decode an ISO 8601 time STR into a time object.
The returned object has the same form as the object returned by
`current-time'."
  (unless (string-match-p "T[^.,Z+-]+" str)
    (setq str (concat str "T00:00:00")))
  (save-match-data
    (string-match "T[^.,Z+-]+\\([.,]\\([0-9]+\\)\\)" str)
    (let ((fraction (match-string 2 str)))
      (when fraction
        (setq str (replace-match "" nil nil str 1)))
      (nconc (parse-iso8601-time-string str)
             (if fraction
                 (let* ((plen (- 6 (length fraction)))
                        (pad (and (> plen 0) (expt 10 plen))))
                   (list (if pad (* pad (string-to-number fraction))
                           (string-to-number (substring fraction 0 6)))
                         0))
               (list 0 0))))))

(defun jupyter-encode-time (time)
  "Encode TIME into an ISO 8601 time string."
  (format-time-string "%FT%T.%6N" time t))

(cl-defun jupyter-encode-raw-message (session
                                      type
                                      &rest plist
                                      &key
                                      content
                                      (msg-id (jupyter-new-uuid))
                                      parent-header
                                      metadata
                                      buffers
                                      &allow-other-keys)
  "Encode a message into a JSON string.
Similar to `jupyter-encode-message', but returns the JSON encoded
string instead of a list of the encoded parts.

PLIST is an extra property list added to the top level of the
message before encoding."
  (declare (indent 2))
  (cl-check-type session jupyter-session)
  (cl-check-type metadata json-plist)
  (cl-check-type content json-plist)
  (cl-check-type parent-header json-plist)
  (cl-check-type buffers list)
  (or content (setq content jupyter--empty-dict))
  (or parent-header (setq parent-header jupyter--empty-dict))
  (or metadata (setq metadata jupyter--empty-dict))
  (or buffers (setq buffers []))
  (let (fplist)
    (while plist
      (cond
       ((memq (car plist)
              '(:content :parent-header :metadata :buffers :msg-id))
        (pop plist)
        (pop plist))
       (t
        (push (prog1 (pop plist)
                (push (pop plist) fplist))
              fplist))))
    (jupyter--encode
     (cl-list*
      :parent_header parent-header
      :header (jupyter--message-header session type msg-id)
      :content content
      :metadata metadata
      :buffers buffers
      fplist))))

(cl-defun jupyter-encode-message (session
                                  type
                                  &key idents
                                  content
                                  (msg-id (jupyter-new-uuid))
                                  parent-header
                                  metadata
                                  buffers
                                  (signer #'jupyter-hmac-sha256))
  (declare (indent 2))
  (cl-check-type session jupyter-session)
  (cl-check-type metadata json-plist)
  (cl-check-type content json-plist)
  (cl-check-type parent-header json-plist)
  (cl-check-type buffers list)
  (or content (setq content jupyter--empty-dict))
  (or parent-header (setq parent-header jupyter--empty-dict))
  (or metadata (setq metadata jupyter--empty-dict))
  (and (stringp idents) (setq idents (list idents)))

  (let ((parts (mapcar #'jupyter--encode
                  (list (jupyter--message-header session type msg-id)
                        parent-header
                        metadata
                        content))))
    (nconc (cl-list* msg-id idents)
           (cl-list* jupyter-message-delimiter
                     (jupyter-sign-message session parts signer)
                     parts)
           buffers)))

(cl-defun jupyter-decode-message (session parts &key (signer #'jupyter-hmac-sha256))
  "Use SESSION to decode message PARTS.
PARTS should be a list of message parts in the order of a valid
Jupyter message, i.e. a list of the form

    (signature header parent-header metadata content buffers...)

If SESSION supports signing messages, then the signature
resulting from the signing of (cdr PARTS) using SESSION should be
equal to SIGNATURE.  An error is thrown if it is not.

If SIGNER is non-nil it should be a function used to sign the
message.  Otherwise the default signing function is used, see
`jupyter-sign-message'.

The returned plist has elements of the form

    (message-part JSON PLIST)

for the keys `:header', `:parent-header', `:metadata', and
`:content'.  JSON is the JSON encoded string of the message part.
For `:header' and `:parent-header', PLIST will be the decoded
message PLIST for the part.  The other message parts are decoded
into property lists on demand, i.e. after a call to
`jupyter-message-metadata' or `jupyter-message-content' PLIST
will be decoded message part.

The binary buffers are left unchanged and will be the value of
the `:buffers' key in the returned plist.  Also, the message ID
and type are available in the top level of the plist as `:msg_id'
and `:msg_type'."
  (when (< (length parts) 5)
    (error "Malformed message.  Minimum length of parts is 5"))
  (when (jupyter-session-key session)
    (let ((signature (car parts)))
      (when (= (length signature) 0)
        (error "Unsigned message"))
      ;; TODO: digest_history
      ;; https://github.com/jupyter/jupyter_client/blob/7a0278af7c1652ac32356d6f00ae29d24d78e61c/jupyter_client/session.py#L915
      (unless (string= (jupyter-sign-message session (cdr parts) signer) signature)
        (error "Invalid signature: %s" signature))))
  (cl-destructuring-bind
      (header parent-header metadata content &rest buffers)
      (cdr parts)
    (let ((dheader (jupyter--decode header)))
      (list
       :header (list 'message-part header dheader)
       :msg_id (plist-get dheader :msg_id)
       :msg_type (plist-get dheader :msg_type)
       ;; Also decode the parent header here since it is used quite often in
       ;; the parent Emacs process
       :parent_header (list 'message-part parent-header
                            (jupyter--decode parent-header))
       :metadata (list 'message-part metadata nil)
       :content (list 'message-part content nil)
       :buffers buffers))))

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
        :user_expressions (or user-expressions jupyter--empty-dict)
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

(cl-defun jupyter-message-comm-open (&key id target-name data)
  (cl-check-type id string)
  (cl-check-type target-name string)
  (cl-check-type data json-plist)
  (list :comm_id id :target_name target-name :data data))

(cl-defun jupyter-message-comm-msg (&key id data)
  (cl-check-type id string)
  (cl-check-type data json-plist)
  (list :comm_id id :data data))

(cl-defun jupyter-message-comm-close (&key id data)
  (cl-check-type id string)
  (cl-check-type data json-plist)
  (list :comm_id id :data data))

(cl-defun jupyter-message-shutdown-request (&key restart)
  (list :restart (if restart t jupyter--false)))

;;; Convenience functions and macros

(defmacro jupyter-with-message-content (msg keys &rest body)
  "For MSG, bind the corresponding KEYS of its contents then evaluate BODY.
KEYS is a list of key names found in the
`jupyter-message-content' of MSG.  The values are bound to their
key names while evaluating BODY.

So to bind the :status key of MSG you would do

    (jupyter-with-message-content msg (status)
      BODY)"
  (declare (indent 2) (debug (form listp body)))
  (if keys
      `(cl-destructuring-bind (&key ,@keys &allow-other-keys)
           (jupyter-message-content ,msg)
         ,@body)
    `(progn ,@body)))

(defmacro jupyter-with-message-data (msg varlist &rest body)
  "For MSG, bind the mimetypes in VARLIST and evaluate BODY.
VARLIST has a similar form to the VARLIST of a `let' binding
except the `cadr' of each binding is a mimetype that will be
passed to `jupyter-message-data'.

So to bind the :text/plain mimetype of MSG to a variable, res,
you would do

    (jupyter-with-message-data msg ((res text/plain))
      BODY)"
  (declare (indent 2) (debug (form (&rest (symbolp symbolp)) body)))
  (let* ((m (make-symbol "msg"))
         (vars
          (mapcar (lambda (el)
               (list (car el)
                     `(jupyter-message-data
                       ,m ',(if (keywordp (cadr el)) (cadr el)
                              (intern (concat  ":" (symbol-name (cadr el))))))))
             varlist)))
    (if vars `(let* ((,m ,msg) ,@vars)
                ,@body)
      `(progn ,@body))))

(defmacro jupyter-message-lambda (keys &rest body)
  "Return a function binding KEYS to fields of a message then evaluating BODY.
The returned function takes a single argument which is expected
to be a Jupyter message property list.

The elements of KEYS can either be a symbol, KEY, or a two
element list (VAL MIMETYPE).  In the former case, KEY will be
bound to the corresponding value of KEY in the
`jupyter-message-content' of the message argument.  In the latter
case, VAL will be bound to the value of the MIMETYPE found in the
`jupyter-message-data' of the message."
  (declare (indent defun) (debug ((&rest [&or symbolp (symbolp symbolp)]) body)))
  (let ((msg (cl-gensym "msg"))
        content-keys
        data-keys)
    (while (car keys)
      (let ((key (pop keys)))
        (push key (if (listp key) data-keys content-keys))))
    `(lambda (,msg)
       ,(cond
         ((and data-keys content-keys)
          `(jupyter-with-message-content ,msg ,content-keys
             (jupyter-with-message-data ,msg ,data-keys
               ,@body)))
         (data-keys
          `(jupyter-with-message-data ,msg ,data-keys
             ,@body))
         (content-keys
          `(jupyter-with-message-content ,msg ,content-keys
             ,@body))
         (t
          `(progn ,@body))))))

(defmacro jupyter--decode-message-part (key msg)
  "Return a form to decode the value of KEY in MSG.
If the value of KEY is a list whose first element is the symbol
`message-part', then if the the third element of the list is nil
set it to the result of calling `jupyter--decode' on the second
element.  If the third element is non-nil, return it.  Otherwise
return the value of KEY in MSG."
  `(let ((part (plist-get ,msg ,key)))
     (if (and (consp part) (eq (car part) 'message-part))
         (or (nth 2 part) (jupyter--decode part))
       part)))

(defun jupyter-message-header (msg)
  "Get the header of MSG."
  (jupyter--decode-message-part :header msg))

(defun jupyter-message-parent-header (msg)
  "Get the parent header of MSG."
  (jupyter--decode-message-part :parent_header msg))

(defun jupyter-message-metadata (msg)
  "Get the metadata key of MSG."
  (jupyter--decode-message-part :metadata msg))

(defun jupyter-message-content (msg)
  "Get the MSG contents."
  (jupyter--decode-message-part :content msg))

(defsubst jupyter-message-id (msg)
  "Get the ID of MSG."
  (or (plist-get msg :msg_id)
      (plist-get (jupyter-message-header msg) :msg_id)))

(defsubst jupyter-message-parent-id (msg)
  "Get the parent ID of MSG."
  (jupyter-message-id (jupyter-message-parent-header msg)))

(defsubst jupyter-message-type (msg)
  "Get the type of MSG."
  (or (plist-get msg :msg_type)
      (plist-get (jupyter-message-header msg) :msg_type)))

(defsubst jupyter-message-session (msg)
  "Get the session ID of MSG."
  (plist-get (jupyter-message-header msg) :session))

(defsubst jupyter-message-parent-type (msg)
  "Get the type of MSG's parent message."
  (jupyter-message-type (jupyter-message-parent-header msg)))

(defun jupyter-message-type-as-keyword (msg-type)
  "Return MSG-TYPE as one of the keys in `jupyter-message-types'.
If MSG-TYPE is already a valid message type keyword, return it.
Otherwise return the MSG-TYPE string as a keyword."
  (if (keywordp msg-type)
      (if (plist-get jupyter-message-types msg-type) msg-type
        (error "Invalid message type (`%s')" msg-type))
    (or (cl-loop
         for (k v) on jupyter-message-types by #'cddr
         thereis (and (string= msg-type v) k))
        (error "Invalid message type (`%s')" msg-type))))

(defun jupyter-message-time (msg)
  "Get the MSG time.
The returned time has the same form as returned by
`current-time'."
  (let* ((header (jupyter-message-header msg))
         (date (plist-member header :data)))
    (when (stringp (car date))
      (setcar date (jupyter-decode-time (car date))))
    (car date)))

(defsubst jupyter-message-get (msg key)
  "Get the value in MSG's `jupyter-message-content' that corresponds to KEY."
  (plist-get (jupyter-message-content msg) key))

(defsubst jupyter-message-data (msg mimetype)
  "Get the message data for a specific mimetype.
MSG should be a message with a `:data' field in its contents.
MIMETYPE is should be a standard media mimetype
keyword (`:text/plain', `:image/png', ...).  If the messages data
has a key corresponding to MIMETYPE, return the value.  Otherwise
return nil."
  (plist-get (jupyter-message-get msg :data) mimetype))

(defsubst jupyter-message-status-idle-p (msg)
  "Determine if MSG is a status: idle message."
  (and (eq (jupyter-message-type msg) :status)
       (equal (jupyter-message-get msg :execution_state) "idle")))

(defun jupyter-message-status-starting-p (msg)
  "Determine if MSG is a status: starting message."
  (and (eq (jupyter-message-type msg) :status)
       (equal (jupyter-message-get msg :execution_state) "starting")))

(provide 'jupyter-messages)

;;; jupyter-messages.el ends here
