;;; jupyter-client.el --- A Jupyter kernel client -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 06 Jan 2018

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

;; The default implementation of a Jupyter kernel client.

;;; Code:

(defgroup jupyter-client nil
  "A Jupyter client."
  :group 'jupyter)

(eval-when-compile (require 'subr-x))
(require 'jupyter-base)
(require 'jupyter-comm-layer)
(require 'jupyter-mime)
(require 'jupyter-messages)

(defface jupyter-eval-overlay
  '((((class color) (min-colors 88) (background light))
     :foreground "navy"
     :weight bold)
    (((class color) (min-colors 88) (background dark))
     :foreground "dodger blue"
     :weight bold))
  "Face used for the input prompt."
  :group 'jupyter-client)

(defcustom jupyter-eval-use-overlays nil
  "Display evaluation results as overlays in the `current-buffer'.
If this variable is non-nil, evaluation results are displayed as
overlays at the end of the line if possible."
  :group 'jupyter-client
  :type 'boolean)

(defcustom jupyter-eval-overlay-prefix "=> "
  "Evaluation result overlays will be prefixed with this string."
  :group 'jupyter-client
  :type 'string)

(defcustom jupyter-eval-short-result-display-function
  (lambda (result) (message "%s" result))
  "Function for displaying short evaluation results.
Evaluation results are considered short when they are less than
`jupyter-eval-short-result-max-lines' long.

The default function is `message', but any function that takes a
single string argument can be used.  For example, to display the
result in a tooltip, the variable can be set to `popup-tip' from
the `popup' package."
  :group 'jupyter-client
  :type 'function)

(defcustom jupyter-eval-short-result-max-lines 10
  "Maximum number of lines for short evaluation results.
Short evaluation results are displayed using
`jupyter-eval-short-result-display-function'.  Longer results are
forwarded to a separate buffer."
  :group 'jupyter-client
  :type 'integer)

(defcustom jupyter-include-other-output nil
  "Whether or not to handle IOPub messages from other clients.
A Jupyter client can receive messages from other clients
connected to the same kernel on the IOPub channel.  You can choose
to ignore these messages by setting
`jupyter-include-other-output' to nil.  If
`jupyter-include-other-output' is non-nil, then any messages that
are not associated with a request from a client are sent to the
client's handler methods with a nil value for the request
argument.  To change the value of this variable for a particular
client use `jupyter-set'."
  :group 'jupyter
  :type 'boolean)

(defcustom jupyter-iopub-message-hook nil
  "Hook run when a message is received on the IOPub channel.
The hook is called with two arguments, the Jupyter client and the
message it received.

Do not add to this hook variable directly, use
`jupyter-add-hook'.  If any of the message hooks return a non-nil
value, the client handlers will be prevented from running for the
message."
  :group 'jupyter
  :type 'hook)
(put 'jupyter-iopub-message-hook 'permanent-local t)

(defcustom jupyter-shell-message-hook nil
  "Hook run when a message is received on the SHELL channel.
The hook is called with two arguments, the Jupyter client and the
message it received.

Do not add to this hook variable directly, use
`jupyter-add-hook'.  If any of the message hooks return a non-nil
value, the client handlers will be prevented from running for the
message."
  :group 'jupyter
  :type 'hook)
(put 'jupyter-shell-message-hook 'permanent-local t)

(defcustom jupyter-stdin-message-hook nil
  "Hook run when a message is received on the STDIN channel.
The hook is called with two arguments, the Jupyter client and the
message it received.

Do not add to this hook variable directly,
use `jupyter-add-hook'.  If any of the message hooks return a
non-nil value, the client handlers will be prevented from running
for the message."
  :group 'jupyter
  :type 'hook)
(put 'jupyter-stdin-message-hook 'permanent-local t)

(declare-function company-begin-backend "ext:company" (backend &optional callback))
(declare-function company-doc-buffer "ext:company" (&optional string))
(declare-function company-idle-begin "ext:company")

(declare-function yas-minor-mode "ext:yasnippet" (&optional arg))
(declare-function yas-expand-snippet "ext:yasnippet" (content &optional start end expand-env))

(declare-function jupyter-insert "jupyter-mime")

;; This is mainly used by the REPL code, but is also set by
;; the `org-mode' client whenever `point' is inside a code
;; block.
(defvar jupyter-current-client nil
  "The kernel client for the `current-buffer'.
This is also let bound whenever a message is handled by a
kernel.")

(put 'jupyter-current-client 'permanent-local t)
(make-variable-buffer-local 'jupyter-current-client)

(defvar jupyter-inhibit-handlers nil
  "Whether or not new requests inhibit client handlers.
If set to t, prevent new requests from running any of the client
handler methods.  If set to a list of `jupyter-message-types',
prevent handler methods from running only for those message
types.

For example to prevent a client from calling its :execute-reply
handler:

    (let ((jupyter-inhibit-handlers '(:execute-reply)))
      (jupyter-send-execute-request client ...))

In addition, if the first element of the list is the symbol
`not', then inhibit handlers not in the list.

Do not set this variable directly, let bind it around specific
requests like the above example.")

(defvar jupyter--clients nil)

;; Define channel classes for method dispatching based on the channel type

(defclass jupyter-kernel-client (jupyter-finalized-object
                                 jupyter-instance-tracker)
  ((tracking-symbol :initform 'jupyter--clients)
   (execution-state
    :type string
    :initform "idle"
    :documentation "The current state of the kernel.  Can be
either \"idle\", \"busy\", or \"starting\".")
   (execution-count
    :type integer
    :initform 1
    :documentation "The *next* execution count of the kernel.
I.e., the execution count that will be assigned to the
next :execute-request sent to the kernel.")
   (requests
    :type hash-table
    :initform (make-hash-table :test 'equal)
    :documentation "A hash table with message ID's as keys.
This is used to register callback functions to run once a reply
from a previously sent request is received.  See
`jupyter-add-callback'.  Note that this is also used to filter
received messages that originated from a previous request by this
client.  Whenever the client sends a message in which a reply is
expected, it sets an entry in this table to represent the fact
that the message has been sent.  So if there is a non-nil value
for a message ID it means that a message has been sent and the
client is expecting a reply from the kernel.")
   (kernel-info
    :type json-plist
    :initform nil
    :documentation "The saved kernel info created when first
initializing this client.  When `jupyter-start-channels' is
called, this will be set to the kernel info plist returned
from an initial `:kernel-info-request'.")
   (kcomm
    :type jupyter-comm-layer
    :documentation "The process which receives events from channels.")
   (session
    :type jupyter-session
    :documentation "The session for this client.")
   (comms
    :type hash-table
    :initform (make-hash-table :test 'equal)
    :documentation "A hash table with comm ID's as keys.
Contains all of the open comms.  Each value is a cons cell (REQ .
DATA) which contains the generating `jupyter-request' that caused
the comm to open and the initial DATA passed to the comm for
initialization.")
   (manager
    :initform nil
    :documentation "If this client was initialized using a
`jupyter-kernel-manager' this slot will hold the manager which
initialized the client.")
   (-buffer
    :type buffer
    :documentation "An internal buffer used to store client local
variables.")))

;;; `jupyter-current-client' language method specializer

(defvar jupyter--generic-lang-used (make-hash-table :test #'eq))

(cl-generic-define-generalizer jupyter--generic-lang-generalizer
  50 (lambda (name &rest _)
       `(when (and ,name (object-of-class-p ,name 'jupyter-kernel-client))
          (gethash (jupyter-kernel-language ,name) jupyter--generic-lang-used)))
  (lambda (tag &rest _)
    (and (eq (car-safe tag) 'jupyter-lang)
         (list tag))))

(cl-generic-define-context-rewriter jupyter-lang (lang)
  `(jupyter-current-client (jupyter-lang ,lang)))

(cl-defmethod cl-generic-generalizers ((specializer (head jupyter-lang)))
  "Support for (jupyter-lang LANG) specializers.
Matches if the kernel language of the `jupyter-kernel-client'
passed as the argument has a language of LANG."
  (puthash (cadr specializer) specializer jupyter--generic-lang-used)
  (list jupyter--generic-lang-generalizer))

;;; Macros

(defmacro define-jupyter-client-handler (type &optional args doc &rest body)
  "Define an implementation of jupyter-handle-TYPE, a Jupyter message handler.
ARGS is a three element argument specification, with the same
meaning as in `cl-defmethod', e.g.

    ((client jupyter-kernel-client) req msg)

When a message is handled by a client handler method, the first
element will be bound to a subclass of `jupyter-kernel-client',
the second to the `jupyter-request' that caused the message to be
handled, and MSG is the message property list.

DOC is an explanation of the handler and defaults to

    \"A :TYPE handler.\"

BODY is the list of expressions to evaluate when the returned
method is called."
  (declare (indent defun) (doc-string 2))
  (when doc
    (unless (stringp doc)
      (setq body (cons doc body)
            doc nil)))
  (when (null body) (setq body (list nil)))
  ;; ARGS is only a list like (client msg)
  (cl-assert (or (null args) (= (length args) 3)) t
             "ARGS should be an argument list like (client req msg) or nil.")
  `(cl-defmethod ,(intern (format "jupyter-handle-%s" type))
     ,(or args
          ;; Internal usage. Most default handlers are just stub
          ;; definitions that should not signal an error if called,
          ;; which is what would happen if no method was defined, so
          ;; reduce the amount of repetition.
          '((_client jupyter-kernel-client) _req _msg))
     ,(or doc (format "A :%s handler." type))
     ,@body))

;; FIXME: Remove the need to call `jupyter-message-*' functions by
;; introducing some kind of property list defaults mechanism (e.g. by
;; appending the defaults to the property list passed to
;; `jupyter-send-*').
;;
;; - Document the :inhibit-handlers key
;; - Document that client is bound to the kernel client in BODY
(defmacro define--jupyter-client-sender (type &optional doc &rest body)
  (declare (indent 2) (doc-string 2))
  (when doc
    (unless (stringp doc)
      (setq body (cons doc body)
            doc nil)))
  (let (defaults)
    (while (keywordp (car body))
      (push (pop body) defaults)
      (push (pop body) defaults))
    (cl-callf nreverse defaults)
    (cl-labels ((keyword-name (k) (intern (substring (symbol-name k) 1)))
                (as-keyword
                 (k) (if (keywordp k) k
                       (intern (format ":%s" k)))))
      `(cl-defgeneric ,(intern (format "jupyter-send-%s" type))
           ((client jupyter-kernel-client)
            &key (inhibit-handlers nil)
            ,@(cl-loop for (k v) on defaults by #'cddr
                       collect (list (keyword-name k) v)))
         ,(or doc (format "Send an :%s message." type))
         (declare (indent 1))
         (let ((jupyter-inhibit-handlers
                (or inhibit-handlers jupyter-inhibit-handlers))
               (msg
                (,(intern (format "jupyter-message-%s" type))
                 ,@(cl-loop
                    for (k _) on defaults by #'cddr
                    nconc (list k (keyword-name k))))))
           (prog1 (jupyter-send client :shell ,(as-keyword type) msg)
             ,@body))))))

;;; Initializing a `jupyter-kernel-client'

(defun jupyter-client-has-manager-p (&optional client)
  "Return non-nil if CLIENT's kernel has a kernel manager.
CLIENT defaults to `jupyter-current-client'."
  (or client (setq client jupyter-current-client))
  (when client
    (cl-check-type client jupyter-kernel-client)
    (and (oref client manager) t)))

(cl-defmethod initialize-instance ((client jupyter-kernel-client) &optional _slots)
  (cl-call-next-method)
  (let ((buffer (generate-new-buffer " *jupyter-kernel-client*")))
    (oset client -buffer buffer)
    (jupyter-add-finalizer client
      (lambda ()
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (jupyter-stop-channels client)))))

(cl-defmethod jupyter-kernel-alive-p ((client jupyter-kernel-client))
  "Return non-nil if the kernel CLIENT is connected to is alive."
  (or (and (jupyter-client-has-manager-p client)
           (jupyter-kernel-alive-p (oref client manager)))
      (jupyter-hb-beating-p client)))

(defun jupyter-clients ()
  "Return a list of all `jupyter-kernel-client' objects."
  (jupyter-all-objects 'jupyter--clients))

(defun jupyter-find-client-for-session (session-id)
  "Return the kernel client whose session has SESSION-ID."
  (or (cl-find-if
       (lambda (x) (string= (jupyter-session-id (oref x session)) session-id))
       (jupyter-clients))
      (error "No client found for session (%s)" session-id)))

(defun jupyter--connection-info (info-or-session)
  "Return the connection plist according to INFO-OR-SESSION.
See `jupyter-comm-initialize'."
  (let ((conn-info (cond
                    ((jupyter-session-p info-or-session)
                     (jupyter-session-conn-info info-or-session))
                    ((json-plist-p info-or-session) info-or-session)
                    ((stringp info-or-session)
                     (if (file-remote-p info-or-session)
                         ;; TODO: Don't tunnel if a tunnel already exists
                         (jupyter-tunnel-connection info-or-session)
                       (unless (file-exists-p info-or-session)
                         (error "File does not exist (%s)" info-or-session))
                       (jupyter-read-plist info-or-session)))
                    (t (signal 'wrong-type-argument
                               (list info-or-session
                                     '(or jupyter-session-p json-plist-p stringp)))))))
    ;; Also validate the signature scheme here.
    (cl-destructuring-bind (&key key signature_scheme &allow-other-keys)
        conn-info
      (when (and (> (length key) 0)
                 (not (functionp
                       (intern (concat "jupyter-" signature_scheme)))))
        (error "Unsupported signature scheme: %s" signature_scheme)))
    conn-info))

;; FIXME: This requires that CLIENT is communicating with a kernel using a
;; `jupyter-channel-ioloop-comm' object.
(cl-defmethod jupyter-comm-initialize ((client jupyter-kernel-client) info-or-session)
  "Initialize CLIENT with connection INFO-OR-SESSION.
INFO-OR-SESSION can be a file name, a plist, or a
`jupyter-session' object that will be used to initialize CLIENT's
connection.

When INFO-OR-SESSION is a file name, read the contents of the
file as a JSON plist and create a new `jupyter-session' from it.
For remote files, create a new `jupyter-session' based on the
plist returned from `jupyter-tunnel-connection'.

When INFO-OR-SESSION is a plist, use it to create a new
`jupyter-session'.

Finally, when INFO-OR-SESSION is a `jupyter-session' it is used
as the session for CLIENT.

The session object will then be used to initialize the client
connection and will be accessible as the session slot of CLIENT.

The necessary keys and values to initialize a connection can be
found at
http://jupyter-client.readthedocs.io/en/latest/kernels.html#connection-files."
  (let ((session (and (jupyter-session-p info-or-session) info-or-session))
        (conn-info (jupyter--connection-info info-or-session)))
    (oset client session
          (or (copy-sequence session)
              (jupyter-session
               :key (plist-get conn-info :key)
               :conn-info conn-info)))
    (jupyter-comm-initialize
     (oref client kcomm)
     (oref client session))))

;;; Client local variables

(defmacro jupyter-with-client-buffer (client &rest body)
  "Run a form inside CLIENT's IOloop subprocess buffer.
BODY is run with the current buffer set to CLIENT's IOloop
subprocess buffer."
  (declare (indent 1))
  `(progn
     (cl-check-type ,client jupyter-kernel-client)
     (with-current-buffer (oref ,client -buffer)
       ,@body)))

(defun jupyter-set (client symbol newval)
  "Set CLIENT's local value for SYMBOL to NEWVAL."
  (jupyter-with-client-buffer client
    (set (make-local-variable symbol) newval)))

(defun jupyter-get (client symbol)
  "Get CLIENT's local value of SYMBOL.
Return nil if SYMBOL is not bound for CLIENT."
  (condition-case nil
      (buffer-local-value symbol (oref client -buffer))
    (void-variable nil)))

(gv-define-simple-setter jupyter-get jupyter-set)

;;; Hooks

(defun jupyter-add-hook (client hook function &optional append)
  "Add to the CLIENT value of HOOK the function FUNCTION.
APPEND has the same meaning as in `add-hook' and FUNCTION is
added to HOOK using `add-hook', but local only to CLIENT.  Note
that the CLIENT should have its channels already started before
this is called."
  (declare (indent 2))
  (jupyter-with-client-buffer client
    (add-hook hook function append t)))

(defun jupyter-remove-hook (client hook function)
  "Remove from CLIENT's value of HOOK the function FUNCTION."
  (jupyter-with-client-buffer client
    (remove-hook hook function t)))

;;; Sending messages

(cl-defgeneric jupyter-generate-request ((_client jupyter-kernel-client) _msg)
  "Generate a `jupyter-request' object for MSG.
This method gives an opportunity for subclasses to initialize a
`jupyter-request' based on the current context.

The default implementation returns a new `jupyter-request' with
the default value for all slots.  Note, the `:id' and
`:inhibited-handlers' slots are overwritten by the caller of this
method."
  (jupyter-request))

(defun jupyter-verify-inhibited-handlers ()
  "Verify the value of `jupyter-inhibit-handlers'.
If it does not contain a valid value, raise an error."
  (or (eq jupyter-inhibit-handlers t)
      (cl-loop
       for msg-type in (if (eq (car jupyter-inhibit-handlers) 'not)
                           (cdr jupyter-inhibit-handlers)
                         jupyter-inhibit-handlers)
       unless (plist-member jupyter-message-types msg-type)
       do (error "Not a valid message type (`%s')" msg-type))))

(cl-defmethod jupyter-send ((client jupyter-kernel-client)
                            channel
                            type
                            message
                            &optional msg-id)
  "Send a message on CLIENT's CHANNEL.
Return a `jupyter-request' representing the sent message.  CHANNEL
is one of the channel keywords, either (:stdin or :shell).
TYPE is one of the `jupyter-message-types'.  MESSAGE is the
message sent on CHANNEL.

Note that you can manipulate how to handle messages received in
response to the sent message, see `jupyter-add-callback' and
`jupyter-request-inhibited-handlers'."
  (declare (indent 1))
  (jupyter-verify-inhibited-handlers)
  (let ((msg-id (or msg-id (jupyter-new-uuid))))
    (when jupyter--debug
      ;; The logging of messages is deferred until the next command loop for
      ;; security reasons.  When sending :input-reply messages that read
      ;; passwords, clearing the password string using `clear-string' happens
      ;; *after* the call to `jupyter-send'.
      (run-at-time 0 nil (lambda () (message "SENDING: %s %s %s" type msg-id message))))
    (jupyter-send (oref client kcomm) 'send channel type message msg-id)
    ;; Anything sent to stdin is a reply not a request so don't add it as a
    ;; pending request
    (unless (eq channel :stdin)
      (let ((req (jupyter-generate-request client message))
            (requests (oref client requests)))
        (setf (jupyter-request-id req) msg-id)
        (setf (jupyter-request-inhibited-handlers req) jupyter-inhibit-handlers)
        (puthash msg-id req requests)
        (puthash "last-sent" req requests)))))

;;; Pending requests

(defun jupyter-requests-pending-p (client)
  "Return non-nil if CLIENT has open requests that the kernel has not handled.
Specifically, this returns non-nil if the last request message
sent to the kernel using CLIENT has not received an idle message
back."
  (cl-check-type client jupyter-kernel-client)
  (jupyter--drop-idle-requests client)
  (with-slots (requests) client
    ;; If there are two requests, then there is really only one since
    ;; "last-sent" is an alias for the other.
    (or (> (hash-table-count requests) 2)
        (when-let* ((last-sent (gethash "last-sent" requests)))
          (not (jupyter-request-idle-p last-sent))))))

(defsubst jupyter-last-sent-request (client)
  "Return the most recent `jupyter-request' made by CLIENT."
  (gethash "last-sent" (oref client requests)))

(defun jupyter-map-pending-requests (client function)
  "Call FUNCTION for all pending requests of CLIENT."
  (declare (indent 1))
  (cl-check-type client jupyter-kernel-client)
  (maphash (lambda (k v)
             (unless (or (equal k "last-sent")
                         (jupyter-request-idle-p v))
               (funcall function v)))
           (oref client requests)))

;;; Event handlers

;;;; Sending/receiving

(defun jupyter--show-event (event)
  (let ((event-name (upcase (symbol-name (car event))))
        (repr (cl-case (car event)
                (sent (format "%s" (cdr event)))
                (message (cl-destructuring-bind (_ channel _idents . msg) event
                           (format "%s" (list
                                         channel
                                         (jupyter-message-type msg)
                                         (jupyter-message-content msg)))))
                (t nil))))
    (when repr
      (message "%s" (concat event-name ": " repr)))))

;; TODO: Get rid of this method
(cl-defmethod jupyter-event-handler ((_client jupyter-kernel-client)
                                     (event (head sent)))
  (when jupyter--debug
    (jupyter--show-event event)))

(cl-defmethod jupyter-event-handler ((client jupyter-kernel-client)
                                     (event (head message)))
  (when jupyter--debug
    (jupyter--show-event event))
  (cl-destructuring-bind (_ channel _idents . msg) event
    (jupyter-handle-message client channel msg)))

;;; Starting communication with a kernel

(cl-defmethod jupyter-start-channels ((client jupyter-kernel-client))
  (jupyter-comm-add-handler (oref client kcomm) client))

(cl-defmethod jupyter-stop-channels ((client jupyter-kernel-client))
  "Stop any running channels of CLIENT."
  (when (slot-boundp client 'kcomm)
    (jupyter-comm-remove-handler (oref client kcomm) client)))

(cl-defmethod jupyter-channels-running-p ((client jupyter-kernel-client))
  "Are any channels of CLIENT running?"
  (jupyter-comm-alive-p (oref client kcomm)))

(cl-defmethod jupyter-channel-alive-p ((client jupyter-kernel-client) channel)
  (jupyter-channel-alive-p (oref client kcomm) channel))

(cl-defmethod jupyter-hb-pause ((client jupyter-kernel-client))
  (when (cl-typep (oref client kcomm) 'jupyter-hb-comm)
    (jupyter-hb-pause (oref client kcomm))))

(cl-defmethod jupyter-hb-unpause ((client jupyter-kernel-client))
  (when (cl-typep (oref client kcomm) 'jupyter-hb-comm)
    (jupyter-hb-unpause (oref client kcomm))))

(cl-defmethod jupyter-hb-beating-p ((client jupyter-kernel-client))
  "Is CLIENT still connected to its kernel?"
  (or (null (cl-typep (oref client kcomm) 'jupyter-hb-comm))
      (jupyter-hb-beating-p (oref client kcomm))))

;;; Message callbacks

(defsubst jupyter--run-callbacks (req msg)
  "Run REQ's MSG callbacks.
See `jupyter-add-callback'."
  (when-let (callbacks (and req (jupyter-request-callbacks req)))
    ;; Callback for all message types
    (when-let (f (alist-get t callbacks))
      (funcall f msg))
    (when-let (f (alist-get (jupyter-message-type msg) callbacks))
      (funcall f msg))))

(defun jupyter--add-callback (req msg-type cb)
  "Helper function for `jupyter-add-callback'.
REQ is a `jupyter-request' object, MSG-TYPE is one of the
keywords corresponding to a received message type in
`jupyter-message-types', and CB is the callback that will be run
when MSG-TYPE is received for REQ."
  (unless (or (plist-member jupyter-message-types msg-type)
              ;; A msg-type of t means that FUNCTION is run for all messages
              ;; associated with a request.
              (eq msg-type t))
    (error "Not a valid message type (`%s')" msg-type))
  (add-function
   :after (alist-get msg-type (jupyter-request-callbacks req) #'identity)
   cb))

(defun jupyter-add-callback (req msg-type cb &rest callbacks)
  "Add a callback to run when a message is received for a request.
REQ is a `jupyter-request' returned by one of the request methods
of a kernel client.  MSG-TYPE is one of the keys in
`jupyter-message-types'.  CB is the callback function to run when
a message with MSG-TYPE is received for REQ.

MSG-TYPE can also be a list, in which case run CB for every
MSG-TYPE in the list.  If MSG-TYPE is t, run CB for every message
received for REQ.

Multiple callbacks can be added for the same MSG-TYPE.  The
callbacks will be called in the order they were added.

Any additional arguments to `jupyter-add-callback' are
interpreted as additional CALLBACKS to add to REQ.  So to add
multiple callbacks you would do

    (jupyter-add-callback
        (jupyter-send-execute-request client :code \"1 + 2\")
      :status (lambda (msg) ...)
      :execute-reply (lambda (msg) ...)
      :execute-result (lambda (msg) ...))"
  (declare (indent 1))
  (if (jupyter-request-idle-p req)
      (error "Request already received idle message")
    (while (and msg-type cb)
      (cl-check-type cb function "Callback should be a function")
      (if (listp msg-type)
          (cl-loop for mt in msg-type
                   do (jupyter--add-callback req mt cb))
        (jupyter--add-callback req msg-type cb))
      (setq msg-type (pop callbacks)
            cb (pop callbacks)))))

;;; Waiting for messages

(defvar jupyter--already-waiting-p nil)

(defun jupyter-wait-until (req msg-type cb &optional timeout progress-msg)
  "Wait until conditions for a request are satisfied.
REQ, MSG-TYPE, and CB have the same meaning as in
`jupyter-add-callback'.  If CB returns non-nil within TIMEOUT
seconds, return the message that caused CB to return non-nil.  If
CB never returns a non-nil value within TIMEOUT, return nil.  Note
that if no TIMEOUT is given, `jupyter-default-timeout' is used.

If PROGRESS-MSG is non-nil, it should be a message string to
display for reporting progress to the user while waiting."
  (declare (indent 2))
  (let (msg)
    (jupyter-add-callback req
      msg-type (lambda (m) (setq msg (when (funcall cb m) m))))
    (let* ((timeout-spec (when jupyter--already-waiting-p
                           (with-timeout-suspend)))
           (jupyter--already-waiting-p t))
      (unwind-protect
          (jupyter-with-timeout
              (progress-msg (or timeout jupyter-default-timeout))
            msg)
        (when timeout-spec
          (with-timeout-unsuspend timeout-spec))))))

(defun jupyter-wait-until-startup (client &optional timeout progress-msg)
  "Wait for CLIENT to receive a status: startup message.
Return non-nil if CLIENT receives the message within TIMEOUT,
otherwise nil.  TIMEOUT defaults to `jupyter-long-timeout'.

If PROGRESS-MSG is non-nil, it should be a message string to
display for reporting progress to the user while waiting."
  (let* ((msg nil)
         (check (lambda (_ m)
                  (when (jupyter-message-status-starting-p m)
                    (setq msg m)))))
    (jupyter-add-hook client 'jupyter-iopub-message-hook check)
    (unwind-protect
        (jupyter-with-timeout
            (progress-msg (or timeout jupyter-long-timeout))
          msg)
      (jupyter-remove-hook client 'jupyter-iopub-message-hook check))))

(defun jupyter-wait-until-idle (req &optional timeout progress-msg)
  "Wait until a status: idle message is received for a request.
REQ has the same meaning as in `jupyter-add-callback'.  If an idle
message for REQ is received within TIMEOUT seconds, return the
message.  Otherwise return nil if the message was not received
within TIMEOUT.  Note that if no TIMEOUT is given, it defaults to
`jupyter-default-timeout'.

If PROGRESS-MSG is non-nil, it is a message string to display for
reporting progress to the user while waiting."
  (or (jupyter-request-idle-p req)
      (jupyter-wait-until req :status
        #'jupyter-message-status-idle-p timeout progress-msg)))

(defun jupyter-wait-until-received (msg-type req &optional timeout progress-msg)
  "Wait until a message of a certain type is received for a request.
MSG-TYPE and REQ have the same meaning as their corresponding
arguments in `jupyter-add-callback'.  If no message that matches
MSG-TYPE is received for REQ within TIMEOUT seconds, return nil.
Otherwise return the first message that matched MSG-TYPE.  Note
that if no TIMEOUT is given, it defaults to
`jupyter-default-timeout'.

If PROGRESS-MSG is non-nil, it is a message string to display for
reporting progress to the user while waiting."
  (declare (indent 1))
  (jupyter-wait-until req msg-type #'identity timeout progress-msg))

(defun jupyter-idle-sync (req)
  "Return only when REQ has received a status: idle message."
  (while (null (jupyter-wait-until-idle req jupyter-long-timeout))))

(defun jupyter-add-idle-sync-hook (hook req &optional append)
  "Add a function to HOOK that waits until REQ receives a status: idle message.
The function will not return until either a status: idle message
has been received by REQ or an error is signaled.  APPEND and has
the same meaning as in `add-hook'.

The function is added to the global value of HOOK.  When the
function is evaluated, it removes itself from HOOK *before*
waiting."
  (cl-check-type req jupyter-request)
  (cl-labels
      ((sync-hook
        ()
        (remove-hook hook #'sync-hook)
        (jupyter-idle-sync req)))
    (add-hook hook #'sync-hook append)))

;;; Client handlers

(cl-defgeneric jupyter-drop-request ((_client jupyter-kernel-client) _req)
  "Called when CLIENT removes REQ, from its request table."
  nil)

(cl-defmethod jupyter-drop-request :before ((_client jupyter-kernel-client) req)
  (when jupyter--debug
    (message "DROPPING-REQ: %s" (jupyter-request-id req))))

(defun jupyter--drop-idle-requests (client)
  "Drop completed requests from CLIENT's request table.
A request is deemed complete when an idle message has been
received for it and it is not the most recently sent request."
  (with-slots (requests) client
    (cl-loop
     with last-sent = (gethash "last-sent" requests)
     for req in (hash-table-values requests)
     when (and (jupyter-request-idle-p req)
               (not (eq req last-sent)))
     do (unwind-protect
            (jupyter-drop-request client req)
          (remhash (jupyter-request-id req) requests)))))

(defsubst jupyter--request-allows-handler-p (req msg)
  "Return non-nil if REQ doesn't inhibit the handler for MSG."
  (let* ((ihandlers (and req (jupyter-request-inhibited-handlers req)))
         (type (and (listp ihandlers)
                    (memq (jupyter-message-type msg) ihandlers))))
    (not (or (eq ihandlers t)
             (if (eq (car ihandlers) 'not) (not type) type)))))

(defun jupyter-handle-message-p (client channel msg)
  "Return non-nil if CLIENT should handle a MSG received on CHANNEL.
Run CLIENT's CHANNEL hook, jupyter-CHANNEL-message-hook,
passing (CLIENT MSG) as arguments to the hook functions.  If all
of the hook functions return nil, then MSG should be handled.
nil is returned otherwise."
  (jupyter-with-client-buffer client
    (let ((hook (pcase channel
                  (:iopub 'jupyter-iopub-message-hook)
                  (:shell 'jupyter-shell-message-hook)
                  (:stdin 'jupyter-stdin-message-hook)
                  (_ (error "Unhandled channel: %s" channel)))))
      (when jupyter--debug
        (message "RUN-HOOK: %s" hook))
      (with-demoted-errors "Error in Jupyter message hook: %S"
        (not (run-hook-with-args-until-success
              hook client msg))))))

(defconst jupyter--client-handlers
  (cl-labels
      ((handler-alist
        (&rest msg-types)
        (cl-loop
         for mt in msg-types
         collect (cons mt (intern
                           (format "jupyter-handle-%s"
                                   (substring (symbol-name mt) 1)))))))
    `((:iopub . ,(handler-alist
                  :shutdown-reply :stream :comm-open :comm-msg
                  :comm-close :execute-input :execute-result
                  :error :status :clear-output :display-data
                  :update-display-data))
      (:shell . ,(handler-alist
                  :execute-reply :shutdown-reply :inspect-reply
                  :complete-reply :history-reply :is-complete-reply
                  :comm-info-reply :kernel-info-reply))
      (:stdin . ,(handler-alist
                  :input-reply :input-request)))))

(defun jupyter--run-handler (client channel msg req)
  (when (jupyter-handle-message-p client channel msg)
    (let* ((msg-type (jupyter-message-type msg))
           (channel-handlers
            (or (alist-get channel jupyter--client-handlers)
                (error "Unhandled channel: %s" channel)))
           (handler (or (alist-get msg-type channel-handlers)
                        (error "Unhandled message type: %s" msg-type))))
      (funcall handler client req msg))))

(defsubst jupyter--update-execution-state (client msg req)
  (pcase (jupyter-message-type msg)
    (:status
     (oset client execution-state
           (jupyter-message-get msg :execution_state)))
    ((or :execute-input
         (and (guard req) :execute-reply))
     (oset client execution-count
           (1+ (jupyter-message-get msg :execution_count))))))

(defsubst jupyter--message-completes-request-p (msg)
  (or (jupyter-message-status-idle-p msg)
      ;; Jupyter protocol 5.1, IPython implementation 7.5.0
      ;; doesn't give status: busy or status: idle messages on
      ;; kernel-info-requests.  Whereas IPython implementation
      ;; 6.5.0 does.  Seen on Appveyor tests.
      ;;
      ;; TODO: May be related jupyter/notebook#3705 as the
      ;; problem does happen after a kernel restart when
      ;; testing.
      (eq (jupyter-message-type msg) :kernel-info-reply)
      ;; No idle message is received after a shutdown reply so
      ;; consider REQ as having received an idle message in
      ;; this case.
      (eq (jupyter-message-type msg) :shutdown-reply)))

(cl-defgeneric jupyter-handle-message ((client jupyter-kernel-client) channel msg)
  "Process a message received on CLIENT's CHANNEL.
CHANNEL is the Jupyter channel that MSG was received on by
CLIENT.  MSG is a message property list and is the Jupyter
message being handled.")

(cl-defmethod jupyter-handle-message ((client jupyter-kernel-client) channel msg)
  "Run callbacks and handler method for MSG.
Before any handling of MSG takes place, update CLIENT's execution
status slots (execution-state, execution-count) based on MSG, let
bind `jupyter-current-client' to CLIENT, and, when there is a
`jupyter-request' sent by CLIENT associated with the
`jupyter-message-parent-id' of MSG, set the
`jupyter-request-last-message' of the request to MSG.

CLIENT may not have sent the request that generated MSG, e.g. if
MSG is an :execute-input request broadcasted to :iopub and not
sent by CLIENT.  In this case, a message handler method is run,
without running any message callbacks, only if
`jupyter-include-other-output' is non-nil for CLIENT.

When MSG has an associated request generated by CLIENT, run the
`jupyter-request-callbacks', if any, for the message before
attempting to run the message handler.  Then remove old,
completed, requests from CLIENT's request table."
  (when msg
    (let ((jupyter-current-client client)
          (req (gethash (jupyter-message-parent-id msg) (oref client requests))))
      (jupyter--update-execution-state client msg req)
      (cond
       (req
        (setf (jupyter-request-last-message req) msg)
        (unwind-protect
            (jupyter--run-callbacks req msg)
          (unwind-protect
              (when (jupyter--request-allows-handler-p req msg)
                (jupyter--run-handler client channel msg req))
            (when (jupyter--message-completes-request-p msg)
              ;; Order matters here.  We want to remove idle requests *before*
              ;; setting another request idle to account for idle messages
              ;; coming in out of order, e.g. before their respective reply
              ;; messages.
              (jupyter--drop-idle-requests client)
              (setf (jupyter-request-idle-p req) t)))))
       (t
        (when (and (or (jupyter-get client 'jupyter-include-other-output)
                       ;; Always handle a startup message
                       (jupyter-message-status-starting-p msg))
                   (jupyter--request-allows-handler-p req msg))
          (jupyter--run-handler client channel msg req)))))))

;;; STDIN handlers

(define-jupyter-client-handler input-request ((client jupyter-kernel-client) _req msg)
  "Handle an input request from CLIENT's kernel.
PROMPT is the prompt the kernel would like to show the user.  If
PASSWORD is t, then `read-passwd' is used to get input from the
user.  Otherwise `read-from-minibuffer' is used."
  (jupyter-with-message-content msg (prompt password)
    (let ((value (condition-case nil
                     ;; Disallow any `with-timeout's from timing out.  This
                     ;; prevents any calls to `jupyter-wait-until-received' from
                     ;; timing out when reading input.  See #35.
                     (let ((timeout-spec (with-timeout-suspend)))
                       (unwind-protect
                           (if (eq password t) (read-passwd prompt)
                             (read-from-minibuffer prompt))
                         (with-timeout-unsuspend timeout-spec)))
                   (quit ""))))
      (unwind-protect
          (jupyter-send
           client :stdin
           :input-reply (jupyter-message-input-reply
                         :value value))
        (when (eq password t)
          (clear-string value)))
      value)))

(defalias 'jupyter-handle-input-reply 'jupyter-handle-input-request)

;;;; Evaluation

(cl-defgeneric jupyter-load-file-code (_file)
  "Return a string suitable to send as code to a kernel for loading FILE.
Use the jupyter-lang method specializer to add a method for a
particular language."
  (error "Kernel language (%s) not supported yet"
         (jupyter-kernel-language jupyter-current-client)))

;;;;; Evaluation routines

(defvar-local jupyter-eval-expression-history nil
  "A client local variable to store the evaluation history.
The evaluation history is used when reading code to evaluate from
the minibuffer.")

(defun jupyter--teardown-minibuffer ()
  "Remove Jupyter related variables and hooks from the minibuffer."
  (setq jupyter-current-client nil)
  (remove-hook 'completion-at-point-functions 'jupyter-completion-at-point t)
  (remove-hook 'minibuffer-exit-hook 'jupyter--teardown-minibuffer t))

;; This is needed since `read-from-minibuffer' expects the history variable to
;; be a symbol whose value is `set' when adding a new history element.  Since
;; `jupyter-eval-expression-history' is a buffer (client) local variable, it would be
;; set in the minibuffer which we don't want.
(defvar jupyter--read-expression-history nil
  "A client's `jupyter-eval-expression-history' when reading an expression.
This variable is used as the history symbol when reading an
expression from the minibuffer.  After an expression is read, the
`jupyter-eval-expression-history' of the client is updated to the
value of this variable.")

(cl-defgeneric jupyter-read-expression ()
  "Read an expression using the `jupyter-current-client' for completion.
The expression is read from the minibuffer and the expression
history is obtained from the `jupyter-eval-expression-history'
client local variable.

Methods that extend this generic function should
`cl-call-next-method' as a last step."
  (cl-check-type jupyter-current-client jupyter-kernel-client
                 "Need a client to read an expression")
  (let* ((client jupyter-current-client)
         (jupyter--read-expression-history
          (jupyter-get client 'jupyter-eval-expression-history)))
    (minibuffer-with-setup-hook
        (lambda ()
          (setq jupyter-current-client client)
          (add-hook 'completion-at-point-functions
                    'jupyter-completion-at-point nil t)
          (add-hook 'minibuffer-exit-hook
                    'jupyter--teardown-minibuffer nil t))
      (prog1 (read-from-minibuffer
              (format "Eval (%s): " (jupyter-kernel-language client))
              nil read-expression-map
              nil 'jupyter--read-expression-history)
        (jupyter-set client 'jupyter-eval-expression-history
                     jupyter--read-expression-history)))))

(defun jupyter-eval (code &optional mime)
  "Send an execute request for CODE, wait for the execute result.
The `jupyter-current-client' is used to send the execute request.
All client handlers are inhibited for the request.  In addition,
the history of the request is not stored.  Return the MIME
representation of the result.  If MIME is nil, return the
text/plain representation."
  (interactive (list (jupyter-read-expression) nil))
  (let ((msg (jupyter-wait-until-received :execute-result
               (let* ((jupyter-inhibit-handlers t)
                      (req (jupyter-send-execute-request jupyter-current-client
                             :code code :store-history nil)))
                 (prog1 req
                   (jupyter-add-callback req
                     :execute-reply
                     (jupyter-message-lambda (status evalue)
                       (unless (equal status "ok")
                         (error "%s" (ansi-color-apply evalue))))))))))
    (when msg
      (jupyter-message-data msg (or mime :text/plain)))))

(defun jupyter-eval-result-callbacks (req beg end)
  "Return a plist containing callbacks used to display evaluation results.
The plist contains default callbacks for the :execute-reply,
:execute-result, and :display-data messages that may be used for
the messages received in response to REQ.

BEG and END are positions in the current buffer marking the
region of code evaluated.

The callbacks are designed to either display evaluation results
using overlays in the current buffer over the region between BEG
and END or in pop-up buffers/frames.  See
`jupyter-eval-use-overlays'."
  (let ((buffer (current-buffer))
        (use-overlays-p (and beg end (jupyter-eval-display-with-overlay-p))))
    (when use-overlays-p
      ;; NOTE: It would make sense to set these markers to nil, e.g. at the end
      ;; of the execute-result or idle messages, but there is no guarantee on
      ;; message order so it may be the case that those message types are
      ;; received before the callbacks that use these markers have fired.
      ;;
      ;; TODO: Do something with finalizers?
      (setq beg (set-marker (make-marker) beg))
      (setq end (set-marker (make-marker) end)))
    (let ((display-overlay
           (if use-overlays-p
               (lambda (val)
                 (when (buffer-live-p buffer)
                   (prog1 t
                     (with-current-buffer buffer
                       (jupyter-eval-display-overlay beg end val)))))
             #'ignore))
          had-result)
      (list
       :execute-reply
       (jupyter-message-lambda (status ename evalue)
         (cond
          ((equal status "ok")
           (unless had-result
             (unless (funcall display-overlay "✔")
               (message "jupyter: eval done"))))
          (t
           (setq ename (ansi-color-apply ename))
           (setq evalue (ansi-color-apply evalue))
           (unless
               ;; Happens in IJulia
               (> (+ (length ename) (length evalue)) 250)
             (if (string-prefix-p ename evalue)
                 ;; Also happens in IJulia
                 (message evalue)
               (message "%s: %s" ename evalue))))))
       :execute-result
       (lambda (msg)
         (setq had-result t)
         (jupyter-with-message-data msg
             ((res text/plain)
              ;; Prefer to display the markdown representation if available.  The
              ;; IJulia kernel will return both plain text and markdown.
              (md text/markdown))
           (let ((jupyter-pop-up-frame (jupyter-pop-up-frame-p :execute-result)))
             (cond
              ((or md (null res))
               (jupyter-with-display-buffer "result" 'reset
                 (jupyter-with-message-content msg (data metadata)
                   (jupyter-insert data metadata))
                 (goto-char (point-min))
                 (jupyter-display-current-buffer-reuse-window)))
              (res
               (setq res (ansi-color-apply res))
               (cond
                ((funcall display-overlay res))
                ((jupyter-line-count-greater-p
                  res jupyter-eval-short-result-max-lines)
                 (jupyter-with-display-buffer "result" 'reset
                   (insert res)
                   (goto-char (point-min))
                   (jupyter-display-current-buffer-reuse-window)))
                (t
                 (funcall jupyter-eval-short-result-display-function
                          (format "%s" res)))))))))
       :display-data
       (jupyter-message-lambda (data metadata)
         (setq had-result t)
         (jupyter-with-display-buffer "display" req
           (jupyter-insert data metadata)
           ;; Don't pop-up the display when it's empty (e.g. jupyter-R
           ;; will open some HTML results in an external browser)
           (when (and (/= (point-min) (point-max)))
             (jupyter-display-current-buffer-guess-where :display-data)))
         ;; TODO: Also inline images?
         (funcall display-overlay "✔"))))))

(defun jupyter-eval-add-callbacks (req &optional beg end)
  "Add evaluation callbacks for REQ.

The callbacks are designed to handle the various message types
that can be generated by an execute-request to, e.g. display the
results of evaluation in a popup buffer or indicate that an error
occurred during evaluation.

The message types that will have callbacks added are
:execute-reply, :execute-result, :display-data, :error, :stream.

BEG and END are positions that mark the region of the current
buffer corresponding to the evaluated code.

See `jupyter-eval-short-result-max-lines' and
`jupyter-eval-use-overlays'."
  (let* ((eval-callbacks (jupyter-eval-result-callbacks req beg end)))
    (apply
     #'jupyter-add-callback req
     (nconc
      eval-callbacks
      (list
       :error
       (jupyter-message-lambda (traceback)
         ;; FIXME: Assumes the error in the
         ;; execute-reply is good enough
         (when (> (apply #'+ (mapcar #'length traceback)) 250)
           (jupyter-display-traceback traceback)))
       :stream
       (jupyter-message-lambda (name text)
         (jupyter-with-display-buffer (pcase name
                                        ("stderr" "error")
                                        (_ "output"))
             req
           (jupyter-insert-ansi-coded-text text)
           (jupyter-display-current-buffer-guess-where :stream))))))
    req))

(cl-defgeneric jupyter-eval-string (str &optional beg end)
  "Evaluate STR using the `jupyter-current-client'.
Return the `jupyter-request' object for the evaluation.

If BEG and END are non-nil they correspond to the region of the
current buffer that STR was extracted from.")

(cl-defmethod jupyter-eval-string (str &optional beg end)
  "Evaluate STR using the `jupyter-current-client'."
  (cl-check-type jupyter-current-client jupyter-kernel-client
                 "Not a valid client")
  (let ((jupyter-inhibit-handlers '(not :input-request))
        (req (jupyter-send-execute-request jupyter-current-client
               :code str :store-history nil)))
    (prog1 req
      (jupyter-eval-add-callbacks req beg end))))

(defun jupyter-eval-string-command (str)
  "Evaluate STR using the `jupyter-current-client'.
If the result of evaluation is more than
`jupyter-eval-short-result-max-lines' long, a buffer displaying
the results is shown.  For less lines, the result is displayed
with `jupyter-eval-short-result-display-function'.

If `jupyter-eval-use-overlays' is non-nil, evaluation results
are displayed in the current buffer instead."
  (interactive (list (jupyter-read-expression)))
  (jupyter-eval-string str))

(defun jupyter-eval-region (beg end)
  "Evaluate a region with the `jupyter-current-client'.
BEG and END are the beginning and end of the region to evaluate.

If the result of evaluation is more than
`jupyter-eval-short-result-max-lines' long, a buffer displaying
the results is shown.  For less lines, the result is displayed
with `jupyter-eval-short-result-display-function'.

If `jupyter-eval-use-overlays' is non-nil, evaluation results
are displayed in the current buffer instead."
  (interactive "r")
  (jupyter-eval-string (buffer-substring-no-properties beg end) beg end))

(defun jupyter-eval-line-or-region (insert)
  "Evaluate the current line or region with the `jupyter-current-client'.
If the current region is active send it using
`jupyter-eval-region', otherwise send the current line.

With a prefix argument, evaluate and INSERT the text/plain
representation of the results in the current buffer."
  (interactive "P")
  (let* ((region (when (use-region-p)
                   (car (region-bounds))))
         (req (jupyter-eval-region
               (or (car region) (line-beginning-position))
               (or (cdr region) (line-end-position)))))
    (prog1 req
      (when insert
        (setf (alist-get :execute-result (jupyter-request-callbacks req))
              (let ((pos (point-marker)))
                (jupyter-message-lambda ((res text/plain))
                  (when res
                    (setq res (ansi-color-apply res))
                    (with-current-buffer (marker-buffer pos)
                      (save-excursion
                        (cond
                         (region
                          (goto-char (car region))
                          (delete-region (car region) (cdr region)))
                         (t
                          (goto-char pos)
                          (end-of-line)
                          (insert "\n")))
                        (set-marker pos nil)
                        (insert res)
                        (when region (push-mark))))))))))))

(defun jupyter-load-file (file)
  "Send the contents of FILE using `jupyter-current-client'."
  (interactive
   (list (read-file-name "File name: " nil nil nil
                         (file-name-nondirectory
                          (or (buffer-file-name) "")))))
  (message "Evaluating %s..." file)
  (setq file (expand-file-name file))
  (if (file-exists-p file)
      (jupyter-eval-string (jupyter-load-file-code file))
    (error "Not a file (%s)" file)))

(defun jupyter-eval-buffer (buffer)
  "Send the contents of BUFFER using `jupyter-current-client'."
  (interactive (list (current-buffer)))
  (jupyter-eval-string (with-current-buffer buffer (buffer-string))))

(defun jupyter-eval-defun ()
  "Evaluate the function at `point'."
  (interactive)
  (when-let* ((bounds (bounds-of-thing-at-point 'defun)))
    (cl-destructuring-bind (beg . end) bounds
      (jupyter-eval-region beg end))))

;;;;;; Evaluation overlays

(defvar jupyter-eval-overlay-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "S-RET") 'jupyter-eval-toggle-overlay)
    (define-key map (kbd "S-<return>") 'jupyter-eval-toggle-overlay)
    map))

(defun jupyter-eval-ov--delete (ov &rest _)
  (delete-overlay ov))

(defun jupyter-eval-ov--remove-all (beg end)
  (dolist (ov (overlays-in beg end))
    (when (overlay-get ov 'jupyter-eval)
      (jupyter-eval-ov--delete ov))))

(defun jupyter-eval-ov--propertize (text &optional newline)
  ;; Display properties can't be nested so use the one on TEXT if available
  (if (get-text-property 0 'display text) text
    (let ((display (concat
                    ;; Add a space before a newline so that `point' stays on
                    ;; the same line when moving to the beginning of the
                    ;; overlay.
                    (if newline " \n" " ")
                    (propertize
                     (concat jupyter-eval-overlay-prefix text)
                     'face 'jupyter-eval-overlay))))
      ;; Ensure `point' doesn't move past the beginning or end of the overlay
      ;; on motion commands.
      (put-text-property 0 1 'cursor t display)
      (put-text-property (1- (length display)) (length display) 'cursor t display)
      (propertize " " 'display display))))

(defun jupyter-eval-ov--fold-boundary (text)
  (string-match-p "\n" text))

(defun jupyter-eval-ov--fold-string (text)
  (when (eq buffer-invisibility-spec t)
    (setq buffer-invisibility-spec '(t)))
  (unless (assoc 'jupyter-eval buffer-invisibility-spec)
    (push (cons 'jupyter-eval t) buffer-invisibility-spec))
  (when-let* ((pos (jupyter-eval-ov--fold-boundary text)))
    (put-text-property pos (length text) 'invisible 'jupyter-eval text)
    text))

(defun jupyter-eval-ov--expand-string (text)
  (prog1 text
    (put-text-property 0 (length text) 'invisible nil text)))

(defun jupyter-eval-ov--make (beg end text)
  (setq text (replace-regexp-in-string "\n+$" "" text))
  (setq text (replace-regexp-in-string "^\n+" "" text))
  (let ((ov (make-overlay beg end nil t)))
    (prog1 ov
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'modification-hooks '(jupyter-eval-ov--delete))
      (overlay-put ov 'insert-in-front-hooks '(jupyter-eval-ov--delete))
      (overlay-put ov 'insert-behind-hooks '(jupyter-eval-ov--delete))
      (overlay-put ov 'keymap jupyter-eval-overlay-keymap)
      (let ((folded (jupyter-eval-ov--fold-string text)))
        (overlay-put ov 'jupyter-eval
                     (list (if folded 'folded 'expanded) text))
        (overlay-put ov 'after-string (jupyter-eval-ov--propertize text))))))

(defun jupyter-eval-ov--expand (ov)
  (when-let* ((eval-props (overlay-get ov 'jupyter-eval)))
    (cl-destructuring-bind (fold text) eval-props
      (when (eq fold 'folded)
        (setf (car (overlay-get ov 'jupyter-eval)) 'expanded)
        (when (jupyter-eval-ov--fold-boundary text)
          (setf (overlay-get ov 'after-string)
                (jupyter-eval-ov--propertize
                 ;; Newline added so that background extends across entire line
                 ;; of the last line in TEXT.
                 (concat (jupyter-eval-ov--expand-string text) "\n")
                 t)))))))

(defun jupyter-eval-ov--fold (ov)
  (when-let* ((eval-props (overlay-get ov 'jupyter-eval)))
    (cl-destructuring-bind (fold text) eval-props
      (when (eq fold 'expanded)
        (setf (car (overlay-get ov 'jupyter-eval)) 'folded)
        (jupyter-eval-ov--fold-string text)
        (setf (overlay-get ov 'after-string)
              (jupyter-eval-ov--propertize text))))))

(defun jupyter-eval-toggle-overlay ()
  "Expand or contract the display of evaluation results around `point'."
  (interactive)
  (let (nearest)
    (dolist (ov (overlays-at (point)))
      (when (and (or (null nearest)
                     (and (> (overlay-start ov) (overlay-start nearest))
                          (< (overlay-end ov) (overlay-end nearest))))
                 (overlay-get ov 'jupyter-eval))
        (setq nearest ov)))
    (when-let* ((props (and nearest (overlay-get nearest 'jupyter-eval))))
      (cl-destructuring-bind (fold _) props
        (if (eq fold 'folded)
            (jupyter-eval-ov--expand nearest)
          (jupyter-eval-ov--fold nearest))))))

(defun jupyter-eval-remove-overlays ()
  "Remove all evaluation result overlays in the buffer."
  (interactive)
  (jupyter-eval-ov--remove-all (point-min) (point-max)))

(defun jupyter-eval-display-overlay (beg end str)
  "Overlay (BEG . END) using STR as an evaluation result.
STR is displayed after the region."
  (save-excursion
    (goto-char end)
    (setq end (if (get-text-property 0 'display str)
                  (min (point-max) (1+ (line-end-position)))
                (skip-syntax-backward "->")
                (point)))
    (jupyter-eval-ov--remove-all (1- end) end)
    (jupyter-eval-ov--make beg end str)))

(defun jupyter-eval-display-with-overlay-p ()
  "Return non-nil if evaluation results should be displayed with overlays."
  (and jupyter-eval-use-overlays
       jupyter-current-client
       (derived-mode-p (jupyter-kernel-language-mode jupyter-current-client))))

;;;;; Handlers

(define--jupyter-client-sender execute-request
  :code nil
  :silent nil
  :store-history t
  :user-expressions nil
  :allow-stdin (jupyter-channel-alive-p client :stdin)
  :stop-on-error nil
  (jupyter-server-mode-set-client client))

(define-jupyter-client-handler execute-reply)

(cl-defgeneric jupyter-handle-payload ((source symbol) _payload)
  "Execute the action in a Jupyter PAYLOAD.
SOURCE is the type of payload and PAYLOAD will be a property list
containing the necessary information to perform the actions of
SOURCE."
  (error "Unhandled payload (%s)" source))

(cl-defmethod jupyter-handle-payload ((payloads vector))
  "Loop over PAYLOADS, calling `jupyter-handle-payload' for each one."
  (cl-loop
   for pl across payloads
   do (jupyter-handle-payload (intern (plist-get pl :source)) pl)))

(cl-defmethod jupyter-handle-payload ((_source (eql page)) pl)
  (let ((text (plist-get (plist-get pl :data) :text/plain))
        (line (or (plist-get pl :start) 0)))
    (jupyter-with-display-buffer "pager" 'reset
      (jupyter-insert-ansi-coded-text text)
      (goto-char (point-min))
      (forward-line line)
      (jupyter-display-current-buffer-reuse-window))))

(cl-defmethod jupyter-handle-payload ((_source (eql edit)) pl)
  (with-current-buffer (find-file-other-window
                        (plist-get pl :filename))
    (goto-char (point-min))
    (forward-line (plist-get pl :line_number))
    (set-window-start (selected-window) (point))))

(cl-defmethod jupyter-handle-payload ((_source (eql edit_magic)) pl)
  (jupyter-handle-payload 'edit pl))

;;;; Inspection

;; TODO: How to add hover documentation support
(defun jupyter-inspect-at-point (&optional buffer detail)
  "Inspect the code at point.
Call `jupyter-inspect' for the `jupyter-code-context' at point.

BUFFER and DETAIL have the same meaning as in `jupyter-inspect'."
  (interactive (list nil 0))
  (cl-destructuring-bind (code pos)
      (jupyter-code-context 'inspect)
    (jupyter-inspect code pos buffer detail)))

(cl-defgeneric jupyter-inspect (code &optional pos buffer detail)
  "Inspect CODE.
Send an `:inspect-request' with the `jupyter-current-client' and
display the results in a BUFFER.

CODE is the code to inspect and POS is your position in the CODE.
If POS is nil, it defaults to the length of CODE.

If BUFFER is nil, display the results in a help buffer.
Otherwise insert the results in BUFFER but do not display it.

DETAIL is the detail level to use for the request and defaults to
0."
  (setq pos (or pos (length code)))
  (unless (and jupyter-current-client
               (object-of-class-p jupyter-current-client 'jupyter-kernel-client))
    (error "Need a valid `jupyter-current-client'"))
  (let ((client jupyter-current-client)
        (msg (jupyter-wait-until-received :inspect-reply
               (jupyter-send-inspect-request jupyter-current-client
                 :code code :pos pos :detail detail)
               ;; Longer timeout for the Julia kernel
               jupyter-long-timeout)))
    (if msg
        (jupyter-with-message-content msg
            (status found)
          (if (and (equal status "ok") (eq found t))
              (let ((inhibit-read-only t))
                (if (buffer-live-p buffer)
                    (with-current-buffer buffer
                      ;; Insert MSG here so that `jupyter-insert' has access to
                      ;; the message type.  This is needed since the python
                      ;; kernel and others may use this information.
                      (jupyter-insert msg)
                      (current-buffer))
                  (with-help-window (help-buffer)
                    (with-current-buffer standard-output
                      (setq other-window-scroll-buffer (current-buffer))
                      (setq jupyter-current-client client)
                      (help-setup-xref
                       (list
                        ;; Don't capture a strong reference to the client
                        ;; object since we don't know when this reference will
                        ;; be cleaned up.
                        (let ((ref (jupyter-weak-ref client)))
                          (lambda ()
                            (let ((jupyter-current-client
                                   (jupyter-weak-ref-resolve ref)))
                              (if jupyter-current-client
                                  (jupyter-inspect code pos nil detail)
                                ;; TODO: Skip over this xref, need to figure
                                ;; out if going forward or backward first.
                                (error "Client has been removed"))))))
                       nil)
                      (jupyter-insert msg)))))
            (message "Nothing found for %s"
                     (with-temp-buffer
                       (insert code)
                       (goto-char pos)
                       (symbol-at-point)))))
      (message "Inspect timed out"))))

(define--jupyter-client-sender inspect-request
  :code nil
  :pos 0
  :detail 0)

(define-jupyter-client-handler inspect-reply)

;;;; Completion

(define--jupyter-client-sender complete-request
  :code nil
  :pos 0)

(define-jupyter-client-handler complete-reply)

;;;;; Code context

(cl-defgeneric jupyter-code-context (type)
  "Return a list, (CODE POS), for the context around `point'.
CODE is the required context for TYPE (either `inspect' or
`completion') and POS is the relative position of `point' within
CODE.  Depending on the current context such as the current
`major-mode', CODE and POS will be used for `:complete-request's
originating from `jupyter-completion-at-point' and
`:inspect-request's from `jupyter-inspect-at-point'.

The default methods return the `jupyter-line-or-region-context'.")

(defun jupyter-line-context ()
  "Return the code context for the current line."
  (jupyter-region-context (line-beginning-position) (line-end-position)))

(defun jupyter-region-context (beg end)
  "Return the code context between BEG and END.
BEG and END are the bounds of the region of text for which to
extract the context.  It is an error if `point' is not within
these bounds.  See `jupyter-code-context' for the form of the
returned list."
  (unless (<= beg (point) end)
    (error "Point not within bounds (%d %d)" beg end))
  (let ((code (buffer-substring-no-properties beg end))
        (pos (1+ (- (point) beg))))
    (list code (min pos (length code)))))

(defun jupyter-line-or-region-context ()
  "Return the code context of the region or line.
If the region is active, return it.  Otherwise return the line."
  (if (region-active-p)
      (jupyter-region-context (region-beginning) (region-end))
    (jupyter-line-context)))

(cl-defmethod jupyter-code-context ((_type (eql inspect)))
  (jupyter-line-or-region-context))

(cl-defmethod jupyter-code-context ((_type (eql completion)))
  (let ((ppss (syntax-ppss)))
    (if (zerop (nth 0 ppss))
        (jupyter-region-context (line-beginning-position) (point))
      (jupyter-region-context
       ;; Return a context including the deepest nested parenthesis and the
       ;; closest contiguous non-whitespace sequence of characters at the top
       ;; level.
       (save-excursion
         (when (nth 1 ppss)
           (goto-char (nth 1 ppss)))
         (skip-syntax-backward "->")
         (skip-syntax-backward "^->")
         (point))
       (point)))))

;;;;; Helpers for completion interface

(defun jupyter-completion-symbol-beginning (&optional pos)
  "Return the beginning position of a completion symbol.
The beginning position of the symbol around `point' is returned.
If no symbol exists around point, then `point' is returned.

If POS is non-nil, goto POS first."
  (save-excursion
    (and pos (goto-char pos))
    ;; FIXME: This is language specific
    (if (and (eq (char-syntax (char-before)) ?.)
             (not (eq (char-before) ?.)))
        ;; Complete operators, but not the field/attribute
        ;; accessor .
        (skip-syntax-backward ".")
      (skip-syntax-backward "w_"))
    (point)))

;; Adapted from `company-grab-symbol-cons'
(defun jupyter-completion-grab-symbol-cons (re &optional max-len)
  "Return the current completion prefix before point.
Return either a STRING or a (STRING . t) pair.  If RE matches the
beginning of the current symbol before point, return the latter.
Otherwise return the symbol before point.  If no completion can be
done at point, return nil.

MAX-LEN is the maximum number of characters to search behind the
begiining of the symbol at point to look for a match of RE."
  (let ((symbol (if (or (looking-at "\\>\\|\\_>")
                        ;; Complete operators
                        (and (char-before)
                             (eq (char-syntax (char-before)) ?.)))
                    (buffer-substring-no-properties
                     (jupyter-completion-symbol-beginning) (point))
                  (unless (and (char-after)
                               (memq (char-syntax (char-after)) '(?w ?_)))
                    ""))))
    (when symbol
      (save-excursion
        (forward-char (- (length symbol)))
        (if (looking-back re (if max-len
                                 (- (point) max-len)
                               (line-beginning-position)))
            (cons symbol t)
          symbol)))))

(defun jupyter-completion-number-p ()
  "Return non-nil if the text before `point' may be a floating point number."
  (and (char-before)
       (or (<= ?0 (char-before) ?9)
           (eq (char-before) ?.))
       (save-excursion
         (skip-syntax-backward "w.")
         (looking-at-p "[0-9]+\\.?[0-9]*"))))

;;;;; Extracting arguments from argument strings
;; This is mainly used for the Julia kernel which will return the type
;; information of method arguments and the methods file locations.

(defconst jupyter-completion-argument-regexp
  (rx
   (group "(" (zero-or-more anything) ")")
   (one-or-more anything) " "
   (group (one-or-more anything)) ?: (group (one-or-more digit)))
  "Regular expression to match arguments and file locations.")

(defun jupyter-completion--arg-extract-1 (pos)
  "Helper function for `jupyter-completion--arg-extract'.
Extract the arguments starting at POS, narrowing to the first
SEXP before extraction."
  (save-restriction
    (goto-char pos)
    (narrow-to-region
     pos (save-excursion (forward-sexp) (point)))
    (jupyter-completion--arg-extract)))

(defun jupyter-completion--arg-extract ()
  "Extract arguments from an argument string.
Works for Julia and Python."
  (let (arg-info
        inner-args ppss depth inner
        (start (1+ (point-min)))
        (get-sexp
         (lambda ()
           (buffer-substring-no-properties
            (point) (progn (forward-sexp) (point)))))
        (get-string
         (lambda (start)
           (string-trim
            (buffer-substring-no-properties
             start (1- (point)))))))
    (while (re-search-forward ",\\|::\\|;" nil t)
      (setq ppss (syntax-ppss)
            depth (nth 0 ppss)
            inner (nth 1 ppss))
      (cl-case (char-before)
        (?:
         (if (eq (char-after) ?{)
             (push (jupyter-completion--arg-extract-1 (point)) inner-args)
           (push (list (list (funcall get-sexp))) inner-args)))
        ((or ?, ?\;)
         (if (/= depth 1)
             (push (jupyter-completion--arg-extract-1 inner) inner-args)
           ;; ((string . sep) . inner-args)
           (push (cons (cons (funcall get-string start) (char-before)) (pop inner-args))
                 arg-info)
           (skip-syntax-forward "-")
           (setq start (point))))))
    (goto-char (point-max))
    (push (cons (cons (funcall get-string start) nil) (pop inner-args)) arg-info)
    (nreverse arg-info)))

(defun jupyter-completion--make-arg-snippet (args)
  "Construct a snippet from ARGS."
  (cl-loop
   with i = 1
   for top-args in args
   ;; TODO: Handle nested arguments
   for ((arg . sep) . inner-args) = top-args
   collect (format (concat "${%d:%s}" (when sep "%c")) i arg sep)
   into constructs
   and do (setq i (1+ i))
   finally return
   (concat "(" (mapconcat #'identity constructs " ") ")")))

;;;;; Completion prefix

(cl-defgeneric jupyter-completion-prefix (&optional re max-len)
  "Return the prefix for the current completion context.
The default method calls `jupyter-completion-grab-symbol-cons'
with RE and MAX-LEN as arguments, RE defaulting to \"\\\\.\".  It
also handles argument lists surrounded by parentheses specially
by considering an open parentheses and the symbol before it as a
completion prefix since some kernels will complete argument lists
if given such a prefix.

Note that the prefix returned is not the content sent to the
kernel, but the prefix used by `jupyter-completion-at-point'.  See
`jupyter-code-context' for what is actually sent to the kernel."
  (or re (setq re "\\."))
  (cond
   ;; FIXME: Needed for cases where all completions are retrieved
   ;; from Base.| and the prefix turns empty again after
   ;; Base.REPLCompletions)|
   ;;
   ;; Actually the problem stems from stting the prefix length to 0
   ;; in company in the case Base.| and we have not selected a
   ;; completion and just pass over it.
   ((and (looking-at-p "\\_>")
         (eq (char-syntax (char-before)) ?\)))
    nil)
   (t
    (unless (jupyter-completion-number-p)
      (jupyter-completion-grab-symbol-cons re max-len)))))

(defun jupyter-completion-construct-candidates (matches metadata)
  "Construct candidates for completion.
MATCHES are the completion matches returned by the kernel,
METADATA is any extra data associated with MATCHES that was
supplied by the kernel."
  (let* ((matches (append matches nil))
         (tail matches)
         (types (append (plist-get metadata :_jupyter_types_experimental) nil))
         (buf))
    (save-current-buffer
      (unwind-protect
          (while tail
            (cond
             ((string-match jupyter-completion-argument-regexp (car tail))
              (let* ((str (car tail))
                     (args-str (match-string 1 str))
                     (end (match-end 1))
                     (path (match-string 2 str))
                     (line (string-to-number (match-string 3 str)))
                     (snippet (progn
                                (unless buf
                                  (setq buf (generate-new-buffer " *temp*"))
                                  (set-buffer buf))
                                (insert args-str)
                                (goto-char (point-min))
                                (prog1 (jupyter-completion--make-arg-snippet
                                        (jupyter-completion--arg-extract))
                                  (erase-buffer)))))
                (setcar tail (substring (car tail) 0 end))
                (put-text-property 0 1 'snippet snippet (car tail))
                (put-text-property 0 1 'location (cons path line) (car tail))
                (put-text-property 0 1 'docsig (car tail) (car tail))))
             ;; TODO: This is specific to the results that
             ;; the python kernel returns, make a support
             ;; function?
             ((string-match-p "\\." (car tail))
              (setcar tail (car (last (split-string (car tail) "\\."))))))
            (setq tail (cdr tail)))
        (when buf (kill-buffer buf))))
    ;; When a type is supplied add it as an annotation
    (when types
      (let ((max-len (apply #'max (mapcar #'length matches))))
        (cl-mapc
         (lambda (match meta)
           (let* ((prefix (make-string (1+ (- max-len (length match))) ? ))
                  (annot (concat prefix (plist-get meta :type))))
             (put-text-property 0 1 'annot annot match)))
         matches types)))
    matches))

;;;;; Completion at point interface

(defvar jupyter-completion-cache nil
  "The cache for completion candidates.
A list that can take the following forms

    (PREFIX . CANDIDATES)
    (fetched PREFIX MESSAGE)

The first form states that the list of CANDIDATES is for the
prefix, PREFIX.

The second form signifies that the CANDIDATES for PREFIX must be
extracted from MESSAGE and converted to the first form.")

(defun jupyter-completion-prefetch-p (prefix)
  "Return non-nil if a prefetch for PREFIX should be performed.
Looks at `jupyter-completion-cache' to determine if its
candidates can be used for PREFIX."
  (not (and jupyter-completion-cache
            (if (eq (car jupyter-completion-cache) 'fetched)
                (equal (nth 1 jupyter-completion-cache) prefix)
              (or (equal (car jupyter-completion-cache) prefix)
                  (and (not (string= (car jupyter-completion-cache) ""))
                       (string-prefix-p (car jupyter-completion-cache) prefix))))
            ;; Invalidate the cache when completing argument lists
            (or (string= prefix "")
                (not (eq (aref prefix (1- (length prefix))) ?\())))))

(defun jupyter-completion-prefetch (fun)
  "Get completions for the current completion context.
Run FUN when the completions are available."
  (cl-destructuring-bind (code pos)
      (jupyter-code-context 'completion)
    (let ((req (let ((jupyter-inhibit-handlers t))
                 (jupyter-send-complete-request
                     jupyter-current-client
                   :code code :pos pos))))
      (prog1 req
        (jupyter-add-callback req :complete-reply fun)))))

(defvar company-minimum-prefix-length)
(defvar company-timer)

(defun jupyter-completion--company-idle-begin ()
  "Trigger an idle completion."
  (when company-timer
    (cancel-timer company-timer))
  (setq company-timer
        ;; NOTE: When we reach here `company-idle-delay' is `now' since
        ;; we are already inside a company completion so we can't use
        ;; it, just use a sensible time value instead.
        (run-with-timer
         0.1 nil
         (lambda (buf win tick pos)
           (let ((company-minimum-prefix-length 0))
             (company-idle-begin buf win tick pos)))
         (current-buffer) (selected-window)
         (buffer-chars-modified-tick) (point))))

(defun jupyter-completion-at-point ()
  "Function to add to `completion-at-point-functions'."
  (let ((prefix (jupyter-completion-prefix)) req)
    (when (and
           prefix jupyter-current-client
           ;; Don't try to send completion requests when the kernel is busy
           ;; since it doesn't appear that kernels respond to these requests
           ;; when the kernel is busy, at least the Julia kernel doesn't.
           ;;
           ;; FIXME: Maybe this is kernel dependent
           (not (jupyter-kernel-busy-p jupyter-current-client)))
      (when (consp prefix)
        (setq prefix (car prefix))
        (when (and (bound-and-true-p company-mode)
                   (< (length prefix) company-minimum-prefix-length))
          (jupyter-completion--company-idle-begin)))
      (when (jupyter-completion-prefetch-p prefix)
        (setq jupyter-completion-cache nil
              req (jupyter-completion-prefetch
                   (lambda (msg) (setq jupyter-completion-cache
                                       (list 'fetched prefix msg))))))
      (list
       (- (point) (length prefix)) (point)
       (completion-table-dynamic
        (lambda (_)
          (when (and req (not (jupyter-request-idle-p req))
                     (not (eq (jupyter-message-type
                               (jupyter-request-last-message req))
                              :complete-reply)))
            ;; Introduce a delay so that we give a chance for the
            ;; :complete-reply message to get handled.
            (sit-for 0.1))
          (when (eq (car jupyter-completion-cache) 'fetched)
            (jupyter-with-message-content (nth 2 jupyter-completion-cache)
                (status matches metadata)
              (setq jupyter-completion-cache
                    (cons (nth 1 jupyter-completion-cache)
                          (when (equal status "ok")
                            (jupyter-completion-construct-candidates
                             matches metadata))))))
          (cdr jupyter-completion-cache)))
       :exit-function
       #'jupyter-completion--post-completion
       :company-location
       (lambda (arg) (get-text-property 0 'location arg))
       :annotation-function
       (lambda (arg) (get-text-property 0 'annot arg))
       :company-docsig
       (lambda (arg) (get-text-property 0 'docsig arg))
       :company-doc-buffer
       #'jupyter-completion--company-doc-buffer))))

(defun jupyter-completion--company-doc-buffer (arg)
  "Send an inspect request for ARG to the kernel.
Use the `company-doc-buffer' to insert the results."
  (let ((buf (company-doc-buffer)))
    (jupyter-inspect arg 1 buf)
    (with-current-buffer buf
      (when (> (point-max) (point-min))
        (let ((inhibit-read-only t))
          (remove-text-properties
           (point-min) (point-max) '(read-only))
          (font-lock-mode 1)
          (goto-char (point-min))
          (current-buffer))))))

(defun jupyter-completion--post-completion (arg status)
  "If ARG is a completion with a snippet, expand the snippet.
Do this only if STATUS is sole or finished."
  (when (memq status '(sole finished))
    (jupyter-completion-post-completion arg)))

(defvar yas-minor-mode)

(cl-defgeneric jupyter-completion-post-completion (candidate)
  "Called when CANDIDATE was selected as the completion candidate.
The default implementation expands the snippet in CANDIDATE's
snippet text property, if any, and if `yasnippet' is available."
  (when (and (get-text-property 0 'snippet candidate)
             (require 'yasnippet nil t))
    (unless yas-minor-mode
      (yas-minor-mode 1))
    ;; Due to packages like smartparens
    (when (eq (char-after) ?\))
      (delete-char 1))
    (yas-expand-snippet
     (get-text-property 0 'snippet candidate)
     (save-excursion
       (forward-sexp -1)
       (point))
     (point))))

;;;; History

(define--jupyter-client-sender history-request
  :output nil
  :raw nil
  :hist-access-type "tail"
  :session nil
  :start nil
  :stop nil
  :n 10
  :pattern nil
  :unique nil)

(define-jupyter-client-handler history-reply)

;;;; Is Complete

(define--jupyter-client-sender is-complete-request
  :code nil)

(define-jupyter-client-handler is-complete-reply)

;;;; Comms

(define--jupyter-client-sender comm-info-request
  :target-name nil)

(define-jupyter-client-handler comm-info-reply)

(define--jupyter-client-sender comm-open
  :id nil
  :target-name nil
  :data nil)

(define--jupyter-client-sender comm-msg
  :id nil
  :data nil)

(define--jupyter-client-sender comm-close
  :id nil
  :data nil)

;;;; Kernel info

(defvar jupyter-kernel-language-mode-properties nil
  "An association list mapping language names to major mode properties.
The lists contain the cached information returned by the
function `jupyter-kernel-language-mode-properties'.")

(defun jupyter-kernel-info (client)
  "Return the kernel info plist of CLIENT.
Return CLIENT's kernel-info slot if non-nil.  Otherwise send a
`:kernel-info-request' to CLIENT's kernel, set CLIENT's
kernel-info slot to the plist retrieved from the kernel, and
return it.

If the kernel CLIENT is connected to does not respond to a
`:kernel-info-request', raise an error.

Note, the value of the :name key in the :language_info property
list is a symbol as opposed to a string for the purposes of
method dispatching.  Also all instances of \" \" in the language
name are changed to \"-\" and all uppercase characters lowered."
  (cl-check-type client jupyter-kernel-client)
  (or (oref client kernel-info)
      (let* ((jupyter-inhibit-handlers t)
             (req (jupyter-send-kernel-info-request client))
             (msg (jupyter-wait-until-received :kernel-info-reply
                    ;; Go to great lengths to ensure we have waited long
                    ;; enough.  When communicating with slow to start kernels
                    ;; behind a kernel server this is necessary.
                    req (* 3 jupyter-long-timeout) "Requesting kernel info...")))
        (unless msg
          (error "Kernel did not respond to kernel-info request"))
        (prog1 (oset client kernel-info (jupyter-message-content msg))
          ;; Canonicalize language name to a language symbol for method dispatching
          (let* ((info (plist-get (oref client kernel-info) :language_info))
                 (lang (plist-get info :name)))
            (plist-put info :name (intern (jupyter-canonicalize-language-string lang))))))))

(defun jupyter-kernel-language-mode-properties (client)
  "Get the `major-mode' info of CLIENT's kernel language.
Return a list

     (MODE SYNTAX-TABLE)

Where MODE is the `major-mode' to use for syntax highlighting
purposes and SYNTAX-TABLE is the syntax table of MODE."
  (cl-check-type client jupyter-kernel-client)
  (cl-destructuring-bind (&key name file_extension &allow-other-keys)
      (plist-get (jupyter-kernel-info client) :language_info)
    (cdr (or (assoc name jupyter-kernel-language-mode-properties)
             (with-temp-buffer
               (let ((buffer-file-name
                      (concat "jupyter-repl-lang" file_extension)))
                 (delay-mode-hooks (set-auto-mode)))
               (let ((item (cons name (list major-mode (syntax-table)))))
                 (prog1 item
                   (push item jupyter-kernel-language-mode-properties))))))))

(defun jupyter-kernel-language (client)
  "Return the language (as a symbol) of the kernel CLIENT is connected to."
  (cl-check-type client jupyter-kernel-client)
  (plist-get (plist-get (jupyter-kernel-info client) :language_info) :name))

(defun jupyter-kernel-language-mode (client)
  "Return the `major-mode' used for CLIENT's kernel language."
  (cl-check-type client jupyter-kernel-client)
  (nth 0 (jupyter-kernel-language-mode-properties client)))

(defun jupyter-kernel-language-syntax-table (client)
  "Return the `syntax-table' used for CLIENT's kernel language."
  (cl-check-type client jupyter-kernel-client)
  (nth 1 (jupyter-kernel-language-mode-properties client)))

(defun jupyter-load-language-support (client)
  "Load language support definitions for CLIENT.
CLIENT is a kernel client."
  (cl-check-type client jupyter-kernel-client)
  (let* ((lang (jupyter-kernel-language client))
         (support (intern (format "jupyter-%s" lang))))
    (require support nil t)))

(define--jupyter-client-sender kernel-info-request)

(define-jupyter-client-handler kernel-info-reply)

;;;; Shutdown

(define--jupyter-client-sender shutdown-request
  :restart nil)

(define-jupyter-client-handler shutdown-reply)

;;; IOPUB handlers

(define-jupyter-client-handler comm-open ((client jupyter-kernel-client) req msg)
  (jupyter-with-message-content msg (id data)
    (puthash id (cons (jupyter-request-id req) data)
             (oref client comms))))

(define-jupyter-client-handler comm-close ((client jupyter-kernel-client) _req msg)
  (jupyter-with-message-content msg (id)
    (remhash id (oref client comms))))

(define-jupyter-client-handler comm-msg)

(define-jupyter-client-handler stream)

(define-jupyter-client-handler execute-input)

(define-jupyter-client-handler execute-result)

(defun jupyter-display-traceback (traceback)
  "Display TRACEBACK in a dedicated buffer."
  (when (or (vectorp traceback) (listp traceback))
    (setq traceback (concat (mapconcat #'identity traceback "\n") "\n")))
  (jupyter-with-display-buffer "traceback" 'reset
    (jupyter-insert-ansi-coded-text traceback)
    (goto-char (point-min))
    (jupyter-display-current-buffer-guess-where :error)))

(define-jupyter-client-handler error)

(defun jupyter-execution-state (client)
  "Return the execution state of CLIENT's kernel."
  (cl-check-type client jupyter-kernel-client)
  (oref client execution-state))

(defun jupyter-kernel-busy-p (client)
  "Return non-nil if the kernel CLIENT is connected to is busy."
  (cl-check-type client jupyter-kernel-client)
  (equal (jupyter-execution-state client) "busy"))

(define-jupyter-client-handler status)

(define-jupyter-client-handler clear-output)

(define-jupyter-client-handler display-data)

(define-jupyter-client-handler update-display-data)

(provide 'jupyter-client)

;;; jupyter-client.el ends here
