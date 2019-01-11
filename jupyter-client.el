;;; jupyter-client.el --- A Jupyter kernel client -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 06 Jan 2018
;; Version: 0.6.0

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

(defgroup jupyter-client nil
  "A Jupyter client."
  :group 'jupyter)

(require 'jupyter-base)
(require 'jupyter-channels)
(require 'jupyter-channel-ioloop)
(require 'jupyter-mime)
(require 'jupyter-messages)

(declare-function company-begin-backend "ext:company" (backend &optional callback))
(declare-function company-doc-buffer "ext:company" (&optional string))
(declare-function company-idle-begin "ext:company")

(declare-function yas-minor-mode "ext:yasnippet" (&optional arg))
(declare-function yas-expand-snippet "ext:yasnippet" (content &optional start end expand-env))

(declare-function hash-table-values "subr-x" (hash-table))
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
handler methods. If set to a list of `jupyter-message-types',
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
   (pending-requests
    :type ring
    :initform (make-ring 10)
    :documentation "A ring of pending `jupyter-request's.
A request is pending if it has not been sent to the kernel via the
client's ioloop slot.")
   (execution-state
    :type string
    :initform "idle"
    :documentation "The current state of the kernel. Can be
either \"idle\", \"busy\", or \"starting\".")
   (requests
    :type hash-table
    :initform (make-hash-table :test 'equal)
    :documentation "A hash table with message ID's as keys.
This is used to register callback functions to run once a reply
from a previously sent request is received. See
`jupyter-add-callback'. Note that this is also used to filter
received messages that originated from a previous request by this
client. Whenever the client sends a message in which a reply is
expected, it sets an entry in this table to represent the fact
that the message has been sent. So if there is a non-nil value
for a message ID it means that a message has been sent and the
client is expecting a reply from the kernel.")
   (kernel-info
    :type json-plist
    :initform nil
    :documentation "The saved kernel info created when first
initializing this client. When `jupyter-start-channels' is
called, this will be set to the kernel info plist returned
from an initial `:kernel-info-request'.")
   (ioloop
    :type (or null jupyter-channel-ioloop)
    :initform nil
    :documentation "The process which receives events from channels.")
   (session
    :type jupyter-session
    :documentation "The session for this client.")
   (comms
    :type hash-table
    :initform (make-hash-table :test 'equal)
    :documentation "A hash table with comm ID's as keys.
Contains all of the open comms. Each value is a cons cell (REQ .
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
variables and intermediate ioloop process output. When the ioloop
slot is non-nil, its `process-buffer' will be `eq' to this
buffer.")
   (channels
    :type list
    :initform nil
    :initarg :channels
    :documentation "A property list describing the channels.
The keys are channel types whose values are the status of the
channels. The exception is the heartbeat channel. The value of
the :hb key is a `jupyter-hb-channel'.")))

;;; `jupyter-current-client' language method specializer

(defvar jupyter--generic-lang-used (make-hash-table :test #'eql))

(cl-generic-define-generalizer jupyter--generic-lang-generalizer
  50 (lambda (name &rest _)
       `(when (and ,name (object-of-class-p ,name 'jupyter-kernel-client))
          ;; TODO: Make `jupyter-kernel-language' a symbol
          ;; to avoid interning a constant string.
          (gethash (intern (jupyter-kernel-language ,name))
                   jupyter--generic-lang-used)))
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

;;; Initializing a `jupyter-kernel-client'

(cl-defmethod initialize-instance ((client jupyter-kernel-client) &rest _slots)
  (cl-call-next-method)
  (let ((buffer (generate-new-buffer " *jupyter-kernel-client*")))
    (oset client -buffer buffer)
    (jupyter-add-finalizer client
      (lambda ()
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        ;; Ensure the ioloop process gets cleaned up when the client goes out
        ;; of scope.
        (when (jupyter-channels-running-p client)
          (jupyter-stop-channels client))))))

(defun jupyter-clients ()
  "Return a list of all `jupyter-kernel-clients'."
  (jupyter-all-objects 'jupyter--clients))

(defun jupyter-find-client-for-session (session-id)
  "Return the kernel client whose session has SESSION-ID."
  (or (cl-find-if
       (lambda (x) (string= (jupyter-session-id (oref x session)) session-id))
       (jupyter-clients))
      (error "No client found for session (%s)" session-id)))

(defun jupyter-initialize-connection (client info-or-session)
  "Initialize CLIENT with connection INFO-OR-SESSION.
INFO-OR-SESSION can be a file name, a plist, or a
`jupyter-session' object that will be used to initialize CLIENT's
connection. When INFO-OR-SESSION is a file name, read the
contents of the file as a JSON plist and create a new
`jupyter-session' from it. For remote files, create a new
`jupyter-session' based on the plist returned from
`jupyter-tunnel-connection'. When INFO-OR-SESSION is a plist, use
it to create a new `jupyter-session'. Finally, when
INFO-OR-SESSION is a `jupyter-session' it is used as the session
for client. The session object used to initialize the connection
will be set as the session slot of CLIENT.

The necessary keys and values to initialize a connection can be
found at
http://jupyter-client.readthedocs.io/en/latest/kernels.html#connection-files.

As a side effect, if CLIENT is already connected to a kernel its
connection is terminated before initializing a new one."
  (cl-check-type client jupyter-kernel-client)
  (let* ((session nil)
         (conn-info
          (cond
           ((jupyter-session-p info-or-session)
            (setq session info-or-session)
            (jupyter-session-conn-info session))
           ((json-plist-p info-or-session)
            info-or-session)
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
    (cl-destructuring-bind
        (&key shell_port iopub_port stdin_port hb_port ip
              key transport signature_scheme
              &allow-other-keys)
        conn-info
      (when (and (> (length key) 0)
                 (not (functionp
                       (intern (concat "jupyter-" signature_scheme)))))
        (error "Unsupported signature scheme: %s" signature_scheme))
      ;; Stop the channels if connected to some other kernel
      (when (jupyter-channels-running-p client)
        (jupyter-stop-channels client))
      ;; Initialize the channels
      (unless session
        (setq session (jupyter-session :key key :conn-info conn-info)))
      (oset client session session)
      (let ((addr (lambda (port) (format "%s://%s:%d" transport ip port))))
        (setf (oref client channels)
              ;; Construct the channel plist
              (cl-list*
               :hb (make-instance
                    'jupyter-hb-channel
                    :session session
                    :endpoint (funcall addr hb_port))
               (cl-loop
                for (channel . port) in `((:stdin . ,stdin_port)
                                          (:shell . ,shell_port)
                                          (:iopub . ,iopub_port))
                collect channel
                and collect
                ;; The session will be associated with these channels in the
                ;; ioloop subprocess. See `jupyter-start-channels'.
                (list :endpoint (funcall addr port)
                      :alive-p nil))))))))

;;; Client local variables

(defmacro jupyter-with-client-buffer (client &rest body)
  "Run a form inside CLIENT's IOloop subprocess buffer.
BODY is run with the current buffer set to CLIENT's IOloop
subprocess buffer."
  (declare (indent 1))
  `(progn
     (cl-check-type ,client jupyter-kernel-client)
     ;; NOTE: -buffer will be set as the IOLoop process buffer, see
     ;; `jupyter-start-channels', but before the IOLoop process is started we
     ;; would like to have a buffer available so that client local variables
     ;; can be set on the buffer. This is why we create our own buffer when a
     ;; client is initialized.
     (with-current-buffer (oref ,client -buffer)
       ,@body)))

(defun jupyter-set (client symbol newval)
  "Set CLIENT's local value for SYMBOL to NEWVAL."
  (jupyter-with-client-buffer client
    (set (make-local-variable symbol) newval)))

(defun jupyter-get (client symbol)
  "Get CLIENT's local value of SYMBOL."
  (jupyter-with-client-buffer client
    (symbol-value symbol)))

;;; Hooks

(defun jupyter-add-hook (client hook function &optional append)
  "Add to the CLIENT value of HOOK the function FUNCTION.
APPEND has the same meaning as in `add-hook' and FUNCTION is
added to HOOK using `add-hook', but local only to CLIENT. Note
that the CLIENT should have its channels already started before
this is called."
  (declare (indent 2))
  (jupyter-with-client-buffer client
    (add-hook hook function append t)))

(defun jupyter-run-hook-with-args-until-success (client hook &rest args)
  "Run CLIENT's value for HOOK with the arguments ARGS.
CLIENT is passed as the first argument and then ARGS."
  (jupyter-with-client-buffer client
    (when jupyter--debug
      (message "RUN-HOOK: %s" hook))
    (with-demoted-errors "Error in Jupyter message hook: %S"
      (apply #'run-hook-with-args-until-success hook client args))))

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
the default value for all slots. Note, the `:id' and
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

(cl-defmethod jupyter-send :before ((_client jupyter-kernel-client)
                                    _channel
                                    type
                                    message
                                    &optional _msg-id)
  (when jupyter--debug
    (message "SENDING: %s %s" type message)))

(cl-defmethod jupyter-send ((client jupyter-kernel-client)
                            channel
                            type
                            message
                            &optional msg-id)
  "Send a message on CLIENT's CHANNEL.
Return a `jupyter-request' representing the sent message. CHANNEL
is one of the channel keywords, either (:stdin or :shell).
TYPE is one of the `jupyter-message-types'. MESSAGE is the
message sent on CHANNEL.

Note that you can manipulate how to handle messages received in
response to the sent message, see `jupyter-add-callback' and
`jupyter-request-inhibited-handlers'."
  (declare (indent 1))
  (let ((ioloop (oref client ioloop)))
    (unless ioloop
      (signal 'wrong-type-argument (list 'jupyter-ioloop ioloop 'ioloop)))
    (jupyter-verify-inhibited-handlers)
    (let ((msg-id (or msg-id (jupyter-new-uuid))))
      (jupyter-send ioloop 'send channel type message msg-id)
      ;; Anything sent to stdin is a reply not a request so don't add it as a
      ;; pending request
      (unless (eq channel :stdin)
        (let ((req (jupyter-generate-request client message)))
          (setf (jupyter-request-id req) msg-id)
          (setf (jupyter-request-inhibited-handlers req) jupyter-inhibit-handlers)
          (jupyter--push-pending-request client req))))))

;;; Pending requests

(defun jupyter--pop-pending-request (client)
  "Return the oldest pending request CLIENT sent to its ioloop."
  (with-slots (pending-requests) client
    (unless (ring-empty-p pending-requests)
      (ring-remove pending-requests))))

(defun jupyter--push-pending-request (client req)
  "For CLIENT, mark REQ as the newest pending request.
Return REQ. A request is pending if it has not been sent to the
kernel via CLIENT's ioloop."
  (with-slots (pending-requests) client
    (prog1 req
      (ring-insert+extend pending-requests req 'grow))))

;;; HB channel methods

(cl-defmethod jupyter-hb-pause ((client jupyter-kernel-client))
  "Pause CLIENT's heartbeeat channel."
  (jupyter-hb-pause (plist-get (oref client channels) :hb)))

(cl-defmethod jupyter-hb-unpause ((client jupyter-kernel-client))
  "Unpause CLIENT's heartbeat channel."
  (jupyter-hb-unpause (plist-get (oref client channels) :hb)))

(cl-defmethod jupyter-hb-beating-p ((client jupyter-kernel-client))
  "Is CLIENT still connected to its kernel?"
  (jupyter-hb-beating-p (plist-get (oref client channels) :hb)))

;;; IOLoop handlers (receiving messages, starting/stopping channels)

;;;; Sending/receiving

(cl-defmethod jupyter-ioloop-handler ((_ioloop jupyter-ioloop)
                                      (client jupyter-kernel-client)
                                      (event (head sent)))
  (cl-destructuring-bind (_ channel-type msg-id) event
    (unless (eq channel-type :stdin)
      ;; Anything sent on stdin is a reply and therefore never added as a
      ;; pending request
      (let ((req (jupyter--pop-pending-request client))
            (requests (oref client requests)))
        (cl-assert (equal (jupyter-request-id req) msg-id)
                   nil "Message request sent out of order to the kernel.")
        (puthash msg-id req requests)
        (puthash "last-sent" req requests)))))

(cl-defmethod jupyter-ioloop-printer ((_ioloop jupyter-ioloop)
                                      (_client jupyter-kernel-client)
                                      (event (head message)))
  (cl-destructuring-bind (_ channel _idents . msg) event
    (format "%s" (list
                  channel
                  (jupyter-message-type msg)
                  (jupyter-message-content msg)))))

(cl-defmethod jupyter-ioloop-handler ((_ioloop jupyter-ioloop)
                                      (client jupyter-kernel-client)
                                      (event (head message)))
  "For CLIENT, queue a message EVENT to be handled."
  (cl-destructuring-bind (_ channel _idents . msg) event
    ;; Run immediately after handling this event, i.e. on the next command loop
    (run-at-time 0 nil #'jupyter-handle-message client channel msg)))

;;;; Channel alive methods

(cl-defmethod jupyter-channel-alive-p ((client jupyter-kernel-client) channel)
  (cl-assert (memq channel '(:hb :stdin :shell :iopub)) t)
  (with-slots (channels) client
    (if (not (eq channel :hb))
        (plist-get (plist-get channels channel) :alive-p)
      (setq channel (plist-get channels :hb))
      ;; The hb channel is implemented locally in the current process whereas the
      ;; other channels are implemented in subprocesses and the current process
      ;; acts as a proxy.
      (and channel (jupyter-channel-alive-p channel)))))

;;;; Start channel methods

(cl-defmethod jupyter-ioloop-handler ((_ioloop jupyter-ioloop)
                                      (client jupyter-kernel-client)
                                      (event (head start-channel)))
  (plist-put (plist-get (oref client channels) (cadr event)) :alive-p t))

(cl-defmethod jupyter-start-channel ((client jupyter-kernel-client) channel)
  (cl-assert (memq channel '(:hb :stdin :shell :iopub)) t)
  (unless (jupyter-channel-alive-p client channel)
    (with-slots (channels) client
      (if (eq channel :hb)
          (jupyter-start-channel (plist-get channels :hb))
        (cl-destructuring-bind (&key endpoint &allow-other-keys)
            (plist-get channels channel)
          (jupyter-send
           (oref client ioloop) 'start-channel channel endpoint))))))

(cl-defmethod jupyter-start-channel :after ((client jupyter-kernel-client) channel)
  "Verify that CLIENT's CHANNEL started.
Raise an error if it did not start within
`jupyter-default-timeout'."
  (unless (or (eq channel :hb) (jupyter-channel-alive-p client channel))
    (with-slots (ioloop) client
      (or (jupyter-ioloop-wait-until ioloop 'start-channel
            (lambda (_) (jupyter-channel-alive-p client channel)))
          (error "Channel not started in ioloop subprocess (%s)" channel)))))

;;;; Stop channel methods

(cl-defmethod jupyter-ioloop-handler ((_ioloop jupyter-ioloop)
                                      (client jupyter-kernel-client)
                                      (event (head stop-channel)))
  (plist-put (plist-get (oref client channels) (cadr event)) :alive-p nil))

(cl-defmethod jupyter-stop-channel ((client jupyter-kernel-client) channel)
  (cl-assert (memq channel '(:hb :stdin :shell :iopub)) t)
  (when (jupyter-channel-alive-p client channel)
    (if (eq channel :hb)
        (jupyter-stop-channel (plist-get (oref client channels) :hb))
      (jupyter-send (oref client ioloop) 'stop-channel channel))))

(cl-defmethod jupyter-stop-channel :after ((client jupyter-kernel-client) channel)
  "Verify that CLIENT's CHANNEL has stopped.
Raise a warning if it has not been stopped within
`jupyter-default-timeout'."
  (unless (or (eq channel :hb) (not (jupyter-channel-alive-p client channel)))
    (with-slots (ioloop) client
      (or (jupyter-ioloop-wait-until ioloop 'stop-channel
            (lambda (_) (not (jupyter-channel-alive-p client channel))))
          (warn "Channel not stopped in ioloop subprocess")))))

;;; Starting/stopping IOLoop

(cl-defmethod jupyter-start-channels :before ((client jupyter-kernel-client)
                                              &rest _)
  "Start CLIENT's channel ioloop."
  (with-slots (ioloop session) client
    (unless ioloop
      (oset client ioloop (jupyter-channel-ioloop))
      (setq ioloop (oref client ioloop)))
    (unless (jupyter-ioloop-alive-p ioloop)
      (jupyter-ioloop-start ioloop session client))))

(cl-defmethod jupyter-start-channels ((client jupyter-kernel-client)
                                      &key (shell t)
                                      (iopub t)
                                      (stdin t)
                                      (hb t))
  "Start the pre-configured channels of CLIENT.
Before starting the channels, ensure that the channel subprocess
responsible for encoding/decoding messages and sending/receiving
messages to/from the kernel is running.

Call `jupyter-start-channel' for every channel whose key has a
non-nil value passed to this function.

If the shell channel is started, send an initial
`:kernel-info-request' to set the kernel-info slot of CLIENT if
necessary."
  (cl-loop
   for (channel . start) in `((:hb . ,hb)
                              (:shell . ,shell)
                              (:iopub . ,iopub)
                              (:stdin . ,stdin))
   when start do (jupyter-start-channel client channel))
  ;; Needed for reliability. Sometimes we are not fast enough to capture the
  ;; startup message of a kernel.
  (sleep-for 0.3))

(cl-defmethod jupyter-stop-channels ((client jupyter-kernel-client))
  "Stop any running channels of CLIENT."
  (cl-loop
   for channel in '(:shell :iopub :stdin :hb)
   do (jupyter-stop-channel client channel)))

(cl-defmethod jupyter-stop-channels :after ((client jupyter-kernel-client)
                                            &rest _)
  "Stop CLIENT's channel ioloop."
  (with-slots (ioloop) client
    (when ioloop
      (jupyter-ioloop-stop ioloop))))

(cl-defmethod jupyter-channels-running-p ((client jupyter-kernel-client))
  "Are any channels of CLIENT running?"
  (cl-loop
   for channel in '(:shell :iopub :stdin :hb)
   thereis (jupyter-channel-alive-p client channel)))

;;; Message callbacks

(defun jupyter--run-callbacks (req msg)
  "Run REQ's MSG callbacks.
See `jupyter-add-callback'."
  (when-let* ((callbacks (and req (jupyter-request-callbacks req))))
    ;; Callback for all message types
    (funcall (alist-get t callbacks #'identity) msg)
    (funcall (alist-get (jupyter-message-type msg) callbacks #'identity) msg)))

(defmacro jupyter--set-callback (place callback)
  "Build a callback from a previous callback at PLACE.
PLACE is a generalized variable and CALLBACK should be bound to a
function that takes a single argument.

Construct a new function, combining any function stored in PLACE
with CALLBACK. If there is a function stored in PLACE, it is
assumed to take a single argument.

The combined function is equivalent to

    (lambda (msg)
      (funcall PLACE msg)
      (funcall CALLBACK msg))

Store the new function in PLACE."
  (gv-letplace (getter setter) place
    (macroexp-let2 nil old getter
      (funcall setter
               `(lambda (msg)
                  (and (functionp ,old) (funcall ,old msg))
                  (funcall ,callback msg))))))

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
  (jupyter--set-callback
   (alist-get msg-type (jupyter-request-callbacks req)) cb))

(defun jupyter-add-callback (req msg-type cb &rest callbacks)
  "Add a callback to run when a message is received for a request.
REQ is a `jupyter-request' returned by one of the request methods
of a kernel client. MSG-TYPE is one of the keys in
`jupyter-message-types'. CB is the callback function to run when
a message with MSG-TYPE is received for REQ.

MSG-TYPE can also be a list, in which case run CB for every
MSG-TYPE in the list. If MSG-TYPE is t, run CB for every message
received for REQ.

Any additional arguments to `jupyter-add-callback' are
interpreted as additional CALLBACKS to add to REQ. So to add
multiple callbacks you would do

    (jupyter-add-callback
        (jupyter-send-execute-request client :code \"1 + 2\")
      :status (lambda (msg) ...)
      :execute-reply (lambda (msg) ...)
      :execute-result (lambda (msg) ...))"
  (declare (indent 1))
  (if (jupyter-request-idle-received-p req)
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

(defun jupyter-wait-until (req msg-type cb &optional timeout progress-msg)
  "Wait until conditions for a request are satisfied.
REQ, MSG-TYPE, and CB have the same meaning as in
`jupyter-add-callback'. If CB returns non-nil within TIMEOUT
seconds, return the message that caused CB to return non-nil. If
CB never returns a non-nil value within TIMEOUT, return nil. Note
that if no TIMEOUT is given, `jupyter-default-timeout' is used.

If PROGRESS-MSG is non-nil, it should be a message string to
display for reporting progress to the user while waiting."
  (declare (indent 2))
  (let (msg)
    (jupyter-add-callback req
      msg-type (lambda (m) (setq msg (when (funcall cb m) m))))
    (jupyter-with-timeout
        (progress-msg (or timeout jupyter-default-timeout))
      msg)))

(defun jupyter-wait-until-idle (req &optional timeout progress-msg)
  "Wait until a status: idle message is received for a request.
REQ has the same meaning as in `jupyter-add-callback'. If an idle
message for REQ is received within TIMEOUT seconds, return the
message. Otherwise return nil if the message was not received
within TIMEOUT. Note that if no TIMEOUT is given, it defaults to
`jupyter-default-timeout'.

If PROGRESS-MSG is non-nil, it is a message string to display for
reporting progress to the user while waiting."
  (jupyter-wait-until req :status
    #'jupyter-message-status-idle-p timeout progress-msg))

(defun jupyter-wait-until-received (msg-type req &optional timeout progress-msg)
  "Wait until a message of a certain type is received for a request.
MSG-TYPE and REQ have the same meaning as their corresponding
arguments in `jupyter-add-callback'. If no message that matches
MSG-TYPE is received for REQ within TIMEOUT seconds, return nil.
Otherwise return the first message that matched MSG-TYPE. Note
that if no TIMEOUT is given, it defaults to
`jupyter-default-timeout'.

If PROGRESS-MSG is non-nil, it is a message string to display for
reporting progress to the user while waiting."
  (declare (indent 1))
  (jupyter-wait-until req msg-type #'identity timeout progress-msg))

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
     when (and (jupyter-request-idle-received-p req)
               (not (eq req last-sent)))
     do (unwind-protect
            (jupyter-drop-request client req)
          (remhash (jupyter-request-id req) requests)))))

(defsubst jupyter--run-handler-p (req msg)
  "Return non-nil if REQ doesn't inhibit the handler for MSG."
  (let* ((ihandlers (and req (jupyter-request-inhibited-handlers req)))
         (type (and (listp ihandlers)
                    (memq (jupyter-message-type msg) ihandlers))))
    (not (or (eq ihandlers t)
             (if (eq (car ihandlers) 'not) (not type) type)))))

(defun jupyter--run-handler-maybe (client channel req msg)
  "Possibly run CLIENT's CHANNEL handler on REQ's received MSG."
  (when (jupyter--run-handler-p req msg)
    (jupyter-handle-message channel client req msg)))

(cl-defmethod jupyter-handle-message ((client jupyter-kernel-client) channel msg)
  "Process a message on CLIENT's CHANNEL.
When a message is received from the kernel, the
`jupyter-handle-message' method is called on the client. The
client method runs any callbacks for the message and possibly
runs the client handler for the channel the message was received
on. The channel's `jupyter-handle-message' method will then pass
the message to the appropriate message handler based on the
message type.

So when a message is received from the kernel the following steps
are taken:

- `jupyter-handle-message' (client)
   - Run callbacks for message
   - Possibly run channel handlers
     - `jupyter-handle-message' (channel)
       - Based on message type, dispatch to
         `jupyter-handle-execute-result',
         `jupyter-handle-kernel-info-reply', ...
   - Remove request from client request table when idle message is received"
  (when msg
    (let* ((jupyter-current-client client)
           (pmsg-id (jupyter-message-parent-id msg))
           (requests (oref client requests))
           (req (gethash pmsg-id requests)))
      (if (not req)
          (when (jupyter-get client 'jupyter-include-other-output)
            (jupyter--run-handler-maybe client channel req msg))
        (setf (jupyter-request-last-message req) msg)
        (when (eq (jupyter-message-type msg) :status)
          (oset client execution-state
                (jupyter-message-get msg :execution_state)))
        (unwind-protect
            (jupyter--run-callbacks req msg)
          (unwind-protect
              (jupyter--run-handler-maybe client channel req msg)
            (when (jupyter-message-status-idle-p msg)
              (setf (jupyter-request-idle-received-p req) t))
            (jupyter--drop-idle-requests client)))))))

;;; Channel handler macros

(defmacro jupyter-dispatch-message-cases (client req msg cases)
  "Dispatch to CLIENT handler's based on REQ and MSG.
CASES defines the handlers to dispatch to based on the
`jupyter-message-type' of MSG and should be a list of lists, the
first element of each inner list being the name of the handler,
excluding the `jupyter-handle-' prefix. The rest of the elements
in the list are the name of the keys that will be extracted from
the `jupyter-message-content' of MSG and passed to the handler in
the same order as they appear. For example,

    (jupyter-dispatch-message-cases client req msg
      ((shutdown-reply restart)
       (stream name text)))

will be transformed to

    (let ((content (jupyter-message-content msg)))
      (pcase (jupyter-message-type msg)
        (:shutdown-reply
          (cl-destructuring-bind (&key restart &allow-other-keys)
              content
            (jupyter-handle-shutdown-reply client req restart)))
        (:stream
          (cl-destructuring-bind (&key name text &allow-other-keys)
              content
            (jupyter-handle-stream client req name text)))
        (_ (warn \"Message type not handled (%s)\"
                (jupyter-message-type msg)))))"
  (declare (indent 3))
  (let ((handlers nil)
        (content (make-symbol "contentvar"))
        (jclient (make-symbol "clientvar"))
        (jreq (make-symbol "reqvar"))
        (jmsg (make-symbol "msgvar")))
    (dolist (case cases)
      (cl-destructuring-bind (msg-type . keys) case
        (let ((handler (intern (format "jupyter-handle-%s" msg-type)))
              (msg-type (intern (concat ":" (symbol-name msg-type)))))
          (push `(,msg-type
                  (cl-destructuring-bind (&key ,@keys &allow-other-keys)
                      ,content
                    (,handler ,jclient ,jreq ,@keys)))
                handlers))))
    `(let* ((,jmsg ,msg)
            (,jreq ,req)
            (,jclient ,client)
            (,content (jupyter-message-content ,jmsg)))
       (pcase (jupyter-message-type ,jmsg)
         ,@handlers
         (_ (warn "Message type not handled (%s)"
                  (jupyter-message-type msg)))))))

;;; STDIN handlers

(cl-defmethod jupyter-handle-message ((_channel (eql :stdin))
                                      client
                                      req
                                      msg)
  (unless (jupyter-run-hook-with-args-until-success
           client 'jupyter-stdin-message-hook msg)
    (jupyter-dispatch-message-cases client req msg
      ((input-reply prompt password)
       (input-request prompt password)))))

(cl-defgeneric jupyter-handle-input-request ((client jupyter-kernel-client)
                                             _req
                                             prompt
                                             password)
  "Handle an input request from CLIENT's kernel.
PROMPT is the prompt the kernel would like to show the user. If
PASSWORD is non-nil, then `read-passwd' is used to get input from
the user. Otherwise `read-from-minibuffer' is used."
  (declare (indent 1))
  (let* ((value nil)
         (msg (jupyter-message-input-reply
               :value (condition-case nil
                          (if (eq password t) (read-passwd prompt)
                            (setq value (read-from-minibuffer prompt)))
                        (quit "")))))
    (jupyter-send client :stdin :input-reply msg)
    (or value "")))

(defalias 'jupyter-handle-input-reply 'jupyter-handle-input-request)

;;; SHELL handlers

;; http://jupyter-client.readthedocs.io/en/latest/messaging.html#messages-on-the-shell-router-dealer-channel
(cl-defmethod jupyter-handle-message ((_channel (eql :shell))
                                      client
                                      req
                                      msg)
  (unless (jupyter-run-hook-with-args-until-success
           client 'jupyter-shell-message-hook msg)
    (jupyter-dispatch-message-cases client req msg
      ((execute-reply status execution_count user_expressions payload)
       (shutdown-reply restart)
       (inspect-reply found data metadata)
       (complete-reply matches cursor_start cursor_end metadata)
       (history-reply history)
       (is-complete-reply status indent)
       (comm-info-reply comms)
       (kernel-info-reply protocol_version implementation
                          implementation_version language_info
                          banner help_links)))))

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

(cl-defgeneric jupyter-read-expression ()
  "Read an expression using the `jupyter-current-client' for completion.
The expression is read from the minibuffer and the expression
history is obtained from the `jupyter-eval-expression-history'
client local variable.

Methods that extend this generic function should
`cl-call-next-method' as a last step."
  (cl-check-type jupyter-current-client jupyter-kernel-client
                 "Need a client to read an expression")
  (let ((client jupyter-current-client))
    (jupyter-with-client-buffer jupyter-current-client
      (minibuffer-with-setup-hook
          (lambda ()
            (setq jupyter-current-client client)
            ;; TODO: Enable the kernel languages mode using
            ;; `jupyter-repl-language-mode', but there are
            ;; issues with enabling a major mode.
            (add-hook 'completion-at-point-functions
                      'jupyter-completion-at-point nil t)
            (add-hook 'minibuffer-exit-hook
                      'jupyter--teardown-minibuffer nil t))
        (read-from-minibuffer
         "Jupyter Ex: " nil
         read-expression-map
         nil 'jupyter-eval-expression-history)))))

(defun jupyter--display-eval-result (msg)
  (jupyter-with-message-data msg ((res text/plain))
    (if (null res)
        (jupyter-with-display-buffer "result" 'reset
          (jupyter-with-message-content msg (data metadata)
            (jupyter-insert data metadata))
          (goto-char (point-min))
          (display-buffer (current-buffer)))
      (setq res (ansi-color-apply res))
      (if (cl-loop
           with nlines = 0
           for c across res when (eq c ?\n) do (cl-incf nlines)
           thereis (> nlines 10))
          (jupyter-with-display-buffer "result" 'reset
            (insert res)
            (goto-char (point-min))
            (display-buffer (current-buffer)))
        (message "%s" res)))))

(defun jupyter-eval (code &optional mime)
  "Send an execute request for CODE, wait for the execute result.
The `jupyter-current-client' is used to send the execute request.
All client handlers are inhibited for the request. In addition,
the history of the request is not stored. Return the MIME
representation of the result. If MIME is nil, return the
text/plain representation."
  (interactive (list (jupyter-read-expression) nil))
  (cl-check-type jupyter-current-client jupyter-kernel-client
                 "Need a client to evaluate code")
  (let ((msg (jupyter-wait-until-received :execute-result
               (let* ((jupyter-inhibit-handlers t)
                      (req (jupyter-send-execute-request jupyter-current-client
                             :code code :store-history nil)))
                 (prog1 req
                   (jupyter-add-callback req
                     :execute-reply
                     (lambda (msg)
                       (jupyter-with-message-content msg (status evalue)
                         (unless (equal status "ok")
                           (error "%s" (ansi-color-apply evalue)))))))))))
    (when msg
      (jupyter-message-data msg (or mime :text/plain)))))

(defun jupyter-eval-string (str &optional cb)
  "Evaluate STR using the `jupyter-current-client'.
Replaces the contents of the last cell in the REPL buffer with
STR before evaluating.

If the result of evaluation is more than 10 lines long, a buffer
displaying the results is shown. For results less than 10 lines
long, the result is displayed in the minibuffer.

CB is a function to call with the `:execute-result' message when
the evalution is successful. When CB is nil, its behavior defaults
to the above explanation."
  (interactive (list (jupyter-read-expression) nil))
  (unless jupyter-current-client
    (user-error "No `jupyter-current-client' set"))
  (let* ((jupyter-inhibit-handlers t)
         (req (jupyter-send-execute-request jupyter-current-client
                :code str :store-history nil))
         (had-result nil))
    (jupyter-add-callback req
      :execute-reply
      (lambda (msg)
        (jupyter-with-message-content msg (status ename evalue)
          (if (equal status "ok")
              (unless had-result
                (message "jupyter: eval done"))
            (message "%s: %s"
                     (ansi-color-apply ename)
                     (ansi-color-apply evalue)))))
      :execute-result
      (or (and (functionp cb) cb)
          (lambda (msg)
            (setq had-result t)
            (jupyter--display-eval-result msg)))
      :error
      (lambda (msg)
        (jupyter-with-message-content msg (traceback)
          ;; FIXME: Assumes the error in the
          ;; execute-reply is good enough
          (when (> (apply '+ (mapcar 'length traceback)) 250)
            (jupyter-display-traceback traceback))))
      :stream
      (lambda (msg)
        (jupyter-with-message-content msg (name text)
          (when (equal name "stdout")
            (jupyter-with-display-buffer "output" req
              (jupyter-insert-ansi-coded-text text)
              (display-buffer (current-buffer)
                              '(display-buffer-below-selected)))))))
    req))

(defun jupyter-eval-region (beg end &optional cb)
  "Evaluate a region with the `jupyter-current-client'.
BEG and END are the beginning and end of the region to evaluate.
CB has the same meaning as in `jupyter-eval-string'. CB is
ignored when called interactively."
  (interactive "r")
  (jupyter-eval-string (buffer-substring-no-properties beg end) cb))

(defun jupyter-eval--insert-result (pos region msg)
  (jupyter-with-message-data msg ((res text/plain))
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
          (when region (push-mark)))))))

(defun jupyter-eval-line-or-region (insert)
  "Evaluate the current line or region with the `jupyter-current-client'.
If the current region is active send it using
`jupyter-eval-region', otherwise send the current line.

With a prefix argument, evaluate and INSERT the text/plain
representation of the results in the current buffer."
  (interactive "P")
  (let ((cb (when insert
              (apply-partially
               #'jupyter-eval--insert-result
               (point-marker) (when (use-region-p)
                                (car (region-bounds)))))))
    (if (use-region-p)
        (jupyter-eval-region (region-beginning) (region-end) cb)
      (jupyter-eval-region (line-beginning-position) (line-end-position) cb))))

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
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'defun)
    (jupyter-eval-region beg end)))

;;;;; Handlers

(cl-defgeneric jupyter-send-execute-request ((client jupyter-kernel-client)
                                             &key code
                                             (silent nil)
                                             (store-history t)
                                             (user-expressions nil)
                                             (allow-stdin
                                              (jupyter-channel-alive-p client :stdin))
                                             (stop-on-error nil))
  "Send an execute request."
  (declare (indent 1))
  (let ((msg (jupyter-message-execute-request
              :code code
              :silent silent
              :store-history store-history
              :user-expressions user-expressions
              :allow-stdin allow-stdin
              :stop-on-error stop-on-error)))
    (jupyter-send client :shell :execute-request msg)))

(cl-defgeneric jupyter-handle-execute-reply ((_client jupyter-kernel-client)
                                             _req
                                             _status
                                             _execution-count
                                             _user-expressions
                                             _payload)
  "Default execute reply handler."
  (declare (indent 1))
  nil)

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

(defun jupyter-inspect (code &optional pos buffer detail)
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
                 :code code :pos pos :detail detail))))
    (if msg
        (jupyter-with-message-content msg
            (status found)
          (if (and (equal status "ok") (eq found t))
              (let ((inhibit-read-only t))
                (if (buffer-live-p buffer)
                    (with-current-buffer buffer
                      ;; Insert MSG here so that `jupyter-insert' has access to
                      ;; the message type. This is needed since the python
                      ;; kernel and others may use this information.
                      (jupyter-insert msg)
                      (current-buffer))
                  (with-help-window (help-buffer)
                    (with-current-buffer standard-output
                      (setq other-window-scroll-buffer (current-buffer))
                      (setq jupyter-current-client client)
                      (help-setup-xref
                       (list
                        ;; We find the client based on session so that we don't
                        ;; capture a reference to the client.
                        (let ((session (jupyter-session-id (oref client session))))
                          (lambda ()
                            (let ((jupyter-current-client
                                   (jupyter-find-client-for-session session)))
                              (if jupyter-current-client
                                  (jupyter-inspect code pos nil detail)
                                (error "Client for session has been removed"))))))
                       nil)
                      (jupyter-insert msg)))))
            (message "Nothing found for %s"
                     (with-temp-buffer
                       (insert code)
                       (goto-char pos)
                       (symbol-at-point)))))
      (message "Inspect timed out"))))

(cl-defgeneric jupyter-send-inspect-request ((client jupyter-kernel-client)
                                             &key code
                                             (pos 0)
                                             (detail 0))
  "Send an inspect request."
  (declare (indent 1))
  (let ((msg (jupyter-message-inspect-request
              :code code :pos pos :detail detail)))
    (jupyter-send client :shell :inspect-request msg)))

(cl-defgeneric jupyter-handle-inspect-reply ((_client jupyter-kernel-client)
                                             _req
                                             _found
                                             _data
                                             _metadata)
  "Default inspect reply handler."
  (declare (indent 1))
  nil)

;;;; Completion

;;;;; Code context

(cl-defgeneric jupyter-code-context (type)
  "Return a list, (CODE POS), for the context around `point'.
CODE is the required context for TYPE (either `inspect' or
`completion') and POS is the relative position of `point' within
CODE. Depending on the current context such as the current
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
extract the context. It is an error if `point' is not within
these bounds. See `jupyter-code-context' for the form of the
returned list."
  (unless (<= beg (point) end)
    (error "Point not within bounds (%d %d)" beg end))
  (let ((code (buffer-substring-no-properties beg end))
        (pos (1+ (- (point) beg))))
    (list code (min pos (length code)))))

(defun jupyter-line-or-region-context ()
  "Return the code context of the region or line.
If the region is active, return it. Otherwise return the line."
  (if (region-active-p)
      (jupyter-region-context (region-beginning) (region-end))
    (jupyter-line-context)))

(cl-defmethod jupyter-code-context ((_type (eql inspect)))
  (jupyter-line-or-region-context))

(cl-defmethod jupyter-code-context ((_type (eql completion)))
  (jupyter-region-context (line-beginning-position) (point)))

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
Return either a STRING or a (STRING . t) pair. If RE matches the
beginning of the current symbol before point, return the latter.
Otherwise return the symbol before point. If no completion can be
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
with RE and MAX-LEN as arguments, RE defaulting to \"\\\\.\". It
also handles argument lists surrounded by parentheses specially
by considering an open parentheses and the symbol before it as a
completion prefix since some kernels will complete argument lists
if given such a prefix.

Note that the prefix returned is not the content sent to the
kernel, but the prefix used by `jupyter-completion-at-point'. See
`jupyter-code-context' for what is actually sent to the kernel."
  (or re (setq re "\\."))
  (cond
   ;; Completing argument lists
   ((and (char-before)
         (eq (char-syntax (char-before)) ?\()
         (or (not (char-after))
             (looking-at-p "\\_>")
             (not (memq (char-syntax (char-after)) '(?w ?_)))))
    (buffer-substring-no-properties
     (jupyter-completion-symbol-beginning (1- (point)))
     (point)))
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

;; TODO: Move functionality into a default `jupyter-handle-complete-reply'
;; method rather than callbacks?
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
         0.3 nil
         (lambda (buf win tick pos)
           (let ((company-minimum-prefix-length 0))
             (company-idle-begin buf win tick pos)))
         (current-buffer) (selected-window)
         (buffer-chars-modified-tick) (point))))

(defun jupyter-completion-at-point ()
  "Function to add to `completion-at-point-functions'."
  (let ((prefix (jupyter-completion-prefix)) req)
    (when (and prefix jupyter-current-client)
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
          (when (and req (not (jupyter-request-idle-received-p req))
                     (not (eq (jupyter-message-type
                               (jupyter-request-last-message req))
                              :complete-reply)))
            (jupyter-wait-until-received :complete-reply req))
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
    (jupyter-inspect arg nil buf)
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

(cl-defgeneric jupyter-send-complete-request ((client jupyter-kernel-client)
                                              &key code
                                              (pos 0))
  "Send a complete request."
  (declare (indent 1))
  (let ((msg (jupyter-message-complete-request
              :code code :pos pos)))
    (jupyter-send client :shell :complete-request msg)))

(cl-defgeneric jupyter-handle-complete-reply ((_client jupyter-kernel-client)
                                              _req
                                              _matches
                                              _cursor-start
                                              _cursor-end
                                              _metadata)
  "Default complete reply handler."
  (declare (indent 1))
  nil)

;;;; History

(cl-defgeneric jupyter-send-history-request ((client jupyter-kernel-client)
                                             &key
                                             output
                                             raw
                                             (hist-access-type "tail")
                                             session
                                             start
                                             stop
                                             (n 10)
                                             pattern
                                             unique)
  "Send a history request."
  (declare (indent 1))
  (let ((msg (jupyter-message-history-request
              :output output
              :raw raw
              :hist-access-type hist-access-type
              :session session
              :start start
              :stop stop
              :n n
              :pattern pattern
              :unique unique)))
    (jupyter-send client :shell :history-request msg)))

(cl-defgeneric jupyter-handle-history-reply ((_client jupyter-kernel-client)
                                             _req
                                             _history)
  "Default history reply handler."
  (declare (indent 1))
  nil)

;;;; Is Complete

(cl-defgeneric jupyter-send-is-complete-request ((client jupyter-kernel-client)
                                                 &key code)
  "Send an is-complete request."
  (declare (indent 1))
  (let ((msg (jupyter-message-is-complete-request
              :code code)))
    (jupyter-send client :shell :is-complete-request msg)))

(cl-defgeneric jupyter-handle-is-complete-reply ((_client jupyter-kernel-client)
                                                 _req
                                                 _status
                                                 _indent)
  "Default is complete reply handler."
  (declare (indent 1))
  nil)

;;;; Comms

(cl-defgeneric jupyter-send-comm-info-request ((client jupyter-kernel-client)
                                               &key target-name)
  "Send a comm-info request."
  (declare (indent 1))
  (let ((msg (jupyter-message-comm-info-request
              :target-name target-name)))
    (jupyter-send client :shell :comm-info-request msg)))

(cl-defgeneric jupyter-send-comm-open ((client jupyter-kernel-client)
                                       &key id
                                       target-name
                                       data)
  (declare (indent 1))
  (let ((msg (jupyter-message-comm-open
              :id id
              :target-name target-name
              :data data)))
    (jupyter-send client :shell :comm-open msg)))

(cl-defgeneric jupyter-send-comm-msg ((client jupyter-kernel-client)
                                      &key id
                                      data)
  (declare (indent 1))
  (let ((msg (jupyter-message-comm-msg
              :id id
              :data data)))
    (jupyter-send client :shell :comm-msg msg)))

(cl-defgeneric jupyter-send-comm-close ((client jupyter-kernel-client)
                                        &key id
                                        data)
  (declare (indent 1))
  (let ((msg (jupyter-message-comm-close
              :id id
              :data data)))
    (jupyter-send client :shell :comm-close msg)))

(cl-defgeneric jupyter-handle-comm-info-reply ((_client jupyter-kernel-client)
                                               _req
                                               _comms)
  "Default comm info. reply handler."
  (declare (indent 1))
  nil)

;;;; Kernel info

(cl-defmethod jupyter-kernel-info ((client jupyter-kernel-client))
  "Return the kernel info plist of CLIENT.
Return CLIENT's kernel-info slot if non-nil. Otherwise send a
`:kernel-info-request' to CLIENT's kernel, set CLIENT's
kernel-info slot to the plist retrieved from the kernel, and
return it.

If the kernel CLIENT is connected to does not respond to a
`:kernel-info-request', raise an error."
  (or (oref client kernel-info)
      (let* ((jupyter-inhibit-handlers t)
             (req (jupyter-send-kernel-info-request client))
             (msg (jupyter-wait-until-received :kernel-info-reply
                    req jupyter-long-timeout "Requesting kernel info...")))
        (unless msg
          (error "Kernel did not respond to kernel-info request"))
        (oset client kernel-info (jupyter-message-content msg)))))

(cl-defmethod jupyter-kernel-language ((client jupyter-kernel-client))
  "Return the language of the kernel CLIENT is connected to."
  (plist-get (plist-get (jupyter-kernel-info client) :language_info) :name))

(defun jupyter-load-language-support (client)
  "Load language support definitions for CLIENT.
CLIENT is a kernel client."
  (cl-assert (object-of-class-p client 'jupyter-kernel-client))
  (let* ((lang (jupyter-kernel-language client))
         (support (intern (concat "jupyter-" lang))))
    (require support nil t)))

(cl-defgeneric jupyter-send-kernel-info-request ((client jupyter-kernel-client))
  "Send a kernel-info request."
  (let ((msg (jupyter-message-kernel-info-request)))
    (jupyter-send client :shell :kernel-info-request msg)))

(cl-defgeneric jupyter-handle-kernel-info-reply ((_client jupyter-kernel-client)
                                                 _req
                                                 _protocol-version
                                                 _implementation
                                                 _implementation-version
                                                 _language-info
                                                 _banner
                                                 _help-links)
  "Default kernel-info reply handler."
  (declare (indent 1))
  nil)

;;;; Shutdown

(cl-defgeneric jupyter-send-shutdown-request ((client jupyter-kernel-client)
                                              &key restart)
  "Request a shutdown of CLIENT's kernel.
If RESTART is non-nil, request a restart instead of a complete shutdown."
  (declare (indent 1))
  (let ((msg (jupyter-message-shutdown-request :restart restart)))
    (jupyter-send client :shell :shutdown-request msg)))

(cl-defgeneric jupyter-handle-shutdown-reply ((_client jupyter-kernel-client)
                                              _req
                                              _restart)
  "Default shutdown reply handler."
  (declare (indent 1))
  nil)

;;; IOPUB handlers

(cl-defmethod jupyter-handle-message ((_channel (eql :iopub))
                                      client
                                      req
                                      msg)
  (unless (jupyter-run-hook-with-args-until-success
           client 'jupyter-iopub-message-hook msg)
    (jupyter-dispatch-message-cases client req msg
      ((shutdown-reply restart)
       (stream name text)
       (comm-open comm_id target_name target_module data)
       (comm-msg comm_id data)
       (comm-close comm_id data)
       (execute-input code execution_count)
       (execute-result execution_count data metadata)
       (error ename evalue traceback)
       (status execution_state)
       (clear-output wait)
       (display-data data metadata transient)
       (update-display-data data metadata transient)))))

(cl-defgeneric jupyter-handle-comm-open ((client jupyter-kernel-client)
                                         req
                                         id
                                         _target-name
                                         _target-module
                                         data)
  (declare (indent 1))
  (let ((comms (oref client comms)))
    (puthash id (cons (jupyter-request-id req) data) comms)))

(cl-defgeneric jupyter-handle-comm-msg ((_client jupyter-kernel-client)
                                        _req
                                        _id
                                        _data)
  (declare (indent 1))
  nil)

(cl-defgeneric jupyter-handle-comm-close ((client jupyter-kernel-client)
                                          _req
                                          id
                                          _data)
  (declare (indent 1))
  (let ((comms (oref client comms)))
    (remhash id comms)))

(cl-defgeneric jupyter-handle-stream ((_client jupyter-kernel-client)
                                      _req
                                      _name
                                      _text)
  "Default stream handler."
  (declare (indent 1))
  nil)

(cl-defgeneric jupyter-handle-execute-input ((_client jupyter-kernel-client)
                                             _req
                                             _code
                                             _execution-count)
  "Default execute input handler."
  (declare (indent 1))
  nil)

(cl-defgeneric jupyter-handle-execute-result ((_client jupyter-kernel-client)
                                              _req
                                              _execution-count
                                              _data
                                              _metadata)
  "Default execute result handler."
  (declare (indent 1))
  nil)

(defun jupyter-display-traceback (traceback)
  "Display TRACEBACK in a dedicated buffer."
  (when (or (vectorp traceback) (listp traceback))
    (setq traceback (concat (mapconcat #'identity traceback "\n") "\n")))
  (jupyter-with-display-buffer "traceback" 'reset
    (jupyter-insert-ansi-coded-text traceback)
    (goto-char (point-min))
    (display-buffer (current-buffer) '(display-buffer-below-selected))))

(cl-defgeneric jupyter-handle-error ((_client jupyter-kernel-client)
                                     _req
                                     _ename
                                     _evalue
                                     _traceback)
  "Default error handler."
  (declare (indent 1))
  nil)

(defun jupyter-execution-state (client)
  "Return the execution state of CLIENT's kernel."
  (cl-check-type client jupyter-kernel-client)
  (oref client execution-state))

(cl-defgeneric jupyter-handle-status ((_client jupyter-kernel-client)
                                      _req
                                      _execution-state)
  "Default status handler."
  (declare (indent 1))
  nil)

(cl-defgeneric jupyter-handle-clear-output ((_client jupyter-kernel-client)
                                            _req
                                            _wait)
  "Default clear output handler."
  (declare (indent 1))
  nil)

(cl-defgeneric jupyter-handle-display-data ((_client jupyter-kernel-client)
                                            _req
                                            _data
                                            _metadata
                                            _transient)
  "Default display data handler."
  (declare (indent 1))
  nil)

(cl-defgeneric jupyter-handle-display-data ((_client jupyter-kernel-client)
                                            _req
                                            _data
                                            _metadata
                                            _transient)
  "Default display data handler."
  (declare (indent 1))
  nil)

(cl-defgeneric jupyter-handle-update-display-data ((_client jupyter-kernel-client)
                                                   _req
                                                   _data
                                                   _metadata
                                                   _transient)
  "Default update display handler"
  (declare (indent 1))
  nil)

(provide 'jupyter-client)

;;; jupyter-client.el ends here
