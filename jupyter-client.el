;;; jupyter-client.el --- A Jupyter kernel client -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 06 Jan 2018
;; Version: 0.0.1

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
(require 'jupyter-messages)

(declare-function hash-table-values "subr-x" (hash-table))

(defvar jupyter--clients nil
  "A list of all live clients.
Clients are removed from this list when their `jupyter-finalizer' is called.")

;; This is mainly used by the REPL code, but is also set by
;; the `org-mode' client whenever `point' is inside a code
;; block.
(defvar jupyter-current-client nil
  "The `jupyter-kernel-client' for the `current-buffer'.")

(put 'jupyter-current-client 'permanent-local t)
(make-variable-buffer-local 'jupyter-current-client)

(defvar jupyter-default-timeout 1
  "The default timeout in seconds for `jupyter-wait-until'.")

(defvar jupyter-long-timeout 10
  "A longer timeout that `jupyter-default-timeout' used for some operations.
A longer timeout is needed, for example, when retrieving the
`jupyter-kernel-info' to allow for the kernel to startup.")

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

;; Define channel classes for method dispatching based on the channel type

(defclass jupyter-kernel-client (eieio-instance-tracker)
  ((tracking-symbol :initform 'jupyter--clients)
   (pending-requests
    :type ring
    :initform (make-ring 10)
    :documentation "A ring of pending `jupyter-request's.
A request is pending if it has not been sent to the kernel via the
client's ioloop slot.")
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
  (push client jupyter--clients)
  (oset client -buffer (generate-new-buffer " *jupyter-kernel-client*")))

(cl-defmethod jupyter-finalize ((client jupyter-kernel-client))
  "Close CLIENT's channels and cleanup internal resources."
  (jupyter-stop-channels client)
  (delete-instance client)
  (when (buffer-live-p (oref client -buffer))
    ;; Don't ask if the buffer should be killed, this is needed because of the
    ;; lock file mechanism for channel subprocesses.
    (with-current-buffer (oref client -buffer)
      (set-buffer-modified-p nil))
    (kill-buffer (oref client -buffer))))

(defun jupyter-kill-kernel-clients ()
  "Call the finalizer for all live Jupyter clients."
  (dolist (client jupyter--clients)
    (jupyter-finalize client)))

(add-hook 'kill-emacs-hook 'jupyter-kill-kernel-clients)

(defun jupyter-clients ()
  "Return a list of all `jupyter-kernel-clients'."
  jupyter--clients)

(defun jupyter-find-client-for-session (session-id)
  "Return the `jupyter-kernel-client' for SESSION-ID."
  (or (catch 'found
        (dolist (client jupyter--clients)
          (when (string= (jupyter-session-id (oref client session)) session-id)
            (throw 'found client))))
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
      (jupyter-stop-channels client)
      ;; Initialize the channels
      (unless session
        (setq session (jupyter-session :key key :conn-info conn-info)))
      (oset client session session)
      (let ((addr (lambda (port) (format "%s://%s:%d" transport ip port))))
        (setf (oref client channels)
              ;; Construct the channel plist
              (cl-loop
               collect :hb
               and collect (make-instance
                            'jupyter-hb-channel
                            :session session
                            :endpoint (funcall addr hb_port))
               for (channel . port) in `((:stdin . ,stdin_port)
                                         (:shell . ,shell_port)
                                         (:iopub . ,iopub_port))
               collect channel
               and collect
               ;; The session will be associated with these channels in the
               ;; ioloop subprocess.
               (list :endpoint (funcall addr port)
                     :alive-p nil)))))))

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
  "Run CLIENT's value for HOOK with the arguments ARGS."
  (jupyter-with-client-buffer client
    (when jupyter--debug
      (message "RUN-HOOK: %s" hook))
    (apply #'run-hook-with-args-until-success hook args)))

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
  (make-jupyter-request))

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

(cl-defmethod jupyter-ioloop-handler ((_ioloop jupyter-ioloop)
                                      (client jupyter-kernel-client)
                                      (event (head message)))
  "For CLIENT, queue a message EVENT to be handled."
  (cl-destructuring-bind (_ channel _idents . msg) event
    ;; Run immediately after handling this event, i.e. on the next command loop
    (run-at-time 0 nil #'jupyter-handle-message client channel msg)))

(cl-defmethod jupyter-channel-alive-p ((client jupyter-kernel-client) channel)
  (with-slots (channels) client
    ;; The hb channel is implemented locally in the current process whereas the
    ;; other channels are implemented in subprocesses and the current process
    ;; acts as a proxy.
    (if (eq channel :hb)
        (and (plist-get channels :hb)
             (jupyter-channel-alive-p (plist-get channels :hb)))
      (plist-get (plist-get channels channel) :alive-p))))

(cl-defmethod jupyter-ioloop-handler ((_ioloop jupyter-ioloop)
                                      (client jupyter-kernel-client)
                                      (event (head start-channel)))
  (with-slots (channels) client
    (plist-put (plist-get channels (cadr event)) :alive-p t)))

(cl-defmethod jupyter-start-channel ((client jupyter-kernel-client) channel)
  (with-slots (ioloop channels) client
    (if (eq channel :hb) (jupyter-start-channel (plist-get channels :hb))
      (let ((endpoint (plist-get (plist-get channels channel) :endpoint)))
        (unless (jupyter-channel-alive-p client channel)
          (jupyter-send ioloop 'start-channel channel endpoint)
          (with-timeout (0.5 (error "Channel not started in ioloop subprocess"))
            (while (not (jupyter-channel-alive-p client channel))
              (accept-process-output (jupyter-ioloop-process ioloop) 0.1 nil 0))))))))

(cl-defmethod jupyter-ioloop-handler ((_ioloop jupyter-ioloop)
                                      (client jupyter-kernel-client)
                                      (event (head stop-channel)))
  (with-slots (channels) client
    (plist-put (plist-get channels (cadr event)) :alive-p nil)))

(cl-defmethod jupyter-stop-channel ((client jupyter-kernel-client) channel)
  (with-slots (ioloop channels) client
    (when (jupyter-channel-alive-p client channel)
      (if (eq channel :hb) (jupyter-stop-channel (plist-get channels :hb))
        (jupyter-send ioloop 'stop-channel channel)
        (with-timeout (0.5 (warn "Channel not stopped in ioloop subprocess"))
          (while (jupyter-channel-alive-p client channel)
            (accept-process-output (jupyter-ioloop-process ioloop) 0.1 nil 0)))))))

;;; Starting/stopping channels

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
  (with-slots (ioloop session -buffer) client
    (jupyter-stop-channels client)
    (unless ioloop
      (oset client ioloop (jupyter-channel-ioloop))
      (setq ioloop (oref client ioloop)))
    (jupyter-ioloop-start ioloop session client :buffer -buffer)
    (cl-loop
     for (channel . start) in `((:hb . ,hb)
                                (:shell . ,shell)
                                (:iopub . ,iopub)
                                (:stdin . ,stdin))
     when start do (jupyter-start-channel client channel))))

(cl-defmethod jupyter-stop-channels ((client jupyter-kernel-client))
  "Stop any running channels of CLIENT."
  (cl-loop
   for channel in '(:shell :iopub :stdin :hb)
   do (jupyter-stop-channel client channel))
  (with-slots (ioloop) client
    (when (jupyter-ioloop-p ioloop)
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
  (when req
    (let* ((callbacks (jupyter-request-callbacks req))
           (cb-all-types (cdr (assoc t callbacks)))
           (cb-for-type (cdr (assoc (jupyter-message-type msg) callbacks))))
      (and cb-all-types (funcall cb-all-types msg))
      (and cb-for-type (funcall cb-for-type msg)))))

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
  (let ((callbacks (jupyter-request-callbacks req)))
    (if (null callbacks)
        (setf (jupyter-request-callbacks req)
              (list (cons msg-type cb)))
      (let ((cb-for-type (assoc msg-type callbacks)))
        (if (not cb-for-type)
            (nconc callbacks (list (cons msg-type cb)))
          (setcdr cb-for-type
                  (let ((ccb (cdr cb-for-type)))
                    (lambda (msg)
                      (funcall ccb msg)
                      (funcall cb msg)))))))))

(defun jupyter-add-callback (req msg-type cb &rest callbacks)
  "Add a callback to run when a message is received for a request.
REQ is a `jupyter-request' returned by one of the request methods
of a `jupyter-kernel-client'. MSG-TYPE is one of the keys in
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
    (setq callbacks (append (list msg-type cb) callbacks))
    (cl-loop for (msg-type cb) on callbacks by 'cddr
             if (listp msg-type)
             do (mapc (lambda (mt) (jupyter--add-callback req mt cb)) msg-type)
             else do (jupyter--add-callback req msg-type cb))))

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
  (setq timeout (or timeout jupyter-default-timeout))
  (cl-check-type timeout number)
  (let ((progress (and (stringp progress-msg)
                       (make-progress-reporter progress-msg)))
        msg)
    (jupyter-add-callback req
      msg-type (lambda (m) (setq msg (when (funcall cb m) m))))
    (with-timeout (timeout nil)
      (while (null msg)
        (sleep-for 0.01)
        (when progress
          (progress-reporter-update progress))))
    (prog1 msg
      (when progress
        (progress-reporter-done progress)))))

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

(defun jupyter--drop-idle-requests (client)
  "Drop completed requests from CLIENT's request table.
A request is deemed complete when an idle message has been
received for it and it is not the most recently sent request."
  (cl-loop
   with requests = (oref client requests)
   with last-sent = (gethash "last-sent" requests)
   for req in (hash-table-values requests)
   when (and (jupyter-request-idle-received-p req)
             (not (eq req last-sent)))
   do (when jupyter--debug
        (message "DROPPING-REQ: %s" (jupyter-request-id req)))
   (remhash (jupyter-request-id req) requests)
   (jupyter-drop-request client req)))

(defun jupyter--run-handler-maybe (client channel req msg)
  "Possibly run CLIENT's CHANNEL handler on REQ's received MSG."
  (let ((inhibited-handlers (and req (jupyter-request-inhibited-handlers req))))
    (unless (or (eq inhibited-handlers t)
                (let ((type (memq (jupyter-message-type msg) inhibited-handlers)))
                  (if (eq (car inhibited-handlers) 'not) (not type)
                    type)))
      (jupyter-handle-message channel client req msg))))

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
    (let* ((pmsg-id (jupyter-message-parent-id msg))
           (requests (oref client requests))
           (req (gethash pmsg-id requests)))
      (if (not req)
          (when (jupyter-get client 'jupyter-include-other-output)
            (jupyter--run-handler-maybe client channel req msg))
        (setf (jupyter-request-last-message req) msg)
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

(defun jupyter-eval (code &optional mime)
  "Send an execute request for CODE, wait for the execute result.
The `jupyter-current-client' is used to send the execute request.
All client handlers except the status handler are inhibited for
the request. In addition, the history of the request is not
stored. Return the MIME representation of the result. If MIME is
nil, return the text/plain representation."
  (let ((msg (jupyter-wait-until-received :execute-result
               (let ((jupyter-inhibit-handlers '(not :status)))
                 (jupyter-send-execute-request jupyter-current-client
                   :code code :store-history nil)))))
    (when msg
      (jupyter-message-data msg (or mime :text/plain)))))

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

(cl-defgeneric jupyter-code-context (type)
  "Return a list, (CODE POS), for the context around `point'.
CODE is the required context for TYPE (either `inspect' or
`completion') and POS is the relative position of `point' within
CODE. Depending on the current context such as the current
`major-mode', CODE and POS will be used for `:complete-request's
originating from `jupyter-completion-at-point' and
`:inspect-request's from `jupyter-inspect-at-point'.

The default methods return the `jupyter-line-or-region-context'.")

(defun jupyter-line-context (&optional start)
  "Return the code context of the current line.
START is the buffer position considered as the start of the line
and defaults to the `line-beginning-position'. See
`jupyter-code-context' for the form of the returned list."
  (or start (setq start (line-beginning-position)))
  (let ((code (buffer-substring-no-properties start (line-end-position)))
        (pos (1+ (- (point) start))))
    (list code (min pos (length code)))))

(defun jupyter-line-or-region-context (&optional start)
  "Return the code context of the region or line.
If the region is active, return the active region context.
Otherwise return the line context. START has the same meaning as
in `jupyter-line-context' and is ignored if the region is active."
  (if (region-active-p)
      (list (buffer-substring-no-properties (region-beginning) (region-end))
            (min (- (region-end) (region-beginning))
                 (1+ (- (point) (region-beginning)))))
    (jupyter-line-context start)))

(cl-defmethod jupyter-code-context ((_type (eql inspect)))
  (jupyter-line-or-region-context))

(cl-defmethod jupyter-code-context ((_type (eql completion)))
  (jupyter-line-or-region-context))

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
      (let ((jupyter-inhibit-handlers t))
        (prog1 (oset client kernel-info
                     (jupyter-message-content
                      (jupyter-wait-until-received :kernel-info-reply
                        (jupyter-send-kernel-info-request client)
                        jupyter-long-timeout
                        "Requesting kernel info...")))
          (unless (oref client kernel-info)
            (error "Kernel did not respond to kernel-info request"))))))

(cl-defmethod jupyter-kernel-language ((client jupyter-kernel-client))
  "Return the language of the kernel CLIENT is connected to."
  (plist-get (plist-get (jupyter-kernel-info client) :language_info) :name))

(defun jupyter-load-language-support (client)
  "Load language support definitions for CLIENT.
CLIENT is a `jupyter-kernel-client'."
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

(cl-defgeneric jupyter-handle-error ((_client jupyter-kernel-client)
                                     _req
                                     _ename
                                     _evalue
                                     _traceback)
  "Default error handler."
  (declare (indent 1))
  nil)

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
