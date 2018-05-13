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
(require 'jupyter-messages)

(declare-function hash-table-values "subr-x" (hash-table))

(defvar jupyter--debug nil
  "Set to non-nil to emit sent and received messages to *Messages*.")

(defvar jupyter-default-timeout 1
  "The default timeout in seconds for `jupyter-wait-until'.")

(defvar jupyter-inhibit-handlers nil
  "Whether or not new requests inhibit some client handlers.
Do not set this variable directly, locally bind it to either t or
a list of some of the keywords in `jupyter-message-types' to
prevent the client from calling some of the handlers. For example
to prevent a client from calling its execute-reply handler, you
would do

    (let ((jupyter-inhibit-handlers '(:execute-reply)))
      (jupyter-execute-request client ...))

If set to t, disable all client handlers.")

;; Define channel classes for method dispatching based on the channel type

(defclass jupyter-shell-channel (jupyter-async-channel)
  ((type
    :initform :shell)))

(defclass jupyter-iopub-channel (jupyter-async-channel)
  ((type
    :initform :iopub)))

(defclass jupyter-stdin-channel (jupyter-async-channel)
  ((type
    :initform :stdin)))

(defclass jupyter-kernel-client ()
  ((requests
    :type hash-table
    :initform (make-hash-table :test 'equal)
    :documentation "A hash table with message ID's as keys. This
is used to register callback functions to run once a reply from a
previously sent request is received. See `jupyter-add-callback'.
Note that this is also used to filter received messages that
originated from a previous request by this client. Whenever the
client sends a message in which a reply is expected, it sets an
entry in this table to represent the fact that the message has
been sent. So if there is a non-nil value for a message ID it
means that a message has been sent and the client is expecting a
reply from the kernel.")
   (ioloop
    :type (or null process)
    :initform nil
    :documentation "The process which polls for events on all
live channels of the client.")
   (session
    :type jupyter-session
    :documentation "The session for this client.")
   (manager
    :initform nil
    :documentation "If this client was initialized using a
`jupyter-kernel-manager' this slot will hold the manager which
initialized the client.")
   ;; TODO: Periodically cleanup these buffers when the object they point to
   ;; are no longer in use. How can we determine if an object has no more
   ;; references? Maybe do something with `post-gc-hook'? Or require explicit
   ;; cleanup?
   (-buffer
    :type buffer
    :documentation "An internal buffer used to store client local
variables and intermediate ioloop process output. When the ioloop
slot is non-nil, its `process-buffer' will be `eq' to this
buffer.")
   (shell-channel
    :type (or null jupyter-shell-channel)
    :initform nil
    :initarg :shell-channel
    :documentation "The shell channel.")
   (iopub-channel
    :type (or null jupyter-iopub-channel)
    :initform nil
    :initarg :iopub-channel
    :documentation "The IOPub channel.")
   (stdin-channel
    :type (or null jupyter-stdin-channel)
    :initform nil
    :initarg :stdin-channel
    :documentation "The stdin channel.")
   (hb-channel
    :type (or null jupyter-hb-channel)
    :initform nil
    :initarg :hb-channel
    :documentation "The heartbeat channel.")))

(cl-defmethod initialize-instance ((client jupyter-kernel-client) &rest _slots)
  (cl-call-next-method)
  (oset client -buffer (generate-new-buffer " *jupyter-kernel-client*")))

(cl-defmethod destructor ((client jupyter-kernel-client) &rest _params)
  "Close CLIENT's channels and cleanup internal resources."
  (jupyter-stop-channels client)
  (when (buffer-live-p (oref client -buffer))
    (kill-buffer (oref client -buffer))))

(defun jupyter-initialize-connection (client info-or-session)
  "Initialize CLIENT with a connection INFO-OR-SESSION.
If INFO-OR-SESSION is the name of a file, assume the file to be a
kernel connection file, read the contents as a plist, and create
a new `jupyter-session' using the plist as the conn-info slot of
the session. INFO-OR-SESSION can also be a plist with the same
meaning as if the plist had been read from a connection file.
Otherwise, INFO-OR-SESSION must be a `jupyter-session' and the
connection is initialized using its conn-info slot.

The session object is then set as the session for each
`jupyter-channel' created for the CLIENT.

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
            (unless (file-exists-p info-or-session)
              (error "File does not exist (%s)" info-or-session))
            (let ((json-array-type 'list)
                  (json-object-type 'plist)
                  (json-false nil))
              (json-read-file info-or-session)))
           (t (signal 'wrong-type-argument
                      (list info-or-session
                            '(or jupyter-session-p json-plist-p stringp)))))))
    (cl-destructuring-bind
        (&key shell_port iopub_port stdin_port hb_port ip
              key transport signature_scheme
              &allow-other-keys)
        conn-info
      (when (and (> (length key) 0)
                 (not (functionp (intern signature_scheme))))
        (error "Unsupported signature scheme: %s" signature_scheme))
      ;; Stop the channels if connected to some other kernel
      (jupyter-stop-channels client)
      ;; Initialize the channels
      (unless session
        (setq session (jupyter-session :key key :conn-info conn-info)))
      (oset client session session)
      (let ((addr (lambda (port) (format "%s://%s:%d" transport ip port))))
        (oset client hb-channel (make-instance
                                 'jupyter-hb-channel
                                 :session session
                                 :endpoint (funcall addr hb_port)))
        (cl-loop
         for (channel . port) in `((stdin-channel . ,stdin_port)
                                   (shell-channel . ,shell_port)
                                   (iopub-channel . ,iopub_port))
         do (setf (slot-value client channel)
                  (make-instance
                   (cl-case channel
                     (stdin-channel 'jupyter-stdin-channel)
                     (shell-channel 'jupyter-shell-channel)
                     (iopub-channel 'jupyter-iopub-channel)
                     (otherwise (error "Wrong channel type")))
                   ;; So channels have access to the client's session
                   ;;
                   ;; See `jupyter-start-channels' for when the :ioloop slot of
                   ;; a channel is set
                   :session session
                   :endpoint (funcall addr port))))))))

;;; Client local variables

(defmacro with-jupyter-client-buffer (client &rest body)
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
  (with-jupyter-client-buffer client
    (set (make-local-variable symbol) newval)))

(defun jupyter-get (client symbol)
  "Get CLIENT's local value of SYMBOL."
  (with-jupyter-client-buffer client
    (symbol-value symbol)))

;;; Hooks

(defun jupyter-add-hook (client hook function &optional append)
  "Add to the CLIENT value of HOOK the function FUNCTION.
APPEND has the same meaning as in `add-hook' and FUNCTION is
added to HOOK using `add-hook', but local only to CLIENT. Note
that the CLIENT should have its channels already started before
this is called."
  (with-jupyter-client-buffer client
    (add-hook hook function append t)))

(defun jupyter-run-hook-with-args (client hook &rest args)
  "Run CLIENT's value for HOOK with the arguments ARGS."
  (with-jupyter-client-buffer client
    (when jupyter--debug
      (message "RUN-HOOK: %s" hook))
    (apply #'run-hook-with-args hook args)))

(defun jupyter-remove-hook (client hook function)
  "Remove from CLIENT's value of HOOK the function FUNCTION."
  (with-jupyter-client-buffer client
    (remove-hook hook function t)))

;;; Sending messages

(cl-defmethod jupyter-send ((client jupyter-kernel-client)
                            channel
                            type
                            message)
  "Send a message on CLIENT's CHANNEL.
Return a `jupyter-request' representing the sent message. CHANNEL
is one of the channel's of CLIENT. TYPE is one of the
`jupyter-message-types'. MESSAGE is the message sent on CHANNEL.

Note that you can manipulate how to handle messages received in
response to the sent message, see `jupyter-add-callback' and
`jupyter-request-inhibited-handlers'."
  (declare (indent 1))
  (let ((ioloop (oref client ioloop)))
    (unless ioloop
      (signal 'wrong-type-argument (list 'process ioloop 'ioloop)))
    (when jupyter--debug
      (message "SENDING: %s %s" type message))
    (jupyter-send channel type message)
    ;; Anything sent to stdin is a reply not a request so don't add it to
    ;; `:pending-requests'.
    (unless (eq (oref channel type) :stdin)
      (let ((req (make-jupyter-request
                  :inhibited-handlers
                  (or (eq jupyter-inhibit-handlers t)
                      (mapcar (lambda (msg-type)
                           (or (plist-get jupyter-message-types msg-type)
                               (error "Not a valid message type (`%s')" msg-type)))
                         jupyter-inhibit-handlers)))))
        (jupyter--ioloop-push-request client req)
        req))))

;;; Channel subprocess (receiving messages)

(defmacro jupyter--ioloop-do-command (poller channels)
  "Read and execute a command from stdin.
SESSION is a variable bound to a `jupyter-session' object, POLLER
is a variable bound to a `zmq-poller' object. and CHANNELS is a
variable bound to an alist of (SOCK . CTYPE) pairs where SOCK is
a `zmq-socket' representing a `jupyter-channel' that has a
channel type of CTYPE.

The parent Emacs process should send the ioloop subprocess cons
cells of the form:

    (CMD . DATA)

where CMD is a symbol representing the command to perform and
DATA is any information needed to run the command. The available
commands that an ioloop subprocess can perform are:

- '(quit) :: Terminate the ioloop.

- '(start-channel . CTYPE) :: Re-connect the socket in CHANNELS
  that represents a `jupyter-channel' whose type is CTYPE.

- '(stop-channel . CTYPE) :: Disconnect the socket that
  corresponds to a channel with type CTYPE in CHANNELS.

- '(send CTYPE . ARGS) :: Call `jupyter-send' with arguments
  SESSION, the socket corresponding to CTYPE, and ARGS. Where
  ARGS is a list of the remaining arguments necessary for the
  `jupyter-send' call.

Any other command sent to the subprocess will be ignored."
  `(cl-destructuring-bind (cmd . args)
       (zmq-subprocess-read)
     (cl-case cmd
       (send
        (cl-destructuring-bind (ctype . args) args
          (let ((channel (cdr (assoc ctype ,channels))))
            (zmq-prin1 (list 'sent ctype (apply #'jupyter-send channel args))))))
       (start-channel
        (cl-destructuring-bind (ctype endpoint identity) args
          (let ((channel (cdr (assoc ctype ,channels))))
            (oset channel endpoint endpoint)
            (jupyter-start-channel channel :identity identity)
            (zmq-poller-add ,poller (oref channel socket) zmq-POLLIN)
            ;; Let the channel start. This avoids problems with the initial
            ;; startup message for the python kernel. Sometimes we arent fast
            ;; enough to get this message.
            (sleep-for 0.1)
            (zmq-prin1 (list 'start-channel ctype)))))
       (stop-channel
        (cl-destructuring-bind (ctype) args
          (let ((channel (cdr (assoc ctype ,channels))))
            (zmq-poller-remove ,poller (oref channel socket))
            (jupyter-stop-channel channel)
            (zmq-prin1 (list 'stop-channel ctype)))))
       (quit
        (signal 'quit nil))
       (otherwise (error "Unhandled command (%s)" cmd)))))

;; FIXME: The subprocess may not get killed if the parent emacs crashes. One
;; solution to this is to send the process id of the parent emacs and
;; periodically check if the process is still alive, then exit the subprocess
;; if the parent process is dead.
;;
;; FIXME: Fix the problem where lots of display_data messages are coming in,
;; then we send a request, and wait for the request id to come back with
;; `jupyter-request-id'. `jupyter-request-id' will time out. it looks like the
;; poller is not noticing the stdin event in this case.
(defun jupyter--ioloop (client)
  "Return the function used for communicating with CLIENT's kernel."
  (let ((sid (jupyter-session-id (oref client session)))
        (skey (jupyter-session-key (oref client session))))
    `(lambda (ctx)
       (push ,(file-name-directory (locate-library "jupyter-base")) load-path)
       (require 'jupyter-base)
       (require 'jupyter-channels)
       (require 'jupyter-messages)
       (let* ((session (jupyter-session :id ,sid :key ,skey))
              (iopub (jupyter-sync-channel :type :iopub :session session))
              (shell (jupyter-sync-channel :type :shell :session session))
              (stdin (jupyter-sync-channel :type :stdin :session session))
              (channels `((:stdin . ,stdin)
                          (:shell . ,shell)
                          (:iopub . ,iopub)))
              (idle-count 0)
              (timeout 20)
              (messages nil))
         (condition-case nil
             (let ((poller (zmq-poller)))
               ;; Poll for stdin messages
               (zmq-poller-add poller 0 zmq-POLLIN)
               (zmq-prin1 '(start))
               (while t
                 (let ((events
                        (condition-case nil
                            (zmq-poller-wait-all poller (1+ (length channels)) timeout)
                          ((zmq-EAGAIN zmq-EINTR zmq-ETIMEDOUT) nil))))
                   ;; Perform a command from stdin
                   (when (alist-get 0 events)
                     (setf (alist-get 0 events nil 'remove) nil)
                     (jupyter--ioloop-do-command poller channels))
                   ;; Queue received messages
                   (dolist (type-channel channels)
                     (cl-destructuring-bind (type . channel) type-channel
                       (when (zmq-assoc (oref channel socket) events)
                         (push (cons type (jupyter-recv channel)) messages))))
                   (if events
                       ;; When messages have been received, reset idle counter
                       ;; and shorten polling timeout
                       (setq idle-count 0 timeout 20)
                     (setq idle-count (1+ idle-count))
                     (when (= idle-count 100)
                       ;; If no messages have been received for 100 polling
                       ;; periods, lengthen timeout so as to not waste CPU
                       ;; cycles
                       (setq timeout 5000))
                     ;; Send queued messages.
                     ;;
                     ;; Pool at least some messages, but not at the cost of
                     ;; responsiveness. If messages are being blasted at us by
                     ;; the kernel ensure that they still get through and not
                     ;; pooled indefinately.
                     ;;
                     ;; TODO: Drop messages if they are comming too frequently
                     ;; to the point where the parent Emacs process would be
                     ;; spending too much time handling messages. Or better
                     ;; yet, reduce the rate at which messages are being sent
                     ;; to the parent process.
                     (when (and messages (or (= idle-count 5)
                                             (> (length messages) 10)))
                       (setq messages (nreverse messages))
                       (while messages
                         (prin1 (cons 'recvd (pop messages))))
                       (zmq-flush 'stdout))))))
           (quit
            (mapc #'jupyter-stop-channel (mapcar #'cdr channels))
            (zmq-prin1 '(quit))))))))

(defun jupyter--ioloop-pop-request (client)
  "Remove a pending request from CLIENT's ioloop subprocess.
Specifically remove the oldest element of the ring located in the
`:pending-requests' property of CLIENT's ioloop
subprocess."
  (let ((ring (process-get (oref client ioloop) :pending-requests)))
    (unless (ring-empty-p ring)
      (ring-remove ring))))

(defun jupyter--ioloop-push-request (client req)
  "Insert a request into CLIENT's pending requests.
Pending requests are `jupyter-request's that have a nil
`jupyter-request--id'. The `jupyter-send' method for a
`jupyter-kernel-client' sends message data to the ioloop
subprocess to encode and send off to the kernel. When the
subprocess sends a message to the kernel, it sends the message ID
associated with the request back to the parent Emacs process
which is when the `jupyter-request--id' field becomes non-nil.

Pending requests are stored in a ring located in the
`:pending-requests' property of an ioloop subprocess. REQ is
added as the newest element in this ring."
  (let ((ring (or (process-get (oref client ioloop) :pending-requests)
                  (let ((ring (make-ring 10)))
                    (process-put (oref client ioloop) :pending-requests ring)
                    ring))))
    (ring-insert+extend ring req 'grow)))

;;; Channel subprocess filter/sentinel

(defun jupyter--ioloop-sentinel (client ioloop _event)
  "The process sentinel for CLIENT's IOLOOP subprocess.
When EVENT is one of the events signifying that the process is
dead, stop the heartbeat channel and set the IOLOOP slot to nil
in CLIENT."
  (cond
   ((not (process-live-p ioloop))
    (jupyter-stop-channel (oref client hb-channel))
    (oset client ioloop nil))))

(defun jupyter--get-channel (client ctype)
  "Get CLIENT's channel based on CTYPE."
  (cl-find-if
   (lambda (channel) (eq (oref channel type) ctype))
   (mapcar (lambda (sym) (slot-value client sym))
      '(hb-channel
        stdin-channel
        shell-channel
        iopub-channel))))

(defun jupyter--ioloop-filter (client event)
  "The process filter for CLIENT's ioloop subprocess.
EVENT will be an s-expression emitted from the function returned
by `jupyter--ioloop'."
  (pcase event
    (`(sent ,ctype ,msg-id)
     (when jupyter--debug
       (message "SENT: %s" msg-id))
     (unless (eq ctype :stdin)
       ;; Anything sent on stdin is a reply and therefore never added to
       ;; `:pending-requests'
       (let ((req (jupyter--ioloop-pop-request client)))
         (setf (jupyter-request--id req) msg-id)
         (puthash msg-id req (oref client requests)))))
    (`(recvd ,ctype ,idents . ,msg)
     (when jupyter--debug
       (message "RECV: %s %s %s %s"
                idents
                (jupyter-message-type msg)
                (jupyter-message-parent-id msg)
                (jupyter-message-content msg)))
     (let ((channel (jupyter--get-channel client ctype)))
       (if (not channel) (warn "No handler for channel type (%s)" ctype)
         (jupyter-queue-message channel (cons idents msg))
         (run-with-timer 0.0001 nil #'jupyter-handle-message client channel))))
    (`(start-channel ,ctype)
     (let ((channel (jupyter--get-channel client ctype)))
       (oset channel status 'running)))
    (`(stop-channel ,ctype)
     (let ((channel (jupyter--get-channel client ctype)))
       (oset channel status 'stopped)))
    ('(start)
     ;; TODO: Generalize setting flag variables for IOLoop events and having
     ;; event callbacks.
     (process-put (oref client ioloop) :start t))
    ('(quit)
     ;; Cleanup handled in sentinel
     (when jupyter--debug
       (message "CLIENT CLOSED")))))

;;; Starting the channel subprocess

(defun jupyter-ioloop-wait-until (event ioloop &optional timeout)
  "Wait until EVENT occurs in IOLOOP.
Currently EVENT can be :start or :quit and this function will
blocks for TIMEOUT seconds until IOLOOP starts or quits depending
on EVENT. If TIMEOUT is nil it defaults to 1 s."
  (or timeout (setq timeout 1))
  (with-timeout (timeout nil)
    (while (null (process-get ioloop event))
      (sleep-for 0.01))
    t))

(defun jupyter--start-ioloop (client)
  "Start CLIENT's channels."
  (unless (oref client ioloop)
    (oset client ioloop
          (zmq-start-process
           (jupyter--ioloop client)
           (apply-partially #'jupyter--ioloop-filter client)
           (apply-partially #'jupyter--ioloop-sentinel client)
           (oref client -buffer)))
    (jupyter-ioloop-wait-until :start (oref client ioloop))))

(cl-defmethod jupyter-start-channels ((client jupyter-kernel-client)
                                      &key (shell t)
                                      (iopub t)
                                      (stdin t)
                                      (hb t))
  "Start the pre-configured channels of CLIENT.
This function calls `jupyter-start-channel' for every channel
that has a non-nil value passed to this function. All channels
are started by default, so to prevent a channel from starting you
would have to pass a nil value for the channel's key. As an
example, to prevent the control channel from starting you would
call this function like so

    (jupyter-start-channels client :stdin nil)

In addition to calling `jupyter-start-channel', a subprocess is
created for each channel which monitors the channel's socket for
input events. Note that this polling subprocess is not created
for the heartbeat channel."
  (jupyter--start-ioloop client)
  (when hb
    (jupyter-start-channel (oref client hb-channel)))
  (cl-loop
   for (sym . start) in `((shell-channel . ,shell)
                          (iopub-channel . ,iopub)
                          (stdin-channel . ,stdin))
   for channel = (slot-value client sym)
   do (oset channel ioloop (oref client ioloop))
   and if start do (jupyter-start-channel channel)))

(cl-defmethod jupyter-stop-channels ((client jupyter-kernel-client))
  "Stop any running channels of CLIENT."
  (cl-loop
   for sym in '(hb-channel shell-channel iopub-channel stdin-channel)
   for channel = (slot-value client sym)
   when channel do (jupyter-stop-channel channel))
  (let ((ioloop (oref client ioloop)))
    (when ioloop
      (zmq-subprocess-send ioloop (cons 'quit nil))
      (with-timeout (1 (delete-process ioloop)
                       (warn "IOloop process not killed by request"))
        (while (oref client ioloop)
          (sleep-for 0 100))))))

(cl-defmethod jupyter-channels-running-p ((client jupyter-kernel-client))
  "Are any channels of CLIENT running?"
  (cl-loop
   for sym in '(hb-channel shell-channel iopub-channel stdin-channel)
   for channel = (slot-value client sym)
   thereis (jupyter-channel-alive-p channel)))

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
  (setq msg-type (or (plist-get jupyter-message-types msg-type)
                     ;; A msg-type of t means that FUNCTION is run for all
                     ;; messages associated with a request.
                     (eq msg-type t)))
  (unless msg-type
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
        (jupyter-execute-request client :code \"1 + 2\")
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

(defun jupyter-wait-until (req msg-type cb &optional timeout)
  "Wait until conditions for a request are satisfied.
REQ, MSG-TYPE, and CB have the same meaning as in
`jupyter-add-callback'. If CB returns non-nil within TIMEOUT
seconds, return the message that caused CB to return non-nil. If
CB never returns a non-nil value within TIMEOUT, return nil. Note
that if no TIMEOUT is given, `jupyter-default-timeout' is used."
  (declare (indent 2))
  (setq timeout (or timeout jupyter-default-timeout))
  (cl-check-type timeout number)
  (let (msg)
    (jupyter-add-callback req
      msg-type (lambda (m) (setq msg (when (funcall cb m) m))))
    (with-timeout (timeout nil)
      (while (null msg)
        (sleep-for 0.01))
      msg)))

(defun jupyter-wait-until-idle (req &optional timeout)
  "Wait until a status: idle message is received for a request.
REQ has the same meaning as in `jupyter-add-callback'. If an idle
message for REQ is received within TIMEOUT seconds, return the
message. Otherwise return nil if the message was not received
within TIMEOUT. Note that if no TIMEOUT is given, it defaults to
`jupyter-default-timeout'."
  (jupyter-wait-until req :status #'jupyter-message-status-idle-p timeout))

(defun jupyter-wait-until-received (msg-type req &optional timeout)
  "Wait until a message of a certain type is received for a request.
MSG-TYPE and REQ have the same meaning as their corresponding
arguments in `jupyter-add-callback'. If no message that matches
MSG-TYPE is received for REQ within TIMEOUT seconds, return nil.
Otherwise return the first message that matched MSG-TYPE. Note
that if no TIMEOUT is given, it defaults to
`jupyter-default-timeout'."
  (declare (indent 1))
  (jupyter-wait-until req msg-type #'identity timeout))

;;; Client handlers

(defun jupyter--drop-idle-requests (client)
  "Drop completed requests from CLIENT's request table.
A request is deemed complete when an idle message has been
received for it."
  (cl-loop
   with requests = (oref client requests)
   with ctime = (current-time)
   ;; Drop idle requests when the last received message time is longer than 1 s
   ;; ago and after the idle message has been received. Sometimes reply
   ;; messages are received after an idle message.
   with secs = '(0 1)
   for req in (hash-table-values requests)
   for id = (jupyter-request-id req)
   for ltime = (jupyter-request-last-message-time req)
   when (and (jupyter-request-idle-received-p req)
             (time-less-p secs (time-subtract ctime ltime)))
   do (when jupyter--debug
        (message "DROPPING-REQ: %s" id))
   (remhash id requests)))

(defun jupyter--run-handler-maybe (client channel req msg)
  (let ((inhibited-handlers (and req (jupyter-request-inhibited-handlers req))))
    (unless (or (eq inhibited-handlers t)
                (member (jupyter-message-type msg) inhibited-handlers))
      (jupyter-handle-message channel client req msg))))

(cl-defmethod jupyter-handle-message ((client jupyter-kernel-client) channel)
  "Process a message on CLIENT's CHANNEL.
When a message is received from the kernel, the
`jupyter-handle-message' method is called on the client. The
client method runs any callbacks for the message and possibly
runs the client handler for the channel the message was received
on. The channel's `jupyter-handle-message' method will then pass
the message to the appropriate message handler based on message
type which terminates the execution path.

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
  (let ((msg (jupyter-get-message channel)))
    (when msg
      (let* ((pmsg-id (jupyter-message-parent-id msg))
             (requests (oref client requests))
             (req (gethash pmsg-id requests)))
        (if (not req)
            (when (jupyter-get client 'jupyter-include-other-output)
              (jupyter--run-handler-maybe client channel req msg))
          (unwind-protect
              (jupyter--run-callbacks req msg)
            (unwind-protect
                (jupyter--run-handler-maybe client channel req msg)
              (setf (jupyter-request-last-message-time req) (current-time))
              (when (jupyter-message-status-idle-p msg)
                (setf (jupyter-request-idle-received-p req) t))
              (jupyter--drop-idle-requests client))))))))

;;; Channel handler macros

(defmacro jupyter-dispatch-message-cases (client req msg cases)
  "Dispatch to CLIENT handler's based on REQ and MSG.
CASES defines the the handlers to dispatch to based on the
`jupyter-message-type' of MSG and should be a list of lists, the
first element of each inner list being the name of the handler,
excluding the `jupyter-handle-' prefix. The rest of the elements
sin the list are the name of the keys that will be extracted from
the `jupyter-message-content' of MSG and passed to the handler in
the same order as they appear. For example,

    (jupyter-dispatch-message-cases client req msg
      ((shutdown-reply restart)
       (stream name text)))

will be transformed to

    (let ((content (jupyter-message-content msg)))
      (pcase (jupyter-message-type msg)
        (\"shutdown_reply\"
          (cl-destructuring-bind (&key restart @allow-other-keys)
              content
            (jupyter-handle-shutdown-reply client req restart)))
        (\"stream\"
          (cl-destructuring-bind (&key name text @allow-other-keys)
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
        (let ((msg-str (replace-regexp-in-string
                        "-" "_" (symbol-name msg-type)))
              (handler (intern (format "jupyter-handle-%s" msg-type))))
          (push `(,msg-str
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

(cl-defmethod jupyter-handle-message ((_channel jupyter-stdin-channel)
                                      client
                                      req
                                      msg)
  (jupyter-run-hook-with-args client 'jupyter-stdin-message-hook msg)
  (jupyter-dispatch-message-cases client req msg
    ((input-reply prompt password))))

(cl-defgeneric jupyter-handle-input-reply ((client jupyter-kernel-client)
                                           _req
                                           prompt
                                           password)
  "Handle an input request from CLIENT's kernel.
PROMPT is the prompt the kernel would like to show the user. If
PASSWORD is non-nil, then `read-passwd' is used to get input from
the user. Otherwise `read-from-minibuffer' is used."
  (declare (indent 1))
  (let* ((channel (oref client stdin-channel))
         (value nil)
         (msg (jupyter-message-input-reply
               :value (condition-case nil
                          (if password (read-passwd prompt)
                            (setq value (read-from-minibuffer prompt)))
                        (quit "")))))
    (jupyter-send client channel "input_reply" msg)
    (or value "")))

;;; SHELL handlers

;; http://jupyter-client.readthedocs.io/en/latest/messaging.html#messages-on-the-shell-router-dealer-channel
(cl-defmethod jupyter-handle-message ((_channel jupyter-shell-channel)
                                      client
                                      req
                                      msg)
  (jupyter-run-hook-with-args client 'jupyter-shell-message-hook msg)
  ;; Let `jupyter-handle-error' handle errors for requests.
  (unless (member (jupyter-message-get msg :status) '("error" "abort"))
    (jupyter-dispatch-message-cases client req msg
      ((execute-reply execution_count user_expressions payload)
       (shutdown-reply restart)
       (inspect-reply found data metadata)
       (complete-reply matches cursor_start cursor_end metadata)
       (history-reply history)
       (is-complete-reply status indent)
       (comm-info-reply comms)
       (kernel-info-reply protocol_version implementation
                          implementation_version language_info
                          banner help_links)))))

(cl-defgeneric jupyter-execute-request ((client jupyter-kernel-client)
                                        &key code
                                        (silent nil)
                                        (store-history t)
                                        (user-expressions nil)
                                        (allow-stdin
                                         (jupyter-channel-alive-p
                                          (oref client stdin-channel)))
                                        (stop-on-error nil))
  "Send an execute request."
  (declare (indent 1))
  (let ((channel (oref client shell-channel))
        (msg (jupyter-message-execute-request
              :code code
              :silent silent
              :store-history store-history
              :user-expressions user-expressions
              :allow-stdin allow-stdin
              :stop-on-error stop-on-error)))
    (jupyter-send client channel "execute_request" msg)))

(cl-defgeneric jupyter-handle-execute-reply ((_client jupyter-kernel-client)
                                             _req
                                             _execution-count
                                             _user-expressions
                                             _payload)
  "Default execute reply handler."
  (declare (indent 1))
  nil)

(cl-defgeneric jupyter-inspect-request ((client jupyter-kernel-client)
                                        &key code
                                        (pos 0)
                                        (detail 0))
  "Send an inspect request."
  (declare (indent 1))
  (let ((channel (oref client shell-channel))
        (msg (jupyter-message-inspect-request
              :code code :pos pos :detail detail)))
    (jupyter-send client channel "inspect_request" msg)))

(cl-defgeneric jupyter-handle-inspect-reply ((_client jupyter-kernel-client)
                                             _req
                                             _found
                                             _data
                                             _metadata)
  "Default inspect reply handler."
  (declare (indent 1))
  nil)

(cl-defgeneric jupyter-complete-request ((client jupyter-kernel-client)
                                         &key code
                                         (pos 0))
  "Send a complete request."
  (declare (indent 1))
  (let ((channel (oref client shell-channel))
        (msg (jupyter-message-complete-request
              :code code :pos pos)))
    (jupyter-send client channel "complete_request" msg)))

(cl-defgeneric jupyter-handle-complete-reply ((_client jupyter-kernel-client)
                                              _req
                                              _matches
                                              _cursor-start
                                              _cursor-end
                                              _metadata)
  "Default complete reply handler."
  (declare (indent 1))
  nil)

(cl-defgeneric jupyter-history-request ((client jupyter-kernel-client)
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
  (let ((channel (oref client shell-channel))
        (msg (jupyter-message-history-request
              :output output
              :raw raw
              :hist-access-type hist-access-type
              :session session
              :start start
              :stop stop
              :n n
              :pattern pattern
              :unique unique)))
    (jupyter-send client channel "history_request" msg)))

(cl-defgeneric jupyter-handle-history-reply ((_client jupyter-kernel-client)
                                             _req
                                             _history)
  "Default history reply handler."
  (declare (indent 1))
  nil)

(cl-defgeneric jupyter-is-complete-request ((client jupyter-kernel-client)
                                            &key code)
  "Send an is-complete request."
  (declare (indent 1))
  (let ((channel (oref client shell-channel))
        (msg (jupyter-message-is-complete-request
              :code code)))
    (jupyter-send client channel "is_complete_request" msg)))

(cl-defgeneric jupyter-handle-is-complete-reply ((_client jupyter-kernel-client)
                                                 _req
                                                 _status
                                                 _indent)
  "Default is complete reply handler."
  (declare (indent 1))
  nil)

(cl-defgeneric jupyter-comm-info-request ((client jupyter-kernel-client)
                                          &key target-name)
  "Send a comm-info request."
  (declare (indent 1))
  (let ((channel (oref client shell-channel))
        (msg (jupyter-message-comm-info-request
              :target-name target-name)))
    (jupyter-send client channel "comm_info_request" msg)))

(cl-defgeneric jupyter-handle-comm-info-reply ((_client jupyter-kernel-client)
                                               _req
                                               _comms)
  "Default comm info. reply handler."
  (declare (indent 1))
  nil)

(cl-defgeneric jupyter-kernel-info-request ((client jupyter-kernel-client))
  "Send a kernel-info request."
  (let ((channel (oref client shell-channel))
        (msg (jupyter-message-kernel-info-request)))
    (jupyter-send client channel "kernel_info_request" msg)))

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

(cl-defgeneric jupyter-shutdown-request ((client jupyter-kernel-client)
                                         &key restart)
  "Request a shutdown of CLIENT's kernel.
If RESTART is non-nil, request a restart instead of a complete shutdown."
  (declare (indent 1))
  (let ((channel (oref client shell-channel))
        (msg (jupyter-message-shutdown-request :restart restart)))
    (jupyter-send client channel "shutdown_request" msg)))

(cl-defgeneric jupyter-handle-shutdown-reply ((_client jupyter-kernel-client)
                                              _req
                                              _restart)
  "Default shutdown reply handler."
  (declare (indent 1))
  nil)

;;; IOPUB handlers

(cl-defmethod jupyter-handle-message ((_channel jupyter-iopub-channel)
                                      client
                                      req
                                      msg)
  (jupyter-run-hook-with-args client 'jupyter-iopub-message-hook msg)
  (jupyter-dispatch-message-cases client req msg
    ((shutdown-reply restart)
     (stream name text)
     (execute-input code execution_count)
     (execute-result execution_count data metadata)
     (error ename evalue traceback)
     (status execution_state)
     (clear-output wait)
     (display-data data metadata transient)
     (update-display-data data metadata transient))))

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
