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

(defclass jupyter-shell-channel (jupyter-async-channel)
  ((type
    :initform :shell)))

(defclass jupyter-iopub-channel (jupyter-async-channel)
  ((type
    :initform :iopub)))

(defclass jupyter-stdin-channel (jupyter-async-channel)
  ((type
    :initform :stdin)))

(defclass jupyter-kernel-client (eieio-instance-tracker)
  ((tracking-symbol :initform 'jupyter--clients)
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
    :type (or null process)
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

(cl-defmethod jupyter-send ((client jupyter-kernel-client)
                            channel
                            type
                            message
                            &optional msg-id)
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
    (or (eq jupyter-inhibit-handlers t)
        (cl-loop
         for msg-type in (if (eq (car jupyter-inhibit-handlers) 'not)
                             (cdr jupyter-inhibit-handlers)
                           jupyter-inhibit-handlers)
         unless (plist-member jupyter-message-types msg-type)
         do (error "Not a valid message type (`%s')" msg-type)))
    (when jupyter--debug
      (message "SENDING: %s %s" type message))
    (let ((msg-id (or msg-id (jupyter-new-uuid))))
      (jupyter-send channel type message msg-id)
      ;; Anything sent to stdin is a reply not a request so don't add it to
      ;; `:pending-requests'.
      (unless (eq (oref channel type) :stdin)
        (let ((req (jupyter-generate-request client message)))
          (setf (jupyter-request-id req) msg-id)
          (setf (jupyter-request-inhibited-handlers req) jupyter-inhibit-handlers)
          (jupyter--ioloop-push-request client req)
          req)))))

;;; Channel subprocess (receiving messages)

(defmacro jupyter--ioloop-do-command (poller channels)
  "Read and execute a command from stdin.
POLLER is a variable bound to a `zmq-poller' object. and CHANNELS
is a variable bound to an alist of (SOCK . CTYPE) pairs where
SOCK is a `zmq-socket' representing a `jupyter-channel' that has
a channel type of CTYPE.

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

(defmacro jupyter--ioloop-with-lock-file (client &rest body)
  "In CLIENT's IOLoop buffer run BODY, ensuring the lock file mechanism works.
This makes sure that when `set-buffer-modified-p' is called, it
properly locks or unlocks the associated lock file for the IOLoop
process."
  (declare (indent 1))
  (let ((lock (make-symbol "--lock")))
    `(with-current-buffer (oref ,client -buffer)
       (let* ((create-lockfiles t)
              (,lock (concat "jupyter-lock" (jupyter-session-id (oref ,client session))))
              (buffer-file-name (expand-file-name ,lock temporary-file-directory))
              (buffer-file-truename (file-truename buffer-file-name)))
         ,@body))))

(defun jupyter--ioloop-unlock (client)
  "Unlock CLIENT's file lock."
  (jupyter--ioloop-with-lock-file client
    (set-buffer-modified-p nil)))

(defun jupyter--ioloop-lock (client)
  "Return the file name of CLIENT's file lock.
Acquire the lock first. Unlock the lock if one exists. The lock
file serves as a proxy for the case when the parent Emacs process
crashes without properly cleaning up its child processes."
  (jupyter--ioloop-with-lock-file client
    (jupyter--ioloop-unlock client)
    (set-buffer-modified-p t)
    (buffer-file-name)))

(defun jupyter--ioloop (client)
  "Return the function used for communicating with CLIENT's kernel."
  (cl-assert (zmq-has "draft") nil "ZMQ built without poller support.")
  (let ((sid (jupyter-session-id (oref client session)))
        (skey (jupyter-session-key (oref client session)))
        (lock (jupyter--ioloop-lock client)))
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
              (messages nil)
              (poller nil)
              (events nil)
              (read-command-from-parent
               (lambda ()
                 (when (alist-get 0 events)
                   (setf (alist-get 0 events nil 'remove) nil)
                   (jupyter--ioloop-do-command poller channels))))
              (queue-messages
               (lambda ()
                 (dolist (type-channel channels)
                   (cl-destructuring-bind (type . channel) type-channel
                     (when (zmq-assoc (oref channel socket) events)
                       (push (cons type (jupyter-recv channel)) messages))))))
              (send-messages-to-parent
               (lambda ()
                 ;; TODO: Throttle messages if they are coming in too hot
                 (when messages
                   (setq messages (nreverse messages))
                   (while messages
                     (prin1 (cons 'recvd (pop messages))))
                   (zmq-flush 'stdout)))))
         (condition-case nil
             (progn
               (setq poller (zmq-poller))
               ;; Poll for stdin messages
               (zmq-poller-add poller 0 zmq-POLLIN)
               (zmq-prin1 '(start))
               (while t
                 (setq events (condition-case nil
                                  (zmq-poller-wait-all
                                   poller (1+ (length channels)) 5000)
                                ((zmq-EAGAIN zmq-EINTR zmq-ETIMEDOUT) nil)))
                 (unless (or events (file-locked-p ,lock))
                   ;; TODO: The parent process probably
                   ;; crashed, cleanup the kernel
                   ;; connection file if there is one.
                   ;; Since the parent Emacs crashed, all
                   ;; of the kernel processes are gone to.
                   (signal 'quit nil))
                 (funcall read-command-from-parent)
                 (funcall queue-messages)
                 (funcall send-messages-to-parent)))
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
Pending requests are stored in a ring located in the
`:pending-requests' property of an ioloop subprocess. REQ is
added as the newest element in this ring."
  (let ((ring (or (process-get (oref client ioloop) :pending-requests)
                  (let ((ring (make-ring 10)))
                    (process-put (oref client ioloop) :pending-requests ring)
                    ring))))
    (ring-insert+extend ring req 'grow)))

;;; HB channel methods

(cl-defmethod jupyter-hb-pause ((client jupyter-kernel-client))
  "Pause CLIENT's heartbeeat channel."
  (jupyter-hb-pause (oref client hb-channel)))

(cl-defmethod jupyter-hb-unpause ((client jupyter-kernel-client))
  "Unpause CLIENT's heartbeat channel."
  (jupyter-hb-unpause (oref client hb-channel)))

(cl-defmethod jupyter-hb-beating-p ((client jupyter-kernel-client))
  "Is CLIENT still connected to its kernel?"
  (jupyter-hb-beating-p (oref client hb-channel)))

;;; Channel subprocess filter/sentinel

(defun jupyter--ioloop-sentinel (client ioloop _event)
  "The process sentinel for CLIENT's IOLOOP subprocess.
When EVENT is one of the events signifying that the process is
dead, stop the heartbeat channel and set the IOLOOP slot to nil
in CLIENT."
  (cond
   ((not (process-live-p ioloop))
    (jupyter-stop-channel (oref client hb-channel))
    (jupyter--ioloop-unlock client)
    (oset client kernel-info nil)
    (oset client ioloop nil))))

(defun jupyter--get-channel (client ctype)
  "Get CLIENT's channel based on CTYPE."
  (catch 'found
    (dolist (channel (mapcar (lambda (sym) (slot-value client sym))
                        '(hb-channel
                          stdin-channel
                          shell-channel
                          iopub-channel)))
      (when (eq (oref channel type) ctype)
        (throw 'found channel)))))

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
       (let ((req (jupyter--ioloop-pop-request client))
             (requests (oref client requests)))
         (cl-assert (equal (jupyter-request-id req) msg-id)
                    nil "Message request sent out of order to the kernel.")
         (puthash msg-id req requests)
         (puthash "last-sent" req requests))))
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
     (when jupyter--debug
       (message "STARTING-CHANNEL: %s" ctype))
     (let ((channel (jupyter--get-channel client ctype)))
       (oset channel status 'running)))
    (`(stop-channel ,ctype)
     (when jupyter--debug
       (message "STOPPING-CHANNEL: %s" ctype))
     (let ((channel (jupyter--get-channel client ctype)))
       ;; TODO: Wrap this in an async channel method, maybe
       ;; re-use `jupyter-stop-channel'. On the first call,
       ;; when we send a stop channel message to the
       ;; subprocess we set the status to pending, then
       ;; here once we know the channel was stopped, we
       ;; call `jupyter-stop-channel' again and it updates
       ;; the status to stopped. Seems too complicated.
       (oset channel status 'stopped)))
    ('(start)
     (when jupyter--debug
       (message "CLIENT STARTING"))
     ;; TODO: Generalize setting flag variables for IOLoop events and having
     ;; event callbacks.
     (process-put (oref client ioloop) :start t))
    ('(quit)
     ;; Cleanup handled in sentinel
     (process-put (oref client ioloop) :quit t)
     (when jupyter--debug
       (message "CLIENT CLOSED")))))

;;; Starting the channel subprocess

(defun jupyter-ioloop-wait-until (event ioloop &optional timeout)
  "Wait until EVENT occurs in IOLOOP.
Currently EVENT can be :start or :quit and this function will
block for TIMEOUT seconds until IOLOOP starts or quits depending
on EVENT. If TIMEOUT is nil it defaults to 1 s."
  (or timeout (setq timeout 1))
  (with-timeout (timeout nil)
    (while (null (process-get ioloop event))
      (accept-process-output ioloop 1))
    t))

(defun jupyter--start-ioloop (client)
  "Start CLIENT's channel subprocess."
  (unless (oref client ioloop)
    (oset client ioloop
          (zmq-start-process
           (jupyter--ioloop client)
           :filter (apply-partially #'jupyter--ioloop-filter client)
           :sentinel (apply-partially #'jupyter--ioloop-sentinel client)
           :buffer (oref client -buffer)))
    (jupyter-ioloop-wait-until :start (oref client ioloop))))

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
    (when (process-live-p ioloop)
      (zmq-subprocess-send ioloop (cons 'quit nil))
      (with-timeout (1 (delete-process ioloop)
                       (message "IOloop process not killed by request"))
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

(cl-defmethod jupyter-handle-message ((client jupyter-kernel-client) channel)
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
  (let ((msg (jupyter-get-message channel)))
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
              (jupyter--drop-idle-requests client))))))))

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

(cl-defmethod jupyter-handle-message ((_channel jupyter-stdin-channel)
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
  (let* ((channel (oref client stdin-channel))
         (value nil)
         (msg (jupyter-message-input-reply
               :value (condition-case nil
                          (if (eq password t) (read-passwd prompt)
                            (setq value (read-from-minibuffer prompt)))
                        (quit "")))))
    (jupyter-send client channel :input-reply msg)
    (or value "")))

(defalias 'jupyter-handle-input-reply 'jupyter-handle-input-request)

;;; SHELL handlers

;; http://jupyter-client.readthedocs.io/en/latest/messaging.html#messages-on-the-shell-router-dealer-channel
(cl-defmethod jupyter-handle-message ((_channel jupyter-shell-channel)
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

;;; Evaluation

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
    (jupyter-send client channel :execute-request msg)))

(cl-defgeneric jupyter-handle-execute-reply ((_client jupyter-kernel-client)
                                             _req
                                             _status
                                             _execution-count
                                             _user-expressions
                                             _payload)
  "Default execute reply handler."
  (declare (indent 1))
  nil)

(cl-defgeneric jupyter-send-inspect-request ((client jupyter-kernel-client)
                                             &key code
                                             (pos 0)
                                             (detail 0))
  "Send an inspect request."
  (declare (indent 1))
  (let ((channel (oref client shell-channel))
        (msg (jupyter-message-inspect-request
              :code code :pos pos :detail detail)))
    (jupyter-send client channel :inspect-request msg)))

(cl-defgeneric jupyter-handle-inspect-reply ((_client jupyter-kernel-client)
                                             _req
                                             _found
                                             _data
                                             _metadata)
  "Default inspect reply handler."
  (declare (indent 1))
  nil)

;;; Completion contexts

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
  (let ((channel (oref client shell-channel))
        (msg (jupyter-message-complete-request
              :code code :pos pos)))
    (jupyter-send client channel :complete-request msg)))

(cl-defgeneric jupyter-handle-complete-reply ((_client jupyter-kernel-client)
                                              _req
                                              _matches
                                              _cursor-start
                                              _cursor-end
                                              _metadata)
  "Default complete reply handler."
  (declare (indent 1))
  nil)

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
    (jupyter-send client channel :history-request msg)))

(cl-defgeneric jupyter-handle-history-reply ((_client jupyter-kernel-client)
                                             _req
                                             _history)
  "Default history reply handler."
  (declare (indent 1))
  nil)

(cl-defgeneric jupyter-send-is-complete-request ((client jupyter-kernel-client)
                                                 &key code)
  "Send an is-complete request."
  (declare (indent 1))
  (let ((channel (oref client shell-channel))
        (msg (jupyter-message-is-complete-request
              :code code)))
    (jupyter-send client channel :is-complete-request msg)))

(cl-defgeneric jupyter-handle-is-complete-reply ((_client jupyter-kernel-client)
                                                 _req
                                                 _status
                                                 _indent)
  "Default is complete reply handler."
  (declare (indent 1))
  nil)

(cl-defgeneric jupyter-send-comm-info-request ((client jupyter-kernel-client)
                                               &key target-name)
  "Send a comm-info request."
  (declare (indent 1))
  (let ((channel (oref client shell-channel))
        (msg (jupyter-message-comm-info-request
              :target-name target-name)))
    (jupyter-send client channel :comm-info-request msg)))

(cl-defgeneric jupyter-send-comm-open ((client jupyter-kernel-client)
                                       &key id
                                       target-name
                                       data)
  (declare (indent 1))
  (let ((channel (oref client shell-channel))
        (msg (jupyter-message-comm-open
              :id id
              :target-name target-name
              :data data)))
    (jupyter-send client channel :comm-open msg)))

(cl-defgeneric jupyter-send-comm-msg ((client jupyter-kernel-client)
                                      &key id
                                      data)
  (declare (indent 1))
  (let ((channel (oref client shell-channel))
        (msg (jupyter-message-comm-msg
              :id id
              :data data)))
    (jupyter-send client channel :comm-msg msg)))

(cl-defgeneric jupyter-send-comm-close ((client jupyter-kernel-client)
                                        &key id
                                        data)
  (declare (indent 1))
  (let ((channel (oref client shell-channel))
        (msg (jupyter-message-comm-close
              :id id
              :data data)))
    (jupyter-send client channel :comm-close msg)))

(cl-defgeneric jupyter-handle-comm-info-reply ((_client jupyter-kernel-client)
                                               _req
                                               _comms)
  "Default comm info. reply handler."
  (declare (indent 1))
  nil)

;;; Accessing kernel info properties

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

;;; Load kernel language support definitions

(defun jupyter-load-language-support (client)
  "Load language support definitions for CLIENT.
CLIENT is a `jupyter-kernel-client'."
  (cl-assert (object-of-class-p client 'jupyter-kernel-client))
  (let* ((lang (jupyter-kernel-language client))
         (support (intern (concat "jupyter-" lang))))
    (require support nil t)))

(cl-defgeneric jupyter-send-kernel-info-request ((client jupyter-kernel-client))
  "Send a kernel-info request."
  (let ((channel (oref client shell-channel))
        (msg (jupyter-message-kernel-info-request)))
    (jupyter-send client channel :kernel-info-request msg)))

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

(cl-defgeneric jupyter-send-shutdown-request ((client jupyter-kernel-client)
                                              &key restart)
  "Request a shutdown of CLIENT's kernel.
If RESTART is non-nil, request a restart instead of a complete shutdown."
  (declare (indent 1))
  (let ((channel (oref client shell-channel))
        (msg (jupyter-message-shutdown-request :restart restart)))
    (jupyter-send client channel :shutdown-request msg)))

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
