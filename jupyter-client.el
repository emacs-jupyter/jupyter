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
(require 'jupyter-connection)
(require 'jupyter-channels)
(require 'jupyter-messages)
(defcustom jupyter-include-other-output nil
  "Whether or not to handle messages not sent by a client.
A Jupyter client can receive messages from other clients
connected to the same kernel on the IOPub channel. You can choose
to ignore these messages by setting
`jupyter-include-other-output' to nil. If
`jupyter-include-other-output' is non-nil, then any messages that
are not associated with a request from a client are sent to the
client's handler methods with a nil value for the request
argument. To change the value of this variable for a particular
client use `jupyter-set'."
  :group 'jupyter
  :type 'boolean)

(defvar jupyter--debug nil
  "Set to non-nil to emit sent and received messages to *Messages*.")

(defvar jupyter-default-timeout 1
  "The default timeout in seconds for `jupyter-wait-until'.")

(defclass jupyter-kernel-client (jupyter-connection)
  ((requests
    :type hash-table
    :initform (make-hash-table :test 'equal)
    :documentation "A hash table with message ID's as keys. This
 is used to register callback functions to run once a reply from
 a previously sent request is received. See
 `jupyter-add-callback'. Note that this is also used to filter
 received messages that originated from a previous request by
 this client. Whenever the client sends a message in which a
 reply is expected, it sets an entry in this table to represent
 the fact that the message has been sent. So if there is a
 non-nil value for a message ID it means that a message has been
 sent and the client is expecting a reply from the kernel.")
   (ioloop
    :type (or null process)
    :initform nil
    :documentation "The process which polls for events on all
 live channels of the client.")
   ;; NOTE: With the current implementation all channels except the heartbeat
   ;;       channel actually communicate with the kernel through the ioloop
   ;;       subprocess. This means that the socket field of the channels are
   ;;       not actually used. They are mainly used to dispatch received
   ;;       messages from the IOLoop subprocess and to hold the endpoint
   ;;       information of the connection.
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
   (hb-channel
    :type (or null jupyter-hb-channel)
    :initform nil
    :initarg :hb-channel
    :documentation "The heartbeat channel.")
   (stdin-channel
    :type (or null jupyter-stdin-channel)
    :initform nil
    :initarg :stdin-channel
    :documentation "The stdin channel.")))

(cl-defmethod initialize-instance ((client jupyter-kernel-client) &rest _slots)
  (cl-call-next-method)
  ;; Add a catch all request for headless messages received from the kernel.
  ;; Set -id to a non-nil value so that `jupyter-request-id' works.
  (puthash nil (make-jupyter-request :-id "" :run-handlers-p nil)
           (oref client requests)))

(defun jupyter-missing-request (client)
  (cl-check-type client jupyter-kernel-client)
  (gethash nil (oref client requests)))

(cl-defmethod jupyter-initialize-connection ((client jupyter-kernel-client)
                                             &optional file-or-plist)
  "Initialize CLIENT with a connection FILE-OR-PLIST.
When FILE-OR-PLIST is a file name, read the JSON connection
information from the file and initialize CLIENT's connection and
channels from the file's contents. When FILE-OR-PLIST is a plist,
initialize CLIENT's connection and channels from the plist. When
FILE-OR-PLIST is nil, then the `conn-info' slot of CLIENT is used
to initialize the connection. The necessary keys to initialize a
connection can be found at
http://jupyter-client.readthedocs.io/en/latest/kernels.html#connection-files.

As a side effect, if CLIENT is already connected to a kernel its
connection is terminated before initializing."
  (let ((conn-info (if file-or-plist
                       (oset client conn-info
                             (if (json-plist-p file-or-plist) file-or-plist
                               (let ((json-array-type 'list)
                                     (json-object-type 'plist)
                                     (json-false nil))
                                 (json-read-file file-or-plist))))
                     (or (ignore-errors (oref client conn-info))
                         (signal 'unbound-slot
                                 (list 'json-plist client 'conn-info))))))
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
      ;; A kernel manager may have already initialized the session, see
      ;; `jupyter-make-client'
      (unless (and (ignore-errors (oref client session))
                   (equal (jupyter-session-key (oref client session)) key))
        (oset client session (jupyter-session :key key)))
      (cl-loop
       with addr = (concat transport "://" ip)
       for (channel . port) in `((stdin-channel . ,stdin_port)
                                 (shell-channel . ,shell_port)
                                 (hb-channel . ,hb_port)
                                 (iopub-channel . ,iopub_port))
       for class = (intern (concat "jupyter-" (symbol-name channel)))
       do (setf (slot-value client channel)
                (make-instance
                 class
                 ;; So channels have access to the client's session
                 :parent-instance client
                 :endpoint (format "%s:%d" addr port)))))))

;;; Lower level sending/receiving
(defmacro with-jupyter-client-buffer (client &rest body)
  "Run a form inside CLIENT's IOloop subprocess buffer."
  (declare (indent 1))
  `(progn
     (cl-check-type client jupyter-kernel-client)
     (with-current-buffer (oref client ioloop)
       ,@body)))

(defun jupyter-set (client symbol newval)
  "Set CLIENT's local value for SYMBOL to NEWVAL."
  (with-jupyter-client-buffer client
    (set (make-local-variable symbol) newval)))

(defun jupyter-get (client symbol)
  "Get CLIENT's local value of SYMBOL."
  (with-jupyter-client-buffer client
    (symbol-value symbol)))


(cl-defmethod jupyter-send ((client jupyter-kernel-client)
                            channel
                            type
                            message
                            &optional flags)
  "Send a message on CLIENT's CHANNEL.
Return a `jupyter-request' representing the sent message. CHANNEL
is one of the channel's of CLIENT. TYPE is one of the values in
`jupyter-message-types' and is the type of the MESSAGE. If FLAGS
is non-nil, it has the same meaning as FLAGS in `zmq-send'. You
can manipulate how to handle messages received in response to the
sent message, see `jupyter-add-callback' and
`jupyter-request-inhibit-handlers'."
  (declare (indent 1))
  (let ((ioloop (oref client ioloop)))
    (unless ioloop
      (signal 'wrong-type-argument (list 'process ioloop 'ioloop)))
    (zmq-subprocess-send (oref client ioloop)
      (list 'send (oref channel type) type message flags))
    ;; Anything sent to stdin is a reply not a request so don't add it to
    ;; `:jupyter-pending-requests.'
    (unless (eq (oref channel type) :stdin)
      (let ((req (make-jupyter-request)))
        (jupyter--ioloop-push-request client req)
        req))))

;;; IOLoop subprocess communication

(defmacro jupyter--ioloop-do-command (session poller channels)
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
  ;; TODO: Would like to convert this to `pcase' but there seems to be issues
  ;; with `pcase', it never matches the pattern correctly in the subprocess
  ;; even though it matches perfectly well in the parent emacs
  `(cl-destructuring-bind (cmd . args)
       (zmq-subprocess-read)
     (cl-case cmd
       (send
        (cl-destructuring-bind (ctype . args) args
          (let ((sock (car (rassoc ctype ,channels))))
            (zmq-prin1
             (list 'sent ctype (apply #'jupyter-send ,session sock args))))))
       (start-channel
        (let ((sock (car (rassoc args ,channels))))
          (zmq-connect sock (zmq-socket-get sock zmq-LAST_ENDPOINT))
          (zmq-poller-register ,poller sock zmq-POLLIN)))
       (stop-channel
        (let ((sock (car (rassoc args ,channels))))
          (zmq-poller-unregister ,poller sock)
          (condition-case err
              (zmq-disconnect
               sock (zmq-socket-get sock zmq-LAST_ENDPOINT))
            (zmq-ENOENT nil))))
       (quit
        (signal 'quit nil))
       (otherwise (error "Unhandled command (%s)" cmd)))))

(defmacro jupyter--ioloop-queue-message (messages priorities elem)
  "Add a single message to MESSAGES.
MESSAGES should be a variable name which is bound to a list or
nil, PRIORITIES should be a variable name which is bound to an
alist of (CTYPE . PRIORITY) pairs where CTYPE is a
`jupyter-channel' type and PRIORITY is a number representing the
priority of the channel. ELEM should be a cons cell

    (CTYPE . IDENTS-MSG)

where CTYPE is the `jupyter-channel' type that IDENTS-MSG was
received on. Note that IDENTS-MSG should be the cons cell
returned by `jupyter-recv'.

Multiple calls to the expansion of
`jupyter--ioloop-queue-message' with the same MESSAGES list will
place ELEM on the list at a position that depends on the
`jupyter-message-time' of MSG and the priority of CTYPE. MESSAGES
will sorted by increasing `jupyter-message-time' of its messages
and in case two messages have `equal' message times, the message
whose channel type has a higher priority will come before the
message that has a channel type with the lower priority."
  (declare (indent 2))
  `(let ((elem ,elem))
     (if (null ,messages) (push elem ,messages)
       ;; Put elem in its sorted position
       (let ((ctype (car elem))
             (mt (jupyter-message-time (cddr elem)))
             (head ,messages)
             (tail ,messages))
         (while (and
                 tail
                 ;; Non-nil if msg should come after tail
                 (let ((tctype (caar tail))
                       (tmt (jupyter-message-time (cddar tail))))
                   (or (time-less-p tmt mt)
                       (when (equal mt tmt)
                         (< (alist-get ctype ,priorities)
                            (alist-get tctype ,priorities))))))
           (setq
            head tail
            tail (cdr tail)))
         (if (eq head tail) (setq ,messages (cons elem head))
           (setcdr head (cons elem tail)))))))

(defmacro jupyter--ioloop-collect-messages
    (session poller channels messages priorities timeout)
  "Collect messages from kernel.
SESSION, POLLER, CHANNELS, MESSAGES, PRIORITIES, and TIMEOUT
should all be variable names bound to objects with the following
meanings:

SESSION - A `jupyter-session'

POLLER - A `zmq-poller'

CHANNELS - An alist of (SOCK . CTYPE) pairs where sock is a
           `zmq-socket' representing a `jupyter-channel' with
           type CTYPE.

MESSAGES - A variable in which to store the collected list of
           messages collected during this polling period. If the
           variable is already bound to a list, new messages
           added to it will be sorted based on the `:date' field
           of the Jupyter message also taking into account
           PRIORITIES.

PRIORITIES - An alist of (CTYPE . PRIORITY) pairs where CTYPE is
             a `jupyter-channel' type with PRIORITY, a number. If
             one channel has a higher priority than another and
             two messages, one from each channel, have the same
             `:date' field, the message with the higher channel
             priority will have its message come before the
             message whose channel has a lower priority in the
             sorted order."
  `(let ((events (zmq-poller-wait-all ,poller 4 ,timeout)))
     (when (alist-get 0 events)
       ;; Got input from stdin, do the command it
       ;; specifies
       (setf (alist-get 0 events nil 'remove) nil)
       (jupyter--ioloop-do-command ,session ,poller ,channels))
     (dolist (sock (mapcar #'car events))
       (jupyter--ioloop-queue-message ,messages ,priorities
         (cons (alist-get sock channels)
               (jupyter-recv ,session sock))))
     events))

;; TODO: Make this more debuggable, I've spent hours wondering why I wasn't
;; receiving messages only to find out (caar elem) should have been (car elem)
;; in `jupyter--ioloop-queue-message'. For some reason the `condition-case' in
;; `zmq--init-subprocess' is not sending back the error. Or more specifically,
;; in the subprocess errors are being turned into warnings.
(defun jupyter--ioloop (client)
  "Return the function used for communicating with CLIENT's kernel."
  (let* ((session (oref client session))
         (sid (jupyter-session-id session))
         (skey (jupyter-session-key session))
         (iopub-ep (oref (oref client iopub-channel) endpoint))
         (shell-ep (oref (oref client shell-channel) endpoint))
         (stdin-ep (oref (oref client stdin-channel) endpoint)))
    `(lambda (ctx)
       (push ,(file-name-directory (locate-library "jupyter-base")) load-path)
       (require 'jupyter-channels)
       (require 'jupyter-messages)
       (let* ((session (jupyter-session :id ,sid :key ,skey))
              (iopub (jupyter-connect-channel :iopub ,iopub-ep ,sid))
              (shell (jupyter-connect-channel :shell ,shell-ep ,sid))
              (stdin (jupyter-connect-channel :stdin ,stdin-ep ,sid))
              (priorities '((:shell . 4)
                            (:iopub . 2)
                            (:stdin . 2)))
              (channels `((,stdin . :stdin)
                          (,shell . :shell)
                          (,iopub . :iopub)))
              (idle-count 0)
              (timeout 20)
              (messages nil))
         (zmq-socket-set iopub zmq-SUBSCRIBE "")
         (condition-case nil
             (with-zmq-poller poller
               ;; Poll for stdin messages
               (zmq-poller-register poller 0 zmq-POLLIN)
               (mapc (lambda (x) (zmq-poller-register poller (car x) zmq-POLLIN))
                  channels)
               (while t
                 (if (jupyter--ioloop-collect-messages
                      session poller channels messages priorities timeout)
                     (setq idle-count 0 timeout 20)
                   (setq idle-count (1+ idle-count))
                   ;; Lengthen timeout so as to not waste CPU cycles
                   (when (= idle-count 100) (setq timeout 100)))
                 ;; Pool at least some messages, but not at the cost of
                 ;; responsiveness. If messages are being blasted at us by the
                 ;; kernel ensure that they still get through and not pooled
                 ;; indefinately.
                 (when (or (= idle-count 5) (> (length messages) 10))
                   (while messages
                     (zmq-prin1 (cons 'recvd (pop messages)))))))
           (quit
            (mapc (lambda (x)
                 (zmq-socket-set (car x) zmq-LINGER 0)
                 (zmq-close (car x)))
               channels)
            (zmq-prin1 (list 'quit))))))))

(defun jupyter--ioloop-pop-request (client)
  "Remove a pending request from CLIENT's ioloop subprocess.
Specifically remove the oldest element of the ring located in the
`:jupyter-pending-requests' property of CLIENT's ioloop
subprocess."
  (let ((ring (process-get (oref client ioloop) :jupyter-pending-requests)))
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

Pending requests are stored as the `:jupyter-pending-requests'
property of an ioloop subprocess. REQ is added as the newest
element in `:jupyter-pending-requests'."
  (let* ((ioloop (oref client ioloop))
         (ring (or (process-get ioloop :jupyter-pending-requests)
                   (let ((ring (make-ring 10)))
                     (process-put ioloop :jupyter-pending-requests ring)
                     ring))))
    (ring-insert+extend ring req 'grow)))

(defun jupyter--ioloop-sentinel (client _ioloop event)
  "The process sentinel for CLIENT's IOLOOP subprocess.
When EVENT is one of the events signifying that the process is
dead, stop the heartbeat channel and set the IOLOOP slot to nil
in CLIENT."
  (cond
   ((cl-loop for type in '("exited" "failed" "finished" "killed" "deleted")
             thereis (string-prefix-p type event))
    (jupyter-stop-channel (oref client hb-channel))
    (when (process-buffer ioloop)
      (kill-buffer (process-buffer ioloop)))
    (oset client ioloop nil))))

(defun jupyter--ioloop-filter (client event)
  "The process filter for CLIENT's ioloop subprocess.
EVENT will be an s-expression emitted from the function returned
by `jupyter--ioloop'."
  (pcase event
    (`(sent ,ctype ,msg-id)
     (when jupyter--debug
       (message "SEND: %s" msg-id))
     (unless (eq ctype :stdin)
       ;; Anything sent on stdin is a reply and therefore never added to
       ;; `:jupyter-pending-requests'
       (let ((req (jupyter--ioloop-pop-request client)))
         (setf (jupyter-request--id req) msg-id)
         (puthash msg-id req (oref client requests)))))
    (`(recvd ,ctype ,idents . ,msg)
     (when jupyter--debug
       (message "RECV: %s %s %s"
                (jupyter-message-type msg)
                (jupyter-message-parent-id msg)
                (jupyter-message-content msg)))
     (let ((channel (cl-find-if (lambda (c) (eq (oref c type) ctype))
                                (mapcar (lambda (x) (slot-value client x))
                                   '(stdin-channel
                                     shell-channel
                                     iopub-channel)))))
       (jupyter-queue-message channel (cons idents msg))
       (run-with-timer 0.001 nil #'jupyter-handle-message client channel)))
    ('(quit)
     ;; Cleanup handled in sentinel
     (when jupyter--debug
       (message "CLIENT CLOSED")))))

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

    (jupyter-start-channels client :control nil)

In addition to calling `jupyter-start-channel', a subprocess is
created for each channel which monitors the channel's socket for
input events. Note that this polling subprocess is not created
for the heartbeat channel."
  (unless (oref client ioloop)
    ;; TODO: Currently there is no way to stop/start a channel individually
    ;; outside of this method. Create channel methods which are aware of a
    ;; client's ioloop so that you can send commands to the ioloop to start and
    ;; stop a channel. Also figure out a way to block until the ioloop says it
    ;; has finished with the operation. This may need changes in
    ;; `jupyter--ioloop'
    (let ((ioloop (zmq-start-process
                   (jupyter--ioloop client)
                   (apply-partially #'jupyter--ioloop-filter client)
                   (apply-partially #'jupyter--ioloop-sentinel client))))
      (oset client ioloop ioloop)
      (set-process-buffer ioloop (generate-new-buffer " *jupyter-kernel-client*"))
      (when hb (jupyter-start-channel (oref client hb-channel)))
      (unless shell
        (zmq-subprocess-send ioloop '(stop-channel :shell)))
      (unless iopub
        (zmq-subprocess-send ioloop '(stop-channel :iopub)))
      (unless stdin
        (zmq-subprocess-send ioloop '(stop-channel :stdin))))))

(cl-defmethod jupyter-stop-channels ((client jupyter-kernel-client))
  "Stop any running channels of CLIENT."
  (when (oref client hb-channel)
    (jupyter-stop-channel (oref client hb-channel)))
  (let ((ioloop (oref client ioloop)))
    (when ioloop
      (zmq-subprocess-send ioloop (cons 'quit nil))
      (with-timeout (1 (delete-process ioloop))
        (while (oref client ioloop)
          (sleep-for 0 100))))))

(cl-defmethod jupyter-channels-running-p ((client jupyter-kernel-client))
  "Are any channels of CLIENT alive?"
  (cl-loop
   for channel in (list 'shell-channel
                        'iopub-channel
                        'hb-channel
                        'stdin-channel)
   ;; FIXME: This does not work with the current implementation of channels
   thereis (jupyter-channel-alive-p (slot-value client channel))))

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
REQ is a `jupyter-request' object, MSG-TYPE should be one of the
keywords corresponding to a received message type in
`jupyter-message-types', and CB will be the callback that will be
run when MSG-TYPE is received for REQ."
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
          (setq cb (apply-partially
                    (lambda (cb1 cb2 msg)
                      (funcall cb1 msg)
                      (funcall cb2 msg))
                    (cdr cb-for-type)
                    cb))
          (setcdr cb-for-type cb))))))

(defun jupyter-add-callback (req msg-type cb &rest callbacks)
  "Add a callback to run when a message is received for a request.
REQ is a `jupyter-request' returned by one of the request methods
of a `jupyter-kernel-client'. MSG-TYPE is a keyword corresponding
to one of the keys in `jupyter-message-types'. CB is the callback
function which will run with a single argument, a message whose
`jupyter-message-parent-id' is `equal' to the
`jupyter-request-id' of REQ and whose `jupyter-message-type'
corresponds to the value of MSG-TYPE in `jupyter-message-types'.
MSG-TYPE can also be a list, in which case run CB for every
MSG-TYPE in the list. If MSG-TYPE is t, then run CB for every
message received for REQ.

Any additional arguments to `jupyter-add-callback' are
interpreted as additional CALLBACKS to add to REQ. So to add
multiple callbacks to a request you would do

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

(defun jupyter-wait-until (req msg-type cb &optional timeout)
  "Wait until conditions for a request are satisfied.
REQ, MSG-TYPE, and CB have the same meaning as in
`jupyter-add-callback'. If CB returns non-nil within TIMEOUT
seconds, return the message that caused CB to return non-nil. If
CB never returns a non-nil value within TIMEOUT, return nil. Note
that if no TIMEOUT is given, `jupyter-default-timeout' is used."
  (declare (indent 1))
  (setq timeout (or timeout jupyter-default-timeout))
  (cl-check-type timeout number)
  (let ((msg nil))
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
MSG-TYPE and REQ has the same meaning as their corresponding
argument in `jupyter-add-callback'. If no message that matches
MSG-TYPE is received for REQ within TIMEOUT seconds, return nil.
Otherwise return the first message that matched MSG-TYPE. Note
that if no TIMEOUT is given, it defaults to
`jupyter-default-timeout'."
  (declare (indent 1))
  (jupyter-wait-until req msg-type #'identity timeout))

;; TODO: Check if kernel has already started. This doesn't seem possible at
;; first glance since a client can connect to a kernel at any time. We may be
;; able to check if client has a kernel manager parent instance.
;;
;; TODO: Global message callbacks, i.e. messages callbacks for all messages of
;; a channel instead of just for a particular request. It would remove the need
;; for `jupyter-missing-request'. It also seems like channel callbacks should
;; be a feature anyways.
(defun jupyter-wait-until-startup (client &optional timeout)
  "Wait until CLIENT receives a status: starting message.
Return the startup message if received by CLIENT within TIMEOUT
seconds otherwise return nil. TIMEOUT defaults to 1 s. Note that
there are no checks to determine if the kernel CLIENT is
connected to has already been started."
  (cl-check-type client jupyter-kernel-client)
  (prog1 (jupyter-wait-until
             ;; TODO: Better name than missing-request. Something like headless
             ;; request seems more accurate since a starting message has no
             ;; parent header, but it still seems like you need to know more
             ;; about it.
             (jupyter-missing-request client) :status
           (lambda (msg) (equal (jupyter-message-get msg :execution_state)
                           "starting"))
           timeout)
    ;; Remove the callback since the `jupyter-missing-request' does not go
    ;; through the normal request code path
    (setf (jupyter-request-callbacks (jupyter-missing-request client)) nil)))

(defun jupyter--drop-idle-requests (client)
  (cl-loop
   with requests = (oref client requests)
   with ctime = (current-time)
   for req in (hash-table-values requests)
   for ltime = (jupyter-request-last-message-time req)
   for id = (jupyter-request-id req)
   when (and (jupyter-request-idle-received-p req)
             (> (float-time (time-subtract ctime ltime)) 60))
   ;; Don't remove the `jupyter-missing-request'
   unless (equal id "") do (remhash id requests)))

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
  (when (jupyter-messages-available-p channel)
    (let* ((msg (jupyter-get-message channel))
           (pmsg-id (jupyter-message-parent-id msg))
           (requests (oref client requests))
           (req (gethash pmsg-id requests)))
      (when (or req jupyter-include-other-output)
        (setf (jupyter-request-last-message-time req) (current-time))
        (unwind-protect
            (jupyter--run-callbacks req msg)
          (unwind-protect
              (when (jupyter-request-run-handlers-p req)
                (jupyter-handle-message channel client req msg))
            ;; Checking for pmsg-id prevents the removal of
            ;; `jupyter-missing-request' for the client
            (when (and pmsg-id (jupyter-message-status-idle-p msg))
              (setf (jupyter-request-idle-received-p req) t))
            (jupyter--drop-idle-requests client)))))))

;;; STDIN message requests/handlers

(cl-defmethod jupyter-handle-message ((channel jupyter-stdin-channel)
                                      client
                                      req
                                      msg)
  (cl-destructuring-bind (&key prompt password &allow-other-keys)
      (jupyter-message-content msg)
    (jupyter-handle-input-reply client req prompt password)))

(cl-defgeneric jupyter-handle-input-reply ((client jupyter-kernel-client)
                                           req
                                           prompt
                                           password)
  "Handle an input request from CLIENT's kernel.
PROMPT is the prompt the kernel would like to show the user. If
PASSWORD is non-nil, then `read-passwd' is used to get input from
the user. Otherwise `read-from-minibuffer' is used."
  ;; TODO: Allow for quiting the input request. In this case, I suppose send an
  ;; interrupt request to the kernel
  (let ((channel (oref client stdin-channel))
        (msg (jupyter-message-input-reply
              :value (funcall (if password #'read-passwd
                                #'read-from-minibuffer)
                              prompt))))
    ;; TODO: Check for 'allow_stdin'
    ;; http://jupyter-client.readthedocs.io/en/latest/messaging.html#stdin-messages
    (jupyter-send client channel "input_reply" msg)))

;;; SHELL message requests/handlers

;; http://jupyter-client.readthedocs.io/en/latest/messaging.html#messages-on-the-shell-router-dealer-channel
(cl-defmethod jupyter-handle-message ((channel jupyter-shell-channel)
                                      client
                                      req
                                      msg)
  (let ((content (jupyter-message-content msg)))
    ;; TODO: How to handle errors? Let the IOPub error message handler deal
    ;; with it? Or do something here?
    (cl-destructuring-bind (&key status _ename _evalue &allow-other-keys) content
      ;; NOTE: Silently does nothing on error
      (unless (member status '("error" "abort"))
        (pcase (jupyter-message-type msg)
          ("execute_reply"
           (cl-destructuring-bind (&key execution_count
                                        user_expressions
                                        payload
                                        &allow-other-keys)
               content
             (jupyter-handle-execute-reply
              client req execution_count user_expressions payload)))
          ("inspect_reply"
           (cl-destructuring-bind (&key found
                                        data
                                        metadata
                                        &allow-other-keys)
               content
             (jupyter-handle-inspect-reply
              client req found data metadata)))
          ("complete_reply"
           (cl-destructuring-bind (&key matches
                                        cursor_start
                                        cursor_end
                                        metadata
                                        &allow-other-keys)
               content
             (jupyter-handle-complete-reply
              client req matches cursor_start cursor_end metadata)))
          ("history_reply"
           (cl-destructuring-bind (&key history &allow-other-keys)
               content
             (jupyter-handle-history-reply client req history)))
          ("is_complete_reply"
           (cl-destructuring-bind (&key status indent &allow-other-keys)
               content
             (jupyter-handle-is-complete-reply client req status indent)))
          ("comm_info_reply"
           (cl-destructuring-bind (&key comms &allow-other-keys)
               content
             (jupyter-handle-comm-info-reply client req comms)))
          ("kernel_info_reply"
           (cl-destructuring-bind (&key protocol_version
                                        implementation
                                        implementation_version
                                        language_info
                                        banner
                                        help_links
                                        &allow-other-keys)
               content
             (jupyter-handle-kernel-info-reply
              client req protocol_version implementation
              implementation_version language_info banner help_links)))
          (_
           (warn "Message type not handled (%s)" (jupyter-message-type msg))))))))

(cl-defgeneric jupyter-execute-request ((client jupyter-kernel-client)
                                        &key code
                                        (silent nil)
                                        (store-history t)
                                        (user-expressions nil)
                                        (allow-stdin t)
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

(cl-defgeneric jupyter-handle-execute-reply ((client jupyter-kernel-client)
                                             req
                                             execution-count
                                             user-expressions
                                             payload)
  "Default execute reply handler."
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

(cl-defgeneric jupyter-handle-inspect-reply ((client jupyter-kernel-client)
                                             req
                                             found
                                             data
                                             metadata)
  "Default inspect reply handler."
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

(cl-defgeneric jupyter-handle-complete-reply ((client jupyter-kernel-client)
                                              req
                                              matches
                                              cursor-start
                                              cursor-end
                                              metadata)
  "Default complete reply handler."
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

(cl-defgeneric jupyter-handle-history-reply ((client jupyter-kernel-client)
                                             req
                                             history)
  "Default history reply handler."
  nil)

(cl-defgeneric jupyter-is-complete-request ((client jupyter-kernel-client)
                                            &key code)
  "Send an is-complete request."
  (declare (indent 1))
  (let ((channel (oref client shell-channel))
        (msg (jupyter-message-is-complete-request
              :code code)))
    (jupyter-send client channel "is_complete_request" msg)))

(cl-defgeneric jupyter-handle-is-complete-reply ((client jupyter-kernel-client)
                                                 req
                                                 status
                                                 indent)
  "Default is complete reply handler."
  nil)

(cl-defgeneric jupyter-comm-info-request ((client jupyter-kernel-client)
                                          &key target-name)
  "Send a comm-info request."
  (declare (indent 1))
  (let ((channel (oref client shell-channel))
        (msg (jupyter-message-comm-info-request
              :target-name target-name)))
    (jupyter-send client channel "comm_info_request" msg)))

(cl-defgeneric jupyter-handle-comm-info-reply ((client jupyter-kernel-client)
                                               req
                                               comms)
  "Default comm info. reply handler."
  nil)

(cl-defgeneric jupyter-kernel-info-request ((client jupyter-kernel-client))
  "Send a kernel-info request."
  (let ((channel (oref client shell-channel))
        (msg (jupyter-message-kernel-info-request)))
    (jupyter-send client channel "kernel_info_request" msg)))

(cl-defgeneric jupyter-handle-kernel-info-reply ((client jupyter-kernel-client)
                                                 req
                                                 protocol-version
                                                 implementation
                                                 implementation-version
                                                 language-info
                                                 banner
                                                 help-links)
  "Default kernel-info reply handler."
  nil)

(cl-defgeneric jupyter-shutdown-request ((client jupyter-kernel-client)
                                         &optional restart)
  "Request a shutdown of CLIENT's kernel.
If RESTART is non-nil, request a restart instead of a complete shutdown."
  (let ((channel (oref client shell-channel))
        (msg (jupyter-message-shutdown-request :restart restart)))
    (jupyter-send client channel "shutdown_request" msg)))

(cl-defgeneric jupyter-handle-shutdown-reply ((client jupyter-kernel-client)
                                              req
                                              restart)
  "Default shutdown reply handler."
  nil)

;;; IOPUB message handlers

(cl-defmethod jupyter-handle-message ((channel jupyter-iopub-channel)
                                      client
                                      req
                                      msg)
  (let ((content (jupyter-message-content msg)))
    (pcase (jupyter-message-type msg)
      ("shutdown_reply"
       (cl-destructuring-bind (&key restart &allow-other-keys)
           content
         (jupyter-handle-shutdown-reply
          client req restart)))
      ("stream"
       (cl-destructuring-bind (&key name text &allow-other-keys)
           content
         (jupyter-handle-stream
          client req name text)))
      ("execute_input"
       (cl-destructuring-bind (&key code execution_count &allow-other-keys)
           content
         (jupyter-handle-execute-input
          client req code execution_count)))
      ("execute_result"
       (cl-destructuring-bind (&key execution_count
                                    data
                                    metadata
                                    &allow-other-keys)
           content
         (jupyter-handle-execute-result
          client req execution_count data metadata)))
      ("error"
       (cl-destructuring-bind (&key ename evalue traceback &allow-other-keys)
           content
         (jupyter-handle-error
          client req ename evalue traceback)))
      ("status"
       (cl-destructuring-bind (&key execution_state &allow-other-keys)
           content
         (jupyter-handle-status
          client req execution_state)))
      ("clear_output"
       (cl-destructuring-bind (&key wait &allow-other-keys)
           content
         (jupyter-handle-clear-output
          client req wait)))
      ("display_data"
       (cl-destructuring-bind (&key data metadata transient &allow-other-keys)
           content
         (jupyter-handle-display-data
          client req data metadata transient)))
      ("update_display_data"
       (cl-destructuring-bind (&key data metadata transient &allow-other-keys)
           content
         (jupyter-handle-update-display-data
          client req data metadata transient)))
      (_
       (warn "Message type not handled (%s)" (jupyter-message-type msg))))))

(cl-defgeneric jupyter-handle-stream ((client jupyter-kernel-client)
                                      req
                                      name
                                      text)
  "Default stream handler."
  nil)

(cl-defgeneric jupyter-handle-execute-input ((client jupyter-kernel-client)
                                             req
                                             code
                                             execution-count)
  "Default execute input handler."
  nil)

(cl-defgeneric jupyter-handle-execute-result ((client jupyter-kernel-client)
                                              req
                                              execution-count
                                              data
                                              metadata)
  "Default execute result handler."
  nil)

(cl-defgeneric jupyter-handle-error ((client jupyter-kernel-client)
                                     req
                                     ename
                                     evalue
                                     traceback)
  "Default error handler."
  nil)

(cl-defgeneric jupyter-handle-status ((client jupyter-kernel-client)
                                      req
                                      execution-state)
  "Default status handler."
  nil)

(cl-defgeneric jupyter-handle-clear-output ((client jupyter-kernel-client)
                                            req
                                            wait)
  "Default clear output handler."
  nil)

(cl-defgeneric jupyter-handle-display-data ((client jupyter-kernel-client)
                                            req
                                            data
                                            metadata
                                            transient)
  "Default display data handler."
  nil)

(cl-defgeneric jupyter-handle-display-data ((client jupyter-kernel-client)
                                            req
                                            data
                                            metadata
                                            transient)
  "Default display data handler."
  nil)

(cl-defgeneric jupyter-handle-update-display-data ((client jupyter-kernel-client)
                                                   req
                                                   data
                                                   metadata
                                                   transient)
  "Default update display handler"
  nil)

(provide 'jupyter-client)

;;; jupyter-client.el ends here
