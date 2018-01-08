(require 'jupyter-base)
(require 'jupyter-connection)
(require 'jupyter-channels)
(require 'jupyter-messages)
(eval-when-compile (require 'cl))

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
   (shell-channel
    :type (or null jupyter-shell-channel)
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
                     (oref client conn-info))))
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
      (let ((addr (concat transport "://" ip)))
        ;; A kernel manager may have already initialized the session, see
        ;; `jupyter-make-client'
        (unless (and (slot-boundp client 'session)
                     (oref client session)
                     (equal (jupyter-session-key (oref client session)) key))
          (oset client session (jupyter-session :key key)))
        (cl-loop
         for (channel . port) in (list (cons 'stdin-channel stdin_port)
                                       (cons 'shell-channel shell_port)
                                       (cons 'hb-channel hb_port)
                                       (cons 'iopub-channel iopub_port))
         for class = (intern (concat "jupyter-" (symbol-name channel)))
         do (setf (slot-value client channel)
                  (make-instance
                   class
                   ;; So channels have access to the client's session
                   :parent-instance client
                   :endpoint (format "%s:%d" addr port))))))))

;;; Lower level sending/receiving

(cl-defmethod jupyter-send ((client jupyter-kernel-client)
                            channel
                            type
                            message
                            &optional flags)
  "Encode MESSAGE and send it on CLIENT's CHANNEL.
The message should have a TYPE corresponding to one of those
found in the jupyter messaging protocol. Optional variable FLAGS
are the flags sent to the underlying `zmq-send-multipart' call
using the CHANNEL's socket."
  (declare (indent 1))
  (zmq-subprocess-send (oref client ioloop)
    (list 'send (oref channel type) type message flags))
  ;; Anything sent to stdin is a reply not a request so don't add it to
  ;; `:jupyter-pending-requests.'
  (unless (eq (oref channel type) :stdin)
    (let ((req (make-jupyter-request)))
      (jupyter--ioloop-push-request client req)
      req)))

(defun jupyter--ioloop (client)
  (let ((iopub-channel (oref client iopub-channel))
        (shell-channel (oref client shell-channel))
        (stdin-channel (oref client stdin-channel)))
    `(lambda (ctx)
       (require 'jupyter-channels ,(locate-library "jupyter-channels"))
       (require 'jupyter-messages ,(locate-library "jupyter-messages"))
       ;; We can splice the session object because it contains primitive types
       (let* ((session ,(oref client session))
              (iopub
               (let ((sock (jupyter-connect-channel
                            :iopub ,(oref (oref client iopub-channel) endpoint)
                            (jupyter-session-id session))))
                 (zmq-socket-set sock zmq-SUBSCRIBE "")
                 sock))
              (shell
               (jupyter-connect-channel
                :shell ,(oref (oref client shell-channel) endpoint)
                (jupyter-session-id session)))
              (stdin
               (jupyter-connect-channel
                :stdin ,(oref (oref client stdin-channel) endpoint)
                (jupyter-session-id session)))
              (channels (list (cons stdin :stdin)
                              (cons shell :shell)
                              (cons iopub :iopub)))
              (priorities (list (cons :shell 4)
                                (cons :iopub 2)
                                (cons :stdin 2)))
              (idle-count 0)
              (timeout 20)
              (queue (make-ring 10)))
         (cl-flet ((send-recvd
                    ()
                    ;; Try and have a consistent order with which messages are
                    ;; received in the parent process. We queue messages
                    ;; received from the kernel and send them up to the parent
                    ;; process only when (1) no messages have been received in
                    ;; two polling periods or (2) when the queue is filled.
                    ;; When sending, the messages are sorted by their send time
                    ;; using the `:date' field of the message `:header'. In the
                    ;; case that two messages have the same send time from the
                    ;; kernel (i.e. when they don't have fractional resolution)
                    ;; the messages are sorted by channel priority.
                    (cl-sort (cddr queue)
                             ;; [<sorted non-nil elements> nil nil ...]
                             (lambda (a b)
                               (cond
                                ((and (eq a nil) (eq a b)) t)
                                ((eq a nil) nil)
                                ((eq b nil) t)
                                (t
                                 ;; elements are (ctype idents . msg)
                                 (let ((ta (jupyter-message-time (cddr a)))
                                       (tb (jupyter-message-time (cddr b))))
                                   (or (time-less-p ta tb)
                                       (when (equal ta tb)
                                         (> (alist-get (car a) priorities)
                                            (alist-get (car b) priorities)))))))))
                    (cl-loop
                     while (not (ring-empty-p queue))
                     for (ctype . msg) = (ring-remove queue)
                     do (zmq-prin1 (list 'recvd ctype msg))))
                   (recv-message
                    (sock ctype)
                    (when (= (ring-length queue) (ring-size queue))
                      (send-recvd))
                    (ring-insert queue (cons ctype (jupyter-recv session sock))))
                   (send-message
                    (sock ctype rest)
                    (zmq-prin1
                     (list 'sent ctype (apply #'jupyter-send session sock rest))))
                   (start-channel
                    (sock)
                    (zmq-connect
                     sock (zmq-socket-get sock zmq-LAST_ENDPOINT))
                    (zmq-poller-register
                     (current-zmq-poller) sock zmq-POLLIN))
                   (stop-channel
                    (sock)
                    (zmq-poller-unregister (current-zmq-poller) sock)
                    (condition-case err
                        (zmq-disconnect
                         sock (zmq-socket-get sock zmq-LAST_ENDPOINT))
                      (zmq-ENOENT nil)
                      (error (signal (car err) (cdr err))))))
           (condition-case nil
               (with-zmq-poller poller
                 ;; Also poll for standard-in events to be able to read commands
                 ;; from the parent emacs process without blocking
                 (zmq-poller-register poller 0 zmq-POLLIN)
                 (mapc (lambda (x) (zmq-poller-register
                            poller (car x) zmq-POLLIN))
                    channels)
                 (unwind-protect
                     (while t
                       (when (and (= idle-count 2)
                                  (> (ring-length queue) 0))
                         (send-recvd))
                       (let ((events (condition-case err
                                         (zmq-poller-wait-all poller 5 timeout)
                                       (zmq-EINTR nil)
                                       ;; TODO: For any other kind of error,
                                       ;; just reset the polling loop by exiting
                                       ;; `with-zmq-poller'.
                                       (error (signal (car err) (cdr err))))))
                         (if (null events)
                             (progn (setq idle-count (1+ idle-count))
                                    (when (= idle-count 50)
                                      (setq timeout 100)))
                           (setq idle-count 0
                                 timeout 20)
                           (when (alist-get 0 events)
                             (setf (alist-get 0 events nil 'remove) nil)
                             (cl-destructuring-bind (cmd . data)
                                 (zmq-subprocess-read)
                               (cl-case cmd
                                 (quit
                                  (signal 'quit nil))
                                 (start-channel
                                  (let* ((ctype data)
                                         (sock (car (rassoc ctype channels))))
                                    (start-channel sock)))
                                 (stop-channel
                                  (let* ((ctype data)
                                         (sock (car (rassoc ctype channels))))
                                    (stop-channel sock)))
                                 (send
                                  (let* ((ctype (car data))
                                         (sock (car (rassoc ctype channels)))
                                         (rest (cdr data)))
                                    (send-message sock ctype rest))))))
                           (cl-loop for (sock . event) in events
                                    do (recv-message
                                        sock (alist-get sock channels))))))
                   (mapc (lambda (s)
                        (zmq-socket-set s zmq-LINGER 0)
                        (zmq-close s))
                      (mapcar #'car channels))))
             (quit (zmq-prin1 (list 'quit)))))))))

(defun jupyter--ioloop-pop-request (client)
  "Remove a pending request from CLIENT's ioloop subprocess.
Specifically remove the oldest element of the ring located in the
`:jupyter-pending-requests' property of CLIENT's ioloop
subprocess."
  (let* ((ring (process-get (oref client ioloop) :jupyter-pending-requests))
         (req (ring-remove ring)))
    req))

(defun jupyter--ioloop-push-request (client req)
  "Insert a request into CLIENT's pending requests.
REQ is inserted as the newest element in CLIENT's pending
requests. See `jupyter--ioloop-pop-request' for where pending
requests are stored for CLIENT."
  (let* ((ioloop (oref client ioloop))
         (ring (or (process-get ioloop :jupyter-pending-requests)
                   (let ((ring (make-ring 10)))
                     (process-put ioloop :jupyter-pending-requests ring)
                     ring))))
    (ring-insert+extend ring req 'grow)))

(defun jupyter--ioloop-sentinel (client event)
  "The process sentinel for CLIENT's IOLOOP subprocess.
When EVENT is one of the events signifying that the process is
dead, stop the heartbeat channel and set the IOLOOP slot to nil
in CLIENT."
  (cond
   ((cl-loop for type in '("exited" "failed" "finished" "killed" "deleted")
             thereis (string-prefix-p type event))
    (jupyter-stop-channel (oref client hb-channel))
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
    (`(recvd ,ctype ,msg)
     (when jupyter--debug
       (message "RECV: %s %s %s"
                (jupyter-message-type (cdr msg))
                (jupyter-message-parent-id (cdr msg))
                (jupyter-message-content (cdr msg))))
     (let ((channel (cl-find-if (lambda (c) (eq (oref c type) ctype))
                                (mapcar (lambda (x) (slot-value client x))
                                   '(stdin-channel
                                     shell-channel
                                     iopub-channel)))))
       (jupyter-push-message channel msg)
       (run-with-timer 0.001 nil #'jupyter-handle-message client channel)))
    ('(quit)
     ;; Cleanup handled in sentinel
     (when jupyter--debug
       (message "KERNEL KILLED")))))

(cl-defmethod jupyter-start-channels ((client jupyter-kernel-client)
                                      &key (shell t)
                                      (iopub t)
                                      (stdin t)
                                      (control t)
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
    (when hb (jupyter-start-channel (oref client hb-channel)))
    (oset client ioloop
          (zmq-start-process
           (jupyter--ioloop client)
           (apply-partially #'jupyter--ioloop-filter client)
           (apply-partially #'jupyter--ioloop-sentinel client)))))

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
to one of the keys in `jupyter-message-types'. MSG-TYPE can also
be a list, in which case run CB for every MSG-TYPE in the list.
If MSG-TYPE is t, then run CB for every message received for REQ.
CB is the callback function which will run with a single
argument, a message whose `jupyter-message-parent-id' is `equal'
to the `jupyter-request-id' of REQ and whose
`jupyter-message-type' corresponds to the value of MSG-TYPE in
`jupyter-message-types'.

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
`jupyter-add-callback'. If CB returns a non-nil within TIMEOUT
seconds, return the message that caused CB to return non-nil. If
CB never returns a non-nil value within TIMEOUT, return nil. Note
that if no TIMEOUT is given, `jupyter-default-timeout' is used."
  (declare (indent 1))
  (setq timeout (or timeout jupyter-default-timeout))
  (cl-check-type timeout number)
  (lexical-let ((msg nil)
                (cb cb))
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
   - Cleanup request when kernel is done processing it"
  (when (jupyter-messages-available-p channel)
    (let* ((msg (jupyter-get-message channel))
           (pmsg-id (jupyter-message-parent-id msg))
           (requests (oref client requests))
           (req (gethash pmsg-id requests)))
      (if (not req)
          ;; Always run handlers of IOPub messages, even when they are not
          ;; associated with any request that was sent by us.
          ;;
          ;; TODO: Would we always want this?
          (when (eq (oref channel type) :iopub)
            (jupyter-handle-message channel client nil msg))
        (unwind-protect
            (jupyter--run-callbacks req msg)
          (unwind-protect
              (when (jupyter-request-run-handlers-p req)
                (jupyter-handle-message channel client req msg))
            (when (jupyter-message-status-idle-p msg)
              (setf (jupyter-request-idle-received-p req) t)
              ;; TODO: Messages associated with the request might still be
              ;; received when the request is removed from the requests table.
              (remhash pmsg-id requests))))))))

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

;;; CONTROL message requests/handlers

(cl-defgeneric jupyter-handle-message ((channel jupyter-control-channel)
                                       client
                                       req
                                       msg)
  (cl-destructuring-bind (&key status ename evalue &allow-other-keys)
      (jupyter-message-content msg)
    (if (equal status "ok")
        ;; FIXME: An interrupt reply is only sent when interrupt_mode is set
        ;; to message in a kernel's kernelspec.
        (pcase (jupyter-message-type msg)
          ("interrupt_reply"
           (jupyter-handle-interrupt-reply client req)))
      ;; FIXME: How to handle errors more generally? Just let the IOPub message
      ;; handle it?
      (if (equal status "error") (error "Error (%s): %s" ename evalue)
        (error "Error: aborted")))))

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

;; FIXME: This breaks the convention that all jupyter-request-* functions
;; returns a message-id future object.
;; (cl-defmethod jupyter-request-interrupt ((client jupyter-kernel-client))
;;   ;; TODO: Check for interrupt_mode of the kernel's kernelspec
;;   ;; http://jupyter-client.readthedocs.io/en/latest/messaging.html#kernel-interrupt
;;   (let ((channel (oref client control-channel)))
;;     (jupyter-send client channel "interrupt_request" ())))

(cl-defgeneric jupyter-handle-interrupt-reply ((client jupyter-kernel-client)
                                               req)
  "Default interrupt reply handler."
  nil)

;;; SHELL message requests/handlers

;; http://jupyter-client.readthedocs.io/en/latest/messaging.html#messages-on-the-shell-router-dealer-channel
(cl-defmethod jupyter-handle-message ((channel jupyter-shell-channel)
                                      client
                                      req
                                      msg)
  (let ((content (jupyter-message-content msg)))
    (cl-destructuring-bind (&key status ename evalue &allow-other-keys) content
      ;; is_complete_reply messages have a status other than "ok" so just
      ;; ensure that the status does not correspond to an error.
      (if (not (member status '("error" "abort")))
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
            (_ (error "Message type not handled yet")))
        ;; FIXME: Do something about errrors here?
        ;; (if (equal status "error")
        ;;     (error "Error (%s): %s"
        ;;            (plist-get content :ename) (plist-get content :evalue))
        ;;   (error "Error: aborted"))
        ))))

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
      (_ (error "Message type not handled yet")))))

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
