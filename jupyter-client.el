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

(cl-defmethod jupyter-initialize-connection
    ((client jupyter-kernel-client)
     file-or-plist)
  "Initialize CLIENT with a connection FILE-OR-PLIST.
If FILE-OR-PLIST is a file name, then it is assumed to be a file
containing a JSON dictionary with the necessary keys for
connecting to a Jupyter kernel. This file will be read using
`json-read-file' and the CLIENT's channels initialized using the
connection info read from the plist created from the files
contents. If FILE-OR-PLIST is a plist, then the CLIENT's channels
is initialized from the plist in the same way as described above.
Again, under the assumption that the plist has the necessay keys
for connecting to a Jupyter kernel, see
http://jupyter-client.readthedocs.io/en/latest/kernels.html#connection-files."
  (cl-check-type file-or-plist (or json-plist file-exists))
  (let ((conn-info (if (json-plist-p file-or-plist) file-or-plist
                     (let ((json-array-type 'list)
                           (json-object-type 'plist)
                           (json-false nil))
                       (json-read-file file-or-plist)))))
    (oset client conn-info conn-info)
    (cl-destructuring-bind
        (&key shell_port iopub_port stdin_port hb_port control_port ip
              key transport signature_scheme kernel_name
              &allow-other-keys)
        conn-info
      (when (and (> (length key) 0)
                 (not (functionp (intern signature_scheme))))
        (error "Unsupported signature scheme: %s" signature_scheme))
      ;; Stop the channels if connected to some other kernel
      (jupyter-stop-channels client)
      (let ((addr (concat transport "://" ip)))
        (oset client session (jupyter-session :key key))
        (oset client stdin-channel
              (make-instance
               'jupyter-stdin-channel
               :endpoint (format "%s:%d" addr stdin_port)))
        (oset client shell-channel
              (make-instance
               'jupyter-shell-channel
               :endpoint (format "%s:%d" addr shell_port)))
        (oset client hb-channel
              (make-instance
               'jupyter-hb-channel
               :endpoint (format "%s:%d" addr hb_port)))
        (oset client iopub-channel
              (make-instance
               'jupyter-iopub-channel
               :endpoint (format "%s:%d" addr iopub_port)))))))

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
                     do (zmq-prin1 (cons 'recvd (ring-remove queue)))))
                   (recv-message
                    (sock ctype)
                    (when (= (ring-length queue) (ring-size queue))
                      (send-recvd))
                    (ring-insert queue (cons ctype (jupyter-recv session sock))))
                   (send-message
                    (sock ctype rest)
                    (zmq-prin1
                     (cons 'sent
                           (cons ctype (apply #'jupyter-send session sock rest)))))
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
               (with-zmq-poller
                ;; Also poll for standard-in events to be able to read commands
                ;; from the parent emacs process without blocking
                (zmq-poller-register (current-zmq-poller) 0 zmq-POLLIN)
                (mapc (lambda (x) (zmq-poller-register (current-zmq-poller)
                                               (car x)
                                               zmq-POLLIN))
                   channels)
                (unwind-protect
                    (while t
                      (when (and (= idle-count 2)
                                 (> (ring-length queue) 0))
                        (send-recvd))
                      (let ((events (condition-case err
                                        (zmq-poller-wait-all (current-zmq-poller) 5 20)
                                      (zmq-EINTR nil)
                                      ;; TODO: For any other kind of error,
                                      ;; just reset the polling loop by exiting
                                      ;; `with-zmq-poller'.
                                      (error (signal (car err) (cdr err))))))
                        (if (null events) (setq idle-count (1+ idle-count))
                          (setq idle-count 0)
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
             (quit (zmq-prin1 (cons 'quit (cons nil nil))))))))))

(defun jupyter--ioloop-pop-request (client)
  (let* ((ring (process-get (oref client ioloop) :jupyter-pending-requests))
         (req (ring-remove ring)))
    req))

(defun jupyter--ioloop-push-request (client req)
  (let* ((ioloop (oref client ioloop))
         (ring (or (process-get ioloop :jupyter-pending-requests)
                   (let ((ring (make-ring 10)))
                     (process-put ioloop :jupyter-pending-requests ring)
                     ring))))
    (ring-insert+extend ring req 'grow)))

(defun jupyter--ioloop-sentinel (client ioloop event)
  (cond
   ((cl-loop for type in '("exited" "failed" "finished" "killed" "deleted")
             thereis (string-prefix-p type event))
    (jupyter-stop-channel (oref client hb-channel))
    (oset client ioloop nil))))

(defun jupyter--ioloop-filter (client event)
  (cl-destructuring-bind (ctype . data) (cdr event)
    (cl-case (car event)
      ;; Cleanup handled in sentinel
      (quit)
      (sent
       (when jupyter--debug
         (message "SEND: %s" data))
       (unless (eq ctype :stdin)
         ;; Anything sent on stdin is a reply and therefore never added to
         ;; `:jupyter-pending-requests'
         (let ((id data)
               (req (jupyter--ioloop-pop-request client)))
           (setf (jupyter-request--id req) id)
           (puthash id req (oref client requests)))))
      (recvd
       (when jupyter--debug
         (message "RECV: %s %s %s"
                  (jupyter-message-type (cdr data))
                  (jupyter-message-parent-id (cdr data))
                  (jupyter-message-content (cdr data))))
       (let ((channel (cl-find-if (lambda (c) (eq (oref c type) ctype))
                                  (mapcar (lambda (x) (slot-value client x))
                                     '(stdin-channel
                                       shell-channel
                                       iopub-channel)))))
         (jupyter-push-message channel data)
         (run-with-timer 0.001 nil #'jupyter-handle-message client channel))))))

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
  "Add REQ MSG-TYPE callback, CB."
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
argument, a message whose `jupyter-message-parent-id' is the same
as the `jupyter-request-id' of REQ and whose
`jupyter-message-type' corresponds to the value of MSG-TYPE in
the `jupyter-message-types' plist. Any additional arguments to
`jupyter-add-callback' are interpreted as additional CALLBACKS to
add to REQ. So to add multiple callbacks to a request you would
do

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

(defun jupyter-wait-until (req msg-type fun &optional timeout)
  "Wait until FUN returns non-nil for a received message.
FUN is run on every received message for request, REQ, that has
type, MSG-TYPE. If FUN does not return a non-nil value before
TIMEOUT, return nil. Otherwise return the message which caused
FUN to return a non-nil value. Note that if TIMEOUT is nil, it
defaults to `jupyter-default-timeout'."
  (declare (indent 1))
  (setq timeout (or timeout jupyter-default-timeout))
  (cl-check-type timeout number)
  (lexical-let ((msg nil)
                (fun fun))
    (jupyter-add-callback req
      msg-type (lambda (m) (setq msg (when (funcall fun m) m))))
    (with-timeout (timeout nil)
      (while (null msg)
        (sleep-for 0.01))
      msg)))

(defun jupyter-wait-until-idle (req &optional timeout)
  "Wait until TIMEOUT for REQ to receive an idle message.
If TIMEOUT is non-nil, it defaults to `jupyter-default-timeout'."
  (jupyter-wait-until req :status #'jupyter-message-status-idle-p timeout))

(defun jupyter-wait-until-received (msg-type req &optional timeout)
  "Wait for a message with MSG-TYPE to be received by CLIENT.
This function waits until CLIENT receives a message from the
kernel that satisfies the following conditions:

1. The message has a type of MSG-TYPE
2. The parent header of the message has a message ID of PMSG-ID

Note that MSG-TYPE should be one of the keys found in
`jupyter--recieved-message-types'. If it is not, an error is
raised.

All of the `jupyter-request-*' functions return a message ID that
can be passed to this function as the PMSG-ID. If the message
associated with PMSG-ID is not expecting to receive a message
with MSG-TYPE, this function will wait forever so be sure that
you are expecting to receive a message of a certain type after
sending one. For example you would not be expecting an
`execute-reply' when you send a kernel info request with
`jupyter-request-kernel-info', but you would be expecting a
`kernel-info-reply'. See the jupyter messaging specification for
more info
http://jupyter-client.readthedocs.io/en/latest/messaging.html"
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
              (remhash pmsg-id requests))))))
    (run-with-timer 0.005 nil #'jupyter-handle-message client channel)))

;;; STDIN message requests/handlers

(cl-defmethod jupyter-handle-message ((channel jupyter-stdin-channel)
                                      client
                                      req
                                      msg)
  (cl-destructuring-bind (&key prompt password &allow-other-keys)
      (jupyter-message-content msg)
    (jupyter-handle-input-reply client req prompt password)))

(cl-defmethod jupyter-handle-input-reply ((client jupyter-kernel-client)
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

(cl-defmethod jupyter-handle-message ((channel jupyter-control-channel)
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

(cl-defmethod jupyter-shutdown-request ((client jupyter-kernel-client)
                                        &optional restart)
  "Request a shutdown of CLIENT's kernel.
If RESTART is non-nil, request a restart instead of a complete shutdown."
  (let ((channel (oref client shell-channel))
        (msg (jupyter-message-shutdown-request :restart restart)))
    (jupyter-send client channel "shutdown_request" msg)))

(cl-defmethod jupyter-handle-shutdown-reply ((client jupyter-kernel-client)
                                             req
                                             restart)
  "Default shutdown reply handler.")

;; FIXME: This breaks the convention that all jupyter-request-* functions
;; returns a message-id future object.
;; (cl-defmethod jupyter-request-interrupt ((client jupyter-kernel-client))
;;   ;; TODO: Check for interrupt_mode of the kernel's kernelspec
;;   ;; http://jupyter-client.readthedocs.io/en/latest/messaging.html#kernel-interrupt
;;   (let ((channel (oref client control-channel)))
;;     (jupyter-send client channel "interrupt_request" ())))

(cl-defmethod jupyter-handle-interrupt-reply ((client jupyter-kernel-client)
                                              req)
  "Default interrupt reply handler.")

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
            (_ (error "Message type not handled yet.")))
        ;; FIXME: Do something about errrors here?
        ;; (if (equal status "error")
        ;;     (error "Error (%s): %s"
        ;;            (plist-get content :ename) (plist-get content :evalue))
        ;;   (error "Error: aborted"))
        ))))

(cl-defmethod jupyter-execute-request ((client jupyter-kernel-client)
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

(cl-defmethod jupyter-handle-execute-reply ((client jupyter-kernel-client)
                                            req
                                            execution-count
                                            user-expressions
                                            payload)
  "Default execute reply handler.")

(cl-defmethod jupyter-inspect-request ((client jupyter-kernel-client)
                                       &key code
                                       (pos 0)
                                       (detail 0))
  "Send an inspect request."
  (declare (indent 1))
  (let ((channel (oref client shell-channel))
        (msg (jupyter-message-inspect-request
              :code code :pos pos :detail detail)))
    (jupyter-send client channel "inspect_request" msg)))

(cl-defmethod jupyter-handle-inspect-reply ((client jupyter-kernel-client)
                                            req
                                            found
                                            data
                                            metadata)
  "Default inspect reply handler.")

(cl-defmethod jupyter-complete-request ((client jupyter-kernel-client)
                                        &key code
                                        (pos 0))
  "Send a complete request."
  (declare (indent 1))
  (let ((channel (oref client shell-channel))
        (msg (jupyter-message-complete-request
              :code code :pos pos)))
    (jupyter-send client channel "complete_request" msg)))

(cl-defmethod jupyter-handle-complete-reply ((client jupyter-kernel-client)
                                             req
                                             matches
                                             cursor-start
                                             cursor-end
                                             metadata)
  "Default complete reply handler.")

(cl-defmethod jupyter-history-request ((client jupyter-kernel-client)
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

(cl-defmethod jupyter-handle-history-reply ((client jupyter-kernel-client)
                                            req
                                            history)
  "Default history reply handler.")

(cl-defmethod jupyter-is-complete-request ((client jupyter-kernel-client)
                                           &key code)
  "Send an is-complete request."
  (declare (indent 1))
  (let ((channel (oref client shell-channel))
        (msg (jupyter-message-is-complete-request
              :code code)))
    (jupyter-send client channel "is_complete_request" msg)))

(cl-defmethod jupyter-handle-is-complete-reply ((client jupyter-kernel-client)
                                                req
                                                status
                                                indent)
  "Default is complete reply handler.")

(cl-defmethod jupyter-comm-info-request ((client jupyter-kernel-client)
                                         &key target-name)
  "Send a comm-info request."
  (declare (indent 1))
  (let ((channel (oref client shell-channel))
        (msg (jupyter-message-comm-info-request
              :target-name target-name)))
    (jupyter-send client channel "comm_info_request" msg)))

(cl-defmethod jupyter-handle-comm-info-reply ((client jupyter-kernel-client)
                                              req
                                              comms)
  "Default comm info. reply handler.")

(cl-defmethod jupyter-kernel-info-request ((client jupyter-kernel-client))
  "Send a kernel-info request."
  (let ((channel (oref client shell-channel))
        (msg (jupyter-message-kernel-info-request)))
    (jupyter-send client channel "kernel_info_request" msg)))

(cl-defmethod jupyter-handle-kernel-info-reply ((client jupyter-kernel-client)
                                                req
                                                protocol-version
                                                implementation
                                                implementation-version
                                                language-info
                                                banner
                                                help-links)
  "Default kernel-info reply handler.")

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
      (_ (error "Message type not handled yet.")))))

(cl-defmethod jupyter-handle-stream ((client jupyter-kernel-client)
                                     req
                                     name
                                     text)
  "Default stream handler.")

(cl-defmethod jupyter-handle-execute-input ((client jupyter-kernel-client)
                                            req
                                            code
                                            execution-count)
  "Default execute input handler.")

(cl-defmethod jupyter-handle-execute-result ((client jupyter-kernel-client)
                                             req
                                             execution-count
                                             data
                                             metadata)
  "Default execute result handler.")

(cl-defmethod jupyter-handle-error ((client jupyter-kernel-client)
                                    req
                                    ename
                                    evalue
                                    traceback)
  "Default error handler.")

(cl-defmethod jupyter-handle-status ((client jupyter-kernel-client)
                                     req
                                     execution-state)
  "Default status handler.")

(cl-defmethod jupyter-handle-clear-output ((client jupyter-kernel-client)
                                           req
                                           wait)
  "Default clear output handler.")

(cl-defmethod jupyter-handle-display-data ((client jupyter-kernel-client)
                                           req
                                           data
                                           metadata
                                           transient)
  "Default display data handler.")

(cl-defmethod jupyter-handle-update-display-data ((client jupyter-kernel-client)
                                                  req
                                                  data
                                                  metadata
                                                  transient)
  "Default update display data handler.")

(provide 'jupyter-client)
