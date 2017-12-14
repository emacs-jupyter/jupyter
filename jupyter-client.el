(require 'json)
(require 'zmq)
(eval-when-compile (require 'cl))
(require 'jupyter-channels)
(require 'jupyter-messages)

(declare-function string-trim-right "subr-x" (str))

;;; Kernel client class

;; TODO: Sometimes I am not receiving any polling messages from the
;; `zmq-ioloop' polling channels. Why is that? I may send some messages and
;; then get some replies, but then it seems that after I send a message, I
;; never get any reply back.

(defclass jupyter-kernel-client ()
  ;; TODO: start local kernel process or populate with kernel connection info
  ((kernel
    :type (or null process)
    :initform nil
    :documentation "The local kernel process or nil if there is no local kernel.")
   (message-callbacks
    :type hash-table
    :initform (make-hash-table :weakness 'key :test 'equal)
    :documentation "A hash table with message ID's as keys. This
 is used to register callback functions to run once a reply from
 a previously sent request is received. See
 `jupyter-add-receive-callback'.")
   (session
    :type jupyter-session
    :initarg :session
    :documentation "The `jupyter-session' object which holds the
 key for authenticating messages. It also holds the unique
 session identification for this client.")
   (shell-channel
    :type jupyter-shell-channel
    :initarg :shell-channel
    :documentation "The shell channel.")
   (control-channel
    :type jupyter-control-channel
    :initarg :control-channel
    :documentation "The control channel.")
   (iopub-channel
    :type jupyter-iopub-channel
    :initarg :iopub-channel
    :documentation "The IOPub channel.")
   (hb-channel
    :type jupyter-hb-channel
    :initarg :hb-channel
    :documentation "The heartbeat channel.")
   (stdin-channel
    :type jupyter-stdin-channel
    :initarg :stdin-channel
    :documentation "The stdin channel.")))

(defun jupyter-kernel-client-from-connection-file (file)
  "Read a connection FILE and return a `jupyter-kernel-client'.
The connection file should have the same form as those found in
in the jupyter runtime directory."
  (cl-destructuring-bind
      (&key shell_port iopub_port stdin_port hb_port control_port ip
            key transport signature_scheme kernel_name
            &allow-other-keys)
      (let ((json-array-type 'list)
            (json-object-type 'plist))
        (json-read-file file))
    (when (and (> (length key) 0)
               (not (functionp (intern signature_scheme))))
      (error "Unsupported signature scheme: %s" signature_scheme))
    (let ((addr (concat transport "://" ip)))
      (jupyter-kernel-client
       :session (jupyter-session :key key)
       :stdin-channel (jupyter-stdin-channel
                       :endpoint (format "%s:%d" addr stdin_port))
       :shell-channel (jupyter-shell-channel
                       :endpoint (format "%s:%d" addr shell_port))
       :control-channel (jupyter-control-channel
                         :endpoint (format "%s:%d" addr control_port))
       :hb-channel (jupyter-hb-channel
                    :endpoint (format "%s:%d" addr hb_port))
       :iopub-channel (jupyter-iopub-channel
                       :endpoint (format "%s:%d" addr iopub_port))))))

(defun jupyter-start-kernel (name &rest args)
  (let ((buf (get-buffer-create (format "*jupyter-kernel-%s*" name))))
    ;; NOTE: `start-file-process' would start the process on a remote host if
    ;; `default-directory' was a remote directory.
    (apply #'start-process
           (format "jupyter-kernel-%s" name) buf
           "jupyter" "console" "--simple-prompt" args)))

(defun jupyter-kernel-client-using-kernel (name)
  ;; TODO;: kernel existence
  (let* ((proc (jupyter-start-kernel name "--kernel" name))
         (path (expand-file-name
                ;; Default connection file name
                (format "kernel-%d.json" (process-id proc))
                (string-trim-right
                 (shell-command-to-string "jupyter --runtime-dir")))))
    (while (not (file-exists-p path))
      (sleep-for 0 10))
    (let ((client (jupyter-kernel-client-from-connection-file path)))
      (set-process-query-on-exit-flag proc nil)
      (oset client kernel proc)
      client)))

(defun jupyter--ioloop-callback (client channel)
  (let* ((ring (oref channel recv-queue)))
    ;; TODO: How many messages does ZMQ store in its internal buffers before it
    ;; starts droping messages? And what socket option can be examined to
    ;; figure this out?
    (unless (= (ring-length ring) (ring-size ring))
      (let* ((res (jupyter--recv-decoded client channel)))
        (ring-insert ring res)
        (run-with-timer 0.01 nil #'jupyter--process-message client channel)))))

(cl-defmethod jupyter-start-channels ((client jupyter-kernel-client)
                                      &key (shell t)
                                      (iopub t)
                                      (stdin t)
                                      (hb t))
  (cl-loop
   with channel = nil
   for (cname . start) in (list (cons 'shell-channel shell)
                                (cons 'iopub-channel iopub)
                                (cons 'hb-channel hb)
                                (cons 'stdin-channel stdin))
   when start
   do (setq channel (eieio-oref client cname))
   and unless (jupyter-channel-alive-p channel)
   do (jupyter-start-channel
       channel :identity (jupyter-session-id (oref client session)))
   (unless (eq (oref channel type) :hb)
     (lexical-let ((channel channel)
                   (client client))
       (zmq-ioloop
        (oref channel socket)
        (function
         (lambda () (jupyter--ioloop-callback client channel))))))))

(cl-defmethod jupyter-stop-channels ((client jupyter-kernel-client))
  "Stop any running channels of CLIENT."
  (cl-loop
   for channel in (mapcar (lambda (c) (eieio-oref client c))
                     (list 'shell-channel
                           'iopub-channel
                           'hb-channel
                           'control-channel
                           'stdin-channel))
   when (jupyter-channel-alive-p channel)
   ;; hb channels create their sockets in a subprocess which gets stopped in
   ;; `jupyter-channel-stop'
   do (unless (eq (oref channel type) :hb)
        (zmq-stop-ioloop (oref channel socket)))
   (jupyter-stop-channel channel)))

(cl-defmethod jupyter-channels-running ((client jupyter-kernel-client))
  "Are any of the channels of CLIENT created and running?"
  (cl-loop
   for channel in (list 'shell-channel
                        'iopub-channel
                        'hb-channel
                        'control-channel
                        'stdin-channel)
   if (jupyter-channel-alive-p (eieio-oref client channel))
   return t))

;;; Lower level sending/receiving

(cl-defmethod jupyter--send ((channel jupyter-channel) parts &optional flags)
  "Send a message with PARTS on the socket associated with CHANNEL.
Optional variable FLAGS, are the flags argument of the `zmq-send'
or `zmq-send-multipart' call used to send parts."
  (declare (indent 1))
  (let ((sock (oref channel socket)))
    (if (listp parts)
        (zmq-send-multipart sock parts flags)
      (zmq-send sock parts flags))))

(cl-defmethod jupyter--send-encoded ((client jupyter-kernel-client)
                                     channel
                                     type
                                     message
                                     &optional flags)
  "Encode MESSAGE and send it on CLIENT's CHANNEL.
The message should have a TYPE as found in the jupyter messaging
protocol. Optional variable FLAGS are the flags sent to the
underlying `zmq-send-multipart' call using the CHANNEL's socket."
  (declare (indent 1))
  (unless (jupyter-channel-alive-p channel)
    (error "Channel not alive: %s" (oref channel type)))
  (cl-destructuring-bind (msg-id . msg)
      (jupyter--encode-message (oref client session) type :content message)
    (jupyter--send channel msg flags)
    msg-id))

(cl-defmethod jupyter--recv ((channel jupyter-channel) &optional flags)
  "Recieve a message on CHANNEL.
Optional variable FLAGS is the flags argument of the
`zmq-recv-multipart' call using the CHANNEL's socket. The return
value is a cons cell, (IDENTS . PARTS), where IDENTS are the
routing identities of the message and PARTS are the message
parts. See
http://jupyter-client.readthedocs.io/en/latest/messaging.html#the-wire-protocol
for more details."
  (jupyter--split-identities (zmq-recv-multipart (oref channel socket) flags)))

;; TODO: Maybe instead of decoding the message directly, use `apply-partially'
;; to delay decoding until the message is actually handled registered with
;; `jupyter-add-receive-callback' or in some subclass.
(cl-defmethod jupyter--recv-decoded ((client jupyter-kernel-client) channel &optional flags)
  (cl-destructuring-bind (idents . parts)
      (jupyter--recv channel flags)
    (cons idents (jupyter--decode-message (oref client session) parts))))

;;; Processing messages

;; TODO: Override `jupyter-kernel-client' to hook into receiving these
;; messages. How would an `ob-jupyter' client do this?
(defconst jupyter--recieved-message-types
  (list 'execute-result "execute_result"
        'execute-reply "execute_reply"
        'inspect-reply "inspect_reply"
        'complete-reply "complete_reply"
        'history-reply "history_reply"
        'is-complete-reply "is_complete_reply"
        'comm-info-reply "comm_info_reply"
        'kernel-info-reply "kernel_info_reply"
        'shutdown-reply "shutdown_reply"
        'interrupt-reply "interrupt_reply"
        'stream "stream"
        'display-data "display_data"
        'update-display-data "update_display_data"
        'execute-input "execute_input"
        'error "error"
        'status "status"
        'clear-output "clear_output"
        'input-reply "input_reply")
  "A plist mapping symbols to received message types.
This is used to give some type of protection against invalid
message types in `jupyter-add-receive-callback'. If the MSG-TYPE
argument does not match one of the keys in this plist, an error
is thrown.")

(defun jupyter-add-receive-callback (client msg-type msg-id function)
  "Add FUNCTION to run when receiving a message reply.

MSG-ID corresponds to the ID of one a message type which expects
a reply. Whenever CLIENT receives a reply message whose parent
header has an ID that matches MSG-ID FUNCTION, along with any
other registered functions for MSG-ID, will be executed."
  (declare (indent 3))
  (cl-check-type client jupyter-kernel-client)
  (let ((mt (plist-get jupyter--recieved-message-types msg-type)))
    (if mt (setq msg-type mt)
      (error "Not a valid message type (`%s')" msg-type)))
  (let* ((message-callbacks (oref client message-callbacks))
         (callbacks (gethash msg-id message-callbacks)))
    (if (not callbacks)
        (puthash msg-id (list (cons msg-type function)) message-callbacks)
      (let ((cb-for-type (assoc msg-type callbacks)))
        (if cb-for-type (setcdr cb-for-type function)
          (nconc callbacks (list (cons msg-type function))))))))

(defun jupyter-wait-until-received (client msg-type pmsg-id)
  "Wait for a message with MSG-TYPE to be received on CLIENT.
This function waits until CLIENT receives a message from the
kernel that satisfies the following conditions:

1. The message has a type of MSG-TYPE
2. The parent header of the message has a message ID of PMSG-ID

Note that MSG-TYPE should be one of the keys found in
`jupyter--recieved-message-types'. If it is not, an error is
raised.

All of the `jupyter-send-*' functions return a message ID that
can be passed to this function as the PMSG-ID. If the message
associated with PMSG-ID is not expecting to receive a message
with MSG-TYPE, this function will wait forever so be sure that
you are expecting to receive a message of a certain type after
sending one. For example you would not be expecting an
`execute-reply' when you send a kernel info request with
`jupyter-send-kernel-info', but you would be expecting a
`kernel-info-reply'. See the jupyter messaging specification for
more info
http://jupyter-client.readthedocs.io/en/latest/messaging.html"
  (declare (indent 2))
  (lexical-let ((msg nil))
    (jupyter-add-receive-callback client msg-type pmsg-id
      (lambda (m) (setq msg m)))
    (while (null msg)
      (sleep-for 0 10))
    msg))

(defun jupyter--process-message (client channel)
  "Process a message on CLIENT's CHANNEL.
This function processes a single message on CLIENT's CHANNEL and
schedules to process more messages at a future time until
CHANNEL's recv-queue is empty."
  (cl-check-type channel jupyter-channel)
  (let ((ring (oref channel recv-queue)))
    (unless (ring-empty-p ring)
      (let* ((ctype (oref channel type))
             (handler (cl-case ctype
                        (:stdin #'jupyter--handle-stdin-message)
                        (:iopub #'jupyter--handle-iopub-message)
                        (:shell #'jupyter--handle-shell-message)
                        (:control #'jupyter--handle-control-message)
                        (otherwise (error "Wrong channel type (%s)." ctype))))
             ;; Messages are stored like (idents . msg) in the ring
             (msg (cdr (ring-remove ring))))
        (unwind-protect
            (funcall handler client msg)
          ;; Run the message callback stored with `jupyter-run-when-received'.
          ;; Currently this is only used for shell messages since those are the
          ;; only ones that send a reply. Note that IOPub messages will have
          ;; the same parent header as the execute_reply which is why we need
          ;; to check the channel type. If we didn't check the channel type,
          ;; the callback would run on the IOPub messages.
          (let* ((message-callbacks (oref client message-callbacks))
                 (pmsg-id (plist-get (plist-get msg :parent_header) :msg_id))
                 (msg-type (plist-get msg :msg_type))
                 (callbacks (gethash pmsg-id message-callbacks))
                 (cb (cdr (and callbacks (assoc msg-type callbacks)))))
            (when cb
              (unwind-protect
                  (funcall cb msg))))))
      (run-with-timer 0.01 nil #'jupyter--process-message client channel))))

;;; Message handlers

;;; stdin messages

(defun jupyter--handle-stdin-message (client msg)
  (cl-check-type client jupyter-kernel-client)
  (cl-destructuring-bind (&key prompt password &allow-other-keys)
      (plist-get msg :content)
    (jupyter-handle-input client prompt password)))

(cl-defmethod jupyter-handle-input ((client jupyter-kernel-client) prompt password)
  (let ((channel (oref client stdin-channel))
        (msg (jupyter-input-reply
              :value (funcall (if password #'read-passwd
                                #'read-from-minibuffer)
                              prompt))))
    ;; TODO: Check for 'allow_stdin'
    ;; http://jupyter-client.readthedocs.io/en/latest/messaging.html#stdin-messages
    (jupyter--send-encoded client channel "input_reply" msg)))

;;; control messages

(defun jupyter--handle-control-message (client msg)
  (cl-check-type client jupyter-kernel-client)
  (cl-destructuring-bind (&key msg_type content &allow-other-keys) msg
    (pcase msg_type
      ("shutdown_reply"
       (cl-destructuring-bind (&key restart &allow-other-keys)
           content
         (jupyter-handle-shutdown client restart)))
      ("interrupt_reply"
       (jupyter-handle-interrupt client)))))

(cl-defmethod jupyter-send-shutdown ((client jupyter-kernel-client) &optional restart)
  "Request a shutdown of the kernel CLIENT is communicating with.
If RESTART is non-nil, request a restart instead of a complete shutdown."
  (let ((channel (oref client control-channel))
        (msg (jupyter-shutdown-request :restart restart)))
    (jupyter--send-encoded client channel "shutdown_request" msg)))

(cl-defmethod jupyter-handle-shutdown ((client jupyter-kernel-client) restart)
  "Default shutdown reply handler.")

(cl-defmethod jupyter-send-interrupt ((client jupyter-kernel-client))
  (let ((channel (oref client control-channel)))
    (jupyter--send-encoded client channel "interrupt_request" ())))

(cl-defmethod jupyter-handle-interrupt ((client jupyter-kernel-client))
  "Default interrupt reply handler.")

;;; shell messages

;; http://jupyter-client.readthedocs.io/en/latest/messaging.html#messages-on-the-shell-router-dealer-channel
(defun jupyter--handle-shell-message (client msg)
  (cl-check-type client jupyter-kernel-client)
  (cl-destructuring-bind (&key msg_type content &allow-other-keys) msg
    (let ((status (plist-get content :status)))
      (if (equal status "ok")
          (pcase msg_type
            ("execute_reply"
             (cl-destructuring-bind (&key execution_count
                                          user_expressions
                                          payload
                                          &allow-other-keys)
                 content
               (message "execute-reply: %s" (plist-get (plist-get msg :parent_header)
                                                       :msg_id))
               (jupyter-handle-execute
                client execution_count user_expressions payload)))
            ("inspect_reply"
             (cl-destructuring-bind (&key found
                                          data
                                          metadata
                                          &allow-other-keys)
                 content
               (jupyter-handle-inspect
                client found data metadata)))
            ("complete_reply"
             (cl-destructuring-bind (&key matches
                                          cursor_start
                                          cursor_end
                                          metadata
                                          &allow-other-keys)
                 content
               (jupyter-handle-complete
                client matches cursor_start cursor_end metadata)))
            ("history_reply"
             (cl-destructuring-bind (&key history &allow-other-keys)
                 content
               (jupyter-handle-history client history)))
            ("is_complete_reply"
             (cl-destructuring-bind (&key status indent &allow-other-keys)
                 content
               (jupyter-handle-is-complete client status indent)))
            ("comm_info_reply"
             (cl-destructuring-bind (&key comms &allow-other-keys)
                 content
               (jupyter-handle-comm-info client comms)))
            ("kernel_info_reply"
             (cl-destructuring-bind (&key protocol_version
                                          implementation
                                          implementation_version
                                          language_info
                                          banner
                                          help_links
                                          &allow-other-keys)
                 content
               (jupyter-handle-kernel-info
                client protocol_version implementation implementation_version
                language_info banner help_links)))
            (_ (error "Message type not handled yet.")))
        (if (equal status "error")
            (error "Error (%s): %s"
                   (plist-get content :ename) (plist-get content :evalue))
          (error "Error: aborted"))))))

(cl-defmethod jupyter-send-execute ((client jupyter-kernel-client)
                                    &key code
                                    (silent nil)
                                    (store-history t)
                                    (user-expressions nil)
                                    (allow-stdin t)
                                    (stop-on-error nil))
  (declare (indent 1))
  (let ((channel (oref client shell-channel))
        (msg (jupyter-execute-request
              :code code
              :silent silent
              :store-history store-history
              :user-expressions user-expressions
              :allow-stdin allow-stdin
              :stop-on-error stop-on-error)))
    (jupyter--send-encoded client channel "execute_request" msg)))

(cl-defmethod jupyter-handle-execute ((client jupyter-kernel-client)
                                      execution-count
                                      user-expressions
                                      payload)
  "Default execute reply handler.")

(cl-defmethod jupyter-send-inspect ((client jupyter-kernel-client)
                                    &key code
                                    (pos 0)
                                    (detail 0))
  (declare (indent 1))
  (let ((channel (oref client shell-channel))
        (msg (jupyter-inspect-request
              :code code :pos pos :detail detail)))
    (jupyter--send-encoded client channel "inspect_request" msg)))

(cl-defmethod jupyter-handle-inspect ((client jupyter-kernel-client)
                                      found
                                      data
                                      metadata)
  "Default inspect reply handler.")

(cl-defmethod jupyter-send-complete ((client jupyter-kernel-client)
                                     &key code
                                     (pos 0))
  (declare (indent 1))
  (let ((channel (oref client shell-channel))
        (msg (jupyter-complete-request
              :code code :pos pos)))
    (jupyter--send-encoded client channel "complete_request" msg)))

(cl-defmethod jupyter-handle-complete ((client jupyter-kernel-client)
                                       matches
                                       cursor-start
                                       cursor-end
                                       metadata)
  "Default complete reply handler.")

(cl-defmethod jupyter-send-history ((client jupyter-kernel-client)
                                    &key
                                    output
                                    raw
                                    hist-access-type
                                    session
                                    start
                                    stop
                                    n
                                    pattern
                                    unique)
  (declare (indent 1))
  (let ((channel (oref client shell-channel))
        (msg (jupyter-history-request
              :output output
              :raw raw
              :hist-access-type hist-access-type
              :session session
              :start start
              :stop stop
              :n n
              :patten pattern
              :unique unique)))
    (jupyter--send-encoded client channel "history_request" msg)))

(cl-defmethod jupyter-handle-history ((client jupyter-kernel-client) history)
  "Default history reply handler.")

(cl-defmethod jupyter-send-is-complete ((client jupyter-kernel-client)
                                        &key code)
  (declare (indent 1))
  (let ((channel (oref client shell-channel))
        (msg (jupyter-is-complete-request
              :code code)))
    (jupyter--send-encoded client channel "is_complete_request" msg)))

(cl-defmethod jupyter-handle-is-complete
    ((client jupyter-kernel-client) status indent)
  "Default is complete reply handler.")

(cl-defmethod jupyter-send-comm-info ((client jupyter-kernel-client)
                                      &key target-name)
  (declare (indent 1))
  (let ((channel (oref client shell-channel))
        (msg (jupyter-complete-request
              :target-name target-name)))
    (jupyter--send-encoded client channel "comm_info_request" msg)))

(cl-defmethod jupyter-handle-comm-info ((client jupyter-kernel-client) comms)
  "Default comm info. reply handler.")

(cl-defmethod jupyter-send-kernel-info ((client jupyter-kernel-client))
  "Get the info of the kernel CLIENT is connected to.
This function returns the `:content' of the kernel-info request.
Note that this function also blocks until the kernel-info reply
is received."
  (let* ((channel (oref client shell-channel)))
    (jupyter--send-encoded client channel "kernel_info_request" ())))

(cl-defmethod jupyter-handle-kernel-info ((client jupyter-kernel-client)
                                          protocol-version
                                          implementation
                                          implementation-version
                                          language-info
                                          banner
                                          help-links)
  "Default kernel-info reply handler.")

;;; iopub messages

(defun jupyter--handle-iopub-message (client msg)
  (cl-check-type client jupyter-kernel-client)
  (cl-destructuring-bind (&key msg_type content &allow-other-keys) msg
    (pcase msg_type
      ("stream"
       (cl-destructuring-bind (&key name text &allow-other-keys)
           content
         (jupyter-handle-stream client name text)))
      ("execute_input"
       (cl-destructuring-bind (&key code execution_count &allow-other-keys)
           content
         (jupyter-handle-execute-input client code execution_count)))
      ("execute_result"
       (cl-destructuring-bind (&key execution_count
                                    data
                                    metadata
                                    &allow-other-keys)
           content
         (message "execute-result: %s" (plist-get (plist-get msg :parent_header)
                                                  :msg_id))
         (jupyter-handle-execute-result client execution_count data metadata)))
      ("error"
       (cl-destructuring-bind (&key ename evalue traceback &allow-other-keys)
           content
         (jupyter-handle-error client ename evalue traceback)))
      ("status"
       (cl-destructuring-bind (&key execution_state &allow-other-keys)
           content
         (jupyter-handle-status client execution_state)))
      ("clear_output"
       (cl-destructuring-bind (&key wait &allow-other-keys)
           content
         (jupyter-handle-clear-output client wait)))
      ("display_data"
       (cl-destructuring-bind (&key data metadata transient &allow-other-keys)
           content
         (jupyter-handle-display-data client data metadata transient)))
      ("update_display_data"
       (cl-destructuring-bind (&key data metadata transient &allow-other-keys)
           content
         (jupyter-handle-update-display-data client data metadata transient)))
      (_ (error "Message type not handled yet.")))))

(cl-defmethod jupyter-handle-stream ((client jupyter-kernel-client) name text)
  "Default stream handler.")

(cl-defmethod jupyter-handle-execute-input ((client jupyter-kernel-client)
                                            code
                                            execution-count)
  "Default execute input handler.")

(cl-defmethod jupyter-handle-execute-result ((client jupyter-kernel-client)
                                             execution-count
                                             data
                                             metadata)
  "Default execute result handler.")

(cl-defmethod jupyter-handle-error ((client jupyter-kernel-client)
                                    ename
                                    evalue
                                    traceback)
  "Default error handler.")

(cl-defmethod jupyter-handle-status ((client jupyter-kernel-client) execution_state)
  "Default status handler.")

(cl-defmethod jupyter-handle-clear-output ((client jupyter-kernel-client) wait)
  "Default clear output handler.")

(cl-defmethod jupyter-handle-display-data ((client jupyter-kernel-client)
                                           data
                                           metadata
                                           transient)
  "Default display data handler.")

(cl-defmethod jupyter-handle-update-display-data ((client jupyter-kernel-client)
                                                  data
                                                  metadata
                                                  transient)
  "Default update display data handler.")

(provide 'jupyter-client)
