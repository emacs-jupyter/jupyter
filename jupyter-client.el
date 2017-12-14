(require 'zmq)
(eval-when-compile (require 'cl))
(require 'jupyter-channels)
(require 'jupyter-messages)

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
      (oset client kernel proc)
      client)))

(defun jupyter-ioloop-callback (client channel)
  (let* ((ring (oref channel recv-queue)))
    ;; TODO: How many messages does ZMQ store in its internal buffers before it
    ;; starts droping messages? And what socket option can be examined to
    ;; figure this out?
    (unless (= (ring-length ring) (ring-size ring))
      (let* ((res (jupyter-recv-decoded client channel)))
        (ring-insert ring res)
        (run-with-timer 0.01 nil #'jupyter-process-message client channel)))))

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
         (lambda () (jupyter-ioloop-callback client channel))))))))

(cl-defmethod jupyter-stop-channels ((client jupyter-kernel-client))
  "Stop any running channels of CLIENT."
  (cl-loop
   for channel in (mapcar (lambda (c) (eieio-oref client c))
                     (list 'shell-channel
                           'iopub-channel
                           'hb-channel
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
                        'stdin-channel)
   if (jupyter-channel-alive-p (eieio-oref client channel))
   return t))

;;; Sending and receiving messages

(cl-defmethod jupyter-send ((channel jupyter-channel) parts &optional flags)
  "Send a message with PARTS on the socket associated with CHANNEL.
Optional variable FLAGS, are the flags argument of the `zmq-send'
or `zmq-send-multipart' call used to send parts."
  (declare (indent 1))
  (let ((sock (oref channel socket)))
    (if (listp parts)
        (zmq-send-multipart sock parts flags)
      (zmq-send sock parts flags))))

(cl-defmethod jupyter-send-encoded ((client jupyter-kernel-client)
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
      (jupyter-encode-message (oref client session) type :content message)
    (jupyter-send channel msg flags)
    msg-id))

(cl-defmethod jupyter-recv ((channel jupyter-channel) &optional flags)
  "Recieve a message on CHANNEL.
Optional variable FLAGS is the flags argument of the
`zmq-recv-multipart' call using the CHANNEL's socket. The return
value is a cons cell, (IDENTS . PARTS), where IDENTS are the
routing identities of the message and PARTS are the message
parts. See
http://jupyter-client.readthedocs.io/en/latest/messaging.html#the-wire-protocol
for more details."
  (jupyter-split-identities (zmq-recv-multipart (oref channel socket) flags)))

(cl-defmethod jupyter-recv-decoded ((client jupyter-kernel-client) channel &optional flags)
  (cl-destructuring-bind (idents . parts)
      (jupyter-recv channel flags)
    (cons idents (jupyter-decode-message (oref client session) parts))))

;;; Processing messages

;; TODO: Override `jupyter-kernel-client' to hook into receiving these
;; messages. How would an `ob-jupyter' client do this?

(defun jupyter-add-receive-callback (client msg-type msg-id function)
  "Add FUNCTION to run when receiving a message reply.

MSG-ID corresponds to the ID of one a message type which expects
a reply. Whenever CLIENT receives a reply message whose parent
header has an ID that matches MSG-ID FUNCTION, along with any
other registered functions for MSG-ID, will be executed."
  (declare (indent 3))
  (cl-check-type client jupyter-kernel-client)
  (let* ((message-callbacks (oref client message-callbacks))
         (callbacks (gethash msg-id message-callbacks)))
    (if (not callbacks)
        (puthash msg-id (list (cons msg-type function)) message-callbacks)
      (let ((cb-for-type (assoc msg-type callbacks)))
        (if cb-for-type (setcdr cb-for-type function)
          (nconc callbacks (list (cons msg-type function))))))))

(defun jupyter-wait-until-received (client msg-type pmsg-id)
  "Wait until MSG-ID is received on CLIENT.
This function registers a callback using
`jupyter-add-receive-callback' and waits for the callback to be
executed. After execution, it returns the corresponding message
reply whose parent header has a message id of MSG-ID.

If CLIENT is not waiting for any reply that can match MSG-ID, an
error is raised."
  (declare (indent 2))
  (lexical-let ((msg nil))
    (jupyter-add-receive-callback client msg-type pmsg-id
      (lambda (m) (setq msg m)))
    (while (null msg)
      (sleep-for 0 10))
    msg))

(defun jupyter-process-message (client channel)
  "Process a message on CLIENT's CHANNEL.
This function processes a single message on CLIENT's CHANNEL and
schedules to process more messages at a future time until
CHANNEL's recv-queue is empty."
  (cl-check-type channel jupyter-channel)
  (let ((ring (oref channel recv-queue)))
    (unless (ring-empty-p ring)
      (let* ((ctype (oref channel type))
             (handler (cl-case ctype
                        (:stdin #'jupyter-handle-stdin-message)
                        (:iopub #'jupyter-handle-iopub-message)
                        (:shell #'jupyter-handle-shell-message)
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
      (run-with-timer 0.01 nil #'jupyter-process-message client channel))))

;;; Message handlers

;;; stdin messages

(cl-defmethod jupyter-handle-stdin-message ((client jupyter-kernel-client) msg)
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
    (jupyter-send-encoded client channel "input_reply" msg)))

;;; shell messages

;; http://jupyter-client.readthedocs.io/en/latest/messaging.html#messages-on-the-shell-router-dealer-channel
(cl-defmethod jupyter-handle-shell-message ((client jupyter-kernel-client) msg)
  (cl-destructuring-bind (&key msg_type content &allow-other-keys) msg
    (cl-destructuring-bind (&key status &allow-other-keys) content
      (if (equal status "ok")
          (pcase msg_type
            ("execute_reply")
            ("inspect_reply")
            ("complete_reply")
            ("history_reply")
            ("is_complete_reply")
            ("comm_info_reply")
            ("kernel_info_reply")
            ("shutdown_reply")
            ("interrupt_reply")
            (_ (error "Message type not handled yet.")))
        (if (equal status "error")
            (error "Error (%s): %s"
                   (plist-get content :ename) (plist-get content :evalue))
          (error "Error: aborted"))))))

(cl-defmethod jupyter-execute ((client jupyter-kernel-client)
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
    (jupyter-send-encoded client channel "execute_request" msg)))

(cl-defmethod jupyter-kernel-info ((client jupyter-kernel-client))
  "Get the info of the kernel CLIENT is connected to.
This function returns the `:content' of the kernel-info request.
Note that this function also blocks until the kernel-info reply
is received."
  (let* ((channel (oref client shell-channel)))
    (plist-get (jupyter-wait-until-received client "kernel_info_reply"
                 (jupyter-send-encoded
                  client channel "kernel_info_request" ()))
               :content)))

(cl-defmethod jupyter-shutdown ((client jupyter-kernel-client) &optional restart)
  "Request a shutdown of the kernel CLIENT is communicating with.
If RESTART is non-nil, request a restart instead of a complete shutdown."
  (let ((channel (oref client shell-channel))
        (msg (jupyter-shutdown-request :restart restart)))
    (jupyter-send-encoded client channel "shutdown_request" msg)))

(cl-defmethod jupyter-interrupt ((client jupyter-kernel-client))
  (let ((channel (oref client shell-channel)))
    (jupyter-send-encoded client channel "interrupt_request" ())))

;;; iopub messages
;; TODO: Display data, update display data

(cl-defmethod jupyter-handle-iopub-message ((client jupyter-kernel-client) msg)
  (cl-destructuring-bind (&key msg_type content &allow-other-keys) msg
    (pcase msg_type
      ("stream" (jupyter-handle-stream client
                                       (plist-get content :name)
                                       (plist-get content :text)))
      ("execute_input" (jupyter-handle-execute-input
                        client
                        (plist-get content :code)
                        (plist-get content :execution_count)))
      ("execute_result")
      ("error")
      ("status")
      ("clear_output")
      ("display_data")
      ("update_display_data")
      (_ (error "Message type not handled yet.")))))

(cl-defmethod jupyter-handle-stream ((client jupyter-kernel-client) name text)
  (cond
   ((equal name "stdout") (print text))
   ;; TODO: DO something better
   ((equal name "stderr") (error text))))

(cl-defmethod jupyter-handle-execute-input ((client jupyter-kernel-client)
                                            code
                                            execution-count)
  ;; TODO
  )

(provide 'jupyter-client)
