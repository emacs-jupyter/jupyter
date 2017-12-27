(require 'json)
(require 'zmq)
(eval-when-compile (require 'cl))
(require 'jupyter-channels)
(require 'jupyter-messages)

(declare-function string-trim-right "subr-x" (str))
(defvar jupyter--debug nil)

;;; Kernel client class

;;; Kernel manager

(defclass jupyter-connection ()
  ((session
    :type jupyter-session
    :initarg :session
    :documentation "The `jupyter-session' object which holds the
 key for authenticating messages.")
   (conn-info
    :type json-plist
    :initarg :conn-info
    :documentation "The connection plist which holds the channel
 ports and other information required for connecting to a kernel.
 See
 http://jupyter-client.readthedocs.io/en/latest/kernels.html#connection-files"))
  :abstract t)

(defclass jupyter-kernel-manager (jupyter-connection)
  ((name
    :initarg :name
    :type string)
   (conn-file
    :type string)
   (kernel
    :type (or null process)
    :initform nil
    :documentation "The local kernel process or nil if no local
 kernel was started by this client.")
   (control-channel
    :type (or null jupyter-control-channel)
    :initform nil)
   (kernel-info
    :type (or null json-plist)
    :initform nil
    :documentation "Contains the result of an initial kernel_info_request
 to the kernel after starting the kernel.")
   (kernel-spec
    :type (or null json-plist)
    :initform nil)))

(cl-defmethod initialize-instance ((manager jupyter-kernel-manager) &rest slots)
  (unless (slot-boundp manager 'name)
    (oset manager name "python")))

(defvar jupyter--kernelspec-dirs nil
  "An alist matching kernel names to their kernelspec
  directories.")

(defun jupyter-available-kernelspecs (&optional force-new)
  (unless (or jupyter--kernelspec-dirs force-new)
    (setq jupyter--kernelspec-dirs
          (mapcar (lambda (s) (let ((s (split-string s " " 'omitnull)))
                      (cons (car s) (cadr s))))
             (seq-subseq
              (split-string
               (shell-command-to-string "jupyter kernelspec list")
               "\n" 'omitnull "[ \t]+")
              1))))
  jupyter--kernelspec-dirs)

(defun jupyter-find-kernelspec (prefix)
  (when prefix
    (let ((kname-path (cl-find-if
                       (lambda (s) (string-prefix-p prefix (car s)))
                       (jupyter-available-kernelspecs)))
          (json-object-type 'plist)
          (json-array-type 'list)
          (json-false nil))
      (when kname-path
        (cons (car kname-path)
              (json-read-file (expand-file-name
                               "kernel.json" (cdr kname-path))))))))

(cl-defun jupyter-create-connection-info (&key
                                          (kernel-name "python")
                                          (transport "tcp")
                                          (ip "127.0.0.1")
                                          (signature-scheme "hmac-sha256")
                                          (key (jupyter--new-uuid))
                                          (hb-port 0)
                                          (stdin-port 0)
                                          (control-port 0)
                                          (shell-port 0)
                                          (iopub-port 0))
  "Create a jupyter connection plist.

The plist has the standard keys found in the jupyter spec. See
http://jupyter-client.readthedocs.io/en/latest/kernels.html#connection-files."
  (unless (or (= (length key) 0)
              (equal signature-scheme "hmac-sha256"))
    (error "Only hmac-sha256 signing is currently supported."))
  (append
   (list :kernel_name kernel-name
         :transport transport
         :ip ip)
   (when (> (length key) 0)
     (list :signature_scheme signature-scheme
           :key key))
   (cl-loop
    with sock = (zmq-socket (current-zmq-context) zmq-REP)
    with addr = (concat transport "://" ip)
    for (channel . port) in (list (cons :hb_port hb-port)
                                  (cons :stdin_port stdin-port)
                                  (cons :control_port control-port)
                                  (cons :shell_port shell-port)
                                  (cons :iopub_port iopub-port))
    collect channel and
    if (= port 0) do (setq port (zmq-bind-to-random-port sock addr))
    and collect port and
    do (zmq-unbind sock (zmq-socket-get sock zmq-LAST_ENDPOINT)) else
    collect port
    finally (zmq-close sock))))

(defun jupyter--start-kernel (kernel-name conn-file env &rest args)
  (let ((process-environment
         (append
          ;; The first entry takes precedence when duplicated variables
          ;; are found in `process-environment'
          (cl-loop
           for e on env by #'cddr
           for k = (car e)
           for v = (cadr e)
           collect (format "%s=%s" (cl-subseq (symbol-name k) 1) v))
          process-environment)))
    (apply #'start-process
           (format "jupyter-kernel-%s" kernel-name)
           (generate-new-buffer (format "*jupyter-kernel-%s*" kernel-name))
           (car args) (cdr args))))

;; TODO: Allow passing arguments like a different kernel file name or different
;; ports and arguments to the kernel
(cl-defmethod jupyter-start-kernel ((manager jupyter-kernel-manager))
  (let ((kname-spec (jupyter-find-kernelspec (oref manager name))))
    (unless kname-spec
      (error "No kernel found that starts with name (%s)" (oref manager name)))
    (cl-destructuring-bind (kernel-name . spec) kname-spec
      ;; Ensure we use the full name of the kernel
      ;; TODO: Require a valid kernel name when initializing the manager
      (oset manager name kernel-name)
      (oset manager kernel-spec spec)
      (let* ((name (oref manager name))
             (conn-info (jupyter-create-connection-info :kernel-name kernel-name))
             (session-key (plist-get conn-info :key))
             (conn-file (expand-file-name
                         (concat "kernel-" session-key ".json")
                         (string-trim-right (shell-command-to-string
                                             "jupyter --runtime-dir")))))
        (oset manager conn-info conn-info)
        (oset manager session (jupyter-session :key session-key))
        ;; Write the connection file
        (with-temp-buffer
          (let ((json-encoding-pretty-print t))
            (insert (json-encode-plist (oref manager conn-info)))
            (write-file conn-file)))
        (let ((atime (nth 4 (file-attributes conn-file))))
          ;; Start the kernel
          (oset manager kernel
                (jupyter--start-kernel
                 kernel-name conn-file (plist-get spec :env)
                 (cl-loop
                  for arg in (plist-get spec :argv)
                  if (equal arg "{connection_file}") collect conn-file
                  else collect arg)))
          ;; TODO: Figure out a better way to talk to a kernel without creating
          ;; a client.
          (if (with-timeout (2 nil)
                (while (equal atime (nth 4 (file-attributes conn-file)))
                  (sleep-for 0 100))
                t)
              (let ((new-conn-file (expand-file-name
                                    (format "kernel-%d.json"
                                            (process-id (oref manager kernel)))
                                    (file-name-directory conn-file))))
                (rename-file conn-file new-conn-file)
                (oset manager conn-file new-conn-file)
                (jupyter-start-channels manager))
            (error "Kernel did not read connection file within timeout.")))))))

(cl-defmethod jupyter-start-channels ((manager jupyter-kernel-manager))
  (let ((control-channel (oref manager control-channel)))
    (if control-channel
        (unless (jupyter-channel-alive-p control-channel)
          (jupyter-start-channel
           control-channel
           :identity (jupyter-session-id (oref manager session))))
      (let ((conn-info (oref manager conn-info)))
        (oset manager control-channel
              (jupyter-control-channel
               :endpoint (format "%s://%s:%d"
                                 (plist-get conn-info :transport)
                                 (plist-get conn-info :ip)
                                 (plist-get conn-info :control_port))))
        (jupyter-start-channels manager)))))

(cl-defmethod jupyter-stop-channels ((manager jupyter-kernel-manager))
  (let ((control-channel (oref manager control-channel)))
    (when control-channel
      (jupyter-stop-channel control-channel)
      (oset manager control-channel nil))))

(cl-defmethod jupyter--send-encoded ((manager jupyter-kernel-manager) type message)
  (unless (member type '("shutdown_request" "interrupt_request"))
    (error "Only shutdown or interrupt requests on control channel (%s)."
           type))
  (let ((session (oref manager session))
        (sock (oref (oref manager control-channel) socket)))
    ;; TODO: Rename to just `jupyter--send'
    (jupyter--send-encoded session sock type message)))

(cl-defmethod jupyter-stop-kernel ((manager jupyter-kernel-manager))
  (when (jupyter-kernel-alive-p manager)
    (jupyter-request-shutdown manager)
    (with-timeout (5 nil)
      (while (jupyter-kernel-alive-p manager)
        (sleep-for 1)))
    ;; Clean up
    (let ((proc (oref manager kernel)))
      (delete-process proc)
      (kill-buffer (process-buffer proc))
      (delete-file (oref manager conn-file)))
    (jupyter-stop-channels manager)
    (oset manager kernel nil)
    (oset manager session nil)
    (oset manager conn-file nil)))

(cl-defmethod jupyter-kernel-alive-p ((manager jupyter-kernel-manager))
  (process-live-p (oref manager kernel)))

(cl-defmethod jupyter-new-kernel-client ((manager jupyter-kernel-manager)
                                         class)
  (unless (oref manager kernel)
    (error "Kernel not started."))
  (let ((client (jupyter-kernel-client-from-conn-info
                 class (oref manager conn-info))))
    client))

(cl-defmethod jupyter-request-shutdown ((manager jupyter-kernel-manager))
  "Request a shutdown of MANAGER's kernel.
If RESTART is non-nil, request a restart instead of a complete shutdown."
  (let ((msg (jupyter-shutdown-request)))
    (jupyter--send-encoded manager "shutdown_request" msg)))

(cl-defmethod jupyter-request-interrupt ((manager jupyter-kernel-manager))
  (if (equal (plist-get (oref manager kernel-spec) :interrupt_mode) "message")
      (let ((msg (jupyter-interrupt-request)))
        (jupyter--send-encoded manager "interrupt_request" msg))
    (interrupt-process (oref manager kernel) t)))

;;; Kernel client class

(defclass jupyter-kernel-client (jupyter-connection)
  ((requests
    :type hash-table
    :initform (make-hash-table :test 'equal)
    :documentation "A hash table with message ID's as keys. This
 is used to register callback functions to run once a reply from
 a previously sent request is received. See
 `jupyter-add-receive-callback'. Note that this is also used to
 filter received messages that originated from a previous request
 by this client. Whenever the client sends a message in which a
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

(cl-defmethod jupyter-client-initialize-connection
    ((client jupyter-kernel-client)
     channel-class
     file-or-plist)
  "Read a connection FILE-OR-PLIST and return a `jupyter-kernel-client'.
If FILE-OR-PLIST is a file name, the connection info is read from
the file. If FILE-OR-PLIST is a plist, it is assumed to be a
connection plist containing the keys required for a connection
plist. See
http://jupyter-client.readthedocs.io/en/latest/kernels.html#connection-files."
  (cl-check-type file-or-plist (or json-plist file-exists))
  (unless (child-of-class-p channel-class 'jupyter-channel-base)
    (error "Not a valid channel class (%s)" channel-class))

  (let ((conn-info (let ((json-array-type 'list)
                         (json-object-type 'plist)
                         (json-false nil))
                     (if (json-plist-p file-or-plist) file-or-plist
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
      ;; TODO: Does this flush the channel queues?
      (jupyter-stop-channels client)
      (let ((addr (concat transport "://" ip)))
        (oset client session (jupyter-session :key key))
        (oset client stdin-channel
              (make-instance
               channel-class
               :type :stdin
               :endpoint (format "%s:%d" addr stdin_port)))
        (oset client shell-channel
              (make-instance
               channel-class
               :type :shell
               :endpoint (format "%s:%d" addr shell_port)))
        (oset client hb-channel
              (jupyter-hb-channel
               :endpoint (format "%s:%d" addr hb_port)))
        (oset client iopub-channel
              (make-instance
               :type :iopub
               :endpoint (format "%s:%d" addr iopub_port)))))))

(cl-defmethod initialize-instance ((client jupyter-kernel-client) &rest slots)
  (let ((km (car (alist-get :kernel-manager slots))))
    (when km
      (cl-check-type km jupyter-kernel-manager)
      (jupyter-client-initialize-connection client (oref km conn-info)))
    client))


;;; Lower level sending/receiving

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
  (let* ((ioloop (oref client ioloop))
         (ring (or (process-get ioloop :jupyter-pending-requests)
                   (let ((ring (make-ring 10)))
                     (process-put ioloop :jupyter-pending-requests
                                  ring)
                     ring))))
    (zmq-subprocess-send (oref client ioloop)
      (list 'send (oref channel type) type message flags))
    ;; Anything sent to stdin is a reply not a request so don't add it to
    ;; :jupyter-pending-requests.
    (unless (eq (oref channel type) :stdin)
      (let ((req (make-jupyter-request)))
        (ring-insert+extend ring req 'grow)
        req))))

(defun jupyter--ioloop (client)
  (let ((iopub-channel (oref client iopub-channel))
        (shell-channel (oref client shell-channel))
        (stdin-channel (oref client stdin-channel))
        (control-channel (oref client control-channel)))
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
              (control
               (jupyter-connect-channel
                :control ,(oref (oref client control-channel) endpoint)
                (jupyter-session-id session)))
              (channels (list (cons control :control)
                              (cons stdin :stdin)
                              (cons shell :shell)
                              (cons iopub :iopub)))
              (priorities (list (cons :control 8)
                                (cons :iopub 4)
                                (cons :stdin 4)
                                (cons :shell 2)))
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
                                (t (or (< (car a) (car b))
                                       (when (= (car a) (car b))
                                         (> (alist-get (cadr a) priorities)
                                            (alist-get (cadr b) priorities))))))))
                    (cl-loop
                     while (not (ring-empty-p queue))
                     do (zmq-prin1 (cons 'recvd (cdr (ring-remove queue))))))
                   (recv-message
                    (sock ctype)
                    (when (= (ring-length queue) (ring-size queue))
                      (send-recvd))
                    ;; msg = (idents . plist)
                    (let* ((msg (jupyter--recv-decoded session sock))
                           (date (plist-get (plist-get (cdr msg) :header) :date))
                           (time (+ (float-time (date-to-time date))
                                    ;; Use the fractional seconds if available
                                    ;; for sorting purposes.
                                    ;;
                                    ;; NOTE: If no fractional time is available
                                    ;; it kind of defeats the purpose of
                                    ;; queuing the messages in this subprocess.
                                    (if (string-match
                                         "T.+\\(\\(?:\\.\\|,\\)[0-9]+\\)" date)
                                        (string-to-number (match-string 1 date))
                                      0))))
                      (ring-insert queue (cons time (cons ctype msg)))))
                   (send-message
                    (sock ctype rest)
                    (zmq-prin1
                     (cons 'sent
                           (cons ctype (apply #'jupyter--send-encoded session
                                              sock rest)))))
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
                ;; Also poll for standard-in events to be able to read commands from
                ;; the parent emacs process without blocking
                (zmq-poller-register (current-zmq-poller) 0 zmq-POLLIN)
                (mapc (lambda (x) (zmq-poller-register (current-zmq-poller)
                                               (car x)
                                               zmq-POLLIN))
                   channels)
                (unwind-protect
                    (while t
                      ;; TODO: Dynamic polling period, if the rate of received
                      ;; events is high, reduce the period. If the rate of
                      ;; received events is low increase it. Sample the rate in
                      ;; a time window that spans multiple polling periods.
                      ;; Polling at 10 ms periods was causing a pretty sizable
                      ;; portion of CPU time to be eaten up.
                      (when (and (= idle-count 2)
                                 (> (ring-length queue) 0))
                        (send-recvd))
                      (let ((events (condition-case err
                                        (zmq-poller-wait-all (current-zmq-poller) 5 20)
                                      (zmq-EINTR nil)
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

(defun jupyter--ioloop-sentinel (client ioloop event)
  (cond
   ((or (string-prefix-p "exited" event)
        (string-prefix-p "failed" event)
        (equal event "finished\n")
        (equal event "killed\n")
        (equal event "deleted\n"))
    (kill-buffer (process-buffer ioloop))
    (when (jupyter-channel-alive-p (oref client hb-channel))
      (jupyter-stop-channel (oref client hb-channel)))
    (oset client ioloop nil))))

(defun jupyter--ioloop-filter (client event)
  (cl-destructuring-bind (ctype . data) (cdr event)
    (cl-case (car event)
      ;; Cleanup handled in sentinel
      (quit)
      ;; data = sent message id
      (sent
       ;; Anything sent on stdin is a reply and therefore never added to
       ;; :jupyter-pending-requests
       (unless (eq ctype :stdin)
         (let* ((ring (process-get
                       (oref client ioloop) :jupyter-pending-requests))
                (req (ring-remove ring)))
           (setf (jupyter-request--id req) data)
           (when jupyter--debug
             (message "SEND: %s" (jupyter-request-id req)))
           (puthash data req (oref client requests)))))
      ;; data = (idents . msg)
      (recvd
       (let* ((channel (cl-find-if
                        (lambda (c) (eq (oref c type) ctype))
                        (mapcar (lambda (x) (eieio-oref client x))
                           '(stdin-channel
                             shell-channel
                             control-channel
                             iopub-channel))))
              (ring (oref channel recv-queue)))
         (when jupyter--debug
           (message "RECV: %s %s %s" (jupyter-message-type (cdr data))
                    (jupyter-message-parent-id (cdr data))
                    (plist-get (cdr data) :content)))
         (if (= (ring-length ring) (ring-size ring))
             ;; Try to process at a later time when the recv-queue is full
             (run-with-timer 0.05 nil #'jupyter--ioloop-filter client event)
           (ring-insert ring data)
           (run-with-timer
            0.01 nil #'jupyter--handle-message client channel)))))))
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
                        'control-channel
                        'stdin-channel)
   if (jupyter-channel-alive-p (eieio-oref client channel))
   return t))

;;; Message callbacks

;; A `jupyter-request' object represents the status of a request to the kernel
;; and holds all the information required to process the messages associated
;; with the request. Whenever a message arrives that is associated with a
;; request's `jupyter-request-id', any callbacks associated with the message
;; type are run (see `jupyter-add-receive-callback'). When a request's
;; `jupyter-idle-received-p' property is non-nil, then it signifies that the
;; request has been handled by the kernel.
(cl-defstruct jupyter-request
  (-id)
  (idle-received-p)
  (callbacks))

(cl-defstruct jupyter-callback
  (ran-p nil)
  (cb nil))

(defun jupyter-request-id (req)
  (with-timeout (0.5 (error "Request not processed."))
    (while (null (jupyter-request--id req))
      (sleep-for 0 10)))
  (jupyter-request--id req))

(defun jupyter--run-callbacks-for-message (req msg)
  (when req
    (let* ((callbacks (jupyter-request-callbacks req))
           (cbt (cdr (assoc t callbacks)))
           (cb (cdr (assoc (jupyter-message-type msg) callbacks))))
      (cl-loop
       for cb in (list cb cbt)
       when cb do
       (funcall (jupyter-callback-cb cb) msg)
       (setf (jupyter-callback-ran-p cb) t)))))

(defun jupyter-add-receive-callback (client msg-type req function)
  "Add FUNCTION to run when receiving a message reply.

The function will be run when CLIENT receives a reply message
that has a type of MSG-TYPE and is a reply due to a request that
has an ID of MSG-ID. As an example, suppose you want to register
a callback when you recieve an `execute-reply' after sending an
execute request. This can be done like so:

    (jupyter-add-receive-callback client 'execute-reply
        (jupyter-request-execute client :code \"y = 1 + 2\")
      (lambda (msg)
        (cl-assert (equal (jupyter-message-type msg) \"execute_reply\"))))

Note that the callback is given the raw decoded message received
from the kernel without any processing done to it."
  (declare (indent 3))
  (cl-check-type client jupyter-kernel-client)
  (let ((mt (plist-get jupyter--received-message-types msg-type)))
    (if mt (setq msg-type mt)
      ;; msg-type = t means to run for every message type associated with
      ;; msg-id
      (unless (eq msg-type t)
        (error "Not a valid received message type (`%s')" msg-type))))
  (if (jupyter-request-idle-received-p req)
      (error "Request already received idle message.")
    (let ((callbacks (jupyter-request-callbacks req))
          (cb (make-jupyter-callback :cb function)))
      (if (null callbacks)
          (setf (jupyter-request-callbacks req) (list (cons msg-type cb)))
        (let* ((cb-for-type (assoc msg-type callbacks)))
          (if cb-for-type (setcdr cb-for-type cb)
            (nconc callbacks (list (cons msg-type cb)))))))))

(defun jupyter-wait-until (client msg-type req timeout cond)
  "Wait until COND returns non-nil for a received message.
COND is run for every received message that has a type of
MSG-TYPE and whose parent header has a message ID of PMSG-ID. If
no messages are received that pass these two conditions before
TIMEOUT (in seconds), this function returns nil. Otherwise it
returns the received message. Note that if TIMEOUT is nil, it
defaults to 1 second."
  (declare (indent 4))
  (setq timeout (or timeout 1))
  (cl-check-type timeout number)
  (lexical-let ((msg nil)
                (cond cond))
    (jupyter-add-receive-callback client msg-type req
      (lambda (m) (setq msg (if (funcall cond m) m nil))))
    (let ((time 0))
      (catch 'timeout
        (while (null msg)
          (when (>= time timeout)
            (throw 'timeout nil))
          (accept-process-output (oref client ioloop) 0.01)
          (setq time (+ time 0.01)))
        msg))))

(defun jupyter-wait-until-idle (client req &optional timeout)
  "Wait until a status: idle message is received for PMSG-ID.
This function waits until TIMEOUT for CLIENT to receive an idle
status message for the request associated with PMSG-ID. If
TIMEOUT is non-nil, it defaults to 1 second."
  (jupyter-wait-until client 'status req timeout
    #'jupyter-message-status-idle-p))

(defun jupyter-wait-until-received (client msg-type req &optional timeout)
  "Wait for a message with MSG-TYPE to be received on CLIENT.
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
  (declare (indent 2))
  (jupyter-wait-until client msg-type req timeout
    #'identity))

(defun jupyter--handle-message (client channel)
  "Process a message on CLIENT's CHANNEL.
When a message is received on CLIENT's channel it is decoded and
added to the CHANNEL's recv-queue and this function is scheduled
to be run at a later time to process the messages in the queue.

To process a message the following steps are taken:

1. A message is removed from the recv-queue
2. A handler function is found base on CHANNEL's type
3. The handler function is called with the CLIENT and the message
   as arguments
4. Any callbacks previously registered for the message are run
5. This function is scheduled to process another message of
   CHANNEL in the future"
  (let ((ring (oref channel recv-queue)))
    (unless (ring-empty-p ring)
      ;; Messages are stored like (idents . msg) in the ring
      (let* ((ctype (oref channel type))
             (handler (cl-case ctype
                        (:stdin #'jupyter--handle-stdin-message)
                        (:iopub #'jupyter--handle-iopub-message)
                        (:shell #'jupyter--handle-shell-message)
                        (:control #'jupyter--handle-control-message)
                        (otherwise (error "Wrong channel type (%s)." ctype))))
             (msg (cdr (ring-remove ring)))
             (pmsg-id (jupyter-message-parent-id msg))
             (requests (oref client requests))
             (req (gethash pmsg-id requests)))
        ;; Drop messages not sent by us.
        ;; TODO: Some messages might be useful.
        (when req
          (when (jupyter-message-status-idle-p msg)
            (setf (jupyter-request-idle-received-p req) t))
          (unwind-protect
              (funcall handler client req msg)
            (unwind-protect
                (jupyter--run-callbacks-for-message req msg)
              ;; Remove the request once an idle message has been received and
              ;; all callbacks have run atleast once. This is done because it
              ;; is not gauranteed that the idle message is received after all
              ;; other messages for a request.
              ;;
              ;; NOTE: this probably doesn't handle all cases.
              (when (and (jupyter-request-idle-received-p req)
                         ;; Check if all callbacks for req have run at least
                         ;; once
                         (if (not (jupyter-request-callbacks req)) t
                           (cl-loop
                            for (reply-type . cb) in (jupyter-request-callbacks req)
                            unless (jupyter-callback-ran-p cb) return nil
                            finally return t)))
                (remhash pmsg-id requests)))))))))

;;; Received message handlers

;;; stdin messages

(defun jupyter--handle-stdin-message (client req msg)
  (cl-destructuring-bind (&key prompt password &allow-other-keys)
      (plist-get msg :content)
    (jupyter-handle-input client req prompt password)))

(cl-defmethod jupyter-handle-input ((client jupyter-kernel-client) req prompt password)
  "Handle an input request from CLIENT's kernel.
PROMPT is the prompt the kernel would like to show the user. If
PASSWORD is non-nil, then `read-passwd' is used to get input from
the user. Otherwise `read-from-minibuffer' is used."
  (let ((channel (oref client stdin-channel))
        (msg (jupyter-input-reply
              :value (funcall (if password #'read-passwd
                                #'read-from-minibuffer)
                              prompt))))
    ;; TODO: Check for 'allow_stdin'
    ;; http://jupyter-client.readthedocs.io/en/latest/messaging.html#stdin-messages
    (jupyter--send-encoded client channel "input_reply" msg)))

;;; control messages

(defun jupyter--handle-control-message (client req msg)
  (cl-destructuring-bind (&key msg_type content &allow-other-keys) msg
    (let ((status (plist-get content :status)))
      (if (equal status "ok")
          ;; fIXME: An interrupt reply is only sent when interrupt_mode is set
          ;; to message in a kernel's kernelspec.
          (pcase msg_type
            ("interrupt_reply"
             (jupyter-handle-interrupt client req)))
        (if (equal status "error")
            (error "Error (%s): %s"
                   (plist-get content :ename) (plist-get content :evalue))
          (error "Error: aborted"))))))

(cl-defmethod jupyter-request-shutdown ((client jupyter-kernel-client) &optional restart)
  "Request a shutdown of CLIENT's kernel.
If RESTART is non-nil, request a restart instead of a complete shutdown."
  (let ((channel (oref client control-channel))
        (msg (jupyter-shutdown-request :restart restart)))
    (jupyter--send-encoded client channel "shutdown_request" msg)))

(cl-defmethod jupyter-handle-shutdown ((client jupyter-kernel-client) req restart)
  "Default shutdown reply handler.")

;; FIXME: This breaks the convention that all jupyter-request-* functions
;; returns a message-id future object.
(cl-defmethod jupyter-request-interrupt ((client jupyter-kernel-client))
  ;; TODO: Check for interrupt_mode of the kernel's kernelspec
  ;; http://jupyter-client.readthedocs.io/en/latest/messaging.html#kernel-interrupt
  (let ((channel (oref client control-channel)))
    (jupyter--send-encoded client channel "interrupt_request" ())))

(cl-defmethod jupyter-handle-interrupt ((client jupyter-kernel-client) req)
  "Default interrupt reply handler.")

;;; shell messages

;; http://jupyter-client.readthedocs.io/en/latest/messaging.html#messages-on-the-shell-router-dealer-channel
(defun jupyter--handle-shell-message (client req msg)
  (cl-destructuring-bind (&key msg_type content &allow-other-keys) msg
    (let ((status (plist-get content :status)))
      ;; We check for error or abort since "is_complete_reply" also contains a
      ;; status field
      (if (not (member status '("error" "abort")))
          (pcase msg_type
            ("execute_reply"
             (cl-destructuring-bind (&key execution_count
                                          user_expressions
                                          payload
                                          &allow-other-keys)
                 content
               (jupyter-handle-execute
                client req execution_count user_expressions payload)))
            ("inspect_reply"
             (cl-destructuring-bind (&key found
                                          data
                                          metadata
                                          &allow-other-keys)
                 content
               (jupyter-handle-inspect
                client req found data metadata)))
            ("complete_reply"
             (cl-destructuring-bind (&key matches
                                          cursor_start
                                          cursor_end
                                          metadata
                                          &allow-other-keys)
                 content
               (jupyter-handle-complete
                client req matches cursor_start cursor_end metadata)))
            ("history_reply"
             (cl-destructuring-bind (&key history &allow-other-keys)
                 content
               (jupyter-handle-history client req history)))
            ("is_complete_reply"
             (cl-destructuring-bind (&key status indent &allow-other-keys)
                 content
               (jupyter-handle-is-complete client req status indent)))
            ("comm_info_reply"
             (cl-destructuring-bind (&key comms &allow-other-keys)
                 content
               (jupyter-handle-comm-info client req comms)))
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
                client req protocol_version implementation implementation_version
                language_info banner help_links)))
            (_ (error "Message type not handled yet.")))
        (if (equal status "error")
            (error "Error (%s): %s"
                   (plist-get content :ename) (plist-get content :evalue))
          (error "Error: aborted"))))))

(cl-defmethod jupyter-request-execute ((client jupyter-kernel-client)
                                       &key code
                                       (silent nil)
                                       (store-history t)
                                       (user-expressions nil)
                                       (allow-stdin t)
                                       (stop-on-error nil))
  "Send an execute request."
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
                                      req
                                      execution-count
                                      user-expressions
                                      payload)
  "Default execute reply handler.")

(cl-defmethod jupyter-request-inspect ((client jupyter-kernel-client)
                                       &key code
                                       (pos 0)
                                       (detail 0))
  "Send an inspect request."
  (declare (indent 1))
  (let ((channel (oref client shell-channel))
        (msg (jupyter-inspect-request
              :code code :pos pos :detail detail)))
    (jupyter--send-encoded client channel "inspect_request" msg)))

(cl-defmethod jupyter-handle-inspect ((client jupyter-kernel-client)
                                      req
                                      found
                                      data
                                      metadata)
  "Default inspect reply handler.")

(cl-defmethod jupyter-request-complete ((client jupyter-kernel-client)
                                        &key code
                                        (pos 0))
  "Send a complete request."
  (declare (indent 1))
  (let ((channel (oref client shell-channel))
        (msg (jupyter-complete-request
              :code code :pos pos)))
    (jupyter--send-encoded client channel "complete_request" msg)))

(cl-defmethod jupyter-handle-complete ((client jupyter-kernel-client)
                                       req
                                       matches
                                       cursor-start
                                       cursor-end
                                       metadata)
  "Default complete reply handler.")

(cl-defmethod jupyter-request-history ((client jupyter-kernel-client)
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
        (msg (jupyter-history-request
              :output output
              :raw raw
              :hist-access-type hist-access-type
              :session session
              :start start
              :stop stop
              :n n
              :pattern pattern
              :unique unique)))
    (jupyter--send-encoded client channel "history_request" msg)))

(cl-defmethod jupyter-handle-history ((client jupyter-kernel-client) req history)
  "Default history reply handler.")

(cl-defmethod jupyter-request-is-complete ((client jupyter-kernel-client)
                                           &key code)
  "Send an is-complete request."
  (declare (indent 1))
  (let ((channel (oref client shell-channel))
        (msg (jupyter-is-complete-request
              :code code)))
    (jupyter--send-encoded client channel "is_complete_request" msg)))

(cl-defmethod jupyter-handle-is-complete
    ((client jupyter-kernel-client) req status indent)
  "Default is complete reply handler.")

(cl-defmethod jupyter-request-comm-info ((client jupyter-kernel-client)
                                         &key target-name)
  "Send a comm-info request."
  (declare (indent 1))
  (let ((channel (oref client shell-channel))
        (msg (jupyter-comm-info-request
              :target-name target-name)))
    (jupyter--send-encoded client channel "comm_info_request" msg)))

(cl-defmethod jupyter-handle-comm-info ((client jupyter-kernel-client) req comms)
  "Default comm info. reply handler.")

(cl-defmethod jupyter-request-kernel-info ((client jupyter-kernel-client))
  "Send a kernel-info request."
  (let* ((channel (oref client shell-channel)))
    (jupyter--send-encoded client channel "kernel_info_request" ())))

(cl-defmethod jupyter-handle-kernel-info ((client jupyter-kernel-client)
                                          req
                                          protocol-version
                                          implementation
                                          implementation-version
                                          language-info
                                          banner
                                          help-links)
  "Default kernel-info reply handler.")

;;; iopub messages

(defun jupyter--handle-iopub-message (client req msg)
  (cl-destructuring-bind (&key msg_type content &allow-other-keys) msg
    (pcase msg_type
      ("shutdown_reply"
       (cl-destructuring-bind (&key restart &allow-other-keys)
           content
         (jupyter-handle-shutdown client req restart)))
      ("stream"
       (cl-destructuring-bind (&key name text &allow-other-keys)
           content
         (jupyter-handle-stream client req name text)))
      ("execute_input"
       (cl-destructuring-bind (&key code execution_count &allow-other-keys)
           content
         (jupyter-handle-execute-input client req code execution_count)))
      ("execute_result"
       (cl-destructuring-bind (&key execution_count
                                    data
                                    metadata
                                    &allow-other-keys)
           content
         (jupyter-handle-execute-result client req execution_count data metadata)))
      ("error"
       (cl-destructuring-bind (&key ename evalue traceback &allow-other-keys)
           content
         (jupyter-handle-error client req ename evalue traceback)))
      ("status"
       (cl-destructuring-bind (&key execution_state &allow-other-keys)
           content
         (jupyter-handle-status client req execution_state)))
      ("clear_output"
       (cl-destructuring-bind (&key wait &allow-other-keys)
           content
         (jupyter-handle-clear-output client req wait)))
      ("display_data"
       (cl-destructuring-bind (&key data metadata transient &allow-other-keys)
           content
         (jupyter-handle-display-data client req data metadata transient)))
      ("update_display_data"
       (cl-destructuring-bind (&key data metadata transient &allow-other-keys)
           content
         (jupyter-handle-update-display-data client req data metadata transient)))
      (_ (error "Message type not handled yet.")))))

(cl-defmethod jupyter-handle-stream ((client jupyter-kernel-client) pmsg-id name text)
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

(cl-defmethod jupyter-handle-status ((client jupyter-kernel-client) req execution_state)
  "Default status handler.")

(cl-defmethod jupyter-handle-clear-output ((client jupyter-kernel-client) req wait)
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
