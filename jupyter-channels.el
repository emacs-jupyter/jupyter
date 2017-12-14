(require 'zmq)
(eval-when-compile (require 'cl))
(require 'eieio)

(defconst jupyter-channel-socket-types
  (list :hb zmq-REQ
        :shell zmq-DEALER
        :iopub zmq-SUB
        :stdin zmq-DEALER
        :control zmq-DEALER)
  "The socket types for the various channels used by `jupyter'.")

;; TODO: Each channel has its own process like the heartbeat channel and does
;; the majority of encoding and decoding messages there. The parent emacs
;; process then is only responsible to carry out the actions in the messages
;; and construct replies which are then sent to the process.
;;
;; The current implementation still creates a process, but only for polling the
;; file descriptor of the socket to check for incoming messages. What would be
;; simpler is to create one polling process and handle messages through a
;; single filter function instead of per channel.
(defclass jupyter-channel ()
  ((type
    :type keyword
    :initarg :type
    :documentation "The type of this channel. Should be one of
 the keys in `jupyter-channel-socket-types', excluding `:hb'
 which corresponds to the heartbeat channel and is handled
 differently than the other channels. See `jupyter-hb-channel'.")
   (endpoint
    :type string
    :initarg :endpoint
    :documentation "The endpoint this channel is connected to.
 Typical endpoints look like \"tcp://127.0.0.1:5555\".")
   (socket
    :type (or null zmq-socket)
    :initform nil
    :documentation "The socket this channel uses to communicate
 with the kernel.")
   (recv-queue
    :type ring
    :initform (make-ring 10)
    :documentation "A queue of messages received on this channel
 that are waiting to be processed. See `jupyter-process-message'
 for its use."))
  :abstract t
  :documentation "A base class for channels used by `jupyter'.")

(defclass jupyter-iopub-channel (jupyter-channel)
  ((type :initform :iopub))
  :documentation "A base class for iopub channels.")

(defclass jupyter-stdin-channel (jupyter-channel)
  ((type :initform :stdin))
  :documentation "A base class for stdin channels.")

(defclass jupyter-shell-channel (jupyter-channel)
  ((type :initform :shell))
  :documentation "A base class for shell channels.")

(defclass jupyter-control-channel (jupyter-channel)
  ((type :initform :control))
  :documentation "A base class for control channels.")

(defun jupyter-connect-endpoint (type endpoint &optional identity)
  "Create socket with type TYPE and connect it to ENDPOINT.
If IDENTITY is non-nil, it will be set as the ROUTING_ID of the
socket."
  (let ((sock (zmq-socket (current-zmq-context) type)))
    (zmq-socket-set sock zmq-LINGER 1000)
    (when identity
      (zmq-socket-set sock zmq-ROUTING_ID identity))
    (zmq-connect sock endpoint)
    sock))

(cl-defmethod jupyter-start-channel ((channel jupyter-channel) &key identity)
  "Start a CHANNEL.
If IDENTITY is non-nil, it is used as the ROUTING_ID of the
underlying channel's socket."
  (unless (jupyter-channel-alive-p channel)
    (let ((sock (jupyter-connect-endpoint
                 (plist-get jupyter-channel-socket-types
                            (oref channel type))
                 (oref channel endpoint)
                 identity)))
      (oset channel socket sock))))

(cl-defmethod jupyter-start-channel ((channel jupyter-iopub-channel) &key identity)
  "Start an iopub CHANNEL subscribed to all messages.
If IDENTITY is non-nil, it is used as the ROUTING_ID of the
underlying channel's socket."
  (let ((sock (cl-call-next-method)))
    (when sock
      (zmq-socket-set sock zmq-SUBSCRIBE ""))))

(cl-defmethod jupyter-stop-channel ((channel jupyter-channel))
  "Stop a CHANNEL.
The underlying socket's LINGER property is set to 0, the socket
is closed, the channel's socket property is set to nil, and any
pending messages in the channels recv-queue are removed. Note
that `jupyter-channel-alive-p' on the CHANNEL will return nil
after a call to this function."
  (when (jupyter-channel-alive-p channel)
    (let ((sock (oref channel socket)))
      (zmq-socket-set sock zmq-LINGER 0)
      (zmq-close sock)
      (cl-loop
       with ring = (oref channel recv-queue)
       repeat (ring-length ring) do (ring-remove ring))
      (oset channel socket nil))))

(cl-defmethod jupyter-channel-alive-p ((channel jupyter-channel))
  "Return non-nil if CHANNEL is alive.
A channel is alive if its socket property is bound to a
`zmq-socket'."
  (and (slot-boundp channel 'socket)
       (not (null (oref channel socket)))))

(defclass jupyter-hb-channel ()
  ((type
    :type keyword
    :initform :hb
    :documentation "The type of this channel. Should be one of
 the keys in `jupyter-channel-socket-types', excluding `:hb'
 which corresponds to the heartbeat channel.")
   (endpoint
    :type string
    :initarg :endpoint
    :documentation "The endpoint this channel is connected to.
 Typical endpoints look like \"tcp://127.0.0.1:5555\".")
   ;; channel must be restarted for this to be updated
   (time-to-dead
    :type integer
    :initform 1
    :documentation "The time in seconds to wait for a response
 from the kernel until the connection is assumed to be dead.")
   (beating
    :type (or boolean symbol)
    :initform t
    :documentation "A flag variable indicating that the heartbeat
 channel is sending and receiving messages with the kernel.")
   (paused
    :type boolean
    :initform nil
    :documentation "A flag variable indicating that the heartbeat
 channel is paused and not communicating with the kernel. To
 pause the heartbeat channel use `jupyter-hb-pause', to unpause
 use `jupyter-hb-unpause'.")
   (process
    :type (or null process)
    :initform nil
    :documentation "The underlying process which runs the
 heartbeat channel and communicates with the kernel."))
  :documentation "A base class for heartbeat channels.")

(cl-defmethod jupyter-hb-beating-p ((channel jupyter-hb-channel))
  "Return non-nil if the kernel associated with CHANNEL is still
connected."
  (when (jupyter-channel-alive-p channel)
    ;; unpause the channel if we are asking for its heartbeat
    (when (oref channel paused)
      (jupyter-hb-unpause channel))
    (oset channel beating 'beating)
    (while (eq (oref channel beating) 'beating)
      (sleep-for 0 10))
    (oref channel beating)))

(cl-defmethod jupyter-channel-alive-p ((channel jupyter-hb-channel))
  "Return non-nil if CHANNEL is alive."
  (process-live-p (oref channel process)))

(cl-defmethod jupyter-hb-pause ((channel jupyter-hb-channel))
  "Pause checking for heartbeat events on CHANNEL."
  (unless (process-live-p (oref channel process))
    (error "Heartbeat process not started"))
  (process-send-string (oref channel process) "pause\n"))

(cl-defmethod jupyter-hb-unpause ((channel jupyter-hb-channel))
  "Unpause checking for heatbeat events on CHANNEL."
  (unless (process-live-p (oref channel process))
    (error "Heartbeat process not started"))
  (process-send-string (oref channel process) "unpause\n"))

(cl-defmethod jupyter-stop-channel ((channel jupyter-hb-channel))
  "Stop a CHANNEL."
  (let ((proc (oref channel process)))
    (when proc
      (delete-process proc)
      (kill-buffer (process-buffer proc))
      (oset channel process nil))))

;; TODO: Either use `zmq-poll' here or use the poller in zmq-subprocess.
(cl-defmethod jupyter-start-channel ((channel jupyter-hb-channel) &key identity)
  "Start a CHANNEL."
  (declare (indent 1))
  (jupyter-stop-channel channel)
  ;; https://github.com/jupyter/jupyter_client/blob/master/jupyter_client/channels.py
  (let*
      ((time-to-dead (oref channel time-to-dead))
       (proc
        (zmq-start-process
         `(lambda (ctx)
            (let ((beating t)
                  (paused nil)
                  (cmd nil)
                  (request-time nil))
              (while t
                (catch 'restart
                  (with-zmq-socket sock ,(plist-get jupyter-channel-socket-types
                                                    (oref channel type))
                      ((zmq-LINGER 1000))
                    ,(when identity
                       `(zmq-socket-set sock zmq-ROUTING_ID ,identity))
                    (zmq-connect sock ,(oref channel endpoint))
                    (with-zmq-poller
                     (zmq-poller-register (current-zmq-poller) sock zmq-POLLIN)
                     (while t
                       (when (setq cmd (read-minibuffer ""))
                         (cl-case cmd
                           (beating (prin1 (cons 'beating beating)))
                           (pause (setq paused t) (prin1 '(pause . t)))
                           (unpause (setq paused nil) '(unpause . t)))
                         (zmq-flush 'stdout))
                       (unless paused
                         ;; Wait to poll again, but only after communicating
                         ;; with the parent process.
                         (when request-time
                           (sit-for
                            (time-to-seconds
                             (time-subtract
                              ,time-to-dead (time-subtract
                                             (current-time)
                                             request-time)))))
                         (zmq-send sock "ping")
                         (setq request-time (current-time))
                         ;; TODO: Handle errors
                         (let ((event (zmq-poller-wait
                                       (current-zmq-poller)
                                       ,(* (ceiling (time-to-seconds
                                                     time-to-dead))
                                           1000))))
                           (if event (progn (zmq-recv sock)
                                            (prin1 (cons 'beating
                                                         (setq beating t)))
                                            (zmq-flush 'stdout))
                             (prin1 (cons 'beating (setq beating nil)))
                             (zmq-flush 'stdout)
                             (throw 'restart t)))))))))))
         (lexical-let ((channel channel))
           (lambda (event)
             (cl-case (car event)
               (pause (oset channel paused (cdr event)))
               (unpause (oset channel paused (not (cdr event))))
               (beating (oset channel beating (cdr event)))
               (otherwise (error "Invalid event from heartbeat channel.")))
             ;; Keep feeding the process. Note that this is necessary for emacs
             ;; subprocesses since `read-minibuffer' in batch mode calls
             ;; `getchar' from the standard c library. There is no way (that I
             ;; have found) to peek at STDIN to determine if there is anything
             ;; available. So having this ping-pong messaging is a work around
             ;; to ensure that the polling loop runs smoothly.
             (unless (oref channel paused)
               (process-send-string (oref channel process) "nil\n")))))))
    ;; Send a first command to kick of the loop
    (process-send-string proc "beating\n")
    ;; Don't query when exiting
    (set-process-query-on-exit-flag proc nil)
    (oset channel process proc)))

(provide 'jupyter-channels)
