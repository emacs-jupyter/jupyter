(require 'zmq)
(require 'cl-lib)
(require 'ring)
(require 'eieio)

(defconst jupyter-channel-socket-types
  (list :hb zmq-REQ
        :shell zmq-DEALER
        :iopub zmq-SUB
        :stdin zmq-DEALER
        :control zmq-DEALER)
  "The socket types for the various channels used by `jupyter'.")

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

(defun jupyter-connect-channel (ctype endpoint &optional identity)
  (let ((sock-type (plist-get jupyter-channel-socket-types ctype)))
    (unless sock-type
      (error "Invalid channel type (%s)" ctype))
    (jupyter-connect-endpoint sock-type endpoint identity)))

(cl-defmethod jupyter-start-channel ((channel jupyter-channel) &key identity)
  "Start a CHANNEL.
If IDENTITY is non-nil, it is used as the ROUTING_ID of the
underlying channel's socket."
  (unless (jupyter-channel-alive-p channel)
    (let ((sock (jupyter-connect-channel
                 (oref channel type) (oref channel endpoint) identity)))
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

(cl-defmethod jupyter-channel-push-message ((channel jupyter-channel) msg)
  (let ((ring (oref channel recv-queue)))
    (ring-insert+extend ring msg 'grow)))

(cl-defmethod jupyter-channel-get-message ((channel jupyter-channel))
  (unless (ring-empty-p (oref channel recv-queue))
    (cl-destructuring-bind (idents . msg)
        (ring-remove (oref channel recv-queue))
      msg)))

(cl-defmethod jupyter-channel-messages-available-p ((channel jupyter-channel))
  (not (ring-empty-p (oref channel recv-queue))))

(defclass jupyter-hb-channel ()
  ((type
    :type keyword
    :initform :hb
    :documentation "The type of this channel is `:hb'.")
   (endpoint
    :type string
    :initarg :endpoint
    :documentation "The endpoint this channel is connected to.
 Typical endpoints look like \"tcp://127.0.0.1:5555\".")
   (socket
    :type (or null zmq-socket)
    :initform nil
    :documentation "The socket used for communicating with the kernel.")
   (time-to-dead
    :type integer
    :initform 1
    :documentation "The time in seconds to wait for a response
 from the kernel until the connection is assumed to be dead. Note
 that this slot only takes effect when starting the channel.")
   (beating
    :type (or boolean symbol)
    :initform t
    :documentation "A flag variable indicating that the heartbeat
 channel is communicating with the kernel.")
   (paused
    :type boolean
    :initform t
    :documentation "A flag variable indicating that the heartbeat
 channel is paused and not communicating with the kernel. To
 pause the heartbeat channel use `jupyter-hb-pause', to unpause
 use `jupyter-hb-unpause'.")
   (timer
    :type (or null timer)
    :initform nil
    :documentation "The timer which sends and receives heartbeat
 messages from the kernel."))
  :documentation "A base class for heartbeat channels.")

(cl-defmethod jupyter-channel-alive-p ((channel jupyter-hb-channel))
  "Return non-nil if CHANNEL is alive."
  (and (oref channel timer) (memq (oref channel timer) timer-list)))

(cl-defmethod jupyter-hb-beating-p ((channel jupyter-hb-channel))
  "Return non-nil if the kernel associated with CHANNEL is still
connected."
  (unless (jupyter-channel-alive-p channel)
    (error "Heartbeat channel not alive"))
  (oref channel beating))

(cl-defmethod jupyter-hb-pause ((channel jupyter-hb-channel))
  "Pause checking for heartbeat events on CHANNEL."
  (unless (jupyter-channel-alive-p channel)
    (error "Heartbeat channel not alive"))
  (oset channel paused t))

(cl-defmethod jupyter-hb-unpause ((channel jupyter-hb-channel))
  "Unpause checking for heatbeat events on CHANNEL."
  (unless (jupyter-channel-alive-p channel)
    (error "Heartbeat channel not alive"))
  (oset channel paused nil))

(cl-defmethod jupyter-stop-channel ((channel jupyter-hb-channel))
  "Stop the heartbeat CHANNEL."
  (when (jupyter-channel-alive-p channel)
    (cancel-timer (oref channel timer))
    (zmq-socket-set (oref channel socket) zmq-LINGER 0)
    (zmq-close (oref channel socket))
    (oset channel socket nil)
    (oset channel timer nil)))

(cl-defmethod jupyter-start-channel ((channel jupyter-hb-channel) &key identity)
  (unless (jupyter-channel-alive-p channel)
    (oset channel socket (jupyter-connect-channel
                          :hb (oref channel endpoint) identity))
    (oset channel timer
          (run-with-timer
           0 (oref channel time-to-dead)
           (lexical-let ((identity identity)
                         (sent nil))
             (lambda (channel)
               (let ((sock (oref channel socket)))
                 (when sent
                   (setq sent nil)
                   (if (condition-case nil
                           (zmq-recv sock zmq-NOBLOCK)
                         (zmq-EAGAIN nil))
                       (oset channel beating t)
                     (oset channel beating nil)
                     (zmq-socket-set sock zmq-LINGER 0)
                     (zmq-close sock)
                     (setq sock (jupyter-connect-channel
                                 :hb (oref channel endpoint) identity))
                     (oset channel socket sock)))
                 (unless (oref channel paused)
                   (zmq-send sock "ping")
                   (setq sent t)))))
           channel))))

(provide 'jupyter-channels)
