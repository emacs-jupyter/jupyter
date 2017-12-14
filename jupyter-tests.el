;; -*- lexical-binding: t -*-

(require 'jupyter-client)
(require 'jupyter-messages)
(require 'cl-lib)
(require 'ert)

(ert-deftest jupyter-messages ()
  (ert-info ("Splitting identities from messages")
    (let ((msg (list "123" "323" jupyter-message-delimiter
                     "msg1" "msg2" "\0\0")))
      (should (equal (jupyter-split-identities msg)
                     (cons (list "123" "323")
                           (list "msg1" "msg2" "\0\0"))))
      (setq msg (list "123" "No" "delim" "in" "message"))
      (should-error (jupyter-split-identities msg))))
  (ert-info ("Creating message headers")
    (let ((header (jupyter-message-header "stdin_reply" "session-id")))
      ;; TODO: Check fields
      (should (plist-get header :msg_id))
      (should (plist-get header :date))
      (should (string= (plist-get header :msg_type) "stdin_reply"))
      (should (string= (plist-get header :version) jupyter-protocol-version))
      (should (string= (plist-get header :username) user-login-name))
      (should (string= (plist-get header :session) "session-id")))
    ;; TODO: Handle other kinds of encoding
    (ert-info ("Encoding/decoding objects")
      (let ((json-object-type 'plist)
            (obj nil))
        (should-not (multibyte-string-p (jupyter-encode-object "foîji")))
        ;; TODO: Only decodes json plists, what to do instead?
        (should-error (jupyter-decode-string (jupyter-encode-object "foîji")))
        (setq obj '(:msg_id 12342 :msg_type "stdin_reply" :session "foîji"))
        (should (json-plist-p obj))
        (should-not (multibyte-string-p (jupyter-encode-object obj)))
        (should (equal (jupyter-decode-string (jupyter-encode-object obj))
                       obj))))))

(ert-deftest jupyter-channels ()
  (ert-info ("Channel types should match their class")
    (should (eq (oref (jupyter-shell-channel) type) :shell))
    (should (eq (oref (jupyter-stdin-channel) type) :stdin))
    (should (eq (oref (jupyter-iopub-channel) type) :iopub))
    (should (eq (oref (jupyter-control-channel) type) :control))
    (should (eq (oref (jupyter-hb-channel) type) :hb)))
  (let ((channel (jupyter-shell-channel :endpoint "tcp://127.0.0.1:5555")))
    (ert-info ("Starting a channel")
      (oset channel socket nil)
      (should-not (oref channel socket))
      (should-not (jupyter-channel-alive-p channel))
      (jupyter-start-channel channel :identity "foo")
      (should (oref channel socket))
      (cl-check-type (oref channel socket) zmq-socket)
      (should (equal (zmq-socket-get (oref channel socket) zmq-ROUTING_ID)
                     "foo"))
      (should (jupyter-channel-alive-p channel)))
    (ert-info ("Stopping a channel")
      (ring-insert (oref channel recv-queue) "bar")
      (let ((sock (oref channel socket)))
        (jupyter-stop-channel channel)
        (should-not (oref channel socket))
        (should-error (zmq-close sock) :type 'zmq-ENOTSOCK)
        ;; All pending messages are droped when a channel is stopped
        (should (= (ring-length (oref channel recv-queue)) 0))
        (should-not (jupyter-channel-alive-p channel)))))
  (ert-info ("Heartbeat channel")
    (let ((proc (zmq-start-process
                 (lambda (ctx)
                   (with-zmq-socket sock zmq-REP
                     (zmq-bind sock "tcp://127.0.0.1:5555")
                     (while t
                       (zmq-recv sock)
                       (zmq-send sock "pong"))))))
          (channel (jupyter-hb-channel :endpoint "tcp://127.0.0.1:5555")))
      (unwind-protect
          (progn
            (ert-info ("Starting the channel")
              (oset channel process nil)
              (should-not (jupyter-channel-alive-p channel))
              (should-not (jupyter-hb-beating-p channel))
              (jupyter-start-channel channel)
              (should (jupyter-channel-alive-p channel))
              (should (jupyter-hb-beating-p channel))
              (should-not (oref channel paused)))
            ;; TODO: Fix pausing the channel
            (ert-info ("Pausing the channel")
              (jupyter-hb-pause channel)
              ;; TODO: Improve these wait times
              (sleep-for 2)
              (should (oref channel paused)))
            (ert-info ("Unpausing the channel")
              (jupyter-hb-unpause channel)
              (sleep-for 2)
              (should-not (oref channel paused)))
            (ert-info ("Checking the status")
              (jupyter-hb-pause channel)
              (sleep-for 2)
              (should (jupyter-hb-beating-p channel))
              ;; Asking if the hb is beating unpauses the channel
              (should-not (oref channel paused))))
        (delete-process proc)
        ;; TODO: Refactor to remove this
        (jupyter-channel-stop channel)
        (kill-buffer (process-buffer proc))))))

(ert-deftest jupyter-client ()
  (let* ((socks (cl-loop repeat 4
                         collect (zmq-socket (current-zmq-context) zmq-REQ)))
         (sock-endpoint
          (cl-loop
           with addr = "tcp://127.0.0.1"
           for sock in socks
           collect (cons sock (format "%s:%d" addr (zmq-bind-to-random-port
                                                    sock addr))))))
    (unwind-protect
        (progn
          (setq client (jupyter-kernel-client
                        :session (jupyter-session
                                  :key "58e05d24-7600e037194e78bde23108de")
                        :shell-channel (jupyter-shell-channel
                                        :endpoint (cdr (nth 0 sock-endpoint)))
                        :iopub-channel (jupyter-iopub-channel
                                        :endpoint (cdr (nth 1 sock-endpoint)))
                        :stdin-channel (jupyter-stdin-channel
                                        :endpoint (cdr (nth 2 sock-endpoint)))
                        :hb-channel (jupyter-hb-channel
                                     :endpoint (cdr (nth 3 sock-endpoint)))))
          (cl-loop
           for cname in (list 'shell-channel 'iopub-channel
                              'hb-channel 'stdin-channel)
           do (should (slot-boundp client cname))
           (if (eq cname 'hb-channel) (cl-check-type (eieio-oref client cname)
                                                     jupyter-hb-channel)
             (cl-check-type (eieio-oref client cname) jupyter-channel))
           (should-not (jupyter-channel-alive-p (eieio-oref client cname))))

          (jupyter-start-channels client)

          (cl-loop
           for cname in (list 'shell-channel 'iopub-channel
                              'hb-channel 'stdin-channel)
           for channel = (eieio-oref client cname)
           unless (eq cname 'hb-channel) do
           (should (slot-boundp channel 'socket))
           (cl-check-type (oref channel socket) zmq-socket)
           and do (should (jupyter-channel-alive-p (eieio-oref client cname)))))
      (mapc (lambda (se) (zmq-close (car se))) sock-endpoint))))

(ert-deftest jupyter-message-callbacks ()
  (let ((client (jupyter-kernel-client-using-kernel "python")))
    (jupyter-start-channels client)
    ;; Let channels start
    (sleep-for 1)
    (unwind-protect
        (progn
          (ert-info ("Invalid message type")
            (should-error
             (jupyter-add-receive-callback
                 client 'exe-res (jupyter-send-execute client :code "y = 1+2\ny")
               (lambda (msg) (+ 1 2)))))
          (ert-info ("Execute on correct message ID")
            (let ((id (jupyter-send-kernel-info client))
                  (recv-id nil))
              (jupyter-send-kernel-info client)
              (jupyter-add-receive-callback client 'kernel-info-reply id
                (lambda (msg)
                  (setq recv-id (plist-get (plist-get msg :parent_header)
                                           :msg_id))))
              ;; Receive messages from kernel
              (sleep-for 1)
              (should (equal id recv-id))))
          (ert-info ("Multiple callbacks on the same message")
            (let ((id (jupyter-send-execute client :code "y = 1+2\ny"))
                  (msg-res nil)
                  (msg-rep nil))
              (jupyter-add-receive-callback client 'execute-result id
                (lambda (msg) (setq msg-res msg)))
              (jupyter-add-receive-callback client 'execute-reply id
                (lambda (msg) (setq msg-rep msg)))
              ;; Receive messages from kernel
              (sleep-for 1)
              (should-not (null msg-res))
              (should-not (null msg-rep))
              ;; execute_result
              (should (equal id (plist-get (plist-get msg-res :parent_header)
                                           :msg_id)))
              (should (equal (plist-get msg-res :msg_type) "execute_result"))
              (cl-destructuring-bind (&key data &allow-other-keys)
                  (plist-get msg-res :content)
                (should (equal (plist-get data :text/plain) "3")))
              ;; execute_reply
              (should (equal id (plist-get (plist-get msg-rep :parent_header)
                                           :msg_id)))
              (should (equal (plist-get msg-rep :msg_type) "execute_reply")))))
      (jupyter-stop-channels client)
      (delete-process (oref client kernel))
      (kill-buffer (process-buffer (oref client kernel))))))

(ert-deftest jupyter-processing-messages ()
  (setq client (jupyter-kernel-client-using-kernel "python"))
  (jupyter-start-channels client)

  ;; TODO: Better interface for this. It looks like I will have to handle
  ;; certain messages specially. For a shutdown message, I will also have to
  ;; delete the process as is done here. Also, I don't like these receive
  ;; callbacks without some type of error handling on the msg-type of the
  ;; reply. If something is mispelled, then it won't fire. And if a certain
  ;; message is not expecting a reply, it will also never fire. This will cause
  ;; hangs when using `jupyter-wait-until-received'.
  ;;
  ;; TODO: `jupyter-shutdown' seems ambiguous. Prefix the sending messages like
  ;; `jupyter-send-shutdown', `jupyter-send-execute', ...
  (lexical-let ((proc (oref client kernel))
                (id (jupyter-shutdown client)))
    (jupyter-add-receive-callback
        client "shutdown_reply" id
      (lambda (msg)
        (when (equal (plist-get (plist-get msg :content) :status)
                     "ok")
          (delete-process proc)
          (kill-buffer (process-buffer proc)))))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
