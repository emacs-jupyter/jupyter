;; -*- lexical-binding: t -*-

(require 'jupyter-client)
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
        (should-not (jupyter-channel-alive-p channel))))
    (ert-info ("Heartbeat channel")
      (cl-flet ((start-hb () (zmq-start-process
                              (lambda (ctx)
                                (with-zmq-socket sock zmq-REP
                                  (zmq-bind sock "tcp://127.0.0.1:5556")
                                  (while t
                                    (zmq-send sock (zmq-recv sock))))))))
        (let ((proc (start-hb))
              (channel (jupyter-hb-channel :endpoint "tcp://127.0.0.1:5556")))
          (unwind-protect
              (progn
                (ert-info ("Starting the channel")
                  (should-not (jupyter-channel-alive-p channel))
                  (jupyter-start-channel channel)
                  ;; Let the channel start
                  (sleep-for 1)
                  (should (jupyter-channel-alive-p channel))
                  (should (jupyter-hb-beating-p channel))
                  (should-not (oref channel paused)))
                (ert-info ("Pausing the channel")
                  (jupyter-hb-pause channel)
                  (should (oref channel paused)))
                (ert-info ("Unpausing the channel")
                  (jupyter-hb-unpause channel)
                  (should-not (oref channel paused))
                  (should (jupyter-hb-beating-p channel)))
                (ert-info ("Checking the heartbeat")
                  (should (jupyter-hb-beating-p channel))
                  (delete-process proc)
                  (kill-buffer (process-buffer proc))
                  (sleep-for (* 2 (oref channel time-to-dead)))
                  (should-not (jupyter-hb-beating-p channel))
                  (setq proc (start-hb))
                  (sleep-for 1)
                  (should (jupyter-hb-beating-p channel)))
                (ert-info ("Stopping the channel")
                  (let ((p (oref channel process)))
                    (jupyter-stop-channel channel)
                    (should-not (process-live-p p))
                    (should-not (oref channel process)))))
            (when (process-live-p proc)
              (delete-process proc))
            (kill-buffer (process-buffer proc))))))))

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
  (with-zmq-context
    (let ((client (jupyter-kernel-client-using-kernel "python")))
      ;; FIXME: Let the kernel startup before connecting, this is necessary
      ;; since we just start a kernel using "jupyter console" at the moment
      ;; which does some initial handshaking with the kernel.
      ;;
      ;; Actually the problem seems to be my use of multiple processes to poll
      ;; for events. Because I have a subprocess per socket, the order of
      ;; received messages is not gauranteed although the kernel sends the
      ;; messages in a certain order. What I really should probably do is poll
      ;; all sockets in a single subprocess.
      (jupyter-start-channels client)
      ;; Let the channels start
      (unwind-protect
          (progn
            (ert-info ("Waiting for messages")
              (should-not
               (null (jupyter-wait-until-idle
                      client (jupyter-request-kernel-info client)))))
            ;; (ert-info ("Callbacks are removed when an idle message is received")
            ;;   (let ((id (jupyter-request-kernel-info client)))
            ;;     (jupyter-add-receive-callback client 'kernel-info-reply id
            ;;       (lambda (msg) 'foo))
            ;;     (should-not (null (gethash id (oref client message-callbacks))))
            ;;     (jupyter-wait-until-idle client id 2)
            ;;     (should (null (gethash id (oref client message-callbacks))))))
            (ert-info ("Message callbacks receive the right messages")
              (ert-info ("Callbacks fire on the right message ID")
                (let ((id (jupyter-request-kernel-info client))
                      (recv-id nil))
                  (jupyter-add-receive-callback client 'kernel-info-reply id
                    (lambda (msg) (setq recv-id (jupyter-message-parent-id msg))))
                  (jupyter-wait-until-idle client id)
                  (should (equal recv-id id))))
              (ert-info ("Different message type, same message ID")
                (let ((id (jupyter-request-execute client :code "y = 1+2\ny"))
                      (msg-res nil)
                      (msg-rep nil))
                  (jupyter-add-receive-callback client 'execute-result id
                    (lambda (msg) (setq msg-res msg)))
                  (jupyter-add-receive-callback client 'execute-reply id
                    (lambda (msg) (setq msg-rep msg)))
                  (should-not (null (jupyter-wait-until-idle client id)))
                  (should (json-plist-p msg-res))
                  (should (json-plist-p msg-rep))
                  (ert-info ("Verify received contents")
                    ;; execute_result
                    (should (equal id (jupyter-message-parent-id msg-res)))
                    (should (equal (plist-get msg-res :msg_type) "execute_result"))
                    ;; execute_reply
                    (should (equal id (jupyter-message-parent-id msg-rep)))
                    (should (equal (plist-get msg-rep :msg_type) "execute_reply")))))))
        (jupyter-stop-channels client)
        (delete-process (oref client kernel))
        (kill-buffer (process-buffer (oref client kernel)))))))


(ert-deftest jupyter-message-types ()
  (let ((client (jupyter-kernel-client-using-kernel "python")))
    (jupyter-start-channels client)
    ;; Let the channels start
    (sleep-for 1)
    (unwind-protect
        (progn
          (ert-info ("Kernel info")
            (let ((res (jupyter-wait-until-received client 'kernel-info-reply
                         (jupyter-request-kernel-info client))))
              (should-not (null res))
              (should (json-plist-p res))
              (should (equal (jupyter-message-type res) "kernel_info_reply"))))
          (ert-info ("Comm info")
            (let ((res (jupyter-wait-until-received client 'comm-info-reply
                         (jupyter-request-comm-info client))))
              (should-not (null res))
              (should (json-plist-p res))
              (should (equal (jupyter-message-type res) "comm_info_reply"))))
          (ert-info ("Execute")
            (let ((res (jupyter-wait-until-received client 'execute-reply
                         (jupyter-request-execute client :code "y = 1 + 2"))))
              (should-not (null res))
              (should (json-plist-p res))
              (should (equal (jupyter-message-type res) "execute_reply"))))
          (ert-info ("Input")
            (cl-letf (((symbol-function 'read-from-minibuffer)
                       (lambda (prompt &rest args) "foo")))
              (let ((res (jupyter-wait-until-received client 'execute-result
                           (jupyter-request-execute client :code "input('')"))))
                (should-not (null res))
                (should (json-plist-p res))
                (should (equal (jupyter-message-type res) "execute_result"))
                (cl-destructuring-bind (&key data &allow-other-keys)
                    (plist-get res :content)
                  (should (equal (plist-get data :text/plain) "'foo'"))))))
          (ert-info ("Inspect")
            (let ((res (jupyter-wait-until-received client 'inspect-reply
                         (jupyter-request-inspect
                          client
                          :code "list((1, 2, 3))"
                          :pos 2
                          :detail 0))))
              (should-not (null res))
              (should (json-plist-p res))
              (should (equal (jupyter-message-type res) "inspect_reply"))))
          (ert-info ("Complete")
            (let ((res (jupyter-wait-until-received client 'complete-reply
                         (jupyter-request-complete
                          client
                          :code "foo = lis"
                          :pos 8))))
              (should-not (null res))
              (should (json-plist-p res))
              (should (equal (jupyter-message-type res) "complete_reply"))))
          (ert-info ("History")
            (let ((res (jupyter-wait-until-received client 'history-reply
                         ;; TODO: "tail" -> 'tail
                         (jupyter-request-history
                          client :hist-access-type "tail" :n 2))))
              (should-not (null res))
              (should (json-plist-p res))
              (should (equal (jupyter-message-type res) "history_reply"))))
          (ert-info ("Is Complete")
            (let ((res (jupyter-wait-until-received client 'is-complete-reply
                         (jupyter-request-is-complete
                          client :code "for i in range(5):"))))
              (should-not (null res))
              (should (json-plist-p res))
              (should (equal (jupyter-message-type res) "is_complete_reply"))))
          (ert-info ("Interrupt")
            (lexical-let ((time (current-time))
                          (interrupt-time nil))
              (jupyter-add-receive-callback
                  client 'status (jupyter-request-execute
                                  client :code "import time\ntime.sleep(2)")
                (lambda (msg)
                  (when (jupyter-message-status-idle-p msg)
                    (setq interrupt-time (current-time)))))
              (sleep-for 0.2)
              (let ((res (jupyter-wait-until-received client 'interrupt-reply
                           (jupyter-request-interrupt client))))
                (should-not (null res))
                (should (json-plist-p res))
                (message "%s " res)
                (should (equal (jupyter-message-type res) "interrupt_reply"))
                (should (< (float-time (time-subtract interrupt-time time))
                           2)))))
          (ert-info ("Shutdown")
            (let ((res (jupyter-wait-until-received client 'shutdown-reply
                         (jupyter-request-shutdown client))))
              (should-not (null res))
              (should (json-plist-p res))
              (should (equal (jupyter-message-type res) "shutdown_reply")))))
      (jupyter-stop-channels client)
      (jupyter-stop-kernel client))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
