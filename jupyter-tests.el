;;; jupyter-tests.el --- Jupyter tests -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 08 Jan 2018
;; Version: 0.0.1
;; X-URL: https://github.com/nathan/jupyter-tests

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

(require 'jupyter-client)
(require 'jupyter-repl)
(require 'jupyter-kernel-manager)
(require 'cl-lib)
(require 'ert)

;; TODO: Required tests
;; - `jupyter-channels'
;; - `jupyter-messages'
;; - `jupyter-request' semantics
;;   - Ensure `jupyter-request-idle-received-p' gets set
;;   - Ensure `jupyter-request-run-handlers-p' actually prevents a handler from running
;; - IOLoop subprocess
;;   - Make sure all commands that can be sent to the subprocess work
;; - Jupyter REPL

(defclass jupyter-echo-client (jupyter-kernel-client)
  ((messages))
  :documentation "A client that echo's any messages sent back to
the channel the message was sent on. No communication is actually
done with a kernel. Every sent message on a channel is just
placed back into the channel's recv-queue. This is mainly for
testing the callback functionality of a
`jupyter-kernel-client'.")

(defun jupyter-test-message (req type content)
  "Return a bare bones message plist for REQ.
TYPE is the message type of the returned message. CONTENT is the
message contents. The actual return value is a cons cell as
expected by `jupyter-queue-message'."
  (cons "" (list :msg_id (jupyter-new-uuid)
                 :msg_type type
                 :parent_header (list :msg_id (jupyter-request-id req))
                 :content content)))

(cl-defmethod initialize-instance ((client jupyter-echo-client) &rest _slots)
  (cl-call-next-method)
  (oset client messages (make-ring 10))
  (cl-loop
   for channel in (list 'shell-channel
                        'iopub-channel
                        'hb-channel
                        'stdin-channel)
   unless (slot-value client channel)
   do (setf (slot-value client channel)
            (make-instance
             (intern (concat
                      "jupyter-"
                      (symbol-name channel)))
             :endpoint "foo://bar"))))

(cl-defmethod jupyter-send ((client jupyter-echo-client)
                            channel
                            type
                            message
                            &optional _flags)
  (let ((req (make-jupyter-request :id (jupyter-new-uuid))))
    (if (string-match "request" (symbol-name type))
        (setq type (intern (replace-match "reply" nil nil (symbol-name type))))
      (error "Not a request message type (%s)" type))
    ;; Message flow
    ;; - status: busy
    ;; - reply message
    ;; - status: idle
    (jupyter-queue-message
     (oref client iopub-channel)
     (jupyter-test-message req :status (list :execution_state "busy")))
    (jupyter-queue-message
     channel (jupyter-test-message req type message))
    (jupyter-queue-message
     (oref client iopub-channel)
     (jupyter-test-message req :status (list :execution_state "idle")))
    (run-at-time
     0.01 nil
     (lambda (client channel)
       (jupyter-handle-message client (oref client iopub-channel))
       (jupyter-handle-message client channel)
       (jupyter-handle-message client (oref client iopub-channel)))
     client channel)
    ;; Needed internally by a `jupyter-kernel-client', this is mainly handled
    ;; by the eventloop.
    (puthash (jupyter-request-id req) req (oref client requests))
    req))

(cl-defmethod jupyter-handle-message ((client jupyter-echo-client) channel)
  (ring-insert+extend (oref client messages)
                      (cdr (ring-ref (oref channel recv-queue) -1))
                      'grow)
  (cl-call-next-method))

(cl-defmacro jupyter-ert-info ((message-form &key ((:prefix prefix-form) "Info: "))
                                &body body)
  "Identical to `ert-info', but clear the REPL buffer before running BODY.
In a REPL buffer, the contents are erased and an input prompt is
inserted.

If the `current-buffer' is not a REPL, this is identical to
`ert-info'."
  (declare (debug ((form &rest [sexp form]) body))
           (indent 1))
  `(ert-info ((quote ,message-form) :prefix (quote ,prefix-form))
     ;; Clear the REPL buffer before each new test section, but do this only if
     ;; the current client is a REPL client
     (when (and jupyter-current-client
                (object-of-class-p jupyter-current-client
                                   'jupyter-repl-client)
                (eq (current-buffer)
                    (oref jupyter-current-client buffer)))
       (let ((inhibit-read-only t))
         (erase-buffer)
         (jupyter-test-repl-ret-sync)))
     ,@body))

(defmacro jupyter-with-echo-client (client &rest body)
  (declare (indent 1) (debug (symbolp &rest form)))
  `(let ((,client (jupyter-echo-client)))
     ,@body))

(defun jupyter-error-if-no-kernelspec (kernel)
  (prog1 kernel
    (unless (car (jupyter-find-kernelspecs
                  (regexp-quote kernel)))
      (error "Kernel not found (%s)" kernel))))

(defmacro jupyter-with-kernel-client (kernel client &rest body)
  "Start a new KERNEL client, bind it to CLIENT, evaluate BODY.
Cleanup the client and delete the kernel process after running
BODY."
  (declare (indent 2) (debug (stringp symbolp &rest form)))
  (let ((manager (make-symbol "--manager")))
    `(cl-destructuring-bind (,manager ,client)
         (jupyter-start-new-kernel
          (jupyter-error-if-no-kernelspec ,kernel))
       (unwind-protect
           (progn ,@body)
         (jupyter-finalize ,manager)
         (jupyter-finalize ,client)))))

(defmacro jupyter-with-python-client (client &rest body)
  "Start a new Python kernel, bind it to CLIENT, evaluate BODY.
Do cleanup of the kernel process afterwards."
  (declare (indent 1) (debug (symbolp &rest form)))
  `(jupyter-with-kernel-client "python" ,client
     ,@body))

(defmacro jupyter-with-kernel-repl (kernel client &rest body)
  "Start a new KERNEL REPL, bind the client to CLIENT, evaluate BODY.
Delete the REPL buffer after running BODY."
  (declare (indent 2) (debug (stringp symbolp &rest form)))
  `(let ((,client (jupyter-run-repl
                   (jupyter-error-if-no-kernelspec ,kernel))))
     (unwind-protect
         (jupyter-with-repl-buffer ,client
           (progn ,@body))
       (cl-letf (((symbol-function 'yes-or-no-p)
                  (lambda (_prompt) t))
                 ((symbol-function 'y-or-n-p)
                  (lambda (_prompt) t)))
         (kill-buffer (oref client buffer))))))

(defmacro jupyter-with-python-repl (client &rest body)
  "Start a new Python REPL and run BODY.
CLIENT is bound to the Python REPL. Delete the REPL buffer after
running BODY."
  (declare (indent 1) (debug (symbolp &rest form)))
  `(jupyter-with-kernel-repl "python" ,client
     ,@body))

(defun jupyter-test-wait-until-idle-repl (client)
  "Wait until the execution state of a REPL CLIENT is idle."
  (while (not (equal (oref client execution-state)
                     "idle"))
    (sleep-for 0.01)))

(defun jupyter-test-repl-ret-sync ()
  "A synchronous version of `jupyter-repl-ret'."
  ;; TODO: There are issues here since the kernel goes through multiple idle ->
  ;; busy cycles
  (jupyter-repl-ret)
  (jupyter-test-wait-until-idle-repl
   jupyter-current-client))

(ert-deftest jupyter-echo-client ()
  (jupyter-with-echo-client client
    (ert-info ("Mock echo client echo's messages back to channel.")
      (let* ((msg (jupyter-message-execute-request :code "foo"))
             (req (jupyter-send client (oref client shell-channel)
                                :execute-request msg)))
        (sleep-for 1.0)
        (setq msgs (nreverse (ring-elements (oref client messages))))
        (should (= (length msgs) 3))
        (should (equal (jupyter-message-type (first msgs)) :status))
        (should (equal (jupyter-message-parent-id (first msgs))
                       (jupyter-request-id req)))
        (should (equal (jupyter-message-get (first msgs) :execution_state) "busy"))
        (should (equal (jupyter-message-type (second msgs)) :execute-reply))
        (should (equal (jupyter-message-parent-id (second msgs))
                       (jupyter-request-id req)))
        (should (equal (jupyter-message-content (second msgs)) msg))
        (should (equal (jupyter-message-type (third msgs)) :status))
        (should (equal (jupyter-message-parent-id (third msgs))
                       (jupyter-request-id req)))
        (should (equal (jupyter-message-get (third msgs) :execution_state) "idle"))))))

(ert-deftest jupyter-callbacks ()
  (jupyter-with-echo-client client
    (let ((req (jupyter-send-execute-request client :code "foo")))
      (ert-info ("Blocking callbacks")
        (jupyter-wait-until-idle req)
        (should (jupyter-request-idle-received-p req)))
      (ert-info ("Error after idle message has been received")
        (should-error (jupyter-add-callback req :status #'identity))))
    (ert-info ("Multiple callbacks, same message type")
      (let* ((callback-count 0)
             (cb (lambda (msg)
                   (should (eq (jupyter-message-type msg) :status))
                   (setq callback-count (1+ callback-count))))
             (req (jupyter-send-execute-request client :code "foo")))
        (jupyter-add-callback req :status cb)
        (jupyter-wait-until-idle req)
        (should (= callback-count 2))))
    (ert-info ("Adding callbacks, message type list")
      (let* ((callback-count 0)
             (cb (lambda (msg)
                   (setq callback-count (1+ callback-count))
                   (should (memq (jupyter-message-type msg)
                                 '(:status :execute-reply)))))
             (req (jupyter-send-execute-request client :code "foo")))
        (jupyter-add-callback req '(:status :execute-reply) cb)
        (jupyter-wait-until-idle req)
        (should (= callback-count 3))))))

(ert-deftest jupyter-messages ()
  (ert-info ("Splitting identities from messages")
    (let ((msg (list "123" "323" jupyter-message-delimiter
                     "msg1" "msg2" "\0\0")))
      (should (equal (jupyter--split-identities msg)
                     (cons (list "123" "323")
                           (list "msg1" "msg2" "\0\0"))))
      (setq msg (list "123" "No" "delim" "in" "message"))
      (should-error (jupyter--split-identities msg))))
  (ert-info ("Creating message headers")
    (let* ((session (jupyter-session :key (jupyter-new-uuid)))
           (header (jupyter--message-header session :input-reply)))
      (should (plist-get header :msg_id))
      (should (plist-get header :date))
      (should (eq (plist-get header :msg_type) :input-reply))
      (should (string= (plist-get header :version) jupyter-protocol-version))
      (should (string= (plist-get header :username) user-login-name))
      (should (string= (plist-get header :session) (jupyter-session-id session)))))
  (ert-info ("Encoding/decoding time strings")
    (should (equal (jupyter--encode-time '(23385 27704 100000))
                   "2018-07-26T01:37:44.100000"))
    (should (equal (jupyter--decode-time "2018-07-26T01:37:44.100")
                   '(23385 27704 100000 0)))
    (should (equal (jupyter--decode-time "2018-07-26T01:37:44.10011122")
                   '(23385 27704 100111 0)))
    (should (equal (jupyter--decode-time "2018-07-26T01:37:44")
                   '(23385 27704 0 0)))
    (should (equal (jupyter--decode-time "2018-07-26")
                   '(23385 21840 0 0)))))

(ert-deftest jupyter-channel-subprocess-lock ()
  (ert-info ("Locking and unlocking a file")
    (with-temp-buffer
      (let ((create-lockfiles t))
        (setq buffer-file-name (expand-file-name "jupyter-lock-test")
              buffer-file-truename (file-truename buffer-file-name))
        (should (not (file-exists-p buffer-file-name)))
        (should (not (file-locked-p buffer-file-name)))
        (set-buffer-modified-p t)
        (should (file-locked-p buffer-file-name))
        (set-buffer-modified-p nil)
        (should (not (file-locked-p buffer-file-name)))
        (should (not (file-exists-p buffer-file-name))))))
  (ert-info ("`jupyter--ioloop-lock-file'")
    (let ((client (jupyter-kernel-client)))
      ;; Lock file names are based on session IDs
      (oset client session (jupyter-session))
      (unwind-protect
          (jupyter-with-client-buffer client
            (should (not buffer-file-name))
            (let ((lock (jupyter--ioloop-lock-file client)))
              (should (file-locked-p lock))
              (should (equal buffer-file-name lock))
              (ert-info ("Lock is acquired regardless of lock state")
                (should (file-locked-p lock))
                (should (equal (jupyter--ioloop-lock-file client) lock))
                (should (file-locked-p lock)))))
        (jupyter-finalize client)))))

(ert-deftest jupyter-channels ()
  (ert-info ("Channel types should match their class")
    (should (eq (oref (jupyter-shell-channel) type) :shell))
    (should (eq (oref (jupyter-stdin-channel) type) :stdin))
    (should (eq (oref (jupyter-iopub-channel) type) :iopub))
    (should (eq (oref (jupyter-hb-channel) type) :hb)))
  (ert-info ("Synchronous channels")
    (let ((channel (jupyter-sync-channel
                    :type :shell
                    :endpoint "tcp://127.0.0.1:5555")))
      (ert-info ("Starting the channel")
        (should-not (jupyter-channel-alive-p channel))
        (jupyter-start-channel channel :identity "foo")
        (should (jupyter-channel-alive-p channel))
        (should (equal (zmq-socket-get (oref channel socket)
                                       zmq-ROUTING-ID)
                       "foo")))
      (ert-info ("Stopping the channel")
        (let ((sock (oref channel socket)))
          (jupyter-stop-channel channel)
          (should-error (zmq-send sock "foo") :type 'zmq-ENOTSOCK)))))
  (ert-info ("Asynchronous channels")
    (let* ((channel (jupyter-async-channel
                     :type :shell
                     :endpoint "tcp://127.0.0.1:5555"))
           (events)
           (ioloop
            (zmq-start-process
             `(lambda ()
                (while t
                  (zmq-prin1 (zmq-subprocess-read))))
             :filter (lambda (event)
                       ;; FIXME: Remove dependence of the channel
                       ;; subprocess on having to set the channel's
                       ;; status slot
                       (when (eq (car event) 'start-channel)
                         (oset channel status 'running))
                       (when (eq (car event) 'stop-channel)
                         (oset channel status 'stopped))
                       (push event events)))))
      (unwind-protect
          (progn
            (ert-info ("Starting the channel")
              (oset channel ioloop ioloop)
              (should-not (jupyter-channel-alive-p channel))
              (jupyter-start-channel channel :identity "foo")
              (should (jupyter-channel-alive-p channel)))
            (jupyter-send channel :msg-type "msg" "id")
            (ert-info ("Stopping the channel")
              (jupyter-stop-channel channel)
              (should-not (jupyter-channel-alive-p channel)))
            (ert-info ("Channel events")
              (should (equal (pop events) '(stop-channel :shell)))
              (should (equal (pop events) '(send :shell :msg-type "msg" "id")))
              (should (equal (pop events) '(start-channel :shell "tcp://127.0.0.1:5555" "foo")))))
        (when (process-live-p ioloop)
          (kill-process ioloop)))))
  (ert-info ("Heartbeat channel")
    (let ((channel (jupyter-hb-channel :endpoint "tcp://127.0.0.1:5556"))
          (died-cb-called nil)
          (jupyter-hb-consider-dead-periods 1))
      (oset channel time-to-dead 0.1)
      (should-not (jupyter-channel-alive-p channel))
      (should-not (jupyter-hb-beating-p channel))
      (should (oref channel paused))
      (oset channel beating t)
      (jupyter-start-channel channel)
      (jupyter-hb-on-kernel-dead channel (lambda () (setq died-cb-called t)))
      (should (jupyter-channel-alive-p channel))
      (should-not (oref channel paused))
      (sleep-for 0.2)
      ;; It seems the timers are run after returning from the first `sleep-for'
      ;; call.
      (sleep-for 0.1)
      (should (oref channel paused))
      (should-not (oref channel beating))
      (should died-cb-called)
      (should (jupyter-channel-alive-p channel))
      (should-not (jupyter-hb-beating-p channel)))))

(ert-deftest jupyter-client ()
  (let* ((ports (cl-loop
                 with sock = (zmq-socket (zmq-current-context) zmq-PUB)
                 for c in '(:shell :hb :iopub :stdin :control)
                 collect c and
                 collect (zmq-bind-to-random-port sock "tcp://127.0.0.1")
                 finally (zmq-close sock)))
         (conn-info `(:shell_port
                      ,(plist-get ports :shell)
                      :key  "8671b7e4-5656e6c9d24edfce81916780"
                      :hb_port
                      ,(plist-get ports :hb)
                      :kernel_name "python"
                      :control_port
                      ,(plist-get ports :control)
                      :signature_scheme "hmac-sha256"
                      :ip "127.0.0.1"
                      :stdin_port
                      ,(plist-get ports :stdin)
                      :transport "tcp"
                      :iopub_port
                      ,(plist-get ports :iopub)))
         (client (jupyter-kernel-client)))
    (ert-info ("`jupyter-initialize-connection'")
      (jupyter-initialize-connection client conn-info)
      (let ((session (oref client session)))
        (should (string= (jupyter-session-key session)
                         (plist-get conn-info :key)))
        (should (eq session (oref (oref client shell-channel) session)))
        (should (eq session (oref (oref client stdin-channel) session)))
        (should (eq session (oref (oref client iopub-channel) session)))
        (should (eq session (oref (oref client hb-channel) session))))
      (should (string= (oref (oref client shell-channel) endpoint)
                       (format "tcp://127.0.0.1:%d" (plist-get ports :shell))))
      (should (string= (oref (oref client stdin-channel) endpoint)
                       (format "tcp://127.0.0.1:%d" (plist-get ports :stdin))))
      (should (string= (oref (oref client iopub-channel) endpoint)
                       (format "tcp://127.0.0.1:%d" (plist-get ports :iopub))))
      (should (string= (oref (oref client hb-channel) endpoint)
                       (format "tcp://127.0.0.1:%d" (plist-get ports :hb))))
      (ert-info ("Re-initializing stops running channels")
        (jupyter-start-channels client)
        (should (jupyter-channels-running-p client))
        (jupyter-initialize-connection client conn-info)
        (should-not (jupyter-channels-running-p client))
        (jupyter-start-channels client)))
    (ert-info ("`jupyter-inhibit-handlers'")
      (let* ((jupyter-inhibit-handlers '(:stream))
             (req (jupyter-send-kernel-info-request client)))
        (should (equal (jupyter-request-inhibited-handlers req)
                       '(:stream)))
        (setq jupyter-inhibit-handlers '(:foo))
        (should-error (jupyter-send-kernel-info-request client))))
    (ert-info ("Finalizer")
      (should (buffer-live-p (oref client -buffer)))
      (should (jupyter-channels-running-p client))
      (jupyter-finalize client)
      (should-not (jupyter-channels-running-p client))
      (should-not (buffer-live-p (oref client -buffer))))))

(ert-deftest jupyter-message-types ()
  (jupyter-with-python-client client
    (ert-info ("Kernel info")
      (let ((res (jupyter-wait-until-received :kernel-info-reply
                   (jupyter-send-kernel-info-request client))))
        (should res)
        (should (json-plist-p res))
        (should (eq (jupyter-message-type res) :kernel-info-reply))))
    (ert-info ("Comm info")
      (let ((res (jupyter-wait-until-received :comm-info-reply
                   (jupyter-send-comm-info-request client))))
        (should-not (null res))
        (should (json-plist-p res))
        (should (eq (jupyter-message-type res) :comm-info-reply))))
    (ert-info ("Execute")
      (let ((res (jupyter-wait-until-received :execute-reply
                   (jupyter-send-execute-request client :code "y = 1 + 2"))))
        (should-not (null res))
        (should (json-plist-p res))
        (should (eq (jupyter-message-type res) :execute-reply))))
    (ert-info ("Input")
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (_prompt &rest _args) "foo")))
        (let ((res (jupyter-wait-until-received :execute-result
                     (jupyter-send-execute-request client :code "input('')"))))
          (should-not (null res))
          (should (json-plist-p res))
          (should (eq (jupyter-message-type res) :execute-result))
          (should (equal (jupyter-message-data data :text/plain) "'foo'")))))
    (ert-info ("Inspect")
      (let ((res (jupyter-wait-until-received :inspect-reply
                   (jupyter-send-inspect-request client
                     :code "list((1, 2, 3))"
                     :pos 2
                     :detail 0))))
        (should-not (null res))
        (should (json-plist-p res))
        (should (eq (jupyter-message-type res) :inspect-reply))))
    (ert-info ("Complete")
      (let ((res (jupyter-wait-until-received :complete-reply
                   (jupyter-send-complete-request client
                     :code "foo = lis"
                     :pos 8))))
        (should-not (null res))
        (should (json-plist-p res))
        (should (eq (jupyter-message-type res) :complete-reply))))
    (ert-info ("History")
      (let ((res (jupyter-wait-until-received :history-reply
                   (jupyter-send-history-request client
                     :hist-access-type "tail" :n 2))))
        (should-not (null res))
        (should (json-plist-p res))
        (should (eq (jupyter-message-type res) :history-reply))))
    (ert-info ("Is Complete")
      (let ((res (jupyter-wait-until-received :is-complete-reply
                   (jupyter-send-is-complete-request client
                     :code "for i in range(5):"))))
        (should-not (null res))
        (should (json-plist-p res))
        (should (eq (jupyter-message-type res) :is-complete-reply))))
    (ert-info ("Shutdown")
      (let ((res (jupyter-wait-until-received :shutdown-reply
                   (jupyter-send-shutdown-request client))))
        (should-not (null res))
        (should (json-plist-p res))
        (should (eq (jupyter-message-type res) :shutdown-reply))))))

(ert-deftest jupyter-completion ()
  (ert-info ("`jupyter-completion-number-p'")
    (with-temp-buffer
      (insert "0.311")
      (should (jupyter-completion-number-p))
      (erase-buffer)
      (insert "0311")
      (should (jupyter-completion-number-p))
      (erase-buffer)
      (insert "0311.")
      (should (jupyter-completion-number-p))
      (erase-buffer)
      (insert "100foo")
      (should-not (jupyter-completion-number-p))
      (erase-buffer)
      (insert "foo100")
      (should-not (jupyter-completion-number-p))))
  (ert-info ("`jupyter-completion-prefetch-p'")
    (let ((jupyter-completion-cache '("foo")))
      (ert-info ("Prefetch when the cached prefix is more specialized")
        (should (jupyter-completion-prefetch-p "f"))
        (should (jupyter-completion-prefetch-p "")))
      (ert-info ("Don't prefetch when the cached prefix is less specialized")
        (should-not (jupyter-completion-prefetch-p "foo"))
        (should-not (jupyter-completion-prefetch-p "foobar")))
      (ert-info ("Prefetch when starting argument lists")
        (should (jupyter-completion-prefetch-p "foobar("))))
    (let ((jupyter-completion-cache '("")))
      (ert-info ("Prefetch when given some context")
        (should-not (jupyter-completion-prefetch-p ""))
        (should (jupyter-completion-prefetch-p "a"))))
    (let ((jupyter-completion-cache '(fetched "")))
      (ert-info ("Prefetch when not processed")
        (should (jupyter-completion-prefetch-p "a"))
        ;; But only if the fetched candidates do not match
        ;; the prefix
        (should-not (jupyter-completion-prefetch-p ""))
        (setq jupyter-completion-cache nil)
        (should (jupyter-completion-prefetch-p ""))
        (should (jupyter-completion-prefetch-p "a"))))))

(ert-deftest jupyter-repl ()
  (jupyter-with-python-repl client
    (should (jupyter-repl-client-has-manager-p))
    (should (jupyter-repl-connected-p))
    (ert-info ("Replacing cell code")
      (should (equal (jupyter-repl-cell-code) ""))
      (jupyter-repl-replace-cell-code "1 + 1")
      (should (equal (jupyter-repl-cell-code) "1 + 1"))
      (jupyter-repl-replace-cell-code "foo\n bar")
      (should (equal (jupyter-repl-cell-code) "foo\n bar"))
      (jupyter-repl-replace-cell-code ""))
    (ert-info ("Cell code position info")
      (jupyter-repl-replace-cell-code "1 + 2")
      (should (= (point) (point-max)))
      (goto-char (1- (point)))
      (should (= (char-after) ?2))
      (should (= (jupyter-repl-cell-code-position) 5))
      (goto-char (line-beginning-position))
      (should (= (char-after) ?1))
      (should (= (jupyter-repl-cell-code-position) 1)))
    (ert-info ("`jupyter-repl-ret'")
      (ert-info ("`point' before last cell in buffer")
        (let ((tick (buffer-modified-tick)))
          (goto-char (point-min))
          (jupyter-test-repl-ret-sync)
          (should (= (point) (point-max)))
          (should (equal tick (buffer-modified-tick)))))
      (ert-info ("No cells in buffer")
        (let ((inhibit-read-only t))
          (erase-buffer))
        (jupyter-test-repl-ret-sync)
        (should (get-text-property (- (point) 2) 'jupyter-cell))))))

(defun jupyter-test-set-dummy-repl-history ()
  "Reset `jupyter-repl-history' to a value used for testing.
The history contains the elements \"1\", \"2\", and \"3\", the
last element being the newest element added to the history."
  (setq-local jupyter-repl-history (make-ring 5))
  (ring-insert jupyter-repl-history 'jupyter-repl-history)
  (jupyter-repl-history-add-input "1")
  (jupyter-repl-history-add-input "2")
  (jupyter-repl-history-add-input "3"))

(ert-deftest jupyter-repl-history ()
  (jupyter-with-python-repl client
    (ert-info ("Rotating REPL history ring")
      (jupyter-test-set-dummy-repl-history)
      (should (null (jupyter-repl-history--next 1)))
      (should (equal (jupyter-repl-history--next 0) "3"))
      (should (equal (jupyter-repl-history--previous 1) "2"))
      (should (equal (jupyter-repl-history--next 1) "3"))
      (should (null (jupyter-repl-history--previous 4)))
      (should (equal (ring-ref jupyter-repl-history 0) "1")))
    (ert-info ("Replacing cell contents with history")
      (jupyter-test-set-dummy-repl-history)
      (should (equal (jupyter-repl-cell-code) ""))
      (jupyter-repl-history-previous)
      (should (equal (jupyter-repl-cell-code) "3"))
      (jupyter-repl-history-previous)
      (should (equal (jupyter-repl-cell-code) "2"))
      (jupyter-repl-history-previous)
      (should (equal (jupyter-repl-cell-code) "1"))
      (should-error (jupyter-repl-history-previous))
      (should (equal (jupyter-repl-cell-code) "1"))
      (jupyter-repl-history-next)
      (should (equal (jupyter-repl-cell-code) "2"))
      (jupyter-repl-history-next)
      (should (equal (jupyter-repl-cell-code) "3"))
      (jupyter-repl-history-next)
      (should (equal (jupyter-repl-cell-code) ""))
      (should-error (jupyter-repl-history-next))
      (should (equal (jupyter-repl-cell-code) "")))))

(ert-deftest jupyter-repl-cell-motions ()
  (jupyter-with-python-repl client
    (jupyter-ert-info ("`jupyter-repl-goto-cell'")
      (let (cell-pos req)
        (setq cell-pos (jupyter-repl-cell-beginning-position))
        (jupyter-test-repl-ret-sync)
        (save-excursion
          (goto-char cell-pos)
          (setq req (jupyter-repl-cell-request)))
        (jupyter-test-repl-ret-sync)
        (should (/= (point) cell-pos))
        (jupyter-repl-goto-cell req)
        (should (= (point) cell-pos))))
    (jupyter-ert-info ("`jupyter-repl-previous-cell'")
      (let (cell-pos1)
        (setq cell-pos1 (jupyter-repl-cell-beginning-position))
        (goto-char cell-pos1)
        (ert-info ("First motion to beginning of current cell")
          (jupyter-repl-replace-cell-code "1 + 1")
          (should (/= (point) cell-pos1))
          (should (= (jupyter-repl-previous-cell) 0))
          (should (= (point) cell-pos1))
          (jupyter-repl-replace-cell-code ""))
        (ert-info ("Motion with count")
          (jupyter-test-repl-ret-sync)
          (jupyter-test-repl-ret-sync)
          (goto-char (jupyter-repl-cell-beginning-position))
          (should (= (jupyter-repl-previous-cell 2) 0))
          (should (= (point) cell-pos1)))
        (ert-info ("First cell of buffer")
          (goto-char cell-pos1)
          (should (= (jupyter-repl-previous-cell) 1))
          (should (= (point) (point-min))))))
    (jupyter-ert-info ("`jupyter-repl-backward-cell'")
      (let (cell-pos1)
        (setq cell-pos1 (jupyter-repl-cell-code-beginning-position))
        (jupyter-test-repl-ret-sync)
        (should-not (= (point) cell-pos1))
        (jupyter-repl-backward-cell)
        (should (= (point) cell-pos1))))
    (jupyter-ert-info ("`jupyter-repl-next-cell'")
      (let (cell-pos1 cell-pos2)
        (setq cell-pos1 (jupyter-repl-cell-beginning-position))
        (ert-info ("Motion with count")
          (jupyter-test-repl-ret-sync)
          (jupyter-test-repl-ret-sync)
          (setq cell-pos2 (jupyter-repl-cell-beginning-position))
          (goto-char cell-pos1)
          (should (= (jupyter-repl-next-cell 2) 0))
          (should (= (point) cell-pos2)))
        (ert-info ("Last cell of buffer")
          (goto-char cell-pos2)
          (should (= (jupyter-repl-next-cell) 1))
          (should (= (point) (point-max))))))
    (jupyter-ert-info ("`jupyter-repl-forward-cell'")
      (let (cell-pos1 cell-pos2)
        (setq cell-pos1 (jupyter-repl-cell-code-beginning-position))
        (jupyter-test-repl-ret-sync)
        (setq cell-pos2 (jupyter-repl-cell-code-beginning-position))
        (goto-char cell-pos1)
        (jupyter-repl-forward-cell)
        (should (= (point) cell-pos2))))))

(ert-deftest jupyter-repl-cell-positions ()
  (jupyter-with-python-repl client
    (ert-info ("Beginning of a cell")
      (should (= (point) (jupyter-repl-cell-code-beginning-position)))
      (should (get-text-property (- (point) 2) 'jupyter-cell))
      (should (jupyter-repl-cell-beginning-p (- (point) 2)))
      (should (= (jupyter-repl-cell-beginning-position) (- (point) 2))))
    (ert-info ("End of unfinalized cell")
      (should-not (jupyter-repl-cell-finalized-p))
      (should-not (get-text-property (point-max) 'jupyter-cell))
      (should (= (jupyter-repl-cell-end-p (point-max))))
      (should (= (jupyter-repl-cell-end-position) (point-max)))
      (should (= (jupyter-repl-cell-code-end-position) (point-max))))
    (ert-info ("End of finalized cell")
      (jupyter-test-repl-ret-sync)
      (should (= (point) (jupyter-repl-cell-code-beginning-position)))
      (goto-char (1- (jupyter-repl-cell-beginning-position)))
      (should (jupyter-repl-cell-end-p))
      (should (= (jupyter-repl-cell-end-position) (point)))
      (should (= (jupyter-repl-cell-code-end-position) (1- (point))))
      (should (jupyter-repl-cell-finalized-p)))
    (ert-info ("Cell boundary errors")
      (goto-char (point-max))
      (jupyter-repl-replace-cell-code "1 + 1")
      (jupyter-wait-until-idle (jupyter-send-execute-request client))
      (forward-line -2)
      (should (eq (car (get-text-property (1- (point)) 'jupyter-cell))
                  'out))
      (should-error (jupyter-repl-cell-beginning-position))
      (should-error (jupyter-repl-cell-end-position)))))

(ert-deftest jupyter-repl-prompts ()
  (jupyter-with-python-repl client
    (ert-info ("Prompt properties")
      (let (prompt-overlay)
        (goto-char (jupyter-repl-cell-beginning-position))
        (setq prompt-overlay (car (overlays-at (point))))
        (should-not (null prompt-overlay))
        (should (equal (get-text-property (point) 'jupyter-cell) '(beginning 1)))
        (should (= (jupyter-repl-cell-count) 1))))
    (ert-info ("Input prompts")
      (goto-char (jupyter-repl-cell-code-beginning-position))
      ;; To prevent prompts from inheriting text properties of cell code there is
      ;; an invisible character at the end of every prompt. This is because
      ;; prompts are implemented as overlays and therefore will inherit the text
      ;; properties of adjacent text, we want to prevent that.
      (should (invisible-p (1- (point))))
      (should (jupyter-repl-cell-beginning-p (- (point) 2)))
      (should (eq (char-after (- (point) 2)) ?\n))
      (let* ((props (text-properties-at (- (point) 2)))
             (cell-property (memq 'jupyter-cell props)))
        (should (not (null cell-property)))
        (should (listp (cdr cell-property)))
        (should (equal (cadr cell-property) '(beginning 1)))))
    (ert-info ("Continuation prompts")

      )
    (ert-info ("Output prompts")

      )))

(defun jupyter-test-src-block (session code test-result)
  (let ((pos (point)))
    (insert
     "#+BEGIN_SRC jupyter-python " ":session " session "\n"
     code "\n"
     "#+END_SRC")
    (let* ((info (org-babel-get-src-block-info)))
      (org-babel-execute-src-block nil info)
      (org-with-point-at (org-babel-where-is-src-block-result nil info)
        (forward-line 1)
        (let ((result
               (string-trim-right
                (buffer-substring-no-properties
                 (point) (goto-char (org-babel-result-end))))))
          (should (equal result test-result))
          (delete-region pos (point)))))))

(defvar org-babel-jupyter-resource-directory nil)

(ert-deftest org-babel-jupyter ()
  (ert-info ("Dynamic result types")
    (let ((session (make-temp-name "ob-jupyter-test")) repl-buffer)
      (unwind-protect
          (with-temp-buffer
            (org-mode)
            (insert
             "#+BEGIN_SRC jupyter-python " ":session " session "\n"
             "#+END_SRC")
            (setq repl-buffer (org-babel-initiate-session))
            (erase-buffer)
            (ert-info ("Scalar results")
              (jupyter-test-src-block session "1 + 1" ": 2"))
            (ert-info ("HTML results")
              (let ((code "\
from IPython.core.display import HTML\n\
HTML('<a href=\"http://foo.com\">link</a>')"))
                (jupyter-test-src-block session code "\
#+BEGIN_EXPORT html
<a href=\"http://foo.com\">link</a>
#+END_EXPORT")))
            (ert-info ("Image results")
              (let* ((default-directory (file-name-directory
                                         (locate-library "jupyter")))
                     (org-babel-jupyter-resource-directory "./")
                     (file (expand-file-name "jupyter.png"))
                     (data (let ((buffer-file-coding-system 'binary))
                             (with-temp-buffer
                               (set-buffer-multibyte nil)
                               (insert-file-contents-literally file)
                               (base64-encode-region (point-min) (point-max) t)
                               (goto-char (point-max))
                               (insert "\n")
                               (buffer-substring-no-properties (point-min) (point-max)))))
                     (image-file-name (org-babel-jupyter-file-name data "png"))
                     (code (format "\
from IPython.display import Image
Image(filename='%s')" file)))
                (unwind-protect
                    (jupyter-test-src-block session code (format "[[file:%s]]" image-file-name))
                  (when (file-exists-p image-file-name)
                    (delete-file image-file-name))))))
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (_prompt) t))
                  ((symbol-function 'y-or-n-p)
                   (lambda (_prompt) t)))
          (when repl-buffer
            (kill-buffer repl-buffer)))))))

;;; jupyter-tests.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
