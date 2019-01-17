;;; jupyter-test.el --- Jupyter tests -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 08 Jan 2018
;; Version: 0.6.0

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

(require 'zmq)
(require 'jupyter-client)
(require 'jupyter-repl)
(require 'jupyter-org-client)
(require 'jupyter-kernel-manager)
(require 'cl-lib)
(require 'ert)

(declare-function org-babel-python-table-or-string "ob-python" (results))

;; TODO: Required tests
;; - Jupyter REPL

;;; Mock

(ert-deftest jupyter-echo-client ()
  :tags '(mock)
  (jupyter-with-echo-client client
    (ert-info ("Mock echo client echo's messages back to channel.")
      (let* ((msg (jupyter-message-execute-request :code "foo"))
             (req (jupyter-send client :shell :execute-request msg)))
        (sleep-for 0.3)
        (setq msgs (nreverse (ring-elements (oref client messages))))
        (should (= (length msgs) 3))
        (should (equal (jupyter-message-type (car msgs)) :status))
        (should (equal (jupyter-message-parent-id (car msgs))
                       (jupyter-request-id req)))
        (should (equal (jupyter-message-get (car msgs) :execution_state) "busy"))
        (should (equal (jupyter-message-type (cadr msgs)) :execute-reply))
        (should (equal (jupyter-message-parent-id (cadr msgs))
                       (jupyter-request-id req)))
        (should (equal (jupyter-message-content (cadr msgs)) msg))
        (should (equal (jupyter-message-type (caddr msgs)) :status))
        (should (equal (jupyter-message-parent-id (caddr msgs))
                       (jupyter-request-id req)))
        (should (equal (jupyter-message-get (caddr msgs) :execution_state) "idle"))))))

;;; Callbacks

(ert-deftest jupyter-wait-until-idle ()
  :tags '(callbacks)
  (jupyter-with-echo-client client
    (let ((req (jupyter-send-execute-request client :code "foo")))
      (ert-info ("Blocking callbacks")
        (jupyter-wait-until-idle req)
        (should (jupyter-request-idle-received-p req)))
      (ert-info ("Error after idle message has been received")
        (should-error (jupyter-add-callback req :status #'identity))))))

(ert-deftest jupyter-callbacks ()
  :tags '(callbacks)
  (jupyter-with-echo-client client
    (ert-info ("Callbacks called on the right message types")
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

;;; `jupyter-insert'

(ert-deftest jupyter-loop-over-mime ()
  :tags '(mime)
  (let ((mimes '(:text/html :text/plain))
        (data (list :text/plain "foo"))
        (metadata nil))
    (ert-info ("No iterations without MIME data")
      (jupyter-loop-over-mime mimes mime data metadata
        (should-not (eq mime :text/html))
        (should (eq mime :text/plain))
        (should (equal data "foo"))
        (should (eq metadata nil))))))

(defvar jupyter-nongraphic-mime-types)

(ert-deftest jupyter-insert ()
  "Test the `jupyter-insert' method."
  :tags '(mime)
  (with-temp-buffer
    (let ((msg (list :data (list :text/plain "foo")
                     :metadata nil)))
      (ert-info ("Return value is the mimetype inserted")
        (should (eq (jupyter-insert msg) :text/plain))
        (should (equal (buffer-string) "foo\n"))
        (erase-buffer))
      (ert-info ("Return nil on invalid mimetype")
        (should-not (jupyter-insert :text/foo "bar"))
        (should-not (jupyter-insert (list :data (list :text/foo "bar")))))
      (ert-info ("Calling with data plist directly")
        (should (eq (jupyter-insert (plist-get msg :data)) :text/plain))
        (should (equal (buffer-string) "foo\n"))
        (erase-buffer))
      (ert-info ("Calling with message plist directly")
        (should (eq (jupyter-insert msg) :text/plain))
        (should (equal (buffer-string) "foo\n"))
        (erase-buffer)))
    (let ((msg (list :data (list :text/plain "foo"
                                 :text/html "<b>bar</b>")
                     :metadata nil)))
      (ert-info ("Mimetype priority")
        (should (eq (jupyter-insert msg) :text/html))
        (should (equal (string-trim (buffer-string)) "bar"))
        (erase-buffer)))
    (let ((data (list :image/jpeg (base64-encode-string "kjdaljk")))
          ;; So that this test runs under ert-runner
          (jupyter-nongraphic-mime-types jupyter-mime-types))
      (ert-info ("The right method specializers are called")
        (cl-letf (((symbol-function #'jupyter-insert-image)
                   (lambda (data &rest _) (insert data))))
          (cl-letf (((symbol-function #'image-type-available-p)
                     (lambda (_typ) nil)))
            (should-not (jupyter-insert data)))
          (should (eq (jupyter-insert data) :image/jpeg))
          (should (equal (buffer-string) "kjdaljk\n"))
          (erase-buffer))))))

(defun jupyter-test-display-id-all (id beg end)
  (not (text-property-not-all beg end 'jupyter-display id)))

(ert-deftest jupyter-insert-with-ids ()
  :tags '(mime display-id)
  (with-temp-buffer
    (let ((id "1")
          (msg (list :data (list :text/plain "foo"))))
      (ert-info ("`jupyter-display-ids' initialization")
        (should-not jupyter-display-ids)
        (should (eq (jupyter-insert id msg) :text/plain))
        (should (hash-table-p jupyter-display-ids))
        (should (equal (buffer-string) "foo\n")))
      (ert-info ("`jupyter-display-ids' is updated with ID")
        (should (not (null (gethash id jupyter-display-ids)))))
      (ert-info ("IDs are `eq'")
        ;; This is done so that they are comparable as text properties.
        (should (eq (gethash id jupyter-display-ids) id)))
      (ert-info ("Text property added to inserted text")
        (should (jupyter-test-display-id-all id (point-min) (point-max)))))))

(ert-deftest jupyter-delete-current-display ()
  :tags '(mime display-id)
  (with-temp-buffer
    (let ((id1 "1")
          (id2 "2")
          (msg (list :data (list :text/plain "foo"))))
      (ert-info ("Actually deletes text with display ID")
        (jupyter-insert id1 msg)
        (should (equal (buffer-string) "foo\n"))
        (goto-char (point-min))
        (jupyter-delete-current-display)
        (should (= (point-min) (point-max))))
      (ert-info ("Does not do anything if no display ID at point")
        (insert "bar")
        (goto-char (point-min))
        (jupyter-delete-current-display)
        (should (equal (buffer-string) "bar"))
        (erase-buffer))
      (ert-info ("Deletes only text with the same display ID")
        (jupyter-insert id1 msg)
        (jupyter-insert id2 msg)
        (goto-char (point-min))
        (jupyter-delete-current-display)
        (should (equal (buffer-string) "foo\n"))
        (should (jupyter-test-display-id-all id2 (point-min) (point-max)))
        (erase-buffer)))))

(ert-deftest jupyter-update-display ()
  :tags '(mime display-id)
  (with-temp-buffer
    (let ((id1 "1")
          (id2 "2")
          (msg1 (list :data (list :text/plain "foo")))
          (msg2 (list :data (list :text/plain "bar"))))
      (ert-info ("Text with matching display ID is actually updated")
        (jupyter-insert id1 msg1)
        (jupyter-insert id2 msg2)
        (should (equal (buffer-string) "foo\nbar\n"))
        (should (jupyter-test-display-id-all
                 id1 (point-min) (+ 4 (point-min))))
        (should (jupyter-test-display-id-all
                 id2 (- (point-max) 4) (point-max)))
        (jupyter-update-display "1" (list :data (list :text/plain "baz")))
        (should (equal (buffer-string) "baz\nbar\n"))
        (should (jupyter-test-display-id-all
                 id1 (point-min) (+ 4 (point-min))))
        (should (jupyter-test-display-id-all
                 id2 (- (point-max) 4) (point-max)))
        (erase-buffer))
      (ert-info ("All displays are updated")
        (jupyter-insert id1 msg1)
        (jupyter-insert id1 msg1)
        (pop-to-buffer (current-buffer))
        (should (equal (buffer-string) "foo\nfoo\n"))
        (should (jupyter-test-display-id-all
                 id1 (point-min) (point-max)))
        (jupyter-update-display "1" (list :data (list :text/plain "baz")))
        (should (equal (buffer-string) "baz\nbaz\n"))
        (should (jupyter-test-display-id-all
                 id1 (point-min) (point-max)))))))

(ert-deftest jupyter-insert-html ()
  :tags '(mime)
  (ert-info ("Correct libxml parser is called depending on prolog")
    (ert-info ("XML prolog means to parse as XML")
      (with-temp-buffer
        (cl-letf* ((xml-parser-called nil)
                   ((symbol-function #'libxml-parse-xml-region)
                    (lambda (&rest _)
                      (prog1 nil
                        (setq xml-parser-called t)))))
          (jupyter-insert :text/html "<?xml version=\"1.0\" encoding=\"UTF-8\"?><p>hello</p>")
          (should xml-parser-called))))
    (ert-info ("Parse as html")
      (with-temp-buffer
        (cl-letf* ((html-parser-called nil)
                   ((symbol-function #'libxml-parse-html-region)
                    (lambda (&rest _)
                      (prog1 nil
                        (setq html-parser-called t)))))
          (jupyter-insert :text/html "<p>hello</p>")
          (should html-parser-called))))))

;;; Messages

(ert-deftest  jupyter-message-identities ()
  :tags '(messages)
  (let ((msg (list "123" "323" jupyter-message-delimiter
                   "msg1" "msg2" "\0\0")))
    (should (equal (jupyter--split-identities msg)
                   (cons (list "123" "323")
                         (list "msg1" "msg2" "\0\0"))))
    (setq msg (list "123" "No" "delim" "in" "message"))
    (should-error (jupyter--split-identities msg))))

(ert-deftest jupyter-message-headers ()
  :tags '(messages)
  (let* ((session (jupyter-session :key (jupyter-new-uuid)))
         (id (jupyter-new-uuid))
         (header (jupyter--message-header session :input-reply id)))
    (should (plist-get header :msg_id))
    (should (plist-get header :date))
    (should (eq (plist-get header :msg_type) :input-reply))
    (should (string= (plist-get header :version) jupyter-protocol-version))
    (should (string= (plist-get header :username) user-login-name))
    (should (string= (plist-get header :session) (jupyter-session-id session)))))

(ert-deftest jupyter-message-time ()
  :tags '(messages)
  (let ((tz (getenv "TZ")))
    (setenv "TZ" "UTC0")
    (should (equal (jupyter--encode-time '(23385 27704 100000))
                   "2018-07-26T06:37:44.100000"))
    (should (equal (jupyter--decode-time "2018-07-26T01:37:44.100")
                   '(23385 9704 100000 0)))
    (should (equal (jupyter--decode-time "2018-07-26T01:37:44.10011122")
                   '(23385 9704 100111 0)))
    (should (equal (jupyter--decode-time "2018-07-26T01:37:44")
                   '(23385 9704 0 0)))
    (should (equal (jupyter--decode-time "2018-07-26")
                   '(23385 3840 0 0)))
    (setenv "TZ" tz)))

(ert-deftest jupyter-message-signing ()
  :tags '(messages)
  (let ((session (jupyter-session :key "foo"))
        (msg (list "" "{\"msg_id\":\"1\",\"msg_type\":\"execute_reply\"}" "{}" "{}" "{}"))
        (signature "f9080fb30e80a1b424895b557b8249157d5f83d6fc897cb96a4d2fa54a1280e6"))
    (should (equal signature
                   (jupyter-sign-message session msg #'jupyter-hmac-sha256)))))

(ert-deftest jupyter-message-decoding ()
  :tags '(messages)
  (let ((session (jupyter-session)))
    (ert-info ("Minimum message length")
      (should-error (jupyter-decode-message session (list "1" "2" "3"))))
    (ert-info ("Form of decoded message")
      (let* ((session (jupyter-session :key "foo"))
             (msg (list "f9080fb30e80a1b424895b557b8249157d5f83d6fc897cb96a4d2fa54a1280e6"
                        "{\"msg_id\":\"1\",\"msg_type\":\"execute_reply\"}" "{}" "{}" "{}"))
             (plist (jupyter-decode-message session msg)))
        (cl-loop
         with true-msg = (list
                          :header '(message-part
                                    "{\"msg_id\":\"1\",\"msg_type\":\"execute_reply\"}"
                                    (:msg_id "1" :msg_type :execute-reply))
                          :msg_id "1"
                          :msg_type :execute-reply
                          :parent_header '(message-part "{}" nil)
                          :content '(message-part "{}" nil)
                          :metadata '(message-part "{}" nil)
                          :buffers nil)
         for key in '(:header :msg_id :msg_type :content
                              :parent_header :metadata :buffers)
         do (should (equal (plist-get true-msg key) (plist-get plist key))))))))

(ert-deftest jupyter-message-encoding ()
  :tags '(messages)
  ;; TODO
  )

(ert-deftest jupyter-message-types ()
  :tags '(client messages)
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
          (should (equal (jupyter-message-data res :text/plain) "'foo'")))))
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

;;; Channels

(ert-deftest jupyter-sync-channel ()
  :tags '(channels)
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

(ert-deftest jupyter-hb-channel ()
  :tags '(channels)
  (should (eq (oref (jupyter-hb-channel) type) :hb))
  (let ((channel (jupyter-hb-channel
                  :endpoint "tcp://127.0.0.1:5556"
                  :session (jupyter-session)))
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
    ;; `jupyter-hb-unpause' needs to explicitly called
    (should (oref channel paused))
    (jupyter-hb-unpause channel)
    (sleep-for 0.2)
    ;; It seems the timers are run after returning from the first `sleep-for'
    ;; call.
    (sleep-for 0.1)
    (should (oref channel paused))
    (should-not (oref channel beating))
    (should died-cb-called)
    (should (jupyter-channel-alive-p channel))
    (should-not (jupyter-hb-beating-p channel))))

;; (ert-deftest jupyter-weak-references ()
;;   ;; Skip this test always. It is here as a reference for intended behavior.
;;   ;; The garbage collector is non-deterministic and I haven't been able to get
;;   ;; it to reliably clean up the objects. Under normal operating conditions,
;;   ;; this works as intended.
;;   (let ((ref (make-hash-table :size 1 :weakness 'value)))
;;     (jupyter-with-python-client client
;;       (puthash t client ref))
;;     (garbage-collect))
;;   (garbage-collect)
;;   (should (= (length (jupyter-clients)) 0))
;;   (should (= (length (jupyter-kernel-managers)) 0)))

;;; Client

;; TODO: Different values of the session argument
(ert-deftest jupyter-initialize-connection ()
  :tags '(client init)
  (let ((conn-info (jupyter-test-conn-info-plist))
        (client (jupyter-kernel-client)))
    (jupyter-initialize-connection client conn-info)
    (with-slots (session channels) client
      (ert-info ("Client session")
        (should (string= (jupyter-session-key session)
                         (plist-get conn-info :key)))
        (should (equal (jupyter-session-conn-info session)
                       conn-info)))
      (ert-info ("Heartbeat channel initialized")
        (should (eq session (oref (plist-get channels :hb) session)))
        (should (string= (oref (plist-get channels :hb) endpoint)
                         (format "tcp://127.0.0.1:%d"
                                 (plist-get conn-info :hb_port)))))
      (ert-info ("Shell, iopub, stdin initialized")
        (cl-loop
         for channel in '(:shell :iopub :stdin)
         for port_sym = (intern (concat (symbol-name channel) "_port"))
         do
         (should (plist-member (plist-get channels channel) :alive-p))
         (should (plist-member (plist-get channels channel) :endpoint))
         (should
          (string= (plist-get (plist-get channels channel) :endpoint)
                   (format "tcp://127.0.0.1:%d"
                           (plist-get conn-info port_sym))))))
      (ert-info ("Initialization stops any running channels")
        (should-not (jupyter-channels-running-p client))
        (jupyter-start-channels client)
        (should (jupyter-channels-running-p client))
        (jupyter-initialize-connection client conn-info)
        (should-not (jupyter-channels-running-p client)))
      (ert-info ("Invalid signature scheme")
        (plist-put conn-info :signature_scheme "hmac-foo")
        (should-error (jupyter-initialize-connection client conn-info))))))

(ert-deftest jupyter-client-channels ()
  :tags '(client channels)
  (ert-info ("Starting/stopping channels")
    (let ((conn-info (jupyter-test-conn-info-plist))
          (client (jupyter-kernel-client)))
      (jupyter-initialize-connection client conn-info)
      (cl-loop
       for channel in '(:hb :shell :iopub :stdin)
       do (should-not (jupyter-channel-alive-p client channel)))
      (jupyter-start-channels client)
      (cl-loop
       for channel in '(:hb :shell :iopub :stdin)
       do (should (jupyter-channel-alive-p client channel)))
      (jupyter-stop-channels client)
      (cl-loop
       for channel in '(:hb :shell :iopub :stdin)
       do (should-not (jupyter-channel-alive-p client channel))))))

(ert-deftest jupyter-inhibited-handlers ()
  :tags '(client handlers)
  (jupyter-with-python-client client
    (let* ((jupyter-inhibit-handlers '(:stream))
           (req (jupyter-send-kernel-info-request client)))
      (should (equal (jupyter-request-inhibited-handlers req)
                     '(:stream)))
      (should-not (jupyter--run-handler-p
                   req (jupyter-test-message
                        req :stream (list :name "stdout" :text "foo"))))
      (setq jupyter-inhibit-handlers '(:foo))
      (should-error (jupyter-send-kernel-info-request client)))))

;;; IOloop

(ert-deftest jupyter-ioloop-lifetime ()
  :tags '(ioloop)
  (let ((ioloop (jupyter-ioloop))
        (jupyter-default-timeout 2))
    (should-not (process-live-p (oref ioloop process)))
    (jupyter-ioloop-start ioloop :tag1)
    (should (equal (jupyter-ioloop-last-event ioloop) '(start)))
    (with-slots (process) ioloop
      (should (process-live-p process))
      (jupyter-ioloop-stop ioloop)
      (should (equal (jupyter-ioloop-last-event ioloop) '(quit)))
      (sleep-for 0.1)
      (should-not (process-live-p process)))))

(defvar jupyter-ioloop-test-handler-called nil
  "Flag variable used for testing the `juyter-ioloop'.")

(cl-defmethod jupyter-ioloop-handler ((_ioloop jupyter-ioloop)
                                      (_tag (eql :test))
                                      (event (head test)))
  (should (equal (cadr event) "message"))
  (setq jupyter-ioloop-test-handler-called t))

(ert-deftest jupyter-ioloop-callbacks ()
  :tags '(ioloop)
  (ert-info ("Callback added before starting the ioloop")
    (let ((ioloop (jupyter-ioloop)))
      (setq jupyter-ioloop-test-handler-called nil)
      (jupyter-ioloop-add-callback ioloop
        `(lambda () (zmq-prin1 (list 'test "message"))))
      (jupyter-ioloop-start ioloop :test)
      (jupyter-ioloop-stop ioloop)
      (should jupyter-ioloop-test-handler-called)))
  (ert-info ("Callback added after starting the ioloop")
    (let ((ioloop (jupyter-ioloop)))
      (setq jupyter-ioloop-test-handler-called nil)
      (jupyter-ioloop-start ioloop :test)
      (jupyter-ioloop-add-callback ioloop
        `(lambda () (zmq-prin1 (list 'test "message"))))
      (jupyter-ioloop-stop ioloop)
      (should jupyter-ioloop-test-handler-called))))

(ert-deftest jupyter-ioloop-setup ()
  :tags '(ioloop)
  (let ((ioloop (jupyter-ioloop)))
    (setq jupyter-ioloop-test-handler-called nil)
    (jupyter-ioloop-add-setup ioloop
      (zmq-prin1 (list 'test "message")))
    (jupyter-ioloop-start ioloop :test)
    (jupyter-ioloop-stop ioloop)
    (should jupyter-ioloop-test-handler-called)))

(ert-deftest jupyter-ioloop-teardown ()
  :tags '(ioloop)
  (let ((ioloop (jupyter-ioloop)))
    (setq jupyter-ioloop-test-handler-called nil)
    (jupyter-ioloop-add-teardown ioloop
      (zmq-prin1 (list 'test "message")))
    (jupyter-ioloop-start ioloop :test)
    (jupyter-ioloop-stop ioloop)
    (should jupyter-ioloop-test-handler-called)))

(ert-deftest jupyter-ioloop-add-event ()
  :tags '(ioloop)
  (let ((ioloop (jupyter-ioloop)))
    (setq jupyter-ioloop-test-handler-called nil)
    (jupyter-ioloop-add-event ioloop test (data)
      "Echo DATA back to the parent process."
      (list 'test data))
    (jupyter-ioloop-start ioloop :test)
    (jupyter-send ioloop 'test "message")
    (jupyter-ioloop-stop ioloop)
    (should jupyter-ioloop-test-handler-called)))

(defun jupyter-ioloop-test-eval-ioloop (ioloop ex)
  (eval
   `(progn
      ,@(oref ioloop setup)
      ,(jupyter-ioloop--event-dispatcher ioloop ex))))

(ert-deftest jupyter-channel-ioloop-send-event ()
  :tags '(ioloop)
  (with-temp-buffer
    (cl-letf ((ioloop (jupyter-channel-ioloop))
              (standard-output (current-buffer))
              (jupyter-ioloop-channels (list (jupyter-sync-channel :type :shell)))
              ((symbol-function #'jupyter-send)
               (lambda (_channel _msg-type _msg msg-id) msg-id)))
      (let ((msg-id (jupyter-new-uuid)))
        (jupyter-ioloop-test-eval-ioloop
         ioloop `(list 'send :shell :execute-request '(msg) ,msg-id))
        (ert-info ("Return value to parent process")
          (let ((result (read (buffer-string))))
            (should (equal result `(sent :shell ,msg-id)))))))))

(ert-deftest jupyter-channel-ioloop-start-channel-event ()
  :tags '(ioloop)
  (with-temp-buffer
    (let* ((ioloop (jupyter-channel-ioloop))
           (standard-output (current-buffer))
           (jupyter-ioloop-channels nil)
           (jupyter-ioloop-session nil)
           (jupyter-ioloop-poller (zmq-poller))
           (channel-endpoint "tcp://127.0.0.1:5555"))
      (jupyter-channel-ioloop--set-session ioloop (jupyter-session :key "foo"))
      (jupyter-ioloop-test-eval-ioloop
       ioloop `(list 'start-channel :shell ,channel-endpoint))
      (should (not (null jupyter-ioloop-channels)))
      (should (jupyter-sync-channel-p (car jupyter-ioloop-channels)))
      (let ((channel (car jupyter-ioloop-channels)))
        (with-slots (type socket endpoint) channel
          (ert-info ("Verify the requested channel was started")
            (should (eq type :shell))
            (should (zmq-socket-p socket))
            (should (equal endpoint channel-endpoint))
            (should (equal (zmq-socket-get socket zmq-LAST-ENDPOINT) channel-endpoint))
            (ert-info ("Identity of socket matches session")
              (should (equal (zmq-socket-get socket zmq-IDENTITY)
                             (jupyter-session-id jupyter-ioloop-session)))))
          (ert-info ("Ensure the channel was added to the poller")
            ;; Raises an error if the socket wasn't added to the poller.
            (zmq-poller-modify
             jupyter-ioloop-poller socket (list zmq-POLLIN zmq-POLLOUT))))
        (ert-info ("Return value to parent process")
          (let ((result (read (buffer-string))))
            (should (equal result `(start-channel :shell)))))))))

(ert-deftest jupyter-channel-ioloop-stop-channel-event ()
  :tags '(ioloop)
  (with-temp-buffer
    (let* ((ioloop (jupyter-channel-ioloop))
           (standard-output (current-buffer))
           (channel (jupyter-sync-channel
                     :type :shell
                     :endpoint "tcp://127.0.0.1:5555"))
           (jupyter-ioloop-channels (list channel))
           (jupyter-ioloop-session (jupyter-session))
           (jupyter-ioloop-poller (zmq-poller)))
      (jupyter-start-channel channel :identity (jupyter-session-id jupyter-ioloop-session))
      (ert-info ("Verify the requested channel stops")
        (should (jupyter-channel-alive-p channel))
        (zmq-poller-add jupyter-ioloop-poller (oref channel socket) zmq-POLLIN)
        (jupyter-ioloop-test-eval-ioloop ioloop `(list 'stop-channel :shell))
        (should-not (jupyter-channel-alive-p channel)))
      (ert-info ("Ensure the channel was removed from the poller")
        (should-error (zmq-poller-modify (oref channel socket) (list zmq-POLLIN zmq-POLLOUT))))
      (ert-info ("Return value to parent process")
        (let ((result (read (buffer-string))))
          (should (equal result `(stop-channel :shell))))))))

;;; Completion

(ert-deftest jupyter-completion-number-p ()
  :tags '(completion)
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

(ert-deftest jupyter-completion-prefetch-p ()
  :tags '(completion)
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
      (should (jupyter-completion-prefetch-p "a")))))

;;; REPL

(ert-deftest jupyter-repl-client-predicates ()
  :tags '(repl)
  (should-not (jupyter-repl-client-has-manager-p))
  (should-not (jupyter-repl-connected-p))
  (jupyter-with-python-repl client
    (should (jupyter-repl-client-has-manager-p))
    (should (jupyter-repl-connected-p))))

(ert-deftest jupyter-repl-cell-predicates ()
  :tags '(repl cell)
  (jupyter-with-python-repl client
    (jupyter-ert-info ("`jupyter-repl-cell-line-p'")
      (should (jupyter-repl-cell-line-p))
      (jupyter-repl-replace-cell-code "1 + 1")
      (should (jupyter-repl-cell-line-p))
      (jupyter-test-repl-ret-sync)
      (should (jupyter-repl-cell-line-p))
      (forward-line -1)
      (should-not (jupyter-repl-cell-line-p)))
    (jupyter-ert-info ("`jupyter-repl-cell-finalized-p'")
      (should-not (jupyter-repl-cell-finalized-p))
      (jupyter-repl-replace-cell-code "1 + 1")
      (should-not (jupyter-repl-cell-finalized-p))
      (jupyter-test-repl-ret-sync)
      (should-not (jupyter-repl-cell-finalized-p))
      (jupyter-repl-backward-cell)
      (should (jupyter-repl-cell-finalized-p)))
    (jupyter-ert-info ("`jupyter-repl-cell-beginning-p'")
      (should-not (jupyter-repl-cell-beginning-p))
      (goto-char (- (point) 2))
      (should (jupyter-repl-cell-beginning-p))
      (should (= (point) (jupyter-repl-cell-beginning-position))))
    (jupyter-ert-info ("`jupyter-repl-cell-end-p'")
      (goto-char (point-max))
      (should (jupyter-repl-cell-end-p))
      (should (= (point) (jupyter-repl-cell-end-position)))
      (jupyter-repl-replace-cell-code "1 + 1")
      (let ((end (point-max)))
        (jupyter-test-repl-ret-sync)
        (jupyter-repl-backward-cell)
        (goto-char end)
        (should (jupyter-repl-cell-end-p))
        (should (= end (jupyter-repl-cell-end-position)))))))

(ert-deftest jupyter-repl-cell-positions ()
  :tags '(repl)
  (jupyter-with-python-repl client
    (jupyter-ert-info ("Cell code position info")
      (jupyter-repl-replace-cell-code "1 + 2")
      (should (= (point) (point-max)))
      (goto-char (1- (point)))
      (should (= (char-after) ?2))
      (should (= (jupyter-repl-cell-code-position) 5))
      (goto-char (line-beginning-position))
      (should (= (char-after) ?1))
      (should (= (jupyter-repl-cell-code-position) 1)))
    (jupyter-ert-info ("Cell code beginning")
      (should (= (point) (jupyter-repl-cell-code-beginning-position)))
      (jupyter-test-repl-ret-sync)
      (should (= (point) (jupyter-repl-cell-code-beginning-position)))
      (jupyter-repl-backward-cell)
      (should (= (point) (jupyter-repl-cell-code-beginning-position))))
    (jupyter-ert-info ("Cell code end")
      (should (= (point-max) (jupyter-repl-cell-code-end-position)))
      (jupyter-test-repl-ret-sync)
      (jupyter-repl-backward-cell)
      (should (= (1+ (line-end-position)) (jupyter-repl-cell-code-end-position))))))

(ert-deftest jupyter-repl-ret ()
  :tags '(repl)
  (jupyter-with-python-repl client
    (jupyter-ert-info ("`point' before last cell in buffer")
      (jupyter-test-repl-ret-sync)
      (let ((tick (buffer-modified-tick)))
        (goto-char (point-min))
        (jupyter-test-repl-ret-sync)
        (should (= (point) (point-max)))
        (should (equal tick (buffer-modified-tick)))))
    (jupyter-ert-info ("No cells in buffer")
      (let ((inhibit-read-only t))
        (erase-buffer))
      (should-not (next-single-property-change (point-min) 'jupyter-cell))
      (jupyter-test-repl-ret-sync)
      (should (next-single-property-change (point-min) 'jupyter-cell)))))

(ert-deftest jupyter-repl-cell-code-replacement ()
  :tags '(repl)
  (jupyter-with-python-repl client
    (jupyter-ert-info ("Replacing cell code")
      (should (equal (jupyter-repl-cell-code) ""))
      (jupyter-repl-replace-cell-code "1 + 1")
      (should (equal (jupyter-repl-cell-code) "1 + 1"))
      (jupyter-repl-replace-cell-code "foo\n bar")
      (should (equal (jupyter-repl-cell-code) "foo\n bar"))
      (jupyter-repl-replace-cell-code ""))))

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
  :tags '(repl)
  (jupyter-with-python-repl client
    (jupyter-ert-info ("Rotating REPL history ring")
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
  :tags '(repl motion)
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
  :tags '(repl motion)
  (jupyter-with-python-repl client
    (jupyter-ert-info ("Beginning of a cell")
      (should (= (point) (jupyter-repl-cell-code-beginning-position)))
      (should (get-text-property (- (point) 2) 'jupyter-cell))
      (should (jupyter-repl-cell-beginning-p (- (point) 2)))
      (should (= (jupyter-repl-cell-beginning-position) (- (point) 2))))
    (jupyter-ert-info ("End of unfinalized cell")
      (should-not (jupyter-repl-cell-finalized-p))
      (should-not (get-text-property (point-max) 'jupyter-cell))
      (should (= (jupyter-repl-cell-end-p (point-max))))
      (should (= (jupyter-repl-cell-end-position) (point-max)))
      (should (= (jupyter-repl-cell-code-end-position) (point-max))))
    (jupyter-ert-info ("End of finalized cell")
      (jupyter-test-repl-ret-sync)
      (should (= (point) (jupyter-repl-cell-code-beginning-position)))
      (goto-char (1- (jupyter-repl-cell-beginning-position)))
      (should (jupyter-repl-cell-end-p))
      (should (= (jupyter-repl-cell-end-position) (point)))
      (should (= (jupyter-repl-cell-code-end-position) (point)))
      (should (jupyter-repl-cell-finalized-p)))
    (jupyter-ert-info ("Cell boundary errors")
      (goto-char (point-max))
      (jupyter-repl-replace-cell-code "1 + 1")
      (jupyter-wait-until-idle (jupyter-send-execute-request client))
      (forward-line -2)
      (should (eq (car (get-text-property (1- (point)) 'jupyter-cell))
                  'out))
      (should-error (jupyter-repl-cell-beginning-position))
      (should-error (jupyter-repl-cell-end-position)))))

(ert-deftest jupyter-repl-prompts ()
  :tags '(repl prompt)
  (let ((jupyter-test-with-new-client t))
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

        ))))

;;; `org-mode'

(defvar org-babel-load-languages)
(defvar org-confirm-babel-evaluate)

(defvar jupyter-org-test-session nil
  "Name of the session for testing Jupyter source blocks.")

(defvar jupyter-org-test-buffer nil
  "`org-mode' buffer for testing Jupyter source blocks.")

(defun jupyter-org-test-setup ()
  (unless jupyter-org-test-session
    (require 'org)
    (setq org-babel-load-languages
          '((python . t)
            (jupyter . t)))
    (setq org-confirm-babel-evaluate nil)
    (setq jupyter-org-test-session (make-temp-name "ob-jupyter-test"))
    (setq jupyter-org-test-buffer (get-buffer-create "ob-jupyter-test"))
    (org-babel-do-load-languages
     'org-babel-load-languages
     org-babel-load-languages)
    (with-current-buffer jupyter-org-test-buffer
      (org-mode)
      (insert
       "#+BEGIN_SRC jupy-python " ":session " jupyter-org-test-session "\n"
       "#+END_SRC")
      (setq jupyter-current-client
            (with-current-buffer (org-babel-initiate-session)
              jupyter-current-client))))
  (with-current-buffer jupyter-org-test-buffer
    (erase-buffer)))

(defmacro jupyter-org-test (&rest body)
  (declare (debug (body)))
  `(progn
     (jupyter-org-test-setup)
     (with-current-buffer jupyter-org-test-buffer
       ,@body)))

(defmacro jupyter-org-test-src-block (block expected-result &rest args)
  "Test source code BLOCK.
EXPECTED-RESULT is a string that the source block's results
should match. ARGS is a plist of header arguments to be set for
the source code block. For example if ARGS is (:results \"raw\")
then the source code block will begin like

    #+BEGIN_SRC jupy-python :results raw ...

Note if ARGS contains a key, regexp, then if regexp is non-nil,
EXPECTED-RESULT is a regular expression to match against the
results instead of an equality match."
  (let (regexp)
    (setq args
          (cl-loop for (arg val) on args by #'cddr
                   if (eq arg :regexp) do (setq regexp val)
                   else collect (cons arg val)))
    `(jupyter-org-test
      (jupyter-org-test-src-block-1 ,block ,expected-result ,regexp ',args))))

(defun jupyter-org-test-make-block (code args)
  (let ((arg-str (mapconcat
                  (lambda (x)
                    (cl-destructuring-bind (name . val) x
                      (concat (symbol-name name) " " (format "%s" val))))
                  args " ")))
    (concat
     "#+BEGIN_SRC jupy-python " arg-str " :session "
     jupyter-org-test-session "\n"
     code "\n"
     "#+END_SRC")))

(defun jupyter-org-test-src-block-1 (code test-result &optional regexp args)
  (insert (jupyter-org-test-make-block code args))
  (let* ((info (org-babel-get-src-block-info))
         (end (point-marker)))
    (set-marker-insertion-type end t)
    (save-window-excursion
      (org-babel-execute-src-block nil info))
    (org-with-point-at (org-babel-where-is-src-block-result nil info)
      (when (equal (alist-get :async args) "yes")
        (jupyter-wait-until-idle
         (ring-ref (oref jupyter-current-client pending-requests) 0)))
      (let ((drawer (org-element-at-point)))
        ;; Handle empty results with just a RESULTS keyword
        ;;
        ;; #+RESULTS:
        (if (eq (org-element-type drawer) 'keyword) ""
          (let ((result (string-trim
                         (if (eq (org-element-type drawer) 'drawer)
                             (buffer-substring-no-properties
                              (org-element-property :contents-begin drawer)
                              (org-element-property :contents-end drawer))
                           (buffer-substring-no-properties
                            (org-element-property :post-affiliated drawer)
                            (org-element-property :end drawer))))))
            (if regexp (should (string-match-p test-result result))
              (should (equal result test-result)))))))))

(defvar org-babel-jupyter-resource-directory nil)

(ert-deftest ob-jupyter-no-results ()
  :tags '(org)
  (jupyter-org-test-src-block "1 + 1;" ""))

(ert-deftest ob-jupyter-scalar-results ()
  :tags '(org)
  (jupyter-org-test-src-block "1 + 1" ": 2")
  (ert-info ("Tables")
    (jupyter-org-test-src-block
     "[[1, 2, 3], [4, 5, 6]]"
     "\
| 1 | 2 | 3 |
| 4 | 5 | 6 |")))

(ert-deftest ob-jupyter-html-results ()
  :tags '(org)
  (jupyter-org-test-src-block
   "\
from IPython.core.display import HTML
HTML('<a href=\"http://foo.com\">link</a>')"
   "\
#+BEGIN_EXPORT html
<a href=\"http://foo.com\">link</a>
#+END_EXPORT"))

(ert-deftest ob-jupyter-markdown-results ()
  :tags '(org)
  (jupyter-org-test-src-block
   "\
from IPython.core.display import Markdown
Markdown('*b*')"
   "\
#+BEGIN_EXPORT markdown
*b*
#+END_EXPORT"))

(ert-deftest ob-jupyter-latex-results ()
  :tags '(org)
  (jupyter-org-test-src-block
   "\
from IPython.core.display import Latex
Latex(r'$\\alpha$')"
   "\
#+BEGIN_EXPORT latex
$\\alpha$
#+END_EXPORT")
  (ert-info ("Raw results")
    (jupyter-org-test-src-block
     "\
from IPython.core.display import Latex
Latex(r'$\\alpha$')"
     "$\\alpha$"
     :results "raw")))

(ert-deftest ob-jupyter-error-results ()
  :tags '(org)
  (jupyter-org-test-src-block
   "from IPython."
   "\
:     from IPython.
:                  ^
: SyntaxError: invalid syntax"
   :regexp t))

(ert-deftest ob-jupyter-image-results ()
  :tags '(org)
  (let* ((default-directory (file-name-directory
                             (locate-library "jupyter")))
         (org-babel-jupyter-resource-directory "./")
         (file (expand-file-name "jupyter.png"))
         (py-version
          (with-current-buffer jupyter-org-test-buffer
            (jupyter-test-ipython-kernel-version
             (oref (oref jupyter-current-client manager) spec))))
         ;; There is a change in how the IPython kernel prints base64 encoded
         ;; images somewhere between [4.6.1, 5.1]. In 5.1, base64 encoded
         ;; images are printed with line breaks whereas in 4.6.1 they are not.
         (line-breaks (version< "4.6.1" py-version))
         (data (let ((buffer-file-coding-system 'binary))
                 (with-temp-buffer
                   (set-buffer-multibyte nil)
                   (insert-file-contents-literally file)
                   (base64-encode-region (point-min) (point-max) line-breaks)
                   (goto-char (point-max))
                   (insert "\n")
                   (buffer-substring-no-properties (point-min) (point-max)))))
         (image-file-name (jupyter-org-image-file-name data "png")))
    (unwind-protect
        (progn
          (jupyter-org-test-src-block
           (format "\
from IPython.display import Image
Image(filename='%s')" file)
           (format "[[file:%s]]" image-file-name))
          (ert-info ("Create a drawer containing file links")
            (jupyter-org-test-src-block
             (format "\
from IPython.display import Image
from IPython.display import display
display(Image(filename='%s'))
Image(filename='%s')" file file)
             (concat
              (format "[[file:%s]]" image-file-name) "\n"
              (format "[[file:%s]]" image-file-name))
             :async "yes"))
          (ert-info ("Append a file link to a drawer")
            (jupyter-org-test-src-block
             (format "\
from IPython.display import Image
from IPython.display import display
display(Image(filename='%s'))
display(Image(filename='%s'))
Image(filename='%s')" file file file)
             (concat
              (format "[[file:%s]]" image-file-name) "\n"
              (format "[[file:%s]]" image-file-name) "\n"
              (format "[[file:%s]]" image-file-name))
             :async "yes"))
          (ert-info ("Image with width and height metadata")
            (jupyter-org-test-src-block
             (format "\
from IPython.display import Image
Image(filename='%s', width=300)" file)
             (concat
              "#+ATTR_ORG: :width 300\n"
              (format "[[file:%s]]" image-file-name)))))
      (when (file-exists-p image-file-name)
        (delete-file image-file-name)))))

(ert-deftest jupyter-org-result ()
  :tags '(org)
  (let ((req (jupyter-org-request)))
    (should (equal (jupyter-org-result req (list :text/plain "foo"))
                   '(fixed-width (:value "foo"))))
    (should (equal (jupyter-org-result req (list :text/html "foo"))
                   '(export-block (:type "html" :value "foo\n"))))
    ;; Calls `org-babel-script-escape' for scalar data
    (should (equal (jupyter-org-result req (list :text/plain "[1, 2, 3]"))
                   "| 1 | 2 | 3 |\n"))
    (should (equal (jupyter-org-result req (list :text/plain "[1, 2, 3] Foo"))
                   '(fixed-width (:value "[1, 2, 3] Foo"))))))

(ert-deftest jupyter-org-result-python ()
  :tags '(org)
  ;; Test that the python language specialized method calls
  ;; `org-babel-python-table-or-string', this is more of a test for method
  ;; order.
  (cl-letf* ((py-method-called nil)
             (req (jupyter-org-request))
             ((symbol-function #'org-babel-python-table-or-string)
              (lambda (results)
                (setq py-method-called t)
                (org-babel-script-escape results)))
             (jupyter-current-client (jupyter-kernel-client)))
    (oset jupyter-current-client kernel-info
          (list :language_info (list :name "python")))
    (should (equal (jupyter-kernel-language jupyter-current-client) "python"))
    ;; Bring in the python specific methods
    (jupyter-load-language-support jupyter-current-client)
    (should (equal (jupyter-org-result req (list :text/plain "[1, 2, 3]"))
                   "| 1 | 2 | 3 |\n"))
    (should py-method-called)))

(ert-deftest jupyter-org-src-block-cache ()
  :tags '(org)
  (let (jupyter-org--src-block-cache)
    (jupyter-org-test
     (insert
      "#+BEGIN_SRC jupy-python :session " jupyter-org-test-session "\n"
      "imp\n"
      "#+END_SRC\n\n\n#+RESULTS:")
     ;; Needed for the text properties
     (font-lock-ensure)
     (goto-char (point-min))
     (forward-line)
     (end-of-line)
     (should-not jupyter-org--src-block-cache)
     (should-not (jupyter-org--same-src-block-p))
     (jupyter-org--set-current-src-block)
     (should jupyter-org--src-block-cache)
     (should (jupyter-org--same-src-block-p))
     (cl-destructuring-bind (params beg end)
         jupyter-org--src-block-cache
       (should (equal (alist-get :session params) jupyter-org-test-session))
       (should (= beg (line-beginning-position)))
       (should (= end (line-beginning-position 2))))
     (ert-info ("End marker updates after insertion")
       (forward-line)
       (insert "new source block text\n")
       ;; #+BEGIN_SRC ...
       ;; imp
       ;; new source block text
       ;; |#+END_SRC
       (cl-destructuring-bind (params beg end)
           jupyter-org--src-block-cache
         (should (equal (alist-get :session params) jupyter-org-test-session))
         (should (= beg (line-beginning-position -1)))
         (should (= end (line-beginning-position))))))))

(ert-deftest jupyter-org-when-in-src-block ()
  :tags '(org)
  (ert-info ("In Jupyter blocks")
    (jupyter-org-test
     (insert
      "#+BEGIN_SRC jupy-python :session " jupyter-org-test-session "\n"
      "1 + 1\n"
      "#+END_SRC\nfoo")
     ;; Needed for the text properties
     (font-lock-ensure)
     (goto-char (point-min))
     (should-not (jupyter-org-when-in-src-block t))
     (forward-line)
     (should (jupyter-org-when-in-src-block t))
     (forward-line)
     (should-not (jupyter-org-when-in-src-block t))
     (forward-line)
     (should-not (jupyter-org-when-in-src-block t))))
  (ert-info ("Not in Jupyter block")
    (jupyter-org-test
     (insert
      "#+BEGIN_SRC python :session " jupyter-org-test-session "\n"
      "1 + 1\n"
      "#+END_SRC\nfoo")
     ;; Needed for the text properties
     (font-lock-ensure)
     (goto-char (point-min))
     (should-not (jupyter-org-when-in-src-block t))
     (forward-line)
     (should-not (jupyter-org-when-in-src-block t))
     (forward-line)
     (should-not (jupyter-org-when-in-src-block t))
     (forward-line)
     (should-not (jupyter-org-when-in-src-block t)))))

(ert-deftest jupyter-org--stream-context-p ()
  :tags '(org)
  (with-temp-buffer
    (org-mode)
    (dolist
        (res '(("\
#+RESULTS:
:RESULTS:
: Foo
:END:" . 27)
               ("\
#+RESULTS:
: Foo
" . 17)
               ("\
#+RESULTS:
#+BEGIN_EXAMPLE
Foo
#+END_EXAMPLE
" . 31)
               ("\
#+RESULTS:
:RESULTS:
#+BEGIN_EXAMPLE
Foo
#+END_EXAMPLE
:END:
" . 41)
               ("\
#+RESULTS:
file:foo
" . nil)))
      (insert (car res))
      (if (cdr res)
          (should (= (jupyter-org--stream-context-p (org-element-at-point)) (cdr res)))
        (should-not (jupyter-org--stream-context-p (org-element-at-point))))
      (erase-buffer))))

(ert-deftest jupyter-org-coalesce-stream-results ()
  :tags '(org)
  (ert-info ("Synchronous")
    (jupyter-org-test-src-block
     "\
print(\"foo\")
print(\"foo\", flush=True)
print(\"foo\")"
     "\
: foo
: foo
: foo"))
  (ert-info ("Asynchronous")
    (ert-info ("Newline after first stream message")
      (jupyter-org-test-src-block
       "\
print(\"foo\")
print(\"foo\", flush=True)
print(\"foo\")"
       "\
: foo
: foo
: foo"
       :async "yes")
      (jupyter-org-test-src-block
       "\
print(\"foo\", flush=True)
print(\"foo\", end=\"\", flush=True)
print(\"foo\")"
       "\
: foo
: foofoo")
      :async "yes")
    (ert-info ("No newline after first stream message")
      (jupyter-org-test-src-block
       "\
print(\"foo\")
print(\"foo\", end=\"\", flush=True)
print(\"bar\")"
       "\
: foo
: foobar"
       :async "yes"))
    (ert-info ("Multiple newlines in appended stream message")
      (ert-info ("Newline after first stream message")
        (jupyter-org-test-src-block
         "\
print(\"foo\")
print(\"foo\", flush=True)
print(\"bar\\nqux\")"
         "\
: foo
: foo
: bar
: qux"
         :async "yes"))
      (ert-info ("No newline after first stream message")
        (jupyter-org-test-src-block
         "\
print(\"foo\")
print(\"foo\", end=\"\", flush=True)
print(\"bar\\nqux\")"
         "\
: foo
: foobar
: qux"
         :async "yes")))
    (ert-info ("fixed-width to example-block promotion")
      (let ((org-babel-min-lines-for-block-output 2))
        (jupyter-org-test-src-block "print(\"z\")" ": z")
        (jupyter-org-test-src-block
         "\
print(\"z\", flush=True)
print(\"z\")"
         "\
#+BEGIN_EXAMPLE
z
z
#+END_EXAMPLE"
         :async "yes")
        (ert-info ("Appending after block promotion")
          (jupyter-org-test-src-block
           "\
print(\"z\", flush=True)
print(\"z\", flush=True)
print(\"z\")"
           "\
#+BEGIN_EXAMPLE
z
z
z
#+END_EXAMPLE"
           :async "yes"))
        (ert-info ("Append to block with newline after first stream message")
          (jupyter-org-test-src-block
           "\
print(\"z\\nz\", flush=True)
print(\"z\")"
           "\
#+BEGIN_EXAMPLE
z
z
z
#+END_EXAMPLE"
           :async "yes"))
        (ert-info ("Append to block without newline after first stream message")
          (jupyter-org-test-src-block
           "\
print(\"z\\nz\", end=\"\", flush=True)
print(\"z\")"
           "\
#+BEGIN_EXAMPLE
z
zz
#+END_EXAMPLE"
           :async "yes"))))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; jupyter-test.el ends here
