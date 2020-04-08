;;; jupyter-test.el --- Jupyter tests -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 08 Jan 2018

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
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
(require 'jupyter-env)
(require 'jupyter-client)
(require 'jupyter-repl)
(require 'jupyter-org-client)
(require 'jupyter-kernel-manager)
(require 'cl-lib)
(require 'ert)
(require 'subr-x)                       ; string-trim

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
        (should (equal (jupyter-message-content (cadr msgs))
                       (plist-get (jupyter-test-message (jupyter-request) nil msg) :content)))
        (should (equal (jupyter-message-type (caddr msgs)) :status))
        (should (equal (jupyter-message-parent-id (caddr msgs))
                       (jupyter-request-id req)))
        (should (equal (jupyter-message-get (caddr msgs) :execution_state) "idle"))))))

;;;; Comm layer

(ert-deftest jupyter-comm-layer ()
  :tags '(mock comm)
  (let ((comm (jupyter-mock-comm-layer))
        (obj (make-jupyter-mock-comm-obj)))
    (jupyter-comm-add-handler comm obj)
    (should (= (length (oref comm handlers)) 1))
    (should (eq (jupyter-weak-ref-resolve (car (oref comm handlers))) obj))
    (should (= (oref comm alive) 1))
    (jupyter-comm-add-handler comm obj)
    (should (= (length (oref comm handlers)) 1))
    (should (eq (jupyter-weak-ref-resolve (car (oref comm handlers))) obj))
    (should (= (oref comm alive) 1))

    (should-not (jupyter-mock-comm-obj-event obj))
    (jupyter-event-handler comm '(event))
    ;; Events are handled in a timer, not right away
    (sleep-for 0.01)
    (should (equal (jupyter-mock-comm-obj-event obj) '(event)))

    (jupyter-comm-remove-handler comm obj)
    (should (= (length (oref comm handlers)) 0))
    (should-not (oref comm alive))
    (jupyter-comm-remove-handler comm obj)))

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

(ert-deftest jupyter-map-mime-bundle ()
  :tags '(mime)
  (let ((res (current-time))
        (content
         '(:data
           (:text/plain
            1 :text/html 2
            :text/latex 3)
           :metadata
           (:text/plain
            4 :text/html 5
            :text/latex 6))))
    (should
     (eq
      res
      (jupyter-map-mime-bundle
          '(:text/html :text/latex :text/plain)
          content
        (lambda (mime content)
          (when (eq mime :text/plain)
            (should (= (plist-get content :data) 1))
            (should (= (plist-get content :metadata) 4))
            res)))))
    (let (called)
      (should-not
       (eq
        res
        (jupyter-map-mime-bundle
            '(:text/html :text/latex :mime-without-data) content
          (lambda (mime content)
            (pcase mime
              (:text/latex
               (should (= (plist-get content :data) 3))
               (should (= (plist-get content :metadata) 6))
               (setq called t)
               nil)
              (:mime-without-data res)
              (_ nil))))))
      (should called))))

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
          (cl-letf (((symbol-function #'image-type-available-p)
                     (lambda (typ) (eq typ 'jpeg))))
            (should (eq (jupyter-insert data) :image/jpeg)))
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

(ert-deftest jupyter-with-display-buffer ()
  :tags '(buffers)
  (jupyter-with-display-buffer "foo" t)
  (jupyter-with-display-buffer "foo" nil
    (should (= jupyter-display-buffer-marker (point-min)))
    (insert "12345")
    (should (= jupyter-display-buffer-marker (point-max)))
    (goto-char (point-min)))
  (jupyter-with-display-buffer "foo" nil
    (should (= (point) (point-max)))
    (should (= jupyter-display-buffer-marker (point-max)))
    (insert "foobar")
    (should (= jupyter-display-buffer-marker (point-max)))
    (should (equal (buffer-string) "12345foobar")))
  (jupyter-with-display-buffer "foo" t
    (should (equal (buffer-string) ""))
    (should (= (point-min) (point-max)))
    (should (= jupyter-display-buffer-marker (point-min)))))

(ert-deftest jupyter-with-contol-code-handling ()
  :tags '(buffers)
  (jupyter-with-display-buffer "foo" t)
  (jupyter-with-display-buffer "foo" nil
    (insert "foo\r"))
  (jupyter-with-display-buffer "foo" nil
    (should (equal (buffer-string) "foo\r"))
    (jupyter-test-text-has-property 'invisible t '(4))
    (insert "foo\r"))
  (jupyter-with-display-buffer "foo" nil
    (should (equal (buffer-string) "foo\r"))
    (insert "bar\r\nbaz\rfoo"))
  (jupyter-with-display-buffer "foo" nil
    (should (equal (buffer-string) "bar\nfoo"))))

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
    (should (equal (jupyter-encode-time '(23385 27704 100000))
                   "2018-07-26T06:37:44.100000"))
    (should (equal (jupyter-decode-time "2018-07-26T01:37:44.100")
                   '(23385 9704 100000 0)))
    (should (equal (jupyter-decode-time "2018-07-26T01:37:44.10011122")
                   '(23385 9704 100111 0)))
    (should (equal (jupyter-decode-time "2018-07-26T01:37:44")
                   '(23385 9704 0 0)))
    (should (equal (jupyter-decode-time "2018-07-26")
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
  (jupyter-test-with-python-client client
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
        (should (eq (jupyter-message-type res) :shutdown-reply))
        ;; Ensure we give the kernel process time to die off
        (when (oref client manager)
          (jupyter-with-timeout (nil jupyter-long-timeout)
            (not (jupyter-kernel-alive-p (oref client manager)))))))))

(ert-deftest jupyter-message-lambda ()
  :tags '(messages)
  (let ((msg (jupyter-test-message
              (jupyter-request) :execute-reply
              (list :status "idle" :data (list :text/plain "foo")))))
    (should (equal (funcall (jupyter-message-lambda (status)
                              status)
                            msg)
                   "idle"))
    (should (equal (funcall (jupyter-message-lambda ((res text/plain))
                              res)
                            msg)
                   "foo"))
    (should (equal (funcall (jupyter-message-lambda (status (res text/plain))
                              (cons status res))
                            msg)
                   (cons "idle" "foo")))))

;;; Channels

(ert-deftest jupyter-zmq-channel ()
  :tags '(channels zmq)
  (let* ((port (car (jupyter-available-local-ports 1)))
         (channel (jupyter-zmq-channel
                   :type :shell
                   :endpoint (format "tcp://127.0.0.1:%s" port))))
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
        (should-not (jupyter-channel-alive-p channel))
        ;; Ensure the socket was disconnected
        (should-error (zmq-send sock "foo" zmq-NOBLOCK) :type 'zmq-EAGAIN)))))

(ert-deftest jupyter-hb-channel ()
  :tags '(channels)
  (should (eq (oref (jupyter-hb-channel) type) :hb))
  (let* ((port (car (jupyter-available-local-ports 1)))
         (channel (jupyter-hb-channel
                   :endpoint (format "tcp://127.0.0.1:%s" port)
                   :session (jupyter-session)))
         (died-cb-called nil)
         (jupyter-hb-max-failures 1))
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

;;; GC

(ert-deftest jupyter-weak-ref ()
  :tags '(gc)
  (let (ref)
    (let ((obj (list 1)))
      (setq ref (jupyter-weak-ref obj)))
    (let ((table (make-hash-table)))
      (dotimes (_ gc-cons-threshold)
        (puthash (random) t table)))
    (garbage-collect)
    (garbage-collect)
    (garbage-collect)
    (should-not (jupyter-weak-ref-resolve ref))))

(defclass jupyter-test-object (jupyter-finalized-object)
  ((val)))

(ert-deftest jupyter-add-finalizer ()
  :tags '(gc)
  (let ((val (list 1)))
    (let ((obj (jupyter-test-object)))
      (oset obj val val)
      (jupyter-add-finalizer obj
        (lambda () (setcar (oref obj val) nil))))
    (ignore (make-list (* 2 gc-cons-threshold) ?0))
    (garbage-collect)
    (should (null (car val)))))


;;; Kernel

(ert-deftest jupyter-locate-python ()
  :tags '(kernel)
  ;; TODO: Generalize for Windows
  (skip-unless (not (memq system-type '(ms-dos windows-nt cygwin))))
  ;; Load file name handlers
  (ignore (file-remote-p "/ssh:foo:"))
  (cl-letf (((symbol-function #'jupyter-command)
             (lambda (&rest _)
               "{\"data\": [\"/home/USER/.local/share/jupyter\", \
\"/home/USER/.julia/conda/3/share/jupyter\", \
\"/usr/local/share/jupyter\", \
\"/usr/share/jupyter\"]}"))
            ((symbol-function #'file-exists-p)
             (lambda (file)
               (member file '("/home/USER/.julia/conda/3/bin/python3"
                              "/ssh:foo:/usr/local/bin/python3")))))
    (should (equal (jupyter-locate-python) "/home/USER/.julia/conda/3/bin/python3"))
    (let ((default-directory "/ssh:foo:"))
      (should (equal (jupyter-locate-python) "/usr/local/bin/python3"))))
  (cl-letf (((symbol-function #'jupyter-command)
             (lambda (&rest _)
               "{\"foo\": [\"/home/USER/.local/share/jupyter\", \
\"/usr/share/jupyter\"]}")))
    (should-error (jupyter-locate-python)))
  (cl-letf (((symbol-function #'jupyter-command)
             (lambda (&rest _)
               "{\"data\": [\"/home/USER/.local/share/jupyter\", \
\"/usr/share/jupyter\"]}"))
            ((symbol-function #'file-exists-p)
             (lambda (_) nil)))
    (should-error (jupyter-locate-python))))

(ert-deftest jupyter-kernel-lifetime ()
  :tags '(kernel)
  (let* ((conn-info (jupyter-local-tcp-conn-info))
         (kernel (jupyter-spec-kernel
                  :spec (jupyter-guess-kernelspec "python")
                  :session (jupyter-session
                            :key (plist-get conn-info :key)
                            :conn-info conn-info))))
    (should-not (jupyter-kernel-alive-p kernel))
    (jupyter-start-kernel kernel)
    (should (jupyter-kernel-alive-p kernel))
    (jupyter-kill-kernel kernel)
    (should-not (jupyter-kernel-alive-p kernel))
    (setq conn-info (jupyter-local-tcp-conn-info))
    (ert-info ("`jupyter-kernel-manager'")
      ;; TODO: Should the manager create a session if one isn't present?
      (oset kernel session (jupyter-session
                            :key (plist-get conn-info :key)
                            :conn-info conn-info))
      (let* ((manager (jupyter-kernel-process-manager :kernel kernel))
             (control-channel (oref manager control-channel))
             process)
        (should-not (jupyter-kernel-alive-p manager))
        (should-not control-channel)
        (jupyter-start-kernel manager)
        (setq process (oref kernel process))
        (setq control-channel (oref manager control-channel))
        (should (jupyter-zmq-channel-p control-channel))
        (should (jupyter-kernel-alive-p manager))
        (should (jupyter-kernel-alive-p kernel))
        (jupyter-shutdown-kernel manager)
        (ert-info ("Kernel shutdown is clean")
          (should-not (process-live-p process))
          (should (zerop (process-exit-status process)))
          (should-not (jupyter-kernel-alive-p manager))
          (should-not (jupyter-kernel-alive-p kernel)))
        (setq control-channel (oref manager control-channel))
        (should-not (jupyter-zmq-channel-p control-channel))))))

(ert-deftest jupyter-command-kernel ()
  :tags '(kernel)
  (let ((kernel (jupyter-command-kernel
                 :spec (jupyter-guess-kernelspec "python"))))
    (ert-info ("Session set after kernel starts")
      (should-not (jupyter-kernel-alive-p kernel))
      (jupyter-start-kernel kernel)
      (should (jupyter-kernel-alive-p kernel))
      (should (oref kernel session))
      (jupyter-kill-kernel kernel)
      (should-not (jupyter-kernel-alive-p kernel)))
    (ert-info ("Can we communicate?")
      (let ((manager (jupyter-kernel-process-manager :kernel kernel)))
        (jupyter-start-kernel manager)
        (unwind-protect
            (let ((jupyter-current-client
                   (jupyter-make-client manager 'jupyter-kernel-client)))
              (jupyter-start-channels jupyter-current-client)
              (unwind-protect
                  (progn
                    (jupyter-wait-until-startup jupyter-current-client)
                    (should (equal (jupyter-eval "1 + 1") "2")))
                (jupyter-stop-channels jupyter-current-client)))
          (jupyter-shutdown-kernel manager))))))

;;; Environment

(ert-deftest jupyter-canonicalize-language-string ()
  :tags '(base env)
  (should (equal (jupyter-canonicalize-language-string "Wolfram Language")
                 "Wolfram-Language"))
  (should (equal (jupyter-canonicalize-language-string "R") "R"))
  (should (equal (jupyter-canonicalize-language-string "/gnu/store/python")
                 "python")))

(ert-deftest jupyter-runtime-directory ()
  :tags '(env)
  (let (dir-created jupyter-runtime-directory)
    (cl-letf (((symbol-function #'jupyter-command)
               (lambda (&rest _) "foo"))
              ((symbol-function #'make-directory)
               (lambda (&rest _)
                 (setq dir-created t))))
      (jupyter-runtime-directory)
      (should dir-created)
      (setq dir-created nil)
      (should (equal jupyter-runtime-directory "foo"))
      (let ((default-directory "/ssh:foo:/"))
        (should (equal (jupyter-runtime-directory) "/ssh:foo:foo"))
        (ert-info ("Variable definition is always local")
          (setq jupyter-runtime-directory nil)
          (jupyter-runtime-directory)
          (should (equal jupyter-runtime-directory "foo")))))))

;;; Client

;; TODO: Different values of the session argument
;;
;; FIXME: Re-work after refactoring the kernelspec -> connectable kernel code paths.
(ert-deftest jupyter-comm-initialize ()
  :tags '(client init)
  (skip-unless nil)
  (jupyter-test-with-python-client client
	(with-slots (session kcomm) client
	  (ert-info ("Client session")
        (should (string= (jupyter-session-key session)
                         (plist-get conn-info :key)))
        (should (equal (jupyter-session-conn-info session)
					   conn-info)))
	  (ert-info ("Heartbeat channel initialized")
        (should (eq session (oref (oref kcomm hb) session)))
        (should (string= (oref (oref kcomm hb) endpoint)
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
        (jupyter-comm-initialize client conn-info)
        (should-not (jupyter-channels-running-p client)))
      (ert-info ("Invalid signature scheme")
        (plist-put conn-info :signature_scheme "hmac-foo")
        (should-error (jupyter-comm-initialize client conn-info))))))

(ert-deftest jupyter-write-connection-file ()
  :tags '(client)
  (skip-unless (not (memq system-type '(ms-dos windows-nt cygwin))))
  (let (file fun)
    (let* ((session (jupyter-session
                     :conn-info (jupyter-local-tcp-conn-info)))
           (client (jupyter-kernel-client))
           (hook (copy-sequence kill-emacs-hook)))
      (setq file (jupyter-write-connection-file session client))
      (should (file-exists-p file))
      (should-not (equal kill-emacs-hook hook))
      (setq fun (car (cl-set-difference kill-emacs-hook hook)))
      (should-not (memq fun hook)))
    (garbage-collect)
    (garbage-collect)
    (garbage-collect)
    (garbage-collect)
    (unwind-protect
        ;; This fails on Windows, probably has something to do with the file
        ;; handle still being opened somehow.
        (should-not (file-exists-p file))
      (when (file-exists-p file)
        (delete-file file)))
    (should-not (memq fun kill-emacs-hook))))

(ert-deftest jupyter-client-channels ()
  :tags '(client channels)
  (ert-info ("Starting/stopping channels")
	;; FIXME: Without a new client, I'm getting
	;;
	;;   (zmq-EFSM "Operation cannot be accomplished in current state")
	;;
	;; on the `jupyter-connect-repl' test pretty consistently.
	(let ((jupyter-test-with-new-client t))
	  (jupyter-test-with-python-client client
		(jupyter-stop-channels client)
		(cl-loop
		 for channel in '(:hb :shell :iopub :stdin)
		 for alive-p = (jupyter-channel-alive-p client channel)
		 do (should-not alive-p))
		(jupyter-start-channels client)
		(cl-loop
		 for channel in '(:hb :shell :iopub :stdin)
		 for alive-p = (jupyter-channel-alive-p client channel)
		 do (should alive-p))
		(jupyter-stop-channels client)
		(cl-loop
		 for channel in '(:hb :shell :iopub :stdin)
		 for alive-p = (jupyter-channel-alive-p client channel)
		 do (should-not alive-p))))))

(ert-deftest jupyter-inhibited-handlers ()
  :tags '(client handlers)
  (jupyter-test-with-python-client client
    (let* ((jupyter-inhibit-handlers '(:stream))
           (req (jupyter-send-kernel-info-request client)))
      (should (equal (jupyter-request-inhibited-handlers req)
                     '(:stream)))
      (should-not (jupyter--run-handler-p
                   req (jupyter-test-message
                        req :stream (list :name "stdout" :text "foo"))))
      (setq jupyter-inhibit-handlers '(:foo))
      (should-error (jupyter-send-kernel-info-request client)))))

(ert-deftest jupyter-requests-pending-p ()
  :tags '(client)
  (jupyter-test-with-python-client client
    (let (pending)
      (jupyter-with-timeout (nil jupyter-long-timeout)
        (while (jupyter-requests-pending-p client)
          (when-let* ((last-sent (gethash "last-sent" (oref client requests))))
            (jupyter-wait-until-idle last-sent))))
      ;; Don't pass CLIENT to `should-not' because `ert' will attempt to
      ;; print the class object on failure and will fail at doing so.
      (setq pending (jupyter-requests-pending-p client))
      (should-not pending)
      (let ((req (jupyter-send-kernel-info-request client)))
        (ert-info ("Pending after send")
          (setq pending (jupyter-requests-pending-p client))
          (should pending)
          (jupyter-wait-until-idle req)
          (setq pending (jupyter-requests-pending-p client))
          (should-not pending))
        (ert-info ("Pending until idle received")
          (setq req (jupyter-send-execute-request client
                      :code "import time; time.sleep(0.2)"))
          ;; Empty out the pending-requests slot of CLIENT
          (jupyter-wait-until-received :status req)
          (setq pending (jupyter-requests-pending-p client))
          (should pending)
          (jupyter-wait-until-idle req)
          (setq pending (jupyter-requests-pending-p client))
          (should-not pending))))))

(ert-deftest jupyter-eval ()
  :tags '(client)
  (jupyter-test-with-python-client client
    (let ((jupyter-current-client client))
      (should (equal (jupyter-eval "1 + 1") "2")))))

(ert-deftest jupyter-line-count-greater-p ()
  :tags '(client)
  (should (jupyter-line-count-greater-p "\n\n" 1))
  (should (jupyter-line-count-greater-p "a\n\n" 1))
  (should (jupyter-line-count-greater-p "\na\n" 1))
  (should (jupyter-line-count-greater-p "a\na\n" 1))
  (should (jupyter-line-count-greater-p "\n\na" 1))
  (should (jupyter-line-count-greater-p "a\n\na" 1))
  (should (jupyter-line-count-greater-p "\na\na" 1))
  (should (jupyter-line-count-greater-p "a\na\na" 1))
  (should-not (jupyter-line-count-greater-p "\n\n" 2))
  (should-not (jupyter-line-count-greater-p "\n\n" 3)))

(ert-deftest jupyter-available-local-ports ()
  :tags '(client)
  (let ((ports (jupyter-available-local-ports 5)))
    (should (= (length ports) 5))
    (dolist (p ports) (should (integerp p)))
    (dolist (proc (process-list))
      (should-not (string-match-p "jupyter-available-local-ports"
                                  (process-name proc))))))

(defvar server-mode)
(defvar server-buffer)

(ert-deftest jupyter-server-mode-set-client ()
  :tags '(client)
  (let (server-buffer)
    (with-temp-buffer
      (setq server-buffer (buffer-name))
      (let ((server-mode t)
            (client (jupyter-kernel-client)))
        (should-not jupyter-current-client)
        (with-temp-buffer
          (jupyter-server-mode-set-client client 0.01))
        (should jupyter-current-client)
        (sleep-for 0.1)
        (should-not jupyter-current-client)))))

(ert-deftest jupyter-map-pending-requests ()
  :tags '(client)
  (let ((err (should-error
              (jupyter-map-pending-requests nil #'identity)
              :type 'wrong-type-argument)))
    (should (equal (nth 1 err) 'jupyter-kernel-client)))
  (jupyter-with-echo-client client
    (let ((r1 (jupyter-request :id "id1"))
          (r2 (jupyter-request :id "id2"))
          (mapped nil))
      (puthash "last-sent" r1 (oref client requests))
      (puthash "id1" r1 (oref client requests))
      (puthash "id2" r2 (oref client requests))
      (jupyter-map-pending-requests client
        (lambda (req) (push req mapped)))
      (should (= (length mapped) 2))
      (should (memq r1 mapped))
      (should (memq r2 mapped))

      (setq mapped nil)
      (setf (jupyter-request-idle-received-p r2) t)
      (jupyter-map-pending-requests client
        (lambda (req) (push req mapped)))
      (should (= (length mapped) 1))
      (should (memq r1 mapped))
      (should-not (memq r2 mapped)))))

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

(ert-deftest jupyter-ioloop-wait-until ()
  :tags '(ioloop)
  (let ((ioloop (jupyter-ioloop)))
    (should-not (jupyter-ioloop-last-event ioloop))
    (jupyter-ioloop-start ioloop :test)
    (should (equal (jupyter-ioloop-last-event ioloop) '(start)))
    (jupyter-ioloop-stop ioloop)))

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
      (should (process-live-p (oref ioloop process)))
      (jupyter-ioloop-add-callback ioloop
        `(lambda () (zmq-prin1 (list 'test "message"))))
      (jupyter-ioloop-wait-until ioloop 'test #'identity)
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

(ert-deftest jupyter-channel-ioloop-send-event ()
  :tags '(ioloop)
  (jupyter-test-channel-ioloop
      (ioloop (jupyter-zmq-channel-ioloop))
    (cl-letf (((symbol-function #'jupyter-send)
               (lambda (_channel _msg-type _msg msg-id) msg-id)))
      (setq jupyter-channel-ioloop-session (jupyter-session :key "foo"))
      (push (jupyter-zmq-channel :type :shell) jupyter-channel-ioloop-channels)
      (let* ((msg-id (jupyter-new-uuid))
             (event `(list 'send :shell :execute-request '(msg) ,msg-id)))
        (jupyter-test-ioloop-eval-event ioloop event)
        (ert-info ("Return value to parent process")
          (let ((result (read (buffer-string))))
            (should (equal result `(sent :shell ,msg-id)))))))))

(ert-deftest jupyter-channel-ioloop-start-channel-event ()
  :tags '(ioloop)
  (jupyter-test-channel-ioloop
      (ioloop (jupyter-zmq-channel-ioloop))
    (setq jupyter-channel-ioloop-session (jupyter-session :key "foo"))
    (let ((channel-endpoint "tcp://127.0.0.1:5555"))
      (ert-info ("start-channel event creates channel")
        (should (null jupyter-channel-ioloop-channels))
        (let ((event `(list 'start-channel :shell ,channel-endpoint)))
          (jupyter-test-ioloop-eval-event ioloop event))
        (should-not (null jupyter-channel-ioloop-channels))
        (let ((channel (object-assoc :shell :type jupyter-channel-ioloop-channels)))
          (should (jupyter-zmq-channel-p channel))))
      (let ((channel (object-assoc :shell :type jupyter-channel-ioloop-channels)))
        (with-slots (type socket endpoint) channel
          (ert-info ("Verify the requested channel was started")
            (should (eq type :shell))
            (should (zmq-socket-p socket))
            (should (equal endpoint channel-endpoint))
            (should (equal (zmq-socket-get socket zmq-LAST-ENDPOINT) channel-endpoint))
            (ert-info ("Identity of socket matches session")
              (should (equal (zmq-socket-get socket zmq-IDENTITY)
                             (jupyter-session-id jupyter-channel-ioloop-session)))))
          (ert-info ("Ensure the channel was added to the poller")
            ;; FIXME: Does it make sense to have this side effect as part of starting
            ;; a channel? It makes it so that we don't reference any `zmq' functions
            ;; in `jupyter-channel-ioloop'.
            (should-error
             (zmq-poller-add jupyter-ioloop-poller socket (list zmq-POLLIN))
             :type 'zmq-EINVAL)))
        (ert-info ("Return value to parent process")
          (let ((result (read (buffer-string))))
            (should (equal result `(start-channel :shell)))))))))

(ert-deftest jupyter-channel-ioloop-stop-channel-event ()
  :tags '(ioloop)
  (jupyter-test-channel-ioloop
      (ioloop (jupyter-zmq-channel-ioloop))
    (setq jupyter-channel-ioloop-session (jupyter-session :key "foo"))
    (let ((event `(list 'start-channel :shell "tcp://127.0.0.1:5556")))
      (jupyter-test-ioloop-eval-event ioloop event)
      (erase-buffer))
    (let* ((channel (object-assoc :shell :type jupyter-channel-ioloop-channels))
           (socket (oref channel socket)))
      (ert-info ("Verify the requested channel stops")
        (should (jupyter-channel-alive-p channel))
        (should (progn (zmq-poller-modify
                        jupyter-ioloop-poller
                        (oref channel socket) (list zmq-POLLIN zmq-POLLOUT))
                       t))
        (jupyter-test-ioloop-eval-event ioloop `(list 'stop-channel :shell))
        (should-not (jupyter-channel-alive-p channel)))
      (ert-info ("Ensure the channel was removed from the poller")
        (should-error
         (zmq-poller-modify jupyter-ioloop-poller socket (list zmq-POLLIN))
         :type 'zmq-EINVAL))
      (ert-info ("Return value to parent process")
        (let ((result (read (buffer-string))))
          (should (equal result `(stop-channel :shell))))))))

(ert-deftest jupyter-zmq-channel-ioloop-send-fast ()
  :tags '(ioloop queue)
  ;; :expected-result :failed
  (jupyter-test-with-python-client client
    (let ((jupyter-current-client client))
      (jupyter-send-execute-request client :code "1 + 1")
      (jupyter-send-execute-request client :code "1 + 1")
      (jupyter-send-execute-request client :code "1 + 1")
      (let ((req (jupyter-send-execute-request client :code "1 + 1")))
        (should
         (equal
          (jupyter-message-data
           (jupyter-wait-until-received :execute-result req jupyter-long-timeout)
           :text/plain)
          "2"))))))

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
  (should-not (jupyter-client-has-manager-p))
  (should-not (jupyter-repl-connected-p))
  (jupyter-test-with-python-repl client
    (should (jupyter-client-has-manager-p))
    (should (jupyter-repl-connected-p))))

(ert-deftest jupyter-repl-cell-predicates ()
  :tags '(repl cell)
  (jupyter-test-with-python-repl client
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
  (jupyter-test-with-python-repl client
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
  (jupyter-test-with-python-repl client
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
  (jupyter-test-with-python-repl client
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
  (jupyter-repl-history-add "1")
  (jupyter-repl-history-add "2")
  (jupyter-repl-history-add "3"))

(ert-deftest jupyter-repl-history ()
  :tags '(repl)
  (ert-info ("Adding to REPL history")
    (let ((jupyter-repl-history (make-ring 5)))
      (ring-insert jupyter-repl-history 'jupyter-repl-history)
      (jupyter-repl-history-add "1")
      (should (equal (ring-elements jupyter-repl-history)
                     '("1" jupyter-repl-history)))
      (ert-info ("Reset history before addition")
        (ring-insert-at-beginning
         jupyter-repl-history (ring-remove jupyter-repl-history 0))
        (should (equal (ring-elements jupyter-repl-history)
                       '(jupyter-repl-history "1")))
        (jupyter-repl-history-add "2")
        (should (equal (ring-elements jupyter-repl-history)
                       '("2" "1" jupyter-repl-history))))
      (ert-info ("Drop oldest element when max reached")
        (jupyter-repl-history-add "3")
        (jupyter-repl-history-add "4")
        (should (equal (ring-elements jupyter-repl-history)
                       '("4" "3" "2" "1" jupyter-repl-history)))
        (jupyter-repl-history-add "5")
        (should (equal (ring-elements jupyter-repl-history)
                       '("5" "4" "3" "2" jupyter-repl-history))))))
  (let (jupyter-repl-history)
    (ert-info ("Rotating REPL history ring")
      (ert-info ("Rotating empty ring")
        (setq jupyter-repl-history (make-ring 5))
        (ring-insert jupyter-repl-history 'jupyter-repl-history)
        (should (null (jupyter-repl-history--rotate -1)))
        (should (null (jupyter-repl-history--rotate 0)))
        (should (null (jupyter-repl-history--rotate 1))))
      (jupyter-test-set-dummy-repl-history)
      (should (null (jupyter-repl-history--rotate 1)))
      (ert-info ("No rotation")
        (should (equal (ring-elements jupyter-repl-history)
                       '("3" "2" "1" jupyter-repl-history)))
        (should (equal (jupyter-repl-history--rotate 0) "3"))
        (should (equal (ring-elements jupyter-repl-history)
                       '("3" "2" "1" jupyter-repl-history))))
      (ert-info ("Rotate to older elements")
        (should (equal (jupyter-repl-history--rotate -1) "2"))
        (should (equal (ring-elements jupyter-repl-history)
                       '("2" "1" jupyter-repl-history "3"))))
      (ert-info ("Rotate to newer elements")
        (should (equal (jupyter-repl-history--rotate 1) "3"))
        (should (equal (ring-elements jupyter-repl-history)
                       '("3" "2" "1" jupyter-repl-history))))
      (ert-info ("Rotations stop at sentinel")
        (should (null (jupyter-repl-history--rotate -4)))
        (should (equal (ring-elements jupyter-repl-history)
                       '("1" jupyter-repl-history "3" "2"))))))
  (jupyter-test-with-python-repl client
    (jupyter-ert-info ("Replacing cell contents with history")
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

(ert-deftest jupyter-repl-history-matching ()
  :tags '(repl)
  (jupyter-test-with-python-repl client
    (jupyter-ert-info ("REPL-input-history completion/matching")
      (cl-macrolet ((Rx (l) `(should (equal (reverse (ring-elements
                                                      jupyter-repl-history))
                                            ,l))) ; oldest -> newest
                    (Hp (p n m) ; "helper" (pat, reps, member)
                        `(let ((i (jupyter-repl-history--match-input ,p ,n)))
                           (should (equal ,m (ring-ref jupyter-repl-history i)))
                           (should (eq (< i 0) (< ,n 0)))))) ; obvious?
        (ert-info ("Create dummy history")
          (jupyter-test-set-dummy-repl-history)
          (ring-extend jupyter-repl-history 2)
          (jupyter-repl-history-add "foo.a")
          (jupyter-repl-history-add "1")
          (jupyter-repl-history-add "foo.b")
          (jupyter-repl-history-add "2")
          (jupyter-repl-history-add "foo.c")
          (jupyter-repl-history-add "3"))
        (ert-info ("Baseline")
          (Rx '(jupyter-repl-history "foo.a" "1" "foo.b" "2" "foo.c" "3"))
          (should (equal (jupyter-repl-cell-code) ""))
          (insert "foo")
          (should (equal (jupyter-repl-cell-code) "foo"))
          (should (= (point) (point-max)))
          ;; Prev
          (Hp "^foo" 1 "foo.c")
          (should (integer-or-marker-p
                   (jupyter-repl-history-previous-matching 1)))
          (Rx '("3" jupyter-repl-history "foo.a" "1" "foo.b" "2" "foo.c"))
          (should (equal (jupyter-repl-cell-code) "foo.c"))
          (should (and (looking-back "foo" (point-at-bol))
                      (looking-at-p "\\.c")))
          ;; Next (ding)
          (should-error (jupyter-repl-history-next-matching 1))
          (Rx '("3" jupyter-repl-history "foo.a" "1" "foo.b" "2" "foo.c"))
          (should (equal (jupyter-repl-cell-code) "foo.c")))
        (ert-info ("Initial input matches oldest item")
          ;; Coverage contrivance (but reachable in normal use)
          (jupyter-repl-history-previous)
          (Rx '("foo.c" "3" jupyter-repl-history "foo.a" "1" "foo.b" "2"))
          (should (equal (jupyter-repl-cell-code) "2"))
          (jupyter-repl-replace-cell-code "foo.c")
          (goto-char (jupyter-repl-cell-code-beginning-position))
          (should (and (search-forward "foo") (looking-at-p "\\.c")))
          ;; Next (ding)
          (should-not (jupyter-repl-history--match-input "^foo" -2))
          (should-error (jupyter-repl-history-next-matching 1))
          (Rx '("foo.c" "3" jupyter-repl-history "foo.a" "1" "foo.b" "2"))
          (should (equal (jupyter-repl-cell-code) "foo.c")))
        (ert-info ("Step once if point at bol")
          (goto-char (jupyter-repl-cell-code-beginning-position))
          (should (looking-at-p "foo\\.c"))
          ;; Next
          (Hp "^" -2 "3")
          (should (jupyter-repl-history-next-matching 1))
          (Rx '(jupyter-repl-history "foo.a" "1" "foo.b" "2" "foo.c" "3"))
          (should (equal (jupyter-repl-cell-code) "3"))
          ;; Prev
          (Hp "^" 2 "foo.c")
          (should (jupyter-repl-history-previous-matching 1))
          (Rx '("3" jupyter-repl-history "foo.a" "1" "foo.b" "2" "foo.c"))
          (should (equal (jupyter-repl-cell-code) "foo.c"))
          (should (and (search-forward "foo") (looking-at-p "\\.c"))))
        (ert-info ("Backward to oldest matching element")
          ;; Prev
          (Hp "^foo" 2 "foo.b")
          (should (jupyter-repl-history-previous-matching)) ; n = nil
          (Rx '("2" "foo.c" "3" jupyter-repl-history "foo.a" "1" "foo.b"))
          (should (looking-at-p "\\.b"))
          (should (equal (jupyter-repl-cell-code) "foo.b"))
          ;; Prev
          (Hp "^foo" 2 "foo.a")
          (should (jupyter-repl-history-previous-matching 1))
          (Rx '("1" "foo.b" "2" "foo.c" "3" jupyter-repl-history "foo.a"))
          (should (looking-at-p "\\.a"))
          (should (equal (jupyter-repl-cell-code) "foo.a"))
          ;; Prev (ding) and repeat
          (should-not (jupyter-repl-history--match-input "^foo" 2))
          (should-error (jupyter-repl-history-previous-matching 1))
          (Rx '("1" "foo.b" "2" "foo.c" "3" jupyter-repl-history "foo.a"))
          (should-error (jupyter-repl-history-previous-matching 1))
          (Rx '("1" "foo.b" "2" "foo.c" "3" jupyter-repl-history "foo.a"))
          (should (equal (jupyter-repl-cell-code) "foo.a")))
        (ert-info ("Forward to most recent matching element")
          ;; Next
          (Hp "^foo" -1 "foo.b")
          (should (jupyter-repl-history-next-matching)) ; n = nil
          (Rx '("2" "foo.c" "3" jupyter-repl-history "foo.a" "1" "foo.b"))
          (should (equal (jupyter-repl-cell-code) "foo.b"))
          ;; Next
          (Hp "^foo" -1 "foo.c")
          (should (jupyter-repl-history-next-matching 1))
          (Rx '("3" jupyter-repl-history "foo.a" "1" "foo.b" "2" "foo.c"))
          (should (equal (jupyter-repl-cell-code) "foo.c"))
          ;; Next (ding)
          (should-error (jupyter-repl-history-next-matching 1))
          (Rx '("3" jupyter-repl-history "foo.a" "1" "foo.b" "2" "foo.c"))
          (should (looking-at-p "\\.c")))
        (ert-info ("Scaled")
          ;; Prev 2x
          (Hp "^foo" 3 "foo.a")
          (should (jupyter-repl-history-previous-matching 2))
          (Rx '("1" "foo.b" "2" "foo.c" "3" jupyter-repl-history "foo.a"))
          (should (equal (jupyter-repl-cell-code) "foo.a"))
          ;; Next 2x
          (Hp "^foo" -2 "foo.c")
          (should (jupyter-repl-history-next-matching 2))
          (Rx '("3" jupyter-repl-history "foo.a" "1" "foo.b" "2" "foo.c"))
          (should (equal (jupyter-repl-cell-code) "foo.c")))
        (ert-info ("Basic history commands still work")
          (jupyter-repl-history-next)
          (Rx '(jupyter-repl-history "foo.a" "1" "foo.b" "2" "foo.c" "3"))
          (should (equal (jupyter-repl-cell-code) "3"))
          (jupyter-repl-history-next)
          (should (equal (jupyter-repl-cell-code) "")))))))

(ert-deftest jupyter-repl-cell-motions ()
  :tags '(repl motion)
  (jupyter-test-with-python-repl client
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

(ert-deftest jupyter-repl-finalize-cell ()
  :tags '(repl)
  (jupyter-test-with-python-repl client
    (jupyter-ert-info ("Finalize the last cell only")
      (should-not (jupyter-repl-cell-finalized-p))
      (jupyter-test-repl-ret-sync)
      (should-not (jupyter-repl-cell-finalized-p))
      (jupyter-repl-backward-cell)
      (should (jupyter-repl-cell-finalized-p))
      (should-not (= (point) (point-max)))
      (jupyter-repl-finalize-cell nil)
      (should (= (point) (point-max)))
      (should (jupyter-repl-cell-finalized-p)))
    (jupyter-ert-info
        ("Don't modify the jupyter-request property of a finalized cell")
      (jupyter-test-repl-ret-sync)
      (jupyter-repl-backward-cell)
      (let* ((finalized-cell-beg (jupyter-repl-cell-beginning-position))
             (finalized-cell-req
              (get-text-property finalized-cell-beg 'jupyter-request)))
        (should finalized-cell-req)
        (jupyter-repl-finalize-cell nil)
        (should (eq (get-text-property finalized-cell-beg 'jupyter-request)
                    finalized-cell-req))))))

(ert-deftest jupyter-repl-cell-positions ()
  :tags '(repl motion)
  (jupyter-test-with-python-repl client
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

(ert-deftest jupyter-repl-map-cells ()
  :tags '(repl)
  (with-temp-buffer
    (insert "foo")
    (insert (propertize "bar" 'field 'cell-code))
    (insert "baz")
    (let (input output)
      (jupyter-repl-map-cells (point-min) (point-max)
        (lambda () (push (buffer-string) input))
        (lambda () (push (buffer-string) output)))
      (should (equal input '("bar")))
      (should (equal output '("baz" "foo"))))))

(ert-deftest jupyter-repl-restart-kernel ()
  :tags '(repl restart)
  (let ((jupyter-test-with-new-client t))
    (jupyter-test-with-python-repl client
      (jupyter-ert-info ("Restart without errors")
        (should (equal (oref client execution-state) "idle"))
        ;; Increment the cell count just to make sure it gets reset to 1 after
        ;; a restart
        (jupyter-repl-update-cell-count 2)
        (let* ((pos (jupyter-repl-cell-beginning-position))
               (restart-p nil)
               (jupyter-include-other-output t)
               (jupyter-iopub-message-hook
                (lambda (_ msg)
                  (when (jupyter-message-status-starting-p msg)
                    (setq restart-p t)))))
          (should-not (jupyter-repl-cell-finalized-p))
          (jupyter-repl-restart-kernel)
          ;; Attempt to catch the status: starting message
          (jupyter-with-timeout (nil jupyter-long-timeout)
            restart-p)
          (should (jupyter-kernel-info client))
          (should (equal (jupyter-repl-cell-code-beginning-position) (point)))
          (should-not (jupyter-repl-cell-finalized-p))
          (goto-char pos)
          (should (jupyter-repl-cell-finalized-p))
          (goto-char (point-max))
          (should (= (jupyter-repl-cell-count) 1))
          (jupyter-repl-sync-execution-state)
          (should (equal (jupyter-execution-state client) "idle")))))))

(ert-deftest jupyter-repl-prompts ()
  :tags '(repl prompt)
  (jupyter-test-with-python-repl client
    (let (cell-prop)
      (jupyter-ert-info ("Prompt properties")
        (let (prompt-overlay)
          (goto-char (jupyter-repl-cell-beginning-position))
          (setq prompt-overlay (car (overlays-at (point))))
          (should-not (null prompt-overlay))
          (setq cell-prop (get-text-property (point) 'jupyter-cell))
          (should (eq (car cell-prop) 'beginning))
          (should (and (numberp (cadr cell-prop))
                       (>= (cadr cell-prop) 1)))
          (should (= (jupyter-repl-cell-count) (cadr cell-prop)))))
      (jupyter-ert-info ("Input prompts")
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
          (should (equal (cadr cell-property) cell-prop)))))
    (jupyter-ert-info ("Continuation prompts")

      )
    (jupyter-ert-info ("Output prompts")

      )))

(ert-deftest jupyter-repl-prompt-margin ()
  :tags '(repl prompt)
  (jupyter-test-with-python-repl client
    (let ((inhibit-read-only t))
      (erase-buffer))
    (let ((jupyter-repl-prompt-margin-width 2))
      (jupyter-repl--reset-prompts)
      (should (= jupyter-repl-prompt-margin-width 2))
      (should (= left-margin-width 2))
      (jupyter-repl-insert-prompt)
      (should (> left-margin-width 2))
      (should (= left-margin-width jupyter-repl-prompt-margin-width))
      (should (= jupyter-repl-prompt-margin-width
                 (length (jupyter-repl-prompt-string)))))))

(ert-deftest jupyter-repl-yank ()
  :tags '(repl yank)
  (jupyter-test-with-python-repl client
    (jupyter-ert-info ("Ensure field property exists after yanking")
      (kill-new "import foo")
      (yank)
      (should (equal (jupyter-repl-cell-code) "import foo"))
      (should-not (text-property-not-all
                   (jupyter-repl-cell-code-beginning-position)
                   (jupyter-repl-cell-code-end-position)
                   'field 'cell-code)))
    (jupyter-ert-info ("Undo rear-nonsticky property inserted by `insert-for-yank'")
      (kill-new "import foo")
      (yank)
      (should (equal (jupyter-repl-cell-code) "import foo"))
      (should-not (get-text-property (1- (jupyter-repl-cell-code-end-position))
                                     'rear-nonsticky)))))

(ert-deftest jupyter-repl-syntax-propertize-function ()
  :tags '(repl)
  ;; TODO: Test field = `cell-code` path
  (jupyter-test-with-python-repl client
    (with-temp-buffer
      (let ((jupyter-current-client client))
        (insert "(foo) bar")
        (jupyter-repl-syntax-propertize-function #'ignore (point-min) (point-max))
        (jupyter-test-text-has-property 'syntax-table '(1 . ?.) '(1 5))
        (erase-buffer)
        (insert "(foo)")
        (jupyter-repl-syntax-propertize-function #'ignore (point-min) (point-max))
        (jupyter-test-text-has-property 'syntax-table '(1 . ?.) '(1 5))
        (erase-buffer)
        (insert "foo (bar)")
        (jupyter-repl-syntax-propertize-function #'ignore (point-min) (point-max))
        (jupyter-test-text-has-property 'syntax-table '(1 . ?.) '(5 9))))))

(ert-deftest jupyter-repl-undo ()
  :tags '(repl yank undo)
  (let ((ensure-field-property
         (lambda ()
           (should-not
            (text-property-not-all
             (jupyter-repl-cell-code-beginning-position)
             (jupyter-repl-cell-code-end-position)
             'field 'cell-code)))))
    (jupyter-test-with-python-repl client
      (jupyter-ert-info ("Undo after yank undoes all the yanked text")
        (kill-new  "import IPython\ndef foo(x)\n\treturn x")
        (undo-boundary)
        (yank)
        (should (equal (jupyter-repl-cell-code) "import IPython\ndef foo(x)\n\treturn x"))
        (funcall ensure-field-property)
        (let ((beg (jupyter-repl-cell-beginning-position)))
          (undo)
          (should (get-text-property beg 'jupyter-cell))
          (goto-char (point-max))
          (should (equal (jupyter-repl-cell-code) ""))))
      (jupyter-ert-info ("Correct undo after inserting continuation prompt")
        ;; See #139
        (insert "\
for item in range(10):
    print(item)")
        (backward-char)
        (undo-boundary)
        (jupyter-test-repl-ret-sync)
        (undo-boundary)
        (should (equal (jupyter-repl-cell-code) "\
for item in range(10):
    print(item
    )"))
        (funcall ensure-field-property)
        (undo)
        (should (equal (jupyter-repl-cell-code) "\
for item in range(10):
    print(item)")))
      (jupyter-ert-info ("Passing through `jupyter-repl-indent-line'")
        (insert "\
next(x")
        (undo-boundary)
        (jupyter-test-repl-ret-sync)
        (undo-boundary)
        (should (equal (jupyter-repl-cell-code)
                       "\
next(x
     "))
        (funcall ensure-field-property)
        (undo)
        (should (equal (jupyter-repl-cell-code)
                       "\
next(x"))))))

(ert-deftest jupyter-repl-after-change ()
  :tags '(repl)
  (jupyter-test-with-python-repl client
    ;; See #38
    (jupyter-ert-info ("Maintain field membership after deleting text at beginning of cell")
      (insert "foo(")
      (should (eql (field-at-pos (jupyter-repl-cell-code-beginning-position)) 'cell-code))
      (backward-char)
      (backward-kill-word 1)
      (should (eql (field-at-pos (jupyter-repl-cell-code-beginning-position)) 'cell-code)))))

(ert-deftest jupyter-repl-propagate-client ()
  :tags '(repl)
  (with-temp-buffer
    (setq jupyter-current-client (jupyter-repl-client))
    (oset jupyter-current-client kernel-info
          (list :language_info
                ;; :name is a symbol, see `jupyter-kernel-info'
                (list :name 'python :file_extension "py")))
    (let ((buffer (generate-new-buffer " *temp*")))
      (unwind-protect
          (progn
            (ert-info ("Verify that `jupyter-current-client' is actually set")
              (should-not (buffer-local-value 'jupyter-current-client buffer))
              (with-current-buffer buffer
                (python-mode))
              (jupyter-repl-propagate-client buffer)
              (should (eq (buffer-local-value 'jupyter-current-client buffer)
                          jupyter-current-client)))
            (ert-info ("Robust to bad arguments")
              (jupyter-repl-propagate-client 1)
              (jupyter-repl-propagate-client
               (generate-new-buffer-name "foo"))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest jupyter-connect-repl ()
  :tags '(repl)
  (jupyter-test-with-python-repl client
    (let ((cclient (jupyter-connect-repl
                    (jupyter-session-conn-info
                     (oref client session)))))
      (unwind-protect
          (let ((msg (jupyter-wait-until-received :execute-result
                       (let ((jupyter-inhibit-handlers t))
                         (jupyter-send-execute-request cclient
                           :code "1 + 1")))))
            (should msg)
            (should (equal (jupyter-message-data msg :text/plain) "2")))
        (with-current-buffer (oref cclient buffer)
          (jupyter-stop-channels cclient)
          (let ((kill-buffer-query-functions nil))
            (kill-buffer)))))))

(ert-deftest jupyter-repl-echo-eval-p ()
  :tags '(repl)
  (jupyter-test-with-python-repl client
    (jupyter-ert-info ("Copying input")
      (let ((jupyter-repl-echo-eval-p t))
        (should (equal (jupyter-repl-cell-code) ""))
        (let ((req (jupyter-eval-string "1 + 1")))
          (should-not (jupyter-request-inhibited-handlers req))
          (jupyter-wait-until-idle req)
          (jupyter-repl-goto-cell req)
          (should (equal (jupyter-repl-cell-code) "1 + 1")))))
    (jupyter-ert-info ("Preserving code")
      (let ((jupyter-repl-echo-eval-p t))
        (jupyter-repl-replace-cell-code "2 + 2")
        (should (equal (jupyter-repl-cell-code) "2 + 2"))
        (let ((req (jupyter-eval-string "1 + 1")))
          (should-not (jupyter-request-inhibited-handlers req))
          (jupyter-wait-until-idle req)
          (goto-char (point-max))
          (should (equal (jupyter-repl-cell-code) "2 + 2")))))
    (jupyter-ert-info ("Not copying input")
      (let ((jupyter-repl-echo-eval-p nil))
        (should (equal (jupyter-repl-cell-code) ""))
        (let ((req (jupyter-eval-string "1 + 1")))
          (should (jupyter-request-inhibited-handlers req))
          (jupyter-wait-until-idle req)
          (should-error (jupyter-repl-goto-cell req)))))
    (ert-info ("Add callbacks when REPL buffer is invisible")
      (cl-letf (((symbol-function #'get-buffer-window)
                 (lambda (&rest _) nil)))
        (ert-info ("`jupyter-repl-echo-eval-p' = t")
          (let* ((jupyter-repl-echo-eval-p t)
                 (req (jupyter-eval-string "1 + 1")))
            (should-not (jupyter-request-inhibited-handlers req))
            (should (jupyter-request-callbacks req))
            (jupyter-wait-until-idle req)))
        (ert-info ("`jupyter-repl-echo-eval-p' = nil")
          (let* ((jupyter-repl-echo-eval-p nil)
                 (req (jupyter-eval-string "1 + 1")))
            (should (jupyter-request-inhibited-handlers req))
            (should (jupyter-request-callbacks req))
            (jupyter-wait-until-idle req)))))
    (ert-info ("No callbacks when REPL buffer visible")
      (cl-letf (((symbol-function #'get-buffer-window)
                 (lambda (&rest _) (selected-window))))
        (ert-info ("`jupyter-repl-echo-eval-p' = t")
          (let* ((jupyter-repl-echo-eval-p t)
                 (req (jupyter-eval-string "1 + 1")))
            (should-not (jupyter-request-inhibited-handlers req))
            (should-not (jupyter-request-callbacks req))
            (jupyter-wait-until-idle req)))
        (ert-info ("`jupyter-repl-echo-eval-p' = nil")
          (let* ((jupyter-repl-echo-eval-p nil)
                 (req (jupyter-eval-string "1 + 1")))
            (should (jupyter-request-inhibited-handlers req))
            (should (jupyter-request-callbacks req))
            (jupyter-wait-until-idle req)))))))

(ert-deftest jupyter-repl-issue-219 ()
  :tags '(repl)
  (skip-unless (version<= "27" emacs-version))
  ;; A symptom of the REPL not setting the right text properties is an
  ;; un-terminating loop in `font-lock-default-fontify-region' this test
  ;; catches that.
  (jupyter-test-with-python-repl client
    (jupyter-with-repl-buffer client
      (jupyter-repl-replace-cell-code "l = [1,2,3]")
      (jupyter-test-repl-ret-sync)
      (let* ((count 0)
             (font-lock-extend-region-functions
              (cons (lambda ()
                      (when (> (setq count (1+ count)) 100)
                        (error "Un-terminating `font-lock-extend-region-functions' handling."))
                      nil)
                    font-lock-extend-region-functions)))
        (font-lock-ensure)))))

;;; `org-mode'

(defvar org-babel-jupyter-resource-directory nil)

(ert-deftest org-babel-jupyter-parse-session ()
  :tags '(org)
  (require 'ob-jupyter)
  (ert-info ("Sessions with a kernel connection file")
    (let ((session (org-babel-jupyter-parse-session "/foo/bar.json")))
      (should (org-babel-jupyter-remote-session-p session))
      (should (org-babel-jupyter-remote-session-connect-repl-p session))
      (should (equal (org-babel-jupyter-session-name session) "/foo/bar.json")))
    (let ((session (org-babel-jupyter-parse-session "/ssh:ec2:foo/bar.json")))
      (should (org-babel-jupyter-remote-session-p session))
      (should (org-babel-jupyter-remote-session-connect-repl-p session))
      (should (equal (org-babel-jupyter-session-name session) "/ssh:ec2:foo/bar.json"))))
  (ert-info ("Server sessions")
    (let ((session (org-babel-jupyter-parse-session "/jpy::foo/bar.json")))
      (should (org-babel-jupyter-server-session-p session))
      (should (equal (org-babel-jupyter-session-name session) "/jpy::foo/bar.json")))
    (let ((session (org-babel-jupyter-parse-session "/jpy::foo/bar")))
      (should (org-babel-jupyter-server-session-p session))
      (should (equal (org-babel-jupyter-session-name session) "/jpy::foo/bar"))))
  (ert-info ("Other remote sessions")
    (let ((session (org-babel-jupyter-parse-session "/ssh:ec2:foo/bar")))
      (should (org-babel-jupyter-remote-session-p session))
      (should-not (org-babel-jupyter-remote-session-connect-repl-p session))
      (should (equal (org-babel-jupyter-session-name session) "/ssh:ec2:foo/bar"))))
  (let ((session (org-babel-jupyter-parse-session "foo/bar")))
    (should (org-babel-jupyter-session-p session))
    (should (equal (org-babel-jupyter-session-name session) "foo/bar"))))

(ert-deftest org-babel-jupyter-initiate-session-by-key ()
  :tags '(org)
  (jupyter-org-test
   (let ((session (make-temp-name "jupyter")))
     (save-excursion
       (insert
        (format "#+begin_src jupyter-python :session %s\n1+1\n#+end_src\n"
                session)))
     (let* ((params (nth 2 (org-babel-get-src-block-info)))
            (key (org-babel-jupyter-session-key params)))
       (should-not (gethash key org-babel-jupyter-session-clients))
       (let ((buffer (org-babel-jupyter-initiate-session-by-key
                      session params))
             (client (gethash key org-babel-jupyter-session-clients)))
         (should (bufferp buffer))
         (should client)
         (should (object-of-class-p client 'jupyter-repl-client))
         (should (eq buffer (oref client buffer)))
         (jupyter-test-kill-buffer buffer)
         (should-not (gethash key org-babel-jupyter-session-clients)))))))

(ert-deftest ob-jupyter-no-results ()
  :tags '(org)
  (jupyter-org-test-src-block "1 + 1;" ""))

(ert-deftest ob-jupyter-scalar-results ()
  :tags '(org)
  (jupyter-org-test-src-block "1 + 1" ": 2\n")
  (ert-info ("Tables")
    (jupyter-org-test-src-block
     "[[1, 2, 3], [4, 5, 6]]"
     "\
| 1 | 2 | 3 |
| 4 | 5 | 6 |
")))

(ert-deftest ob-jupyter-html-results ()
  :tags '(org)
  (jupyter-org-test-src-block
   "\
from IPython.core.display import HTML
HTML('<a href=\"http://foo.com\">link</a>')"
   "\
#+BEGIN_EXPORT html
<a href=\"http://foo.com\">link</a>
#+END_EXPORT
"))

(ert-deftest ob-jupyter-markdown-results ()
  :tags '(org)
  (jupyter-org-test-src-block
   "\
from IPython.core.display import Markdown
Markdown('*b*')"
   "\
#+BEGIN_EXPORT markdown
*b*
#+END_EXPORT
"))

(ert-deftest ob-jupyter-latex-results ()
  :tags '(org)
  (jupyter-org-test-src-block
   "\
from IPython.core.display import Latex
Latex(r'$\\alpha$')"
   "\
#+BEGIN_EXPORT latex
$\\alpha$
#+END_EXPORT
")
  (ert-info ("Raw results")
    (jupyter-org-test-src-block
     "\
from IPython.core.display import Latex
Latex(r'$\\alpha$')"
     "$\\alpha$\n"
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
             (thread-first jupyter-current-client
               (slot-value 'manager)
               (slot-value 'kernel)
               (slot-value 'spec)))))
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
           (format "[[file:%s]]\n" image-file-name))
          (ert-info ("Create a drawer containing file links")
            (jupyter-org-test-src-block
             (format "\
from IPython.display import Image
from IPython.display import display
display(Image(filename='%s'))
Image(filename='%s')" file file)
             (concat
              ":RESULTS:\n"
              (format "[[file:%s]]" image-file-name) "\n"
              (format "[[file:%s]]" image-file-name) "\n"
              ":END:\n")
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
              ":RESULTS:\n"
              (format "[[file:%s]]" image-file-name) "\n"
              (format "[[file:%s]]" image-file-name) "\n"
              (format "[[file:%s]]" image-file-name) "\n"
              ":END:\n")
             :async "yes"))
          (ert-info ("Image with width and height metadata")
            (jupyter-org-test-src-block
             (format "\
from IPython.display import Image
Image(filename='%s', width=300)" file)
             (concat
              ":RESULTS:\n"
              "#+ATTR_ORG: :width 300\n"
              (format "[[file:%s]]" image-file-name) "\n"
              ":END:\n"))))
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

(ert-deftest jupyter-org-request-at-point ()
  :tags '(org)
  (jupyter-org-test
   (insert (format "\
#+begin_src jupyter-python :session %s :async yes
1 + 1;
#+end_src" jupyter-org-test-session))
   (goto-char (point-min))
   (org-babel-execute-src-block)
   (let ((req (jupyter-org-request-at-point)))
     (should req)
     (should (jupyter-org-request-p req))
     (jupyter-wait-until-idle req)
     (should-not (jupyter-org-request-at-point)))))

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
          (list :language_info (list :name 'python)))
    (should (equal (jupyter-kernel-language jupyter-current-client) 'python))
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
      "#+BEGIN_SRC jupyter-python :session " jupyter-org-test-session "\n"
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
      "#+BEGIN_SRC jupyter-python :session " jupyter-org-test-session "\n"
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

(ert-deftest jupyter-org--append-to-fixed-width ()
  :tags '(org)
  (with-temp-buffer
    (org-mode)
    (pop-to-buffer (current-buffer))
    (insert ": foo\n")
    (skip-chars-backward "\n")
    (jupyter-org--append-to-fixed-width "bar" nil)
    (should (equal (buffer-string) ": foobar\n"))
    (skip-chars-backward "\n")
    (jupyter-org--append-to-fixed-width "bar" t)
    (should (equal (buffer-string) "\
: foobar
: bar
"))
    (skip-chars-backward "\n")
    (jupyter-org--append-to-fixed-width "a\nb" nil)
    (should (equal (buffer-string) "\
: foobar
: bara
: b
"))
    (skip-chars-backward "\n")
    (jupyter-org--append-to-fixed-width "a\nb" t)
    (should (equal (buffer-string) "\
: foobar
: bara
: b
: a
: b
"))))

(defvar org-edit-src-preserve-indentation)
(defvar org-src-preserve-indentation)

(ert-deftest jupyter-org--append-to-example-block ()
  :tags '(org)
  (let ((org-src-preserve-indentation 0))
    (with-temp-buffer
      (org-mode)
      (insert "\
#+begin_example
|
#+end_example
")
      (search-backward "|")
      (forward-char)
      (jupyter-org--append-to-example-block "a\nb" nil)
      (should (equal (buffer-string) "\
#+begin_example
|a
b
#+end_example
"))
      (backward-char)
      (jupyter-org--append-to-example-block "a\nb" t)
      (should (equal (buffer-string) "\
#+begin_example
|a
b
a
b
#+end_example
"))))
  (when (version<= "9.2" (org-version))
    (let ((org-src-preserve-indentation nil)
          (org-edit-src-preserve-indentation 2))
      (ert-info ("Example block indentation")
        (with-temp-buffer
          (org-mode)
          (insert "\
#+begin_example
  |
#+end_example
")
          (search-backward "|")
          (forward-char)
          (jupyter-org--append-to-example-block "ab" nil)
          (should (equal (buffer-string) "\
#+begin_example
  |ab
#+end_example
"))
          (backward-char)
          (jupyter-org--append-to-example-block "ab" t)
          (should (equal (buffer-string) "\
#+begin_example
  |ab
  ab
#+end_example
"))
          (backward-char)
          ;; TODO: What to about the case of removing the common indentation
          ;; while appending to a line?
          (jupyter-org--append-to-example-block " a\n  b" nil)
          (should (equal (buffer-string) "\
#+begin_example
  |ab
  ab a
  b
#+end_example
"))
          (backward-char)
          (jupyter-org--append-to-example-block " a\n b" t)
          (should (equal (buffer-string) "\
#+begin_example
  |ab
  ab a
  b
  a
  b
#+end_example
")))))))

(ert-deftest jupyter-org-indent-inserted-region ()
  :tags '(org)
  (with-temp-buffer
    (insert " a")
    (jupyter-org-indent-inserted-region nil
      (insert "\nb\n c\n"))
    (should (equal (buffer-string) " a\n b\n  c\n"))
    (erase-buffer)
    (jupyter-org-indent-inserted-region 2
      (insert "a\n b\n c\n"))
    (should (equal (buffer-string) "  a\n   b\n   c\n"))
    (erase-buffer)
    (jupyter-org-indent-inserted-region nil
      (insert " a\nb\nc"))
    (should (equal (buffer-string) " a\nb\nc"))))

(ert-deftest jupyter-org-coalesce-stream-results ()
  :tags '(org)
  (let ((org-edit-src-content-indentation 0))
    (ert-info ("Synchronous")
      (jupyter-org-test-src-block
       "\
print(\"foo\")
print(\"foo\", flush=True)
print(\"foo\")"
       "\
: foo
: foo
: foo
"))
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
: foo
"
         :async "yes")
        (jupyter-org-test-src-block
         "\
print(\"foo\", flush=True)
print(\"foo\", end=\"\", flush=True)
print(\"foo\")"
         "\
: foo
: foofoo
")
        :async "yes")
      (ert-info ("No newline after first stream message")
        (jupyter-org-test-src-block
         "\
print(\"foo\")
print(\"foo\", end=\"\", flush=True)
print(\"bar\")"
         "\
: foo
: foobar
"
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
: qux
"
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
: qux
"
           :async "yes")))
      (ert-info ("fixed-width to example-block promotion")
        (let ((org-babel-min-lines-for-block-output 2))
          (jupyter-org-test-src-block "print(\"z\")" ": z\n")
          (jupyter-org-test-src-block
           "\
print(\"z\", flush=True)
print(\"z\")"
           "\
#+BEGIN_EXAMPLE
z
z
#+END_EXAMPLE
"
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
#+END_EXAMPLE
"
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
#+END_EXAMPLE
"
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
#+END_EXAMPLE
"
             :async "yes")))))))


(ert-deftest jupyter-org-example-block-indentation ()
  :tags '(org)
  (skip-unless (version<= "9.2" (org-version)))
  (let ((org-babel-min-lines-for-block-output 2)
        (org-edit-src-content-indentation 2))
    (ert-info ("Appending obeys `org-edit-src-content-indentation'")
      (jupyter-org-test-src-block
       "\
print(\"z\", flush=True)
print(\"z\")"
       "\
#+BEGIN_EXAMPLE
  z
  z
#+END_EXAMPLE
"
       :async "yes"))))

(ert-deftest jupyter-org-wrapping-with-drawer ()
  :tags '(org)
  (ert-info ("Preserve whitespace after wrapping a result")
    (jupyter-org-test-src-block
     "\
print(\"foo\", flush=True)
1 + 1"
     "\
:RESULTS:
: foo
: 2
:END:
"
     :async "yes")
    (jupyter-org-test-src-block
     "\
print(\"foo\", flush=True)
1 + 1"
     "\
:RESULTS:
: foo
: 2
:END:
")))

(ert-deftest jupyter-org-issue-126 ()
  :tags '(org)
  (jupyter-org-test
   (insert (format "\
* H1
  - list1
    #+begin_src jupyter-python :session %s :async yes
      print(\"Hello\")
    #+end_src

* H2
" jupyter-org-test-session))
   (org-babel-previous-src-block)
   (org-babel-execute-src-block)
   (with-current-buffer (org-babel-initiate-session)
     (jupyter-wait-until-idle (jupyter-last-sent-request jupyter-current-client)))
   (org-back-to-heading)
   (org-down-element)
   (should (eq (org-element-type (org-element-context)) 'plain-list))
   (org-babel-next-src-block)
   (should (eq (org-element-type (org-element-context)) 'src-block))
   (should (org-babel-where-is-src-block-result))
   (goto-char (org-babel-where-is-src-block-result))
   (let ((result (org-element-context)))
     (should (eq (org-element-type result) 'fixed-width))
     (should (equal (org-element-property :value result) "Hello")))))

(ert-deftest jupyter-org-font-lock-ansi-escapes ()
  :tags '(org)
  (jupyter-org-test-src-block
   "print('AB\x1b[43mCD\x1b[0mEF')"
   ": AB[43mCD[0mEF\n")
  (jupyter-org-test-src-block
   "\
from IPython.display import publish_display_data
publish_display_data({'text/plain': 'AB\x1b[43mCD\x1b[0mEF'});"
   ": AB[43mCD[0mEF\n")
  (with-temp-buffer
    (org-mode)
    (jupyter-org-interaction-mode 1)
    (let ((test-fun
           (lambda (face-pos invisible-pos)
             (font-lock-ensure)
             (jupyter-test-text-has-property 'invisible t invisible-pos)
             (should (listp (get-text-property face-pos 'face)))
             (should (get-text-property face-pos 'font-lock-face))
             (should (eq (caar (get-text-property face-pos 'face)) 'background-color)))))
      (insert ": AB[43mCD[0mEF")
      (funcall test-fun 10 '(5 6 7 8 9 12 13 14 15))
      ;; Test the cached faces path
      (remove-text-properties (point-min) (point-max) '(face))
      (funcall test-fun 10 '(5 6 7 8 9 12 13 14 15))
      (erase-buffer)
      (insert "\
#+begin_example
AB[43mCD[0mEF
#+end_example")
      ;; Test the cached faces path
      (remove-text-properties (point-min) (point-max) '(face))
      (funcall test-fun 24 '(19 20 21 22 23 26 27 28 29))))
  (ert-info ("Leading indentation")
    (with-temp-buffer
      (org-mode)
      (jupyter-org-interaction-mode 1)
      (pop-to-buffer (current-buffer))
      (let ((beg (+ (point-min) 2)) end)
        (insert "  : AB[43mCD[0mEF\n")
        (insert "  : AB[43mCD[0mEF\n")
        (setq end (1- (point)))
        (insert "hey\n")
        (goto-char (point-min))
        (jupyter-org-font-lock-ansi-escapes (point-max))
        (should-not (text-property-not-all beg end 'jupyter-ansi t))))))

(ert-deftest jupyter-org-closest-jupyter-language ()
  :tags '(org)
  (jupyter-org-test
   (insert "\
#+BEGIN_SRC jupyter-foo
#+END_SRC

x

#+BEGIN_SRC jupyter-bar
#+END_SRC

#+BEGIN_SRC baz
#+END_SRC
")
   (re-search-backward "x")
   (should (equal (jupyter-org-closest-jupyter-language)
                  "jupyter-bar"))
   (forward-line -1)
   (should (equal (jupyter-org-closest-jupyter-language)
                  "jupyter-foo"))
   (forward-line 2)
   (should (equal (jupyter-org-closest-jupyter-language)
                  "jupyter-bar"))
   (goto-char (point-max))
   (should (equal (jupyter-org-closest-jupyter-language)
                  "jupyter-bar"))
   (forward-line -3)
   (should (equal (jupyter-org-closest-jupyter-language)
                  "jupyter-bar"))))

(ert-deftest jupyter-org-define-key ()
  :tags '(org)
  (jupyter-org-test
   (save-excursion
     (insert (format "\
#+begin_src jupyter-python :session %s
1 + 1
#+end_src" jupyter-org-test-session)))
   ;; Needed for the text properties
   (font-lock-ensure)
   (forward-line)
   (let ((test-key
          (lambda (key &optional lang no-def)
            (or lang (setq lang 'jupyter))
            (let* ((jupyter-org-interaction-mode-map (make-sparse-keymap))
                   (test-def-called nil)
                   (test-def (lambda ()
                               (interactive)
                               (setq test-def-called t))))
              (jupyter-org-define-key key test-def lang)
              (let ((def (lookup-key jupyter-org-interaction-mode-map key)))
                (if no-def (should-not def)
                  (should (functionp def))
                  (call-interactively test-def)
                  (should test-def-called)))))))
     (ert-info ("Simple definition")
       (funcall test-key "i"))
     (ert-info ("Prefix keys")
       (funcall test-key (kbd "C-x C-e")))
     (ert-info ("Language based keys")
       (funcall test-key (kbd "g") 'python)
       (funcall test-key (kbd "g") 'julia 'no-def))
     (forward-line -1)
     (ert-info ("No definition outside source block")
       (funcall test-key (kbd "g") 'python 'no-def)))))

(ert-deftest org-babel-jupyter-src-block-session ()
  :tags '(org)
  (jupyter-org-test
   (insert "\
#+BEGIN_SRC jupyter-foo :session bar
#+END_SRC")
   (goto-char (point-min))
   (should-error (org-babel-jupyter-src-block-session))
   (erase-buffer)
   (insert "\
#+BEGIN_SRC jupyter-foo :kernel bar
#+END_SRC")
   (goto-char (point-min))
   (should-error (org-babel-jupyter-src-block-session))
   (erase-buffer)
   (insert "\
#+BEGIN_SRC jupyter-foo :session bar :kernel bar
#+END_SRC")
   (goto-char (point-min))
   (should (equal (org-babel-jupyter-src-block-session)
                  (org-babel-jupyter-session-key
                   (nth 2 (org-babel-get-src-block-info 'light)))))
   (erase-buffer)

   (insert "\
#+NAME: foo
#+BEGIN_SRC jupyter-foo :session bar :kernel bar
#+END_SRC

#+CALL: foo()")
   (should (equal (org-babel-jupyter-src-block-session)
                  (org-babel-jupyter-session-key
                   (nth 2 (org-babel-lob-get-info)))))))

(ert-deftest org-babel-jupyter-override-src-block ()
  :tags '(org)
  (let* ((lang (cl-gensym))
         (overriding-funs (cl-set-difference
                           org-babel-jupyter--babel-ops
                           '(variable-assignments expand-body))))
	(cl-macrolet
		((var-symbol
		  (var lang)
		  `(org-babel-jupyter--babel-var-symbol ,var ,lang))
		 (op-symbol
		  (op lang)
		  `(org-babel-jupyter--babel-op-symbol ,op ,lang))
		 (advice-p
		  (not name)
		  `(,(if not 'should-not 'should)
			(advice-member-p 'ob-jupyter ,name))))
	  (set (var-symbol 'header-args (format "jupyter-%s" lang))
		   '((:kernel . "foo")))
	  (unwind-protect
		  (progn
			(ert-info ("Overriding")
			  (org-babel-jupyter-override-src-block lang)
			  (dolist (fn overriding-funs)
				(advice-p nil (op-symbol fn lang)))
			  (should (equal (symbol-value (var-symbol 'header-args lang))
							 '((:kernel . "foo")))))
			(ert-info ("Restoring")
			  (org-babel-jupyter-restore-src-block lang)
			  (dolist (fn overriding-funs)
				(advice-p t (op-symbol fn lang)))
			  (should-not (symbol-value (var-symbol 'header-args lang)))))
		(dolist (op org-babel-jupyter--babel-ops)
		  (obarray-remove obarray (op-symbol op lang)))
		(obarray-remove obarray (var-symbol 'header-args (format "jupyter-%s" lang)))
		(dolist (op org-babel-jupyter--babel-vars)
		  (obarray-remove obarray (var-symbol op lang)))))))

(ert-deftest org-babel-jupyter-strip-ansi-escapes ()
  :tags '(org)
  (jupyter-org-test
   (insert "\
#+BEGIN_SRC jupyter-foo
#+END_SRC

#+RESULTS:
: AB[43mCD[0mEF\n")
   (org-babel-jupyter-strip-ansi-escapes 'latex)
   (should (equal (buffer-string)
                  "\
#+BEGIN_SRC jupyter-foo
#+END_SRC

#+RESULTS:
: ABCDEF\n"))
   (erase-buffer)
   (insert "\
#+BEGIN_SRC jupyter-foo
#+END_SRC

#+RESULTS:
")
   (org-babel-jupyter-strip-ansi-escapes 'latex)
   (should (equal (buffer-string)
                  "\
#+BEGIN_SRC jupyter-foo
#+END_SRC

#+RESULTS:
"))
   (erase-buffer)
   (insert "\
#+BEGIN_SRC jupyter-foo
#+END_SRC
")
   (org-babel-jupyter-strip-ansi-escapes 'latex)
   (should (equal (buffer-string)
                  "\
#+BEGIN_SRC jupyter-foo
#+END_SRC
"))))

(ert-deftest org-babel-jupyter-:results-header-arg ()
  :tags '(org)
  (ert-info ("scalar suppresses table output")
    (jupyter-org-test-src-block
     "[1, 2, 3]"
     "| 1 | 2 | 3 |\n"
     ;; Ensure that no interference happens from removing the file header
     ;; argument.
     :file "foo"
     ;; FIXME: How to handle header arguments consistently in the async vs sync
     ;; case.
     :async "yes")
    (jupyter-org-test-src-block
     "[1, 2, 3]"
     ": [1, 2, 3]\n"
     :results "scalar")))

(ert-deftest org-babel-jupyter-:dir-header-arg ()
  :tags '(org)
  (let ((convert-path
         (lambda (s)
           ;; Convert forward slashes to backslashes on Windows
           (if (memq system-type '(windows-nt cygwin ms-dos))
               (replace-regexp-in-string "/" "\\\\" s)
             s))))
    (ert-info ("Python")
      (jupyter-org-test-src-block
       "\
import os
os.path.abspath(os.getcwd())"
       (concat ": " (funcall convert-path (expand-file-name "~")) "\n")
       :dir "~")
      (ert-info ("Directory restored")
        (jupyter-org-test-src-block
         "\
import os
os.path.abspath(os.getcwd())"
         (concat ": "
                 (funcall convert-path
                          (expand-file-name
                           (directory-file-name default-directory))) "\n"))))))

(ert-deftest jupyter-org--find-mime-types ()
  :tags '(org mime)
  (ert-info ("Mimetype priority overwrite")
    (should (equal (jupyter-org--find-mime-types "text")
                   '(:text/plain)))
    (should (equal (jupyter-org--find-mime-types "image")
                   '(:image/png)))
    (should (equal (jupyter-org--find-mime-types "plain html")
                   '(:text/plain :text/html)))
    (should (equal (jupyter-org--find-mime-types "org jpeg")
                   '(:text/org :image/jpeg)))
    (should (equal (jupyter-org--find-mime-types "plain foo html bar")
                   '(:text/plain :text/html)))
    (should (equal (jupyter-org--find-mime-types "foo bar")
                   '()))))

(ert-deftest org-babel-jupyter-:display-header-arg ()
  :tags '(org)
  (jupyter-org-test-src-block
   "\
from IPython.display import publish_display_data
publish_display_data({'text/plain': \"foo\", 'text/latex': \"$\\alpha$\"});"
   ": foo\n"
   :display "plain"))

(ert-deftest org-babel-jupyter-babel-call ()
  :tags '(org babel)
  (jupyter-org-test
   (insert (format "\
#+NAME: foo
#+begin_src jupyter-python :async yes :session %s
1 + 1
#+end_src

" jupyter-org-test-session))
   (insert "
#+CALL: foo()")
   (org-ctrl-c-ctrl-c)
   (beginning-of-line)
   (jupyter-wait-until-idle (jupyter-org-request-at-point))
   (goto-char (org-babel-where-is-src-block-result))
   (forward-line)
   (should (looking-at-p ": 2\n"))))

(ert-deftest org-babel-jupyter-inline-blocks ()
  :tags '(org)
  (ert-info ("Treat inline and non-inline results similarly")
    ;; #204
    (let ((src (format "\
#+NAME: hello_jupyter
#+BEGIN_SRC jupyter-python :results value :display plain :session %s
\"hello\"
#+END_SRC

" jupyter-org-test-session)))
      (jupyter-org-test
       (insert src)
       (insert "call_hello_jupyter()")
       (let ((pos (point)))
         (beginning-of-line)
         (org-ctrl-c-ctrl-c)
         (goto-char pos)
         (should (looking-at-p " {{{results(=hello=)}}}"))))
      (jupyter-org-test
       (save-excursion (insert src))
       (org-ctrl-c-ctrl-c)
       (goto-char (org-babel-where-is-src-block-result))
       (forward-line)
       (should (looking-at-p ": hello"))))))


;; Local Variables:
;; byte-compile-warnings: (unresolved obsolete lexical)
;; eval: (and (functionp 'aggressive-indent-mode) (aggressive-indent-mode -1))
;; End:
;;; jupyter-test.el ends here
