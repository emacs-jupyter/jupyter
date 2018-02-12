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

(defclass jupyter-echo-client (jupyter-kernel-client)
  ((messages))
  :documentation "A client that echo's any messages sent back to
the channel the message was sent on. No communication is actually
done with a kernel. Every sent message on a channel is just
placed back into the channel's recv-queue. This is mainly for
testing the callback functionality of a
`jupyter-kernel-client'.")

(defun jupyter-test-message (req type content)
  ;; `jupyter-queue-message' expects a cons cell of the form (idents . msg)
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
  (let ((req (make-jupyter-request :-id (jupyter-new-uuid))))
    (if (string-match "request" type)
        (setq type (replace-match "reply" nil nil type))
      (error "Not a request message type (%s)" type))
    ;; Message flow
    ;; - status: busy
    ;; - reply message
    ;; - status: idle
    (jupyter-queue-message
     (oref client iopub-channel)
     (jupyter-test-message
      req "status" (list :execution_state "busy")))
    (jupyter-queue-message
     channel (jupyter-test-message req type message))
    (jupyter-queue-message
     (oref client iopub-channel)
     (jupyter-test-message
      req "status" (list :execution_state "idle")))
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

(defmacro with-jupyter-echo-client (client &rest body)
  (declare (indent 1))
  `(let ((,client (jupyter-echo-client)))
     ,@body))

(ert-deftest jupyter-echo-client ()
  (with-jupyter-echo-client client
    (ert-info ("Mock echo client echo's messages back to channel.")
      (let* ((msg (jupyter-message-execute-request :code "foo"))
             (req (jupyter-send client (oref client shell-channel)
                                "execute_request" msg)))
        (sleep-for 0.5)
        (setq msgs (nreverse (ring-elements (oref client messages))))
        (should (= (length msgs) 3))
        (should (equal (jupyter-message-type (first msgs))
                       "status"))
        (should (equal (jupyter-message-parent-id (first msgs))
                       (jupyter-request-id req)))
        (should (equal (jupyter-message-get (first msgs) :execution_state)
                       "busy"))
        (should (equal (jupyter-message-type (second msgs))
                       "execute_reply"))
        (should (equal (jupyter-message-parent-id (second msgs))
                       (jupyter-request-id req)))
        (should (equal (jupyter-message-content (second msgs))
                       msg))
        (should (equal (jupyter-message-type (third msgs))
                       "status"))
        (should (equal (jupyter-message-parent-id (third msgs))
                       (jupyter-request-id req)))
        (should (equal (jupyter-message-get (third msgs) :execution_state)
                       "idle"))))))

(ert-deftest jupyter-callbacks ()
  (with-jupyter-echo-client client
    (ert-info ("Request callbacks")
      (ert-info ("Blocking callbacks")
        (let ((req (jupyter-execute-request client :code "foo")))
          (should (jupyter-wait-until-idle req))
          (should (jupyter-request-idle-received-p req))
          ;; Can't add callbacks after an idle message has been received
          (should-error (jupyter-add-callback req :status #'identity))))
      (ert-info ("Callback runs for the right message")
        (let ((req1 (jupyter-execute-request client :code "foo"))
              (req2 (jupyter-kernel-info-request client))
              ran-callbacks)
          ;; callback for all message types received from a request
          (jupyter-add-callback req1
            t (lambda (msg)
                (push 1 ran-callbacks)
                (should (member (jupyter-message-type msg)
                                '("execute_reply" "status")))
                (should (equal (jupyter-message-parent-id msg)
                               (jupyter-request-id req1)))))
          (jupyter-add-callback req2
            t (lambda (msg)
                (push 2 ran-callbacks)
                (should (member (jupyter-message-type msg)
                                '("kernel_info_reply" "status")))
                (should (equal (jupyter-message-parent-id msg)
                               (jupyter-request-id req2)))))
          (should (jupyter-wait-until-idle req2))
          (setq ran-callbacks (nreverse ran-callbacks))
          (should (equal ran-callbacks '(1 1 1 2 2 2)))))
      (ert-info ("Multiple callbacks for a single message type")
        (let* ((ran-callbacks nil)
               (req (jupyter-execute-request client :code "foo")))
          (jupyter-add-callback req
            :execute-reply (lambda (_msg) (push 1 ran-callbacks)))
          (jupyter-add-callback req
            :execute-reply (lambda (_msg) (push 2 ran-callbacks)))
          (jupyter-wait-until-idle req)
          (setq ran-callbacks (nreverse ran-callbacks))
          (should (equal ran-callbacks '(1 2))))))))

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
           (header (jupyter--message-header session "stdin_reply")))
      ;; TODO: Check fields
      (should (plist-get header :msg_id))
      (should (plist-get header :date))
      (should (string= (plist-get header :msg_type) "stdin_reply"))
      (should (string= (plist-get header :version) jupyter-protocol-version))
      (should (string= (plist-get header :username) user-login-name))
      (should (string= (plist-get header :session) (jupyter-session-id session))))
    ;; TODO: Handle other kinds of encoding
    (ert-info ("Encoding/decoding objects")
      (let ((json-object-type 'plist)
            (obj nil))
        (should-not (multibyte-string-p (jupyter--encode "foîji")))
        ;; TODO: Only decodes json plists, what to do instead?
        (should-error (jupyter--decode (jupyter--encode "foîji")))
        (setq obj '(:msg_id 12342 :msg_type "stdin_reply" :session "foîji"))
        (should (json-plist-p obj))
        (should-not (multibyte-string-p (jupyter--encode obj)))
        (should (equal (jupyter--decode (jupyter--encode obj))
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
      (should (equal (zmq-socket-get (oref channel socket) zmq-ROUTING-ID)
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
                              (lambda (_ctx)
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
                  (should (jupyter-channel-alive-p channel))
                  (should (oref channel paused))
                  (jupyter-hb-unpause channel)
                  (sleep-for 2)
                  (should-not (oref channel paused))
                  (should (jupyter-hb-beating-p channel)))
                (ert-info ("Pausing the channel")
                  (jupyter-hb-pause channel)
                  (oset channel beating 'beating)
                  (should (oref channel paused))
                  (sleep-for 1)
                  ;; Channel shouldn't update when paused
                  (should (eq (oref channel beating) 'beating)))
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
                  (let ((p (oref channel timer)))
                    (jupyter-stop-channel channel)
                    (should-not (memq p timer-list))
                    (should-not (oref channel timer)))))
            (when (process-live-p proc)
              (delete-process proc)
              (kill-buffer (process-buffer proc)))
            (when (jupyter-channel-alive-p channel)
              (jupyter-stop-channel channel))))))))

(ert-deftest jupyter-client ()
  (let* ((socks (cl-loop repeat 4
                         collect (zmq-socket (zmq-current-context) zmq-REQ)))
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

(ert-deftest jupyter-channel-subprocess ()
  (ert-info ("Queuing messages preserves sort order")
    (cl-flet ((queue-msg
               (messages priorities msg)
               (jupyter--ioloop-queue-message messages priorities msg)
               messages))
      (let* ((priorities '((:iopub . 1)
                           (:shell . 2)))
             (messages nil)
             (time '(0 0 100 0))
             (out-of-order
              `((:iopub . (nil . (:header (:date ,time))))
                (:shell . (nil . (:header (:date ,time))))
                (:shell . (nil . (:header (:date ,(time-add time '(0 0 100))))))
                (:iopub . (nil . (:header (:date ,(time-subtract
                                                   time '(0 0 100)))))))))
        (setq msg (nth 0 out-of-order))
        (setq messages (queue-msg messages priorities msg))
        (should
         (equal messages `((:iopub . (nil . (:header (:date ,time)))))))

        (setq msg (nth 1 out-of-order))
        (setq messages (queue-msg messages priorities msg))
        (should (equal  messages `((:shell . (nil . (:header (:date ,time))))
                                   (:iopub . (nil . (:header (:date ,time)))))))

        (setq msg (nth 2 out-of-order))
        (setq messages (queue-msg messages priorities msg))
        (should
         (equal
          messages `((:shell . (nil . (:header (:date ,time))))
                     (:iopub . (nil . (:header (:date ,time))))
                     (:shell . (nil . (:header (:date ,(time-add time '(0 0 100)))))))))

        (setq msg (nth 3 out-of-order))
        (setq messages (queue-msg messages priorities msg))
        (should
         (equal
          messages `((:iopub . (nil . (:header (:date ,(time-subtract time '(0 0 100))))))
                     (:shell . (nil . (:header (:date ,time))))
                     (:iopub . (nil . (:header (:date ,time))))
                     (:shell . (nil . (:header (:date ,(time-add time '(0 0 100)))))))))))))

(ert-deftest jupyter-message-types ()
  (let* ((manager (jupyter-kernel-manager "python"))
         (client (jupyter-make-client manager 'jupyter-kernel-client)))
    (jupyter-start-channels client)
    ;; Let the channels start
    (sleep-for 1)
    (unwind-protect
        (progn
          (ert-info ("Kernel info")
            (let ((res (jupyter-wait-until-received :kernel-info-reply
                         (jupyter-kernel-info-request client))))
              (should-not (null res))
              (should (json-plist-p res))
              (should (equal (jupyter-message-type res) "kernel_info_reply"))))
          (ert-info ("Comm info")
            (let ((res (jupyter-wait-until-received :comm-info-reply
                         (jupyter-comm-info-request client))))
              (should-not (null res))
              (should (json-plist-p res))
              (should (equal (jupyter-message-type res) "comm_info_reply"))))
          (ert-info ("Execute")
            (let ((res (jupyter-wait-until-received :execute-reply
                         (jupyter-execute-request client :code "y = 1 + 2"))))
              (should-not (null res))
              (should (json-plist-p res))
              (should (equal (jupyter-message-type res) "execute_reply"))))
          (ert-info ("Input")
            (cl-letf (((symbol-function 'read-from-minibuffer)
                       (lambda (_prompt &rest _args) "foo")))
              (let ((res (jupyter-wait-until-received :execute-result
                           (jupyter-execute-request client :code "input('')"))))
                (should-not (null res))
                (should (json-plist-p res))
                (should (equal (jupyter-message-type res) "execute_result"))
                (should (equal (jupyter-message-data data :text/plain) "'foo'")))))
          (ert-info ("Inspect")
            (let ((res (jupyter-wait-until-received :inspect-reply
                         (jupyter-inspect-request client
                           :code "list((1, 2, 3))"
                           :pos 2
                           :detail 0))))
              (should-not (null res))
              (should (json-plist-p res))
              (should (equal (jupyter-message-type res) "inspect_reply"))))
          (ert-info ("Complete")
            (let ((res (jupyter-wait-until-received :complete-reply
                         (jupyter-complete-request client
                           :code "foo = lis"
                           :pos 8))))
              (should-not (null res))
              (should (json-plist-p res))
              (should (equal (jupyter-message-type res) "complete_reply"))))
          (ert-info ("History")
            (let ((res (jupyter-wait-until-received :history-reply
                         (jupyter-history-request client
                           :hist-access-type "tail" :n 2))))
              (should-not (null res))
              (should (json-plist-p res))
              (should (equal (jupyter-message-type res) "history_reply"))))
          (ert-info ("Is Complete")
            (let ((res (jupyter-wait-until-received :is-complete-reply
                         (jupyter-is-complete-request client
                           :code "for i in range(5):"))))
              (should-not (null res))
              (should (json-plist-p res))
              (should (equal (jupyter-message-type res) "is_complete_reply"))))
          (ert-info ("Shutdown")
            (let ((res (jupyter-wait-until-received :shutdown-reply
                         (jupyter-shutdown-request client))))
              (should-not (null res))
              (should (json-plist-p res))
              (should (equal (jupyter-message-type res) "shutdown_reply")))))
      (jupyter-stop-channels client)
      (jupyter-stop-kernel client))))

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
          (when repl-buffer (kill-buffer repl-buffer)))))))

;;; jupyter-tests.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
