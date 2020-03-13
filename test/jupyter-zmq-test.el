;;; jupyter-zmq-test.el --- Jupyter tests that require ZMQ -*- lexical-binding: t -*-

;; Copyright (C) 2020 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 13 Mar 2020

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
(require 'jupyter-ioloop)
(require 'jupyter-zmq-channel-ioloop)
(require 'jupyter-zmq-channel-comm)

(defvar jupyter-test-zmq-sockets (make-hash-table :weakness 'key))

(advice-add 'zmq-socket
            :around (lambda (&rest args)
                      (let ((sock (apply args)))
                        (prog1 sock
                          (puthash sock t jupyter-test-zmq-sockets)))))

(add-hook
 'kill-emacs-hook
 (lambda ()
   ;; Do this cleanup to avoid core dumps on Travis due to epoll reconnect
   ;; attempts.
   (cl-loop
    for sock being the hash-keys of jupyter-test-zmq-sockets do
    (ignore-errors
      (zmq-set-option sock zmq-LINGER 0)
      (zmq-close sock)))
   (ignore-errors (zmq-context-terminate (zmq-current-context)))))

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
  :tags '(channels zmq)
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

;;; Kernel

(ert-deftest jupyter-kernel-lifetime ()
  :tags '(kernel zmq)
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

;;; Client
;; FIXME: These two tests should be written so that they don't depend on ZMQ
;; and then moved back into `jupyter-test.el`.

;; TODO: Different values of the session argument
;; TODO: Update for new `jupyter-channel-ioloop-comm'
(ert-deftest jupyter-comm-initialize ()
  :tags '(client init zmq)
  (skip-unless nil)
  ;; The default comm is a jupyter-channel-ioloop-comm
  (let ((conn-info (jupyter-test-conn-info-plist))
        (client (jupyter-kernel-client)))
    (oset client kcomm (jupyter-zmq-channel-comm))
    (jupyter-comm-initialize client conn-info)
    ;; kcomm by default is a `jupyter-channel-ioloop-comm'
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

(ert-deftest jupyter-client-channels ()
  :tags '(client channels zmq)
  (ert-info ("Starting/stopping channels")
    (let ((conn-info (jupyter-test-conn-info-plist))
          (client (jupyter-kernel-client)))
      (oset client kcomm (jupyter-zmq-channel-comm))
      (jupyter-comm-initialize client conn-info)
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
       do (should-not alive-p)))))

;;; IOloop

(defun jupyter-test-ioloop-eval-event (ioloop event)
  (eval
   `(progn
      ,@(oref ioloop setup)
      ,(jupyter-ioloop--event-dispatcher ioloop event))))

(defmacro jupyter-test-channel-ioloop (ioloop &rest body)
  (declare (indent 1))
  (let ((var (car ioloop))
        (val (cadr ioloop)))
    (with-temp-buffer
      `(let* ((,var ,val)
              (standard-output (current-buffer))
              (jupyter-channel-ioloop-channels nil)
              (jupyter-channel-ioloop-session nil)
              ;; Needed so that `jupyter-ioloop-environment-p' passes
              (jupyter-ioloop-stdin t)
              (jupyter-ioloop-poller (zmq-poller)))
         (unwind-protect
             (progn ,@body)
           (zmq-poller-destroy jupyter-ioloop-poller)
           (jupyter-ioloop-stop ,var))))))

(ert-deftest jupyter-ioloop-lifetime ()
  :tags '(ioloop zmq)
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
  :tags '(ioloop zmq)
  (let ((ioloop (jupyter-ioloop)))
    (should-not (jupyter-ioloop-last-event ioloop))
    (jupyter-ioloop-start ioloop :test)
    (should (equal (jupyter-ioloop-last-event ioloop) '(start)))
    (jupyter-ioloop-stop ioloop)))

(ert-deftest jupyter-ioloop-callbacks ()
  :tags '(ioloop zmq)
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
  :tags '(ioloop zmq)
  (let ((ioloop (jupyter-ioloop)))
    (setq jupyter-ioloop-test-handler-called nil)
    (jupyter-ioloop-add-setup ioloop
      (zmq-prin1 (list 'test "message")))
    (jupyter-ioloop-start ioloop :test)
    (jupyter-ioloop-stop ioloop)
    (should jupyter-ioloop-test-handler-called)))

(ert-deftest jupyter-ioloop-teardown ()
  :tags '(ioloop zmq)
  (let ((ioloop (jupyter-ioloop)))
    (setq jupyter-ioloop-test-handler-called nil)
    (jupyter-ioloop-add-teardown ioloop
      (zmq-prin1 (list 'test "message")))
    (jupyter-ioloop-start ioloop :test)
    (jupyter-ioloop-stop ioloop)
    (should jupyter-ioloop-test-handler-called)))

(ert-deftest jupyter-ioloop-add-event ()
  :tags '(ioloop zmq)
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
  :tags '(ioloop zmq)
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
  :tags '(ioloop zmq)
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
  :tags '(ioloop zmq)
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
  :tags '(ioloop queue zmq)
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
