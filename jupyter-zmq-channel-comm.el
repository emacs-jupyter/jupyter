;;; jupyter-zmq-channel-comm.el --- Communication layer using ZMQ sockets -*- lexical-binding: t -*-

;; Copyright (C) 2019 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 27 Jun 2019
;; Version: 0.8.1

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

;; A communication layer using `jupyter-zmq-channel' objects for communicating
;; with a kernel. This communication layer is mainly meant for speed comparison
;; with the `jupyter-channel-ioloop-comm' layer. It implements communication in
;; the current Emacs instance and comparing it with the
;; `jupyter-channel-ioloop-comm' shows how much of a slow down there is when
;; all the processing of messages happens in the current Emacs instance.
;;
;; Running the test suit using `jupyter-zmq-channel-comm' vs
;; `jupyter-channel-ioloop-comm' shows, very roughly, around a 2x speed up
;; using `jupyter-channel-ioloop-comm'.

;;; Code:

(require 'jupyter-zmq-channel)
(require 'jupyter-comm-layer)
(eval-when-compile (require 'subr-x))

(defclass jupyter-zmq-channel-comm (jupyter-comm-layer
                                    jupyter-hb-comm
                                    jupyter-comm-autostop)
  ((session :type jupyter-session)
   (channels :type (list-of (or keyword jupyter-zmq-channel)) :initform nil)
   (thread)))

(cl-defmethod initialize-instance ((_comm jupyter-zmq-channel-comm) &optional _slots)
  (unless (functionp 'make-thread)
    (error "Need threading support"))
  (cl-call-next-method))

(cl-defmethod jupyter-comm-id ((comm jupyter-zmq-channel-comm))
  (format "session=%s" (truncate-string-to-width
                        (jupyter-session-id (oref comm session))
                        9 nil nil "…")))

(defun jupyter-zmq-channel-comm--check (comm)
  (condition-case err
      (cl-loop
       for channel-type in '(:iopub :shell :stdin)
       for channel = (plist-get (oref comm channels) channel-type)
       for msg = (and (jupyter-channel-alive-p channel)
                      (with-slots (session socket) channel
                        (condition-case nil
                            (jupyter-recv session socket zmq-DONTWAIT)
                          ((zmq-EINTR zmq-EAGAIN) nil))))
       when msg do (jupyter-event-handler
                    comm (cl-list* 'message channel-type msg)))
    (error
     (thread-signal (car (all-threads)) (car err)
                    (cons 'jupyter-zmq-channel-comm--check (cdr err)))
     (signal (car err) (cdr err)))))

(cl-defmethod jupyter-comm-start ((comm jupyter-zmq-channel-comm))
  (jupyter-start-channel (oref comm hb))
  (cl-loop
   for channel in '(:iopub :shell :stdin)
   do (jupyter-start-channel (plist-get (oref comm channels) channel)))
  (oset comm thread
        (make-thread
         (lambda ()
           (while (jupyter-comm-alive-p comm)
             (jupyter-zmq-channel-comm--check comm)
             (thread-yield)
             (thread-yield))))))

(cl-defmethod jupyter-comm-stop ((comm jupyter-zmq-channel-comm))
  (when (and (slot-boundp comm 'thread)
             (thread-alive-p (oref comm thread)))
    (thread-signal (oref comm thread) 'quit nil)
    (slot-makeunbound comm 'thread))
  (jupyter-stop-channel (oref comm hb))
  (cl-loop
   for channel in '(:iopub :shell :stdin)
   do (jupyter-stop-channel (plist-get (oref comm channels) channel))))

(cl-defmethod jupyter-comm-alive-p ((comm jupyter-zmq-channel-comm))
  (jupyter-channels-running-p comm))

(cl-defmethod jupyter-channel-alive-p ((comm jupyter-zmq-channel-comm) channel)
  (if (eq channel :hb)
      (and (slot-boundp comm 'hb)
           (jupyter-channel-alive-p (oref comm hb)))
    (let ((c (plist-get (oref comm channels) channel)))
      (and c (jupyter-channel-alive-p c)))))

(cl-defmethod jupyter-channels-running-p ((comm jupyter-zmq-channel-comm))
  (or (cl-loop
       for channel in '(:shell :iopub :stdin)
       thereis (jupyter-channel-alive-p comm channel))
      (and (slot-boundp comm 'hb)
           (jupyter-channel-alive-p (oref comm hb)))))

;;;; Channel start/stop methods

(cl-defmethod jupyter-stop-channel ((comm jupyter-zmq-channel-comm) channel)
  (when (jupyter-channel-alive-p comm channel)
    (if (eq channel :hb)
        (jupyter-stop-channel (oref comm hb))
      (jupyter-stop-channel (plist-get (oref comm channels) channel)))))

(cl-defmethod jupyter-start-channel ((comm jupyter-zmq-channel-comm) channel)
  (unless (jupyter-channel-alive-p comm channel)
    (if (eq channel :hb) (jupyter-start-channel (oref comm hb))
      (jupyter-start-channel (plist-get (oref comm channels) channel)))))

(cl-defmethod jupyter-initialize-connection ((comm jupyter-zmq-channel-comm)
                                             (session jupyter-session))
  (cl-call-next-method)
  (let ((endpoints (jupyter-session-endpoints session)))
    (oset comm session (copy-sequence session))
    (oset comm hb (make-instance
                   'jupyter-hb-channel
                   :session (oref comm session)
                   :endpoint (plist-get endpoints :hb)))
    (oset comm channels
          (cl-loop
           for channel in '(:stdin :shell :iopub)
           collect channel and
           collect (jupyter-zmq-channel
                    :type channel
                    :session (oref comm session)
                    :endpoint (plist-get endpoints channel))))))

(cl-defmethod jupyter-send ((comm jupyter-zmq-channel-comm)
                            _ channel-type msg-type msg msg-id)
  (let ((channel (plist-get (oref comm channels) channel-type)))
    ;; Run the event handler on the next command loop since the expectation is
    ;; the client is that sending is asynchronous. There may be some code that
    ;; makes assumptions based on this.
    (run-at-time
     0 nil (lambda (id)
             (jupyter-event-handler comm (list 'sent channel-type id)))
     (jupyter-send channel msg-type msg msg-id))))

(provide 'jupyter-zmq-channel-comm)

;;; jupyter-zmq-channel-comm.el ends here
