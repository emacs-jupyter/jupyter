;;; jupyter-channel-ioloop-comm.el --- Communication layer using jupyter-channel-ioloop -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 27 Jun 2019

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

;; Implements the `jupyter-comm-layer' interface on-top of a `jupyter-ioloop'
;; subclass that implements the `jupyter-channel' interface through events sent
;; to the ioloop.  The `jupyter-ioloop' must implement a start-channel,
;; stop-channel, and a send event.  For the start-channel and stop-channel
;; events the `jupyter-ioloop' must send back a list like
;;
;;     (start-channel :hb) or (stop-channel :shell)
;;
;; for confirmation that the corresponding channel was indeed started or
;; stopped.  The start-channel event should accept two arguments, (CHANNEL
;; ENDPOINT), used to start CHANNEL.  The stop-channel event should accept a
;; single argument, CHANNEL, and stop the channel in the `jupyter-ioloop'
;; environment.
;;
;; Initializing the connection
;;
;; The `jupyter-comm-initialize' method should be called before calling
;; `jupyter-comm-start' and should be passed a `jupyter-session' object used to
;; initialize the `jupyter-channel-ioloop' object.

;;; Code:

(require 'jupyter-base)
(require 'jupyter-ioloop-comm)
(require 'jupyter-channel-ioloop)

(cl-defstruct jupyter-proxy-channel endpoint alive-p)

(defclass jupyter-channel-ioloop-comm (jupyter-ioloop-comm
                                       jupyter-hb-comm
                                       jupyter-comm-autostop)
  ((ioloop-class :type class :initarg :ioloop-class)
   (session :type jupyter-session)
   (channels :type (list-of (or keyword jupyter-proxy-channel)) :initform nil)))

(cl-defmethod initialize-instance ((comm jupyter-channel-ioloop-comm) &optional _slots)
  (cl-call-next-method)
  (unless (slot-boundp comm 'ioloop-class)
    (oset comm ioloop-class 'jupyter-channel-ioloop))
  (with-slots (ioloop-class) comm
    (jupyter-error-if-not-client-class-p ioloop-class 'jupyter-channel-ioloop)
    (oset comm ioloop (make-instance ioloop-class))))

(cl-defmethod jupyter-comm-id ((comm jupyter-channel-ioloop-comm))
  (format "session=%s" (truncate-string-to-width
                        (jupyter-session-id (oref comm session))
                        9 nil nil "…")))

(cl-defmethod jupyter-comm-initialize ((comm jupyter-channel-ioloop-comm)
                                             (session jupyter-session))
  (cl-call-next-method)
  (let ((endpoints (jupyter-session-endpoints session)))
    (oset comm session (copy-sequence session))
    (oset comm hb (make-instance
                   'jupyter-hb-channel
                   :session (oref comm session)
                   :endpoint (plist-get endpoints :hb)))
    (oset comm channels (cl-loop
                         for channel in '(:stdin :shell :iopub)
                         collect channel and
                         collect (make-jupyter-proxy-channel
                                  :endpoint (plist-get endpoints channel)
                                  :alive-p nil)))))

(cl-defmethod jupyter-comm-start ((comm jupyter-channel-ioloop-comm))
  (with-slots (ioloop session) comm
    (unless (jupyter-ioloop-alive-p ioloop)
      (jupyter-channel-ioloop-set-session ioloop (oref comm session))
      (jupyter-ioloop-start
       ioloop (lambda (event)
                (pcase (car event)
                  ;; These channel events are from `jupyter-channel-ioloop'
                  ('start-channel
                   (setf (jupyter-proxy-channel-alive-p
                          (plist-get (oref comm channels) (cadr event)))
                         t))
                  ('stop-channel
                   (setf (jupyter-proxy-channel-alive-p
                          (plist-get (oref comm channels) (cadr event)))
                         nil))
                  (_ (jupyter-event-handler comm event))))))
    (cl-loop
     for channel in '(:hb :shell :iopub :stdin)
     do (jupyter-start-channel comm channel))))

(cl-defmethod jupyter-comm-stop ((comm jupyter-channel-ioloop-comm))
  (cl-loop
   for channel in '(:hb :shell :iopub :stdin)
   do (jupyter-stop-channel comm channel))
  (cl-call-next-method))

;;;; Channel querying methods

(cl-defmethod jupyter-comm-alive-p ((comm jupyter-channel-ioloop-comm))
  (cl-loop
   for channel in '(:shell :iopub :stdin :hb)
   thereis (jupyter-channel-alive-p comm channel)))

(cl-defmethod jupyter-channel-alive-p ((comm jupyter-channel-ioloop-comm) channel)
  (if (eq channel :hb)
      (and (slot-boundp comm 'hb)
           (jupyter-channel-alive-p (oref comm hb)))
    (with-slots (ioloop) comm
      (and ioloop (jupyter-ioloop-alive-p ioloop)
           (jupyter-proxy-channel-alive-p
            (plist-get (oref comm channels) channel))))))

;;;; Channel start/stop methods

(cl-defmethod jupyter-stop-channel ((comm jupyter-channel-ioloop-comm) channel)
  (when (jupyter-channel-alive-p comm channel)
    (if (eq channel :hb) (jupyter-stop-channel (oref comm hb))
      (with-slots (ioloop) comm
        (jupyter-send ioloop 'stop-channel channel)
        ;; Verify that the channel stops
        (jupyter-with-timeout
            (nil jupyter-default-timeout
                 (error "Channel not stopped in ioloop subprocess (%s)" channel))
          (not (jupyter-channel-alive-p comm channel)))))))

(cl-defmethod jupyter-start-channel ((comm jupyter-channel-ioloop-comm) channel)
  (unless (jupyter-channel-alive-p comm channel)
    (if (eq channel :hb) (jupyter-start-channel (oref comm hb))
      (let ((endpoint (jupyter-proxy-channel-endpoint
                       (plist-get (oref comm channels) channel))))
        (with-slots (ioloop) comm
          (jupyter-send ioloop 'start-channel channel endpoint)
          ;; Verify that the channel starts
          (jupyter-with-timeout
              (nil jupyter-default-timeout
                   (error "Channel not started in ioloop subprocess (%s)" channel))
            (jupyter-channel-alive-p comm channel)))))))

(provide 'jupyter-channel-ioloop-comm)

;;; jupyter-channel-ioloop-comm.el ends here
