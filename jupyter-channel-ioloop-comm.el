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
(require 'jupyter-connection)

;;; New implementation

(cl-defstruct jupyter--proxy-channel endpoint alive-p)

(defun jupyter--make-channel-group (session)
  (let ((endpoints (jupyter-session-endpoints session)))
    (append
     (list 'channel-group t
           :hb (make-instance
                'jupyter-hb-channel
                :session session
                :endpoint (plist-get endpoints :hb)))
     (cl-loop
      for channel in '(:control :shell :iopub :stdin)
      collect channel
      collect (make-jupyter--proxy-channel
               :endpoint (plist-get endpoints channel)
               :alive-p nil)))))

(defun jupyter--channel-alive-p (ioloop chgroup channel)
  (if (eq channel :hb)
      (let ((hb (plist-get chgroup channel)))
        (and hb (jupyter-channel-alive-p hb)))
    (and ioloop (jupyter-ioloop-alive-p ioloop)
         (jupyter--proxy-channel-alive-p
          (plist-get chgroup channel)))))

(defun jupyter--start-channel (ioloop chgroup channel)
  (unless (jupyter--channel-alive-p ioloop chgroup channel)
    (if (eq channel :hb) (jupyter-start-channel (plist-get chgroup channel))
      (let ((endpoint (jupyter--proxy-channel-endpoint
                       (plist-get chgroup channel))))
        (jupyter-send ioloop 'start-channel channel endpoint)
        ;; Verify that the channel starts
        (jupyter-with-timeout
            (nil jupyter-default-timeout
                 (error "Channel not started in ioloop subprocess (%s)" channel))
          (jupyter--channel-alive-p ioloop chgroup channel))))))

(defun jupyter--stop-channel (ioloop chgroup channel)
  (when (jupyter--channel-alive-p ioloop chgroup channel)
    (if (eq channel :hb) (jupyter-stop-channel (plist-get chgroup channel))
      (jupyter-send ioloop 'stop-channel channel)
      ;; Verify that the channel stops
      (jupyter-with-timeout
          (nil jupyter-default-timeout
               (error "Channel not stopped in ioloop subprocess (%s)" channel))
        (not (jupyter--channel-alive-p ioloop chgroup channel))))))

(defun make-jupyter-async-connection (session handler)
  "Send kernel messages asynchronously."
  (require 'jupyter-zmq-channel-ioloop)
  (let ((channels '(:hb :shell :iopub :stdin))
        (chgroup (jupyter--make-channel-group session))
        (ioloop (make-instance 'jupyter-zmq-channel-ioloop)))
    (jupyter-channel-ioloop-set-session ioloop session)
    (make-jupyter-connection
     :hb (plist-get chgroup :hb)
     :id (lambda ()
           (format "session=%s" (truncate-string-to-width
                                 (jupyter-session-id session)
                                 9 nil nil "…")))
     :start (lambda (&optional channel)
              (unless (jupyter-ioloop-alive-p ioloop)
                (jupyter-ioloop-start
                 ioloop (lambda (event)
                          (pcase (car event)
                            ;; These channel events are from
                            ;; `jupyter-channel-ioloop'
                            ('start-channel
                             (setf (jupyter--proxy-channel-alive-p
                                    (plist-get chgroup (cadr event)))
                                   t))
                            ('stop-channel
                             (setf (jupyter--proxy-channel-alive-p
                                    (plist-get chgroup (cadr event)))
                                   nil))
                            (_
                             (funcall handler event))))))
              (if channel (jupyter--start-channel ioloop chgroup channel)
                (cl-loop
                 for channel in channels
                 do (jupyter--start-channel ioloop chgroup channel))))
     :stop (lambda (&optional channel)
             (if channel (jupyter--stop-channel ioloop chgroup channel)
               (cl-loop
                for channel in channels
                do (jupyter--stop-channel ioloop chgroup channel))
               (jupyter-ioloop-stop ioloop))
             (jupyter-ioloop-stop ioloop))
     :send (lambda (&rest event)
             (apply #'jupyter-send ioloop event))
     :alive-p (lambda (&optional channel)
                (if channel (jupyter--channel-alive-p ioloop chgroup channel)
                  (cl-loop
                   for channel in channels
                   thereis (jupyter--channel-alive-p ioloop chgroup channel)))))))

;;; Old implementation

(cl-defstruct jupyter-proxy-channel endpoint alive-p)

(defclass jupyter-channel-ioloop-comm (jupyter-ioloop-comm
                                       jupyter-comm-autostop)
  ((conn :type jupyter-connection)
   (hb :type jupyter-channel)
   (session :type jupyter-session)))

(cl-defmethod jupyter-comm-id ((comm jupyter-channel-ioloop-comm))
  (jupyter-conn-id (oref comm conn)))

(cl-defmethod jupyter-comm-initialize ((comm jupyter-channel-ioloop-comm)
                                       (session jupyter-session))
  (oset comm session session)
  (let ((conn (make-jupyter-async-connection
               session (lambda (event) (jupyter-event-handler comm event)))))
    (oset comm conn conn)
    (oset comm hb (jupyter-connection-hb conn))))

(cl-defmethod jupyter-comm-start ((comm jupyter-channel-ioloop-comm))
  (jupyter-start (oref comm conn)))

(cl-defmethod jupyter-comm-stop ((comm jupyter-channel-ioloop-comm))
  (jupyter-stop (oref comm conn)))

(cl-defmethod jupyter-send ((comm jupyter-channel-ioloop-comm) &rest event)
  (apply #'jupyter--send (oref comm conn) event))

;;;; Channel querying methods

(cl-defmethod jupyter-comm-alive-p ((comm jupyter-channel-ioloop-comm))
  (jupyter-alive-p (oref comm conn)))

(cl-defmethod jupyter-channel-alive-p ((comm jupyter-channel-ioloop-comm) channel)
  (jupyter-alive-p (oref comm conn) channel))

;;;; Channel start/stop methods

(cl-defmethod jupyter-stop-channel ((comm jupyter-channel-ioloop-comm) channel)
  (jupyter-stop (oref comm conn) channel))

(cl-defmethod jupyter-start-channel ((comm jupyter-channel-ioloop-comm) channel)
  (jupyter-start (oref comm conn) channel))

;;;; HB channel methods

(cl-defmethod jupyter-hb-beating-p ((comm jupyter-channel-ioloop-comm))
  (jupyter-hb-beating-p (oref comm hb)))

(cl-defmethod jupyter-hb-pause ((comm jupyter-channel-ioloop-comm))
  (jupyter-hb-pause (oref comm hb)))

(cl-defmethod jupyter-hb-unpause ((comm jupyter-channel-ioloop-comm))
  (jupyter-hb-unpause (oref comm hb)))

(provide 'jupyter-channel-ioloop-comm)

;;; jupyter-channel-ioloop-comm.el ends here
