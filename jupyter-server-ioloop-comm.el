;;; jupyter-server-ioloop-comm.el --- Async support for Jupyter kernel servers -*- lexical-binding: t -*-

;; Copyright (C) 2020 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 11 Mar 2020

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

;; Async support for Jupyter kernel servers. Requires ZMQ.

;;; Code:

(require 'jupyter-server)
(require 'jupyter-ioloop-comm)
(require 'jupyter-server-ioloop)

(defgroup jupyter-server-ioloop-comm nil
  "Async support for Jupyter kernel servers (requires ZMQ)."
  :group 'jupyter)

(defclass jupyter-server-ioloop-comm (jupyter-server jupyter-ioloop-comm)
  ())

(defclass jupyter-server-ioloop-kernel-comm (jupyter-server-abstract-kcomm)
  ())

;;; `jupyter-ioloop-comm' event handlers

(cl-defmethod jupyter-event-handler ((comm jupyter-server-ioloop-comm)
                                     (event (head disconnect-channels)))
  (let ((kernel-id (cadr event)))
    (with-slots (ioloop) comm
      (cl-callf2 remove kernel-id
                 (process-get (oref ioloop process) :kernel-ids)))))

(cl-defmethod jupyter-event-handler ((comm jupyter-server-ioloop-comm)
                                     (event (head connect-channels)))
  (let ((kernel-id (cadr event)))
    (with-slots (ioloop) comm
      (cl-callf append (process-get (oref ioloop process) :kernel-ids)
        (list kernel-id)))))

(cl-defmethod jupyter-event-handler ((comm jupyter-server-ioloop-comm) event)
  "Send EVENT to all clients connected to COMM.
Each client must have a KERNEL slot which, in turn, must have an
ID slot. The second element of EVENT is expected to be a kernel
ID. Send EVENT, with the kernel ID excluded, to a client whose
kernel has a matching ID."
  (let ((kernel-id (cadr event)))
    (setq event (cons (car event) (cddr event)))
    (jupyter-comm-handler-loop comm client
      (when (equal kernel-id (oref (oref client kernel) id))
        ;; TODO: Since the event handlers of CLIENT will eventually call the
        ;; `jupyter-handle-message' of a `jupyter-kernel-client' we really
        ;; don't need to do any filtering based off of a `jupyter-session-id',
        ;; but maybe should? The `jupyter-handle-message' method will only
        ;; handle messages that have a parent ID of a previous request so there
        ;; already is filtering at the kernel client level.
        (jupyter-event-handler client event)))))

;;; `jupyter-ioloop-comm' methods

(cl-defmethod jupyter-comm-start ((comm jupyter-server-ioloop-comm))
  (unless (and (slot-boundp comm 'ioloop)
               (jupyter-ioloop-alive-p (oref comm ioloop)))
    ;; TODO: Is a write to the cookie file and then a read of the cookie file
    ;; whenever connecting a websocket in a subprocess good enough? If, e.g.
    ;; the notebook is restarted and it clears the login information, there are
    ;; sometimes error due to `jupyter-api-request' trying to ask for login
    ;; information which look like "wrong type argument listp, [http://...]".
    ;; They don't seem to happens with the changes mentioned, but is it enough?
    (url-cookie-write-file)
    (oset comm ioloop (jupyter-server-ioloop
                       :url (oref comm url)
                       :ws-url (oref comm ws-url)
                       :ws-headers (jupyter-api-auth-headers comm)))
    (cl-call-next-method)))

(cl-defmethod jupyter-comm-add-handler ((comm jupyter-server-ioloop-comm)
                                      (kcomm jupyter-server-ioloop-kernel-comm))
  (cl-call-next-method)
  (with-slots (id) (oref kcomm kernel)
    (unless (jupyter-server-kernel-connected-p comm id)
      (jupyter-server--connect-channels comm id))))

(cl-defmethod jupyter-comm-remove-handler ((comm jupyter-server-ioloop-comm)
                                         (kcomm jupyter-server-ioloop-kernel-comm))
  (with-slots (id) (oref kcomm kernel)
    (when (jupyter-server-kernel-connected-p comm id)
      (jupyter-send comm 'disconnect-channels id)
      (unless (jupyter-ioloop-wait-until (oref comm ioloop)
                  'disconnect-channels #'identity)
        (error "Timeout when disconnecting websocket for kernel id %s" id))))
  (cl-call-next-method))

(cl-defmethod jupyter-server-kernel-connected-p ((comm jupyter-server-ioloop-comm) id)
  "Return non-nil if COMM has a WebSocket connection to a kernel with ID."
  (and (jupyter-comm-alive-p comm)
       (member id (process-get (oref (oref comm ioloop) process) :kernel-ids))))

;; `jupyter-server-ioloop-kcomm'

(cl-defmethod jupyter-comm-start ((comm jupyter-server-ioloop-kernel-comm) &rest _ignore)
  "Register COMM to receive server events.
If SERVER receives events that have the same kernel ID as the
kernel associated with COMM, then COMM's `jupyter-event-handler'
will receive those events."
  (with-slots (server) (oref comm kernel)
    (jupyter-comm-add-handler server comm)))

(cl-defmethod jupyter-comm-stop ((comm jupyter-server-ioloop-kernel-comm) &rest _ignore)
  "Disconnect COMM from receiving server events."
  (jupyter-comm-remove-handler (oref (oref comm kernel) server) comm))

(cl-defmethod jupyter-comm-alive-p ((comm jupyter-server-ioloop-kernel-comm))
  "Return non-nil if COMM can receive server events for its associated kernel."
  (with-slots (kernel) comm
    (and (jupyter-server-kernel-connected-p
          (oref kernel server)
          (oref kernel id))
         (catch 'member
           (jupyter-comm-handler-loop (oref kernel server) client
             (when (eq client comm)
               (throw 'member t)))))))

(cl-defmethod jupyter-send ((comm jupyter-server-ioloop-kernel-comm) event-type &rest event)
  "Use COMM to send an EVENT to the server with type, EVENT-TYPE.
SERVER will direct EVENT to the right kernel based on the kernel
ID of the kernel associated with COMM."
  (with-slots (kernel) comm
    (unless (jupyter-comm-alive-p comm)
      (jupyter-comm-start comm))
    (apply #'jupyter-send (oref kernel server) event-type (oref kernel id) event)))

(provide 'jupyter-server-ioloop-comm)

;;; jupyter-server-ioloop-comm.el ends here
