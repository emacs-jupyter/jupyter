;;; jupyter-server-kernel.el --- Working with kernels behind a Jupyter server -*- lexical-binding: t -*-

;; Copyright (C) 2020 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 23 Apr 2020

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

;; Holds the definitions of `jupyter-server', what communicates to the
;; Jupyter server using the REST API, and `jupyter-kernel-server' a
;; representation of a kernel on a server.

;;; Code:

(require 'jupyter-kernel)
(require 'jupyter-rest-api)
(require 'jupyter-server-ioloop)

(defgroup jupyter-server-kernel nil
  "Kernel behind a Jupyter server"
  :group 'jupyter)

;;; `jupyter-server'


(defvar-local jupyter-current-server nil
  "The `jupyter-server' associated with the current buffer.
Used in, e.g. a `jupyter-server-kernel-list-mode' buffer.")

(put 'jupyter-current-server 'permanent-local t)

(defvar jupyter--servers nil)

;; TODO: We should really rename `jupyter-server' to something like
;; `jupyter-server-client' since it isn't a representation of a server, but a
;; communication channel with one.
(defclass jupyter-server (jupyter-rest-client
                          eieio-instance-tracker)
  ((tracking-symbol :initform 'jupyter--servers)
   (ioloop :type jupyter-ioloop)
   (handlers :type list :initform nil)
   (kernelspecs
    :type json-plist
    :initform nil
    :documentation "Kernelspecs for the kernels available behind this gateway.
Access should be done through `jupyter-available-kernelspecs'.")))

(defun jupyter-servers ()
  "Return a list of all `jupyter-server's."
  jupyter--servers)

(defun jupyter-gc-servers ()
  "Forget `jupyter-servers' that are no longer accessible at their hosts."
  (dolist (server (jupyter-servers))
    (unless (jupyter-api-server-exists-p server)
      (when (jupyter-ioloop-alive-p (oref server ioloop))
        (jupyter-ioloop-stop (oref server ioloop)))
      (jupyter-api-delete-cookies (oref server url))
      (delete-instance server))))


(defun jupyter-server--refresh-comm (server)
  "Stop and then start SERVER communication.
Reconnect the previously connected kernels when starting."
  (when (jupyter-ioloop-alive-p (oref server ioloop))
    (let ((connected (cl-remove-if-not
                      (apply-partially #'jupyter-server-kernel-connected-p server)
                      (mapcar (lambda (kernel) (plist-get kernel :id))
                              (jupyter-api-get-kernel server)))))
      (jupyter-ioloop-stop (oref server ioloop))
      (jupyter-server--start-comm server)
      (while connected
        (jupyter-server--connect-channels server (pop connected))))))

(cl-defmethod jupyter-api-request :around ((server jupyter-server) _method &rest _plist)
  (condition-case nil
      (cl-call-next-method)
    (jupyter-api-unauthenticated
     (if (memq jupyter-api-authentication-method '(ask token password))
         (oset server auth jupyter-api-authentication-method)
       (error "Unauthenticated request, can't attempt re-authentication \
with default `jupyter-api-authentication-method'"))
     (prog1 (cl-call-next-method)
       (jupyter-server--refresh-comm server)))))

(cl-defmethod jupyter-server-kernel-connected-p ((comm jupyter-server) id)
  "Return non-nil if COMM has a WebSocket connection to a kernel with ID."
  (and (jupyter-ioloop-alive-p (oref comm ioloop))
       (member id (process-get (oref (oref comm ioloop) process) :kernel-ids))))

(cl-defmethod jupyter-server-kernelspecs ((server jupyter-server) &optional refresh)
  "Return the kernelspecs on SERVER.
By default the available kernelspecs are cached.  To force an
update of the cached kernelspecs, give a non-nil value to
REFRESH.

The kernelspecs are returned in the same form as returned by
`jupyter-available-kernelspecs'."
  (when (or refresh (null (oref server kernelspecs)))
    (let ((specs (jupyter-api-get-kernelspec server)))
      (unless specs
        (error "Can't retrieve kernelspecs from server @ %s" (oref server url)))
      (oset server kernelspecs specs)
      (plist-put (oref server kernelspecs) :kernelspecs
                 (cl-loop
                  with specs = (plist-get specs :kernelspecs)
                  for (_ spec) on specs by #'cddr
                  for name = (plist-get spec :name)
                  collect (make-jupyter-kernelspec
                           :name name
                           :plist (plist-get spec :spec))))))
  (plist-get (oref server kernelspecs) :kernelspecs))

(cl-defmethod jupyter-server-has-kernelspec-p ((server jupyter-server) name)
  "Return non-nil if SERVER can launch kernels with kernelspec NAME."
  (jupyter-guess-kernelspec name (jupyter-server-kernelspecs server)))



;;; Kernel definition

(cl-defstruct (jupyter-server-kernel
               (:include jupyter-kernel))
  (server jupyter-current-server
          :documentation "The kernel server.")
  (id nil
      :type (or null string)
      :documentation "The kernel ID."))

(cl-defmethod jupyter-alive-p ((kernel jupyter-server-kernel))
  (pcase-let (((cl-struct jupyter-server-kernel server id) kernel))
    (and id server
         ;; TODO: Cache this call
         (condition-case err
             (jupyter-api-get-kernel server id)
           (file-error nil)             ; Non-existent server
           (jupyter-api-http-error
            (unless (= (nth 1 err) 404) ; Not Found
              (signal (car err) (cdr err)))))
         (cl-call-next-method))))

(defun jupyter-server-kernel (&rest args)
  "Return a representation of a kernel on a Jupyter server.
ARGS is a property list used to initialize the returned
`jupyter-server-kernel'.  The following keys of ARGS are handled
specially:

  - If :spec is present and it is the name of a kernelspec, then
    the SPEC of the returned kernel will be the one associated
    with that name on the server."
  (cl-assert (jupyter-server-p (plist-get args :server)))
  (when (stringp (plist-get args :spec))
    (let ((server (plist-get args :server))
          (name (plist-get args :spec)))
      (plist-put args :spec
                 (or (jupyter-guess-kernelspec
                      name (jupyter-server-kernelspecs server))
                     (error "No kernelspec matching %s @ %s" name
                            (oref server url))))))
  (apply #'make-jupyter-server-kernel args))

;;;; Kernel management 

(cl-defmethod jupyter-launch ((kernel jupyter-server-kernel))
  "Launch KERNEL based on its kernelspec.
When KERNEL does not have an ID yet, launch KERNEL on SERVER
using its SPEC."
  (pcase-let
	  (((cl-struct jupyter-server-kernel server id spec session) kernel))
	(unless session
	  (and id (setq id (or (jupyter-server-kernel-id-from-name server id) id)))
	  (if id
          ;; When KERNEL already has an ID before it has a session,
          ;; assume we are connecting to an already launched kernel.  In
          ;; this case, make sure the KERNEL's SPEC is the same as the
          ;; one being connected to.
          ;;
          ;; Note, this also has the side effect of raising an error
          ;; when the ID does not match one on the server.
		  (unless spec
			(let ((model (jupyter-api-get-kernel server id)))
			  (setf (jupyter-kernel-spec kernel) 
					(jupyter-guess-kernelspec
					 (plist-get model :name)
					 (jupyter-server-kernelspecs server)))))
		(let ((plist (jupyter-api-start-kernel
					  server (jupyter-kernelspec-name spec))))
		  (setf (jupyter-server-kernel-id kernel) (plist-get plist :id))))
      ;; TODO: Replace with the real session object
	  (setf (jupyter-kernel-session kernel) (jupyter-session))))
  (cl-call-next-method))

(cl-defmethod jupyter-shutdown ((kernel jupyter-server-kernel))
  (pcase-let
      (((cl-struct jupyter-server-kernel server id session) kernel))
    (cl-call-next-method)
    (when session
      (jupyter-api-shutdown-kernel server id))))

(cl-defmethod jupyter-interrupt ((kernel jupyter-server-kernel))
  (pcase-let (((cl-struct jupyter-server-kernel server id) kernel))
    (jupyter-api-interrupt-kernel server id)))

(cl-defstruct jupyter-server--event-handler id fn)

(defun jupyter-server--start-comm (server)
  (unless (and (slot-boundp server 'ioloop)
               (jupyter-ioloop-alive-p (oref server ioloop)))
    ;; Write the cookies to file so that they can be read
    ;; by the subprocess.
    (url-cookie-write-file)
    (let ((ioloop (jupyter-server-ioloop
                   :url (oref server url)
                   :ws-url (oref server ws-url)
                   :ws-headers (jupyter-api-auth-headers server))))
      (oset server ioloop ioloop)
      (jupyter-ioloop-start
       ioloop
       (lambda (event)
         (let ((event-type (car event))
               (event-kid (cadr event)))
           (pcase event-type
             ('connect-channels
              (cl-callf append (process-get (oref ioloop process) :kernel-ids)
                (list event-kid)))
             ('disconnect-channels
              (cl-callf2 remove event-kid
                         (process-get (oref ioloop process) :kernel-ids)))
             (_
              (setq event (cons event-type (cddr event)))
              (cl-loop
               for handler in (oref server handlers)
               when (string= event-kid
                             (jupyter-server--event-handler-id handler))
               do (funcall
                   (jupyter-server--event-handler-fn handler)
                   event))))))))))

(defun jupyter-server--connect-channels (server id)
  (jupyter-send (oref server ioloop) 'connect-channels id)
  (unless (jupyter-server-kernel-connected-p server id)
    (jupyter-with-timeout
        (nil jupyter-default-timeout
             (error "Timeout when connecting websocket to kernel id %s" id))
      (jupyter-server-kernel-connected-p server id))))

(defun jupyter-server--disconnect-channels (server id)
  ;; from the comm-remove-handler of a server
  (jupyter-send (oref server ioloop) 'disconnect-channels id)
  (unless (jupyter-ioloop-wait-until (oref server ioloop)
              'disconnect-channels #'identity)
    (error "Timeout when disconnecting websocket for kernel id %s" id)))

(cl-defmethod jupyter-connection ((kernel jupyter-server-kernel) (handler function))
  (pcase-let* (((cl-struct jupyter-server-kernel server id) kernel)
               (-handler (make-jupyter-server--event-handler
                          :id id :fn handler)))
    (jupyter-server--start-comm server)
    (make-jupyter-connection
     :id (lambda ()
           (or (jupyter-server-kernel-name server id)
               (format "kid=%s" (truncate-string-to-width id 9 nil nil "…"))))
     :start (lambda (&optional channel)
              (if channel (error "Can't start individual channels")
                (jupyter-server--connect-channels server id)
                (cl-callf2 cl-adjoin -handler (oref server handlers))))
     :stop (lambda (&optional channel)
             (if channel (error "Can't stop individual channels")
               (jupyter-server--disconnect-channels server id)
               (cl-callf2 delq -handler (oref server handlers))))
     :send (lambda (&rest event)
             (apply #'jupyter-send (oref server ioloop)
                    (car event) id (cdr event)))
     :alive-p (lambda (&optional _channel)
                (and (jupyter-server-kernel-connected-p server id)
                     (memq -handler (oref server handlers)))))))

(provide 'jupyter-server-kernel)

;;; jupyter-server-kernel.el ends here



