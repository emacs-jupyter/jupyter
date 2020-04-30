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
(require 'jupyter-connection)

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
   (conn :type jupyter-connection)
   (handlers :type list :initform nil)
   (kernelspecs
    :type json-plist
    :initform nil
    :documentation "Kernelspecs for the kernels available behind this gateway.
Access should be done through `jupyter-available-kernelspecs'.")))

(cl-defmethod jupyter-connection ((ioloop jupyter-server-ioloop) (handler function))
  (let ((kernel-ids '()))
    (make-jupyter-connection
     :alive-p
     (lambda (&rest _)
       (jupyter-ioloop-alive-p ioloop))
     :send
     (lambda (&rest event)
       (apply #'jupyter-send ioloop event)
       (let ((event-type (car event)))
         (when (memq event-type '(connect-channels disconnect-channels))
           (unless (jupyter-ioloop-wait-until ioloop
                       (car event) #'identity)
             (error "Timeout when sending server event: %s" event)))))
     :start
     (lambda (&rest _)
       (when (jupyter-ioloop-alive-p ioloop)
         (jupyter-ioloop-stop ioloop))
       ;; Write the cookies to file so that they can be
       ;; read by the subprocess.
       (url-cookie-write-file)
       (jupyter-ioloop-start
        ioloop (lambda (event)
                 (let ((event-type (car event)))
                   (pcase event-type
                     ('connect-channels (push event-kid kernel-ids))
                     ('disconnect-channels (pop event-kid kernel-ids))
                     (_ (funcall handler event))))))
       ;; Re-connect kernels if there were some
       (when kernel-ids
         (let ((connected kernel-ids))
           (setq kernel-ids nil)
           (while connected
             (jupyter-server--connect-channels server (pop connected))))))
     :stop
     (lambda (&rest _)
       (jupyter-ioloop-stop ioloop)))))

(cl-defstruct jupyter-server--event-handler id fn)

(cl-defmethod jupyter-connection ((server jupyter-server) (ioloop jupyter-server-ioloop))
  (cl-call-next-method
   ioloop (lambda (event)
            (pcase-let ((`(,type ,kid . ,rest) event))
              (setq event (cons type rest))
              (cl-loop
               for handler in (oref server handlers)
               when (string= kid (jupyter-server--event-handler-id handler))
               do (funcall
                   (jupyter-server--event-handler-fn handler)
                   event))))))

(defvar jupyter-server--ioloop-connections (make-hash-table :weakness 'key))

(defun jupyter-server--ioloop-connection (server)
  (or (gethash server jupyter-server--ioloop-connections)
      (puthash server (jupyter-connection
                       server (jupyter-server-ioloop
                               :url (oref server url)
                               :ws-url (oref server ws-url)
                               :ws-headers (jupyter-api-auth-headers server)))
               jupyter-server--ioloop-cache)))

(cl-defmethod jupyter-connection ((server jupyter-server) (handler jupyter-server--event-handler))
  (pcase-let (((cl-struct jupyter-server--event-handler id) handler)
              (icomm (jupyter-server--ioloop-connection server)))
    (make-jupyter-connection
     :id (lambda ()
           (or (jupyter-server-kernel-name server id)
               (format "kid=%s" (truncate-string-to-width id 9 nil nil "…"))))
     :start (lambda (&optional channel)
              (if channel (error "Can't start individual channels")
                (jupyter-send icomm 'connect-channels id)
                (cl-callf2 cl-adjoin handler (oref server handlers))))
     :stop (lambda (&optional channel)
             (if channel (error "Can't stop individual channels")
               (jupyter-send icomm 'disconnect-channels id)
               (cl-callf2 delq handler (oref server handlers))))
     :send (lambda (&rest event)
             (apply #'jupyter-send icomm
                    (car event) id (cdr event)))
     :alive-p (lambda (&optional _channel)
                (and (memq handler (oref server handlers))
                     (jupyter-alive-p icomm))))))

(defun jupyter-servers ()
  "Return a list of all `jupyter-server's."
  jupyter--servers)

(defun jupyter-gc-servers ()
  "Forget `jupyter-servers' that are no longer accessible at their hosts."
  (dolist (server (jupyter-servers))
    (unless (jupyter-api-server-exists-p server)
      (jupyter-stop (oref server conn))
      (jupyter-api-delete-cookies (oref server url))
      (delete-instance server))))

(cl-defmethod jupyter-api-request :around ((server jupyter-server) _method &rest _plist)
  (condition-case nil
      (cl-call-next-method)
    (jupyter-api-unauthenticated
     (if (memq jupyter-api-authentication-method '(ask token password))
         (oset server auth jupyter-api-authentication-method)
       (error "Unauthenticated request, can't attempt re-authentication \
with default `jupyter-api-authentication-method'"))
     (prog1 (cl-call-next-method)
       (jupyter-stop (oref server conn))
       (jupyter-start (oref server conn))))))

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
  "Return a `jupyter-server-kernel' initialized with ARGS."
  (apply #'make-jupyter-server-kernel args))

(cl-defmethod jupyter-kernel :extra "server" (&rest args)
  "Return a representation of a kernel on a Jupyter server.
If ARGS contains a :server key, return a `jupyter-server-kernel'
initialized using ARGS.  If ARGS contains a :spec key whose value
is the name of a kernelspec, the returned kernel's spec slot will
be set to the corresponding `jupyter-kernelspec'.

Call the next method if ARGS does not contain :server."
  (let ((server (plist-get args :server)))
    (if (not server) (cl-call-next-method)
      (cl-assert (object-of-class-p server 'jupyter-server))
      (let ((spec (plist-get args :spec)))
        (when (stringp spec)
          (plist-put args :spec
                     (or (jupyter-guess-kernelspec
                          spec (jupyter-server-kernelspecs server))
                         (error "No kernelspec matching %s @ %s" spec
                                (oref server url))))))
      (apply #'jupyter-server-kernel args))))

;;; Client connection

(cl-defmethod jupyter-connection ((kernel jupyter-server-kernel) (handler function))
  (pcase-let (((cl-struct jupyter-server-kernel server id) kernel))
    (cl-call-next-method server (make-jupyter-server--event-handler
                                 :id id :fn handler))))

;;; Kernel management

(cl-defmethod jupyter-launch ((kernel jupyter-server-kernel))
  "Launch KERNEL based on its kernelspec.
When KERNEL does not have an ID yet, launch KERNEL on SERVER
using its SPEC."
  (pcase-let (((cl-struct jupyter-server-kernel server id spec session) kernel))
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
          (setf (jupyter-server-kernel-id kernel) (plist-get plist :id))
		  (sit-for 1)))
      ;; TODO: Replace with the real session object
	  (setf (jupyter-kernel-session kernel) (jupyter-session))))
  (cl-call-next-method))

(cl-defmethod jupyter-shutdown ((kernel jupyter-server-kernel))
  (pcase-let (((cl-struct jupyter-server-kernel server id session) kernel))
    (cl-call-next-method)
    (when session
      (jupyter-api-shutdown-kernel server id))))

(cl-defmethod jupyter-interrupt ((kernel jupyter-server-kernel))
  (pcase-let (((cl-struct jupyter-server-kernel server id) kernel))
    (jupyter-api-interrupt-kernel server id)))

(provide 'jupyter-server-kernel)

;;; jupyter-server-kernel.el ends here



