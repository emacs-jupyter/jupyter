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

(cl-defmethod jupyter-connection ((server jupyter-server))
  "Return a list of two functions, the first used to send events
to SERVER and the second used to add a message handler to
SERVER's event stream.  A handler passed an even number of times
will cause it to no longer handle SERVER's events.  The handler
is also removed if it returns nil after handling an event."
  (let ((url (oref server url))
        (ws-url (oref server ws-url))
        (handlers '())
        (kernel-ids '())
        (ioloop nil))
    (cl-labels
        ((set-ioloop
          (auth-headers)
          (setq ioloop
                (jupyter-server-ioloop
                 :url url
                 :ws-url ws-url
                 :ws-headers auth-headers)))
         (ch-action
          (action id)
          (jupyter-send ioloop action id)
          (unless (jupyter-ioloop-wait-until ioloop action #'identity)
            (error "Timeout when %sconnecting server channels"
                   (if (eq action 'connect-channels) "" "dis"))))
         (reconnect-kernels
          ()
          (when kernel-ids
            (let ((ids kernel-ids))
              ;; Reset KERNEL-IDS since it will be updated after the
              ;; channels have been re-connected.
              (setq kernel-ids nil)
              (while ids
                (ch-action 'connect-channels (pop ids))))))
         (start
          ()
          ;; No need to check for IOLOOP being nil since it will
          ;; already be set before the first call to this function.
          (unless (jupyter-ioloop-alive-p ioloop)
            ;; Write the cookies to file so that they can be read by
            ;; the subprocess.
            (url-cookie-write-file)
            (jupyter-ioloop-start
             ioloop
             (lambda (event)
               (pcase (car event)
                 ((and type (guard (not (memq type '(connect-channels
                                                     disconnect-channels)))))
                  (cl-loop
                   for handler in handlers
                   do (funcall handler event)))
                 ((and 'connect-channels (let id (cadr event)))
                  (cl-pushnew id kernel-ids :test #'string=))
                 ((and 'disconnect-channels (let id (cadr event)))
                  (cl-callf2 delete id kernel-ids)))))
            (reconnect-kernels))
          ioloop)
         (stop
          ()
          (when (jupyter-ioloop-alive-p ioloop)
            (jupyter-ioloop-stop ioloop))))
      (set-ioloop (jupyter-api-auth-headers server))
      (list (lambda (&rest args)
              (pcase (car args)
                ('message (apply #'jupyter-send (start) 'send (cdr args)))
                ('alive-p (jupyter-ioloop-alive-p ioloop))
                ('start (start) nil)
                ('stop (stop) nil)
                ((and 'add-handler (let h (cadr args)))
                 (start)
                 (cl-pushnew h handlers))
                ((and 'remove-handler (let h (cadr args)))
                 (cl-callf2 delq h handlers)
                 (unless handlers
                   (stop)))
                ((or 'connect-channels 'disconnect-channels)
                 (start)
                 (apply #'ch-action args))
                ((and 'auth-headers (let auth-headers (cadr args)))
                 (stop)
                 (set-ioloop auth-headers)
                 (when handlers
                   (start)))
                (_ (error "Unhandled IO: %s" args))))
            (make-finalizer stop)))))

(cl-defmethod jupyter-connection ((spec (head server)))
  (jupyter-connection (jupyter-server :url (cadr spec))))

(defun jupyter-servers ()
  "Return a list of all `jupyter-server's."
  jupyter--servers)

(defun jupyter-gc-servers ()
  "Forget `jupyter-servers' that are no longer accessible at their hosts."
  (dolist (server (jupyter-servers))
    (unless (jupyter-api-server-exists-p server)
      ;; TODO: Stopping a connection, stops all subordinate
      ;; connections and disconnects all subordinate clients.
      (jupyter-stop (jupyter-io server))
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
          :read-only t
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
If ARGS has a :server key, return a `jupyter-server-kernel'
initialized using ARGS.  If ARGS also has a :spec key, whose
value is the name of a kernelspec, the returned kernel's spec
slot will be the corresponding `jupyter-kernelspec'.

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

(cl-defmethod jupyter-connection ((kernel jupyter-server-kernel))
  "Return a list representing a connection to KERNEL.
The list looks like (IO RESERVED...) where IO is a function
taking any number of arguments and RESERVED are used internally.

This function is called by `jupyter-io', which see."
  (pcase-let* (((cl-struct jupyter-server-kernel server id) kernel)
               (server-io (jupyter-io server))
               (handlers '())
               (connected nil)
               (filter-by-id
                (lambda (event)
                  (pcase-let ((`(,type ,kid . ,rest) event))
                    (when (string= kid id)
                      (jupyter-run-handlers handlers (cons type rest)))))))
    (cl-macrolet ((server-io (&rest args)
                             (if (eq (car (last args)) 'args)
                                 `(apply server-io ,@args)
                               `(funcall server-io ,@args))))
      (cl-labels ((start
                   ()
                   (unless connected
                     (server-io 'connect-channels id)
                     (server-io 'add-handler filter-by-id)
                     (setq connected t)))
                  (stop
                   ()
                   (when connected
                     (server-io 'remove-handler filter-by-id)
                     (server-io 'disconnect-channels id)
                     (setq connected nil))))
        ;; These functions should not depend on KERNEL. See
        ;; `jupyter-connections' for the reason why.
        (list
         (lambda (&rest args)
           (pcase (car args)
             ('message
              (start)
              (server-io 'message id args))
             ('start (start) nil)
             ('stop (stop) nil)
             ('alive-p
              (and (server-io 'alive-p) connected))
             ((and 'add-handler (let h (cadr args)))
              (start)
              (cl-pushnew h handlers))
             ((and 'remove-handler (let h (cadr args)))
              (cl-callf2 delq h handlers)
              (unless handlers
                (stop)))
             ('hb nil)
             (_
              (server-io args))))
         (make-finalizer stop))))))

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
