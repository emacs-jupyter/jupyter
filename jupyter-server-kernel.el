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
(require 'jupyter-monads)

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
(defclass jupyter-server (jupyter-rest-client eieio-instance-tracker)
  ((tracking-symbol :initform 'jupyter--servers)
   (kernelspecs
    :type json-plist
    :initform nil
    :documentation "Kernelspecs for the kernels available behind this gateway.
Access should be done through `jupyter-available-kernelspecs'.")))

(cl-defmethod make-instance ((class (subclass jupyter-server)) &rest slots)
  (cl-assert (plist-get slots :url))
  (or (cl-loop
       with url = (plist-get slots :url)
       for server in jupyter--servers
       if (equal url (oref server url)) return server)
      (cl-call-next-method)))

(defun jupyter-servers ()
  "Return a list of all `jupyter-server's."
  jupyter--servers)

(defun jupyter-gc-servers ()
  "Delete `jupyter-server' instances that are no longer accessible."
  (dolist (server (jupyter-servers))
    (unless (jupyter-api-server-exists-p server)
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
       (jupyter-reauthenticate-websockets server)))))

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
  ;; TODO: Make this read only by only allowing creating
  ;; representations of kernels that have already been launched and
  ;; have a connection to the kernel.
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
                     ;; TODO: (jupyter-server-kernelspec server "python3")
                     ;; which returns an I/O action and then arrange
                     ;; for that action to be bound by mlet* and set
                     ;; as the spec value. Or better yet, have
                     ;; `jupyter-kernel' return a delayed kernel with
                     ;; the server connection already open and
                     ;; kernelspecs already retrieved.
                     (or (jupyter-guess-kernelspec
                          spec (jupyter-server-kernelspecs server))
                         ;; TODO: Return the error to the I/O context.
                         (error "No kernelspec matching %s @ %s" spec
                                (oref server url))))))
      (apply #'jupyter-server-kernel args))))

;;; Websocket IO

(defvar jupyter--reauth-subscribers (make-hash-table :weakness 'key :test 'eq))

(defun jupyter-reauthenticate-websockets (server)
  "Re-authenticate WebSocket connections of SERVER."
  (mapc (lambda (sub)
          (jupyter-run-with-io sub
            (jupyter-publish 'reauthenticate)))
        (gethash server jupyter--reauth-subscribers)))

(cl-defmethod jupyter-websocket-io :around (thing)
  "Cache the I/O object of THING in `jupyter-io-cache'."
  (or (gethash thing jupyter-io-cache)
      (puthash thing (cl-call-next-method) jupyter-io-cache)))

(cl-defmethod jupyter-websocket-io ((kernel jupyter-server-kernel))
  "Return a list of three elements representing an I/O connection to kernel.
The returned list looks like (ACTION-SUB MSG-PUB STATUS-PUB)
where

ACTION-SUB is a subscriber of websocket actions to start, stop,
or send a Jupyter message on the websocket.

MSG-PUB is a publisher of Jupyter messages received from the
websocket.

STATUS-PUB is a publisher of status changes to the websocket.

TODO The form of content each sends/consumes."
  (jupyter-do-launch kernel)
  (pcase-let*
      (((cl-struct jupyter-server-kernel server id) kernel)
       (msg-pub (jupyter-publisher))
       (status-pub (jupyter-publisher))
       (on-message
        (lambda (ws frame)
          (pcase (websocket-frame-opcode frame)
            ((or 'text 'binary)
             (let ((msg (jupyter-read-plist-from-string
                         (websocket-frame-payload frame))))
               (jupyter-run-with-io msg-pub
                 (jupyter-publish (cons 'message msg)))))
            (_
             (jupyter-run-with-io status-pub
               (jupyter-publish
                 (list 'error (websocket-frame-opcode frame))))))))
       (make-websocket
        (lambda ()
          (jupyter-api-kernel-websocket
           server id
           :custom-header-alist (jupyter-api-auth-headers server)
           ;; TODO: on-error publishes to status-pub
           :on-message on-message)))
       (ws (funcall make-websocket))
       (kernel-io
        (jupyter-publisher
          (lambda (event)
            (pcase event
              (`(message . ,rest) (jupyter-content rest))
              (`(send ,channel ,msg-type ,content ,msg-id)
               (websocket-send-text
                ws (let* ((cd (websocket-client-data ws))
                          (session (plist-get cd :session)))
                     (jupyter-encode-raw-message session msg-type
                       :channel channel
                       :msg-id msg-id
                       :content content))))
              ('start (websocket-ensure-connected ws))
              ('stop (websocket-close ws)))))))
    (push (jupyter-subscriber
            (lambda (_reauth)
              (websocket-close ws)
              (setq ws (funcall make-websocket))))
          (gethash server jupyter--reauth-subscribers))
    (jupyter-run-with-io msg-pub
      (jupyter-subscribe kernel-io))
    kernel-io))

;;; Kernel management

;; The KERNEL argument is optional here so that `jupyter-do-launch'
;; does not require more than one argument just to handle this case.
(cl-defmethod jupyter-do-launch ((server jupyter-server) &optional (kernel string))
  (cl-check-type kernel string)
  (let* ((spec (jupyter-guess-kernelspec
                kernel (jupyter-server-kernelspecs server)))
         (plist (jupyter-api-start-kernel
                 server (jupyter-kernelspec-name spec))))
    (jupyter-kernel :server server :id (plist-get plist :id) :spec spec)))

;; FIXME: Don't allow creating kernels without them being launched.
(cl-defmethod jupyter-do-launch ((kernel jupyter-server-kernel))
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

(cl-defmethod jupyter-do-shutdown ((kernel jupyter-server-kernel))
  (pcase-let (((cl-struct jupyter-server-kernel server id session) kernel))
    (cl-call-next-method)
    (when session
      (jupyter-api-shutdown-kernel server id))))

(cl-defmethod jupyter-do-interrupt ((kernel jupyter-server-kernel))
  (pcase-let (((cl-struct jupyter-server-kernel server id) kernel))
    (jupyter-api-interrupt-kernel server id)))

(provide 'jupyter-server-kernel)

;;; jupyter-server-kernel.el ends here
