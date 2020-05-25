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

(defvar jupyter--servers-1 (make-hash-table :weakness 'value :test #'equal))

;; TODO: We should really rename `jupyter-server' to something like
;; `jupyter-server-client' since it isn't a representation of a server, but a
;; communication channel with one.
(defclass jupyter-server (jupyter-rest-client eieio-instance-tracker)
  ((tracking-symbol :initform 'jupyter--servers)
   (conn :type jupyter-connection)
   (handlers :type list :initform nil)
   (kernelspecs
    :type json-plist
    :initform nil
    :documentation "Kernelspecs for the kernels available behind this gateway.
Access should be done through `jupyter-available-kernelspecs'.")))

(cl-defmethod make-instance ((class (subclass jupyter-server)) &rest slots)
  (cl-assert (plist-get slots :url))
  (or (gethash (plist-get slots :url) jupyter--servers-1)
      (puthash (plist-get slots :url)
               (cl-call-next-method) jupyter--servers-1)))

(defun jupyter-server-ioloop-io (ioloop)
  (let* ((ids '())
         (event-pub (jupyter-publisher))
         (channels-pub (jupyter-publisher))
         (event-handler
          (lambda (event)
            (if (not (memq (car event)
                           '(connect-channels disconnect-channels)))
                (jupyter-run-with-io event-pub
                  (jupyter-publish event))
              (pcase (car event)
                ((and 'connect-channels (let id (cadr event)))
                 (cl-pushnew id ids :test #'string=))
                ((and 'disconnect-channels (let id (cadr event)))
                 (cl-callf2 delete id ids)))
              ;; Notify subscribers that the connected kernels have
              ;; changed.  Currently only `jupyter-server' uses this.
              (jupyter-run-with-io channels-pub
                (jupyter-publish event)))))
         (start
          (lambda ()
            (unless (jupyter-ioloop-alive-p ioloop)
              ;; Write the cookies to file so that they can be read by
              ;; the subprocess.
              (url-cookie-write-file)
              (jupyter-ioloop-start ioloop event-handler)
              (when ids
                (let ((head ids))
                  ;; Reset KERNEL-IDS since it will be updated after the
                  ;; channels have been re-connected.
                  (setq ids nil)
                  (while head
                    (jupyter-send ioloop 'connect-channels (pop head))))))
            nil))
         (action-sub
          (jupyter-subscriber
            (lambda (action)
              (pcase (if (listp action) (car action) action) 
                ('send
                 (funcall start)
                 (apply #'jupyter-send ioloop action))
                ('event
                 (funcall start)
                 (apply #'jupyter-send ioloop (cdr action)))
                ('start (funcall start))
                ('stop
                 (when (jupyter-ioloop-alive-p ioloop)
                   (jupyter-ioloop-stop ioloop))))))))
    (jupyter-return-delayed
      (list action-sub channels-pub event-pub))))

(defun jupyter-server-kernel-action-sub (action-sub channels-pub)
  "Return a subscriber that connects/disconnects kernel channels.
The subscriber will wait until the channels have been
connected/disconnected before returning.

Content like '(connect-channels ID) or '(disconnect-channels ID)
can be submitted to connect or disconnect the WebSocket channels
of a kernel with ID, a string.

ACTION-SUB is a subscriber of content like

    '(event connect-channels ID)

and does the actual connecting/disconnecting of kernel channels.

CHANNELS-PUB is a publisher of the status changes of a kernel's
channels and publishes content like '(connect-channels ID) when
the corresponding action has been completed."
  (jupyter-subscriber
    (lambda (content)
      (unless (and (memq (car-safe content)
                         '(connect-channels disconnect-channels))
                   (stringp (car (cdr-safe content))))
        (error "Unknown value: %s" content))
      (pcase-let ((`(,action ,id) content)
                  (done nil))
        (jupyter-run-with-io channels-pub
          ;; TODO: (subscribe (subscriber ...)) -> (subscribe ...)
          ;;
          ;; Need to make a publisher struct type to distinguish
          ;; between publisher functions and regular functions
          ;; first.
          (jupyter-subscribe
            (jupyter-subscriber
              (lambda (event)
                (when (and (eq (car event) action)
                           (string= id (cadr event)))
                  (setq done t)
                  (jupyter-unsubscribe))))))
        (jupyter-run-with-io action-sub
          (jupyter-publish (list 'event action id)))
        ;; TODO: Synchronization I/O actions?
        ;;
        ;;     (jupyter-with-io pub
        ;;       (jupyter-wait (lambda () cond)))
        (jupyter-with-timeout
            (nil jupyter-default-timeout
                 (error "Timeout when %sconnecting server channels"
                        (if (eq action 'connect-channels) "" "dis")))
          done)))))

;; TODO: Figure out how to refresh the connection with new
;; auth-headers.  I think its just call this function again.  Due to
;; the functional design, all references to the old objects should get
;; cleaned up.
(cl-defmethod jupyter-add-finalizer (obj finalizer)
  (cl-callf append (gethash obj jupyter-finalizer-pool)
    (list (make-finalizer finalizer))))

(defun jupyter-server-io (server)
  (let ((ioloop (jupyter-server-ioloop
                 :url (oref server url)
                 :ws-url (oref server ws-url)
                 :ws-headers (jupyter-api-auth-headers server))))
    ;; TODO: Another instance where it would be great for mlet* to
    ;; support `pcase' patterns.  Or should it be the other way round?
    ;; Make a `pcase' macro for I/O values.
    ;;
    ;; (pcase (jupyter-server-ioloop-io ioloop)
    ;;   ((jupyter-io `(,action-sub ,kernel-channels-pub ,ioloop-event-pub))
    ;;    ...))
    (jupyter-mlet* ((value (jupyter-server-ioloop-io ioloop)))
      (pcase-let ((`(,action-sub ,channels-pub ,event-pub) value))
        (jupyter-add-finalizer server
          (lambda ()
            (jupyter-run-with-io action-sub
              (jupyter-publish 'stop))))
        ;; FIXME: mlet* should wrap the result in
        ;; `jupyter-return-delayed'.
        (jupyter-return-delayed
          (list action-sub
                (jupyter-server-kernel-action-sub action-sub channels-pub)
                event-pub))))))

(cl-defmethod jupyter-io ((server jupyter-server))
  (jupyter-mlet* ((io (jupyter-server-io server)))
    io))

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
       (jupyter-send (jupyter-io server) 'auth-headers
                     (jupyter-api-auth-headers server))))))

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

;;; Client connection

(defun jupyter-server-kernel-io (kernel)
  ;; TODO: What about disconnecting channels?  Do that at a later
  ;; stage.
  (pcase-let (((cl-struct jupyter-server-kernel server id) kernel)
              (discarded nil))
    (jupyter-mlet* ((server-io (jupyter-io server)))
      (pcase-let*
          ((`(,action-sub ,kernel-action ,event-pub) server-io)
           (kernel-io
            (jupyter-publisher
              (lambda (event)
                (if discarded
                    (error "Kernel I/O no longer available")
                  (pcase event
                    ((and `(message ,kid . ,rest)
                          (guard (string= kid id)))
                     (jupyter-content rest))
                    (`(unsubscribe ,kid)
                     (when (string= kid id)
                       (jupyter-unsubscribe)))
                    (_
                     (jupyter-run-with-io action-sub
                       (jupyter-publish (cl-list* 'send id args))))))))))
        (jupyter-do
          (jupyter-with-io kernel-action
            (jupyter-publish (list 'connect-channels id)))
          (jupyter-with-io event-pub
            (jupyter-subscribe kernel-io))
          (jupyter-return-delayed
            (list kernel-io
                  ;; TODO: Bring this, as an action, into kernel-io
                  (lambda ()
                    (jupyter-run-with-io event-pub
                      ;; TODO: How can this be avoided?
                      (jupyter-publish (list 'unsubscribe id)))
                    (jupyter-run-with-io kernel-action
                      (jupyter-publish (list 'disconnect-channels id)))
                    (setq discarded t)))))))))

(cl-defmethod jupyter-io ((kernel jupyter-server-kernel))
  (jupyter-server-kernel-io kernel))


;;; Websocket IO

(defun jupyter--websocket (kernel)
  (cl-check-type kernel jupyter-server-kernel)
  (make-jupyter-delayed
   :value (lambda ()
            (pcase-let
                (((cl-struct jupyter-server-kernel server id) kernel)
                 (msg-pub (jupyter-publisher))
                 (status-pub (jupyter-publisher)))
              (list
               (jupyter-api-kernel-websocket
                server id
                :custom-header-alist (jupyter-api-auth-headers server)
                :on-message
                (lambda (_ws frame)
                  (pcase (websocket-frame-opcode frame)
                    ((or 'text 'binary)
                     (jupyter-run-with-io msg-pub
                       (jupyter-publish
                         (jupyter-read-plist-from-string
                          (websocket-frame-payload frame)))))
                    (_
                     (jupyter-run-with-io status-pub
                       (jupyter-publish
                         (list 'error (websocket-frame-opcode frame))))))))
               msg-pub
               status-pub)))))

(defun jupyter-return-websocket-io (kernel)
  "Return a list of three elements representing an I/O connection to kernel.
The returned list looks like (ACTION-SUB MSG-PUB STATUS-PUB)
where

ACTION-SUB is a subscriber of websocket actions to start, stop,
or send a Jupyter message on the websocket.

MSG-PUB is a publisher of Jupyter messages received from the
websocket.

STATUS-PUB is a publisher of status changes to the websocket.

TODO The form of content each sends/consumes."
  (cl-check-type kernel jupyter-server-kernel)
  (jupyter-mlet* ((value (jupyter-do
                           (jupyter-kernel-launch kernel)
                           (jupyter--websocket kernel))))
    (pcase-let ((`(,ws ,msg-pub ,status-pub) value))
      ;; Make sure the websocket is cleaned up when it is garbage
      ;; collected.
      (plist-put (websocket-client-data ws)
                 :finalizer (make-finalizer (lambda () (websocket-close ws))))
      (jupyter-return-delayed
        (list
         ;; The websocket action subscriber.
         (jupyter-subscriber
           (lambda (msg)
             (pcase msg
               (`(send ,channel ,msg-type ,content ,msg-id)
                (websocket-send-text
                 ws (jupyter-encode-raw-message
                        (plist-get (websocket-client-data ws) :session) msg-type
                      :channel channel
                      :msg-id msg-id
                      :content content)))
               ('start (websocket-ensure-connected ws))
               ('stop (websocket-close ws)))))
         ;; The websocket message publisher.
         msg-pub
         ;; The websocket status publisher.
         status-pub)))))


;;; Kernel management

(cl-defmethod jupyter-launch ((server jupyter-server) (kernel string))
  (let* ((spec (jupyter-guess-kernelspec
                kernel (jupyter-server-kernelspecs server)))
         (plist (jupyter-api-start-kernel
                 server (jupyter-kernelspec-name spec))))
    (jupyter-kernel :server server :id (plist-get plist :id) :spec spec)))

;; FIXME: Don't allow creating kernels without them being launched.
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
