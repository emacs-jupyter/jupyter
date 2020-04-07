;;; jupyter-server.el --- Support for the Jupyter kernel servers -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 02 Apr 2019

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

;; Overview of implementation
;;
;; A `jupyter-server' communicates with a Jupyter kernel server (either the
;; notebook or a kernel gateway) via the Jupyter REST API.  Given the URL and
;; Websocket URL for the server, the `jupyter-server' object can launch kernels
;; using the function `jupyter-server-start-new-kernel'.  The kernelspecs
;; available on the server can be accessed by calling
;; `jupyter-server-kernelspecs'.
;;
;; `jupyter-server-start-new-kernel' returns a list (KM KC) where KM is a
;; `jupyter-server-kernel-manager' and KC is a kernel client that can
;; communicate with the kernel managed by KM.  `jupyter-server-kernel-manager'
;; sends requests to the server using the `jupyter-server' object to manage the
;; lifetime of the kernel and ensures that a websocket is opened so that kernel
;; clients created using `jupyter-make-client' can communicate with the kernel.
;;
;; Communication with the channels of the kernels that are launched on the
;; `jupyter-server' is established via a `jupyter-server-ioloop' which
;; multiplexes the channels of all the kernel servers.  The kernel ID the server
;; associated with a kernel can then be used to filter messages for a
;; particular kernel and to send messages to a kernel through the
;; `jupyter-server-ioloop'.
;;
;; `jupyter-server-kernel-comm' is a `jupyter-comm-layer' that handles the
;; communication of a client with a server kernel.  The job of the
;; `jupyter-server-kernel-comm' is to connect to the `jupyter-server's event
;; stream and filter the messages to handle those of a particular kernel
;; identified by kernel ID.
;;
;; Starting REPLs
;;
;; You can launch kernels without connecting clients to them by using
;; `jupyter-server-launch-kernel'.  To connect a REPL to a launched kernel use
;; `jupyter-connect-server-repl'.  To both launch and connect a REPL use
;; `jupyter-run-server-repl'.  All of the previous commands determine the server
;; to use by using the `jupyter-current-server' function, which see.
;;
;; Managing kernels on a server
;;
;; To get an overview of all live kernels on a server you can call
;; `jupyter-server-list-kernels'.  From the buffer displayed there are a number
;; of keys bound that enable you to manage the kernels on the server.  See
;; `jupyter-server-kernel-list-mode-map'.
;;
;; TODO: Find where it would be appropriate to call `delete-instance' on a
;;`jupyter-server' that does not have any websockets open, clients connected,
;; or HTTP connections open, or is not bound to `jupyter-current-server' in any
;; buffer.

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'jupyter-repl)
(require 'jupyter-rest-api)
(require 'jupyter-kernel-manager)
(require 'jupyter-ioloop-comm)
(require 'jupyter-server-ioloop)

(declare-function jupyter-tramp-file-name-p "jupyter-tramp" (filename))
(declare-function jupyter-tramp-server-from-file-name "jupyter-tramp" (filename))
(declare-function jupyter-tramp-file-name-from-url "jupyter-tramp" (url))

(defgroup jupyter-server nil
  "Support for the Jupyter kernel gateway"
  :group 'jupyter)

(defvar-local jupyter-current-server nil
  "The `jupyter-server' associated with the current buffer.
Used in, e.g. a `jupyter-server-kernel-list-mode' buffer.")

(put 'jupyter-current-server 'permanent-local t)

;;; `jupyter-server'

(defvar jupyter--servers nil)

;; TODO: We should really rename `jupyter-server' to something like
;; `jupyter-server-client' since it isn't a representation of a server, but a
;; communication channel with one.
(defclass jupyter-server (jupyter-rest-client
                          jupyter-ioloop-comm
                          eieio-instance-tracker)
  ((tracking-symbol :initform 'jupyter--servers)
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
      (when (jupyter-comm-alive-p server)
        (jupyter-comm-stop server))
      (jupyter-api-delete-cookies (oref server url))
      (delete-instance server))))

;;; `jupyter-server-kernel'

;; TODO: Add the server as a slot
(defclass jupyter-server-kernel (jupyter-kernel)
  ((server
    :type jupyter-server
    :initarg :server
    :documentation "The kernel server.")
   (id
    :type string
    :initarg :id
    :documentation "The kernel ID.")))

(cl-defmethod jupyter-kernel-alive-p ((kernel jupyter-server-kernel))
  (and (slot-boundp kernel 'id)
       (slot-boundp kernel 'server)
       ;; TODO: Cache this call
       (condition-case err
           (jupyter-api-get-kernel (oref kernel server) (oref kernel id))
         (file-error nil)               ; Non-existent server
         (jupyter-api-http-error
          (unless (= (nth 1 err) 404)    ; Not Found
            (signal (car err) (cdr err)))))))

(cl-defmethod jupyter-start-kernel ((kernel jupyter-server-kernel) &rest _ignore)
  (with-slots (server spec) kernel
    (jupyter-server--verify-kernelspec server spec)
    (cl-destructuring-bind (&key id &allow-other-keys)
        (jupyter-api-start-kernel server (car spec))
      (oset kernel id id))))

(cl-defmethod jupyter-kill-kernel ((_kernel jupyter-server-kernel))
  ;; The notebook server already takes care of forcing shutdown of a kernel.
  (ignore))

(defclass jupyter-server-kernel-comm (jupyter-comm-layer)
  ((kernel :type jupyter-server-kernel :initarg :kernel)))

(cl-defmethod jupyter-comm-id ((comm jupyter-server-kernel-comm))
  (let* ((kernel (oref comm kernel))
         (id (oref kernel id)))
    (or (jupyter-server-kernel-name (oref kernel server) id)
        (format "kid=%s" (truncate-string-to-width id 9 nil nil "…")))))

;;; Assigning names to kernel IDs

(defvar jupyter-server-kernel-names nil
  "An alist mapping URLs to alists mapping kernel IDs to human friendly names.
For example

    \((\"http://localhost:8888\"
      (\"72d92ded-1275-440a-852f-90f655197305\" . \"thermo\"))\)

You can persist this alist across Emacs sessions using `desktop',
`savehist', or any other session persistence package.  For
example, when using `savehist' you can add the following to your
init file to persist the server names across Emacs sessions.

    \(savehist-mode\)
    \(add-to-list 'savehist-additional-variables 'jupyter-server-kernel-names\).")

(defun jupyter-server-cull-kernel-names (&optional server)
  "Ensure all names in `jupyter-server-kernel-names' map to existing kernels.
If SERVER is non-nil only check the kernels on SERVER, otherwise
check all kernels on all existing servers."
  (let ((servers (if server (list server)
                   (jupyter-gc-servers)
                   (jupyter-servers))))
    (unless server
      ;; Only remove non-existing servers when culling all kernels on all
      ;; servers.
      (let ((urls (mapcar (lambda (x) (oref x url)) servers)))
        (cl-callf2 cl-remove-if-not (lambda (x) (member (car x) urls))
                   jupyter-server-kernel-names)))
    (dolist (server servers)
      (when-let* ((names (assoc (oref server url) jupyter-server-kernel-names)))
        (setf (alist-get (oref server url)
                         jupyter-server-kernel-names nil nil #'equal)
              (cl-loop
               for kernel across (jupyter-api-get-kernel server)
               for name = (assoc (plist-get kernel :id) names)
               when name collect name))))))

(defun jupyter-server-kernel-name (server id)
  "Return the associated name of the kernel with ID on SERVER.
If there is no name associated, return nil.  See
`jupyter-server-kernel-names'."
  (cl-check-type server jupyter-server)
  (let ((kernel-names (assoc (oref server url) jupyter-server-kernel-names)))
    (cdr (assoc id kernel-names))))

(defun jupyter-server-kernel-id-from-name (server name)
  "Return the ID of the kernel that has NAME on SERVER.
If NAME does not have a kernel associated, return nil.  See
`jupyter-server-kernel-names'."
  (cl-check-type server jupyter-server)
  (jupyter-server-cull-kernel-names server)
  (let ((kernel-names (assoc (oref server url) jupyter-server-kernel-names)))
    (car (rassoc name kernel-names))))

(defun jupyter-server-name-kernel (server id name)
  "NAME the kernel with ID on SERVER.
See `jupyter-server-kernel-names'."
  (cl-check-type server jupyter-server)
  (setf (alist-get id
                   (alist-get (oref server url)
                              jupyter-server-kernel-names
                              nil nil #'equal)
                   nil nil #'equal)
        name))

(defun jupyter-server-name-client-kernel (client name)
  "For the kernel connected to CLIENT associate NAME.
CLIENT must be communicating with a `jupyter-server-kernel', the
ID of the kernel will be associated with NAME, see
`jupyter-server-kernel-names'."
  (cl-check-type client jupyter-kernel-client)
  (cl-check-type (oref client kcomm) jupyter-server-kernel-comm)
  (let* ((kernel (thread-first client
                   (oref kcomm)
                   (oref kernel)))
         (id (oref kernel id)))
    (jupyter-server-name-kernel (oref kernel server) id name)))

;;; Plumbing

;;;; `jupyter-server' events

(cl-defmethod jupyter-event-handler ((comm jupyter-server)
                                     (event (head disconnect-channels)))
  (let ((kernel-id (cadr event)))
    (with-slots (ioloop) comm
      (cl-callf2 remove kernel-id
                 (process-get (oref ioloop process) :kernel-ids)))))

(cl-defmethod jupyter-event-handler ((comm jupyter-server)
                                     (event (head connect-channels)))
  (let ((kernel-id (cadr event)))
    (with-slots (ioloop) comm
      (cl-callf append (process-get (oref ioloop process) :kernel-ids)
        (list kernel-id)))))

(cl-defmethod jupyter-event-handler ((comm jupyter-server) event)
  "Send EVENT to all clients connected to COMM.
Each client must have a KERNEL slot which, in turn, must have an
ID slot.  The second element of EVENT is expected to be a kernel
ID.  Send EVENT, with the kernel ID excluded, to a client whose
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

;;;; `jupyter-server' methods

(defun jupyter-server--connect-channels (server id)
  (jupyter-send server 'connect-channels id)
  (jupyter-with-timeout
      (nil jupyter-default-timeout
           (error "Timeout when connecting websocket to kernel id %s" id))
    (jupyter-server-kernel-connected-p server id)))

(defun jupyter-server--refresh-comm (server)
  "Stop and then start SERVER communication.
Reconnect the previously connected kernels when starting."
  (when (jupyter-comm-alive-p server)
    (let ((connected (cl-remove-if-not
                      (apply-partially #'jupyter-server-kernel-connected-p server)
                      (mapcar (lambda (kernel) (plist-get kernel :id))
                         (jupyter-api-get-kernel server)))))
      (jupyter-comm-stop server)
      (jupyter-comm-start server)
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

(cl-defmethod jupyter-comm-start ((comm jupyter-server))
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

(cl-defmethod jupyter-comm-add-handler ((comm jupyter-server)
                                      (kcomm jupyter-server-kernel-comm))
  (cl-call-next-method)
  (with-slots (id) (oref kcomm kernel)
    (unless (jupyter-server-kernel-connected-p comm id)
      (jupyter-server--connect-channels comm id))))

(cl-defmethod jupyter-comm-remove-handler ((comm jupyter-server)
                                         (kcomm jupyter-server-kernel-comm))
  (with-slots (id) (oref kcomm kernel)
    (when (jupyter-server-kernel-connected-p comm id)
      (jupyter-send comm 'disconnect-channels id)
      (unless (jupyter-ioloop-wait-until (oref comm ioloop)
                  'disconnect-channels #'identity)
        (error "Timeout when disconnecting websocket for kernel id %s" id))))
  (cl-call-next-method))

(cl-defmethod jupyter-server-kernel-connected-p ((comm jupyter-server) id)
  "Return non-nil if COMM has a WebSocket connection to a kernel with ID."
  (and (jupyter-comm-alive-p comm)
       (member id (process-get (oref (oref comm ioloop) process) :kernel-ids))))

(defun jupyter-server--verify-kernelspec (server spec)
  (cl-destructuring-bind (name _ . kspec) spec
    (let ((server-spec (assoc name (jupyter-server-kernelspecs server))))
      (unless server-spec
        (error "No kernelspec matching %s on server @ %s"
               name (oref server url)))
      (when (cl-loop
             with sspec = (cddr server-spec)
             for (k v) on sspec by #'cddr
             thereis (not (equal (plist-get kspec k) v)))
        (error "%s kernelspec doesn't match one on server @ %s"
               name (oref server url))))))

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
                  ;; Uses the same format as `jupyter-available-kernelspecs'
                  ;;     (name dir . spec)
                  collect (cons (plist-get spec :name)
                                (cons nil (plist-get spec :spec)))))))
  (plist-get (oref server kernelspecs) :kernelspecs))

(cl-defmethod jupyter-server-has-kernelspec-p ((server jupyter-server) name)
  "Return non-nil if SERVER can launch kernels with kernelspec NAME."
  (jupyter-guess-kernelspec name (jupyter-server-kernelspecs server)))

;;;; `jupyter-server-kernel-comm' methods

(cl-defmethod jupyter-comm-start ((comm jupyter-server-kernel-comm) &rest _ignore)
  "Register COMM to receive server events.
If SERVER receives events that have the same kernel ID as the
kernel associated with COMM, then COMM's `jupyter-event-handler'
will receive those events."
  (with-slots (server) (oref comm kernel)
    (jupyter-comm-start server)
    (jupyter-comm-add-handler server comm)))

(cl-defmethod jupyter-comm-stop ((comm jupyter-server-kernel-comm) &rest _ignore)
  "Disconnect COMM from receiving server events."
  (jupyter-comm-remove-handler (oref (oref comm kernel) server) comm))

(cl-defmethod jupyter-send ((comm jupyter-server-kernel-comm) event-type &rest event)
  "Use COMM to send an EVENT to the server with type, EVENT-TYPE.
SERVER will direct EVENT to the right kernel based on the kernel
ID of the kernel associated with COMM."
  (with-slots (kernel) comm
    (unless (jupyter-comm-alive-p comm)
      (jupyter-comm-start comm))
    (apply #'jupyter-send (oref kernel server) event-type (oref kernel id) event)))

(cl-defmethod jupyter-comm-alive-p ((comm jupyter-server-kernel-comm))
  "Return non-nil if COMM can receive server events for its associated kernel."
  (with-slots (kernel) comm
    (and (jupyter-server-kernel-connected-p
          (oref kernel server)
          (oref kernel id))
         (catch 'member
           (jupyter-comm-handler-loop (oref kernel server) client
             (when (eq client comm)
               (throw 'member t)))))))

;; TODO: Remove the need for these methods, they are remnants from an older
;; implementation.  They will need to be removed from `jupyter-kernel-client'.
(cl-defmethod jupyter-channel-alive-p ((comm jupyter-server-kernel-comm) _channel)
  (jupyter-comm-alive-p comm))

;;;; `jupyter-server-kernel-manager'

(defclass jupyter-server-kernel-manager (jupyter-kernel-manager)
  ((comm :type jupyter-server-kernel-comm)))

(cl-defmethod jupyter-comm-start ((manager jupyter-server-kernel-manager))
  "Start a websocket connection to MANAGER's kernel.
MANAGER's COMM slot will be set to the `jupyter-comm-layer'
receiving events on the websocket when this method returns."
  (with-slots (kernel comm) manager
    (unless (slot-boundp manager 'comm)
      (oset manager comm (jupyter-server-kernel-comm :kernel kernel)))
    (unless (jupyter-comm-alive-p comm)
      (jupyter-comm-start comm))))

(cl-defmethod jupyter-comm-stop ((manager jupyter-server-kernel-manager))
  "Stop a websocket connection to MANAGER's kernel."
  (when (slot-boundp manager 'comm)
    (with-slots (comm) manager
      (when (jupyter-comm-alive-p comm)
        (jupyter-comm-stop comm)))))

(cl-defmethod jupyter-kernel-alive-p ((manager jupyter-server-kernel-manager))
  (jupyter-kernel-alive-p (oref manager kernel)))

(cl-defmethod jupyter-start-kernel ((manager jupyter-server-kernel-manager) &rest _ignore)
  "Ensure that the gateway can receive events from its kernel."
  (with-slots (kernel) manager
    (unless (jupyter-kernel-alive-p kernel)
      (jupyter-start-kernel kernel))
    (jupyter-comm-start manager)))

(cl-defmethod jupyter-interrupt-kernel ((manager jupyter-server-kernel-manager))
  (with-slots (kernel) manager
    (jupyter-api-interrupt-kernel (oref kernel server) (oref kernel id))))

(cl-defmethod jupyter-shutdown-kernel ((manager jupyter-server-kernel-manager) &optional restart _timeout)
  (with-slots (kernel) manager
    (let ((server (oref kernel server)))
      (if restart (jupyter-api-restart-kernel server (oref kernel id))
        (jupyter-comm-stop manager)
        (when (jupyter-kernel-alive-p manager)
          (jupyter-api-shutdown-kernel server (oref kernel id)))))))

(cl-defmethod jupyter-make-client ((manager jupyter-server-kernel-manager) _class &rest _slots)
  (let ((client (cl-call-next-method)))
    (prog1 client
      (jupyter-comm-start manager)
      (oset client kcomm (oref manager comm)))))

;;; Finding exisisting kernel managers and servers

(defun jupyter-server-find-manager (server id)
  "Return a kernel manager managing kernel with ID on SERVER.
Return nil if none could be found."
  (cl-loop
   for manager in (jupyter-kernel-managers)
   thereis (and (cl-typep manager 'jupyter-server-kernel-manager)
                (with-slots (kernel) manager
                  (and (eq (oref kernel server) server)
                       (equal (oref kernel id) id)))
                manager)))

(defun jupyter-find-server (url &optional ws-url)
  "Return a live `jupyter-server' that lives at URL.
Finds a server that matches both URL and WS-URL.  When WS-URL the
default set by `jupyter-rest-client' is used.

Return nil if no `jupyter-server' could be found."
  (with-slots (url ws-url)
      (apply #'make-instance 'jupyter-rest-client
             (append (list :url url)
                     (when ws-url (list :ws-url ws-url))))
    (cl-loop for server in (jupyter-servers)
             thereis (and (equal (oref server url) url)
                          (equal (oref server ws-url) ws-url)
                          server))))

;;; Helpers for commands

(defun jupyter-completing-read-server-kernel (server)
  "Use `completing-read' to select a kernel on SERVER.
A model of the kernel is returned as a property list and has at
least the following keys:

- :id :: The ID used to identify the kernel on the server
- :last_activity :: The last channel activity of the kernel
- :name :: The kernelspec name used to start the kernel
- :execution_state :: The status of the kernel
- :connections :: The number of websocket connections for the kernel"
  (let* ((kernels (jupyter-api-get-kernel server))
         (display-names
          (if (null kernels) (error "No kernels @ %s" (oref server url))
            (mapcar (lambda (k)
                 (cl-destructuring-bind
                     (&key name id last_activity &allow-other-keys) k
                   (concat name " (last activity: " last_activity ", id: " id ")")))
               kernels)))
         (name (completing-read "kernel: " display-names nil t)))
    (when (equal name "")
      (error "No kernel selected"))
    (nth (- (length display-names)
            (length (member name display-names)))
         (append kernels nil))))

(define-error 'jupyter-server-non-existent
  "The server doesn't exist")

(defun jupyter-current-server (&optional ask)
  "Return an existing `jupyter-server' or ASK for a new one.
If ASK is non-nil, always ask for a URL and return the
`jupyter-server' object corresponding to it.  If no Jupyter server
at URL exists, `signal' a `jupyter-server-non-existent' error
with error data being URL.

If the buffer local value of `jupyter-current-server' is non-nil,
return its value.  If `jupyter-current-server' is nil and the
`jupyter-current-client' is communicating with a kernel behind a
kernel server, return the `jupyter-server' managing the
connection.

If `jupyter-current-client' is nil or not communicating with a
kernel behind a server and if `default-directory' is a Jupyter
remote file name, return the `jupyter-server' object
corresponding to that connection.

If all of the above fails, either return the most recently used
`jupyter-server' object if there is one or ask for one based off
a URL."
  (interactive "P")
  (let ((read-url-make-server
         (lambda ()
           ;; From the list of available server
           ;; (if (> (length jupyter--servers) 1)
           ;;     (let ((server (cdr (completing-read
           ;;                         "Jupyter Server: "
           ;;                         (mapcar (lambda (x) (cons (oref x url) x))
           ;;                            jupyter--servers)))))
           ;;   )
           (jupyter-gc-servers)
           (let* ((url (read-string "Server URL: " "http://localhost:8888"))
                  (ws-url (read-string "Websocket URL: "
                                       (let ((u (url-generic-parse-url url)))
                                         (setf (url-type u) "ws")
                                         (url-recreate-url u)))))
             (or (jupyter-find-server url ws-url)
                 (let ((server (jupyter-server :url url :ws-url ws-url)))
                   (if (jupyter-api-server-exists-p server) server
                     (delete-instance server)
                     (signal 'jupyter-server-non-existent (list url)))))))))
    (let ((server
           (if ask (funcall read-url-make-server)
             (cond
              (jupyter-current-server)
              ;; Server of the current kernel client
              ((and jupyter-current-client
                    (object-of-class-p
                     (oref jupyter-current-client kcomm)
                     'jupyter-server-kernel-comm)
                    (thread-first jupyter-current-client
                      (oref kcomm)
                      (oref kernel)
                      (oref server))))
              ;; Server of the current TRAMP remote context
              ((and (file-remote-p default-directory)
                    (jupyter-tramp-file-name-p default-directory)
                    (jupyter-tramp-server-from-file-name default-directory)))
              ;; Most recently accessed
              (t
               (or (car jupyter--servers)
                   (funcall read-url-make-server)))))))
      (prog1 server
        (setq jupyter--servers
              (cons server (delq server jupyter--servers)))))))

;;; Commands

;;;###autoload
(defun jupyter-server-launch-kernel (server)
  "Start a kernel on SERVER.

With a prefix argument, ask to select a server if there are
mutiple to choose from, otherwise the most recently used server
is used as determined by `jupyter-current-server'."
  (interactive (list (jupyter-current-server current-prefix-arg)))
  (let* ((specs (jupyter-server-kernelspecs server))
         (spec (jupyter-completing-read-kernelspec specs)))
    (jupyter-api-start-kernel server (car spec))))

;;; REPL

;; TODO: When closing the REPL buffer and it is the last connected client as
;; shown by the :connections key of a `jupyter-api-get-kernel' call, ask to
;; also shutdown the kernel.
;;
;; TODO: When calling `jupyter-stop-channels' and there is only one client to a
;; `jupyter-server-kernel-comm', tell the `jupyter-server-ioloop' to disconnect
;; the channels.
(defun jupyter-server-start-new-kernel (server kernel-name &optional client-class)
  "Start a managed Jupyter kernel on SERVER.
KERNEL-NAME is the name of the kernel to start.  It can also be
the prefix of a valid kernel name, in which case the first kernel
in ‘jupyter-server-kernelspecs’ that has KERNEL-NAME as a
prefix will be used.

Optional argument CLIENT-CLASS is a subclass
of ‘jupyer-kernel-client’ and will be used to initialize a new
client connected to the kernel.  CLIENT-CLASS defaults to the
symbol ‘jupyter-kernel-client’.

Return a list (KM KC) where KM is the kernel manager managing the
lifetime of the kernel on SERVER.  KC is a new client connected to
the kernel whose class is CLIENT-CLASS.  Note that the client’s
‘manager’ slot will also be set to the kernel manager instance,
see ‘jupyter-make-client’."
  (or client-class (setq client-class 'jupyter-kernel-client))
  (let* ((specs (jupyter-server-kernelspecs server))
         (kernel (jupyter-server-kernel
                  :server server
                  :spec (jupyter-guess-kernelspec kernel-name specs)))
         (manager (jupyter-server-kernel-manager :kernel kernel)))
    ;; Needs to be started before calling `jupyter-make-client' since that
    ;; method will send a request to start a websocket channel to the kernel.
    ;; FIXME: This should be done in a `jupyter-comm-initialize' method,
    ;; but first that method needs to be generalize in `jupyter-client.el'
    (unless (jupyter-kernel-alive-p manager)
      (jupyter-start-kernel manager))
    (let ((client (jupyter-make-client manager client-class)))
      (jupyter-start-channels client)
      (list manager client))))

;;;###autoload
(defun jupyter-run-server-repl
    (server kernel-name &optional repl-name associate-buffer client-class display)
  "On SERVER start a kernel with KERNEL-NAME.

With a prefix argument, ask to select a server if there are
mutiple to choose from, otherwise the most recently used server
is used as determined by `jupyter-current-server'.

REPL-NAME, ASSOCIATE-BUFFER, CLIENT-CLASS, and DISPLAY all have
the same meaning as in `jupyter-run-repl'."
  (interactive
   (let ((server (jupyter-current-server current-prefix-arg)))
     (list server
           (car (jupyter-completing-read-kernelspec
                 (jupyter-server-kernelspecs server)))
           ;; FIXME: Ambiguity with `jupyter-current-server' and
           ;; `current-prefix-arg'
           (when (and current-prefix-arg
                      (y-or-n-p "Name REPL? "))
             (read-string "REPL Name: "))
           t nil t)))
  (or client-class (setq client-class 'jupyter-repl-client))
  (jupyter-error-if-not-client-class-p client-class 'jupyter-repl-client)
  (cl-destructuring-bind (_manager client)
      (jupyter-server-start-new-kernel server kernel-name client-class)
    (jupyter-bootstrap-repl client repl-name associate-buffer display)))

;;;###autoload
(defun jupyter-connect-server-repl
    (server kernel-id &optional repl-name associate-buffer client-class display)
  "On SERVER, connect to the kernel with KERNEL-ID.

With a prefix argument, ask to select a server if there are
mutiple to choose from, otherwise the most recently used server
is used as determined by `jupyter-current-server'.

REPL-NAME, ASSOCIATE-BUFFER, CLIENT-CLASS, and DISPLAY all have
the same meaning as in `jupyter-connect-repl'."
  (interactive
   (let ((server (jupyter-current-server current-prefix-arg)))
     (list server
           (plist-get (jupyter-completing-read-server-kernel server) :id)
           ;; FIXME: Ambiguity with `jupyter-current-server' and
           ;; `current-prefix-arg'
           (when (and current-prefix-arg
                      (y-or-n-p "Name REPL? "))
             (read-string "REPL Name: "))
           t nil t)))
  (or client-class (setq client-class 'jupyter-repl-client))
  (jupyter-error-if-not-client-class-p client-class 'jupyter-repl-client)
  (let* ((specs (jupyter-server-kernelspecs server))
         (manager
          (or (jupyter-server-find-manager server kernel-id)
              (let ((model (jupyter-api-get-kernel server kernel-id)))
                (jupyter-server-kernel-manager
                 :kernel (jupyter-server-kernel
                          :id kernel-id
                          :server server
                          :spec (assoc (plist-get model :name) specs))))))
         (client (jupyter-make-client manager client-class)))
    (jupyter-start-channels client)
    (jupyter-bootstrap-repl client repl-name associate-buffer display)))

;;; `jupyter-server-kernel-list'

(defun jupyter-server-kernel-list-do-shutdown ()
  "Shutdown the kernel corresponding to the current entry."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (really (yes-or-no-p
                       (format "Really shutdown %s kernel? "
                               (aref (tabulated-list-get-entry) 0)))))
    (let ((manager (jupyter-server-find-manager jupyter-current-server id)))
      (if manager (jupyter-shutdown-kernel manager)
        (jupyter-api-shutdown-kernel jupyter-current-server id)))
    (tabulated-list-delete-entry)))

(defun jupyter-server-kernel-list-do-restart ()
  "Restart the kernel corresponding to the current entry."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (really (yes-or-no-p "Really restart kernel? ")))
    (let ((manager (jupyter-server-find-manager jupyter-current-server id)))
      (if manager (jupyter-shutdown-kernel manager 'restart)
        (jupyter-api-restart-kernel jupyter-current-server id)))
    (revert-buffer)))

(defun jupyter-server-kernel-list-do-interrupt ()
  "Interrupt the kernel corresponding to the current entry."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (jupyter-api-interrupt-kernel jupyter-current-server id)
    (revert-buffer)))

(defun jupyter-server-kernel-list-new-repl ()
  "Connect a REPL to the kernel corresponding to the current entry."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (let ((jupyter-current-client
           (jupyter-connect-server-repl jupyter-current-server id)))
      (revert-buffer)
      (jupyter-repl-pop-to-buffer))))

(defun jupyter-server-kernel-list-launch-kernel ()
  "Launch a new kernel on the server."
  (interactive)
  (jupyter-server-launch-kernel jupyter-current-server)
  (revert-buffer))

(defun jupyter-server-kernel-list-name-kernel ()
  "Name the kernel under `point'."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (name (read-string
                     (let ((cname (jupyter-server-kernel-name
                                   jupyter-current-server id)))
                       (if cname (format "Rename %s to: " cname)
                         (format "Name kernel [%s]: " id))))))
    (when (zerop (length name))
      (jupyter-server-kernel-list-name-kernel))
    (jupyter-server-name-kernel jupyter-current-server id name)
    (revert-buffer)))

(defvar jupyter-server-kernel-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-i") #'jupyter-server-kernel-list-do-interrupt)
    (define-key map (kbd "d") #'jupyter-server-kernel-list-do-shutdown)
    (define-key map (kbd "C-c C-d") #'jupyter-server-kernel-list-do-shutdown)
    (define-key map (kbd "C-c C-r") #'jupyter-server-kernel-list-do-restart)
    (define-key map [follow-link] nil) ;; allows mouse-1 to be activated
    (define-key map [mouse-1] #'jupyter-server-kernel-list-new-repl)
    (define-key map (kbd "RET") #'jupyter-server-kernel-list-new-repl)
    (define-key map (kbd "C-RET") #'jupyter-server-kernel-list-launch-kernel)
    (define-key map (kbd "C-<return>") #'jupyter-server-kernel-list-launch-kernel)
    (define-key map (kbd "<return>") #'jupyter-server-kernel-list-new-repl)
    (define-key map "R" #'jupyter-server-kernel-list-name-kernel)
    (define-key map "r" #'revert-buffer)
    (define-key map "g" #'revert-buffer)
    map))

(define-derived-mode jupyter-server-kernel-list-mode
  tabulated-list-mode "Jupyter Server Kernels"
  "A list of live kernels on a Jupyter kernel server."
  (tabulated-list-init-header)
  (tabulated-list-print)
  (let ((inhibit-read-only t)
        (url (oref jupyter-current-server url)))
    (overlay-put
     (make-overlay 1 2)
     'before-string
     (concat (propertize url 'face '(fixed-pitch default)) "\n")))
  ;; So that `dired-jump' will visit the directory of the kernel server.
  (setq default-directory
        (jupyter-tramp-file-name-from-url
         (oref jupyter-current-server url))))

(defun jupyter-server--kernel-list-format ()
  (let* ((get-time
          (lambda (a)
            (or (get-text-property 0 'jupyter-time a)
                (let ((time (jupyter-decode-time a)))
                  (prog1 time
                    (put-text-property 0 1 'jupyter-time time a))))))
         (time-sort
          (lambda (a b)
            (time-less-p
             (funcall get-time (aref (nth 1 a) 2))
             (funcall get-time (aref (nth 1 b) 2)))))
         (conn-sort
          (lambda (a b)
            (< (string-to-number (aref (nth 1 a) 4))
               (string-to-number (aref (nth 1 b) 4))))))
    `[("Name" 17 t)
      ("ID" 38 nil)
      ("Activity" 20 ,time-sort)
      ("State" 10 nil)
      ("Conns." 6 ,conn-sort)]))

(defun jupyter-server--kernel-list-entries ()
  (cl-loop
   with names = nil
   for kernel across (jupyter-api-get-kernel jupyter-current-server)
   collect
   (cl-destructuring-bind
       (&key name id last_activity execution_state
             connections &allow-other-keys)
       kernel
     (let* ((time (jupyter-decode-time last_activity))
            (name (propertize
                   (or (jupyter-server-kernel-name jupyter-current-server id)
                       (let ((same (cl-remove-if-not
                                    (lambda (x) (string-prefix-p name x)) names)))
                         (when same (setq name (format "%s<%d>" name (length same))))
                         (push name names)
                         name))
                   'face 'font-lock-constant-face))
            (activity (propertize (jupyter-format-time-low-res time)
                                  'face 'font-lock-doc-face
                                  'jupyter-time time))
            (conns (propertize (number-to-string connections)
                               'face 'shadow))
            (state (propertize execution_state
                               'face (pcase execution_state
                                       ("busy" 'warning)
                                       ("idle" 'shadow)
                                       ("starting" 'success)))))
       (list id (vector name id activity state conns))))))

;;;###autoload
(defun jupyter-server-list-kernels (server)
  "Display a list of live kernels on SERVER.
When called interactively, ask to select a SERVER when given a
prefix argument otherwise the `jupyter-current-server' will be
used."
  (interactive (list (jupyter-current-server current-prefix-arg)))
  (if (zerop (length (jupyter-api-get-kernel server)))
      (when (yes-or-no-p (format "No kernels at %s; launch one? "
                                 (oref server url)))
        (jupyter-server-launch-kernel server)
        (jupyter-server-list-kernels server))
    (with-current-buffer
        (jupyter-get-buffer-create (format "kernels[%s]" (oref server url)))
      (setq jupyter-current-server server)
      (if (eq major-mode 'jupyter-server-kernel-list-mode)
          (revert-buffer)
        (setq tabulated-list-format (jupyter-server--kernel-list-format)
              tabulated-list-entries #'jupyter-server--kernel-list-entries
              tabulated-list-sort-key (cons "Activity" t))
        (jupyter-server-kernel-list-mode)
        ;; So that `dired-jump' will visit the directory of the kernel server.
        (setq default-directory
              (jupyter-tramp-file-name-from-url (oref server url))))
      (jupyter-display-current-buffer-reuse-window))))

(provide 'jupyter-server)

;;; jupyter-server.el ends here
