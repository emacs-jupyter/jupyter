;;; jupyter-base.el --- Core definitions for Jupyter -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 06 Jan 2018
;; Version: 0.6.0
;; Keywords: jupyter literate-programming

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
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

;; This file holds the core requires, variables, and type definitions necessary
;; for jupyter.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'eieio-base)
(require 'subr-x) ; `string-trim'
(require 'json)
(require 'zmq)
(require 'hmac-def)
(require 'jupyter-kernelspec)

(declare-function tramp-dissect-file-name "tramp" (name &optional nodefault))
(declare-function tramp-tramp-file-p "tramp" (name))
(declare-function tramp-file-name-user "tramp")
(declare-function tramp-file-name-host "tramp")
(declare-function jupyter-message-content "jupyter-messages" (msg))

;;; Custom variables

(defcustom jupyter-include-other-output nil
  "Whether or not to handle IOPub messages from other clients.
A Jupyter client can receive messages from other clients
connected to the same kernel on the IOPub channel. You can choose
to ignore these messages by setting
`jupyter-include-other-output' to nil. If
`jupyter-include-other-output' is non-nil, then any messages that
are not associated with a request from a client are sent to the
client's handler methods with a nil value for the request
argument. To change the value of this variable for a particular
client use `jupyter-set'."
  :group 'jupyter
  :type 'boolean)

(defcustom jupyter-iopub-message-hook nil
  "Hook run with one argument, a message received on the IOPub channel.
Do not add to this hook variable directly, use
`jupyter-add-hook'. If any of the message hooks return a non-nil
value, the client handlers will be prevented from running for the
message."
  :group 'jupyter
  :type 'hook)
(put 'jupyter-iopub-message-hook 'permanent-local t)

(defcustom jupyter-shell-message-hook nil
  "Hook run with one argument, a message received on the SHELL channel.
Do not add to this hook variable directly, use
`jupyter-add-hook'. If any of the message hooks return a non-nil
value, the client handlers will be prevented from running for the
message."
  :group 'jupyter
  :type 'hook)
(put 'jupyter-shell-message-hook 'permanent-local t)

(defcustom jupyter-stdin-message-hook nil
  "Hook run with one argument, a message received on the STDIN channel.
Do not add to this hook variable directly, use
`jupyter-add-hook'. If any of the message hooks return a non-nil
value, the client handlers will be prevented from running for the
message."
  :group 'jupyter
  :type 'hook)
(put 'jupyter-stdin-message-hook 'permanent-local t)

(defcustom jupyter-runtime-directory (string-trim-right
                                      (shell-command-to-string
                                       "jupyter --runtime-dir"))
  "The Jupyter runtime directory.
When a new kernel is started through `jupyter-start-kernel', this
directory is where kernel connection files are written to."
  :group 'jupyter
  :type 'string)

(defconst jupyter-root (file-name-directory load-file-name)
  "Root directory containing emacs-jupyter.")

(defconst jupyter-protocol-version "5.3"
  "The jupyter protocol version that is implemented.")

(defconst jupyter-socket-types
  (list :hb zmq-REQ
        :shell zmq-DEALER
        :iopub zmq-SUB
        :stdin zmq-DEALER
        :control zmq-DEALER)
  "The socket types for the various channels used by `jupyter'.")

(defconst jupyter-message-types
  (list :execute-result "execute_result"
        :execute-request "execute_request"
        :execute-reply "execute_reply"
        :inspect-request "inspect_request"
        :inspect-reply "inspect_reply"
        :complete-request "complete_request"
        :complete-reply "complete_reply"
        :history-request "history_request"
        :history-reply "history_reply"
        :is-complete-request "is_complete_request"
        :is-complete-reply "is_complete_reply"
        :comm-info-request "comm_info_request"
        :comm-info-reply "comm_info_reply"
        :comm-open "comm_open"
        :comm-msg "comm_msg"
        :comm-close "comm_close"
        :kernel-info-request "kernel_info_request"
        :kernel-info-reply "kernel_info_reply"
        :shutdown-request "shutdown_request"
        :shutdown-reply "shutdown_reply"
        :interupt-request "interrupt_request"
        :interrupt-reply "interrupt_reply"
        :stream "stream"
        :display-data "display_data"
        :update-display-data "update_display_data"
        :execute-input "execute_input"
        :error "error"
        :status "status"
        :clear-output "clear_output"
        :input-reply "input_reply"
        :input-request "input_request")
  "A plist mapping keywords to Jupyter message type strings.
The plist values are the message types either sent or received
from the kernel.")

(defconst jupyter-mime-types '(:application/vnd.jupyter.widget-view+json
                               :text/html :text/markdown
                               :image/svg+xml :image/jpeg :image/png
                               :text/latex :text/plain)
  "MIME types handled by Jupyter.")

(defconst jupyter-nongraphic-mime-types '(:application/vnd.jupyter.widget-view+json
                                          :text/html :text/markdown
                                          :text/plain)
  "MIME types that can be used in terminal Emacs.")

(defvar jupyter--debug nil
  "When non-nil, some parts of Jupyter will emit debug statements.")


(defvar jupyter-default-timeout 1
  "The default timeout in seconds for `jupyter-wait-until'.")

(defvar jupyter-long-timeout 10
  "A longer timeout that `jupyter-default-timeout' used for some operations.
A longer timeout is needed, for example, when retrieving the
`jupyter-kernel-info' to allow for the kernel to startup.")

;;; Macros

(defmacro jupyter-with-timeout (spec &rest wait-forms)
  "Periodically evaluate WAIT-FORMS until timeout.
Or until WAIT-FORMS evaluates to a non-nil value.

Wait until timeout SECONDS, periodically evaluating WAIT-FORMS
until it returns non-nil. If WAIT-FORMS returns non-nil, stop
waiting and return its value. Otherwise if timeout SECONDS
elapses, evaluate TIMEOUT-FORMS and return its value.

If PROGRESS is non-nil and evaluates to a string, a progress
reporter will be used with PROGRESS as the message while waiting.

SPEC takes the form (PROGRESS SECONDS TIMEOUT-FORMS...).

\(fn (PROGRESS SECONDS TIMEOUT-FORMS...) WAIT-FORMS...)"
  (declare (indent 1) (debug ((form form body) body)))
  (let ((res (make-symbol "res"))
        (prog (make-symbol "prog"))
        (prog-msg (make-symbol "prog-msg")))
    `(let* ((,res nil)
            (,prog-msg ,(pop spec))
            (,prog (and (stringp ,prog-msg)
                        (make-progress-reporter ,prog-msg))))
       (with-timeout (,(pop spec) ,@spec)
         (while (not (setq ,res (progn ,@wait-forms)))
           (accept-process-output nil 0.0001)
           (when ,prog (progress-reporter-update ,prog))))
       (prog1 ,res
         (when ,prog (progress-reporter-done ,prog))))))

(defmacro jupyter-with-insertion-bounds (beg end bodyform &rest afterforms)
  "Bind BEG and END to `point-marker's, evaluate BODYFORM then AFTERFORMS.
The END marker will advance if BODYFORM inserts text in the
current buffer. Thus after BODYFORM is evaluated, AFTERFORMS will
have access to the bounds of the text inserted by BODYFORM in the
variables BEG and END. The result of evaluating BODYFORM is
returned."
  (declare (indent 3) (debug (symbolp symbolp form body)))
  `(let ((,beg (point-marker))
         (,end (point-marker)))
     (set-marker-insertion-type end t)
     (unwind-protect
         (prog1 ,bodyform ,@afterforms)
       (set-marker ,beg nil)
       (set-marker ,end nil))))

(defmacro jupyter-loop-over-mime (mime-order mime data metadata &rest bodyforms)
  "Loop over MIME types in MIME-ORDER.
MIME-ORDER should evaluate to a list of MIME types to loop over.

MIME will be bound to the MIME type for the current iteration.
DATA and METADATA are variables that hold the property list of
MIME data to loop over and any associated metadata, respectively.

Evaluate BODYFORMS with DATA and METADATA temporarily bound to
the data and metadata of the MIME type for the current iteration.
If BODYFORMS returns non-nil, return its value. Otherwise loop
over the next MIME type in MIME-ORDER that has a non-nil value in
the DATA property list."
  (declare (indent 4) (debug ([&or form symbolp listp]
                              symbolp symbolp symbolp body)))
  `(cl-loop
    for ,mime in ,mime-order
    thereis (let ((,data (plist-get ,data ,mime))
                  (,metadata (plist-get ,metadata ,mime)))
              (when ,data ,@bodyforms))))

;;;; Display buffers

(defvar-local jupyter-display-buffer-marker nil
  "The marker to store the last output position of an output buffer.
See `jupyter-with-display-buffer'.")

(defvar-local jupyter-display-buffer-request-id nil
  "The last `jupyter-request' message ID that generated output.")

(defun jupyter-get-buffer-create (name)
  "Return a buffer with some special properties.

- The buffer's name is based on NAME, specifically it will be
  \"*jupyter-NAME*\"

- Its `major-mode' will be `special-mode'."
  (let* ((bname (format "*jupyter-%s*" name))
         (buffer (get-buffer bname)))
    (unless buffer
      (setq buffer (get-buffer-create bname))
      (with-current-buffer buffer
        (unless (eq major-mode 'special-mode)
          (special-mode))))
    buffer))

(defun jupyter--reset-display-buffer-p (arg)
  "Return non-nil if the current output buffer should be reset.
If ARG is a `jupyter-request', reset the buffer if ARG's
`jupyter-request-id' is no equal to the
`jupyter-buffer-last-request-id'. If ARG is not a
`jupyter-request-id', return ARG."
  (if (jupyter-request-p arg)
      ;; Reset the output buffer is the last request ID does not
      ;; match the current request's ID.
      (let ((id (jupyter-request-id arg)))
        (and (not (equal id jupyter-display-buffer-request-id))
             (setq jupyter-display-buffer-request-id id)
             t))
    ;; Otherwise reset the output buffer if RESET evaluates to a
    ;; non-nil value
    arg))

(defmacro jupyter-with-display-buffer (name reset &rest body)
  "With the REPL output buffer corresponding to NAME, run BODY.
The buffer corresponding to NAME will be obtained by a call to
`jupyter-get-buffer-create'. An output buffer differs from a
documentation buffer by maintaining its previous output and
moving `point' to the end of the last output.

RESET should be a form or symbol to determine if the output
buffer should be reset before evaluating BODY. If RESET is nil,
no reset is ever performed. If RESET evaluates to a
`jupyter-request' object, reset the buffer if the previous
request that generated output in the buffer is not the same
request. Otherwise if RESET evaluates to any non-nil value, reset
the output buffer."
  (declare (indent 2) (debug (stringp [&or atom form] body)))
  (let ((buffer (make-symbol "buffer")))
    `(let ((,buffer (jupyter-get-buffer-create ,name)))
       (setq other-window-scroll-buffer ,buffer)
       (with-current-buffer ,buffer
         (let ((inhibit-read-only t))
           (when (jupyter--reset-display-buffer-p ,reset)
             (erase-buffer)
             (if jupyter-display-buffer-marker
                 (set-marker jupyter-display-buffer-marker (point))
               (setq jupyter-display-buffer-marker (point-marker))))
           (goto-char jupyter-display-buffer-marker)
           (jupyter-with-control-code-handling ,@body)
           (set-marker jupyter-display-buffer-marker (point)))))))

;;; Signing functions/UUID

(defun jupyter-sha256 (object)
  "Return the SHA256 hash of OBJECT."
  (secure-hash 'sha256 object nil nil t))

(define-hmac-function jupyter-hmac-sha256 jupyter-sha256 64 32)

(defun jupyter-new-uuid ()
  "Return a version 4 UUID."
  (format "%04x%04x-%04x-%04x-%04x-%06x%06x"
          (cl-random 65536)
          (cl-random 65536)
          (cl-random 65536)
          ;; https://tools.ietf.org/html/rfc4122
          (let ((r (cl-random 65536)))
            (if (= (byteorder) ?l)
                ;; ?l = little-endian
                (logior (logand r 4095) 16384)
              ;; big-endian
              (logior (logand r 65295) 64)))
          (let ((r (cl-random 65536)))
            (if (= (byteorder) ?l)
                (logior (logand r 49151) 32768)
              (logior (logand r 65471) 128)))
          (cl-random 16777216)
          (cl-random 16777216)))

(defclass jupyter-instance-tracker ()
  ((tracking-symbol :type symbol))
  :documentation "Similar to `eieio-instance-tracker', but keeping weak references.
To access all the objects in TRACKING-SYMBOL, use
`jupyter-all-objects'."
  :abstract t)

(cl-defmethod initialize-instance ((obj jupyter-instance-tracker) &rest _)
  (cl-call-next-method)
  (let ((sym (oref obj tracking-symbol)))
    (unless (hash-table-p (symbol-value sym))
      (put sym 'jupyter-instance-tracker t)
      (set sym (make-hash-table :weakness 'key)))
    (puthash obj t (symbol-value sym))))

(defun jupyter-all-objects (sym)
  "Return all tracked objects in tracking SYM.
SYM is a symbol used for tracking objects that inherit from the class
`jupyter-instance-tracker'."
  (let ((table (symbol-value sym)))
    (when (hash-table-p table)
      (cl-assert (get sym 'jupyter-instance-tracker) t)
      (hash-table-keys table))))

(defclass jupyter-finalized-object ()
  ((finalizers :type list :initform nil))
  :documentation "A list of finalizers."
  :documentation "A base class for cleaning up resources.
Adds the method `jupyter-add-finalizer' which maintains a list of
finalizer functions to be called when the object is garbage
collected.")

(cl-defgeneric jupyter-add-finalizer ((obj jupyter-finalized-object) finalizer)
  "Cleanup resources automatically.
FINALIZER if a function to be added to a list of finalizers that
will be called when OBJ is garbage collected."
  (declare (indent 1))
  (cl-check-type finalizer function)
  (push (make-finalizer finalizer) (oref obj finalizers)))

;;; Session object definition

(cl-defstruct (jupyter-session
               (:constructor nil)
               (:constructor
                jupyter-session
                (&key
                 (conn-info nil)
                 (id (jupyter-new-uuid))
                 (key nil))))
  "A `jupyter-session' holds the information needed to
authenticate messages. A `jupyter-session' contains the following
fields:

- CONN-INFO :: The connection info. property list of the kernel
  this session is used to sign messages for.

- ID :: A string of bytes used as the `zmq-ROUTING-ID' for every
  `jupyter-channel' that utilizes the sessiong object.

- KEY :: The key used when signing messages. If KEY is nil,
  message signing is not performed."
  (conn-info nil :read-only t)
  (id nil :read-only t)
  (key nil :read-only t))

;;; Request object definition

(cl-defstruct (jupyter-request
               (:constructor nil)
               (:constructor jupyter-request))
  "A `jupyter-request' encapsulates the current status of a
request to a kernel. A `jupyter-request' consists of the
following fields:

- ID :: A UUID to match a `jupyter-request' to the received
        messages of a kernel.

- TIME :: The time at which the request was made.

- IDLE-RECEIVED-P :: A flag variable that is set to t when a
                    `jupyter-kernel-client' has received the
                    status: idle message for the request.

- LAST-MESSAGE :: The raw message property list of the last
                  message received by the kernel in response to
                  this request.

- INHIBITED-HANDLERS :: A list of handler message types to
                        prevent the running of that particular
                        handler. If set to t, disable all
                        handlers for this request. Note this
                        should not be set directly, dynamically
                        bind `jupyter-inhibit-handlers' before
                        making the request.

- CALLBACKS :: An alist mapping message types to their
               corresponding callbacks. This alist is modified
               through calls to `jupyter-add-callback' on the request."
  (id "")
  (time (current-time))
  (idle-received-p nil)
  (last-message nil)
  (inhibited-handlers nil)
  (callbacks))

;;; Connecting to a kernel's channels

(defun jupyter-tunnel-connection (conn-file &optional server)
  "Forward local ports to the remote ports in CONN-FILE.
CONN-FILE is the path to a Jupyter connection file, SERVER is the
host that the kernel connection in CONN-FILE is located. Return a
copy of the connection plist in CONN-FILE, but with the ports
replaced by the local ports used for the forwarding.

If CONN-FILE is a `tramp' file name, the SERVER arguments will be
ignored and the host will be extracted from the information
contained in the file name.

Note that `zmq-make-tunnel' is used to create the tunnels."
  (let ((conn-info (jupyter-read-plist conn-file))
        (sock (zmq-socket (zmq-current-context) zmq-REP)))
    (when (tramp-tramp-file-p conn-file)
      (let* ((vec (tramp-dissect-file-name conn-file))
             (user (tramp-file-name-user vec)))
        (setq server (if user (concat user "@" (tramp-file-name-host vec))
                       (tramp-file-name-host vec)))))
    (unwind-protect
        (cl-loop
         with remoteip = (plist-get conn-info :ip)
         for (key maybe-rport) on conn-info by #'cddr
         collect key and if (memq key '(:hb_port :shell_port :control_port
                                                 :stdin_port :iopub_port))
         collect (let ((lport (zmq-bind-to-random-port sock "tcp://127.0.0.1")))
                   (zmq-unbind sock (zmq-socket-get sock zmq-LAST-ENDPOINT))
                   (prog1 lport
                     (zmq-make-tunnel lport maybe-rport server remoteip)))
         else collect maybe-rport)
      (zmq-socket-set sock zmq-LINGER 0)
      (zmq-close sock))))

(cl-defun jupyter-create-connection-info (&key
                                          (kernel-name "python")
                                          (transport "tcp")
                                          (ip "127.0.0.1")
                                          (signature-scheme "hmac-sha256")
                                          (key (jupyter-new-uuid))
                                          (hb-port 0)
                                          (stdin-port 0)
                                          (control-port 0)
                                          (shell-port 0)
                                          (iopub-port 0))
  "Create a connection info plist used to connect to a kernel.

The plist has the standard keys found in the jupyter spec. See
http://jupyter-client.readthedocs.io/en/latest/kernels.html#connection-files.
A port number of 0 for a channel means to use a randomly assigned
port for that channel."
  (unless (or (= (length key) 0)
              (equal signature-scheme "hmac-sha256"))
    (error "Only hmac-sha256 signing is currently supported"))
  (append
   (list :kernel_name kernel-name
         :transport transport
         :ip ip)
   (when (> (length key) 0)
     (list :signature_scheme signature-scheme
           :key key))
   (cl-loop
    with sock = (zmq-socket (zmq-current-context) zmq-REP)
    with addr = (concat transport "://" ip)
    for (channel . port) in `((:hb_port . ,hb-port)
                              (:stdin_port . ,stdin-port)
                              (:control_port . ,control-port)
                              (:shell_port . ,shell-port)
                              (:iopub_port . ,iopub-port))
    collect channel and
    if (= port 0) do (setq port (zmq-bind-to-random-port sock addr))
    and collect port else collect port
    finally
    (zmq-socket-set sock zmq-LINGER 0)
    (zmq-close sock))))

(defun jupyter-connect-endpoint (type endpoint &optional identity)
  "Create socket with TYPE and connect to ENDPOINT.
If IDENTITY is non-nil, it will be set as the ROUTING-ID of the
socket. Return the created socket."
  (let ((sock (zmq-socket (zmq-current-context) type)))
    (prog1 sock
      (zmq-socket-set sock zmq-LINGER 1000)
      (when identity
        (zmq-socket-set sock zmq-ROUTING-ID identity))
      (zmq-connect sock endpoint))))

(defun jupyter-connect-channel (ctype endpoint &optional identity)
  "Create a socket based on a Jupyter channel type.
CTYPE is one of the symbols `:hb', `:stdin', `:shell',
`:control', or `:iopub' and represents the type of channel to
connect to ENDPOINT. If IDENTITY is non-nil, it will be set as
the ROUTING-ID of the socket. Return the created socket."
  (let ((sock-type (plist-get jupyter-socket-types ctype)))
    (unless sock-type
      (error "Invalid channel type (%s)" ctype))
    (jupyter-connect-endpoint sock-type endpoint identity)))

;;; Helper functions

(defun jupyter-read-plist (file)
  "Read a JSON encoded FILE as a property list."
  (let ((json-object-type 'plist))
    (json-read-file file)))

(defun jupyter-read-plist-from-string (string)
  "Read a property list from a JSON encoded STRING."
  (let ((json-object-type 'plist))
    (json-read-from-string string)))

(defun jupyter-normalize-data (plist &optional metadata)
  "Return a list (DATA META) from PLIST.
DATA is a property list of mimetype data extracted from PLIST. If
PLIST is a message plist, then DATA will be the value of the
:data key in the messages contents. If PLIST is not a message
plist, then DATA is either the :data key of PLIST or PLIST
itself.

A similar extraction process is performed for the :metadata key
of PLIST which will be the META argument in the return value. If
no :metadata key can be found, then META will be METADATA."
  (list
   (or
    ;; Allow for passing message plists
    (plist-get (jupyter-message-content plist) :data)
    ;; Allow for passing (jupyter-message-content msg)
    (plist-get plist :data)
    ;; Otherwise assume the plist contains mimetypes
    plist)
   (or (plist-get (jupyter-message-content plist) :metadata)
       (plist-get plist :metadata)
       metadata)))

(provide 'jupyter-base)

;;; jupyter-base.el ends here
