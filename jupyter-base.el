;;; jupyter-base.el --- Core definitions for Jupyter -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 06 Jan 2018

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

;; This file holds the core requires, variables, and type definitions necessary
;; for jupyter.

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'cl-lib)
(require 'eieio)
(require 'eieio-base)
(require 'json)

(declare-function tramp-dissect-file-name "tramp" (name &optional nodefault))
(declare-function tramp-file-name-user "tramp")
(declare-function tramp-file-name-host "tramp")
(declare-function jupyter-message-content "jupyter-messages" (msg))
(declare-function jupyter-new-uuid "jupyter-messages")

;;; Custom variables

(defcustom jupyter-pop-up-frame nil
  "Whether or not buffers should be displayed in a new frame by default.
Note, this variable is only considered when evaluating code
interactively with functions like `jupyter-eval-line-or-region'.

If equal to nil, frames will never be popped up.  When equal to t,
pop-up frames instead of windows.

`jupyter-pop-up-frame' can also be a list of message type
keywords for messages which will cause frames to be used.  For any
message type not in the list, windows will be used instead.
Currently only `:execute-result', `:error', and `:stream'
messages consider this variable."
  :group 'jupyter
  :type '(choice (const :tag "Pop up frames" t)
                 (const :tag "Pop up windows" nil)
                 ;; TODO: These are the only ones where `jupyter-pop-up-frame'
                 ;; is checked at the moment.
                 (set (const :execute-result)
                      (const :error)
                      (const :stream))))

(defconst jupyter-root (file-name-directory load-file-name)
  "Root directory containing emacs-jupyter.")

(defconst jupyter-protocol-version "5.3"
  "The jupyter protocol version that is implemented.")

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


(defvar jupyter-default-timeout 2.5
  "The default timeout in seconds for `jupyter-wait-until'.")

(defvar jupyter-long-timeout 10
  "A longer timeout than `jupyter-default-timeout' used for some operations.
A longer timeout is needed, for example, when retrieving the
`jupyter-kernel-info' to allow for the kernel to startup.")

(defconst jupyter-version "0.8.2-dev"
  "Current version of Jupyter.")

;;; Macros

(defmacro jupyter-with-timeout (spec &rest wait-forms)
  "Periodically evaluate WAIT-FORMS until timeout.
Or until WAIT-FORMS evaluates to a non-nil value.

Wait until timeout SECONDS, periodically evaluating WAIT-FORMS
until it returns non-nil.  If WAIT-FORMS returns non-nil, stop
waiting and return its value.  Otherwise if timeout SECONDS
elapses, evaluate TIMEOUT-FORMS and return its value.

If PROGRESS is non-nil and evaluates to a string, a progress
reporter will be used with PROGRESS as the message while waiting.

SPEC takes the form (PROGRESS SECONDS TIMEOUT-FORMS...).

\(fn (PROGRESS SECONDS TIMEOUT-FORMS...) WAIT-FORMS...)"
  (declare (indent 1) (debug ((form form body) body)))
  (let ((res (make-symbol "res"))
        (prog (make-symbol "prog"))
        (prog-msg (make-symbol "prog-msg"))
        (timeout (make-symbol "timeout"))
        (wait-time (make-symbol "wait-time")))
    `(let* ((,res nil)
            (,prog-msg ,(pop spec))
            (,timeout ,(pop spec))
            (,wait-time (/ ,timeout 10.0))
            (,prog (and (stringp ,prog-msg)
                        (make-progress-reporter ,prog-msg))))
       (with-timeout (,timeout ,@spec)
         (while (not (setq ,res (progn ,@wait-forms)))
           (accept-process-output nil ,wait-time)
           (when ,prog (progress-reporter-update ,prog))))
       (prog1 ,res
         (when ,prog (progress-reporter-done ,prog))))))

(defmacro jupyter-with-insertion-bounds (beg end bodyform &rest afterforms)
  "Bind BEG and END to `point-marker's, evaluate BODYFORM then AFTERFORMS.
The END marker will advance if BODYFORM inserts text in the
current buffer.  Thus after BODYFORM is evaluated, AFTERFORMS will
have access to the bounds of the text inserted by BODYFORM in the
variables BEG and END.  The result of evaluating BODYFORM is
returned."
  (declare (indent 3) (debug (symbolp symbolp form body)))
  `(let ((,beg (point-marker))
         (,end (point-marker)))
     (set-marker-insertion-type ,end t)
     (unwind-protect
         (prog1 ,bodyform ,@afterforms)
       (set-marker ,beg nil)
       (set-marker ,end nil))))

(defun jupyter-map-mime-bundle (mime-types content fun)
  "For each mime-type in MIME-TYPES, call FUN with its data in CONTENT.
If the result of evaluating FUN on the data of a mime-type is
non-nil, return it.  Otherwise, call FUN for the next mime-type.
Return nil if FUN was evaluated on all mime-types without a
non-nil result.  FUN is only called on mime-types that have data
in CONTENT.

CONTENT is a mime bundle, a property list containing a :data key
and, optionally, a :metadata key that are themselves property
lists with mime-type keywords as keys.

A call to FUN looks like this

    \(funcall fun MIME-TYPE '(:data D :metadata M))

where D will be the data associated with MIME-TYPE in CONTENT and
M is any associated metadata."
  (declare (indent 2))
  (cl-destructuring-bind (&key data metadata &allow-other-keys)
      content
    (catch 'mime-type
      (mapc
       (lambda (mime-type)
         (let ((d (plist-get data mime-type))
               (m (plist-get metadata mime-type)))
           (if d
               (let ((r (funcall fun mime-type `(:data ,d :metadata ,m))))
                 (if r (throw 'mime-type r))))))
       mime-types)
      nil)))

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
        ;; For buffers such as the jupyter REPL, showing trailing whitespaces
        ;; may be a nuisance (as evidenced by the Python banner).
        (setq-local show-trailing-whitespace nil)
        (unless (eq major-mode 'special-mode)
          (special-mode))))
    buffer))

(defun jupyter--reset-display-buffer-p (arg)
  "Return non-nil if the current output buffer should be reset.
If ARG is a `jupyter-request', reset the buffer if ARG's
`jupyter-request-id' is no equal to the
`jupyter-buffer-last-request-id'.  If ARG is not a
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
  "In a buffer with a name derived from NAME current, evaluate BODY.
The buffer's name is obtained by a call to
`jupyter-get-buffer-create'.

A display buffer is similar to a *Help* buffer, but maintains its
previous output on subsequent invocations that use the same NAME
and BODY is wrapped using `jupyter-with-control-code-handling' so
that any insertions into the buffer that contain ANSI escape
codes are properly handled.

Note, before BODY is evaluated, `point' is moved to the end of
the most recent output.

Also note, the `jupyter-current-client' variable in the buffer
that BODY is evaluated in is let bound to whatever value it has
before making that buffer current.

RESET is a form or symbol that determines if the buffer should be
erased before evaluating BODY.  If RESET is nil, no erasing of the
buffer is ever performed.  If RESET evaluates to a
`jupyter-request' object, reset the buffer if the previous
request that generated output in the buffer is not the same
request.  Otherwise if RESET evaluates to any non-nil value, reset
the output buffer."
  (declare (indent 2) (debug (stringp [&or atom form] body)))
  (let ((buffer (make-symbol "buffer"))
        (client (make-symbol "client")))
    `(let ((,client jupyter-current-client)
           (,buffer (jupyter-get-buffer-create ,name)))
       (setq other-window-scroll-buffer ,buffer)
       (with-current-buffer ,buffer
         (unless jupyter-display-buffer-marker
           (setq jupyter-display-buffer-marker (point-max-marker))
           (set-marker-insertion-type jupyter-display-buffer-marker t))
         (let ((inhibit-read-only t)
               (jupyter-current-client ,client))
           (when (jupyter--reset-display-buffer-p ,reset)
             (erase-buffer)
             (set-marker jupyter-display-buffer-marker (point)))
           (goto-char jupyter-display-buffer-marker)
           (jupyter-with-control-code-handling ,@body))))))

(defun jupyter-display-current-buffer-reuse-window (&optional msg-type alist &rest actions)
  "Convenience function to call `display-buffer' on the `current-buffer'.
If a window showing the current buffer is already available,
re-use it.

If ALIST is non-nil it is used as the ACTION alist of
`display-buffer'.

If MSG-TYPE is specified, it should be one of the keywords in
`jupyter-message-types' and is used in setting `pop-up-frames'
and `pop-up-windows'.  See `jupyter-pop-up-frame'.

The rest of the arguments are display ACTIONS tried after
attempting to re-use a window and before attempting to pop-up a
new window or frame."
  (let* ((jupyter-pop-up-frame (jupyter-pop-up-frame-p msg-type))
         (pop-up-frames (and jupyter-pop-up-frame 'graphic-only))
         (pop-up-windows (not jupyter-pop-up-frame))
         (display-buffer-base-action
          (cons
           (append '(display-buffer-reuse-window)
                   (delq nil actions))
           alist)))
    (display-buffer (current-buffer))))

(defun jupyter-pop-up-frame-p (msg-type)
  "Return non-nil if a frame should be popped up for MSG-TYPE."
  (or (eq jupyter-pop-up-frame t)
      (memq msg-type jupyter-pop-up-frame)))

(defun jupyter-display-current-buffer-guess-where (msg-type)
  "Display the current buffer in a window or frame depending on MSG-TYPE.
Call `jupyter-display-current-buffer-reuse-window' passing
MSG-TYPE as argument.  If MSG-TYPE should be displayed in a window
and the current buffer is not already being displayed, display
the buffer below the selected window."
  (jupyter-display-current-buffer-reuse-window
   msg-type nil (unless (jupyter-pop-up-frame-p msg-type)
                  #'display-buffer-below-selected)))

;;; Some useful classes

(defclass jupyter-instance-tracker ()
  ((tracking-symbol :type symbol))
  :documentation "Similar to `eieio-instance-tracker', but keeping weak references.
To access all the objects in TRACKING-SYMBOL, use
`jupyter-all-objects'."
  :abstract t)

(cl-defmethod initialize-instance ((obj jupyter-instance-tracker) &optional _slots)
  (cl-call-next-method)
  (let ((sym (oref obj tracking-symbol)))
    (unless (hash-table-p (symbol-value sym))
      (put sym 'jupyter-instance-tracker t)
      (set sym (make-hash-table :weakness 'key)))
    (puthash obj t (symbol-value sym))))

(defun jupyter-all-objects (sym)
  "Return all tracked objects in tracking SYM.
SYM is a symbol used for tracking objects that inherit from the
class corresponding to the symbol `jupyter-instance-tracker'."
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
authenticate messages.  A `jupyter-session' contains the following
fields:

- CONN-INFO :: The connection info. property list of the kernel
  this session is used to sign messages for.

- ID :: A string of bytes that uniquely identifies this session.

- KEY :: The key used when signing messages.  If KEY is nil,
  message signing is not performed."
  (conn-info nil :read-only t)
  (id nil :read-only t)
  (key nil :read-only t))

(cl-defmethod jupyter-session-endpoints ((session jupyter-session))
  "Return a property list containing the endpoints from SESSION."
  (cl-destructuring-bind
      (&key shell_port iopub_port stdin_port hb_port ip transport
            &allow-other-keys)
      (jupyter-session-conn-info session)
    (cl-assert (and transport ip))
    (let ((addr (lambda (port) (format "%s://%s:%d" transport ip port))))
      (cl-loop
       for (channel . port) in `((:hb . ,hb_port)
                                 (:stdin . ,stdin_port)
                                 (:shell . ,shell_port)
                                 (:iopub . ,iopub_port))
       do (cl-assert port) and
       collect channel and collect (funcall addr port)))))

;;; Request object definition

(cl-defstruct (jupyter-request
               (:constructor nil)
               (:constructor jupyter-request))
  "Represents a request made to a kernel.
Requests sent by a client always return something that can be
interpreted as a `jupyter-request'.  It holds the state of a
request as the kernel and client communicate messages between
each other.  A client has a request table to keep track of all
requests that are not considered idle.  The most recent idle
request is also kept track of.

Each request contains: a message ID, a time sent, a last message
received by the client that sent it, a list of message types that
tell the client to not call the handler methods of those types,
and an alist mapping message types to callback functions a client
should call."
  (id "")
  (time (current-time) :read-only t)
  (idle-p nil)
  (last-message nil)
  (inhibited-handlers nil)
  (callbacks nil))

;;; Connecting to a kernel's channels

(eval-when-compile (require 'tramp))

(defun jupyter-available-local-ports (n)
  "Return a list of N ports available on the localhost."
  (let (servers)
    (unwind-protect
        (cl-loop
         repeat n
         do (push (make-network-process
                   :name "jupyter-available-local-ports"
                   :server t
                   :host "127.0.0.1"
                   :service t)
                  servers)
         finally return (mapcar (lambda (p) (cadr (process-contact p))) servers))
      (mapc #'delete-process servers))))

(defun jupyter-make-ssh-tunnel (lport rport server remoteip)
  (or remoteip (setq remoteip "127.0.0.1"))
  (start-process
   "jupyter-ssh-tunnel" nil
   "ssh"
   ;; Run in background
   "-f"
   ;; Wait until the tunnel is open
   "-o ExitOnForwardFailure=yes"
   ;; Local forward
   "-L" (format "127.0.0.1:%d:%s:%d" lport remoteip rport)
   server
   ;; Close the tunnel if no other connections are made within 60
   ;; seconds
   "sleep 60"))

(defun jupyter-tunnel-connection (conn-file &optional server)
  "Forward local ports to the remote ports in CONN-FILE.
CONN-FILE is the path to a Jupyter connection file, SERVER is the
host that the kernel connection in CONN-FILE is located.  Return a
copy of the connection plist in CONN-FILE, but with the ports
replaced by the local ports used for the forwarding.

If CONN-FILE is a `tramp' file name, the SERVER argument will be
ignored and the host will be extracted from the information
contained in the file name.

Note only SSH tunnels are currently supported."
  (catch 'no-tunnels
    (let ((conn-info (jupyter-read-plist conn-file)))
      (when (and (file-remote-p conn-file)
                 (functionp 'tramp-dissect-file-name))
        (pcase-let (((cl-struct tramp-file-name method user host)
                     (tramp-dissect-file-name conn-file)))
          (pcase method
            ;; TODO: Document this in the README along with the fact that
            ;; connection files can use /ssh: TRAMP files.
            ("docker"
             ;; Assume docker is using the -p argument to publish its exposed
             ;; ports to the localhost.  The ports used in the container should
             ;; be the same ports accessible on the local host.  For example, if
             ;; the shell port is on 1234 in the container, the published port
             ;; flag should be "-p 1234:1234".
             (throw 'no-tunnels conn-info))
            (_
             (setq server (if user (concat user "@" host)
                            host))))))
      (let* ((keys '(:hb_port :shell_port :control_port
                              :stdin_port :iopub_port))
             (lports (jupyter-available-local-ports (length keys))))
        (cl-loop
         with remoteip = (plist-get conn-info :ip)
         for (key maybe-rport) on conn-info by #'cddr
         collect key and if (memq key keys)
         collect (let ((lport (pop lports)))
                   (prog1 lport
                     (jupyter-make-ssh-tunnel lport maybe-rport server remoteip)))
         else collect maybe-rport)))))

(defun jupyter-read-connection (conn-file)
  "Return the connection information in CONN-FILE.
Return a property list representation of the JSON in CONN-FILE, a
Jupyter connection file.

If CONN-FILE is a remote file, possibly create an SSH tunnel
between the localhost and the kernel on the remote host where
CONN-FILE lives.  The returned connection info. will reflect
these changes.

See `jupyter-tunnel-connection' for more details on creating
tunnels.  For more information on connection files, see
https://jupyter-client.readthedocs.io/en/stable/kernels.html#connection-files"
  (if (file-remote-p conn-file)
      (jupyter-tunnel-connection conn-file)
    (jupyter-read-plist conn-file)))

;;; Helper functions

(defun jupyter-canonicalize-language-string (str)
  "Return STR with \" \" converted to \"-\".
The `file-name-nondirectory' of STR will be converted and
returned if it looks like a file path."
  ;; The call to `file-name-nondirectory' is here to be more robust when
  ;; running on systems like Guix or Nix. Some builders on those kinds of
  ;; systems will indiscriminately replace "python" with something like
  ;; "/gnu/store/.../bin/python" when building the kernelspecs.
  (replace-regexp-in-string " " "-" (file-name-nondirectory str)))

(defvar server-buffer)
(defvar jupyter-current-client)
(defvar jupyter-server-mode-client-timer nil
  "Timer used to unset `jupyter-current-client' from `server-buffer'.")

;; FIXME: This works if we only consider a single send request that will also
;; finish within TIMEOUT which is probably 99% of the cases.  It doesn't work
;; for multiple requests that have been sent using different clients where one
;; sets the client in `server-buffer' and, before a file is opened by the
;; underlying kernel, another sets the client in `server-buffer'.

(defun jupyter-server-mode-set-client (client &optional timeout)
  "Set CLIENT as the `jupyter-current-client' in the `server-buffer'.
Kill `jupyter-current-client's local value in `server-buffer'
after TIMEOUT seconds, defaulting to `jupyter-long-timeout'.

If a function causes a buffer to be displayed through
emacsclient, e.g. when a function calls an external command that
invokes the EDITOR, we don't know when the buffer will be
displayed.  All we know is that the buffer that will be current
before display will be the `server-buffer'.  So we temporarily set
`jupyter-current-client' in `server-buffer' so that the client
gets a chance to be propagated to the displayed buffer, see
`jupyter-repl-persistent-mode'.

For this to work properly you should have something like the
following in your Emacs configuration

    (server-mode 1)
    (setenv \"EDITOR\" \"emacsclient\")

before starting any Jupyter kernels.  The kernel also has to know
that it should use EDITOR to open files."
  (when (bound-and-true-p server-mode)
    ;; After switching to a server buffer, keep the client alive in `server-buffer'
    ;; to account for multiple files being opened by the server.
    (unless (and (boundp 'server-switch-hook)
                 (memq #'jupyter-server-mode--unset-client-soon
                       server-switch-hook))
      (add-hook 'server-switch-hook #'jupyter-server-mode--unset-client-soon))
    (with-current-buffer (get-buffer-create server-buffer)
      (setq jupyter-current-client client)
      (jupyter-server-mode--unset-client-soon timeout))))

(defun jupyter-server-mode-unset-client ()
  "Set `jupyter-current-client' to nil in `server-buffer'."
  (when (and (bound-and-true-p server-mode)
             (get-buffer server-buffer))
    (with-current-buffer server-buffer
      (setq jupyter-current-client nil))))

(defun jupyter-server-mode--unset-client-soon (&optional timeout)
  (when (timerp jupyter-server-mode-client-timer)
    (cancel-timer jupyter-server-mode-client-timer))
  (setq jupyter-server-mode-client-timer
        (run-at-time (or timeout jupyter-long-timeout)
                     nil #'jupyter-server-mode-unset-client)))

(defun jupyter-read-plist (file)
  "Read a JSON encoded FILE as a property list."
  (let ((json-object-type 'plist))
    (json-read-file file)))

(defun jupyter-read-plist-from-string (string)
  "Read a property list from a JSON encoded STRING."
  (let ((json-object-type 'plist))
    (json-read-from-string string)))

(defun jupyter-normalize-data (plist &optional metadata)
  "Return a property list (:data DATA :metadata META) from PLIST.
DATA is a property list of mimetype data extracted from PLIST.
If PLIST is a message plist, DATA will be the value of the :data
key in the `jupyter-message-content'.  Otherwise, DATA is either
the :data key of PLIST or PLIST itself.

A similar extraction process is performed for the :metadata key
of PLIST which will be the META argument in the return value.  If
no :metadata key can be found, then META will be METADATA."
  (list :data (or
               ;; Allow for passing message plists
               (plist-get (jupyter-message-content plist) :data)
               ;; Allow for passing (jupyter-message-content msg)
               (plist-get plist :data)
               ;; Otherwise assume the plist contains mimetypes
               plist)
        :metadata (or (plist-get (jupyter-message-content plist) :metadata)
                      (plist-get plist :metadata)
                      metadata)))

(defun jupyter-line-count-greater-p (str n)
  "Return non-nil if STR has more than N lines."
  (string-match-p
   (format "^\\(?:[^\n]*\n\\)\\{%d,\\}" (1+ n))
   str))

(defun jupyter-format-time-low-res (time)
  "Return a description string describing TIME.
If TIME is nil return \"Never\", otherwise return strings like

    \"1 day ago\", \"an hour ago\", \"in 10 minutes\", ...

depending on the relative value of TIME from the `current-time'.
TIME is assumed to have the same form as the return value of
`current-time'."
  (if (null time) "Never"
    (let* ((seconds (- (float-time time)
                       (float-time (current-time))))
           (past (< seconds 0))
           (seconds (abs seconds))
           (minutes (floor (/ seconds 60.0)))
           (hours (floor (/ seconds 3600.0)))
           (days (floor (/ seconds 86400.0))))
      (cond
       ((< seconds 60)
        (if (or past
                ;; Account for discrepancies between time resolution
                (< seconds 0.1))
            "a few seconds ago"
          "in a few seconds"))
       ((not (zerop days))
        (format "%s%d day%s%s"
                (if past "" "in ")
                days
                (if (= days 1) "" "s")
                (if past " ago" "")))
       ((not (zerop hours))
        (if (= hours 1)
            (if past "an hour ago"
              "in one hour")
          (format "%s%d hours%s"
                  (if past "" "in ")
                  hours
                  (if hours " ago" ""))))
       ((not (zerop minutes))
        (if (= minutes 1)
            (if past "a minute ago"
              "in one minute")
          (format "%s%d minutes%s"
                  (if past "" "in ")
                  minutes
                  (if past " ago" ""))))))))

;;; Simple weak references
;; Thanks to Chris Wellon https://nullprogram.com/blog/2014/01/27/

(defun jupyter-weak-ref (object)
  "Return a weak reference for OBJECT."
  (let ((ref (make-hash-table :weakness 'value :size 1)))
    (prog1 ref
      (puthash t object ref))))

(defsubst jupyter-weak-ref-resolve (ref)
  "Resolve a weak REF.
Return nil if the underlying object has been garbage collected,
otherwise return the underlying object."
  (gethash t ref))

;;; Errors

(defun jupyter-error-if-not-client-class-p (class &optional check-class)
  "Signal a wrong-type-argument error if CLASS is not a client class.
If CHECK-CLASS is provided check CLASS against it.  CHECK-CLASS
defaults to `jupyter-kernel-client'."
  (or check-class (setq check-class 'jupyter-kernel-client))
  (cl-assert (class-p check-class))
  (unless (child-of-class-p class check-class)
    (signal 'wrong-type-argument
            (list (list 'subclass check-class) class))))

(provide 'jupyter-base)

;;; jupyter-base.el ends here
