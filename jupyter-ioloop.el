;;; jupyter-ioloop.el --- Jupyter channel subprocess -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 03 Nov 2018

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

;; An ioloop encapsulates a subprocess that communicates with its parent
;; process in a pre-defined way.  The parent process sends events (lists with a
;; head element tagging the type of event and the rest of the elements being
;; the arguments), via a call to the `jupyter-send' method of a
;; `jupyter-ioloop'.  The ioloop subprocess then handles the event in its
;; environment.  You add an event that can be handled in the ioloop environment
;; by calling `jupyter-ioloop-add-event' before calling `jupyter-ioloop-start'.
;;
;; When one of the events added through `jupyter-ioloop-add-event'
;; returns something other than nil, it is sent back to the parent
;; process and the handler function passed to `jupyter-ioloop-start'
;; is called.
;;
;; An example that will echo back what was sent to the ioloop as a message in
;; the parent process:
;;
;; (let ((ioloop (jupyter-ioloop))
;;   (jupyter-ioloop-add-event ioloop echo (data)
;;     "Return DATA back to the parent process."
;;     (list 'echo data))
;;   (jupyter-ioloop-start ioloop (lambda (event) (message "%s" (cadr event))))
;;   (jupyter-send ioloop 'echo "Message")
;;   (jupyter-ioloop-stop ioloop))

;;; Code:

(require 'jupyter-base)
(require 'zmq)
(eval-when-compile (require 'subr-x))

(defvar jupyter-ioloop-poller nil
  "The polling object being used to poll for events in an ioloop.")

(defvar jupyter-ioloop-stdin nil
  "A file descriptor or ZMQ socket used to receive events in an ioloop.")

(defvar jupyter-ioloop-nsockets 1
  "The number of sockets being polled by `jupyter-ioloop-poller'.")

(defvar jupyter-ioloop-pre-hook nil
  "A hook called at the start of every polling loop.
The hook is called with no arguments.")

(defvar jupyter-ioloop-post-hook nil
  "A hook called at the end of every polling loop.
The hook is called with a single argument, the list of polling
events that occurred for this iteration or nil.  The polling
events have the same value as the return value of
`zmq-poller-wait-all'.")

(defvar jupyter-ioloop-timers nil)

(defvar jupyter-ioloop-timeout 200
  "Maximum time (in ms) to wait for polling events on `jupyter-ioloop-poller'.")

(defvar jupyter-ioloop--argument-types nil
  "Argument types added via `jupyter-ioloop-add-arg-type'.")

(defun jupyter-ioloop-environment-p ()
  "Return non-nil if this Emacs instance is an IOLoop subprocess."
  (and noninteractive jupyter-ioloop-stdin jupyter-ioloop-poller))

(defclass jupyter-ioloop (jupyter-finalized-object)
  ((process :type (or null process) :initform nil)
   (callbacks :type list :initform nil)
   (events :type list :initform nil)
   (setup :type list :initform nil)
   (teardown :type list :initform nil))
  :documentation "An interface for sending asynchronous messages via a subprocess.

An ioloop starts an Emacs subprocess setup to send events back
and forth between the parent Emacs process and the ioloop
asynchronously.  The ioloop subprocess is essentially a polling
loop that polls its stdin and any sockets that may have been
created in the ioloop environment and performs pre-defined
actions when stdin sends an event.  The structure of the
subprocess is the following

\(progn
  (let ((jupyter-ioloop-poller (zmq-poller)))
    <jupyter-ioloop-setup>
    <send start event to parent>
    (condition-case nil
      (while t
        (run-hook 'jupyter-ioloop-pre-hook)
        <poll for stdin/socket events>
        (run-hook 'jupyter-ioloop-post-hook))
     (quit
       <jupyter-ioloop-teardown>
       <send quit event to parent>))))

<jupyter-ioloop-setup> is replaced by the form in the setup slot
of an ioloop and can be conveniently added to using
`jupyter-ioloop-add-setup'.

<jupyter-ioloop-teardown> is replaced with the teardown slot and
can be added to using `jupyter-ioloop-add-teardown'.

<poll for stdin/socket events> is replaced by code that will
listen for stdin/socket events using `jupyter-ioloop-poller'.

You add events to be handled by the subprocess using
`jupyter-ioloop-add-event', the return value of any event added
is what is sent to the parent Emacs process and what will
eventually be the sole argument to the handler function passed to
`jupyter-ioloop-start'.  To suppress the subprocess from sending
anything back to the parent, ensure nil is returned by the form
created by `jupyter-ioloop-add-event'.

See `jupyter-channel-ioloop' for an example of its usage.")

(cl-defmethod initialize-instance ((ioloop jupyter-ioloop) &optional _slots)
  (cl-call-next-method)
  (jupyter-add-finalizer ioloop
    (lambda ()
      (with-slots (process) ioloop
        (when (process-live-p process)
          (delete-process process))))))

(defun jupyter-ioloop-wait-until (ioloop event cb &optional timeout progress-msg)
  "Wait until EVENT occurs on IOLOOP.
If EVENT occurs, call CB and return its value if non-nil.  CB is
called with a single argument, an event list whose first element
is EVENT.  If CB returns nil, continue waiting until EVENT occurs
again or until TIMEOUT seconds elapses, TIMEOUT defaults to
`jupyter-default-timeout'.  If TIMEOUT is reached, return nil.

If PROGRESS-MSG is non-nil, a progress reporter will be displayed
while waiting using PROGRESS-MSG as the message."
  (declare (indent 2))
  (cl-check-type ioloop jupyter-ioloop)
  (jupyter-with-timeout
      (progress-msg (or timeout jupyter-default-timeout))
    (let ((e (jupyter-ioloop-last-event ioloop)))
      (when (eq (car-safe e) event) (funcall cb e)))))

(defun jupyter-ioloop-last-event (ioloop)
  "Return the last event received on IOLOOP."
  (cl-check-type ioloop jupyter-ioloop)
  (and (oref ioloop process)
       (process-get (oref ioloop process) :last-event)))

(defmacro jupyter-ioloop-add-setup (ioloop &rest body)
  "Set IOLOOP's `jupyter-ioloop-setup' slot to BODY.
BODY is the code that will be evaluated before the IOLOOP sends a
start event to the parent process."
  (declare (indent 1))
  `(setf (oref ,ioloop setup)
         (append (oref ,ioloop setup)
                 (quote ,body))))

(defmacro jupyter-ioloop-add-teardown (ioloop &rest body)
  "Set IOLOOP's `jupyter-ioloop-teardown' slot to BODY.
BODY is the code that will be evaluated just before the IOLOOP
sends a quit event to the parent process."
  (declare (indent 1))
  `(setf (oref ,ioloop teardown)
         (append (oref ,ioloop teardown)
                 (quote ,body))))

(defmacro jupyter-ioloop-add-arg-type (tag fun)
  "Add a new argument type for arguments in `jupyter-ioloop-add-event'.
If an argument has the form (arg TAG), where TAG is a symbol, in
the ARGS argument of `jupyter-ioloop-add-event', replace it with
the result of evaluating the form returned by FUN on arg in the
IOLOOP environment.

For example suppose we define an argument type, jupyter-channel:

    (jupyter-ioloop-add-arg-type jupyter-channel
      (lambda (arg)
        `(or (object-assoc ,arg :type jupyter-channel-ioloop-channels)
             (error \"Channel not alive (%s)\" ,arg))))

and define an event like

    (jupyter-ioloop-add-event ioloop stop-channel ((channel jupyter-channel))
      (jupyter-stop-channel channel))

Finally after adding other events and starting the ioloop we send
an event like

    (jupyter-send ioloop 'stop-channel :shell)

Then before the stop-channel event defined by
`jupyter-ioloop-add-event' is called in the IOLOOP environment,
the value for the channel argument passed by the `jupyter-send'
call is replaced by the form returned by the function specified
in the `jupyter-ioloop-add-arg-type' call."
  (declare (indent 1))
  `(progn
     (setf (alist-get ',tag jupyter-ioloop--argument-types nil 'remove) nil)
     ;; NOTE: FUN is quoted to ensure lexical closures aren't created
     (push (cons ',tag ,(list '\` fun)) jupyter-ioloop--argument-types)))

(defun jupyter-ioloop--replace-args (args)
  "Convert special arguments in ARGS.
Map over ARGS, converting its elements into

    ,arg or ,(app (lambda (x) BODY) arg)

for use in a `pcase' form.  The latter form occurs when one of
ARGS is of the form (arg TAG) where TAG is one of the keys in
`jupyter-ioloop--argument-types'.  BODY will be replaced with the
result of calling the function associated with TAG in
`jupyter-ioloop--argument-types'.

Return the list of converted arguments."
  (mapcar (lambda (arg)
       (pcase arg
         (`(,val ,tag)
          (let ((form (alist-get tag jupyter-ioloop--argument-types)))
            (list '\, (list 'app `(lambda (x) ,(funcall form 'x)) val))))
         (_ (list '\, arg))))
     args))

(defmacro jupyter-ioloop-add-event (ioloop event args &optional doc &rest body)
  "For IOLOOP, add an EVENT handler.
ARGS is a list of arguments that are bound when EVENT occurs.  DOC
is an optional documentation string describing what BODY, the
expression which will be evaluated when EVENT occurs, does.  If
BODY evaluates to any non-nil value, it will be sent to the
parent Emacs process.  A nil value for BODY means don't send
anything.

Some arguments are treated specially:

If one of ARGS is a list (<sym> tag) where <sym> is any symbol,
then the parent process that sends EVENT to IOLOOP is expected to
send a value that will be bound to <sym> and be handled by an
argument handler associated with tag before BODY is evaluated in
the IOLOOP process, see `jupyter-ioloop-add-arg-type'."
  (declare (indent 3) (doc-string 4) (debug t))
  (unless (stringp doc)
    (when doc
      (setq body (cons doc body))))
  `(setf (oref ,ioloop events)
         (cons (list (quote ,event) (quote ,args) (quote ,body))
               (cl-remove-if (lambda (x) (eq (car x) (quote ,event)))
                             (oref ,ioloop events)))))

(defun jupyter-ioloop--event-dispatcher (ioloop exp)
  "For IOLOOP return a form suitable for matching against EXP.
That is, return an expression which will cause an event to be
fired if EXP matches any event types handled by IOLOOP.

TODO: Explain these
By default this adds the events quit, callback, and timer."
  (let ((user-events
         (cl-loop
          for (event args body) in (oref ioloop events)
          for cond = (list '\` (cl-list*
                                event (jupyter-ioloop--replace-args args)))
          if (memq event '(quit callback timer))
          do (error "Event can't be one of quit, callback, or, timer")
          ;; cond = `(event ,arg1 ,arg2 ...)
          else collect `(,cond ,@body))))
    `(let* ((cmd ,exp)
            (res (pcase cmd
                   ,@user-events
                   ;; Default events
                   (`(timer ,id ,period ,cb)
                    ;; Ensure we don't send anything back to the parent process
                    (prog1 nil
                      (let ((timer (run-at-time 0.0 period (byte-compile cb))))
                        (puthash id timer jupyter-ioloop-timers))))
                   (`(callback ,cb)
                    ;; Ensure we don't send anything back to the parent process
                    (prog1 nil
                      (setq jupyter-ioloop-timeout 0)
                      (add-hook 'jupyter-ioloop-pre-hook (byte-compile cb) 'append)))
                   ('(quit) (signal 'quit nil))
                   (_ (error "Unhandled command %s" cmd)))))
       ;; Can only send lists at the moment
       (when (and res (listp res)) (zmq-prin1 res)))))

(cl-defgeneric jupyter-ioloop-add-callback ((ioloop jupyter-ioloop) cb)
  "In IOLOOP, add CB to be run in the IOLOOP environment.
CB is run at the start of every polling loop.  Callbacks are
called in the order they are added.

WARNING: A function added as a callback should be quoted to avoid
sending closures to the IOLOOP.  An example:

    (jupyter-ioloop-add-callback ioloop
      `(lambda () (zmq-prin1 'foo \"bar\")))"
  (declare (indent 1))
  (cl-assert (functionp cb))
  (cl-callf append (oref ioloop callbacks) (list cb))
  (when (process-live-p (oref ioloop process))
    (jupyter-send ioloop 'callback (macroexpand-all cb))))

(defun jupyter-ioloop-poller-add (socket events)
  "Add SOCKET to be polled using the `jupyter-ioloop-poller'.
EVENTS are the polling events that should be listened for on
SOCKET.  If `jupyter-ioloop-poller' is not a `zmq-poller' object
do nothing."
  (when (zmq-poller-p jupyter-ioloop-poller)
    (zmq-poller-add jupyter-ioloop-poller socket events)
    (cl-incf jupyter-ioloop-nsockets)))

(defun jupyter-ioloop-poller-remove (socket)
  "Remove SOCKET from the `jupyter-ioloop-poller'.
If `jupyter-ioloop-poller' is not a `zmq-poller' object do
nothing."
  (when (zmq-poller-p jupyter-ioloop-poller)
    (zmq-poller-remove jupyter-ioloop-poller socket)
    (cl-decf jupyter-ioloop-nsockets)))

(defun jupyter-ioloop--body (ioloop on-stdin)
  `(let (events)
     (condition-case nil
         (progn
           ,@(oref ioloop setup)
           ;; Initialize any callbacks that were added before the ioloop was
           ;; started
           (setq jupyter-ioloop-pre-hook
                 (mapcar (lambda (f) (unless (byte-code-function-p f) (byte-compile f)))
                    (append jupyter-ioloop-pre-hook
                            (quote ,(mapcar #'macroexpand-all
                                       (oref ioloop callbacks))))))
           ;; Notify the parent process we are ready to do something
           (zmq-prin1 '(start))
           (let ((on-stdin (byte-compile (lambda () ,on-stdin))))
             (while t
               (run-hooks 'jupyter-ioloop-pre-hook)
               (setq events
                     (condition-case nil
                         (zmq-poller-wait-all
                          jupyter-ioloop-poller
                          jupyter-ioloop-nsockets
                          jupyter-ioloop-timeout)
                       ((zmq-EAGAIN zmq-EINTR zmq-ETIMEDOUT) nil)))
               (let ((stdin-event (zmq-assoc jupyter-ioloop-stdin events)))
                 (when stdin-event
                   (setq events (delq stdin-event events))
                   (funcall on-stdin)))
               (run-hook-with-args 'jupyter-ioloop-post-hook events))))
       (quit
        ,@(oref ioloop teardown)
        (zmq-prin1 '(quit))))))

(defun jupyter-ioloop--function (ioloop port)
  "Return the function that does the work of IOLOOP.
The returned function is suitable to send to a ZMQ subprocess for
evaluation using `zmq-start-process'.

If PORT is non-nil the returned function will create a ZMQ PULL
socket to receive events from the parent process on the PORT of
the local host, otherwise events are expected to be received on
STDIN.  This is useful on Windows systems which don't allow
polling the STDIN file handle."
  `(lambda (ctx)
     (push ,(file-name-directory (locate-library "jupyter-base")) load-path)
     (require 'jupyter-ioloop)
     (setq jupyter-ioloop-poller (zmq-poller))
     (setq jupyter-ioloop-stdin
           ,(if port
                `(let ((sock (zmq-socket ctx zmq-PAIR)))
                   (prog1 sock
                     (zmq-connect sock (format "tcp://127.0.0.1:%s" ,port))))
              0))
     (zmq-poller-add jupyter-ioloop-poller jupyter-ioloop-stdin zmq-POLLIN)
     ,(jupyter-ioloop--body
       ioloop (jupyter-ioloop--event-dispatcher
               ioloop (if port '(read (zmq-recv-decoded jupyter-ioloop-stdin))
                        '(zmq-subprocess-read))))))

(defun jupyter-ioloop-alive-p (ioloop)
  "Return non-nil if IOLOOP is ready to receive/send events."
  (cl-check-type ioloop jupyter-ioloop)
  (with-slots (process) ioloop
    (and (process-live-p process) (process-get process :start))))

(defun jupyter-ioloop--make-filter (ioloop handler)
  (lambda (event)
    (let ((process (oref ioloop process)))
      (process-put process :last-event event)
      (cond
       ((eq (car-safe event) 'start)
        (process-put process :start t))
       ((eq (car-safe event) 'quit)
        (process-put process :quit t))
       (t
        (funcall handler event))))))

(cl-defgeneric jupyter-ioloop-start ((ioloop jupyter-ioloop)
                                     handler
                                     &key buffer)
  "Start an IOLOOP.
HANDLER is a function of one argument and will be passed an event
received by the subprocess that IOLOOP represents, an event is
just a list.

If IOLOOP was previously running, it is stopped first.

If BUFFER is non-nil it should be a buffer that will be used as
the IOLOOP subprocess buffer, see `zmq-start-process'."
  (jupyter-ioloop-stop ioloop)
  (let (stdin port)
    ;; NOTE: A socket is used to read input from the parent process to avoid
    ;; the stdin buffering done when using `read-from-minibuffer' in the
    ;; subprocess.  When `noninteractive', `read-from-minibuffer' uses
    ;; `getc_unlocked' internally and `getc_unlocked' reads from the stdin FILE
    ;; object as opposed to reading directly from STDIN_FILENO.  The problem is
    ;; that FILE objects are buffered streams which means that every message
    ;; the parent process sends does not necessarily correspond to a POLLIN
    ;; event on STDIN_FILENO in the subprocess.  Since we only call
    ;; `read-from-minibuffer' when there is a POLLIN event on STDIN_FILENO
    ;; there is the potential that a message is waiting to be handled in the
    ;; buffer used by stdin which will only get handled if we send more
    ;; messages to the subprocess thereby creating more POLLIN events.
    (when (or t (memq system-type '(windows-nt ms-dos cygwin)))
      (setq stdin (zmq-socket (zmq-current-context) zmq-PAIR))
      (setq port (zmq-bind-to-random-port stdin "tcp://127.0.0.1")))
    (let ((process (zmq-start-process
                    (jupyter-ioloop--function ioloop (when stdin port))
                    ;; We go through this Emacs-fu, brought to you by Chris
                    ;; Wellons, https://nullprogram.com/blog/2014/01/27/,
                    ;; because we want OBJECT to be the final say in when
                    ;; everything gets garbage collected.  If OBJECT loses
                    ;; scope, the ioloop process should be killed off.  This
                    ;; wouldn't happen if we hold a strong reference to
                    ;; OBJECT.
                    :filter (jupyter-ioloop--make-filter ioloop handler)
                    :buffer buffer)))
      (oset ioloop process process)
      (when stdin
        (process-put process :stdin stdin))
      (jupyter-ioloop-wait-until ioloop 'start #'identity))))

(cl-defgeneric jupyter-ioloop-stop ((ioloop jupyter-ioloop))
  "Stop IOLOOP.
Send a quit event to IOLOOP, wait until it actually quits before
returning."
  (with-slots (process) ioloop
    (when (process-live-p process)
      (jupyter-send ioloop 'quit)
      (unless (jupyter-ioloop-wait-until ioloop 'quit #'identity)
        (delete-process process))
      (when-let* ((stdin (process-get process :stdin))
                  (socket-p (zmq-socket-p stdin)))
        (zmq-unbind stdin (zmq-get-option stdin zmq-LAST-ENDPOINT))))))

(defvar jupyter-ioloop--send-buffer nil)

(defun jupyter-ioloop--dump-message (plist)
  (with-current-buffer
      (if (buffer-live-p jupyter-ioloop--send-buffer)
          jupyter-ioloop--send-buffer
        (setq jupyter-ioloop--send-buffer
              (get-buffer-create " *jupyter-ioloop-send*")))
    (erase-buffer)
    (let (print-level print-length)
      (prin1 plist (current-buffer)))
    (buffer-string)))

(cl-defmethod jupyter-send ((ioloop jupyter-ioloop) &rest args)
  "Using IOLOOP, send ARGS to its process.

All arguments passed to this function are sent as a list to the
process unchanged.  This means that all arguments should be
serializable."
  (with-slots (process) ioloop
    (cl-assert (process-live-p process))
    (let ((stdin (process-get process :stdin)))
      (if stdin
          (let ((msg (jupyter-ioloop--dump-message args)) sent)
            (while (not sent)
              (condition-case nil
                  (progn
                    (zmq-send-encoded stdin msg nil zmq-DONTWAIT)
                    (setq sent t))
                (zmq-EAGAIN (accept-process-output nil 0)))))
        (zmq-subprocess-send process args)))))

(provide 'jupyter-ioloop)

;;; jupyter-ioloop.el ends here
