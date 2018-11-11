;;; jupyter-ioloop.el --- Jupyter channel subprocess -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 03 Nov 2018
;; Version: 0.0.1
;; Package-Requires: ((emacs "26"))

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

;; An ioloop encapsulates a subprocess that communicates with its parent
;; process in a pre-defined way. The parent process sends events (lists with a
;; head element tagging the type of event and the rest of the elements being
;; the arguments), via a call to the `jupyter-send' method of a
;; `jupyter-ioloop'. The ioloop subprocess then handles the event in its
;; environment. You add an event that can be handled in the ioloop environment
;; by calling `jupyter-ioloop-add-event' before calling `jupyter-ioloop-start'.
;;
;; In the event handler of the ioloop, you may optionally return another event
;; back to the parent process. In this case, when the parent process receives
;; the event it is dispatched to an appropriate `jupyter-ioloop-handler'.
;;
;; An example that will echo back what was sent to the ioloop as a message in
;; the parent process:
;;
;; (cl-defmethod jupyter-ioloop-handler ((ioloop jupyter-ioloop) (tag (eql :tag1)) (event (head echo)))
;;   (message "%s" (cadr event)))
;;
;; (let ((ioloop (jupyter-ioloop))
;;   (jupyter-ioloop-add-event ioloop echo (data)
;;     "Return DATA back to the parent process."
;;     (list 'echo data))
;;   (jupyter-ioloop-start ioloop :tag1)
;;   (jupyter-send ioloop 'echo "Message")
;;   (jupyter-ioloop-stop ioloop))

;; TODO: `jupyter-ioloop-printer': Print a serialized representation of an
;; object, a list with the first element tagging the kind of object.
;;
;; TODO: `jupyter-ioloop-reader': Read an object based on the tag in the ioloop environment.
;;
;; The above methods would give a way to reconstruct somewhat arbitrary objects
;; in the ioloop.

;;; Code:

(require 'jupyter-base)

(defvar jupyter-ioloop-poller nil
  "The polling object being used to poll for events in an ioloop.")

(defvar jupyter-ioloop-nsockets 1
  "The number of sockets being polled by `jupyter-ioloop-poller'.")

(defvar jupyter-ioloop-pre-hook nil
  "A hook called at the start of every polling loop.
The hook is called with no arguments.")

(defvar jupyter-ioloop-post-hook nil
  "A hook called at the end of every polling loop.
The hook is called with a single argument, the list of polling
events that occurred for this iteration, see
the return value of `zmq-poller-wait-all'.")

(defvar jupyter-ioloop-timers nil)

(defvar jupyter-ioloop-timeout 200)

(defvar jupyter-ioloop--argument-types nil
  "Argument types added via `jupyter-ioloop-add-arg-type'.")

(cl-defstruct (jupyter-ioloop
               (:constructor jupyter-ioloop))
  process
  ;; The object used to dispatch on the IOLoop
  object
  callbacks
  events
  setup
  teardown)

(cl-defgeneric jupyter-ioloop-handler ((_ioloop jupyter-ioloop) obj event)
  "Define a new IOLOOP handler, dispatching on OBJ, for EVENT.
OBJ will be the value of the `jupyter-ioloop-object' slot of
IOLOOP and EVENT will be an event as received by a filter
function described in `zmq-start-process'."
  (unless (memq (car-safe event) '(start quit))
    (error "Unhandled event (%s %s)" obj event)))

(defun jupyter-ioloop-wait-until (ioloop event &optional timeout)
  "Wait until EVENT occurs in IOLOOP.
Currently EVENT can be :start or :quit and this function will
block for TIMEOUT seconds until IOLOOP starts or quits depending
on EVENT. If TIMEOUT is nil it defaults to 1 s."
  (cl-assert (jupyter-ioloop-p ioloop))
  (cl-assert (memq event '(:start :quit)))
  (or timeout (setq timeout 1))
  (with-timeout (timeout nil)
    (while (null (process-get (jupyter-ioloop-process ioloop) event))
      (sleep-for 0.1))
    t))

(cl-defmethod jupyter-ioloop-handler ((ioloop jupyter-ioloop) _obj (_ (head quit)))
  (process-put (jupyter-ioloop-process ioloop) :quit t))

(cl-defmethod jupyter-ioloop-handler ((ioloop jupyter-ioloop) _obj (_ (head start)))
  (process-put (jupyter-ioloop-process ioloop) :start t))

(defmacro jupyter-ioloop-add-setup (ioloop &rest body)
  "Set IOLOOP's `jupyter-ioloop-setup' slot to BODY.
BODY is the code that will be evaluated before the IOLOOP sends a
start event to the parent process."
  (declare (indent 1))
  `(setf (jupyter-ioloop-setup ,ioloop)
         (append (jupyter-ioloop-setup ,ioloop)
                 (quote ,body))))

(defmacro jupyter-ioloop-add-teardown (ioloop &rest body)
  "Set IOLOOP's `jupyter-ioloop-teardown' slot to BODY.
BODY is the code that will be evaluated just before the IOLOOP
sends a quit event to the parent process.

After BODY is evaluated in the IOLOOP environment, the channels
in `jupyter-ioloop-channels' will be stopped before sending the
quit event."
  (declare (indent 1))
  `(setf (jupyter-ioloop-teardown ,ioloop) (quote ,body)))

(defmacro jupyter-ioloop-add-arg-type (tag fun)
  "Add a new argument type for arguments in `jupyter-ioloop-add-event'.
If an argument has the form (arg TAG), where TAG is a symbol, in
the ARGS argument of `jupyter-ioloop-add-event', replace it with
the result of evaluating the form returned by FUN on arg in the
IOLOOP environment.

For example suppose we define an argument type, jupyter-channel:

    (jupyter-ioloop-add-arg-type jupyter-channel
      (lambda (arg)
        `(or (object-assoc ,arg :type jupyter-ioloop-channels)
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
     (setq jupyter-ioloop--argument-types
           (delq (assoc ',tag jupyter-ioloop--argument-types)
                 jupyter-ioloop--argument-types))

     (push
      (cons ',tag
            ;; Ensure we don't create lexical closures
            ,(list '\` fun))
      jupyter-ioloop--argument-types)))

(defun jupyter-ioloop--replace-args (args)
  "Convert special arguments in ARGS.
Map over ARGS, converting its elements into

    ,arg or ,(app (lambda (x) BODY) arg)

for use in a `pcase' form. The latter form occurs when one of
ARGS is of the form (arg TAG) where TAG is one of the keys in
`jupyter-ioloop--argument-types'. BODY will be replaced with the
result of calling the function associated with TAG in
`jupyter-ioloop--argument-types'.

Return the list of converted arguments."
  (cl-loop
   with arg-type = nil
   for arg in args
   if (and (listp arg)
           (setq arg-type (assoc (cadr arg) jupyter-ioloop--argument-types)))
   ;; ,(app (lambda (x) ...) arg)
   collect (list '\, (list 'app `(lambda (x) ,(funcall (cdr arg-type) 'x))
                           (car arg)))
   ;; ,arg
   else collect (list '\, arg)))

(defmacro jupyter-ioloop-add-event (ioloop event args &optional doc &rest body)
  "For IOLOOP, add an EVENT handler.
ARGS is a list of arguments that are bound when EVENT occurs. DOC
is an optional documentation string describing what BODY, the
expression which will be evaluated when EVENT occurs, does. If
BODY evaluates to any non-nil value, it will be sent to the
parent Emacs process. A nil value for BODY means don't send
anything.

Some arguments are treated specially:

If one of ARGS is a list (<sym> jupyter-channel) where <sym> is
any symbol, then the parent process that sends EVENT to IOLOOP is
expected to give a channel type as the argument and the type will
be converted into the corresponding channel object and bound to
<sym> before evaluating BODY. The available channels are in
`jupyter-ioloop-channels'."
  (declare (indent 3) (doc-string 4) (debug t))
  (unless (stringp doc)
    (when doc
      (setq body (cons doc body))))
  `(setf (jupyter-ioloop-events ,ioloop)
         (cons (list (quote ,event) (quote ,args) (quote ,body))
               (cl-remove-if (lambda (x) (eq (car x) (quote ,event)))
                             (jupyter-ioloop-events ,ioloop)))))

(defun jupyter-ioloop--event-dispatcher (ioloop exp)
  "For IOLOOP return a form suitable for matching against EXP.
That is, return an expression which will cause an event to be
fired if EXP matches any event types handled by IOLOOP.

TODO: Explain these
By default this adds the events quit, callback, and timer."
  `(let* ((cmd ,exp)
          (res (pcase cmd
                 ,@(cl-loop
                    for (event args body) in (jupyter-ioloop-events ioloop)
                    for cond = (list '\` (cl-list* event (jupyter-ioloop--replace-args args)))
                    if (memq event '(quit callback timer))
                    do (error "Event can't be one of quit, callback, or, timer")
                    ;; cond = `(event ,arg1 ,arg2 ...)
                    else collect `(,cond ,@body))
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
     (when (and res (listp res)) (zmq-prin1 res))))

(cl-defgeneric jupyter-ioloop-add-callback ((ioloop jupyter-ioloop) cb)
  "In IOLOOP, add CB to be run in the IOLOOP environment.
CB is run at the start of every polling loop. Callbacks are
called in the order they are added.

WARNING: A function added as a callback should be quoted to avoid
sending closures to the IOLOOP. An example:

    (jupyter-ioloop-add-callback ioloop
      `(lambda () (zmq-prin1 'foo \"bar\")))"
  (declare (indent 1))
  (cl-assert (functionp cb))
  (setf (jupyter-ioloop-callbacks ioloop)
        (append (jupyter-ioloop-callbacks ioloop) (list cb)))
  (when (process-live-p (jupyter-ioloop-process ioloop))
    (zmq-subprocess-send (jupyter-ioloop-process ioloop)
      (list 'callback (macroexpand-all cb)))))

(defun jupyter-ioloop-poller-add (socket events)
  "Add SOCKET to be polled using the `jupyter-ioloop-poller'.
EVENTS are the polling events that should be listened for on SOCKET."
  (when (zmq-poller-p jupyter-ioloop-poller)
    (zmq-poller-add jupyter-ioloop-poller socket events)
    (cl-incf jupyter-ioloop-nsockets)))

(defun jupyter-ioloop-poller-remove (socket)
  "Remove SOCKET from the `jupyter-ioloop-poller'."
  (when (zmq-poller-p jupyter-ioloop-poller)
    (zmq-poller-remove jupyter-ioloop-poller socket)
    (cl-decf jupyter-ioloop-nsockets)))

(defun jupyter-ioloop--function (ioloop)
  "Return the function that does the work of IOLOOP.
The returned function is suitable to send to a ZMQ subprocess for
evaluation using `zmq-start-process'."
  `(lambda (ctx)
     (push ,(file-name-directory (locate-library "jupyter-base")) load-path)
     (require 'jupyter-ioloop)
     (setq jupyter-ioloop-poller (zmq-poller)
           ;; Initialize any callbacks that were added before the ioloop was started
           jupyter-ioloop-pre-hook
           (mapcar #'byte-compile (quote ,(mapcar #'macroexpand-all
                                        (jupyter-ioloop-callbacks ioloop))))
           ;; Timeout used when polling for events, whenever there are callbacks
           ;; this gets set to 0.
           jupyter-ioloop-timeout (if jupyter-ioloop-pre-hook 0 200))
     ;; Poll for stdin messages
     (zmq-poller-add jupyter-ioloop-poller 0 zmq-POLLIN)
     (let (events)
       (condition-case nil
           (progn
             ,@(jupyter-ioloop-setup ioloop)
             ;; Notify the parent process we are ready to do something
             (zmq-prin1 '(start))
             (while t
               (run-hooks 'jupyter-ioloop-pre-hook)
               (setq events
                     (condition-case nil
                         (zmq-poller-wait-all
                          jupyter-ioloop-poller
                          jupyter-ioloop-nsockets
                          jupyter-ioloop-timeout)
                       ((zmq-EAGAIN zmq-EINTR zmq-ETIMEDOUT) nil)))
               (let ((stdin-event (assq 0 events)))
                 (when stdin-event
                   (setq events (delq stdin-event events))
                   ,(jupyter-ioloop--event-dispatcher
                     ioloop '(zmq-subprocess-read))))
               (when events
                 (run-hook-with-args 'jupyter-ioloop-post-hook events))))
         (quit
          ,@(jupyter-ioloop-teardown ioloop)
          (zmq-prin1 '(quit)))))))

(cl-defmethod jupyter-ioloop-printer (_ioloop _obj event)
  (format "%s" (cdr event)))

(defun jupyter-ioloop--filter (ioloop event)
  (when jupyter--debug
    (message
     (concat "%s: " (jupyter-ioloop-printer ioloop (jupyter-ioloop-object ioloop) event))
     (format (upcase (symbol-name (car event))))))
  (jupyter-ioloop-handler ioloop (jupyter-ioloop-object ioloop) event))

(defun jupyter-ioloop--sentinel (ioloop proc _)
  (when (memq (process-status proc) '(exit signal))
    ;; Reset the object the ioloop was started with, since the object most
    ;; likely contains a reference to the ioloop itself. We want to allow the
    ;; object to be garbage collected if the IOLoop is killed.
    (setf (jupyter-ioloop-object ioloop) nil)))

(cl-defgeneric jupyter-ioloop-start ((ioloop jupyter-ioloop) &rest _args)
  "Start an IOLOOP.")

(cl-defmethod jupyter-ioloop-start ((ioloop jupyter-ioloop) object &key buffer)
  "Start an IOLOOP.
OBJECT is an object which is used to dispatch on when the current
Emacs process receives an event to handle from IOLOOP, see
`jupyter-ioloop-handler'.

If BUFFER is non-nil it should be a buffer that will be used as
the IOLOOP subprocess buffer, see `zmq-start-process'."
  (jupyter-ioloop-stop ioloop)
  (setf (jupyter-ioloop-object ioloop) object)
  (setf (jupyter-ioloop-process ioloop)
        (zmq-start-process
         (jupyter-ioloop--function ioloop)
         :filter (apply-partially #'jupyter-ioloop--filter ioloop)
         :sentinel (apply-partially #'jupyter-ioloop--sentinel ioloop)
         :buffer buffer))
  (jupyter-ioloop-wait-until ioloop :start))

(cl-defgeneric jupyter-ioloop-stop ((ioloop jupyter-ioloop))
  "Stop IOLOOP.
Send a quit event to IOLOOP, wait until it actually quits before
returning."
  (when (process-live-p (jupyter-ioloop-process ioloop))
    (jupyter-send ioloop 'quit)
    (unless (jupyter-ioloop-wait-until ioloop :quit)
      (delete-process (jupyter-ioloop-process ioloop)))))

(cl-defmethod jupyter-send ((ioloop jupyter-ioloop) &rest args)
  "Using IOLOOP, send ARGS to its process.

All arguments passed to this function are sent as a list to the
process unchanged. This means that all arguments should be
serializable."
  (cl-assert (process-live-p (jupyter-ioloop-process ioloop)))
  (zmq-subprocess-send (jupyter-ioloop-process ioloop) args))

(provide 'jupyter-ioloop)

;;; jupyter-ioloop.el ends here
