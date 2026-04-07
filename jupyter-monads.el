;;; jupyter-monads.el --- Monadic Jupyter -*- lexical-binding: t -*-

;; Copyright (C) 2020-2024 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 11 May 2020

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

;; TODO: Generalize `jupyter-with-io' and `jupyter-do' for any monad,
;; not just the I/O one.

;;; Code:

(require 'jupyter-base)
(require 'jupyter-seq)

(declare-function jupyter-handle-message "jupyter-client")
(declare-function jupyter-kernel-io "jupyter-client")
(declare-function jupyter-client-io "jupyter-client")
(declare-function jupyter-generate-request "jupyter-client")
(declare-function jupyter-wait-until-idle "jupyter-client" (req &optional timeout progress-msg))

(defun jupyter-get-state ()
  "Return the current state as a monadic value."
  (lambda (state) (cons state state)))

(defun jupyter-put-state (value)
  "Return a monadic value that sets the current state to VALUE."
  (lambda (_state) (cons nil value)))

(defun jupyter-return (value)
  "Return VALUE as a monadic value."
  (declare (indent 0)
           (compiler-macro
            (lambda (exp)
              (cond
               ((if (atom value)
                    (or (memq value '(nil t))
                        (not (symbolp value)))
                  (eq (car value) 'quote))
                `(lambda (state) (cons ,value state)))
               (t exp)))))
  (lambda (state) (cons value state)))

(defun jupyter-bind (mvalue mfn)
  "Bind monadic value MVALUE to monadic function MFN."
  (declare (indent 1))
  (lambda (state)
    (pcase-let* ((`(,value . ,state) (funcall mvalue state)))
      (funcall (funcall mfn value) state))))

(defmacro jupyter-mlet* (varlist &rest body)
  "Bind the monadic values in VARLIST, evaluate BODY.
Return the result of evaluating BODY.  The result of evaluating
BODY should be another monadic value.

Note that VARLIST has the same meaning as in `let' with the addition
that there is support for destructuring using `pcase' patterns."
  (declare (indent 1) (debug ((&rest (symbolp form)) body)))
  (if (null varlist)
      (if (zerop (length body)) '(jupyter-return nil)
        `(progn ,@body))
    (pcase-let ((`(,pat ,mvalue) (car varlist)))
      `(jupyter-bind ,mvalue
         ,(if (symbolp pat)
              `(lambda (,pat)
                 (jupyter-mlet* ,(cdr varlist)
                   ,@body))
            (let ((arg (make-symbol "arg")))
              `(lambda (,arg)
                 (pcase-let ((,pat ,arg))
                   (jupyter-mlet* ,(cdr varlist)
                     ,@body)))))))))

(defmacro jupyter-do (&rest actions)
  "Return a monadic value that performs all actions in ACTIONS.
The actions are evaluated in the order given.  The result of the
returned action is the result of the last action in ACTIONS."
  (declare (indent 0) (debug (body)))
  (cond
   ((zerop (length actions)) '(jupyter-return nil))
   ((= 1 (length actions))
    (car actions))
   (t
    `(jupyter-mlet* ((_ ,(car actions)))
       (jupyter-do ,@(cdr actions))))))

(defun jupyter-push (s)
  "Push S as the first element of the state as a monadic value.
The state within the context is assumed to be a list and S is pushed as
the first element."
  (jupyter-mlet* ((state (jupyter-get-state)))
    (jupyter-put-state
     (cons s (if (listp state) state (list state))))))

(defun jupyter-pop ()
  "Pop the first element from the state as a monadic value.
Return a monadic value that returns that first element when bound in the
context."
  (jupyter-mlet* ((state (jupyter-get-state)))
    (let ((value (if (listp state)
                     (pop state)
                   (prog1 state
                     (setq state nil)))))
      (jupyter-do
        (jupyter-put-state state)
        (jupyter-return value)))))

(defun jupyter-get-client ()
  "Return a monadic value that returns the client."
  (jupyter-mlet* ((state (jupyter-get-state)))
    (let ((client (if (listp state)
                      (car state)
                    state)))
      (jupyter-return client))))

(defun jupyter-set-client (client)
  "Return a monadic value that sets the client."
  (cl-check-type client jupyter-kernel-client)
  (jupyter-mlet* ((state (jupyter-get-state)))
    (jupyter-put-state
     (if (listp state)
         (cons client (cdr state))
       client))))

(defun jupyter-at-point (action)
  "Return a monadic value evaluating ACTION at `point'.
If `point' doesn't point to a valid buffer position when attempting to
run ACTION, e.g. the buffer has been killed, do nothing and return nil
as the monadic value."
  (let ((marker (point-marker)))
    (jupyter-mlet* ((state (jupyter-get-state)))
      (jupyter-return
        (when (and (buffer-live-p (marker-buffer marker))
                   (marker-position marker))
          (unwind-protect
              (with-current-buffer (marker-buffer marker)
                (save-excursion
                  (save-restriction
                    (widen)
                    (goto-char (marker-position marker))
                    (jupyter-run-with-state state
                      action))))
            (move-marker marker nil)))))))

(defmacro jupyter-with-bindings* (varlist action)
  "Return a monadic value that evaluates ACTION with bound variables.
VARLIST is a list of variables as in `let*', return a monadic value that
evaluates ACTION with those variables bound to their values in the
context of the evaluation environment of ACTION.

If an element of VARLIST is simply a variable name then, when ACTION is
evaluated, that name will be bound to the value it had at the time the
returned value is generated.  This is useful, for example, to bind
dynamic variables to the same values, during the evaluation of ACTION,
they had when the returned value is generated.  If an element of VARLIST
is a list (VAR VALUE), then VALUE is bound to VAR when ACTION is
evaluated."
  (declare (indent 1))
  (let ((syms (mapcar (lambda (_) (gensym)) varlist)))
    `(let* ,(cl-mapcar (lambda (s v)
                         (list s (if (listp v) (cadr v)
                                   v)))
                       syms varlist)
       (jupyter-mlet* ((state (jupyter-get-state)))
         (let* ,(cl-mapcar (lambda (s v) (list (if (listp v) (car v)
                                            v)
                                          s))
                           syms varlist)
           (jupyter-return
             (jupyter-run-with-state state
               ,action)))))))

(defmacro jupyter-with-temporary-state (state &rest body)
  (declare (indent 1))
  (let ((new-state (make-symbol "state"))
        (saved-state (make-symbol "saved-state")))
    `(let ((,new-state ,state))
       (jupyter-mlet* ((,saved-state (jupyter-get-state)))
         (jupyter-do
           (jupyter-put-state ,new-state)
           (jupyter-mlet* ((value (progn ,@body)))
             (jupyter-do
               (jupyter-put-state ,saved-state)
               (jupyter-return value))))))))

(defun jupyter-run-with-state (state mvalue)
  "Pass STATE as the state to MVALUE, return the resulting value."
  (declare (indent 1))
  ;; Discard the final state
  (car (funcall mvalue state)))

(defmacro jupyter-run-with-io (io &rest body)
  "Return the result of evaluating the I/O value BODY evaluates to.
All I/O operations are done in the context of IO."
  (declare (indent 1) (debug (form body)))
  `(jupyter-run-with-state ,io (progn ,@body)))

(defmacro jupyter-run-with-client (client &rest body)
  "Return the result of evaluating the monadic value BODY evaluates to.
The initial state given to the monadic value is CLIENT."
  (declare (indent 1) (debug (form body)))
  `(jupyter-run-with-state ,client (progn ,@body)))

(defmacro jupyter-run (spec action)
  "Convenience macro to run ACTION with particular initial state.
SPEC is a property list which currently can take on the following
values:

    \\='(:client current)

which means to run ACTION with the state being the
`jupyter-current-client'."
  (declare (indent 1))
  (let ((form action))
    (while spec
      (let ((arg (pop spec)))
        (pcase arg
          ((and `:client
                (guard (and (not (keywordp (car spec)))
                            (eq (pop spec) 'current))))
           (setq form
                 `(jupyter-run-with-client
                      jupyter-current-client
                    ,form)))
          ((and `:io (let io (pop spec)))
           (setq form
                 `(jupyter-run-with-io ,io
                    ,form)))
          (_
           (error "Unhandled keyword: %s" arg)))))
    form))

(defmacro jupyter-with-io (io &rest body)
  "Return an I/O action evaluating BODY in IO's context.
The result of the returned action is the result of the I/O action
BODY evaluates to."
  (declare (indent 1) (debug (form body)))
  `(lambda (_)
     (jupyter-run-with-io ,io ,@body)))

;;; Publisher/subscriber

(define-error 'jupyter-subscribed-subscriber
  "A subscriber cannot be subscribed to.")

(defun jupyter-subscriber (sub-fn)
  "Return a subscriber evaluating SUB-FN on published content.
SUB-FN should return the result of evaluating
`jupyter-unsubscribe' if the subscriber's subscription should be
canceled.

Ex. Unsubscribe after consuming one message

    (jupyter-subscriber
      (lambda (value)
        (message \"The published content: %s\" value)
        (jupyter-unsubscribe)))

    Used like this, where sub is the above subscriber:

    (jupyter-run-with-io (jupyter-publisher)
      (jupyter-subscribe sub)
      (jupyter-publish (list \='topic \"today's news\")))"
  (declare (indent 0))
  (lambda (sub-content)
    (pcase sub-content
      (`(content ,content) (funcall sub-fn content))
      (`(subscribe ,_) (signal 'jupyter-subscribed-subscriber nil))
      (_ (error "Unhandled subscriber content: %s" sub-content)))))

(defun jupyter-content (value)
  "Arrange for VALUE to be sent to subscribers of a publisher."
  (list 'content value))

(defsubst jupyter-unsubscribe ()
  "Arrange for the current subscription to be canceled.
A subscriber (or publisher with a subscription) can return the
result of this function to cancel its subscription with the
publisher providing content."
  (list 'unsubscribe))

(define-error 'jupyter-publisher-subscribers-had-errors
  "Publisher's subscribers had errors")

(defun jupyter-distribute-content (pub-fn content subs)
  "Apply PUB-FN on submitted CONTENT to produce published content.
Call each subscriber in SUBS on the published content.  Remove
those subscribers that cancel their subscription.

When a subscriber signals an error it is noted and the remaining
subscribers are processed.  After processing all subscribers, a
`jupyter-publisher-subscribers-had-errors' error is raised with
the data being the list of errors raised when calling
subscribers.  Note, when a subscriber errors, it remains in the
list of subscribers."
  (pcase (funcall pub-fn content)
    ((and `(content ,_) sub-content)
     ;; NOTE: The first element of SUBS is ignored here so that the
     ;; pointer to the subscriber list remains the same for each
     ;; publisher, even when subscribers are being destructively
     ;; removed.
     (let ((errors nil))
       (while (cadr subs)
         (condition-case err
             ;; Publish subscriber content to subscribers
             (pcase (funcall (cadr subs) sub-content)
               ;; Destructively remove the subscriber when it returns an
               ;; unsubscribe value.
               ('(unsubscribe) (setcdr subs (cddr subs)))
               (_ (pop subs)))
           (error
            ;; Skip over any subscribers that raised an error.
            (pop subs)
            (push err errors))))
       ;; Inform about the errors.
       (when errors
         (signal 'jupyter-publisher-subscribers-had-errors errors)))
     nil)
    ;; Cancel a publisher's subscription to another publisher.
    ('(unsubscribe) '(unsubscribe))
    (_ nil)))

(defun jupyter-publisher (&optional pub-fn)
  "Return a publisher function.
A publisher function is a closure, function with a local scope,
that maintains a list of subscribers and distributes the content
that PUB-FN returns to each of them.

PUB-FN is a function that optionally returns content to
publish (by returning the result of `jupyter-content' on a
value).  It's called when a value is submitted for publishing
using `jupyter-publish', like this:

    (let ((pub (jupyter-publisher
                 (lambda (submitted-value)
                   (message \"Publishing %s to subscribers\" submitted-value)
                   (jupyter-content submitted-value)))))
      (jupyter-run-with-io pub
        (jupyter-publish (list 1 2 3))))

The default for PUB-FN is `jupyter-content'.  See
`jupyter-subscribe' for an example on how to subscribe to a
publisher.

If no content is returned by PUB-FN, no content is sent to
subscribers.

A publisher can also be a subscriber of another publisher.  In
this case, if PUB-FN returns the result of `jupyter-unsubscribe'
its subscription is canceled.

Ex. Publish the value 1 regardless of what is given to PUB-FN.

    (jupyter-publisher
      (lambda (_)
        (jupyter-content 1)))

Ex. Publish \='app if \='app is given to a publisher, nothing is sent
    to subscribers otherwise.  In this case, a publisher is a
    filter of the value given to it for publishing.

    (jupyter-publisher
      (lambda (value)
        (if (eq value \='app)
          (jupyter-content value))))"
  (declare (indent 0))
  (let ((subs (list 'subscribers))
        (pub-fn (or pub-fn #'jupyter-content)))
    ;; A publisher value is either a value representing a subscriber
    ;; or a value representing content to send to subscribers.
    (lambda (pub-value)
      (pcase (car-safe pub-value)
        ('content (jupyter-distribute-content pub-fn (cadr pub-value) subs))
        ('subscribe (cl-pushnew (cadr pub-value) (cdr subs)))
        (_ (error "Unhandled publisher content: %s" pub-value))))))

(defun jupyter-subscribe (sub)
  "Return an I/O action that subscribes SUB to published content.
If a subscriber (or a publisher with a subscription to another
publisher) returns the result of `jupyter-unsubscribe', its
subscription is canceled.

Ex. Subscribe to a publisher and unsubscribe after receiving two
    messages.

    (let* ((msgs \='())
           (pub (jupyter-publisher))
           (sub (jupyter-subscriber
                  (lambda (n)
                    (if (> n 2) (jupyter-unsubscribe)
                      (push n msgs))))))
      (jupyter-run-with-io pub
        (jupyter-subscribe sub))
      (cl-loop
       for x in \='(1 2 3)
       do (jupyter-run-with-io pub
            (jupyter-publish x)))
      (reverse msgs)) ; => \='(1 2)"
  (declare (indent 0))
  (jupyter-mlet* ((io (jupyter-get-state)))
    (funcall io (list 'subscribe sub))
    (jupyter-return nil)))

(defun jupyter-publish (value)
  "Return an I/O action that submits VALUE to publish as content."
  (declare (indent 0))
  (jupyter-mlet* ((io (jupyter-get-state)))
    (funcall io (jupyter-content value))
    (jupyter-return nil)))

;;; Working with requests

(defun jupyter-sent (dreq)
  (jupyter-do dreq))

(defun jupyter-messages (req)
  "Return a sequence of the messages of REQ.
A value is returned that returns a sequence (see `seqp')."
  (jupyter-mlet* ((req req))
    (jupyter-return
      (jupyter-message-seq req))))

(defun jupyter-idle (&optional req timeout)
  "Return a value that waits until REQ becomes idle.
Returns the idled request.

If REQ is nil, it defaults to a request that solely syncs the
client's execution state of the kernel.

TIMEOUT seconds may elapse before a
`jupyter-timeout-before-message' error is raised and no idle
message has arrived."
  (or req (setq req
                (jupyter-execute-request
                 :silent t
                 :store-history nil
                 :code ""
                 :handlers nil)))
  (jupyter-mlet* ((req req))
    (let ((jupyter-long-timeout (or timeout jupyter-long-timeout)))
      (seq-find
       #'jupyter-message-status-idle-p
       (jupyter-message-seq req)))
    (jupyter-return req)))

(defun jupyter-find-message (req pred &optional timeout)
  "Return the first message passing PRED in REQ's messages.
Nil is returned if none were found.

TIMEOUT seconds may elapse before a
`jupyter-timeout-before-message' error is raised and no idle
message has arrived."
  (declare (indent 1))
  (jupyter-mlet* ((seq (jupyter-messages req)))
    (jupyter-return
      (let ((jupyter-long-timeout (or timeout jupyter-long-timeout)))
        (seq-find pred seq)))))

(defun jupyter-reply (req &optional timeout)
  "Return the reply message of REQ.
TIMEOUT has the same meaning as in `jupyter-idle'."
  (jupyter-find-message req #'jupyter-message-reply-p timeout))

(defun jupyter-result (req &optional timeout)
  "Return the result message of REQ.
TIMEOUT has the same meaning as in `jupyter-idle'."
  (jupyter-find-message req #'jupyter-message-result-p timeout))

(defun jupyter-add-subscriber (sub)
  "Return an action that makes SUB a message subscriber for the next request.
SUB is a function that takes a single argument, a message
property list."
  (jupyter-mlet* ((state (jupyter-get-state)))
    (if (null state)
        (push (jupyter-subscriber sub) state)
      (unless (listp state)
        (setq state (list state)))
      (push (jupyter-subscriber sub) (cdr state)))
    (jupyter-put-state state)))

;; Defined in jupyter-client.el
(defvar jupyter--current-request)
(defvar jupyter-inhibit-handlers)

(defun jupyter-message-subscribed (req cbs)
  "Return a monadic value that subscribes CBS to a request's message publisher.
REQ is monadic value that evaluates to a `jupyter-request', i.e. the
monadic values returned by `jupyter-*-request' functions.

CBS is an alist mapping message types to callback subscribers like

    `((\"execute_reply\" ,(lambda (msg) ...))
      ...)

The subscribers are called only for the associated message type.

All callback subscribers take a single argument, a message.  The current
request which generated the message can be accessed through the
`jupyter-current-request' function.

CBS can also be a function, in which case it is itself the callback
subscriber to apply to all messages of any type.

The returned value returns the request that is resolved, subscribing the
callbacks before resolving the request."
  (let (rreq)
    (jupyter-do
      (jupyter-add-subscriber
       (if (functionp cbs)
           (lambda (msg)
             (when-let* ((jupyter--current-request rreq))
               (funcall cbs msg)))
         (lambda (msg)
           (when-let*
               ((msg-type (jupyter-message-type msg))
                (fn (car (alist-get msg-type cbs nil nil #'string=)))
                (jupyter--current-request rreq))
             (funcall fn msg)))))
      (jupyter-mlet* ((sreq req))
        (jupyter-return
          (setq rreq sreq))))))

;; When replaying messages, the request message publisher is already
;; unsubscribed from any upstream publishers.
(defun jupyter--debug-replay-requests ()
  (setq jupyter--debug-request-queue (nreverse jupyter--debug-request-queue))
  (while jupyter--debug-request-queue
    (pcase-let ((`(,client ,req) (pop jupyter--debug-request-queue)))
      (cl-loop
       for msg in (jupyter-request-messages req)
       do (condition-case nil
              (jupyter-handle-message client (plist-get msg :channel) msg)
            (error (setq jupyter--debug-request-queue
                         (nreverse jupyter--debug-request-queue))))))))

;;; Request

(defun jupyter-request-message-handler-with-fallback (&optional fallback)
  "Return a request message handler as a message publisher.
Optional FALLBACK is a function that takes a message when there are no
requests available to handle that message.  FALLBACK is called both when
the message is not a `jupyter-message-p' as well as when it is and there
is no associated request."
  (let ((idle-request-ids nil)
        (msg-publishers (make-hash-table :test #'equal)))
    (list
     (jupyter-subscriber
       (lambda (action)
         (pcase action
           (`(publisher ,id ,(and f (pred functionp)))
            (puthash id
                     (funcall f (gethash id msg-publishers))
                     msg-publishers))
           (`(idle ,id)
            (push id idle-request-ids)))))
     (jupyter-publisher
       (lambda (msg)
         (if (not (jupyter-message-p msg))
             ;; Send what doesn't appear to be a message as is.
             (and fallback (funcall fallback msg))
           (when (and idle-request-ids
                      ;; A heuristic for assuming that an idle request
                      ;; has stopped receiving messages is when there is
                      ;; a status: busy message received by the client
                      ;; after the request has become idle.  In this
                      ;; case, the kernel has completed handling all of
                      ;; the idle requests and is moving on to processing
                      ;; a new request so the assumption is that there
                      ;; would be no more messages received for any of
                      ;; the idle requests and therefore they can be
                      ;; safely removed from the table of live request
                      ;; message publishers.
                      (jupyter-message-status-busy-p msg))
             (while idle-request-ids
               (let ((id (pop idle-request-ids)))
                 (when-let* ((pub (gethash id msg-publishers)))
                   ;; Notify subscribers that no more messages are
                   ;; arriving.
                   (jupyter-run-with-io pub
                     (jupyter-publish jupyter-empty-message)))
                 (remhash id msg-publishers))))
           (if-let* ((pub (gethash
                           (jupyter-message-parent-id msg)
                           msg-publishers)))
               (jupyter-run-with-io pub
                 (jupyter-publish msg))
             ;; No message publisher for the parent request.
             (and fallback (funcall fallback msg)))))))))

(defun jupyter-message-publisher (client req)
  "Return a publisher that publishes REQ's messages.
Any subscribers to this publisher will receive the message
property lists of the received messages for REQ.  Note, that
non-messages are passed to subscribers as is.  The
`jupyter-empty-message' indicates the end of the message stream."
  (let (pub
        (make-pub
         (lambda (req)
           (setf
            (jupyter-request-message-publisher req)
            (jupyter-publisher
              (lambda (msg)
                (when (jupyter-valid-message-p msg)
                  (setf (jupyter-request-last-message req) msg)
                  (cl-callf nconc (jupyter-request-messages req)
                    (list msg))
                  (let ((type (jupyter-message-type msg)))
                    (when (or (jupyter-message-status-idle-p msg)
                              ;; Jupyter protocol 5.1, IPython
                              ;; implementation 7.5.0 doesn't give
                              ;; status: busy or status: idle messages
                              ;; on kernel-info-requests.  Whereas
                              ;; IPython implementation 6.5.0 does.
                              ;; Seen on Appveyor tests.
                              ;;
                              ;; TODO: May be related
                              ;; jupyter/notebook#3705 as the problem
                              ;; does happen after a kernel restart
                              ;; when testing.
                              (string= type "kernel_info_reply")
                              ;; No idle message is received after a
                              ;; shutdown reply so consider REQ as
                              ;; having received an idle message in
                              ;; this case.
                              (string= type "shutdown_reply"))
                      (setf (jupyter-request-idle-p req) t)
                      (jupyter-run-with-io
                          (jupyter-client-io client)
                        (jupyter-publish
                          (list 'idle (jupyter-request-id req)))))))
                (jupyter-content msg)))))))
    (jupyter-run-with-io (jupyter-client-io client)
      (jupyter-publish
        (list
         'publisher
         (jupyter-request-id req)
         (lambda (p)
           (setq pub (or p (funcall make-pub req)))))))
    pub))

(defun jupyter-request (type &rest content)
  "Return an action that sends a `jupyter-request'.
TYPE is the message type of the message that CONTENT, a property
list, represents."
  (declare (indent 1))
  (let ((inhibited-handlers jupyter-inhibit-handlers))
    (jupyter-mlet* ((client (jupyter-get-client)))
      (jupyter-message-subscribed
       (jupyter-mlet* ((client (jupyter-get-client)))
         (let* ((channel (jupyter-channel-from-request-type type))
                (req (jupyter-generate-request
                      client
                      :type type
                      :content content
                      ;; Anything sent to stdin is a reply not a request
                      ;; so consider the "request" completed.
                      :idle-p (string= "stdin" channel)
                      :inhibited-handlers inhibited-handlers))
                (id (jupyter-request-id req))
                ;; As a side effect, this populates the table which
                ;; holds live request publishers if not already
                ;; present for the request.
                (pub (jupyter-message-publisher client req)))
           ;; NOTE The state is assumed to be a list with a client as the
           ;; first element and message subscribers as the rest of the
           ;; elements or just a client.
           (let ((subscribe
                  (jupyter-mlet* ((client (jupyter-pop))
                                  (subscribers (jupyter-get-state)))
                    ;; Enforce an order that the subscribers having
                    ;; earlier subscriptions get called before later.
                    (when subscribers
                      (dolist (sub subscribers)
                        (jupyter-run-with-io pub
                          (jupyter-subscribe sub))))
                    (jupyter-put-state client)))
                 (send
                  (jupyter-mlet* ((client (jupyter-get-client)))
                    (jupyter-run-with-io (jupyter-kernel-io client)
                      (jupyter-publish (list 'send channel type content id)))
                    (jupyter-debug "Send MSG: %s %s %s"
                                   (jupyter-request-id req)
                                   type content)
                    (when (eq jupyter--debug 'message)
                      (push (list client req) jupyter--debug-request-queue))
                    (jupyter-return req))))
             (jupyter-do subscribe send))))
       ;; The subscriber that handles the client handler interface.
       ;; This means the client handlers are called last in the order
       ;; the subscribers are called.
       (lambda (msg)
         (when (and (jupyter-valid-message-p msg)
                    (not (eq jupyter--debug 'message)))
           (when (and (string= type "execute_request")
                      (jupyter-message-status-busy-p msg))
             (jupyter-server-mode-set-client client))
           (let ((channel (jupyter-message-channel msg))
                 (jupyter-inhibit-handlers inhibited-handlers))
             (jupyter-handle-message client channel msg))))))))

(provide 'jupyter-monads)

;;; jupyter-monads.el ends here
