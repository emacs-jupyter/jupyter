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
;;
;; TODO: Allow pcase patterns in mlet*
;;
;;     (jupyter-mlet* ((value (jupyter-server-kernel-io kernel)))
;;       (pcase-let ((`(,kernel-sub ,event-pub) value))
;;         ...))
;;
;;     into
;;
;;     (jupyter-mlet* ((`(,kernel-sub ,event-pub)
;;                      (jupyter-server-kernel-io kernel)))
;;       ...)

;;; Code:

(require 'jupyter-base)

(declare-function jupyter-handle-message "jupyter-client")
(declare-function jupyter-kernel-io "jupyter-client")
(declare-function jupyter-generate-request "jupyter-client")
(declare-function jupyter-wait-until-idle "jupyter-client" (req &optional timeout progress-msg))

(defconst jupyter--return-nil (lambda (state) (cons nil state)))

(defun jupyter-return (value)
  "Return a monadic value wrapping VALUE."
  (declare (indent 0)
           (compiler-macro
            (lambda (exp)
              (cond
               ((null value)
                'jupyter--return-nil)
               ((if (atom value)
                    (not (symbolp value))
                  (eq (car value) 'quote))
                `(lambda (state) (cons ,value state)))
               (t exp)))))
  (lambda (state) (cons value state)))

(defun jupyter-get-state ()
  "Return a monadic valid whose unwrapped value is the current state."
  (lambda (state) (cons state state)))

(defun jupyter-put-state (value)
  "Return a monadic value that sets the current state to VALUE.
The unwrapped value is nil."
  (lambda (_state) (cons nil value)))

(defun jupyter-bind (mvalue mfn)
  "Bind MVALUE to MFN."
  (declare (indent 1))
  (lambda (state)
    (pcase-let* ((`(,value . ,state) (funcall mvalue state)))
      (funcall (funcall mfn value) state))))

(defmacro jupyter-mlet* (varlist &rest body)
  "Bind the monadic values in VARLIST, evaluate BODY.
Return the result of evaluating BODY.  The result of evaluating
BODY should be another monadic value."
  (declare (indent 1) (debug ((&rest (symbolp form)) body)))
  (if (null varlist)
      (if (zerop (length body)) 'jupyter--return-nil
        `(progn ,@body))
    (pcase-let ((`(,name ,mvalue) (car varlist)))
      `(jupyter-bind ,mvalue
         (lambda (,name)
           (jupyter-mlet* ,(cdr varlist)
             ,@body))))))

(defmacro jupyter-do (&rest actions)
  "Return a monadic value that performs all actions in ACTIONS.
The actions are evaluated in the order given.  The result of the
returned action is the result of the last action in ACTIONS."
  (declare (indent 0) (debug (body)))
  (if (zerop (length actions)) 'jupyter--return-nil
    (let ((result (make-symbol "result")))
      `(jupyter-mlet*
           ,(cl-loop
             for action being the elements of actions using (index i)
             for sym = (if (= i (1- (length actions))) result '_)
             collect `(,sym ,action))
         (jupyter-return ,result)))))

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
  (lambda (io)
    (funcall io (list 'subscribe sub))
    (cons nil io)))

(defun jupyter-publish (value)
  "Return an I/O action that submits VALUE to publish as content."
  (declare (indent 0))
  (lambda (io)
    (funcall io (jupyter-content value))
    (cons nil io)))

;;; Working with requests

(define-error 'jupyter-timeout-before-idle "Timeout before idle")

(defun jupyter-sent (dreq)
  (jupyter-mlet* ((client (jupyter-get-state))
                  (req dreq))
    (let ((type (jupyter-request-type req)))
      (jupyter-run-with-io (jupyter-kernel-io client)
        (jupyter-do
          (jupyter-subscribe (jupyter-request-message-publisher req))
          (jupyter-publish
            (list 'send
                  (jupyter-channel-from-request-type type)
                  type
                  (jupyter-request-content req)
                  (jupyter-request-id req))))))
    (jupyter-return req)))

(defun jupyter-idle (dreq &optional timeout)
  "Wait until DREQ has become idle, return DREQ.
Signal a `jupyter-timeout-before-idle' error if TIMEOUT seconds
elapses and the request has not become idle yet."
  (jupyter-mlet* ((req (jupyter-sent dreq)))
    (or (jupyter-wait-until-idle req timeout)
        (signal 'jupyter-timeout-before-idle (list req)))
    (jupyter-return req)))

(defun jupyter-messages (dreq &optional timeout)
  "Return all the messages of REQ.
TIMEOUT has the same meaning as in `jupyter-idle'."
  (jupyter-mlet* ((req (jupyter-idle dreq timeout)))
    (jupyter-return (jupyter-request-messages req))))

(defun jupyter-find-message (msg-type msgs)
  "Return a message whose type is MSG-TYPE in MSGS."
  (cl-find-if
   (lambda (msg)
     (let ((type (jupyter-message-type msg)))
       (string= type msg-type)))
   msgs))

(defun jupyter-reply (dreq &optional timeout)
  "Return the reply message of REQ.
TIMEOUT has the same meaning as in `jupyter-idle'."
  (jupyter-mlet* ((msgs (jupyter-messages dreq timeout)))
    (jupyter-return
      (cl-find-if
       (lambda (msg)
         (let ((type (jupyter-message-type msg)))
           (string-suffix-p "_reply" type)))
       msgs))))

(defun jupyter-result (dreq &optional timeout)
  "Return the result message of REQ.
TIMEOUT has the same meaning as in `jupyter-idle'."
  (jupyter-mlet* ((msgs (jupyter-messages dreq timeout)))
    (jupyter-return
      (cl-find-if
       (lambda (msg)
         (let ((type (jupyter-message-type msg)))
           (string-suffix-p "_result" type)))
       msgs))))

(defun jupyter-message-subscribed (dreq cbs)
  "Return an IO action that subscribes CBS to a request's message publisher.
IO-REQ is an IO action that evaluates to a sent request.  CBS is
an alist mapping message types to callback functions like

    `((\"execute_reply\" ,(lambda (msg) ...))
      ...)

The returned IO action returns the sent request after subscribing
the callbacks."
  (jupyter-mlet* ((req dreq))
    (jupyter-run-with-io
        (jupyter-request-message-publisher req)
      (jupyter-subscribe
        (jupyter-subscriber
          (lambda (msg)
            (when-let*
                ((msg-type (jupyter-message-type msg))
                 (fn (car (alist-get msg-type cbs nil nil #'string=))))
              (funcall fn msg))))))
    (jupyter-return req)))

;; When replaying messages, the request message publisher is already
;; unsubscribed from any upstream publishers.
(defun jupyter--debug-replay-requests ()
  (setq jupyter--debug-request-queue (nreverse jupyter--debug-request-queue))
  (while jupyter--debug-request-queue
    (pcase-let ((`(,client ,req) (pop jupyter--debug-request-queue)))
      (cl-loop
       for msg in (jupyter-request-messages req)
       do (condition-case nil
              (jupyter-handle-message
               client (plist-get msg :channel)
               (cl-list* :parent-request req msg))
            (error (setq jupyter--debug-request-queue
                         (nreverse jupyter--debug-request-queue))))))))

;;; Request

(defun jupyter-message-publisher (req)
  (let ((id (jupyter-request-id req)))
    (jupyter-publisher
      (lambda (msg)
        (pcase (jupyter-message-type msg)
          ;; Send what doesn't appear to be a message as is.
          ((pred null) (jupyter-content msg))
          ;; A status message after a request goes idle means there is
          ;; a new request and there will, theoretically, be no more
          ;; messages for the idle one.
          ;;
          ;; FIXME: Is that true? Figure out the difference between a
          ;; status: busy and a status: idle message.
          ((and type (guard (jupyter-request-idle-p req))
                (guard (string= type "status")))
           (jupyter-unsubscribe))
          ;; TODO: `jupyter-message-parent-id' -> `jupyter-parent-id'
          ;; and the like.
          ((guard (string= id (jupyter-message-parent-id msg)))
           (setf (jupyter-request-last-message req) msg)
           (cl-callf nconc (jupyter-request-messages req) (list msg))
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
                        (string= (jupyter-message-type msg) "kernel_info_reply")
                        ;; No idle message is received after a
                        ;; shutdown reply so consider REQ as
                        ;; having received an idle message in
                        ;; this case.
                        (string= (jupyter-message-type msg) "shutdown_reply"))
                (setf (jupyter-request-idle-p req) t))
              (jupyter-content
               (cl-list* :parent-request req msg))))))))

(defvar jupyter-inhibit-handlers)

(defun jupyter-request (type &rest content)
  "Return an IO action that sends a `jupyter-request'.
TYPE is the message type of the message that CONTENT, a property
list, represents."
  (declare (indent 1))
  (let ((ih jupyter-inhibit-handlers))
    (lambda (client)
      (let* ((req (jupyter-generate-request
                   client
                   :type type
                   :content content
                   :client client
                   ;; Anything sent to stdin is a reply not a request
                   ;; so consider the "request" completed.
                   :idle-p (string= "stdin"
                                    (jupyter-channel-from-request-type type))
                   :inhibited-handlers ih))
             (pub (jupyter-message-publisher req)))
        (setf (jupyter-request-message-publisher req) pub)
        (if (eq jupyter--debug 'message)
            (push (list client req) jupyter--debug-request-queue)
          (when (string= (jupyter-request-type req)
                         "execute_request")
            (jupyter-server-mode-set-client client))
          (jupyter-run-with-io pub
            (jupyter-subscribe
              (jupyter-subscriber
                (lambda (msg)
                  ;; Only handle what looks to be a Jupyter message.
                  (when (jupyter-message-type msg)
                    (let ((channel (plist-get msg :channel)))
                      (jupyter-handle-message client channel msg))))))))
        (cons req client)))))

(provide 'jupyter-monads)

;;; jupyter-monads.el ends here
