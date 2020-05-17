;;; jupyter-monads.el --- Monadic Jupyter I/O -*- lexical-binding: t -*-

;; Copyright (C) 2020 Nathaniel Nicandro

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

;; TODO: Add a state monad to log state changes of a kernel, client, etc.
;;
;; TODO: Generalize `jupyter-with-io' and `jupyter-do' for any monad,
;; not just the I/O one.
;;
;; TODO: Rename delayed -> io since any monadic value really
;; represents some kind of delayed value.
;;
;; TODO: Implement seq interface?

;; The context of an I/O action is the current I/O publisher.
;;
;; The context of a publisher is its list of subscribers.
;;
;; The context of a subscriber is whether or not it remains subscribed
;; to a publisher.

;;; Code:

(defgroup jupyter-monads nil
  "Monadic Jupyter I/O"
  :group 'jupyter)

(cl-defstruct jupyter-delayed value)

(defun jupyter-scalar-p (x)
  (or (symbolp x) (numberp x) (stringp x)
      (and (listp x)
           (memq (car x) '(quote function closure)))))

(defconst jupyter-io-nil (make-jupyter-delayed :value (lambda () nil)))

;; TODO: Any monadic value is really a kind of delayed value in some
;; sense, since it represents some staged computation to be evaluated
;; later.  Change the name to `jupyter-return-io' and also change
;; `jupyter-delayed' to `jupyter-io'.
(defun jupyter-return-delayed (value)
  "Return an I/O value that evaluates BODY in the I/O context.
The result of BODY is the unboxed value of the I/O value.  BODY
is evaluated only once."
  (declare (indent 0) (debug (&rest form)))
  (make-jupyter-delayed :value (lambda () value)))

(defvar jupyter-current-io
  (lambda (content)
    (error "Unhandled I/O: %s" content)))

(defun jupyter-bind-delayed (io-value io-fn)
  "Bind IO-VALUE to IO-FN.
Binding causes the evaluation of a delayed value, IO-VALUE (a
closure), in the current I/O context.  The unwrapped
value (result of evaluating the closure) is then passed to IO-FN
which returns another delayed value.  Thus binding involves
unwrapping a value by evaluating a closure and giving the result
to IO-FN which returns another delayed value to be bound at some
future time.  Before, between, and after the two calls to
IO-VALUE and IO-FN, the I/O context is maintained."
  (declare (indent 1))
  (pcase (funcall (jupyter-delayed-value io-value))
	((and req (cl-struct jupyter-request client))
     ;; TODO: If the delayed value is bound and its a request, doesn't
     ;; that mean the request was sent and so the client will already
     ;; be `jupyter-current-client'.
     (let ((jupyter-current-client client))
	   (funcall io-fn req)))
	(`(timeout ,(and req (cl-struct jupyter-request)))
	 (error "Timed out: %s" (cl-prin1-to-string req)))
	(`,value (funcall io-fn value))))

(defmacro jupyter-mlet* (varlist &rest body)
  "Bind the I/O values in VARLIST, evaluate BODY.
Return the result of evaluating BODY, which should be another I/O
value."
  (declare (indent 1) ((&rest (symbolp form)) body))
  (letrec ((value (make-symbol "value"))
           (binder
            (lambda (vars)
              (if (zerop (length vars))
                  (if (zerop (length body)) 'jupyter-io-nil
                    `(progn ,@body))
                (pcase-let ((`(,name ,io-value) (car vars)))
                  `(jupyter-bind-delayed ,io-value
                     (lambda (,value)
                       ,(if (eq name '_)
                            ;; FIXME: Avoid this.
                            `(ignore ,value)
                          `(setq ,name ,value))
                       ,(funcall binder (cdr vars)))))))))
    `(let (,@(delq '_ (mapcar #'car varlist)))
       ,(funcall binder varlist))))

(defmacro jupyter-with-io (io &rest body)
  "Return an I/O action evaluating BODY in IO's I/O context.
The result of the returned action is the result of the I/O action
BODY evaluates to."
  (declare (indent 1) (debug (form body)))
  `(make-jupyter-delayed
    :value (lambda ()
             (let ((jupyter-current-io ,io))
               (jupyter-mlet* ((result (progn ,@body)))
                 result)))))

(defmacro jupyter-run-with-io (io &rest body)
  "Return the result of evaluating the I/O value BODY evaluates to.
The result is return as an I/O value.  All I/O operations are
done in the context of IO."
  (declare (indent 1) (debug (form body)))
  `(jupyter-mlet* ((result (jupyter-with-io ,io
                             ,@body)))
     result))

;; do (for the IO monad) takes IO actions (IO values), which are
;; closures of zero argument wrapped in the `jupyter-delay' type, and
;; evaluates them in sequence one after the other.  In the IO monad,
;; composition is equivalent to one IO action being performed after
;; the other.
;;
;; Based on explanations at
;; https://wiki.haskell.org/Introduction_to_Haskell_IO/Actions
(defmacro jupyter-do (&rest io-actions)
  "Return an I/O action that performs all actions in IO-ACTIONS.
The actions are evaluated in the order given.  The result of the
returned action is the result of the last action in IO-ACTIONS."
  (declare (indent 0))
  (if (zerop (length io-actions)) 'jupyter-io-nil
    (letrec ((before
              (lambda (io-actions)
                (if (= (length io-actions) 1) (car io-actions)
                  `(jupyter-then ,(funcall before (cdr io-actions))
                     ,(car io-actions))))))
      (funcall before (reverse io-actions)))))

(defun jupyter-then (io-a io-b)
  "Return an I/O action that performs IO-A then IO-B.
The result of the returned action is the result of IO-B."
  (declare (indent 1))
  (make-jupyter-delayed
   :value (lambda ()
            (jupyter-mlet* ((_ io-a)
                            (result io-b))
              result))))

;;; Kernel
;;
;; I/O actions that manage a kernel's lifetime.

;; TODO: Swap definitions with `jupyter-launch', same for the others.
;; (jupyter-launch :kernel "python")
;; (jupyter-launch :spec "python")
(defun jupyter-kernel-launch (&rest args)
  (make-jupyter-delayed
   :value (lambda ()
            (let ((kernel (apply #'jupyter-kernel args)))
              (jupyter-launch kernel)
              kernel))))

(defun jupyter-kernel-interrupt (kernel)
  (make-jupyter-delayed
   :value (lambda ()
            (jupyter-interrupt kernel)
            kernel)))

(defun jupyter-kernel-shutdown (kernel)
  (make-jupyter-delayed
   :value (lambda ()
            (jupyter-shutdown kernel)
            kernel)))

;;; Publisher/subscriber
;;
;; TODO: Wrap the subscriber functions in a struct
;; (cl-defstruct jupyter-subscriber id io ...)
;;
;; TODO: Verify monadic laws.

(define-error 'jupyter-subscribed-subscriber
  "A subscriber cannot be subscribed to.")

(defun jupyter-subscriber (fn)
  "Return a subscriber evaluating FN for side-effects on published content."
  (declare (indent 0))
  (lambda (sub-content)
    ;; TODO: fn -> fun
    (pcase sub-content
      (`(content ,content) (funcall fn content))
      (`(subscribe ,_) (signal 'jupyter-subscribed-subscriber nil))
      (_ (error "Unhandled content: %s" sub-content)))))

(defun jupyter-send-content (value)
  "Arrange for VALUE to be sent to subscribers of a publisher."
  (list 'content value))

(defsubst jupyter-unsubscribe ()
  "Arrange for the current subscription to be canceled.
A subscriber (or publisher with a subscription) can return the
result of this function to cancel its subscription with the
publisher providing content."
  (list 'unsubscribe))

;; Bind the publisher's submitted content to the subscribers.
;; Filtering through PUB-FN first.  Subscribers return whether or not
;; their subscription should be kept.  The result of binding is the
;; remaining list of subscribers.
;;
;; Subscribers (Content a) -> (a -> Content b) -> Subscribers ()
;;
;; NOTE: This is called pseudo-bind since, to be monadic, the
;; signature should be something like
;;
;;     Subscribers FilteredContent a -> (a -> FilteredContent b) -> Subscribers FilteredContent b
(defun jupyter-pseudo-bind-subscribers (subs-value pub-fn)
  (pcase (funcall pub-fn (cdr subs-value))
    ((and `(content ,_) sub-content)
     (mapcar
      (lambda (sub)
        (condition-case error
            ;; This recursion may be a problem if
            ;; there is a lot of content filtering (by
            ;; subscribing publishers to publishers).
            (pcase (funcall sub sub-content)
              ('(unsubscribe) nil)
              (_ sub))
          (error
           (message "Jupyter: I/O subscriber error: %S"
                    (error-message-string error))
           ;; Keep the subscription on error.
           sub)))
      (car subs-value)))
    (_ (car subs-value))))

;; In the context external to a publisher, i.e. in the context where a
;; message was published, the content is built up and then published.
;; In the context of a publisher, that content is filtered through
;; PUB-FN before being passed along to subscribers.  So PUB-FN is a
;; filter of content.  Subscribers receive filtered content or no
;; content at all depending on if a value wrapped by
;; `jupyter-send-content' is returned by PUB-FN or not.
(defun jupyter-publisher (&optional pub-fn)
  "Return a publisher that publishes content to subscribers.
PUB-FN is a function that takes a normal value and produces
content to send to the publisher's subscribers (by returning the
result of `jupyter-send-content' on a value).  If no content is
sent by PUB-FN, no content is sent to subscribers.  The default
for PUB-FN is `jupyter-send-content'.

Ex. Publish the value 1 regardless of what is given to PUB-FN.

    (jupyter-publisher
      (lambda (_)
        (jupyter-send-content 1)))

Ex. Publish 'app if 'app is given to a publisher, nothing is sent
    to subscribers otherwise.  In this case, a publisher is a
    filter of the value given to it for publishing.

    (jupyter-publisher
      (lambda (value)
        (if (eq value 'app)
          (jupyter-send-content value))))"
  (declare (indent 0))
  (let ((subs '())
        (pub-fn (or pub-fn #'jupyter-send-content)))
    ;; A publisher value is either a value representing a subscriber
    ;; or a value representing content to send to subscribers.
    (lambda (pub-value)
      (pcase (car-safe pub-value)
        ('content (setq subs (delq nil (jupyter-pseudo-bind-subscribers
                                        (cons subs (cadr pub-value)) pub-fn))))
        ('subscribe (cl-pushnew (cadr pub-value) subs)))
      nil)))
;; In the publisher context, subscriber content is the monadic value
;; and the monadic functions are those functions that return content
;; to send to subscribers.  A publishing function like PUB-FN is
;; actually not monadic since it does not always return content
;; (because content can be filtered).
;;
;; The result of filtering a publisher's content is a publisher that
;; publishes filtered content.
(defun jupyter-filter-content (pub pub-fn)
  "Return a publisher subscribed to PUB's content.
The returned publisher filters content to its subscribers through
PUB-FN."
  (declare (indent 1))
  (let ((sub (jupyter-publisher pub-fn)))
    (jupyter-run-with-io pub
      (jupyter-subscribe sub))
    sub))

(defun jupyter-consume-content (pub sub-fn)
  "Return a subscriber subscribed to PUB's content.
The subscriber evaluates SUB-FN on the published content."
  (declare (indent 1))
  (let ((sub (jupyter-subscriber sub-fn)))
    (jupyter-run-with-io pub
      (jupyter-subscribe sub))
    sub))

(defsubst jupyter--subscribe (sub)
  (list 'subscribe sub))

(defun jupyter-subscribe (sub)
  "Return an I/O action that subscribes SUB to published content.
If a subscriber (or a publisher with a subscription to another
publisher) returns the result of `jupyter-unsubscribe', its
subscription is canceled.

Ex. Subscribe to a publisher and unsubscribe after receiving two
    messages.

    (let* ((msgs '())
           (pub (jupyter-publisher))
           (sub (jupyter-subscriber
                  (lambda (n)
                    (if (> n 2) (jupyter-unsubscribe)
                      (push n msgs))))))
      (jupyter-run-with-io pub
        (jupyter-subscribe sub))
      (cl-loop
       for x in '(1 2 3)
       do (jupyter-run-with-io pub
            (jupyter-publish x)))
      (reverse msgs)) ; => '(1 2)"
  (declare (indent 0))
  (make-jupyter-delayed
   :value (lambda ()
            (funcall jupyter-current-io (jupyter--subscribe sub))
            nil)))

(defun jupyter-publish (value)
  "Return an I/O action that publishes VALUE as content.
The content will be sent to the subscribers of the publisher in
whatever I/O context the action is evaluated in."
  (declare (indent 0))
  (make-jupyter-delayed
   :value (lambda ()
            (funcall jupyter-current-io (jupyter-send-content value))
            nil)))

;;; IO Event

(defun jupyter-channel-io (session)
  (let* ((channels '(:shell :iopub :stdin))
         (ch-group
          (cl-loop
           with endpoints = (jupyter-session-endpoints session)
           for ch in channels
           collect ch
           collect (list 'endpoint (plist-get endpoints ch)
                         'alive-p nil))))
    (cl-macrolet ((continue-after
                   (cond on-timeout)
                   `(jupyter-with-timeout
                        (nil jupyter-default-timeout ,on-timeout)
                      ,cond)))
      (cl-labels
          ((ch-put
            (ch prop value)
            (plist-put (plist-get ch-group ch) prop value))
           (ch-get
            (ch prop)
            (plist-get (plist-get ch-group ch) prop))
           (ch-alive-p
            (ch)
            (ch-get ch 'alive-p))
           (ch-start
            (ch)
            (unless (ch-alive-p ch)
              ;; FIXME: Bring in ioloop?  See `jupyter-kernel-process'.
              (jupyter-run-with-io ioloop
                (jupyter-do
                  (jupyter-publish
                    'start-channel ch (ch-get ch 'endpoint)
                    ;; TODO: Make this actually work. Send a
                    ;; start-channel event and pass it an IO
                    ;; context that sets the alive-p flag for
                    ;; the channel in this current IO context.
                    ;; The ioloop will send a notification to
                    ;; this I/O context if the channel dies.
                    (jupyter-subscriber
                      (lambda (alive-p)
                        (ch-put ch 'alive-p alive-p))))
                  (jupyter-return-delayed
                    (continue-after
                     (ch-alive-p ch)
                     (error "Channel not started: %s" ch)))))))
           (ch-stop
            (ch)
            (when (ch-alive-p ch)
              (jupyter-run-with-io ioloop
                (jupyter-do
                  (jupyter-publish 'stop-channel ch)
                  (jupyter-return-delayed
                    (continue-after
                     (not (ch-alive-p ch))
                     (error "Channel not stopped: %s" ch))))))))
        (list
         (jupyter-subscriber
           (lambda (msg)
             (pcase msg
               ('start
                (cl-loop
                 for ch in channels
                 do (ch-start ch)))
               ('stop
                (cl-loop
                 for ch in channels
                 do (ch-stop ch))
                (and hb (jupyter-hb-pause hb))
                (setq hb nil)))))
         (jupyter-publisher
           (lambda (_status)
             (unless hb
               (setq hb
                     (make-instance
                      'jupyter-hb-channel
                      :session session
                      :endpoint (plist-get endpoints :hb))))
             (jupyter-send-content
              (append (list :hb hb)
                      (cl-loop
                       for ch in channels
                       collect ch and collect (ch-alive-p ch)))))))))))

;;; Websocket IO

(defun jupyter--websocket (kernel)
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
  (cl-assert (cl-typep kernel 'jupyter-server-kernel))
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

;;; Request

(defun jupyter-timeout (req)
  (list 'timeout req))

(defun jupyter-idle (io-req)
  (jupyter-then io-req
    (lambda (req)
      (jupyter-return-delayed
        (if (jupyter-wait-until-idle req) req
          (jupyter-timeout req))))))
(defun jupyter-request (type &rest content)
  "Return an IO action that sends a `jupyter-request'.
TYPE is the message type of the message that CONTENT, a property
list, represents.

See `jupyter-io' for more information on IO actions."
  (declare (indent 1))
  (setq type (intern (format ":%s-request"
                             (replace-regexp-in-string "_" "-" type))))
  ;; FIXME: Implement `jupyter-new-request-publisher'
  ;;
  ;; Build up a request and return an I/O action that sends it.  After
  ;; sending, the kernel client in the I/O context
  (let* ((msgs '())
         ;; Messages may still be arriving for the most recent
         ;; request, e.g. stdout message of an already idle request.
         ;; This variable ensures requests are not unsubscribed from a
         ;; kernel's message too early by only unsubscribing when they
         ;; aren't the most recent request.
         (most-recent t)
         (ch (if (memq type '(:input-reply :input-request))
                 :stdin
               :shell))
         (req-complete-pub (jupyter-publisher))
         (req (make-jupyter-request
               ;; TODO: `jupyter-with-client' similar to
               ;; `jupyter-with-io' but on a functional client.
               :client jupyter-current-client
               ;; The handler methods of client that are suppressed
               ;; from being evaluated.
               :inhibited-handlers jupyter-inhibit-handlers
               :type type
               :content content))
         (id (jupyter-request-id req))
         (req-msgs-pub
          (jupyter-publisher
            (lambda (value)
              (cond
               ((and (not most-recent) (jupyter-request-idle-p req))
                (jupyter-run-with-io req-complete-pub
                  (jupyter-publish req))
                (jupyter-unsubscribe))
               (t
                (pcase value
                  ((and `(,channel . ,msg)
                        ;; TODO: `jupyter-message-parent-id' -> `jupyter-parent-id'
                        ;; and the like.
                        (guard (string= id (jupyter-message-parent-id msg))))
                   (push msg msgs)
                   (when (or (jupyter-message-status-idle-p msg)
                             ;; Jupyter protocol 5.1, IPython implementation 7.5.0
                             ;; doesn't give status: busy or status: idle messages on
                             ;; kernel-info-requests.  Whereas IPython implementation
                             ;; 6.5.0 does.  Seen on Appveyor tests.
                             ;;
                             ;; TODO: May be related jupyter/notebook#3705 as the
                             ;; problem does happen after a kernel restart when
                             ;; testing.
                             (eq (jupyter-message-type msg) :kernel-info-reply)
                             ;; No idle message is received after a shutdown reply so
                             ;; consider REQ as having received an idle message in
                             ;; this case.
                             (eq (jupyter-message-type msg) :shutdown-reply))
                     (setf (jupyter-request-messages req) (nreverse msgs))
                     (setf (jupyter-request-idle-p req) t))
                   (jupyter-send-content value)))))))))
    (jupyter-do
      (jupyter-subscribe req-msgs-pub)
      (jupyter-publish (list 'send ch type content id))
      (jupyter-with-io
          (jupyter-new-request-publisher jupyter-current-client)
        (jupyter-do
          ;; Subscribers to the new request publisher of a
          ;; client are given another publisher to subscribe
          ;; to.  When the request is complete, the publisher
          ;; publishes the actual request object.
          ;;
          ;; TODO: Is there a better approach?  Maybe have a
          ;; new and completed request publisher of a client.
          ;; This way we avoid having to create a
          ;; req-complete-pub on every new request.
          (jupyter-publish req-complete-pub)
          (jupyter-subscribe
            (jupyter-subscriber
              (lambda (_)
                (setq most-recent nil)
                (jupyter-unsubscribe))))
          (jupyter-return-delayed req-msgs-pub))))))

(provide 'jupyter-monads)

;;; jupyter-monads.el ends here

