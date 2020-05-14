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

;; 

;;; Code:

(defgroup jupyter-monads nil
  "Monadic Jupyter I/O"
  :group 'jupyter)

(defmacro jupyter-return-delayed (&rest body)
  "Return an I/O value that evaluates BODY in the I/O context.
The result of BODY is the unboxed value of the I/O value.  BODY
is evaluated only once."
  (declare (indent 0))
  `(lambda () ,@body))

(defconst jupyter-io-nil (jupyter-return-delayed nil))

(defvar jupyter-current-io
  (lambda (args)
    (pcase args
      (`(,(or 'message 'subscribe) . ,_)
       (error "Not implemented"))
      (_ (error "Unhandled IO: %s" args)))))

;; TODO: How to incorporate `make-thread', `thread-join'?
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
  (pcase (funcall io-value)
	((and req (cl-struct jupyter-request client)
		  (let jupyter-current-client client))
	 (funcall io-fn req))
	(`(timeout ,(and req (cl-struct jupyter-request)))
	 (error "Timed out: %s" (cl-prin1-to-string req)))
	(`,value (funcall io-fn value))))

;; mlet* is like do, but for varlist. The IO values in varlist are
;; bound, just like do, and then body is evaluated in the functional
;; context.
(defmacro jupyter-mlet* (varlist &rest body)
  "Bind the delayed values in VARLIST, evaluate BODY."
  (declare (indent 1))
  (letrec ((val (make-symbol "val"))
           (binder
            (lambda (vars)
              (if (zerop (length vars))
                  `(let ((res (progn ,@body)))
                     (if res (jupyter-return-delayed res)
                       jupyter-io-nil))
                `(jupyter-bind-delayed (cadar vars)
                  (lambda (,val)
                    ,@(unless (eq (caar vars) '_)
                        `((setq ,(caar vars) ,val)))
                    ,(funcall binder (cdr vars))))))))
    `(let (,@(mapcar #'car varlist))
       ,(funcall binder varlist))))

(defmacro jupyter-with-io (io &rest body)
  (declare (indent 1))
  `(let ((jupyter-current-io ,io))
     ,@body))

(defmacro jupyter-do (&rest io-fns)
  (declare (indent 0))
  ;; Thread IO through the monad, return the resulting IO-VALUE.
  (if (zerop (length io-fns))
      'jupyter-io-nil
    `(cl-reduce #'jupyter-bind-delayed
                (list ,@io-fns)
                :initial-value jupyter-io-nil)))

(defun jupyter-after (io-value io-fn)
  "Return an I/O action that binds IO-VALUE to IO-FN.
That is, IO-VALUE is bound to IO-FN within the I/O context."
  (declare (indent 1))
  (jupyter-return-delayed
	(jupyter-bind-delayed io-value io-fn)))

;;; Kernel
;;
;; I/O actions that manage a kernel's lifetime.

;; TODO: Swap definitions with `jupyter-launch', same for the others.
;; (jupyter-launch :kernel "python")
;; (jupyter-launch :spec "python")
(defun jupyter-kernel-launch (&rest args)
  (jupyter-return-delayed
    (let ((kernel (apply #'jupyter-kernel args)))
      (jupyter-launch kernel)
      kernel)))

(defun jupyter-kernel-interrupt (kernel)
  (jupyter-return-delayed
    (jupyter-interrupt kernel)))

;; TODO: Have this notify the I/O context by returning something like
;;
;;     ('shutdown kernel).
(defun jupyter-kernel-shutdown (kernel)
  (jupyter-return-delayed
    (jupyter-shutdown kernel)))

;;; Publisher/subscriber
;;
;; TODO: Wrap the subscriber functions in a struct
;; (cl-defstruct jupyter-subscriber id io ...)
;;
;; TODO: Verify monadic laws.
(defun jupyter-send-content (val)
  "Arrange for VAL to be sent to subscribers of a publisher."
  (list 'content val))

;; In the context external to a publisher, i.e. in the context where a
;; message was published, the content is built up and then published.
;; In the context of a publisher, that content is filtered through
;; PUB-FN before being passed along to subscribers.  So PUB-FN is a
;; filter of published messages.  Subscribers receive filtered
;; messages or no message at all.
(defun jupyter-consume-content (sub-content sub-fn)
  (pcase sub-content
    (`(content ,content) (funcall sub-fn content))
    (`(subscribe ,_) (error "A subscriber cannot be subscribed to"))
    (_ (error "Unhandled content: %s" sub-content))))

(defun jupyter-subscriber (sub-fn)
  "Return a subscriber that consumes content with SUB-FN."
  (declare (indent 0))
  (lambda (sub-content)
    (jupyter-consume-content sub-content sub-fn)))

(defun jupyter-cancel-subscription ()
  "Cancel the current subscriber's subscription.
If this function is called when a subscriber is handling the
messages of a publisher, cancel the subscription."
  (throw 'jupyter-subscription-cancelled t))

(defun jupyter-publisher-context (pub-subs pub-fn value-or-sub)
  (let ((sub-content (if (eq (car-safe value-or-sub) 'subscribe)
                         value-or-sub
                       (funcall pub-fn value))))
    (pcase sub-content
      (`(content ,content)
       (delq nil (mapcar
                  (lambda (pub-sub)
                    (let ((keep nil))
                      (with-demoted-errors "Jupyter: Error in subscriber: %S"
                        (catch 'jupyter-subscription-cancelled
                          (funcall pub-sub content)
                          (setq keep t))
                        (if keep sub))))
                  pub-subs)))
      (`(subscribe ,sub) (cl-pushnew sub pub-subs)))))

(defun jupyter-publisher (pub-fn)
  "Return a publisher that publishes content to subscribers with PUB-FN.
PUB-FN takes a normal value and produces content to send to the
publisher's subscribers.  If no content is published by PUB-FN,
no content is sent to subscribers.  In other words, PUB-FN acts
as a filter of content."
  (declare (indent 0))
  (let ((subs '()))
    (lambda (value-or-sub)
      (setq subs (jupyter-publisher-context subs pub-fn value-or-sub)))))

(defun jupyter-filter-content (pub pub-fn)
  "Compose publisher functions.
Return a publisher subscribed to PUB's content.  The returned
publisher filters content to its subscribers through PUB-FN."
  (declare (indent 1))
  (let ((subscribed-pub (jupyter-publisher pub-fn)))
    (jupyter-with-io pub
      (jupyter-do
        (jupyter-subscribe subscribed-pub)))
    subscribed-pub))

(defun jupyter-subscribe (sub)
  "Return an I/O value subscribing SUB to the current publisher/subscriber."
  (declare (indent 0))
  (jupyter-return-delayed
    (funcall jupyter-current-io (list 'subscribe sub))
    nil))

(defun jupyter-publish (&rest content)
  "Return an I/O value publishing CONTENT."
  (declare (indent 0))
  (jupyter-return-delayed
    (funcall jupyter-current-io content)
    nil))

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
      (cl-labels ((ch-put
                   (ch prop value)
                   (plist-put (plist-get ch-group ch) prop value))
                  (ch-get
                   (ch prop)
                   (plist-get (plist-get ch-group ch) prop))
                  (ch-alive-p
                   (ch)
                   (and (funcall io 'alive-p)
                        (ch-get ch 'alive-p)))
                  (ch-start
                   (ch)
                   (unless (ch-alive-p ch)
                     (funcall io 'message 'start-channel ch
                              (ch-get ch 'endpoint))
                     (continue-after
                      (ch-alive-p ch)
                      (error "Channel not started: %s" ch))))
                  (ch-stop
                   (ch)
                   (when (ch-alive-p ch)
                     (funcall io 'message 'stop-channel ch)
                     (continue-after
                      (not (ch-alive-p ch))
                      (error "Channel not stopped: %s" ch)))))
        (jupyter-io-lambda (_)
          ('start
           (cl-loop
            for ch in channels
            do (ch-start ch)))
          ('stop
           (cl-loop
            for ch in channels
            do (ch-stop ch))
           (and hb (jupyter-hb-pause hb))
           (setq hb nil))
          ('alive-p
           (and (or (null hb) (jupyter-alive-p hb))
                (cl-loop
                 for ch in channels
                 do (ch-alive-p ch))))
          ('hb
           (unless hb
             (setq hb
                   (make-instance
                    'jupyter-hb-channel
                    :session session
                    :endpoint (plist-get endpoints :hb))))
           hb))))))

;; Kernel -> IO Function
(defun jupyter-kernel-websocket-io (kernel)
  (jupyter-launch kernel)
  (pcase-let* (((cl-struct jupyter-server-kernel server id) kernel)
               (websocket (jupyter-api-kernel-websocket server id)))
    (jupyter-websocket-io websocket)))

;;; Websocket IO

;; A monadic function in the I/O stream monad.
;;
;; There is a gap in evaluation time between sending and receiving.
;; We do not know when a message will be received.  A stream fires of
;; message events
(defun jupyter-websocket-io (websocket &optional custom-header-alist)
  (let ((handlers '())
        (events '()))
    (cl-labels
        ((on-message
          (_ws frame)
          ;; This represents a source of new IO messages.
          (cl-case (websocket-frame-opcode frame)
            ((text binary)
             (let* ((msg (jupyter-read-plist-from-string
                          (websocket-frame-payload frame)))
                    ;; TODO: Get rid of some of these explicit/implicit `intern' calls
                    (channel (intern (concat ":" (plist-get msg :channel))))
                    (msg-type (jupyter-message-type-as-keyword
                               (jupyter-message-type msg)))
                    (parent-header (plist-get msg :parent_header)))
               (plist-put msg :msg_type msg-type)
               (plist-put parent-header :msg_type msg-type)
               ;; NOTE: The nil is the identity field expected by a
               ;; `jupyter-channel-ioloop', it is mimicked here.
               (jupyter-run-handlers handlers
                                     (cl-list* 'message channel nil msg))))
            (t
             (error "Unhandled websocket frame opcode (%s)"
                    (websocket-frame-opcode frame))))))
      ;; The pattern is (with-io pub (subscribe subscriber) pub)
      ;; passing along the publisher to attach subscribers.
      (jupyter-io-lambda (&rest args)
        ('message
         (cl-destructuring-bind (channel msg-type msg &optional msg-id) args
           (websocket-send-text
            ws (jupyter-encode-raw-message
                   (plist-get (websocket-client-data ws) :session) msg-type
                 :channel (substring (symbol-name channel) 1)
                 :msg-id msg-id
                 :content msg))))
        ('subscribe )
        ('start (websocket-ensure-connected websocket))
        ('stop (websocket-close websocket))
        ('alive-p (websocket-openp websocket))))))

;;; Request

(defun jupyter-timeout (req)
  (list 'timeout req))

(defun jupyter-idle-wait (req)
  (jupyter-return-delayed
    (if (jupyter-wait-until-idle req) req
      (jupyter-timeout req))))

(defun jupyter-idle (io-req)
  (jupyter-after io-req #'jupyter-idle-wait))

(defun jupyter-request (type &rest content)
  "Return an IO action that sends a `jupyter-request'.
TYPE is the message type of the message that CONTENT, a property
list, represents.

See `jupyter-io' for more information on IO actions."
  (declare (indent 1))
  (setq type (intern (format ":%s-request"
                             (replace-regexp-in-string "_" "-" type))))
  (jupyter-return-delayed
    (let* ((req (make-jupyter-request
                 ;; TODO: `jupyter-with-client' similar to
                 ;; `jupyter-with-io' but on a functional client.
                 :client jupyter-current-client
                 :type type
                 :content content))
           ;; TODO: Figure out if the subscribers are garbage
           ;; collected when the subscription is cancelled.
           (req-msgs-pub
            (jupyter-publisher
              (lambda (event)
                (when (jupyter-request-idle-p req)
                  (jupyter-cancel-subscription))
                (pcase (car event)
                  ((and 'message (let `(,channel . ,msg) (cdr event))
                        ;; TODO: `jupyter-message-parent-id' -> `jupyter-parent-id'
                        ;; and the like.
                        (guard (string= id (jupyter-message-parent-id msg))))
                   (cl-callf nconc (jupyter-request-messages req)
                     (list msg))
                   (when (jupyter--message-completes-request-p msg)
                     (setf (jupyter-request-idle-p req) t))
                   (jupyter-send-content msg))))))
           (ch (if (memq type '(:input-reply :input-request))
                   :stdin
                 :shell))
           (id (jupyter-request-id req)))
      (jupyter-do
        (jupyter-subscribe req-msgs-pub)
        (jupyter-publish 'send ch type content id))
      (list req req-msgs-pub))))

(provide 'jupyter-monads)

;;; jupyter-monads.el ends here

