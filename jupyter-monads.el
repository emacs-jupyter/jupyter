(require 'thunk)

(defun jupyter-return (value)
  (declare (indent 0))
  (lambda (_io) value))

;; Adapted from `thunk-delay'
(defmacro jupyter-return-thunk (&rest body)
  (declare (indent 0))
  `(let (forced val)
     (lambda (_io)
       (unless forced
         (setf val (progn ,@body))
         (setf forced t))
       val)))

(defconst jupyter-io-nil (jupyter-return nil))

(defvar jupyter-current-io
  (lambda (&rest args)
	(error "Unhandled IO: %s" args)))

;; TODO: Keep track of the function bound to a io-value such that the
;; function is accessible
(defun jupyter-bind (io-value fn)
  "Bind MVALUE to MFN."
  (declare (indent 1))
  (pcase (funcall io-value jupyter-current-io)
	((and req (cl-struct jupyter-request client)
		  (let jupyter-current-client client))
	 (funcall fn req))
	(`(timeout ,(and req (cl-struct jupyter-request)))
	 (error "Timed out: %s" (cl-prin1-to-string req)))
	(`,value (funcall fn value))))

(defmacro jupyter-mlet* (varlist &rest body)
  (declare (indent 1))
  (letrec ((result (make-symbol "result"))
           (binder
            (lambda (vars)
              (if (zerop (length vars))
                  `(jupyter-return-thunk ,@body)
                `(jupyter-bind ,(cadar vars)
                   (lambda (val)
                     (setq ,(caar vars) val)
                     ,(funcall binder (cdr vars))))))))
    `(let ,(cons result (mapcar #'car varlist))
       ;; nil is bound here to kick off the chain of binds.
       ;; TODO Is it safe to assume nil?
       (jupyter-bind jupyter-io-nil
         ,(funcall binder varlist))
       ,result)))

(defun jupyter--do (&rest mfns)
  (cl-reduce
   (lambda (io-value mfn)
	 (jupyter-bind io-value mfn))
   mfns :initial-value jupyter-io-nil))

(defmacro jupyter-do (io &rest forms)
  (declare (indent 1))
  `(let ((jupyter-current-io ,io))
	 (jupyter--do ,@forms)))

(defun jupyter-after (io-value io-fn)
  "Return an I/O action that binds IO-VALUE to IO-FN.
That is, IO-FN is evaluated after binding IO-VALUE within the I/O
context."
  (declare (indent 1))
  (lambda (_)
	(jupyter-bind io-value io-fn)))


(defun jupyter-idle (io-req)
  (jupyter-after io-req
	(lambda (req)
	  (jupyter-return
	   (if (jupyter-wait-until-idle req) req
		 (list 'timeout req))))))

;; MsgType -> MsgList -> (IO -> Req)
;; (IO -> Req) represents an IO monadic value. IO Req
(defun jupyter-request (type &rest content)
  "Return an IO action that sends a `jupyter-request'.
TYPE is the message type of the message that CONTENT, a property
list, represents.

See `jupyter-io' for more information on IO actions."
  (declare (indent 1))
  (setq type (intern (format ":%s-request"
                             (replace-regexp-in-string "_" "-" type))))
  (lambda (io)
    (let* ((req (make-jupyter-request
                 :client jupyter-current-client
                 :type type
                 :content content))
           (ch (if (memq type '(:input-reply :input-request))
                   :stdin
                 :shell))
           (id (jupyter-request-id req)))
      (letrec ((handler
                (lambda (event)
                  (pcase (car event)
                    ((and 'message (let `(,channel . ,msg) (cdr event))
                          (guard (string= id (jupyter-message-parent-id msg))))
                     (cl-callf nconc (jupyter-request-messages req)
                       (list msg))
                     (when (jupyter--message-completes-request-p msg)
                       (setf (jupyter-request-idle-p req) t)
                       (jupyter-send io 'remove-handler handler)))))))
        (jupyter-send io 'message ch type content id)
        (jupyter-send io 'add-handler handler)
        req))))
