;;; jupyter-org-client.el --- Org integration -*- lexical-binding: t -*-

;; Copyright (C) 2018-2024 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 02 Jun 2018

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

;; A subclass of a Jupyter kernel client that integrates with `org-mode'
;; src-blocks.

;;; Code:

(require 'jupyter-repl)
(require 'ob)

(declare-function org-babel-python-table-or-string "ob-python" (results))
(declare-function org-babel-jupyter-initiate-session "ob-jupyter" (&optional session params))
(declare-function org-babel-jupyter-src-block-session "ob-jupyter" ())
(declare-function org-babel-jupyter-session-initiated-p "ob-jupyter" (params))
(declare-function org-babel-jupyter-language-p "ob-jupyter" (lang))
(declare-function org-element-context "org-element" (&optional element))
(declare-function org-element-create "org-element" (type &optional props &rest children))
(declare-function org-element-type "org-element" (element))
(declare-function org-element-normalize-string "org-element" (s))
(declare-function org-element-at-point "org-element" ())
(declare-function org-element-parse-buffer "org-element" (&optional granularity visible-only))
(declare-function org-element-map "org-element"
                  (data types fun &optional info first-match no-recursion with-affiliated))
(declare-function org-drag-element-forward "org-element" ())
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-set-contents "org-element" (element &rest contents))
(declare-function org-element-latex-fragment-parser "org-element" ())
(declare-function org-element-latex-environment-parser "org-element" (limit affiliated))
(declare-function org-element-interpret-data "org-element" (data))
(declare-function org-element-put-property "org-element" (element property value))
(declare-function orgtbl-to-orgtbl "org-table" (table params))
(declare-function org-table-align "org-table" ())
(declare-function org-in-src-block-p "org" (&optional inside))
(declare-function org-next-block "org" (arg &optional backward block-regexp))
(declare-function org-at-table-p "org" ())
(declare-function org-inside-LaTeX-fragment-p "org" ())
(declare-function org-latex-preview "org" (&optional arg))

(defcustom jupyter-org-auto-connect t
  "Automatically establish a connection to a src-block session.
If this variable is non-nil, then a connection to a src-block
session is automatically established under certain conditions,
e.g. during auto-completion.  Otherwise there would have to be an
available connection already if this variable is nil for features
like auto-completion to work.

When this variable is nil, you can establish a connection to a
session by, for example, executing a src-block."
  :group 'ob-jupyter
  :type 'boolean)

(defcustom jupyter-org-queue-requests nil
  "Whether or not source block evaluations should be queued.
When this variable is nil and, for example, multiple source
blocks are executed in rapid succession the underlying
\"execute_request\" messages are sent to the kernel immediately
and are queued on the kernel side so that when one of the source
blocks raises an error, the kernel will typically just execute
the next \"execute_request\" message queued up so the effect is
that source blocks that come after the failed one are executed.
Some may find this behavior undesirable.

Instead, when this variable is non-nil, the \"execute_request\"
messages of the source blocks are queued on the client side and
whenever one of the source blocks raises an error, all of the
queued \"execute_request\" messages are aborted and don't get
sent to the kernel so the effect is that source blocks that come
after the failed one are not executed."
  :group 'ob-jupyter
  :type 'boolean)

(defcustom jupyter-org-display-execution-time nil
  "Whether or not to display the execution time of a source block.
If this variable is nil, the execution times are not displayed.
When it is t, display the execution times regardless of how long
it took to execute.  When it is a number, display the execution
time when it is longer than that many seconds.

To clear the execution time information from the source block
simply edit it or call `jupyter-org-clear-execution-time'."
  :group 'ob-jupyter
  :type '(choice (const :tag "Never display" nil)
                 (const :tag "Always display" t)
                 (number :tag "Display when above this threshold (in seconds)")))

(defcustom jupyter-org-resource-directory "./.ob-jupyter/"
  "Directory used to store automatically generated image files.
See `jupyter-org-image-file-name'."
  :group 'ob-jupyter
  :type 'string)

(defcustom jupyter-org-toggle-latex t
  "Whether to automatically display latex fragments or not.
If a source block returns LaTeX fragments, LaTeX images will
automatically be shown if this is non-nil."
  :group 'ob-jupyter
  :type 'boolean)

(defcustom jupyter-org-pandoc-convertable
  '("html" "markdown" "latex")
  "Export blocks to convert to `org-mode' when ':pandoc t' header is set."
  :group 'ob-jupyter
  :type '(repeat string))

(defcustom jupyter-org-adjust-image-size t
  "Try to best fit image output in the result block.

If non-nil, and `org-image-actual-width' is set to a list, the
image will not be stretched if its width is smaller than \(car
`org-image-actual-width'\).  This is done by inserting an
#+ATTR_ORG keyword above the file path.

See also the docstring of `org-image-actual-width' for more details."
  :group 'ob-jupyter
  :type 'boolean)

(defconst jupyter-org-mime-types '(:text/org
                                   ;; Prioritize images over html
                                   :image/svg+xml :image/jpeg :image/png
                                   :text/html :text/markdown
                                   :text/latex :text/plain)
  "MIME types handled by Jupyter Org.")

(defclass jupyter-org-client (jupyter-repl-client)
  ((most-recent-request
    :type (or jupyter-request null)
    :initform nil
    :initarg :most-recent-request
    :documentation "The most recently sent request.")
   (last-queued-request
    :type (or jupyter-request null)
    :initform nil
    :initarg :last-queued-request
    :documentation "The last queued request.")))

(cl-defstruct (jupyter-org-request
               (:include jupyter-request)
               (:constructor nil)
               (:constructor jupyter-org-request))
  result-type
  block-params
  file
  results
  silent-p
  id-cleared-p
  inline-block-p
  marker
  async-p
  overlay)

(defvar org-babel-jupyter-resolving-reference-p)

(defun jupyter-org-execute-async-p (params)
  "Return non-nil if an execution should be asynchronous based on PARAMS.

PARAMS are the source block arguments as returned by,
e.g. `org-babel-get-src-block-info'."
  (and (member (alist-get :async params) '("yes" nil))
       (not org-babel-jupyter-resolving-reference-p)))

(defmacro jupyter-org-with-point-at (req &rest body)
  "Move to the associated marker of REQ while evaluating BODY.
If the marker points nowhere don't evaluate BODY, just do
nothing and return nil."
  (declare (indent 1))
  `(pcase-let (((cl-struct jupyter-org-request marker) ,req))
     (when (and (marker-buffer marker) (marker-position marker))
       (org-with-point-at marker
         ,@body))))

;;; `jupyter-kernel-client' interface

;;;; `jupyter-request' interface

(defvar org-babel-jupyter-current-src-block-params)

(defun jupyter-org--make-overlay  (beg end &optional inline)
  "Create overlay between BEG and END positions and return it."
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'jupyter t)
    (overlay-put overlay 'face 'secondary-selection)
    (let ((read-only
	       (list
	        (lambda (&rest _)
	          (user-error
	           "Cannot modify an area of a source block being executed")))))
      (overlay-put overlay 'modification-hooks read-only)
      (overlay-put overlay 'insert-in-front-hooks read-only)
      (unless inline
        (overlay-put overlay 'insert-behind-hooks read-only)))
    overlay))

(defun jupyter-org--remove-overlay (req)
  (when (overlayp (jupyter-org-request-overlay req))
    (delete-overlay (jupyter-org-request-overlay req))))

(cl-defmethod jupyter-generate-request ((_client jupyter-org-client) &rest slots)
  "Return a `jupyter-org-request' for the current source code block."
  (if (and org-babel-current-src-block-location
           org-babel-jupyter-current-src-block-params
           (provided-mode-derived-p
            (buffer-local-value
             ;; Handle indirect buffers used by packages like polymode, see #171.
             'major-mode (or (buffer-base-buffer) (current-buffer)))
            'org-mode))
      ;; Only use a `jupyter-org-request' when executing code blocks, setting
      ;; the `major-mode' context isn't enough, consider when a client is
      ;; started due to sending a completion request.
      (save-excursion
        (goto-char org-babel-current-src-block-location)
        (jupyter-org-clear-execution-time)
        (let* ((context (org-element-context))
               (block-params org-babel-jupyter-current-src-block-params)
               (result-params (alist-get :result-params block-params))
               (req
                (apply #'jupyter-org-request
                       (append
                        (list
                         :marker (copy-marker org-babel-current-src-block-location)
                         :inline-block-p (and (memq (org-element-type context)
                                                    '(inline-babel-call inline-src-block))
                                              t)
                         :result-type (alist-get :result-type block-params)
                         :file (alist-get :file block-params)
                         :block-params block-params
                         :async-p (jupyter-org-execute-async-p block-params)
                         :silent-p (car (or (member "none" result-params)
                                            (member "silent" result-params))))
                        slots))))
          (put-text-property
           org-babel-current-src-block-location
           (1+ org-babel-current-src-block-location)
           'jupyter-request req)
          (setf (jupyter-org-request-overlay req)
                (pcase (org-element-type context)
                  (`src-block
                   (jupyter-org--make-overlay
                    (save-excursion
                      (goto-char (jupyter-org-element-begin-after-affiliated context))
                      (line-beginning-position 2))
                    (jupyter-org-element-contents-end context)))
                  ((and type (or `inline-src-block `babel-call `inline-babel-call))
                   (jupyter-org--make-overlay
                    (jupyter-org-element-begin-after-affiliated context)
                    (jupyter-org-element-end-before-blanks context)
                    (memq type '(inline-src-block babel-call inline-babel-call))))))
          req))
    (cl-call-next-method)))

(defun jupyter-org-request-at-point ()
  "Return the `jupyter-org-request' associated with `point' or nil."
  (when-let* ((context (org-element-context))
              (babel-p (memq (org-element-type context)
                             '(src-block babel-call
                               inline-babel-call inline-src-block)))
              (pos (jupyter-org-element-begin-after-affiliated context))
              (req (get-text-property pos 'jupyter-request)))
    (and (not (jupyter-request-idle-p req))
         req)))

;;;; Stream

(cl-defmethod jupyter-handle-stream ((_client jupyter-org-client) (req jupyter-org-request) msg)
  (jupyter-with-message-content msg (text)
    (if (jupyter-org-request-inline-block-p req)
        (jupyter-with-display-buffer "org-results" req
          (jupyter-with-insertion-bounds
              beg end (insert text)
            (ansi-color-apply-on-region beg end))
          (jupyter-display-current-buffer-reuse-window))
      (jupyter-org--add-result req text))))

;;;; Errors

(defvar jupyter-org-goto-error-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] #'jupyter-org-goto-error)
    (define-key map (kbd "RET") #'jupyter-org-goto-error)
    map)
  "Keymap for jumping to an error in a source code block.")

(defun jupyter-org-goto-error ()
  "Go to the error location specified by the jupyter-error-loc text property.
If `point' has a non-nil jupyter-error-loc property, jump to that
line in the previous source block.  See
`jupyter-org-error-location'."
  (interactive)
  (when-let* ((loc (get-text-property (point) 'jupyter-error-loc)))
    (with-current-buffer (marker-buffer loc)
      (jupyter-display-current-buffer-reuse-window))
    (goto-char loc)))

;;;;; `jupyter-org-error-location'
;; Inspiration from https://kitchingroup.cheme.cmu.edu/blog/2017/06/10/Adding-keymaps-to-src-blocks-via-org-font-lock-hook/

(defconst jupyter-org--goto-error-string "[goto error]")

(cl-defgeneric jupyter-org-error-location ()
  "Return the line number corresponding to an error from a traceback.
This method is called with `point' at `point-min' in a buffer
containing the traceback of the last error that occurred due to
execution of a source block.  It should return the line number
relative to the source block that caused the error or nil if a
line number could not be found."
  (ignore))

(defun jupyter-org--goto-error-string (req)
  (let* ((buffer (current-buffer))
         (loc (jupyter-org-with-point-at req
                (forward-line (or (with-current-buffer buffer
                                    (save-excursion
                                      (goto-char (point-min))
                                      (jupyter-org-error-location)))
                                  0))
                (point-marker))))
    (propertize jupyter-org--goto-error-string
                'jupyter-error-loc loc
                'face 'link
                'keymap jupyter-org-goto-error-map)))

(defun jupyter-org-add-error-keymap (limit)
  "Add keymaps to text that contain a jupyter-error-loc property.
Search up to LIMIT from `point' for any text to add the keymap
to."
  (save-restriction
    (narrow-to-region (point) limit)
    (let ((pos (point)) end)
      (while (setq pos (next-single-property-change pos 'jupyter-error-loc))
        (when (get-text-property pos 'jupyter-error-loc)
          (setq end (next-single-property-change pos 'jupyter-error-loc))
          (put-text-property pos end 'keymap jupyter-org-goto-error-map)
          (setq pos end))))))

;;;;; Handler

(defvar org-font-lock-hook)

(cl-defmethod jupyter-handle-error ((_client jupyter-org-client) (req jupyter-org-request) msg)
  (jupyter-with-message-content msg (traceback)
    (setq traceback (org-element-normalize-string
                     (mapconcat #'identity traceback "\n")))
    (pcase-let (((cl-struct jupyter-org-request inline-block-p silent-p) req))
      (cond
       ((or inline-block-p silent-p)
        ;; Remove old inline results when an error happens since, if this was not
        ;; done, it would look like the code which caused the error produced the
        ;; old result.
        (when inline-block-p
          (jupyter-org-with-point-at req
            (org-babel-remove-inline-result)))
        (jupyter-with-display-buffer "traceback" 'reset
          (jupyter-insert-ansi-coded-text traceback)
          (goto-char (point-min))
          (when silent-p
            (insert (jupyter-org--goto-error-string req) "\n\n"))
          (jupyter-display-current-buffer-reuse-window)))
       (t
        ;; The keymap property in the string returned by
        ;; `jupyter-org--goto-error-string' gets removed by font-lock so ensure it
        ;; is re-added.
        (unless (memq 'jupyter-org-add-error-keymap org-font-lock-hook)
          (add-hook 'org-font-lock-hook 'jupyter-org-add-error-keymap nil t))
        (jupyter-org--add-result
         req (jupyter-org-comment
              (with-temp-buffer
                (insert traceback)
                (jupyter-org--goto-error-string req))))
        (jupyter-org--add-result req traceback))))))

;;;; Execute result

(cl-defmethod jupyter-handle-execute-result ((_client jupyter-org-client) (req jupyter-org-request) msg)
  (unless (eq (jupyter-org-request-result-type req) 'output)
    (jupyter-with-message-content msg (data metadata)
      (when (jupyter-org-request-inline-block-p req)
        ;; For inline results, only text/plain results are allowed at the moment.
        ;;
        ;; TODO: Handle all of the different macro types for inline results, see
        ;; `org-babel-insert-result'.
        (setq data `(:text/plain ,(plist-get data :text/plain))))
      (jupyter-org--add-result req data metadata))))

;;;; Display data

(cl-defmethod jupyter-handle-display-data ((_client jupyter-org-client) (req jupyter-org-request) msg)
  ;; TODO: Add request objects as text properties of source code blocks to
  ;; implement display IDs.  Or how can #+NAME be used as a display ID?
  ;;
  ;; Only the data of the execute-result message is inserted into the buffer
  ;; for inline code blocks.
  (jupyter-with-message-content msg (data metadata)
    (if (jupyter-org-request-inline-block-p req)
        (jupyter-with-display-buffer "org-results" req
          (jupyter-insert data metadata)
          (jupyter-display-current-buffer-reuse-window)
          (set-window-point (get-buffer-window (current-buffer)) (point-min)))
      (jupyter-org--add-result req data metadata))))

;;;; Execute reply

(cl-defmethod jupyter-handle-payload ((_source (eql set_next_input)) pl
                                      &context (major-mode org-mode))
  ;; Assumes `point' is at a src-block element
  (let ((src-block (org-element-at-point))
        (result-p (org-babel-where-is-src-block-result)))
    (save-excursion
      (goto-char (jupyter-org-element-end-before-blanks src-block))
      (forward-line -1)
      ;; Create an empty src-block after the current one but before any of the
      ;; current source block's results
      (org-babel-demarcate-block)
      (org-next-block 1)
      (when result-p
        (org-drag-element-forward))
      (forward-line)
      (insert (org-element-normalize-string (plist-get pl :text))))))

(defun jupyter-org--display-execution-time (req)
  "In the Org buffer of REQ, show the REQ's execution time."
  (pcase jupyter-org-display-execution-time
    ((and (or (and `t
                   (let time (jupyter-execution-time req)))
              (and (pred numberp) secs
                   (let time (jupyter-execution-time req))
                   (guard (> time secs))))
          (let (cl-struct jupyter-org-request inline-block-p) req)
          (guard (not inline-block-p)))
     (jupyter-org-with-point-at req
       (let* ((src-block (org-element-at-point))
              (ov (make-overlay
                   (org-element-property :begin src-block)
                   ;; Exclude the newline to make it look like
                   ;;
                   ;;    #+end_src Execution time ...
                   (1- (jupyter-org-element-end-before-blanks src-block)))))
         (let ((delete
                (list (lambda (&rest _)
                        (delete-overlay ov)))))
           (overlay-put ov 'evaporate t)
           (overlay-put ov 'jupyter-execution-time time)
           (overlay-put ov 'modification-hooks delete)
           (overlay-put ov 'insert-in-front-hooks delete)
           (overlay-put ov 'insert-behind-hooks delete))
         (overlay-put
          ov 'after-string
          (concat " " (propertize
                       (format "Execution time: %s"
                               (jupyter-format-time time))
                       'face 'bold-italic))))))))

(defun jupyter-org-clear-execution-time ()
  "Clear the execution time overlay for the source block at point."
  (interactive)
  (let ((el (org-element-at-point)))
    (pcase (org-element-type el)
      ((or `src-block `babel-call)
       (dolist (ov (overlays-at (org-element-property :begin el)))
         (when (overlay-get ov 'jupyter-execution-time)
           (delete-overlay ov)))))))

(cl-defmethod jupyter-handle-execute-reply ((_client jupyter-org-client) (req jupyter-org-request) msg)
  (jupyter-org--display-execution-time req)
  (jupyter-with-message-content msg (status payload)
    (when payload
      (jupyter-org-with-point-at req
        (jupyter-handle-payload payload)))
    (jupyter-org--remove-overlay req)
    (if (equal status "ok")
        (message "Code block evaluation complete.")
      (message "An error occurred when evaluating code block."))
    (when (jupyter-org-request-async-p req)
      (jupyter-org--clear-async-indicator req)
      (jupyter-org-with-point-at req
        (run-hooks 'org-babel-after-execute-hook)))))

;;; Queueing requests

;;;###autoload
(defun jupyter-org-toggle-request-queuing ()
  "Toggle on or off client side queuing."
  (interactive)
  (setq jupyter-org-queue-requests
        (not jupyter-org-queue-requests)))

(defun jupyter-org-abort (req)
  "Abort REQ.
Set the request as being idle.  Remove any indication that REQ is
a running execute_request from the Org buffer.  Publish an abort
message down the chain of subscribers to the REQ's message
publisher to indicate that any subsequent, queued, requests
should also be aborted."
  (setf (jupyter-request-idle-p req) t)
  (let ((client (jupyter-request-client req)))
    (when (eq (oref client last-queued-request) req)
      (oset client last-queued-request nil)))
  (jupyter-org--remove-overlay req)
  (jupyter-org--clear-async-indicator req)
  (let ((marker (jupyter-org-request-marker req)))
    (message (format "Source block execution in %s at position %s canceled"
                     (buffer-name (marker-buffer marker))
                     (marker-position marker))))
  (with-demoted-errors "Error while aborting subscribers: %S"
    (jupyter-run-with-io
        (jupyter-request-message-publisher req)
      ;; Propagate the abort down the chain of queued requests.
      (jupyter-publish 'abort)))
  (jupyter-unsubscribe))

(defun jupyter-org-maybe-queued (dreq)
  "Return a monadic value that either sends or continues to delay DREQ.
DREQ is an already delayed request, as returned by
`jupyter-request' and friends.  When the value is bound to a
client, using e.g. `jupyter-run-with-client', send DREQ if there
are no queued requests otherwise queue DREQ.  The value returns
the unboxed request contained in DREQ.

If the variable `jupyter-org-queue-requests' is nil, just send
the request immediately instead of attempting to queue it."
  (if (not jupyter-org-queue-requests)
      (jupyter-sent dreq)
    (jupyter-mlet* ((client (jupyter-get-state))
                    (req dreq))
      (let* ((send
              (lambda (req)
                (jupyter-run-with-client client
                  (jupyter-mlet* ((req (jupyter-sent
                                        (jupyter-return req))))
                    (oset client most-recent-request req)
                    (jupyter-run-with-io
                        (jupyter-request-message-publisher req)
                      (jupyter-subscribe
                        (jupyter-subscriber
                          (lambda (msg)
                            (when (or (eq msg 'abort)
                                      (equal (jupyter-message-type msg) "execute_reply"))
                              (when (eq (oref client most-recent-request) req)
                                (oset client most-recent-request nil))
                              (jupyter-unsubscribe))))))
                    (when (eq (oref client last-queued-request) req)
                      (oset client last-queued-request nil))
                    (jupyter-return req)))))
             (queue
              ;; Subscribe REQ to the message publisher of QREQ such that
              ;; REQ is sent or aborted when QREQ receives an
              ;; execute_reply.
              (lambda (qreq req)
                (let ((pub (jupyter-request-message-publisher qreq)))
                  (jupyter-run-with-io pub
                    (jupyter-subscribe
                      (jupyter-subscriber
                        (lambda (msg)
                          (if (eq msg 'abort)
                              (jupyter-org-abort req)
                            (pcase (jupyter-message-type msg)
                              ("execute_reply"
                               (jupyter-with-message-content msg (status)
                                 (if (equal status "ok")
                                     (funcall send req)
                                   (jupyter-org-abort req)))
                               (jupyter-unsubscribe))))))))))))
        (let ((mreq (oref client most-recent-request)) qreq)
          (cond
           ((null mreq)
            (funcall send req))
           ((setq qreq (oref client last-queued-request))
            (funcall queue qreq req)
            (oset client last-queued-request req))
           (t
            (funcall queue mreq req)
            (oset client last-queued-request req)))
          (jupyter-return req))))))

;;; Caching the current source block's information

(defvar jupyter-org--src-block-cache nil
  "A list (PARAMS BEG END) of most recently visited source block.
PARAMS is the source block parameters of the Jupyter source block
between BEG and END.  BEG and END are markers.

Can also take the form (invalid PARAMS BEG END) which means that the
cache may need to be recomputed.")

(defun jupyter-org--at-cached-src-block-p ()
  (pcase jupyter-org--src-block-cache
    (`(invalid . ,_) nil)
    (`(,_ ,beg ,end)
     (and
      (marker-position beg)
      (marker-position end)
      (<= beg (point) end)))))

(defun jupyter-org--set-src-block-cache ()
  "Set the src-block cache.
If set successfully or if `point' is already inside the cached
source block, return non-nil.  Otherwise, when `point' is not
inside a Jupyter src-block, return nil."
  (unless jupyter-org--src-block-cache
    (setq jupyter-org--src-block-cache
          (list (list 'invalid nil (make-marker)
                      (let ((end (make-marker)))
                        ;; Move the end marker when text is inserted
                        (set-marker-insertion-type end t)
                        end)))))
  (if (org-in-src-block-p 'inside)
      (or (jupyter-org--at-cached-src-block-p)
          (when-let* ((el (org-element-at-point))
                      (info (and (eq (org-element-type el) 'src-block)
                                 (org-babel-jupyter-language-p
                                  (org-element-property :language el))
                                 (org-babel-get-src-block-info t el)))
                      (params (nth 2 info)))
            (when (eq (car jupyter-org--src-block-cache) 'invalid)
              (pop jupyter-org--src-block-cache))
            (pcase-let (((and cache `(,_ ,beg ,end))
                         jupyter-org--src-block-cache))
              (setcar cache params)
              (save-excursion
                (goto-char (org-element-property :post-affiliated el))
                (move-marker beg (line-beginning-position 2))
                (goto-char (org-element-property :end el))
                (skip-chars-backward "\r\n")
                (move-marker end (line-beginning-position))))
            t))
    ;; Invalidate cache when going outside of a source block.  This
    ;; way if the language of the block changes we don't end up using
    ;; the cache since it is only used for Jupyter blocks.
    (pcase jupyter-org--src-block-cache
      ((and `(,x . ,_) (guard (not (eq x 'invalid))))
       (push 'invalid jupyter-org--src-block-cache)))
    nil))

(defmacro jupyter-org-when-in-src-block (&rest body)
  "Evaluate BODY when inside a Jupyter source block.
Return the result of BODY when it is evaluated, otherwise nil is
returned."
  (declare (debug (body)))
  `(when (jupyter-org--set-src-block-cache)
     ,@body))

(defmacro jupyter-org-with-src-block-bounds (beg end &rest body)
  "With BEG and END set to the bounds of the current src-block evaluate BODY.
BODY is only evaluated when the current source block is a Jupyter
source block and `point' is within its contents.  Returns the
result of BODY or nil when it isn't evaluated."
  (declare (indent 2) (debug (body)))
  `(jupyter-org-when-in-src-block
    (pcase-let ((`(,_ ,,beg ,,end) jupyter-org--src-block-cache))
      ,@body)))

(defun jupyter-org-src-block-params (&optional previous)
  "Return the src-block parameters for the current Jupyter src-block.
If PREVIOUS is non-nil and `point' is not in a Jupyter source
block, return the parameters of the most recently visited source
block, but only if it was in the same buffer.  Otherwise return
nil."
  (jupyter-org--set-src-block-cache)
  (pcase jupyter-org--src-block-cache
    ((and (and (guard (and previous
                           (not (jupyter-org--at-cached-src-block-p)))))
          `(invalid ,params ,beg . ,_)
          (guard (eq (marker-buffer beg) (current-buffer))))
     ;; NOTE There are probably cases where the parameters could no
     ;; longer be valid, hence the invalid tag.  This is mainly for
     ;; the purposes of creating a mode line according to
     ;; `jupyter-org-interaction-mode-line-display-most-recent'.
     params)
    (`(invalid . ,_) nil)
    (`(,params . ,_) params)))

(defun jupyter-org--with-src-block-client (thunk)
  (when-let* ((params (jupyter-org-src-block-params))
              (buffer
               (and (or jupyter-org-auto-connect
                        (org-babel-jupyter-session-initiated-p
                         params 'noerror))
                    (org-babel-jupyter-initiate-session
                     (alist-get :session params) params)))
              (client (or (buffer-local-value
                           'jupyter-current-client buffer)
                          (error "No client in session buffer!")))
              (syntax (jupyter-kernel-language-syntax-table client)))
    (let ((jupyter-current-client client))
      (with-syntax-table syntax
        (funcall thunk)))))

(defmacro jupyter-org-with-src-block-client (&rest body)
  "Evaluate BODY with `jupyter-current-client' set to the session's client.
BODY is evaluate and its result returned only when the client
associated with the source block is connected to its kernel and
`point' is within the contents of the source block.  If no client
exists for the session yet and `jupyter-org-auto-connect' is
non-nil, a new client is initiated for the session before
evaluating BODY.  When `jupyter-org-auto-connect' is nil and
there is no client or when `point' is not in a Jupyter source
block, don't evaluate BODY and return nil.

In addition to evaluating BODY with an active Jupyter client set,
the `syntax-table' will be set to that of the REPL buffer's."
  (declare (debug (body)))
  `(jupyter-org--with-src-block-client
    (lambda () ,@body)))

;;; Completion in code blocks

(cl-defmethod jupyter-code-context ((_type (eql inspect))
                                    &context (major-mode org-mode))
  (when (org-in-src-block-p 'inside)
    (jupyter-line-context)))

(cl-defmethod jupyter-code-context ((_type (eql completion))
                                    &context (major-mode org-mode))
  (jupyter-org-with-src-block-bounds beg end
    (list (buffer-substring-no-properties beg end)
          (- (point) beg))))

(defun jupyter-org-completion-at-point ()
  (jupyter-org-with-src-block-client
   (jupyter-completion-at-point)))

;;; Inspection

(cl-defmethod jupyter-inspect (&context (major-mode org-mode)
                                        &rest _ignore)
  (jupyter-org-with-src-block-client
   (cl-call-next-method)))

;;; Key bindings in code blocks

(defvar jupyter-org-interaction-mode-map (make-sparse-keymap))

(defun jupyter-org--key-def (key vect)
  "Get KEY's definition, using VECT to lookup the keymap to search.
`jupyter-org-interaction-mode-map' contains keymaps bound to
single element vectors like [jupyter] or [python] which hold the
keybindings available for a particular language, [python], or for
any Jupyter code block, [jupyter]."
  (let* ((map (lookup-key jupyter-org-interaction-mode-map vect))
         (cmd (and (keymapp map) (lookup-key map key))))
    (and (functionp cmd) cmd)))

(defun jupyter-org--define-key-filter (key &rest _)
  "Return the definition for KEY when inside a Jupyter src-block or nil."
  ;; Fall back to regular `org-mode' keys when the current point is invisible,
  ;; e.g. folded subtrees.
  (unless (org-invisible-p)
    (jupyter-org-with-src-block-client
     (let ((lang (jupyter-kernel-language)))
       (or (jupyter-org--key-def key `[,lang])
           (jupyter-org--key-def key [jupyter]))))))

(defun jupyter-org--call-with-src-block-client (def)
  "Call DEF interactively with the current src-block's client."
  (jupyter-org-with-src-block-client
   (call-interactively def)))

(defvar jupyter-org--defining-key-p nil)

(defun jupyter-org-define-key (key def &optional lang)
  "Bind KEY to DEF, but only when inside a Jupyter code block.

When `point' is inside a Jupyter code block, DEF is called using
the `jupyter-current-client' of the session associated with the
code block, see `jupyter-org-with-src-block-client'.

If LANG is non-nil, it is a language symbol such as python or
julia.  Only bind KEY to DEF whenever the underlying kernel
language is LANG.  If LANG is nil, then KEY is bound to DEF
regardless of kernel language.  Note, the same key can be bound
for different kernel languages.

All of the keys are bound in `jupyter-org-interaction-mode-map'
and they only take effect when the variable
`jupyter-org-interaction-mode' is non-nil."
  ;; From http://endlessparentheses.com/define-context-aware-keys-in-emacs.html
  ;;
  ;; But the dynamic keybindings in code blocks is inspired by John Kitchin's
  ;; extensions to ob-ipython.
  (setq lang `[,(or lang 'jupyter)])
  (let ((map (or (lookup-key jupyter-org-interaction-mode-map lang)
                 (define-key jupyter-org-interaction-mode-map lang
                   (make-sparse-keymap)))))
    (define-key map key
      (let ((cmd (lambda ()
                   (interactive)
                   (jupyter-org--call-with-src-block-client def))))
        (if (symbolp def)
            (defalias (make-symbol (symbol-name def))
              cmd (documentation def))
          cmd))))
  (let ((jupyter-org--defining-key-p t))
    (unless (functionp (lookup-key jupyter-org-interaction-mode-map key))
      (define-key jupyter-org-interaction-mode-map key
        (list 'menu-item "" nil :filter
              (lambda (&rest _)
                (if jupyter-org--defining-key-p
                    ;; Stub definition so that `lookup-key' returns a non-nil
                    ;; value since the normal filter only returns a definition
                    ;; when inside a source block.  We only need to make the
                    ;; definition for KEY once and not on every re-definition
                    ;; of KEY for a particular language.
                    #'undefined
                  (jupyter-org--define-key-filter key))))))))

(jupyter-org-define-key (kbd "C-x C-e") #'jupyter-eval-line-or-region)
(jupyter-org-define-key (kbd "C-M-x") #'jupyter-eval-defun)
(jupyter-org-define-key (kbd "M-i") #'jupyter-inspect-at-point)
(jupyter-org-define-key (kbd "C-c M-:") #'jupyter-eval-string-command)
(jupyter-org-define-key (kbd "C-c C-r") #'jupyter-repl-restart-kernel)
(jupyter-org-define-key (kbd "C-c C-i") #'jupyter-repl-interrupt-kernel)

;;; Handling ANSI escapes in kernel output

;; NOTE: We cache the properties here since this is called during the font-lock
;; process (and maybe shouldn't be?) which means that it can be called many
;; times on the same region.  We don't want to re-compute the faces on each
;; call.
(defun jupyter-org--ansi-color-apply-on-region (begin end)
  "Handle ANSI escape codes between (BEGIN . END) and cache the results.
If (BEGIN . END) is not marked with a jupyter-ansi text property,
apply `jupyter-ansi-color-apply-on-region' on the region and mark
it with a non-nil jupyter-ansi property.  Otherwise, prepend any
non-nil jupyter-face properties in the region to the face
property."
  ;; Don't add these changes to the undo list, gives a slight speed up.
  (let ((buffer-undo-list t)
        (inhibit-modification-hooks t)
        next begin1 end1)
    (while (/= begin end)
      (setq next (next-single-property-change begin 'jupyter-ansi nil end))
      (cond
       ((get-text-property begin 'jupyter-ansi)
        (setq begin1 begin
              end1 next
              begin next)
        (while (/= begin1 end1)
          (setq next (next-single-property-change
                      begin1 'jupyter-face nil end1))
          (when (get-text-property begin1 'jupyter-face)
            (font-lock-prepend-text-property
             begin1 next 'face (get-text-property begin1 'jupyter-face)))
          (setq begin1 next)))
       (t
        (put-text-property begin next 'jupyter-ansi t)
        (jupyter-ansi-color-apply-on-region begin next 'jupyter-face)
        (setq begin next))))))

;; Adapted from `org-fontify-meta-lines-and-blocks-1'
(defun jupyter-org-font-lock-ansi-escapes (limit)
  (let ((case-fold-search t))
    (when (re-search-forward
           "^[ \t]*\\(#\\+begin_example[ \t]*\\|: .*\\)$" limit t)
      (let ((beg (match-beginning 1))
            (beg1 (line-beginning-position 2))
            end)
        (cond
         ;; example block
         ((not (eq (char-after beg) ?:))
          (when (re-search-forward
                 "^[ \t]*#\\+end_example\\>.*"
                 nil t) ;; on purpose, we look further than LIMIT
            (setq end (min (point-max) (1- (match-beginning 0))))
            (jupyter-org--ansi-color-apply-on-region beg1 end)))
         ;; fixed width
         (t
          (setq end (or (and (re-search-forward "^[ \t]*[^ \t:]" nil t)
                             (1- (match-beginning 0)))
                        (point-max)))
          (jupyter-org--ansi-color-apply-on-region beg end)))))))

;;; `jupyter-org-interaction-mode'

(defvar org-font-lock-keywords)

(define-minor-mode jupyter-org-interaction-mode
  "Minor mode for interacting with a Jupyter REPL from an `org-mode' buffer.
When this minor mode is enabled, some of the keybindings
available in `jupyter-repl-interaction-mode' are also available
when `point' is inside a Jupyter code block.  Completion is also
enabled when `point' is inside a code block.

In addition, ANSI escape sequences in example blocks or
fixed-width elements are fontified.

By default this mode is enabled in every `org-mode' buffer.

key             binding
---             -------

C-M-x           `jupyter-eval-defun'
M-i             `jupyter-inspect-at-point'

C-c TAB         `jupyter-repl-interrupt-kernel'
C-c C-r         `jupyter-repl-restart-kernel'

C-x C-e         `jupyter-eval-line-or-region'"
  :group 'ob-jupyter
  :init-value nil
  (cond
   (jupyter-org-interaction-mode
    (add-hook 'completion-at-point-functions 'jupyter-org-completion-at-point nil t)
    (add-hook 'after-revert-hook 'jupyter-org-interaction-mode nil t)
    (setq-local char-property-alias-alist
                (copy-tree char-property-alias-alist))
    (cl-callf append (alist-get 'invisible char-property-alias-alist)
      '(jupyter-invisible))
    (unless (cl-find-if
             (lambda (x) (eq (car x) 'jupyter-org-font-lock-ansi-escapes))
             org-font-lock-keywords)
      (cl-callf append org-font-lock-keywords
        '((jupyter-org-font-lock-ansi-escapes)))))
   (t
    (remove-hook 'completion-at-point-functions 'jupyter-org-completion-at-point t)
    (remove-hook 'after-revert-hook 'jupyter-org-interaction-mode t)
    (cl-callf2 delq 'jupyter-invisible
               (alist-get 'invisible char-property-alias-alist))
    (cl-callf2 cl-remove-if
        (lambda (x) (eq (car x) 'jupyter-org-font-lock-ansi-escapes))
        org-font-lock-keywords))))

(add-hook 'org-mode-hook 'jupyter-org-interaction-mode)

;;; Constructing org syntax trees

(defvar org-element-all-objects)
(defvar org-element-all-elements)

(defun jupyter-org-object-p (element)
  "Return non-nil if ELEMENT's type is a member of `org-element-all-objects'."
  (memq (org-element-type element) org-element-all-objects))

(defun jupyter-org-raw-string-p (str)
  "Return non-nil if STR can be inserted as is during result insertion."
  (and (stringp str) (get-text-property 0 'jupyter-org str)))

(defun jupyter-org-raw-string (str)
  "Return STR, ensuring that it is flagged as already being `org' syntax.
Adds a non-nil jupyter-org text property on the first character
of STR.  If a string returned by `jupyter-org-result' has a
non-nil jupyter-org property on the first character, it is
inserted without modification as the result of a code block."
  (prog1 str
    (put-text-property 0 1 'jupyter-org t str)))

(defun jupyter-org-table-string (str)
  "Return STR, ensuring that it is flagged as containing an `org' table.
We need a way to distinguish a table string that is easily
removed from the code block vs a regular string that will need to
be wrapped in a drawer.  Used in `jupyter-org-babel-result-p'."
  (prog1 (jupyter-org-raw-string str)
    (put-text-property 0 1 'org-table t str)))

(defun jupyter-org-comment (value)
  "Return a comment `org-element' with VALUE."
  (org-element-create 'comment (list :value value)))

(defun jupyter-org-export-block-or-pandoc (type value params)
  "Return VALUE, either converted with pandoc or in an export block.
If PARAMS has non-nil value for key ':pandoc' and TYPE is in
`jupyter-org-pandoc-convertable', convert the result with pandoc.
Otherwise, wrap it in an export block."
  (if (and (alist-get :pandoc params)
           (member type jupyter-org-pandoc-convertable))
      (list 'pandoc
            (list :text "Converting result using Pandoc..."
                  :type type
                  :value value))
    (jupyter-org-export-block type value)))

(defun jupyter-org-export-block (type value)
  "Return an export-block `org-element'.
The block will export TYPE and the contents of the block will be
VALUE."
  (org-element-create 'export-block
                      (list :type type
                            :value (org-element-normalize-string value))))

(defun jupyter-org-file-link (path)
  "Return a file link `org-element' that points to PATH."
  (org-element-create 'link
                      (list :type "file"
                            :path path
                            :type-explicit-p t)))

(defun jupyter-org-image-link (path &optional width height)
  "Return an `org-element' for an image at PATH.
If a WIDTH or HEIGHT are provided, then return a paragraph
element with an affiliated keyword ATTR_ORG.  So that the image
link will be rendered like

    #+ATTR_ORG :width 300 :height 300
    [[file:<path>]]

Otherwise return a `jupyter-org-file-link' for PATH."
  (if (or width height)
      (let ((attrs (concat
                    (when width
                      (concat ":width " (number-to-string width)))
                    (when height
                      (concat (when width " ")
                              ":height " (number-to-string height))))))
        (org-element-create 'paragraph (list :attr_org (list attrs))
                            (jupyter-org-file-link path)
                            "\n"))
    (jupyter-org-file-link path)))

(defun jupyter-org-src-block (language parameters value &optional switches)
  "Return a src-block `org-element'.
LANGUAGE, PARAMETERS, VALUE, and SWITCHES all have the same
meaning as a src-block `org-element'."
  (declare (indent 2))
  (org-element-create 'src-block
                      (list :language language
                            :parameters parameters
                            :switches switches
                            :value value)))

(defun jupyter-org-example-block (value)
  "Return an example-block `org-element' with VALUE."
  (org-element-create 'example-block
                      (list :value (org-element-normalize-string value))))

;; From `org-babel-insert-result'
(defun jupyter-org-tabulablep (r)
  "Return non-nil when R can be turned into an `org-mode' table."
  (and (listp r)
       (null (cdr (last r)))
       (cl-every
        (lambda (e) (or (atom e) (null (cdr (last e)))))
        r)))

;; From `org-babel-insert-result'
(defun jupyter-org-table-to-orgtbl (table)
  "Return TABLE formatted as an `org-mode' table string."
  (with-temp-buffer
    (insert (concat (orgtbl-to-orgtbl
                     (if (cl-every
                          (lambda (e)
                            (or (eq e 'hline) (listp e)))
                          table)
                         table
                       (list table))
                     nil)
                    "\n"))
    (goto-char (point-min))
    (when (org-at-table-p) (org-table-align))
    (buffer-string)))

(defun jupyter-org-scalar (value)
  "Return a scalar VALUE.
If VALUE is a string, return either a fixed-width `org-element'
or example-block depending on
`org-babel-min-lines-for-block-output'.

If VALUE is another `org-element' return it unchanged.

If VALUE is a list and can be represented as a table, return an
`org-mode' table as a string.  To distinguish the table from a
regular string, it has a non-nil org-table text property on its
first character.

Otherwise, return VALUE formated as a fixed-width `org-element'."
  (cond
   ((stringp value)
    (if (>= (jupyter-org-count-lines value)
            org-babel-min-lines-for-block-output)
        (jupyter-org-example-block value)
      (org-element-create 'fixed-width (list :value value))))
   ((and (listp value)
         (or (memq (car value) org-element-all-objects)
             (memq (car value) org-element-all-elements)))
    value)
   ((and (listp value)
         (jupyter-org-tabulablep value))
    (jupyter-org-table-string (jupyter-org-table-to-orgtbl value)))
   (t
    (org-element-create 'fixed-width (list :value (format "%S" value))))))

(defun jupyter-org-results-drawer (&rest results)
  "Return a drawer `org-element' containing RESULTS.
RESULTS can be either strings or other `org-element's.  Newlines
are added after every `org-element' object in RESULTS, such as
file links, so that each result appears on a single line in the
string representation of the drawer.  The returned drawer has a
name of \"RESULTS\"."
  (apply #'org-element-set-contents
         (list 'drawer (list :drawer-name "RESULTS"))
         (cl-loop
          for res in results
          if (jupyter-org-object-p res)
          collect res into ret and collect "\n" into ret
          else collect res into ret
          finally return
          (let ((last (last ret)))
            ;; Ensure the last element has a newline if it is already a string.
            ;; This is to avoid situations like
            ;;
            ;; :RESULTS:
            ;; foo:END:
            (when (and (stringp (car last))
                       (not (zerop (length (car last)))))
              (setcar last (org-element-normalize-string (car last))))
            ret))))

;;; Inserting results

;;;; `jupyter-org-result'

(defun jupyter-org-image-file-name (data ext)
  "Return a file name based on DATA and EXT.
`jupyter-org-resource-directory' is used as the directory name of
the file, the `sha1' hash of DATA is used as the base name, and
EXT is used as the extension."
  (let ((dir (prog1 jupyter-org-resource-directory
               (unless (file-directory-p jupyter-org-resource-directory)
                 (make-directory jupyter-org-resource-directory))))
        (ext (if (= (aref ext 0) ?.) ext
               (concat "." ext))))
    (concat (file-name-as-directory dir) (sha1 data) ext)))

(defvar org-image-actual-width)

(defun jupyter-org--add-image-ext-maybe (file &optional ext)
  "Return a file name for FILE with EXT.
If FILE is an image file with a specified extension, return FILE.
If FILE is an image file without a specified extension and EXT is
non-nil, return FILE with EXT as the extension.  Otherwise, return
FILE."
  (cond
   ((null file) file)
   ((image-supported-file-p file) file)
   ((not (null ext)) (concat file "." ext))
   (t file)))

(defun jupyter-org--image-result (mime content params &optional b64-encoded)
  "Return an org-element suitable for inserting an image.
MIME is the image mimetype, CONTENT is a property list

    (:data D :metadata M)

where D is the image data for MIME and M any metadata.  D is
written to file and an org-element link to the file is returned.

PARAMS are the `jupyter-org-request-block-params', the ones
passed to `org-babel-execute:jupyter', of the source block that
returned D.  If PARAMS contains a :file key, it's value is used
as the image file name.  Otherwise a file name is created, see
`jupyter-org-image-file-name'.  In the case that a file exists
with the same name being used, it is overwritten.

If B64-ENCODED is non-nil, the image data is assumed to be a
base64 encoded string and will be decoded before writing to file.

If METADATA contains a :width or :height key, then the returned
org-element will have an ATTR_ORG affiliated keyword containing
the width or height of the image.  When there is no :width or
:height, an ATTR_ORG keyword containing the true size of the
image may still be added, see `jupyter-org-adjust-image-size'."
  (let* ((file-ext (cl-case mime
                     (:image/png "png")
                     (:image/jpeg "jpg")
                     (:image/svg+xml "svg")))
         (overwrite (not (null (alist-get :file params))))
         (file (or (jupyter-org--add-image-ext-maybe (alist-get :file params) file-ext)
                   (jupyter-org-image-file-name
                    (plist-get content :data)
                    file-ext))))
    (when (or overwrite (not (file-exists-p file)))
      (let ((buffer-file-coding-system
             (if b64-encoded 'binary
               buffer-file-coding-system))
            (require-final-newline nil))
        (with-temp-file file
          (insert (plist-get content :data))
          (when b64-encoded
            (base64-decode-region (point-min) (point-max))))))
    (cl-destructuring-bind (&key width height &allow-other-keys)
        (plist-get content :metadata)
      (when (and jupyter-org-adjust-image-size (null width)
                 (numberp (car-safe org-image-actual-width)))
        (let ((image-width (car (image-size
                                 (create-image (expand-file-name file))
                                 'pixels))))
          (when (< image-width (car org-image-actual-width))
            (setq width image-width))))
      (jupyter-org-image-link file width height))))

(cl-defgeneric jupyter-org-result (_mime _content _params)
  "Return an `org-element' representing a result.
Either a string or an `org-element' is a valid return value of
this method.  The former will be inserted as is, while the latter
will be inserted by calling `org-element-interpret-data' first.

The returned result should be a representation of a MIME type's
CONTENT. CONTENT is a property list like

    \='(:data DATA :metadata METADATA)

that contains the DATA/METADATA of the mime type.  As an example,
if MIME is `:text/markdown', then DATA should be the markdown
string.  The returned result in this case will be

    (jupyter-org-export-block \"markdown\" DATA)"
  (ignore))

(defun jupyter-org--find-mime-types (req-types)
  "Return the keywords in `jupyter-org-mime-types' that match REQ-TYPES.
REQ-TYPES is a string such as \"plain\", \"plain html\", or
\"text/plain\".  The string \"text\" is translated to the keyword
`:text/plain' and \"image\" to `:image/png'.

If a match is not found, return nil."
  (when (stringp req-types)
    ;; Iterate the user-specified mimetypes looking for symbols that match a
    ;; symbol in `jupyter-org-mime-types'.  Invalid mimetypes are ignored.
    (cl-loop
     with translations = `(("text" . :text/plain)
                           ("image" . :image/png))
     for req-type in (split-string req-types)
     for match = (or (cdr (assoc req-type translations))
                     (let ((regexp (if (string-match "/" req-type)
                                       req-type
                                     (concat "/" req-type "$"))))
                       (cl-loop for mime-type in jupyter-org-mime-types
                                if (string-match regexp (symbol-name mime-type))
                                return mime-type)))
     if match collect match)))

(defun jupyter-org-display-mime-types (req)
  "Return the mime types to display for REQ."
  (or (jupyter-org--find-mime-types
       (alist-get :display (jupyter-org-request-block-params req)))
      jupyter-org-mime-types))

(defun jupyter-org-element-p (obj)
  "Return non-nil if OBJ is an Org element."
  (let ((type (org-element-type obj)))
    (and type
         (or (eq type 'plain-text)
             (memq type org-element-all-objects)
             (memq type org-element-all-elements)))))

(defun jupyter-org-get-result (req plist &optional metadata)
  "For REQ, return a rendered form of a message PLIST.
PLIST and METADATA have the same meaning as in
`jupyter-normalize-data'.

Given the source block parameters of REQ, loop over the mime
types in `jupyter-org-mime-types' calling

    (jupyter-org-result MIME CONTENT PARAMS)

for each one.  Return the result of the call for the first
mime-type that has a non-nil result.

MIME is the current mime-type, CONTENT is a property list

    (:data ... :metadata ...)

containing the data of the mime-type and PARAMS are the source
block parameters.

If the source block parameters have a value for the :display
header argument, like \"image/png html plain\", then loop over
those mime types instead."
  (if (jupyter-org-element-p plist) plist
    (pcase-let (((cl-struct jupyter-org-request block-params file) req))
      (let* ((mime-types (jupyter-org-display-mime-types req)))
        ;; Push :file back into PARAMS if it was present in
        ;; `org-babel-execute:jupyter'.  That function removes it because
        ;; we don't want `org-babel-insert-result' to handle it.
        (when file
          (push (cons :file file) block-params))
        (or (jupyter-org-with-point-at req
              (jupyter-map-mime-bundle mime-types
                  (jupyter-normalize-data plist metadata)
                (lambda (mime content)
                  (jupyter-org-result mime content block-params))))
            (let ((warning
                   (format
                    "%s did not return requested mimetype(s): %s"
                    (jupyter-message-type (jupyter-request-last-message req))
                    mime-types)))
              (display-warning 'jupyter warning)
              nil))))))

(cl-defmethod jupyter-org-result ((_mime (eql :application/vnd.jupyter.widget-view+json)) _content _params)
  ;; TODO: Clickable text to open up a browser
  (jupyter-org-scalar "Widget"))

(defvar org-table-line-regexp)

(cl-defmethod jupyter-org-result ((_mime (eql :text/org)) content _params)
  (let ((data (plist-get content :data)))
    (if (string-match-p org-table-line-regexp data)
        (jupyter-org-table-string data)
      (jupyter-org-raw-string data))))

(cl-defmethod jupyter-org-result ((mime (eql :image/png)) content params)
  (jupyter-org--image-result mime content params 'b64-encoded))

(cl-defmethod jupyter-org-result ((mime (eql :image/jpeg)) content params)
  (jupyter-org--image-result mime content params 'b64-encoded))

(cl-defmethod jupyter-org-result ((mime (eql :image/svg+xml)) content params)
  (jupyter-org--image-result mime content params))

(cl-defmethod jupyter-org-result ((_mime (eql :text/markdown)) content params)
  (jupyter-org-export-block-or-pandoc
   "markdown" (plist-get content :data) params))

(defun jupyter-org--parse-latex-element (data)
  "Return a latex-fragment or latex-environment org-element obtained from DATA.
DATA is inserted into a temporary buffer and an org-element latex
fragment or environment is parsed and returned.  If neither can be
parsed, wrap DATA in a minipage environment and return it."
  (with-temp-buffer
    (insert data)
    (let ((elts (org-element-map (org-element-parse-buffer)
                    '(latex-fragment latex-environment) 'identity)))
      (cond ((and (= (length elts) 1) (car elts)))
            (t
             ;; If all else fails, wrap DATA in a minipage environment
             (org-element-create 'latex-environment (list :value (concat "\
\\begin{minipage}{\\textwidth}
\\begin{flushright}\n" data "\n\\end{flushright}
\\end{minipage}"))))))))

(cl-defmethod jupyter-org-result ((_mime (eql :text/latex)) content params)
  (if (member "raw" (alist-get :result-params params))
      (jupyter-org--parse-latex-element (plist-get content :data))
    (jupyter-org-export-block-or-pandoc
     "latex" (plist-get content :data) params)))

(cl-defmethod jupyter-org-result ((_mime (eql :text/html)) content params)
  (jupyter-org-export-block-or-pandoc "html" (plist-get content :data) params))

;; NOTE: The order of :around methods is that the more specialized
;; wraps the more general, this makes sense since it is how the
;; primary methods work as well.
;;
;; Using an :around method to attempt to guarantee that this is called
;; as the outer most method.  Kernel languages should extend the
;; primary method.
(cl-defmethod jupyter-org-result :around ((_mime (eql :text/plain)) _content params)
  "Do some final transformations of the result.
Call the next method, if it returns \"scalar\" results, return a
new \"scalar\" result with the result of calling
`org-babel-script-escape' on the old result."
  (let ((result (cl-call-next-method)))
    (jupyter-org-scalar
     (cond
      ((and (stringp result)
            ;; Don't assume non-empty string, see #144
            (not (zerop (length result)))
            ;; Don't attempt to create a table when we just want scalar results
            ;; FIXME: `jupyter-org-scalar' also considers a table a scalar, but
            ;; `org-mode' doesn't.
            (not (member "scalar" (alist-get :result-params params)))
            ;; Be a little more stringent than `org-babel-script-escape'.  It
            ;; gives bad results on the following
            ;;
            ;;     [1] Foo bar
            (when-let* ((beg (car (memq (aref result 0) '(?\[ ?\{ ?\())))
                        (end (pcase beg
                               (?\[ ?\])
                               (?\{ ?\})
                               (?\( ?\)))))
              (eq end (aref result (1- (length result))))))
       (org-babel-script-escape result))
      (t result)))))

(cl-defmethod jupyter-org-result ((_mime (eql :text/plain)) content _params)
  (plist-get content :data))

;;;; Helper functions

(defvar org-babel-jupyter-async-inline-results-pending-indicator)

(defun jupyter-org--clear-async-indicator (req)
  "Clear any async indicators of REQ in the buffer."
  (unless (jupyter-org-request-id-cleared-p req)
    (jupyter-org-with-point-at req
      (if (jupyter-org-request-inline-block-p req)
          (when-let* ((pos (org-babel-where-is-src-block-result)))
            (goto-char pos)
            (when-let* ((result (org-element-context))
                        (args (org-element-property :args result))
                        (arg (and (= (length args) 1) (car args))))
              (when (equal
                     (concat "="
                             org-babel-jupyter-async-inline-results-pending-indicator
                             "=")
                     arg)
                (jupyter-run-with-state req
                  (jupyter-org-inserted-result '(:text/plain "")))
                (setf (jupyter-org-request-id-cleared-p req) t))))
        (when (search-forward (jupyter-request-id req) nil t)
          (delete-region (line-beginning-position)
                         (1+ (line-end-position)))
          (setf (jupyter-org-request-id-cleared-p req) t))))))

(defun jupyter-org-element-begin-after-affiliated (element)
  "Return the beginning position of ELEMENT after any affiliated keywords."
  (or (org-element-property :post-affiliated element)
      (org-element-property :begin element)))

(defun jupyter-org-element-end-before-blanks (element)
  "Return the end position of ELEMENT, before any :post-blank lines."
  (- (org-element-property :end element)
     (or (org-element-property :post-blank element) 0)))

(defun jupyter-org-element-contents-end (element)
  "Return the end position for the contents of ELEMENT in the current buffer."
  (or (org-element-property :contents-end element)
      (save-excursion
        (goto-char (jupyter-org-element-end-before-blanks element))
        (line-beginning-position 0))))

(defun jupyter-org-count-lines (text)
  "Return the number of lines in TEXT."
  (let ((start -1)
        (count 0))
    (while (setq start (string-search "\n" text (1+ start)))
      (cl-incf count))
    count))

(defun jupyter-org-delete-blank-line ()
  "If the current line is blank, delete it."
  (when (looking-at-p "^[\t ]*$")
    (delete-region (line-beginning-position)
                   (min (point-max) (1+ (line-end-position))))))

(defun jupyter-org-strip-last-newline (string)
  "Return STRING with its last newline removed."
  (replace-regexp-in-string "\n\\'" "" string))

(defun jupyter-org-delete-element (element)
  "Delete an `org' ELEMENT from the buffer.
Leave its affiliated keywords and preserve any blank lines that
appear after the element."
  ;; Force deferred property to compute the properties before deleting
  ;; the element from the buffer, the ELEMENT is used elsewhere even
  ;; after it has been removed from the buffer.  For Org >= 9.7.
  (when (functionp 'org-element-properties-resolve)
    (org-element-properties-resolve element t))
  (delete-region (jupyter-org-element-begin-after-affiliated element)
                 (jupyter-org-element-end-before-blanks element)))

(defun jupyter-org-babel-result-p (result)
  "Return non-nil if RESULT can be removed by `org-babel-remove-result'."
  (or (and (stringp result)
           ;; Org tables are returned as strings by this time.  So we need
           ;; something to distinguish them from regular strings.  See
           ;; `jupyter-org-scalar'.
           (get-text-property 0 'org-table result))
      (memq (org-element-type result)
            '(example-block
              export-block fixed-width item
              link plain-list src-block table))))

(defun jupyter-org--strip-properties (element)
  "Strip away properties which may interfere with insertion of ELEM."
  ;; Ensure that a #+RESULTS: line is not prepended to context when calling
  ;; `org-element-interpret-data'.
  (org-element-put-property element :results nil)
  ;; Ensure there is no post-blank since `org-element-interpret-data'
  ;; already normalizes the string.
  (org-element-put-property element :post-blank nil))

(defun jupyter-org--first-result-context-p (context)
  (not
   (pcase (org-element-type context)
     (`drawer (equal "RESULTS"
                     (upcase (org-element-property :drawer-name context))))
     (`,type (or (jupyter-org-babel-result-p context)
                 (or (memq type '(latex-fragment latex-environment))
                     ;; TODO: Figure out a better way.  I predict there will be more
                     ;; situations where a comment would be useful to add.  That means
                     ;; we would have to verify each one.
                     (and (eq type 'comment)
                          (equal jupyter-org--goto-error-string
                                 (org-element-property :value context)))))))))

;;;; Stream results

;;;;; Helper functions

(defun jupyter-org--stream-result-p (result)
  (and (stringp result)
       (not (jupyter-org-raw-string-p result))))

(defun jupyter-org--mark-stream-result-newline (result)
  "Remember if RESULT ended in a newline.
Add a non-nil jupyter-stream-newline property to the most
recently inserted stream RESULT if it ends in a newline.  This is
so that `jupyter-org--append-stream-result' can properly insert a
newline or not before inserting subsequent stream results.

Assumes `point' is at the end of the last source block result."
  (or (stringp result) (setq result (org-element-property :value result)))
  (when (and result (not (zerop (length result)))
             (eq (aref result (1- (length result))) ?\n))
    (when (looking-back "#\\+END_EXAMPLE\n"
                        (line-beginning-position 0))
      (goto-char (match-beginning 0)))
    (put-text-property (1- (point)) (point) 'jupyter-stream-newline t)))

(defun jupyter-org--stream-append-position (context)
  "Return the position at which to append a stream result.
Return nil if CONTEXT does not represent a stream context."
  (save-excursion
    (goto-char (if (eq (org-element-type context) 'drawer)
                   (jupyter-org-element-contents-end context)
                 (jupyter-org-element-end-before-blanks context)))
    (beginning-of-line
     ;; When `point' is not at the beginning of a line, then it is on the last
     ;; line of the element contents/container so just go to the beginning of
     ;; the line.
     (when (= (line-beginning-position) (point))
       0))
    (skip-chars-forward " \t")
    (when (looking-at-p "\\(?::[\t ]\\|#\\+END_EXAMPLE\\)")
      (line-end-position (unless (eq (char-after) ?:) 0)))))

;; Adapted from `jupyter-handle-control-codes'
(defun jupyter-org--handle-control-codes (beg end)
  "Handle any control sequences between BEG and END."
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (let ((char (char-after)))
        (cond
         ((eq char ?\r)
          (if (< (1+ (point)) end)
              (if (memq (char-after (1+ (point)))
                        '(?\n ?\r))
                  (delete-char 1)
                (let ((end (1+ (point))))
                  (beginning-of-line)
                  (when (looking-at-p ": ")
                    (forward-char 2))
                  (delete-region (point) end)))
            (add-text-properties (point) (1+ (point))
                                 '(invisible t))
            (forward-char)))
         ((eq char ?\a)
          (delete-char 1)
          (beep))
         ((eq char ?\C-h)
          ;; FIXME: Consider fixed width regions
          (delete-region (1- (point)) (1+ (point))))
         (t
          (forward-char)))))))

;;;;; Fixed width -> example block promotion

(defun jupyter-org--fixed-width-to-example-block (element text keep-newline)
  "Replace the fixed-width ELEMENT with an example-block.
Append TEXT to the contents of the block.  If KEEP-NEWLINE is
non-nil, ensure that the appended RESULT begins on a newline."
  (jupyter-org-delete-element element)
  ;; Delete a newline that will be re-inserted by `org-element-interpret-data'.
  (when (eq (char-after) ?\n)
    (delete-char 1))
  (insert (org-element-interpret-data
           (jupyter-org-example-block
            (concat
             (let ((old-text
                    (org-element-normalize-string
                     (org-element-property :value element))))
               (if keep-newline old-text
                 (substring old-text 0 -1)))
             text)))))

;;;;; Append stream result

(defun jupyter-org--append-to-fixed-width (text keep-newline)
  "Append TEXT to the fixed-width element at point.
`point' is assumed to be at the insertion point.  If KEEP-NEWLINE is
non-nil, ensure that the appended TEXT begins on a newline."
  (save-match-data
    (let ((first-newline (string-match "\n" text)))
      (if (not (or first-newline keep-newline))
          (insert text)
        (let (head tail)
          (if keep-newline
              (setq head "\n"
                    tail text)
            (setq head (substring text 0 (1+ first-newline))
                  tail (substring text (1+ first-newline))))
          (insert head)
          ;; Delete the newline that will be re-inserted by
          ;; `org-element-interpret-data'
          (when (eq (char-after) ?\n)
            (delete-char 1))
          (unless (string-empty-p tail)
            (insert (org-element-interpret-data
                     (jupyter-org-scalar
                      (jupyter-org-strip-last-newline tail))))))))))

(defun jupyter-org--append-to-example-block (text keep-newline)
  "Append TEXT to the end of the current example block.
`point' is assumed to be at the end of the last line of the
example block contents.

If KEEP-NEWLINE is non-nil, add a newline before appending
TEXT."
  ;; Delete the newline that will be re-inserted by the call to
  ;; `org-element-normalize-string'.
  (when (eq (char-after) ?\n)
    (delete-char 1))
  (setq text (org-element-normalize-string text))
  ;; From `org-element-example-block-interpreter'
  (when (and (not org-src-preserve-indentation)
             (/= 0 org-edit-src-content-indentation)
             (version<= "9.2" (org-version)))
    (let ((ind (make-string org-edit-src-content-indentation ?\s))
          head tail)
      (if keep-newline
          (setq head ""
                tail text)
        (let ((first-newline (save-match-data
                               (string-match "\n" text))))
          (setq head (substring text 0 (1+ first-newline))
                tail (substring text (1+ first-newline)))))
      (setq text (concat head (replace-regexp-in-string
                               "^[ \t]*\\S-"
                               (concat ind "\\&")
                               (org-remove-indentation tail))))))
  (insert (concat (when keep-newline "\n") text)))

(defun jupyter-org--append-stream (text)
  "Append a stream RESULT.
Either append to the current fixed-width element or example block.

When appending to fixed-width elements, if appending RESULT
causes the total number of lines to exceed
`org-babel-min-lines-for-block-output' replace the fixed-width
element by an example-block containing both the original contents
of the fixed-width element and RESULT concatenated together."
  (let ((keep-newline (get-text-property (point) 'jupyter-stream-newline))
        (context (org-element-at-point)))
    (cond
     ((eq (org-element-type context) 'fixed-width)
      (let ((promote-to-block-p
             (>= (+ (count-lines
                     (jupyter-org-element-begin-after-affiliated context)
                     (jupyter-org-element-end-before-blanks context))
                    (jupyter-org-count-lines text))
                 org-babel-min-lines-for-block-output)))
        (if promote-to-block-p
            (jupyter-org--fixed-width-to-example-block context text keep-newline)
          (jupyter-org--append-to-fixed-width text keep-newline))))
     (t
      (jupyter-org--append-to-example-block text keep-newline)))))

(defun jupyter-org--prepare-context (context)
  "Prepare CONTEXT for insertion of an additional result.
Return a list of Org elements which should be prepended to any
additional result, e.g. by wrapping all the elements in
`jupyter-org-results-drawer'.

If CONTEXT is a drawer, move `point' to the end of its contents
so that the new result can be appended to the contents of the
drawer.  Otherwise, delete CONTEXT from the buffer."
  (let ((elems
         (pcase (org-element-type context)
           ;; Go to the end of the drawer to insert the new result.
           (`drawer
            (goto-char (jupyter-org-element-contents-end context))
            nil)
           ;; Any other context that looks like a result needs to be removed
           ;; since it, along with the new result will be wrapped in a drawer
           ;; and re-inserted into the buffer.
           (`table
            ;; The `org-element-contents' of a table is nil which interferes
            ;; with how `org-element-table-interpreter' works when calling
            ;; `org-element-interpret-data' so set the contents and delete
            ;; CONTEXT from the buffer.
            (org-element-set-contents
             context (delete-and-extract-region
                      (org-element-property :contents-begin context)
                      (jupyter-org-element-end-before-blanks context)))
            (list context))
           (_
            (jupyter-org-delete-element context)
            (jupyter-org-delete-blank-line)
            (list context)))))
    (mapcar #'jupyter-org--strip-properties elems)))

;;;; Insert result

(defmacro jupyter-org-indent-inserted-region (indentation &rest body)
  "Indent the region inserted by BODY using INDENTATION.
If INDENTATION is nil, it defaults to `current-indentation'."
  (declare (indent 1))
  (let ((indent (make-symbol "indent")))
    `(let ((,indent ,(or indentation '(current-indentation))))
       (jupyter-with-insertion-bounds
           beg end (progn ,@body)
         (when (and (numberp ,indent) (> ,indent 0))
           (indent-rigidly beg end ,indent))))))

(defvar org-bracket-link-regexp)

(defun jupyter-org--insert-stream (context text)
  (let ((res-begin (point))
        append-pos)
    (cond
     ((jupyter-org--first-result-context-p context)
      (insert (org-element-interpret-data
               (jupyter-org-scalar
                (jupyter-org-strip-last-newline
                 text)))))
     ((setq append-pos (jupyter-org--stream-append-position context))
      (goto-char append-pos)
      (jupyter-org--append-stream text))
     (t
      (let ((result (jupyter-org-scalar
                     (jupyter-org-strip-last-newline
                      text)))
            (elems (jupyter-org--prepare-context context)))
        (insert (org-element-interpret-data
                 (if elems
                     (apply #'jupyter-org-results-drawer
                            (append elems (list result)))
                   result))))))
    ;; Handle ANSI control codes in the stream output.
    (let ((end (point-marker)))
      (unwind-protect
          (jupyter-org--handle-control-codes
           (if append-pos
               (save-excursion
                 (goto-char append-pos)
                 ;; Go back one line to account for an edge case
                 ;; where a control code is at the end of a line.
                 (line-beginning-position 0))
             res-begin)
           end)
        (set-marker end nil)))
    ;; Add a text property to the last newline of the result so that
    ;; appending stream results works, see
    ;; `jupyter-org--append-stream-result'.
    (when (and text (not (zerop (length text)))
               (eq (aref text (1- (length text))) ?\n))
      (if (looking-back "#\\+END_EXAMPLE\n"
                        (line-beginning-position 0))
          (goto-char (match-beginning 0)))
      (put-text-property (1- (point)) (point) 'jupyter-stream-newline t))))

(defun jupyter-org--insert-nonstream (context result)
  (cond
   ((jupyter-org--first-result-context-p context)
    (insert (org-element-interpret-data
             (if (jupyter-org-babel-result-p result)
                 result
               ;; Wrap the result if it can't be removed by
               ;; `org-babel'.
               (jupyter-org-results-drawer result)))))
   (t
    (let ((elems (jupyter-org--prepare-context context)))
      (insert (org-element-interpret-data
               (if elems
                   (apply #'jupyter-org-results-drawer
                          (append elems (list result)))
                 (if (or (jupyter-org-babel-result-p result)
                         (eq (org-element-type context) 'drawer))
                     result
                   (jupyter-org-results-drawer result))))))))
  (when (/= (point) (line-beginning-position))
    ;; Org objects such as file links do not have a newline added when
    ;; converting to their string representation by
    ;; `org-element-interpret-data' so insert one in these cases.
    (insert "\n"))
  (when (and jupyter-org-toggle-latex
             (memq (org-element-type result)
                   '(latex-fragment latex-environment)))
    (save-excursion
      ;; Go to a position contained in the fragment
      (forward-line -1)
      (skip-syntax-forward "-")
      (let ((ov (car (overlays-at (point)))))
        (unless (and ov (eq (overlay-get ov 'org-overlay-type)
                            'org-latex-overlay))
          (org-latex-preview))))))

(defun jupyter-org-inserted-result (data &optional metadata)
  "Return a monadic value that inserts DATA and METADATA as an Org element."
  (jupyter-mlet* ((req (jupyter-get-state)))
    (pcase-let (((cl-struct jupyter-org-request
                            inline-block-p block-params client)
                 req))
      (let ((result (jupyter-org-get-result req data metadata)))
        (jupyter-org-with-point-at req
          (if inline-block-p
              (org-babel-insert-result
               (if (stringp result) result
                 (or (org-element-property :value result) ""))
               (alist-get :result-params block-params)
               nil nil (jupyter-kernel-language client))
            (let ((res-begin (org-babel-where-is-src-block-result 'insert)))
              (goto-char res-begin)
              (let ((context (org-element-context))
                    (indent (current-indentation)))
                ;; Handle file links which are org element objects and are contained
                ;; within paragraph contexts.
                (when (eq (org-element-type context) 'paragraph)
                  (save-excursion
                    (goto-char (jupyter-org-element-begin-after-affiliated context))
                    (when (looking-at-p (format "^[ \t]*%s[ \t]*$" org-link-bracket-re))
                      (setq context (org-element-context)))))
                ;; Skip past the #+RESULTS line
                (forward-line 1)
                (unless (bolp) (insert "\n"))
                (jupyter-org-indent-inserted-region indent
                  (if (jupyter-org--stream-result-p result)
                      (jupyter-org--insert-stream context result)
                    (when (eq (org-element-type result) 'pandoc)
                      (setq result (jupyter-org-pandoc-placeholder-element req result)))
                    (jupyter-org--insert-nonstream context result)))))))
        (jupyter-return result)))))

(defun jupyter-org--start-pandoc-conversion (el cb)
  (jupyter-pandoc-convert
   (org-element-property :type el) "org"
   (org-element-property :value el)
   cb))

(defun jupyter-org-pandoc-placeholder-element (req el)
  "Launch a Pandoc conversion process of EL, return a placeholder string.
REQ is the `jupyter-org-request' which generated EL as a result.

The placeholder string is meant to be inserted into the Org
buffer and replaced with the result of conversion when ready.

EL is an Org element with the properties

    :text  The placeholder text to use.
    :type  The type of syntax from which to convert.
    :value The code with the corresponding syntax."
  (letrec ((buf (current-buffer))
           (src-pos (copy-marker (jupyter-org-request-marker req)))
           (cb (lambda ()
                 (let ((to-string (buffer-string)))
                   (with-current-buffer buf
                     (save-excursion
                       (goto-char src-pos)
                       (set-marker src-pos nil)
                       (when (text-property-search-forward 'jupyter-pandoc proc)
                         (delete-region (point)
                                        (let ((pos (next-single-property-change
                                                    (point) 'jupyter-pandoc)))
                                          (if pos (min (1+ pos) (point-max))
                                            (point-max))))
                         (insert to-string)))))))
           (proc (jupyter-org--start-pandoc-conversion el cb)))
    (jupyter-org-raw-string
     (propertize
      (org-element-property :text el)
      'jupyter-pandoc proc))))

;;;; Add result

(defun jupyter-org-processed-result (data &optional metadata)
  (jupyter-mlet* ((req (jupyter-get-state)))
    (pcase-let (((cl-struct jupyter-org-request silent-p async-p) req))
      (when (equal silent-p "silent")
        (message "%s" (if (jupyter-org-element-p data)
                          (org-element-interpret-data data)
                        (jupyter-map-mime-bundle (jupyter-org-display-mime-types req)
                            (jupyter-normalize-data data metadata)
                          (lambda (_mime content)
                            (org-babel-script-escape (plist-get content :data)))))))
      (cond
       (async-p
        (jupyter-org--clear-async-indicator req)
        (if silent-p (jupyter-return (jupyter-org-get-result req data metadata))
          (jupyter-org-inserted-result data metadata)))
       (t
        (jupyter-return
          (if (jupyter-org-element-p data) data
            (list :data data :metadata metadata))))))))

(defun jupyter-org--add-result (req data &optional metadata)
  (pcase-let (((cl-struct jupyter-org-request async-p) req)
              (result (jupyter-run-with-state req
                        (jupyter-org-processed-result data metadata))))
    (unless async-p
      (push result (jupyter-org-request-results req)))))

;;; org-babel functions
;; These are meant to be called by `org-babel-execute:jupyter'

(defun jupyter-org-pending-async-results (req)
  "Finish up bookkeeping for an asynchronous source block REQ.
Setup `org-babel-after-execute-hook' to insert the ID of REQ as
the result of the associated source block, to signify that the
results of REQ are pending, and run any other hook functions that
were present before this function was called.

This function always returns nil and is intended to be used as
the return value for asynchronous Jupyter source blocks in
`org-babel-execute:jupyter'."
  (prog1 nil
    (let ((log-max message-log-max)
          (hook org-babel-after-execute-hook))
      (setq message-log-max nil)
      (setq org-babel-after-execute-hook
            (list (lambda ()
                    (setq message-log-max log-max)
                    (unwind-protect
                        (jupyter-org--add-result
                         req (list :text/plain (jupyter-org-request-id req)))
                      (setq org-babel-after-execute-hook hook)
                      (run-hooks 'org-babel-after-execute-hook))))))))

(defun jupyter-org--coalesce-stream-results (results)
  "Return RESULTS with all contiguous stream results concatenated."
  (let (lst str)
    (while (consp results)
      (let ((value (pop results)))
        (if (jupyter-org--stream-result-p value)
            (cl-callf concat str value)
          (when str
            (push str lst)
            (setq str nil))
          (push value lst))))
    (when str
      (push str lst))
    (nreverse lst)))

(defun jupyter-org--process-pandoc-results (results)
  (let* ((results (copy-sequence results))
         (head results)
         (procs '()))
    (while head
      (when (eq (org-element-type (car head)) 'pandoc)
        (push
         (jupyter-org--start-pandoc-conversion
          (car head)
          (let ((h head))
            (lambda ()
              (setcar h (jupyter-org-raw-string (buffer-string))))))
         procs))
      (setq head (cdr head)))
    (while procs
      (while (process-live-p (car procs))
        (accept-process-output nil 0.1))
      (pop procs))
    results))

(defun jupyter-org-sync-results (req)
  "Return the result string in org syntax for the results of REQ.
Meant to be used as the return value of
`org-babel-execute:jupyter'."
  (pcase-let (((cl-struct jupyter-org-request block-params
                          results silent-p block-params)
               req))
    (setq results (jupyter-org--coalesce-stream-results (nreverse results)))
    (if silent-p
        (org-babel-script-escape
         (mapconcat
          (lambda (result)
            (if (consp result)
                (or (jupyter-mime-value result :text/plain) "")
              (cl-check-type result string)
              result))
          results
          "\n"))
      (when-let* ((results
                   (mapcar (lambda (r)
                        (if (jupyter-org--stream-result-p r)
                            (jupyter-org-scalar
                             (jupyter-org-strip-last-newline r))
                          r))
                      (jupyter-org--process-pandoc-results
                       (mapcar (apply-partially #'jupyter-org-get-result req)
                          results))))
                  (result-params (alist-get :result-params block-params)))
        (org-element-interpret-data
         (if (or (and (= (length results) 1)
                      (jupyter-org-babel-result-p (car results)))
                 (member "raw" result-params))
             (car results)
           (apply #'jupyter-org-results-drawer results)))))))

(provide 'jupyter-org-client)

;;; jupyter-org-client.el ends here
