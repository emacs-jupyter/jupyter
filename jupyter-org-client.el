;;; jupyter-org-client.el --- Org integration -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 Nathaniel Nicandro

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

(eval-when-compile (require 'subr-x))
(require 'jupyter-repl)
(require 'ob)

(declare-function org-babel-python-table-or-string "ob-python" (results))
(declare-function org-babel-jupyter-initiate-session "ob-jupyter" (&optional session params))
(declare-function org-babel-jupyter-src-block-session "ob-jupyter" ())
(declare-function org-babel-jupyter-language-p "ob-jupyter" (lang))
(declare-function org-element-context "org-element" (&optional element))
(declare-function org-element-create "org-element" (type &optional props &rest children))
(declare-function org-element-type "org-element" (element))
(declare-function org-element-normalize-string "org-element" (s))
(declare-function org-element-at-point "org-element" ())
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
(declare-function org-toggle-latex-fragment "org" (&optional arg))

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
  ())

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
  async-p)

;;; `jupyter-kernel-client' interface

;;;; `jupyter-request' interface

(defvar org-babel-jupyter-current-src-block-params)

(cl-defmethod jupyter-generate-request ((_client jupyter-org-client) _msg)
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
        (let* ((context (org-element-context))
               (block-params org-babel-jupyter-current-src-block-params)
               (result-params (alist-get :result-params block-params)))
          (jupyter-org-request
           :marker (copy-marker org-babel-current-src-block-location)
           :inline-block-p (and (memq (org-element-type context)
                                      '(inline-babel-call inline-src-block))
                                t)
           :result-type (alist-get :result-type block-params)
           :file (alist-get :file block-params)
           :block-params block-params
           :async-p (equal (alist-get :async block-params) "yes")
           :silent-p (car (or (member "none" result-params)
                              (member "silent" result-params))))))
    (cl-call-next-method)))

(cl-defmethod jupyter-drop-request ((_client jupyter-org-client)
                                    (req jupyter-org-request))
  (when (markerp (jupyter-org-request-marker req))
    (set-marker (jupyter-org-request-marker req) nil)))

(defvar org-babel-jupyter-session-clients) ; in ob-jupyter.el

(defun jupyter-org-request-at-point ()
  "Return the `jupyter-org-request' associated with `point' or nil."
  (when-let* ((session (org-babel-jupyter-src-block-session))
              (client (gethash session org-babel-jupyter-session-clients)))
    (catch 'req
      (jupyter-map-pending-requests client
        (lambda (req)
          (when (jupyter-org-request-p req)
            (let ((marker (jupyter-org-request-marker req)))
              (and (equal (marker-position marker) (point))
                   (equal (marker-buffer marker) (current-buffer))
                   (throw 'req req)))))))))

;;;; Stream

(cl-defmethod jupyter-handle-stream ((_client jupyter-org-client) (req jupyter-org-request) msg)
  (jupyter-with-message-content msg (text)
    (if (jupyter-org-request-inline-block-p req)
        (jupyter-with-display-buffer "org-results" req
          (insert (ansi-color-apply text))
          (pop-to-buffer (current-buffer)))
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
    (pop-to-buffer (marker-buffer loc))
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
         (loc (org-with-point-at (jupyter-org-request-marker req)
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
    (cond
     ((or (jupyter-org-request-inline-block-p req)
          (jupyter-org-request-silent-p req))
      ;; Remove old inline results when an error happens since, if this was not
      ;; done, it would look like the code which caused the error produced the
      ;; old result.
      (when (jupyter-org-request-inline-block-p req)
        (org-with-point-at (jupyter-org-request-marker req)
          (org-babel-remove-inline-result)))
      (jupyter-with-display-buffer "traceback" 'reset
        (jupyter-insert-ansi-coded-text traceback)
        (goto-char (point-min))
        (when (jupyter-org-request-silent-p req)
          (insert (jupyter-org--goto-error-string req) "\n\n"))
        (pop-to-buffer (current-buffer))))
     (t
      ;; The keymap property in the string returned by
      ;; `jupyter-org--goto-error-string' gets removed by font-lock so ensure it
      ;; is re-added.
      (unless (memq 'jupyter-org-add-error-keymap org-font-lock-hook)
        (add-hook 'org-font-lock-hook 'jupyter-org-add-error-keymap nil t))
      (setq traceback (ansi-color-apply traceback))
      (jupyter-org--add-result
       req (jupyter-org-comment
            (with-temp-buffer
              (insert traceback)
              (jupyter-org--goto-error-string req))))
      (jupyter-org--add-result req traceback)))))

;;;; Execute result

(cl-defmethod jupyter-handle-execute-result ((client jupyter-org-client) (req jupyter-org-request) msg)
  (unless (eq (jupyter-org-request-result-type req) 'output)
    (jupyter-with-message-content msg (data metadata)
      (cond
       ((jupyter-org-request-inline-block-p req)
        ;; For inline results, only text/plain results are allowed at the moment.
        ;;
        ;; TODO: Handle all of the different macro types for inline results, see
        ;; `org-babel-insert-result'.
        (setq data `(:text/plain ,(plist-get data :text/plain)))
        (let ((result (let ((r (jupyter-org-result req data metadata)))
                        (if (stringp r) r
                          (or (org-element-property :value r) "")))))
          (if (jupyter-org-request-async-p req)
              (org-with-point-at (jupyter-org-request-marker req)
                (org-babel-insert-result
                 result (jupyter-org-request-block-params req)
                 nil nil (jupyter-kernel-language client)))
            ;; The results are returned in `org-babel-execute:jupyter' in the
            ;; synchronous case
            (jupyter-org--add-result req result))))
       (t
        (jupyter-org--add-result req (jupyter-org-result req data metadata)))))))

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
          (pop-to-buffer (current-buffer))
          (set-window-point (get-buffer-window (current-buffer)) (point-min)))
      (jupyter-org--add-result req (jupyter-org-result req data metadata)))))

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

(cl-defmethod jupyter-handle-execute-reply ((_client jupyter-org-client) (req jupyter-org-request) msg)
  (jupyter-with-message-content msg (status payload)
    (when payload
      (org-with-point-at (jupyter-org-request-marker req)
        (jupyter-handle-payload payload)))
    (if (equal status "ok")
        (message "Code block evaluation complete.")
      (message "An error occurred when evaluating code block."))
    (when (jupyter-org-request-async-p req)
      (jupyter-org--clear-request-id req)
      (run-hooks 'org-babel-after-execute-hook))))

;;; Completion in code blocks

(defvar jupyter-org--src-block-cache nil
  "A list of three elements (SESSION BEG END).
SESSION is the Jupyter session to use for completion requests for
a code block between BEG and END.

BEG and END are the bounds of the source block which made the
most recent completion request.")

(defsubst jupyter-org--src-block-beg ()
  (nth 1 jupyter-org--src-block-cache))

(defsubst jupyter-org--src-block-end ()
  (nth 2 jupyter-org--src-block-cache))

(defun jupyter-org--same-src-block-p ()
  (when jupyter-org--src-block-cache
    (cl-destructuring-bind (_ beg end)
        jupyter-org--src-block-cache
      (and
       (marker-position beg)
       (marker-position end)
       (<= beg (point) end)))))

(defun jupyter-org--set-current-src-block ()
  (unless (jupyter-org--same-src-block-p)
    (let* ((el (org-element-at-point))
           (lang (org-element-property :language el)))
      (when (org-babel-jupyter-language-p lang)
        (let* ((info (org-babel-get-src-block-info el))
               (params (nth 2 info))
               (beg (save-excursion
                      (goto-char (org-element-property :post-affiliated el))
                      (line-beginning-position 2)))
               (end (save-excursion
                      (goto-char (org-element-property :end el))
                      (skip-chars-backward "\r\n")
                      (line-beginning-position))))
          (unless jupyter-org--src-block-cache
            (setq jupyter-org--src-block-cache
                  (list nil (point-marker) (point-marker)))
            ;; Move the end marker when text is inserted
            (set-marker-insertion-type (nth 2 jupyter-org--src-block-cache) t))
          (setf (nth 0 jupyter-org--src-block-cache) params)
          (cl-callf move-marker (nth 1 jupyter-org--src-block-cache) beg)
          (cl-callf move-marker (nth 2 jupyter-org--src-block-cache) end))))))

(defmacro jupyter-org-when-in-src-block (&rest body)
  "Evaluate BODY when inside a Jupyter source block.
Return the result of BODY when it is evaluated, otherwise nil is
returned."
  (declare (debug (body)))
  `(if (not (org-in-src-block-p 'inside))
       ;; Invalidate cache when going outside of a source block.  This way if
       ;; the language of the block changes we don't end up using the cache
       ;; since it is only used for Jupyter blocks.
       (when jupyter-org--src-block-cache
         (set-marker (nth 1 jupyter-org--src-block-cache) nil)
         (set-marker (nth 2 jupyter-org--src-block-cache) nil)
         (setq jupyter-org--src-block-cache nil))
     (jupyter-org--set-current-src-block)
     (when (jupyter-org--same-src-block-p)
       ,@body)))

(defmacro jupyter-org-with-src-block-client (&rest body)
  "Evaluate BODY with `jupyter-current-client' set to the session's client.
A client is initialized if necessary.

If `point' is not inside the code of a Jupyter source block, BODY
is not evaluated and nil is returned.  Return the result of BODY
when it is evaluated.

In addition to evaluating BODY with an active Jupyter client set,
the `syntax-table' will be set to that of the REPL buffers."
  (declare (debug (body)))
  (let ((params (make-symbol "params"))
        (syntax (make-symbol "syntax")))
    `(jupyter-org-when-in-src-block
      (let* ((,params (car jupyter-org--src-block-cache))
             (jupyter-current-client
              (buffer-local-value 'jupyter-current-client
                                  (org-babel-jupyter-initiate-session
                                   (alist-get :session ,params) ,params)))
             (,syntax (jupyter-kernel-language-syntax-table
                       jupyter-current-client)))
        (with-syntax-table ,syntax
          ,@body)))))

(cl-defmethod jupyter-code-context ((_type (eql inspect))
                                    &context (major-mode org-mode))
  (when (org-in-src-block-p 'inside)
    (jupyter-line-context)))

(cl-defmethod jupyter-code-context ((_type (eql completion))
                                    &context (major-mode org-mode))
  ;; Always called from within a valid code block.  See
  ;; `jupyter-org-completion-at-point'.
  (list (buffer-substring-no-properties
         (jupyter-org--src-block-beg)
         (jupyter-org--src-block-end))
        (- (point) (jupyter-org--src-block-beg))))

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
     (let ((lang (jupyter-kernel-language jupyter-current-client)))
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
non-nil font-lock-face properties in the region to the face
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
                      begin1 'font-lock-face nil end1))
          (when (get-text-property begin1 'font-lock-face)
            (font-lock-prepend-text-property
             begin1 next 'face (get-text-property begin1 'font-lock-face)))
          (setq begin1 next)))
       (t
        (put-text-property begin next 'jupyter-ansi t)
        (jupyter-ansi-color-apply-on-region begin next)
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
      (jupyter-org-raw-string (jupyter-pandoc-convert type "org" value))
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
  (org-element-create 'link (list :type "file" :path path)))

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
    (if (cl-loop with i = 0 for c across value if (eq c ?\n) do (cl-incf i)
                 thereis (>= i org-babel-min-lines-for-block-output))
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

(defun jupyter-org-latex-fragment (value)
  "Return a latex-fragment `org-element' consisting of VALUE."
  (org-element-create 'latex-fragment (list :value value)))

(defun jupyter-org-latex-environment (value)
  "Return a latex-fragment `org-element' consisting of VALUE."
  (org-element-create 'latex-environment (list :value value)))

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
  (let* ((overwrite (not (null (alist-get :file params))))
         (file (or (alist-get :file params)
                   (jupyter-org-image-file-name
                    (plist-get content :data)
                    (cl-case mime
                      (:image/png "png")
                      (:image/jpeg "jpg")
                      (:image/svg+xml "svg"))))))
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

    '(:data ... :metadata ...)

that contains the data/metadata of the mime type.

As an example, if DATA only contains the mimetype
`:text/markdown', then the returned results is

    (jupyter-org-export-block \"markdown\" data)"
  (ignore))

(defun jupyter-org--find-mime-types (req-types)
  "Return the keywords in `jupyter-org-mime-types' that match REQ-TYPES.

If a match is not found, return nil.  Try to be intelligent and
return what the user might intend to use.

REQ-TYPES can be a string such as `plain', `plain html', or
`text/plain'.  The string `text' is translated to `:text/plain'
and `image' to `:image/png'."
  (when req-types
    ;; Iterate the user-specified mimetypes looking for symbols that match a
    ;; symbol in `jupyter-org-mime-types'.  Invalid mimetypes are ignored.
    (delete nil
            (mapcar
             (lambda (req-type)
               (pcase req-type
                 ("text" :text/plain)
                 ("image" :image/png)
                 ((pred stringp)
                  (let ((regexp (if (string-match "/" req-type)
                                    req-type
                                  (concat "/" req-type "$"))))
                    (cl-loop for ii in jupyter-org-mime-types
                             if (string-match regexp (symbol-name ii))
                             return ii)))))
               (split-string req-types)))))

(cl-defmethod jupyter-org-result ((req jupyter-org-request) plist &optional metadata)
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
  (cl-assert plist json-plist)
  (let* ((params (jupyter-org-request-block-params req))
         (display-mime-types (jupyter-org--find-mime-types
                              (alist-get :display params))))
    ;; Push :file back into PARAMS if it was present in
    ;; `org-babel-execute:jupyter'.  That function removes it because
    ;; we don't want `org-babel-insert-result' to handle it.
    (when (jupyter-org-request-file req)
      (push (cons :file (jupyter-org-request-file req)) params))
    (cond
     ((jupyter-map-mime-bundle (or display-mime-types jupyter-org-mime-types)
          (jupyter-normalize-data plist metadata)
        (lambda (mime content)
          (jupyter-org-result mime content params))))
     (t
      (let ((warning
             (format
              "%s did not return requested mimetype(s): %s"
              (jupyter-message-type (jupyter-request-last-message req))
              (or display-mime-types jupyter-org-mime-types))))
        (display-warning 'jupyter warning))))))

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
    (goto-char (point-min))
    ;; Try to determine if we are in an environment or fragment
    (if (save-excursion
          (forward-char 2)
          (org-inside-LaTeX-fragment-p))
        (org-element-latex-fragment-parser)
      ;; If we are not in a fragment, try to parse an environment
      (let ((env (ignore-errors
                   (org-element-latex-environment-parser
                    (point-max) nil))))
        (if (eq (org-element-type env) 'latex-environment) env
          ;; If all else fails, wrap DATA in a minipage
          ;; environment
          (jupyter-org-latex-environment
           (concat "\
\\begin{minipage}{\\textwidth}
\\begin{flushright}\n" data "\n\\end{flushright}
\\end{minipage}")))))))

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
                        (end (and beg (pcase beg
                                        (?\[ ?\])
                                        (?\{ ?\})
                                        (?\( ?\))))))
              (eq end (aref result (1- (length result))))))
       (org-babel-script-escape result))
      (t result)))))

(cl-defmethod jupyter-org-result ((_mime (eql :text/plain)) content _params)
  (plist-get content :data))

;;;; Helper functions

(defsubst jupyter-org--first-result-context-p (context)
  (cl-case (org-element-type context)
    (drawer (not (equal "RESULTS"
                        (org-element-property :drawer-name context))))
    (t (not (jupyter-org--wrappable-element-p context)))))

(defun jupyter-org--clear-request-id (req)
  "Delete the ID of REQ in the `org-mode' buffer if present."
  (unless (jupyter-org-request-id-cleared-p req)
    (org-with-point-at (jupyter-org-request-marker req)
      (when (search-forward (jupyter-request-id req) nil t)
        (delete-region (line-beginning-position)
                       (1+ (line-end-position)))
        (setf (jupyter-org-request-id-cleared-p req) t)))))

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

;;;; Wrapping results

(defun jupyter-org--wrappable-element-p (element)
  "Return non-nil if ELEMENT can be removed and wrapped in a drawer."
  (or (jupyter-org-babel-result-p element)
      (let ((type (org-element-type element)))
        (or (memq type '(latex-fragment latex-environment))
            ;; TODO: Figure out a better way.  I predict there will be more
            ;; situations where a comment would be useful to add.  That means
            ;; we would have to verify each one.
            (and (eq type 'comment)
                 (equal jupyter-org--goto-error-string
                        (org-element-property :value element)))))))

(defun jupyter-org--wrap-result-maybe (context result)
  "Depending on CONTEXT, wrap RESULT in a drawer."
  (cond
   ((jupyter-org--first-result-context-p context)
    ;; Only wrap the result if it can't be removed by `org-babel'.
    (if (jupyter-org-babel-result-p result) result
      (jupyter-org-results-drawer result)))
   ((jupyter-org--wrappable-element-p context)
    (prog1 (jupyter-org-results-drawer context result)
      ;; Ensure that a #+RESULTS: line is not prepended to context when calling
      ;; `org-element-interpret-data'.
      (org-element-put-property context :results nil)
      ;; Ensure there is no post-blank since `org-element-interpret-data'
      ;; already normalizes the string.
      (org-element-put-property context :post-blank nil)))
   (t
    result)))

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

(defun jupyter-org--stream-context-p (context)
  "Determine if CONTEXT is a stream result.
Return the insertion point to append new stream output if CONTEXT
is a stream result.  Otherwise return nil."
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

(defsubst jupyter-org--append-stream-result-p (context result)
  "Depending on CONTEXT, return the position where RESULT should be appended.
Return nil if CONTEXT does not represent a previous stream result
already present in the buffer or if RESULT is not a stream
result."
  (and (jupyter-org--stream-result-p result)
       (not (jupyter-org--first-result-context-p context))
       (jupyter-org--stream-context-p context)))

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

(defun jupyter-org--fixed-width-to-example-block (element result keep-newline)
  "Replace the fixed-width ELEMENT with an example-block.
Append RESULT to the contents of the block.  If KEEP-NEWLINE is
non-nil, ensure that the appended RESULT begins on a newline."
  (jupyter-org-delete-element element)
  ;; Delete a newline that will be re-inserted by `org-element-interpret-data'.
  (when (eq (char-after) ?\n)
    (delete-char 1))
  (insert (org-element-interpret-data
           (jupyter-org-example-block
            (concat
             (let ((old-result
                    (org-element-normalize-string
                     (org-element-property :value element))))
               (if keep-newline old-result
                 (substring old-result 0 -1)))
             result)))))

;;;;; Append stream result

(defun jupyter-org--append-to-fixed-width (result keep-newline)
  "Append RESULT to the fixed-width element at point.
`point' is assumed to be at the insertion point.  If KEEP-NEWLINE is
non-nil, ensure that the appended RESULT begins on a newline."
  (save-match-data
    (let ((first-newline (string-match "\n" result)))
      (if (not (or first-newline keep-newline))
          (insert result)
        (let (head tail)
          (if keep-newline
              (setq head "\n"
                    tail result)
            (setq head (substring result 0 (1+ first-newline))
                  tail (substring result (1+ first-newline))))
          (insert head)
          ;; Delete the newline that will be re-inserted by
          ;; `org-element-interpret-data'
          (when (eq (char-after) ?\n)
            (delete-char 1))
          (insert (thread-last tail
                    jupyter-org-strip-last-newline
                    jupyter-org-scalar
                    org-element-interpret-data)))))))

(defun jupyter-org--append-to-example-block (result keep-newline)
  "Append RESULT to the end of the current example block.
`point' is assumed to be at the end of the last line of the
example block contents.

If KEEP-NEWLINE is non-nil, add a newline before appending
RESULT."
  ;; Delete the newline that will be re-inserted by the call to
  ;; `org-element-normalize-string'.
  (when (eq (char-after) ?\n)
    (delete-char 1))
  (setq result (org-element-normalize-string result))
  ;; From `org-element-example-block-interpreter'
  (when (and (not org-src-preserve-indentation)
             (/= 0 org-edit-src-content-indentation)
             (version<= "9.2" (org-version)))
    (let ((ind (make-string org-edit-src-content-indentation ?\s))
          head tail)
      (if keep-newline
          (setq head ""
                tail result)
        (let ((first-newline (save-match-data
                               (string-match "\n" result))))
          (setq head (substring result 0 (1+ first-newline))
                tail (substring result (1+ first-newline)))))
      (setq result (concat head (replace-regexp-in-string
                                 "^[ \t]*\\S-"
                                 (concat ind "\\&")
                                 (org-remove-indentation tail))))))
  (insert (concat (when keep-newline "\n") result)))

(defun jupyter-org--append-stream-result (result)
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
                    (cl-loop
                     with i = 0
                     for c across result when (eq c ?\n) do (cl-incf i)
                     finally return i))
                 org-babel-min-lines-for-block-output)))
        (if promote-to-block-p
            (jupyter-org--fixed-width-to-example-block context result keep-newline)
          (jupyter-org--append-to-fixed-width result keep-newline))))
     (t
      (jupyter-org--append-to-example-block result keep-newline)))))

(defsubst jupyter-org--prepare-append-result (context)
  "Depending on CONTEXT do something to prepare for inserting a result.

- If CONTEXT is a drawer move `point' to the end of its contents.

- If CONTEXT is a table, delete it from the buffer and set the
  deleted text as the contents of CONTEXT.

- Otherwise, if CONTEXT is a previous unwrapped result, delete it
  from the buffer.  An unwrapped result is a previous result not
  wrapped in a drawer."
  (cl-case (org-element-type context)
    ;; Go to the end of the drawer to insert the new result.
    (drawer
     (goto-char (jupyter-org-element-contents-end context)))
    ;; Any other context that looks like a result needs to be removed
    ;; since it, along with the new result will be wrapped in a drawer
    ;; and re-inserted into the buffer.
    (table
     ;; The `org-element-contents' of a table is nil which interferes
     ;; with how `org-element-table-interpreter' works when calling
     ;; `org-element-interpret-data' so set the contents and delete
     ;; CONTEXT from the buffer.
     (org-element-set-contents
      context (delete-and-extract-region
               (org-element-property :contents-begin context)
               (jupyter-org-element-end-before-blanks context))))
    (t
     (when (jupyter-org--wrappable-element-p context)
       (jupyter-org-delete-element context)
       (jupyter-org-delete-blank-line)))))

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

(defsubst jupyter-org--normalized-insertion-context ()
  "Return an `org-element' of the current results context.
Assumes `point' is on the #+RESULTS keyword line."
  (let ((context (org-element-context)))
    ;; Handle file links which are org element objects and are contained
    ;; within paragraph contexts.
    (when (eq (org-element-type context) 'paragraph)
      (save-excursion
        (goto-char (jupyter-org-element-begin-after-affiliated context))
        (when (looking-at-p (format "^[ \t]*%s[ \t]*$" org-bracket-link-regexp))
          (setq context (org-element-context)))))
    context))

(defun jupyter-org--do-insert-result (req result)
  (org-with-point-at (jupyter-org-request-marker req)
    (let ((res-begin (org-babel-where-is-src-block-result 'insert)))
      (goto-char res-begin)
      (let* ((indent (current-indentation))
             (context (jupyter-org--normalized-insertion-context))
             (pos (jupyter-org--append-stream-result-p context result)))
        (cond
         (pos
          (goto-char pos)
          (jupyter-org-indent-inserted-region indent
            (jupyter-org--append-stream-result result)))
         (t
          (forward-line 1)
          (unless (bolp) (insert "\n"))
          (jupyter-org--prepare-append-result context)
          (jupyter-org-indent-inserted-region indent
            (jupyter-org--insert-result req context result))))
        (when (jupyter-org--stream-result-p result)
          (let ((end (point-marker)))
            (unwind-protect
                (jupyter-org--handle-control-codes
                 (if pos (save-excursion
                           (goto-char pos)
                           ;; Go back one line to account for an edge case
                           ;; where a control code is at the end of a line.
                           (line-beginning-position 0))
                   res-begin)
                 end)
              (set-marker end nil)))
          (jupyter-org--mark-stream-result-newline result))))))

(cl-defgeneric jupyter-org--insert-result (req context result)
  "For REQ and given CONTEXT, insert RESULT.
REQ is a `jupyter-org-request' that contains the context of the
source block for which RESULT will be inserted as a result of.

CONTEXT is the `org-element-context' for the results of the
source block associated with REQ.

RESULT is the new result, as an org element, to be inserted.")

(cl-defmethod jupyter-org--insert-result (_req context result)
  (insert (org-element-interpret-data
           (jupyter-org--wrap-result-maybe
            context (if (jupyter-org--stream-result-p result)
                        (thread-last result
                          jupyter-org-strip-last-newline
                          jupyter-org-scalar)
                      result))))
  (when (/= (point) (line-beginning-position))
    ;; Org objects such as file links do not have a newline added when
    ;; converting to their string representation by
    ;; `org-element-interpret-data' so insert one in these cases.
    (insert "\n")))

(cl-defmethod jupyter-org--insert-result :after (_req _context result)
  "Toggle display of LaTeX fragment results depending on `jupyter-org-toggle-latex'."
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
          (org-toggle-latex-fragment))))))

;;;; Add result

(defun jupyter-org--add-result (req result)
  (cond
   ((jupyter-org-request-silent-p req)
    (unless (equal (jupyter-org-request-silent-p req) "none")
      (message "%s" (org-element-interpret-data result))))
   ((jupyter-org-request-async-p req)
    (jupyter-org--clear-request-id req)
    (jupyter-org--do-insert-result req result))
   (t
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
          (hook org-babel-after-execute-hook)
          (id (jupyter-org-scalar (jupyter-org-request-id req))))
      (setq message-log-max nil)
      (setq org-babel-after-execute-hook
            (list (lambda ()
                    (setq message-log-max log-max)
                    (unwind-protect
                        (jupyter-org--add-result req id)
                      (setq org-babel-after-execute-hook hook)
                      (run-hooks 'org-babel-after-execute-hook))))))))

(defun jupyter-org--coalesce-stream-results (results)
  "Return RESULTS with all contiguous stream results concatenated.
All stream results are then turned into fixed-width or
example-block elements."
  (let ((head-to-scalar
         (lambda (a)
           ;; Convert the head element of A to a scalar if its a
           ;; stream result, return A.
           (when (jupyter-org--stream-result-p (car a))
             (setcar a (jupyter-org-scalar
                        (jupyter-org-strip-last-newline
                         (car a)))))
           a)))
    (nreverse
     (funcall
      head-to-scalar
      (cl-reduce
       (lambda (a b)
         (if (and (jupyter-org--stream-result-p b)
                  (jupyter-org--stream-result-p (car a)))
             (setcar a (concat (car a) b))
           (funcall head-to-scalar a)
           (push b a))
         a)
       results
       :initial-value nil)))))

(defun jupyter-org-sync-results (req)
  "Return the result string in org syntax for the results of REQ.
Meant to be used as the return value of
`org-babel-execute:jupyter'."
  (when-let* ((results (jupyter-org--coalesce-stream-results
                        (nreverse (jupyter-org-request-results req))))
              (params (jupyter-org-request-block-params req))
              (result-params (alist-get :result-params params)))
    (org-element-interpret-data
     (if (or (and (= (length results) 1)
                  (jupyter-org-babel-result-p (car results)))
             (member "raw" result-params))
         (car results)
       (apply #'jupyter-org-results-drawer results)))))

(provide 'jupyter-org-client)

;;; jupyter-org-client.el ends here
