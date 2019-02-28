;;; jupyter-org-client.el --- Org integration -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 02 Jun 2018
;; Version: 0.7.1

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

;; A subclass of a Jupyter kernel client that integrates with `org-mode'
;; src-blocks.

;;; Code:

(require 'jupyter-repl)
(require 'ob)
(require 'org-element)

(declare-function org-element-context "org-element" (&optional element))
(declare-function org-in-src-block-p "org" (&optional inside))
(declare-function org-babel-python-table-or-string "ob-python" (results))
(declare-function org-babel-jupyter-initiate-session "ob-jupyter" (&optional session params))

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

(defconst jupyter-org-mime-types '(:text/org
                                   ;; Prioritize images over html
                                   :image/svg+xml :image/jpeg :image/png
                                   :text/html :text/markdown
                                   :text/latex :text/plain)
  "MIME types handled by Jupyter Org.")

(defclass jupyter-org-client (jupyter-repl-client)
  ((block-params
    :initform nil
    :documentation "The parameters of the most recently executed
source code block. Set by `org-babel-execute:jupyter'.")))

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
  async)

;;; `jupyter-kernel-client' interface

;;;; `jupyter-request' interface

(cl-defmethod jupyter-generate-request ((client jupyter-org-client) _msg
                                        &context (major-mode (eql org-mode)))
  "Return a `jupyter-org-request' for the current source code block."
  (if org-babel-current-src-block-location
      ;; Only use a `jupyter-org-request' when executing code blocks, setting
      ;; the `major-mode' context isn't enough, consider when a client is
      ;; started due to sending a completion request.
      (let* ((block-params (oref client block-params))
             (result-params (alist-get :result-params block-params)))
        (jupyter-org-request
         :marker (copy-marker org-babel-current-src-block-location)
         :inline-block-p (save-excursion
                           (goto-char org-babel-current-src-block-location)
                           (and (memq (org-element-type (org-element-context))
                                      '(inline-babel-call inline-src-block))
                                t))
         :result-type (alist-get :result-type block-params)
         :file (alist-get :file block-params)
         :block-params block-params
         :async (equal (alist-get :async block-params) "yes")
         :silent-p (car (or (member "none" result-params)
                            (member "silent" result-params)))))
    (cl-call-next-method)))

(cl-defmethod jupyter-drop-request ((_client jupyter-org-client)
                                    (req jupyter-org-request))
  (when (markerp (jupyter-org-request-marker req))
    (set-marker (jupyter-org-request-marker req) nil)))

;;;; Stream

(cl-defmethod jupyter-handle-stream ((_client jupyter-org-client)
                                     (req jupyter-org-request)
                                     _name
                                     text)
  (if (jupyter-org-request-inline-block-p req)
      (jupyter-with-display-buffer "org-results" req
        (insert (ansi-color-apply text))
        (pop-to-buffer (current-buffer)))
    (jupyter-org--add-result
     req (with-temp-buffer
           (jupyter-with-control-code-handling
            (insert (ansi-color-apply text)))
           (buffer-string)))))

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
line in the previous source block. See
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
execution of a source block. It should return the line number
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

(cl-defmethod jupyter-handle-error ((_client jupyter-org-client)
                                    (req jupyter-org-request)
                                    _ename
                                    _evalue
                                    traceback)
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
    (jupyter-org--add-result req traceback))))

;;;; Execute result

(cl-defmethod jupyter-handle-execute-result ((_client jupyter-org-client)
                                             (req jupyter-org-request)
                                             _execution-count
                                             data
                                             metadata)
  (unless (eq (jupyter-org-request-result-type req) 'output)
    (cond
     ((jupyter-org-request-inline-block-p req)
      ;; For inline results, only text/plain results are allowed
      ;;
      ;; TODO: Possibly file links are allowed as well. See
      ;; `org-babel-insert-result'
      (setq data (plist-get data :text/plain))
      (if (jupyter-org-request-async req)
          (org-with-point-at (jupyter-org-request-marker req)
            (org-babel-insert-result data))
        ;; The results are returned in `org-babel-execute:jupyter' in the
        ;; synchronous case
        (jupyter-org--add-result req data)))
     (t
      (jupyter-org--add-result req (jupyter-org-result req data metadata))))))

;;;; Display data

(cl-defmethod jupyter-handle-display-data ((_client jupyter-org-client)
                                           (req jupyter-org-request)
                                           data
                                           metadata
                                           ;; TODO: Add request objects as text
                                           ;; properties of source code blocks
                                           ;; to implement display IDs. Or how
                                           ;; can #+NAME be used as a display
                                           ;; ID?
                                           _transient)
  ;; Only the data of the execute-result message is inserted into the buffer
  ;; for inline code blocks.
  (if (jupyter-org-request-inline-block-p req)
      (jupyter-with-display-buffer "org-results" req
        (jupyter-insert data metadata)
        (pop-to-buffer (current-buffer))
        (set-window-point (get-buffer-window (current-buffer)) (point-min)))
    (jupyter-org--add-result req (jupyter-org-result req data metadata))))

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

(cl-defmethod jupyter-handle-execute-reply ((_client jupyter-org-client)
                                            (req jupyter-org-request)
                                            status
                                            _execution-count
                                            _user-expressions
                                            payload)
  (when payload
    (org-with-point-at (jupyter-org-request-marker req)
      (jupyter-handle-payload payload)))
  (if (equal status "ok")
      (message "Code block evaluation complete.")
    (message "An error occurred when evaluating code block."))
  (when (jupyter-org-request-async req)
    (jupyter-org--clear-request-id req)
    (run-hooks 'org-babel-after-execute-hook)))

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
      (when (string-prefix-p "jupyter-" lang)
        (let* ((info (org-babel-get-src-block-info el))
               (params (nth 2 info))
               (beg (save-excursion
                      (goto-char
                       (org-element-property :post-affiliated el))
                      (line-beginning-position 2)))
               (end (save-excursion
                      (goto-char
                       (org-element-property :end el))
                      (let ((pblank (org-element-property :post-blank el)))
                        (line-beginning-position
                         (unless (zerop pblank) (- pblank)))))))
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
       ;; Invalidate cache when going outside of a source block. This way if
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

If `point' is not at a Jupyter source block, BODY is not
evaluated and nil is returned. Return the result of BODY when it
is evaluated.

In addition to evaluating BODY with an active Jupyter client set,
the `syntax-table' will be set to that of the REPL buffers."
  (declare (debug (body)))
  `(jupyter-org-when-in-src-block
    (let* ((params (car jupyter-org--src-block-cache))
           (jupyter-current-client
            (buffer-local-value 'jupyter-current-client
                                (org-babel-jupyter-initiate-session
                                 (alist-get :session params) params)))
           (syntax (jupyter-kernel-language-syntax-table jupyter-current-client)))
      (with-syntax-table syntax
        ,@body))))

(cl-defmethod jupyter-code-context ((_type (eql inspect))
                                    &context (major-mode org-mode))
  (when (org-in-src-block-p 'inside)
    (jupyter-line-context)))

(cl-defmethod jupyter-code-context ((_type (eql completion))
                                    &context (major-mode org-mode))
  ;; Always called from within a valid code block. See
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
                                        &rest _)
  (jupyter-org-with-src-block-client
   (cl-call-next-method)))

;;; Org helper functions

;;;###autoload
(defun jupyter-org-insert-src-block (&optional below)
  "Insert a src block above the current point.
With prefix arg BELOW, insert it below the current point.

If point is in a block, copy the header to the new block"
  (interactive "P")
  (if (org-in-src-block-p)
      (let* ((src (org-element-context))
             (start (org-element-property :begin src))
             (end (org-element-property :end src))
             (lang (org-element-property :language src))
             (switches (org-element-property :switches src))
             (parameters (org-element-property :parameters src))
             location)
        (if below
            (progn
              (goto-char start)
              (setq location (org-babel-where-is-src-block-result nil nil))
              (if (not  location)
                  (goto-char end)
                (goto-char location)
                (goto-char (org-element-property :end (org-element-context))))
              (insert (format "#+BEGIN_SRC %s\n\n#+END_SRC\n\n"
                              (concat lang (and lang " ")
                                      switches (and switches " ")
                                      parameters (and parameters " "))))
              (forward-line -3))
          ;; after current block
          (goto-char (org-element-property :begin (org-element-context)))
          (insert (format "#+BEGIN_SRC %s\n\n#+END_SRC\n\n"
                          (concat lang (and lang " ")
                                  switches (and switches " ")
                                  parameters (and parameters " "))))
          (forward-line -3)))

    ;; Not in a src block, just insert a block
    (beginning-of-line)
    (insert (format "#+BEGIN_SRC %s\n\n#+END_SRC\n" (completing-read "Language: " (mapcar 'car org-babel-load-languages))))
    (forward-line -1)))

;;;###autoload
(defun jupyter-org-split-src-block (&optional below)
  "Split the current src block with point in upper block.

With a prefix BELOW move point to lower block."
  (interactive "P")
  (let* ((src-block (org-element-context)))
    (unless (eq (org-element-type src-block) 'src-block)
      (error "Not in a source block"))
    (let ((lang (org-element-property :language src-block))
          (switches (org-element-property :switches src-block))
          (parameters (org-element-property :parameters src-block)))
      (beginning-of-line)
      (insert (format "#+END_SRC\n\n#+BEGIN_SRC %s\n"
                      (concat lang (and lang " ")
                              switches (and switches " ")
                              parameters (and parameters " "))))
      (unless below
        (beginning-of-line)
        (forward-line -3)
        (forward-char -1)))))

;;;###autoload
(defun jupyter-org-execute-and-next-block (&optional new)
  "Execute this block and jump or add a new one.

If a new block is created, use the same language, switches and parameters.
With prefix arg NEW, always insert new cell."
  (interactive "P")
  (org-babel-execute-src-block)
  ;; we ignore-errors here because when there is no next block it is a
  ;; ... user-error, not nil.
  (let* ((lang (car (org-babel-get-src-block-info t)))
         (next-block (ignore-errors
                       (save-excursion
                         (catch 'block
                           (while (setq next-block (org-babel-next-src-block))
                             (when (string= lang (org-element-property :language (org-element-context)))
                               (throw 'block next-block))))))))
    (if (or new (not next-block))
        (jupyter-org-insert-src-block t)
      (goto-char (match-beginning 0)))))

;;;###autoload
(defun jupyter-org-execute-to-point ()
  "Execute all the src blocks that start before point."
  (interactive)
  (let ((p (point)))
    (save-excursion
      (goto-char (point-min))
      (while (and (org-babel-next-src-block) (< (point) p))
        (org-babel-execute-src-block)))))

;;;###autoload
(defun jupyter-org-restart-and-execute-to-point ()
  "Kill the kernel and run src-blocks to point."
  (interactive)
  ;; FIXME: restaring the kernel does not work
  (call-interactively #'jupyter-repl-restart-kernel)
  (jupyter-org-execute-to-point))

;;;###autoload
(defun jupyter-org-restart-kernel-execute-buffer ()
  "Restart kernel and execute buffer."
  (interactive)
  ;; FIXME: restaring the kernel does not work
  (call-interactively #'jupyter-repl-restart-kernel)
  (org-babel-execute-buffer))

;;;###autoload
(defun jupyter-org-jump-to-visible-block ()
  "Jump to a visible src block with avy."
  (interactive)
  (avy-with jupyter-org-jump-to-block
            (avy-jump "#\\+BEGIN_SRC" :beg (point-min) :end (point-max))))

;;;###autoload
(defun jupyter-org-jump-to-block (&optional N)
  "Jump to a block in the buffer.
If narrowing is in effect, only a block in the narrowed region.
Use a numeric prefix N to specify how many lines of context to use.
Defaults to 3."
  (interactive "p")
  (let ((p '()))
    (when (= 1 N) (setq N 3))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-babel-src-block-regexp nil t)
        (push (list (format "line %s:\n%s"
                            (line-number-at-pos (match-beginning 0))
                            (save-excursion
                              (goto-char (match-beginning 0))
                              (let ((s (point)))
                                (forward-line N)
                                (buffer-substring s (point)))))
                    (line-number-at-pos (match-beginning 0)))
              p)))
    (ivy-read "block: " (reverse p)
              :action (lambda (candidate)
                        (goto-char (point-min))
                        (forward-line (1- (second candidate)))
                        (outline-show-entry)
                        (recenter)))))

;;;###autoload
(defun jupyter-org-edit-header ()
  "Edit the src-block header in the minibuffer."
  (interactive)
  (let ((src-info (org-babel-get-src-block-info 'light)))
    (unless src-info
      (error "Not in a source block"))
    (let* ((header-start (sixth src-info))
           (header-end (save-excursion (goto-char header-start)
                                       (line-end-position))))
      (setf (buffer-substring header-start header-end)
            (read-string "Header: "
                         (buffer-substring header-start header-end))))))

;;;###autoload
(defun jupyter-org-kill-block-and-results ()
  "Kill the block and its results."
  (interactive)
  (let ((src (org-element-context))
        (result-start (org-babel-where-is-src-block-result))
        end)
    (if result-start
        (save-excursion
          (goto-char result-start)
          (setq end (org-element-property :end src)))
      (setq end (org-element-property :end src)))
    (kill-region
     (org-element-property :begin src)
     end)))

;;;###autoload
(defun jupyter-org-copy-block-and-results ()
  "Copy the src block at the current point and its results."
  (interactive)
  (let ((src (org-element-context))
        (result-start (org-babel-where-is-src-block-result))
        end)
    (if result-start
        (save-excursion
          (goto-char result-start)
          (setq end (org-babel-result-end)))
      (setq end (org-element-property :end src)))
    (kill-new
     (buffer-substring
      (org-element-property :begin src)
      end))))

;;;###autoload
(defun jupyter-org-clone-block (&optional below)
  "Clone the block."
  (interactive "P")
  (let* ((src (org-element-context))
         (code (org-element-property :value src)))
    (unless (eq (org-element-type src) 'src-block)
      (error "Not in a source block"))
    (jupyter-org-insert-src-block (not below))
    (delete-char 1)
    (insert code)
    ;; jump back to start of new block
    (org-babel-previous-src-block)
    (org-babel-next-src-block)))

;;;###autoload
(defun jupyter-org-merge-blocks ()
  "Merge the current block with the next block."
  (interactive)
  (let ((current-src-block (org-element-context))
        (next-src-block (save-excursion
                          (org-babel-next-src-block)
                          (org-element-context))))
    (unless (eq (org-element-type current-src-block) 'src-block)
      (error "Not in a source block"))
    (let ((merged-code (concat (org-element-property :value current-src-block)
                               (org-element-property :value next-src-block)))
          (lang (org-element-property :language current-src-block))
          (switches (or (org-element-property :switches current-src-block) ""))
          (parameters (or (org-element-property :parameters current-src-block) "")))
      ;; Remove source blocks
      (mapc (lambda (src-block)
              (goto-char (org-element-property :begin src-block))
              (org-babel-remove-result)
              (setf (buffer-substring (org-element-property :begin src-block)
                                      (org-element-property :end src-block))
                    ""))
            (list next-src-block current-src-block))
      ;; Now create the merged block, point is where the current block was
      (insert (format "#+BEGIN_SRC %s %s %s\n%s#+END_SRC\n\n"
                      lang switches parameters merged-code))
      (forward-line -3)
      (end-of-line))))

;;;###autoload
(defun jupyter-org-move-src-block (&optional below)
  "Move source block before of after another.

If BELOW is non-nil, move the block down, otherwise move it up."
  (interactive)
  ;;skip all this if there are no previous or next source block
  (when (condition-case nil
            (save-excursion
              (if below
                  (org-babel-next-src-block)
                (org-babel-previous-src-block)))
          (error nil))
    (let* ((src (org-element-context))
           (results-start (org-babel-where-is-src-block-result))
           (results-end
            (when results-start
              (save-excursion
                (goto-char results-start)
                (goto-char (org-babel-result-end))
                ;; if line is empty, take that empty line by moving down
                (when (looking-at-p "[[:space:]]*$")
                  (forward-line 1))
                (point)))))
      ;; kill from the start of the source block to the end of the results
      ;; ... if there are no results, to the end of the source block
      (kill-region
       (org-element-property :begin src)
       (or results-end (org-element-property :end src))))
    (if below
        ;; if below, move past the next source block or its result
        (let ((next-src-block-head (org-babel-where-is-src-block-head)))
          (if next-src-block-head
              (goto-char next-src-block-head)
            (org-babel-next-src-block))
          (let ((next-src-block (org-element-context))
                (next-results-start (org-babel-where-is-src-block-result)))
            (if next-results-start
                (progn
                  (goto-char next-results-start)
                  (goto-char (org-babel-result-end))
                  (when (looking-at-p "[[:space:]]*$")
                    (forward-line 1)))
              (goto-char (org-element-property :end next-src-block)))))
      ;; else, move to the begining of the previous block
      (org-babel-previous-src-block))
    (save-excursion (org-yank)))) ; keep cursor where the yank takes place

;;;###autoload
(defun jupyter-org-clear-all-results ()
  "Clear all results in the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (org-babel-next-src-block)
      (org-babel-remove-result))))

;;;###autoload
(defun jupyter-org-restart-kernel-execute-block ()
  "Restart kernel and execute block."
  (interactive)
  (let* ((params (car jupyter-org--src-block-cache))
         (jupyter-current-client
          (buffer-local-value 'jupyter-current-client
                              (org-babel-jupyter-initiate-session
                               (alist-get :session params) params))))
    ;; FIXME: retarting the kernel does not work
    (jupyter-repl-restart-kernel))
  (org-babel-execute-src-block-maybe))

(with-eval-after-load 'hydra
  (defhydra jupyter-org-hydra (:color blue :hint nil)
    "
        Execute                   Navigate       Edit             Misc
----------------------------------------------------------------------
    _<return>_: current           _i_: previous  _w_: move up     _/_: inspect
  _S-<return>_: current to next   _k_: next      _s_: move down   _l_: clear result
_S-M-<return>_: to point          _g_: visible   _x_: kill        _L_: clear all
  _s-<return>_: Restart/block     _G_: any       _n_: copy
_M-s-<return>_: Restart/to point  ^ ^            _c_: clone
  _H-<return>_: Restart/buffer    ^ ^            _m_: merge
           _r_: Goto repl         ^ ^            _-_: split
           ^ ^                    ^ ^            _+_: insert above
           ^ ^                    ^ ^            _=_: insert below
           ^ ^                    ^ ^            _h_: header"
    ("<return>" org-ctrl-c-ctrl-c :color red)
    ("S-<return>" jupyter-org-execute-and-next-block :color red)
    ("S-M-<return>" jupyter-org-execute-to-point)
    ("s-<return>" jupyter-org-restart-kernel-execute-block)
    ("M-s-<return>" jupyter-org-restart-and-execute-to-point)
    ("H-<return>" jupyter-org-restart-kernel-execute-buffer)
    ("r" org-babel-switch-to-session)

    ("i" org-babel-previous-src-block :color red)
    ("k" org-babel-next-src-block :color red)
    ("g" jupyter-org-jump-to-visible-block)
    ("G" jupyter-org-jump-to-block)

    ("w" jupyter-org-ob-move-src-block :color red)
    ("s" (jupyter-org-move-src-block t) :color red)
    ("x" jupyter-org-kill-block-and-results)
    ("n" jupyter-org-copy-block-and-results)
    ("c" jupyter-org-clone-block)
    ("m" jupyter-org-merge-blocks)
    ("-" jupyter-org-split-src-block)
    ("+" jupyter-org-insert-src-block)
    ("=" (jupyter-org-insert-src-block t))
    ("l" org-babel-remove-result)
    ("L" jupyter-org-clear-all-results)
    ("h" jupyter-org-edit-header)

    ("/" jupyter-inspect-at-point)))

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
  (jupyter-org-with-src-block-client
   (let ((lang (jupyter-kernel-language jupyter-current-client)))
     (or (jupyter-org--key-def key `[,lang])
         (jupyter-org--key-def key [jupyter])))))

(defun jupyter-org--call-with-src-block-client (def)
  "Call DEF interactively with the current src-block's client."
  (jupyter-org-with-src-block-client
   (call-interactively def)))

(defun jupyter-org-define-key (key def &optional lang)
  "Bind KEY to DEF, but only when inside a Jupyter code block.

When `point' is inside a Jupyter code block, DEF is called using
the `jupyter-current-client' of the session associated with the
code block, see `jupyter-org-with-src-block-client'.

If LANG is non-nil, it is a language symbol such as python or
julia. Only bind KEY to DEF whenever the underlying kernel
language is LANG. If LANG is nil, then KEY is bound to DEF
regardless of kernel language. Note, the same key can be bound
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
  ;; NOTE: This will set the key multiple times if the same binding is used
  ;; for different kernel languages even though it only needs to be defined
  ;; once. `lookup-key' won't work for checking if it is already defined since
  ;; the filter function of the menu-item returns nil if a client isn't
  ;; defined.
  (define-key jupyter-org-interaction-mode-map key
    `(menu-item
      "" nil :filter
      ,(apply-partially #'jupyter-org--define-key-filter key))))

(jupyter-org-define-key (kbd "C-x C-e") #'jupyter-eval-line-or-region)
(jupyter-org-define-key (kbd "C-M-x") #'jupyter-eval-defun)
(jupyter-org-define-key (kbd "M-i") #'jupyter-inspect-at-point)
(jupyter-org-define-key (kbd "C-c C-r") #'jupyter-repl-restart-kernel)
(jupyter-org-define-key (kbd "C-c C-i") #'jupyter-repl-interrupt-kernel)
;; FIXME: find a better map? We want the hydra to be available outside of jupyter blocks
(with-eval-after-load 'hydra
  (local-set-key (kbd "C-c h") #'jupyter-org-hydra/body))

;;; `jupyter-org-interaction-mode'

(define-minor-mode jupyter-org-interaction-mode
  "Minor mode for interacting with a Jupyter REPL from an `org-mode' buffer.
When this minor mode is enabled, some of the keybindings
available in `jupyter-repl-interaction-mode' are also available
when `point' is inside a Jupyter code block. Completion is also
enabled when `point' is inside a code block.

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
    (add-hook 'after-revert-hook 'jupyter-org-interaction-mode nil t))
   (t
    (remove-hook 'completion-at-point-functions 'jupyter-org-completion-at-point t)
    (remove-hook 'after-revert-hook 'jupyter-org-interaction-mode t))))

(add-hook 'org-mode-hook 'jupyter-org-interaction-mode)

;;; Constructing org syntax trees

(defun jupyter-org-object-p (element)
  "Return non-nil if ELEMENT's type is a member of `org-element-all-objects'."
  (memq (org-element-type element) org-element-all-objects))

(defun jupyter-org-raw-string-p (str)
  "Return non-nil if STR can be inserted as is during result insertion."
  (and (stringp str) (get-text-property 0 'jupyter-org str)))

(defun jupyter-org-raw-string (str)
  "Return STR, ensuring that it is flagged as already being `org' syntax.
Adds a non-nil jupyter-org text property on the first character
of STR. If a string returned by `jupyter-org-result' has a
non-nil jupyter-org property on the first character, it is
inserted without modification as the result of a code block."
  (prog1 str
    (put-text-property 0 1 'jupyter-org t str)))

(defun jupyter-org-comment (value)
  "Return a comment `org-element' with VALUE."
  (list 'comment (list :value value)))

(defun jupyter-org-export-block (type value)
  "Return an export-block `org-element'.
The block will export TYPE and the contents of the block will be
VALUE."
  (list 'export-block (list :type type
                            :value (org-element-normalize-string value))))

(defun jupyter-org-file-link (path)
  "Return a file link `org-element' that points to PATH."
  (list 'link (list :type "file" :path path)))

(defun jupyter-org-image-link (path &optional width height)
  "Return an `org-element' for an image at PATH.
If a WIDTH or HEIGHT are provided, then return a paragraph
element with an affiliated keyword ATTR_ORG. So that the image
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
        (list 'paragraph (list :attr_org (list attrs))
              (jupyter-org-file-link path)
              "\n"))
    (jupyter-org-file-link path)))

(defun jupyter-org-src-block (language parameters value &optional switches)
  "Return a src-block `org-element'.
LANGUAGE, PARAMETERS, VALUE, and SWITCHES all have the same
meaning as a src-block `org-element'."
  (declare (indent 2))
  (list 'src-block (list :language language
                         :parameters parameters
                         :switches switches
                         :value value)))

(defun jupyter-org-example-block (value)
  "Return an example-block `org-element' with VALUE."
  (list 'example-block (list :value (org-element-normalize-string value))))

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
`org-mode' table as a string. To distinguish the table from a
regular string, it has a non-nil org-table text property on its
first character.

Otherwise, return VALUE formated as a fixed-width `org-element'."
  (cond
   ((stringp value)
    (if (cl-loop with i = 0 for c across value if (eq c ?\n) do (cl-incf i)
                 thereis (>= i org-babel-min-lines-for-block-output))
        (jupyter-org-example-block value)
      (list 'fixed-width (list :value value))))
   ((and (listp value)
         (or (memq (car value) org-element-all-objects)
             (memq (car value) org-element-all-elements)))
    value)
   ((and (listp value)
         (jupyter-org-tabulablep value))
    (let ((table (jupyter-org-raw-string (jupyter-org-table-to-orgtbl value))))
      (prog1 table
        ;; We need a way to distinguish a table string that is easily removed
        ;; from the code block vs a regular string that will need to be
        ;; wrapped in a drawer. See `jupyter-org--append-result'.
        (put-text-property 0 1 'org-table t table))))
   (t
    (list 'fixed-width (list :value (format "%S" value))))))

(defun jupyter-org-latex-fragment (value)
  "Return a latex-fragment `org-element' consisting of VALUE."
  (list 'latex-fragment (list :value value)))

(defun jupyter-org-latex-environment (value)
  "Return a latex-fragment `org-element' consisting of VALUE."
  (list 'latex-environment (list :value value)))

(defun jupyter-org-results-drawer (&rest results)
  "Return a drawer `org-element' containing RESULTS.
RESULTS can be either strings or other `org-element's. Newlines
are added after every `org-element' object in RESULTS, such as
file links, so that each result appears on a single line in the
string representation of the drawer. The returned drawer has a
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

(defun jupyter-org--image-result (mime params b64-encoded data &optional metadata)
  "Return an org-element suitable for inserting an image.
MIME is the image mimetype, PARAMS is the
`jupyter-org-request-block-params' that caused this result to be
returned and DATA is the image data.

If B64-ENCODED is non-nil, DATA assumed to be a base64 encoded
string and will be decoded before writing to file.

If PARAMS contains a :file key, it is used as the FILENAME.
Otherwise a file name is created, see
`jupyter-org-image-file-name'. Note, when PARAMS contains a :file
key any existing file with the file name will be overwritten when
writing the image data. This is not the case when a new file name
is created.

If METADATA contains a :width or :height key, then the returned
org-element will have an ATTR_ORG affiliated keyword containing
the width or height of the image."
  (let* ((overwrite (not (null (alist-get :file params))))
         (file (or (alist-get :file params)
                   (jupyter-org-image-file-name
                    data (cl-case mime
                           (:image/png "png")
                           (:image/jpeg "jpg")
                           (:image/svg+xml "svg"))))))
    (when (or overwrite (not (file-exists-p file)))
      (let ((buffer-file-coding-system
             (if b64-encoded 'binary
               buffer-file-coding-system))
            (require-final-newline nil))
        (with-temp-file file
          (insert data)
          (when b64-encoded
            (base64-decode-region (point-min) (point-max))))))
    (cl-destructuring-bind (&key width height &allow-other-keys)
        metadata
      (jupyter-org-image-link file width height))))

(cl-defgeneric jupyter-org-result (_mime _params _data &optional _metadata)
  "Return an `org-element' representing a result.
Either a string or an `org-element' is a valid return value of
this method. The former will be inserted as is, while the latter
will be inserted by calling `org-element-interpret-data' first.

The returned result should be a representation of a MIME type
whose value is DATA.

DATA is the data representing the MIME type and METADATA is any
associated metadata associated with the MIME DATA.

As an example, if DATA only contains the mimetype
`:text/markdown', then the returned results is

    (jupyter-org-export-block \"markdown\" data)"
  (ignore))

(defun jupyter-org--find-mime-types (req-types)
  "Return the keywords in `jupyter-org-mime-types' that match REQ-TYPES.

If a match is not found, return nil. Try to be intelligent and
return what the user might intend to use.

REQ-TYPES can be a string such as `plain', `plain html', or
`text/plain'. The string `text' is translated to `:text/plain'
and `image' to `:image/png'."
  (when req-types
    ;; Iterate the user-specified mimetypes looking for symbols that match a
    ;; symbol in `jupyter-org-mime-types'. Invalid mimetypes are ignored.
    (delete nil
            (mapcar (lambda (req-type)
                 (cond
                  ((string= req-type "text") :text/plain)
                  ((string= req-type "image") :image/png)
                  ((stringp req-type)
                   (let ((regexp (if (string-match "/" req-type)
                                     req-type
                                   (concat "/" req-type "$"))))
                     (cl-loop for ii in jupyter-org-mime-types
                              if (string-match regexp (symbol-name ii))
                              return ii)))))
               (split-string req-types)))))

(cl-defmethod jupyter-org-result ((req jupyter-org-request) plist
                                  &optional metadata &rest _)
  "For REQ, return the rendered DATA.
PLIST is a property list, (:mimetype1 value1 ...), containing the
different representations of a result returned by a kernel. Note,
PLIST can also be a full message property list or a property list
with a :data and :metadata key.

METADATA is the metadata plist used to render PLIST with, as
returned by the Jupyter kernel. METADATA typically contains
information such as the size of an image to be rendered.


Using the `jupyter-org-request-block-params' of REQ, loop over
the MIME types in `jupyter-org-mime-types' calling

    (jupyter-org-result MIME PARAMS DATA METADATA)

for each MIME type. Return the result of the first iteration in
which the above call returns a non-nil value. PARAMS is the REQ's
`jupyter-org-request-block-params', DATA and METADATA are the
data and metadata of the current MIME type.

If PARAMS has a space separated string associated with its
:display key like \"image/png html plain\", i.e. MIME subtypes or
full MIME types, then loop over the corresponding subset of MIME
types in `jupyter-org-mime-types' in the same order given. This
adds support for the :display header argument that can be
passed to Jupyter org-mode source blocks."
  (cl-assert plist json-plist)
  (let* ((params (jupyter-org-request-block-params req))
         (display-mime-types (jupyter-org--find-mime-types
                              (alist-get :display params))))
    (when (jupyter-org-request-file req)
      (push (cons :file (jupyter-org-request-file req)) params))
    (cl-destructuring-bind (data metadata)
        (jupyter-normalize-data plist metadata)
      (or (jupyter-loop-over-mime
              (or display-mime-types jupyter-org-mime-types)
              mime data metadata
            (jupyter-org-result mime params data metadata))
          (prog1 nil
            (warn "No valid mimetype found %s"
                  (cl-loop for (k _v) on data by #'cddr collect k)))))))

(cl-defmethod jupyter-org-result ((_mime (eql :application/vnd.jupyter.widget-view+json))
                                  _params _data &optional _metadata)
  ;; TODO: Clickable text to open up a browser
  (jupyter-org-scalar "Widget"))

(cl-defmethod jupyter-org-result ((_mime (eql :text/org)) _params data
                                  &optional _metadata)
  (jupyter-org-raw-string data))

(cl-defmethod jupyter-org-result ((mime (eql :image/png)) params data
                                  &optional metadata)
  (jupyter-org--image-result mime params 'b64-encoded data metadata))

(cl-defmethod jupyter-org-result ((mime (eql :image/jpeg)) params data
                                  &optional metadata)
  (jupyter-org--image-result mime params 'b64-encoded data metadata))

(cl-defmethod jupyter-org-result ((mime (eql :image/svg+xml)) params data
                                  &optional metadata)
  (jupyter-org--image-result mime params nil data metadata))

(cl-defmethod jupyter-org-result ((_mime (eql :text/markdown)) _params data
                                  &optional _metadata)
  (jupyter-org-export-block "markdown" data))

(defun jupyter-org--parse-latex-element (data)
  "Return a latex-fragment or latex-environment org-element obtained from DATA.
DATA is inserted into a temporary buffer and an org-element latex
fragment or environment is parsed and returned. If neither can be
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

(cl-defmethod jupyter-org-result ((_mime (eql :text/latex)) params data
                                  &optional _metadata)
  (if (member "raw" (alist-get :result-params params))
      (jupyter-org--parse-latex-element data)
    (jupyter-org-export-block "latex" data)))

(cl-defmethod jupyter-org-result ((_mime (eql :text/html)) _params data
                                  &optional _metadata)
  (jupyter-org-export-block "html" data))

;; NOTE: The order of :around methods is that the more specialized wraps the
;; more general, this makes sense since it is how the primary methods work as
;; well.
;;
;; Using an :around method to attempt to guarantee that this is called as the
;; outer most method. Kernel languages should extend the primary method.
(cl-defmethod jupyter-org-result :around ((_mime (eql :text/plain)) params _data
                                  &optional _metadata)
  "Do some final transformations of the result.
Call the next method, if it returns \"scalar\" results, return a
new \"scalar\" result with the result of calling
`org-babel-script-escape' on the old result."
  (let ((result (cl-call-next-method)))
    (jupyter-org-scalar
     (cond
      ((and (stringp result)
            ;; Don't attempt to create a table when we just want scalar results
            ;; FIXME: `jupyter-org-scalar' also considers a table a scalar, but
            ;; `org-mode' doesn't.
            (not (member "scalar" (alist-get :result-params params)))
            ;; Be a little more stringent than `org-babel-script-escape'. It
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

(cl-defmethod jupyter-org-result ((_mime (eql :text/plain)) _params data
                                  &optional _metadata)
  (ansi-color-apply data))

;;;; Helper functions

(defun jupyter-org--clear-request-id (req)
  "Delete the ID of REQ in the `org-mode' buffer if present."
  (org-with-point-at (jupyter-org-request-marker req)
    (when (search-forward (jupyter-request-id req) nil t)
      (delete-region (line-beginning-position)
                     (1+ (line-end-position)))
      (setf (jupyter-org-request-id-cleared-p req) t))))

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

(defun jupyter-org--delete-element (element)
  "Delete an `org' ELEMENT from the buffer.
Leave its affiliated keywords and preserve any blank lines that
appear after the element."
  (delete-region (jupyter-org-element-begin-after-affiliated element)
                 (jupyter-org-element-end-before-blanks element))
  ;; Delete a blank line after the element
  (when (eq (char-after) ?\n)
    (delete-char 1)))

(defun jupyter-org-strip-last-newline (string)
  "Return STRING with its last newline removed."
  (replace-regexp-in-string "\n\\'" "" string))

(defun jupyter-org--insert-element (element)
  "Insert ELEMENT."
  ;; `org-element-interpret-data' will add the newline back.
  (when (eq (char-after) ?\n)
    (delete-char 1))
  (insert (org-element-interpret-data element)))

(defun jupyter-org-babel-result-p (result)
  "Return non-nil if RESULT can be removed by `org-babel-remove-result'."
  (or (and (stringp result)
           ;; Org tables are returned as strings by this time. So we need
           ;; something to distinguish them from regular strings. See
           ;; `jupyter-org-scalar'.
           (get-text-property 0 'org-table result))
      (memq (org-element-type result)
            '(example-block
              export-block fixed-width item
              link plain-list src-block table))))

(defun jupyter-org--wrappable-element-p (element)
  "Return non-nil if ELEMENT can be removed and wrapped in a drawer."
  (or (jupyter-org-babel-result-p element)
      (let ((type (org-element-type element)))
        (or (memq type '(latex-fragment latex-environment))
            ;; TODO: Figure out a better way. I predict there will be more
            ;; situations where a comment would be useful to add. That means
            ;; we would have to verify each one.
            (and (eq type 'comment)
                 (equal jupyter-org--goto-error-string
                        (org-element-property :value element)))))))

;;;; Wrapping results

(defun jupyter-org--wrap-result-maybe (context result)
  "Depending on CONTEXT, wrap RESULT in a drawer."
  ;; If a keyword is the context, this is the first result.
  (if (eq (org-element-type context) 'keyword)
      ;; Only wrap the result if it can't be removed by `org-babel'.
      (if (jupyter-org-babel-result-p result) result
        (jupyter-org-results-drawer result))
    (if (jupyter-org--wrappable-element-p context)
        (prog1 (jupyter-org-results-drawer context result)
          ;; Ensure that a #+RESULTS: line is not prepended to context
          ;; when calling `org-element-interpret-data'.
          (org-element-put-property context :results nil)
          ;; Ensure there is no post-blank since
          ;; `org-element-interpret-data' already normalizes the string.
          (org-element-put-property context :post-blank nil))
      result)))

(defun jupyter-org--delete-unwrapped-result (element)
  "Delete ELEMENT from the buffer.
If ELEMENT represents a previous result it, along with the result
about to be inserted, will be wrapped in a drawer."
  (if (eq (org-element-type element) 'table)
      ;; The `org-element-contents' of a table is nil which interferes with how
      ;; `org-element-table-interpreter' works when calling
      ;; `org-element-interpret-data' so set the contents and delete ELEMENT from the
      ;; buffer.
      (org-element-set-contents
       element (delete-and-extract-region
                (org-element-property :contents-begin element)
                (jupyter-org-element-end-before-blanks element)))
    (when (jupyter-org--wrappable-element-p element)
      (jupyter-org--delete-element element))))

;;;; Stream results

;;;;; Helper functions

(defun jupyter-org--stream-result-p (result)
  (and (stringp result)
       (not (jupyter-org-raw-string-p result))))

(defun jupyter-org--mark-stream-result-newline (result)
  "Remember if RESULT ended in a newline.
Add a non-nil jupyter-stream-newline property to the most
recently inserted stream RESULT if it ends in a newline. This is
so that `jupyter-org--append-stream-result' can properly insert a
newline or not before inserting subsequent stream results.

Assumes `point' is at the end of the last source block result."
  (or (stringp result) (setq result (org-element-property :value result)))
  (when (and result
             (eq (aref result (1- (length result))) ?\n))
    (when (looking-back "#\\+END_EXAMPLE\n"
                        (line-beginning-position 0))
      (goto-char (match-beginning 0)))
    (put-text-property (1- (point)) (point) 'jupyter-stream-newline t)))

(defun jupyter-org--stream-context-p (context)
  "Determine if CONTEXT is a stream result.
Return the insertion point to append new stream output if CONTEXT
is a stream result. Otherwise return nil."
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
    (when (looking-at-p "\\(?::[\t ]\\|#\\+END_EXAMPLE\\)")
      (line-end-position (unless (eq (char-after) ?:) 0)))))

;;;;; Fixed width -> example block promotion

(defun jupyter-org--fixed-width-to-example-block (element result keep-newline)
  "Replace the fixed-width ELEMENT with an example-block.
Append RESULT to the contents of the block. If KEEP-NEWLINE is
non-nil, ensure that the appended RESULT begins on a newline."
  (jupyter-org--delete-element element)
  (jupyter-org--insert-element
   (jupyter-org-example-block
    (concat
     ;; TODO: optimize this
     (let ((old-result
            (org-element-normalize-string
             (org-element-property :value element))))
       (if keep-newline old-result
         (substring old-result 0 -1)))
     result))))

(defsubst jupyter-org--fixed-width-append (result keep-newline)
  (if (not (or keep-newline (string-match "\n" result)))
      (insert result)
    (if keep-newline (insert "\n")
      (insert (substring result 0 (match-end 0))))
    (jupyter-org--insert-element
     (jupyter-org-scalar
      (jupyter-org-strip-last-newline
       (if keep-newline result
         (substring result (match-end 0))))))))

(defun jupyter-org--append-to-fixed-width (result keep-newline)
  "Append RESULT to the fixed-width element at point.
`point' is assumed to be at the insertion point. If KEEP-NEWLINE is
non-nil, ensure that the appended RESULT begins on a newline.

If appending RESULT causes the total number of lines to exceed
`org-babel-min-lines-for-block-output' replace the fixed-width
element by an example-block containing both the original contents
of the fixed-width element and RESULT concatenated together.

If RESULT ends in a newline, place a non-nil
jupyter-stream-newline property on the ending newline after
insertion into the buffer."
  (let* ((context (org-element-at-point))
         (promote-to-block-p
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
      (jupyter-org--fixed-width-append result keep-newline))))

;;;;; Append stream result

(defun jupyter-org--append-stream-result (result)
  (let ((keep-newline (get-text-property (point) 'jupyter-stream-newline)))
    (if (eq (char-after (line-beginning-position)) ?:)
        (jupyter-org--append-to-fixed-width result keep-newline)
      ;; Append at the end of the current example-block. In this case
      ;; POS is the end of the last line of contents.
      ;;
      ;;     ...
      ;;     foo|
      ;;     #+END_EXAMPLE
      ;;
      ;; Delete the newline that will be re-inserted by the call to
      ;; `org-element-normalize-string'.
      (delete-char 1)
      (setq result (org-element-normalize-string result))
      ;; From `org-element-example-block-interpreter'
      (when (and (not org-src-preserve-indentation)
                 (/= 0 org-edit-src-content-indentation)
                 (version<= "9.2" (org-version)))
        (let ((ind (make-string org-edit-src-content-indentation ?\s)))
          (setq result (replace-regexp-in-string
                        "^[ \t]*\\S-"
                        (concat ind "\\&")
                        (org-remove-indentation result)))))
      (insert (concat (when keep-newline "\n") result)))))

;;;; Insert result

(defun jupyter-org--insert-result (context result)
  (insert (org-element-interpret-data
           (jupyter-org--wrap-result-maybe
            context (if (jupyter-org--stream-result-p result)
                        (jupyter-org-scalar
                         (jupyter-org-strip-last-newline result))
                      result))))
  (when (/= (point) (line-beginning-position))
    ;; Org objects such as file links do not have a newline added when
    ;; converting to their string representation by
    ;; `org-element-interpret-data' so insert one in these cases.
    (insert "\n")))

;;;; Add/append results

(defun jupyter-org--append-result (req result)
  (org-with-point-at (jupyter-org-request-marker req)
    (let ((result-beg (org-babel-where-is-src-block-result 'insert))
          (inhibit-redisplay (not debug-on-error)))
      (goto-char result-beg)
      ;; Move past the #+RESULTS: keyword. This is needed so that we can pick
      ;; up object contexts like file links as opposed to paragraph contexts.
      (forward-line)
      (let* ((context (org-element-context))
             (stream-append-pos
              (and (jupyter-org--stream-result-p result)
                   (jupyter-org--stream-context-p context))))
        (cond
         (stream-append-pos
          (goto-char stream-append-pos)
          (jupyter-org--append-stream-result result))
         (t
          (cl-case (org-element-type context)
            ;; Go to the end of the drawer to insert the new result.
            (drawer
             (goto-char (jupyter-org-element-contents-end context)))
            ;; Insert the first result. In this case `point' is at the first
            ;; line after the #+RESULTS keyword.
            (keyword nil)
            ;; Any other context is a previous result. Remove it from the
            ;; buffer since it, along with the new result will be wrapped in a
            ;; drawer and re-inserted into the buffer.
            (t
             (jupyter-org--delete-unwrapped-result context)))
          (jupyter-org--insert-result context result)
          (when jupyter-org-toggle-latex
            (when (memq (org-element-type result)
                        '(latex-fragment latex-environment))
              (save-excursion
                ;; Go to a position contained in the fragment
                (forward-line -1))
              (let ((ov (car (overlays-at (point)))))
                (unless (and ov (eq (overlay-get ov 'org-overlay-type)
                                    'org-latex-overlay))
                  (org-toggle-latex-fragment)))))))
        (when (jupyter-org--stream-result-p result)
          (jupyter-org--mark-stream-result-newline result))))))

(defun jupyter-org--add-result (req result)
  "For REQ, add RESULT.
For a synchronous request, RESULT is added to REQ's results slot
and all results are processed once the src-block has finished
running.

For an asynchronous request, RESULT is appended directly to the
buffer."
  (if (jupyter-org-request-silent-p req)
      (unless (equal (jupyter-org-request-silent-p req) "none")
        (message "%s" (org-element-interpret-data result)))
    (cond
     ((jupyter-org-request-async req)
      (when (and (jupyter-org-request-async req)
                 (not (jupyter-org-request-id-cleared-p req)))
        (jupyter-org--clear-request-id req))
      (jupyter-org--append-result req result))
     (t
      (push result (jupyter-org-request-results req))))))

;;; org-babel functions
;; These are meant to be called by `org-babel-execute:jupyter'
;; FIXME: This is ugly!

(defun jupyter-org--restore-message-function ()
  (advice-remove 'message #'ignore)
  (remove-hook 'pre-command-hook #'jupyter-org--restore-message-function))

(defun jupyter-org--ignore-message-until-command ()
  (advice-add 'message :override #'ignore)
  (add-hook 'pre-command-hook #'jupyter-org--restore-message-function))

(defun jupyter-org-insert-async-id (req)
  "Insert the ID of REQ.
Meant to be used as the return value of
`org-babel-execute:jupyter'."
  (prog1 nil
    (jupyter-org--ignore-message-until-command)
    (setq org-babel-after-execute-hook
          (let ((hook org-babel-after-execute-hook))
            (lambda ()
              (jupyter-org--restore-message-function)
              (unwind-protect
                  (jupyter-org--add-result
                   req (jupyter-org-scalar (jupyter-org-request-id req)))
                (setq org-babel-after-execute-hook hook)
                (run-hooks 'org-babel-after-execute-hook)))))))

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
                        (jupyter-org-strip-last-newline (car a)))))
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
