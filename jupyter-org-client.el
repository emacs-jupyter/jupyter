;;; jupyter-org-client.el --- Org integration -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 02 Jun 2018
;; Version: 0.6.0

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

;;

;;; Code:

(require 'jupyter-repl)
(require 'ob)
(require 'org-element)

(declare-function org-at-drawer-p "org")
(declare-function org-in-regexp "org" (regexp &optional nlines visually))
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
  results
  silent
  id-cleared-p
  marker
  async)

;;; `jupyter-kernel-client' interface

;;;; `jupyter-request' interface

(cl-defmethod jupyter-generate-request ((client jupyter-org-client) _msg
                                        &context (major-mode (eql org-mode)))
  "Return a `jupyter-org-request' for the current source code block."
  (let* ((block-params (oref client block-params))
         (result-params (alist-get :result-params block-params)))
    (jupyter-org-request
     :marker (copy-marker org-babel-current-src-block-location)
     :result-type (alist-get :result-type block-params)
     :block-params block-params
     :async (equal (alist-get :async block-params) "yes")
     :silent (car (or (member "none" result-params)
                      (member "silent" result-params))))))

(cl-defmethod jupyter-drop-request ((_client jupyter-org-client)
                                    (req jupyter-org-request))
  (set-marker (jupyter-org-request-marker req) nil))

;;;; Stream

(cl-defmethod jupyter-handle-stream ((_client jupyter-org-client)
                                     (req jupyter-org-request)
                                     _name
                                     text)
  ;; TODO: This is not good because there can be arbitrary output which may get
  ;; recognized as org syntax. We need to wrap this into a an example block or
  ;; scalar block. Probably an example block and then coalesce any output into
  ;; a single example block when new output comes in and the last results in a
  ;; drawer is an example block.
  (jupyter-org--add-result
   req (with-temp-buffer
         (jupyter-with-control-code-handling
          (insert (ansi-color-apply text)))
         (buffer-string))))

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
  (let ((loc (get-text-property (point) 'jupyter-error-loc)))
    ;; TODO: Ensure we are at the right source block by storing the hash of the
    ;; code as a text property using `org-babel-sha1-hash'. Then don't jump to
    ;; the error if the hash doesn't match. Better yet, we could hash the line
    ;; corresponding to the error and then scan the lines of the source block
    ;; to find the right one if the original location doesn't match.
    (when loc
      (org-babel-previous-src-block)
      (forward-line loc))))

;;;;; `jupyter-org-error-location'
;; Inspiration from https://kitchingroup.cheme.cmu.edu/blog/2017/06/10/Adding-keymaps-to-src-blocks-via-org-font-lock-hook/
;; TODO: Do something similar for `jupyter-inspect-at-point' using `jupyter-org-with-src-block-client'.

(cl-defgeneric jupyter-org-error-location ()
  "Return the line number corresponding to an error from a traceback.
This method is called with `point' at `point-min' in a buffer
containing the traceback of the last error that occurred due to
execution of a source block. It should return the line number
relative to the source block that caused the error or nil if a
line number could not be found."
  (ignore))

(defun jupyter-org-goto-error-string (traceback)
  "Return a propertized string with a jupyter-error-loc property.
The property is obtained by calling `jupyter-org-error-location'
in a buffer whose contents contain TRACEBACK."
  (let ((loc
         (with-temp-buffer
           (insert traceback)
           (goto-char (point-min))
           (jupyter-org-error-location))))
    (when loc
      (unless (memq 'jupyter-org-add-error-keymap org-font-lock-hook)
        (add-hook 'org-font-lock-hook 'jupyter-org-add-error-keymap nil t))
      (propertize "[goto error]" 'jupyter-error-loc loc))))

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
  ;; (jupyter-with-display-buffer "traceback" 'reset
  ;;   (jupyter-insert-ansi-coded-text
  ;;    (mapconcat #'identity traceback "\n"))
  ;;   (goto-char (line-beginning-position))
  ;;   (pop-to-buffer (current-buffer)))
  (setq traceback (org-element-normalize-string
                   (ansi-color-apply (mapconcat #'identity traceback "\n"))))
  (let ((goto-error (jupyter-org-goto-error-string traceback)))
    (when goto-error
      (jupyter-org--add-result req (jupyter-org-comment goto-error))))
  ;; (jupyter-org-scalar (format "%s: %s" ename (ansi-color-apply evalue)))
  (jupyter-org--add-result req traceback))

;;;; Execute result

(cl-defmethod jupyter-handle-execute-result ((_client jupyter-org-client)
                                             (req jupyter-org-request)
                                             _execution-count
                                             data
                                             metadata)
  (unless (eq (jupyter-org-request-result-type req) 'output)
    (jupyter-org--add-result
     req (jupyter-org-result req data metadata))))

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
  (jupyter-org--add-result req (jupyter-org-result req data metadata)))

;;;; Execute reply

(cl-defmethod jupyter-handle-execute-reply ((client jupyter-org-client)
                                            (req jupyter-org-request)
                                            status
                                            execution-count
                                            _user-expressions
                                            payload)
  ;; TODO: Re-use the REPL's handler somehow?
  (oset client execution-count (1+ execution-count))
  (when payload
    (jupyter-repl--handle-payload payload))
  (if (equal status "ok")
      (message "Code block evaluation complete.")
    (message "An error occurred when evaluating code block."))
  (when (jupyter-org-request-async req)
    (jupyter-org--clear-request-id req)
    (run-hooks 'org-babel-after-execute-hook)))

;;; Completions in code blocks

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
      (when (string-prefix-p "jupy-" lang)
        (let* ((info (org-babel-get-src-block-info el))
               (params (nth 2 info))
               (beg (save-excursion
                      (goto-char
                       (org-element-property :begin el))
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
       (prog1 nil
         (when jupyter-org--src-block-cache
           (set-marker (nth 1 jupyter-org--src-block-cache) nil)
           (set-marker (nth 2 jupyter-org--src-block-cache) nil)
           (setq jupyter-org--src-block-cache nil)))
     (jupyter-org--set-current-src-block)
     (when (jupyter-org--same-src-block-p)
       ,@body)))

(defmacro jupyter-org-with-src-block-client (&rest body)
  "Evaluate BODY with `jupyter-current-client' set to the session's client.
If `point' is not at a Jupyter source block, BODY is not
evaluated and nil is returned. Return the result of BODY when it
is evaluated.

In addition to evaluating BODY with an active Jupyter client set,
the `syntax-table' will be set to that of the REPL buffers."
  (declare (debug (body)))
  `(jupyter-org-when-in-src-block
    (let* ((params (car jupyter-org--src-block-cache))
           (buffer (org-babel-jupyter-initiate-session
                    (alist-get :session params) params))
           (syntax (with-current-buffer buffer (syntax-table)))
           (jupyter-current-client
            (with-current-buffer buffer jupyter-current-client)))
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

(defun jupyter-org-enable-completion ()
  "Enable autocompletion in Jupyter source code blocks."
  (add-hook 'completion-at-point-functions 'jupyter-org-completion-at-point nil t))

(add-hook 'org-mode-hook 'jupyter-org-enable-completion)

;;; Constructing org syntax trees

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

(defun jupyter-org-src-block (language parameters value &optional switches)
  "Return a src-block `org-element'.
LANGUAGE, PARAMETERS, VALUE, and SWITCHES all have the same
meaning as a src-block `org-element'."
  (declare (indent 2))
  (list 'src-block (list :language language
                         :parameters parameters
                         :switched switches
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
RESULTS can be either strings or other `org-element's. The
returned drawer has a name of \"RESULTS\"."
  ;; Ensure the last element has a newline if it is already a string. This is
  ;; to avoid situations like
  ;;
  ;; :RESULTS:
  ;; foo:END:
  (let ((last (last results)))
    (when (and (stringp (car last))
               (not (zerop (length (car last)))))
      (setcar last (org-element-normalize-string (car last)))))
  (apply #'org-element-set-contents
         (list 'drawer (list :drawer-name "RESULTS"))
         results))

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

(defun jupyter-org--image-result (mime params data)
  "Return a cons cell (\"file\" . FILENAME).
MIME is the image mimetype, PARAMS is the
`jupyter-org-request-block-params' that caused this result to be
returned and DATA is the image DATA.

If PARAMS contains a :file key, it is used as the FILENAME.
Otherwise a file name is created, see
`jupyter-org-image-file-name'. Note, when PARAMS contains a :file
key any existing file with the file name will be overwritten when
writing the image data. This is not the case when a new file name
is created."
  (let* ((overwrite (not (null (alist-get :file params))))
         (base64-encoded (memq mime '(:image/png :image/jpeg)))
         (file (or (alist-get :file params)
                   (jupyter-org-image-file-name
                    data (cl-case mime
                           (:image/png "png")
                           (:image/jpeg "jpg")
                           (:image/svg+xml "svg"))))))
    (when (or overwrite (not (file-exists-p file)))
      (let ((buffer-file-coding-system
             (if base64-encoded 'binary
               buffer-file-coding-system))
            (require-final-newline nil))
        (with-temp-file file
          (insert data)
          (when base64-encoded
            (base64-decode-region (point-min) (point-max))))))
    (jupyter-org-file-link file)))

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

(cl-defmethod jupyter-org-result ((req jupyter-org-request) plist &optional metadata)
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
data and metadata of the current MIME type."
  (cl-assert plist json-plist)
  (let ((params (jupyter-org-request-block-params req)))
    (cl-destructuring-bind (data metadata)
        (jupyter-normalize-data plist metadata)
      (or (jupyter-loop-over-mime
              jupyter-org-mime-types mime data metadata
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
                                  &optional _metadata)
  ;; TODO: Add ATTR_ORG with the width and height. This can be done for example
  ;; by adding a function to `org-babel-after-execute-hook'.
  (jupyter-org--image-result mime params data))

(cl-defmethod jupyter-org-result ((mime (eql :image/jpeg)) params data
                                  &optional _metadata)
  (jupyter-org--image-result mime params data))

(cl-defmethod jupyter-org-result ((mime (eql :image/svg+xml)) params data
                                  &optional _metadata)
  (jupyter-org--image-result mime params data))

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
(cl-defmethod jupyter-org-result :around ((_mime (eql :text/plain)) &rest _)
  "Do some final transformations of the result.
Call the next method, if it returns \"scalar\" results, return a
new \"scalar\" result with the result of calling
`org-babel-script-escape' on the old result."
  (let ((result (cl-call-next-method)))
    (jupyter-org-scalar
     (cond
      ((stringp result) (org-babel-script-escape result))
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

(defun jupyter-org-element-end-before-blanks (el)
  "Return the end position of EL, before any :post-blank lines."
  (- (org-element-property :end el)
     (or (org-element-property :post-blank el) 0)))

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

(defun jupyter-org-newline-ending-p (string)
  "Return non-nil if STRING ends in a newline character."
  (eq (aref string (1- (length string))) ?\n))

(defun jupyter-org--stream-result-p (result)
  (and (stringp result)
       (not (jupyter-org-raw-string-p result))))

(defun jupyter-org--stream-context-p (context)
  "Determine if CONTEXT is a stream result.
Return the insertion point to append new stream output if CONTEXT
is a stream result. Otherwise return nil."
  (save-excursion
    (goto-char (if (eq (org-element-type context) 'drawer)
                   (org-element-property :contents-end context)
                 (jupyter-org-element-end-before-blanks context)))
    (beginning-of-line 0)
    (when (looking-at-p "\\(?::[\t ]\\|#\\+END_EXAMPLE\\)")
      (line-end-position
       (unless (eq (char-after) ?:) 0)))))

;;;; Appending stream results

(defun jupyter-org--delete-element (element)
  "Delete an `org' ELEMENT from the buffer.
Leave its affiliated keywords and preserve and blank lines that
appear after the element."
  (delete-region (or (org-element-property :post-affiliated element)
                     (org-element-property :begin element))
                 (jupyter-org-element-end-before-blanks element)))

(defun jupyter-org--fixed-width-to-example-block (element result keep-newline)
  "Replace the fixed-width ELEMENT with an example-block.
Append RESULT to the contents of the block. If KEEP-NEWLINE is
non-nil, ensure that the appended RESULT begins on a newline."
  (jupyter-org--delete-element element)
  (insert (org-element-interpret-data
           (jupyter-org-example-block
            (concat
             (if keep-newline
                 (org-element-property :value element)
               (substring (org-element-property :value element) 0 -1))
             result)))))

(defsubst jupyter-org--fixed-width-inline-p (result)
  (not (string-match "\n" result)))

(defsubst jupyter-org--fixed-width-append (result keep-newline)
  ;; Delete the following newline that is re-inserted due to
  ;; `org-element-interpret-data'
  (delete-char 1)
  (if keep-newline
      (insert "\n" (org-element-interpret-data
                    (jupyter-org-scalar result)))
    (insert (substring result 0 (match-end 0))
            (org-element-interpret-data
             (jupyter-org-scalar
              (substring result (match-end 0)))))))

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
          (>= (+ (count-lines (org-element-property :begin context)
                              (org-element-property :end context))
                 (cl-loop
                  with i = 0
                  for c across result when (eq c ?\n) do (cl-incf i)
                  finally return i))
              org-babel-min-lines-for-block-output)))
    (if promote-to-block-p
        (jupyter-org--fixed-width-to-example-block context result keep-newline)
      (if (and (jupyter-org--fixed-width-inline-p result)
               (not keep-newline))
          (insert result)
        (jupyter-org--fixed-width-append result keep-newline)))))

(defun jupyter-org--append-stream-result (context result)
  "Append a stream result to the current stream results.
Return non-nil if RESULT was appended.

RESULT is appended if the most recently inserted result is a
stream result, as determined by CONTEXT."
  (let ((pos (jupyter-org--stream-context-p context)))
    (when pos
      (prog1 t
        (goto-char pos)
        (let ((keep-newline (get-text-property
                             (point) 'jupyter-stream-newline)))
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
            (insert (concat (when keep-newline "\n")
                            (org-element-normalize-string result)))))))))

(defun jupyter-org--mark-stream-result-newline (result)
  "Remember if RESULT ended in a newline.
Add a non-nil jupyter-stream-newline property to the most
recently inserted stream RESULT if it ends in a newline. This is
so that `jupyter-org--append-stream-result' can properly insert a
newline or not before inserting subsequent stream results.

Assumes `point' is at the end of the last source block result."
  (or (stringp result) (setq result (org-element-property :value result)))
  (when (and result (jupyter-org-newline-ending-p result))
    (when (looking-back "#\\+END_EXAMPLE\n"
                        (line-beginning-position 0))
      (goto-char (match-beginning 0)))
    (put-text-property (1- (point)) (point) 'jupyter-stream-newline t)))

;;;; Wrapping results in a drawer

(defun jupyter-org--wrap-result-maybe (context result)
  "Depending on CONTEXT, wrap RESULT in a drawer."
  (if (eq (org-element-type context) 'drawer)
      ;; No need to wrap a result if a drawer already exists.
      result
    (cond
     ;; If a keyword is the context, this is the first result.
     ((eq (org-element-type context) 'keyword)
      ;; Only wrap the result if it can't be removed by `org-babel'.
      (if (jupyter-org-babel-result-p result) result
        (jupyter-org-results-drawer result)))
     ;; A result already exists and is not already wrapped in a drawer, wrap
     ;; the result already present along with the new result.
     (t
      (prog1 (jupyter-org-results-drawer context result)
        ;; Ensure that a #+RESULTS: line is not prepended to context
        ;; when calling `org-element-interpret-data'.
        (org-element-put-property context :results nil)
        ;; Ensure there is no post-blank since
        ;; `org-element-interpret-data' already normalizes the string.
        (org-element-put-property context :post-blank nil))))))

(defun jupyter-org--display-latex (limit)
  "Show inline latex fragments between LIMIT and `point'."
  (save-excursion
    (while (< limit (point))
      (let ((delim-pos (org-inside-LaTeX-fragment-p)))
        (when delim-pos
          (cl-destructuring-bind (_delim . pos)
              delim-pos
            (goto-char pos)
            (let ((ov (car (overlays-at pos))))
              (unless (and ov (eq (overlay-get ov 'org-overlay-type)
                                  'org-latex-overlay))
                (org-toggle-latex-fragment)))))
        (backward-word)))))

;;;; Actually appending the results

(defun jupyter-org--goto-result-insertion-point (context)
  "Go the insertion point for new results depending on CONTEXT.
If CONTEXT is a drawer, go to the end the last element contained
in the drawer. Otherwise, assuming `point' is on the line
containing the #+RESULTS affiliated keyword, go forward a line."
  (cond
   ((eq (org-element-type context) 'drawer)
    ;; Go to |:END:
    (if (org-element-property :contents-end context)
        (goto-char (org-element-property :contents-end context))
      (goto-char (jupyter-org-element-end-before-blanks context))
      (beginning-of-line 0)))
   (t
    ;; Insert the results after the #+RESULTS: line
    (forward-line))))

(defun jupyter-org--append-result (req result)
  (org-with-point-at (jupyter-org-request-marker req)
    (let ((result-beg (org-babel-where-is-src-block-result 'insert))
          (inhibit-redisplay (not debug-on-error)))
      (goto-char result-beg)
      (let* ((context (org-element-context))
             (stream-result-p (jupyter-org--stream-result-p result)))
        (cond
         ;; Append stream results to the current stream results
         ((and stream-result-p
               (jupyter-org--append-stream-result context result)))
         ;; Append everything else
         (t
          (jupyter-org--goto-result-insertion-point context)
          ;; Don't delete the current result if its a drawer or if there aren't
          ;; any results yet.
          (unless (memq (org-element-type context) '(keyword drawer))
            (jupyter-org--delete-element context))
          (let ((limit (point)))
            (insert (org-element-interpret-data
                     (jupyter-org--wrap-result-maybe
                      context (if stream-result-p
                                  (jupyter-org-scalar result)
                                result))))
            ;; Objects do not have a newline appended so insert one when
            ;; the result is an object.
            (when (memq (org-element-type result) org-element-all-objects)
              (insert "\n"))
            (when jupyter-org-toggle-latex
              (jupyter-org--display-latex limit)))))
        (when stream-result-p
          (jupyter-org--mark-stream-result-newline result))))))

(defun jupyter-org--add-result (req result)
  "For REQ, add RESULT.
REQ is a `jupyter-org-request' and if the request is a
synchronous request, RESULT will be pushed to the list of results
in the request's results slot or appended to the buffer if REQ is
already complete. Otherwise, when the request is asynchronous,
RESULT is inserted at the location of the code block for the
request."
  (if (jupyter-org-request-silent req)
      (unless (equal (jupyter-org-request-silent req) "none")
        (message "%s" (org-element-interpret-data result)))
    (if (not (jupyter-org-request-async req))
        (push result (jupyter-org-request-results req))
      (when (and (jupyter-org-request-async req)
                 (not (jupyter-org-request-id-cleared-p req)))
        (jupyter-org--clear-request-id req))
      (jupyter-org--append-result req result))))

(defun jupyter-org--coalesce-stream-results (results)
  "Return RESULTS with all contiguous stream results concatenated.
All stream results are then turned into fixed-width or
example-block elements."
  (let ((head (nreverse
               (cl-reduce
                (lambda (a b)
                  (if (and (jupyter-org--stream-result-p b)
                           (jupyter-org--stream-result-p (car a)))
                      (setcar a (concat (car a) b))
                    (push b a))
                  a)
                results
                :initial-value nil))))
    (prog1 head
      (setq results head)
      (while results
        (when (jupyter-org--stream-result-p (car results))
          (setcar results (jupyter-org-scalar (car results))))
        (pop results)))))

;;; org-babel functions
;; These are meant to be called by `org-babel-execute:jupyter'

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

(defun jupyter-org-sync-results (req)
  "Return the result string in org syntax for the results of REQ.
Meant to be used as the return value of
`org-babel-execute:jupyter'."
  (let ((results (jupyter-org--coalesce-stream-results
                  (nreverse (jupyter-org-request-results req)))))
    (when results
      (let* ((params (jupyter-org-request-block-params req))
             (result-params (alist-get :result-params params)))
        (org-element-interpret-data
         (if (or (and (= (length results) 1)
                      (jupyter-org-babel-result-p (car results)))
                 (member "raw" result-params))
             (car results)
           (apply #'jupyter-org-results-drawer results)))))))

(provide 'jupyter-org-client)

;;; jupyter-org-client.el ends here
