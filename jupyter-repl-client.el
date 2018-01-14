;;; jupyter-repl-client.el --- A Jupyter REPL client -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 08 Jan 2018
;; Version: 0.0.1
;; Keywords:
;; X-URL: https://github.com/nathan/jupyter-repl-client

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

(defgroup jupyter-repl-client nil
  "A Jupyter REPL client"
  :group 'communication)

(require 'jupyter-base)
(require 'jupyter-client)
(require 'jupyter-kernel-manager)
(require 'xterm-color)
(require 'shr)
(require 'ring)

;; TODO: Read up on how method tags can be used, see
;; https://ericabrahamsen.net/tech/2016/feb/bbdb-eieio-object-oriented-elisp.html

;; TODO: Still need to figure out how to remove requests from the client's
;; request table.

;; TODO: Fallbacks for when the language doesn't have a major mode installed.

;; TODO: Add a text property to all output depending on which handler it came
;; from. Then it would make it easier to traverse the buffer and collect
;; information to save the REPL to a notebook format. But what about the text
;; properties from `xterm-color-filter'? Maybe I can use overlays, this way the
;; text is preserved but color is still there.

(defface jupyter-repl-input-prompt
  '((((class color) (min-colors 88) (background light))
     :foreground "darkseagreen2")
    (((class color) (min-colors 88) (background dark))
     :foreground "darkolivegreen"))
  "Face used for the input prompt."
  :group 'jupyter-repl)

(defface jupyter-repl-output-prompt
  '((((class color) (min-colors 88) (background light))
     :foreground "indianred3")
    (((class color) (min-colors 88) (background dark))
     :foreground "darkred"))
  "Face used for the input prompt."
  :group 'jupyter-repl)

(defcustom jupyter-repl-maximum-size 1024
  "Maximum number of lines before the buffer is truncated."
  :group 'jupyter-repl)

(defcustom jupyter-repl-history-maximum-length 100
  "The maximum number of history elements to keep track of."
  :group 'jupyter-repl)

(defcustom jupyter-repl-prompt-margin-width 12
  "The width of the margin which displays prompt strings."
  :group 'jupyter-repl)

(defclass jupyter-repl-client (jupyter-kernel-client)
  ((kernel-info)
   (buffer :initarg :buffer)
   (execution-count :initform 1)))

(defvar jupyter-repl-lang-buffer nil
  "A buffer with the `major-mode' set to the REPL language's `major-mode'.")

(defvar jupyter-repl-current-client nil
  "The `jupyter-repl-client' for the `current-buffer'.")

(defvar jupyter-repl-kernel-manager nil
  "If the REPL is connected to a kernel which was started as a
 subprocess. This will contain the kernel manager used to control
 the lifetime of the kernel.")

(defvar jupyter-repl-lang-mode nil
  "The major mode corresponding to the kernel's language.")

(defvar jupyter-repl-history nil
  "The history of the current Jupyter REPL.")

(defvar jupyter-repl-fontify-buffers nil
  "Buffers used for fontification.")

(defvar jupyter-repl-use-builtin-is-complete nil
  "Whether or not to send an is_complete_request to a kernel.
If a Jupyter kernel does not respond to an is_complete_request,
this variable is automatically set to t and code in a cell is
considered complete if the last line in a code cell is a blank
line, i.e. if RET is pressed twice in a row.")

;;; Convenience macros

(defmacro with-jupyter-repl-buffer (client &rest body)
  "Switch to CLIENT's buffer before running BODY.
This switches to CLIENT's buffer slot, sets `inhibit-read-only'
to t, and then runs BODY. Afterwards, if CLIENT's buffer is
currently being shown in a window, move windows `point' to the
value of `point' in the buffer."
  (declare (indent 1) (debug (symbolp &rest form)))
  `(with-current-buffer (oref ,client buffer)
     (let ((inhibit-read-only t))
       (prog1 (progn ,@body)
         (let ((win (get-buffer-window)))
           (when win (set-window-point win (point))))))))

(defmacro jupyter-repl-do-at-request (client req &rest body)
  "Switch to CLIENT's buffer, move to then end of REQ, and run BODY.
Switching to CLIENT's buffer is accomplished using
`with-jupyter-repl-buffer'. After switching, `point' is moved to
the `jupyter-repl-cell-beginning-position' of the cell after the
one associated with REQ, where REQ is a `jupyter-request'
previously made using CLIENT. This position is where any output
of REQ should be inserted.

Note that `inhibit-modification-hooks' is set to t when BODY is
run, this prevents any line continuation prompts to be inserted
for multi-line output."
  (declare (indent 2) (debug (symbolp &rest form)))
  `(with-jupyter-repl-buffer ,client
     (save-excursion
       (let ((inhibit-modification-hooks t))
         (jupyter-repl-goto-cell ,req)
         (jupyter-repl-next-cell)
         ,@body))))

(defmacro with-jupyter-repl-lang-buffer (&rest body)
  "Run BODY in the `jupyter-repl-lang-buffer' of the `current-buffer'.
The contents of `jupyter-repl-lang-buffer' is erased before
running BODY."
  (declare (indent 0) (debug (&rest form)))
  `(with-current-buffer jupyter-repl-lang-buffer
     (let ((inhibit-read-only t))
       (erase-buffer)
       ,@body)))

(defmacro with-jupyter-repl-cell (&rest body)
  "Narrow to the current cell, run BODY, then widen.
The cell is narrowed to the region between and including
`jupyter-repl-cell-beginning-position' and
`jupyter-repl-cell-end-position'. Note that this assumes that the
`current-buffer' is a Jupyter REPL buffer."
  (declare (indent 0) (debug (&rest form)))
  `(save-excursion
     (save-restriction
       (narrow-to-region (jupyter-repl-cell-beginning-position)
                         (jupyter-repl-cell-end-position))
       ,@body)))

;;; Inserting text into the REPL buffer

(defun jupyter-repl-add-font-lock-properties (start end &optional object)
  "Add font lock text properties between START and END in the `current-buffer'.
START, END, and OBJECT have the same meaning as in
`add-text-properties'. Add the font lock properties needed for
text inserted into a `jupyter-repl-client's buffer."
  (add-text-properties
   start end '(fontified t font-lock-fontified t font-lock-multiline t) object)
  (font-lock-fillin-text-property
   start end 'font-lock-face 'default object))

(defun jupyter-repl-get-fontify-buffer (mode)
  (let ((buf (alist-get mode jupyter-repl-fontify-buffers)))
    (unless buf
      (setq buf (get-buffer-create
                 (format " *jupyter-repl-fontify[%s]*" mode)))
      (with-current-buffer buf
        (funcall mode))
      (setf (alist-get mode jupyter-repl-fontify-buffers) buf))
    buf))

(defun jupyter-repl-fontify-according-to-mode (mode str)
  "Fontify a string according to MODE.
MODE should be a function which sets the `major-mode'. MODE will
be called with the `current-buffer' set to a temporary buffer,
STR will then be inserted and fontified. Return the fontified
string, which can then be inserted into a Jupyter REPL buffer."
  ;; Adapted from `org-src-font-lock-fontify-block'
  (with-current-buffer (jupyter-repl-get-fontify-buffer mode)
    (let ((inhibit-modification-hooks nil))
      (erase-buffer)
      (insert str)
      (font-lock-ensure)
      (let ((pos (point-min)) next)
        (while (setq next (next-property-change pos))
          ;; Handle additional properties from font-lock, so as to
          ;; preserve, e.g., composition.
          (dolist (prop (cons 'face font-lock-extra-managed-props))
            (let ((new-prop (get-text-property pos prop)))
              (put-text-property
               (+ 1 (1- pos)) (1- (+ 1 next))
               (if (eq prop 'face) 'font-lock-face
                 prop)
               (if (eq prop 'face) (or new-prop 'default)
                 new-prop))))
          (setq pos next))))
    (jupyter-repl-add-font-lock-properties (point-min) (point-max))
    (cond
     ((and fill-paragraph-function
           (not (eq fill-paragraph-function t)))
      (goto-char (point-min))
      (fill-paragraph)
      (while (and (= (fill-forward-paragraph 1) 0)
                  (/= (point) (point-max)))
        (fill-paragraph)))
     ((not (memq fill-forward-paragraph-function
                 '(forward-paragraph)))
      (fill-region (point-min) (point-max))))
    (buffer-string)))

(defun jupyter-repl-insert (&rest args)
  "Insert text into the `current-buffer', possibly with text properties.

This acts like `insert' except that the leading elements of ARGS
can contain the following keywords along with their values:

- `:read-only' :: A non-nil value makes the text to be inserted,
  read only. This is t by default, so to make text editable you
  will have to do something like:
    (jupyter-repl-insert :read-only nil \"<editable text>\")

- `:properties' :: A list of text properties and their values to
  be added to the inserted text. This defaults to an empty list.

- `:inherit-properties' :: A non-nil value will use
  `insert-and-inherit' instead of `insert' for the function used
  to insert the text. This is nil by default."
  (let ((arg nil)
        (read-only t)
        (properties nil)
        (insert-fun #'insert))
    (while (keywordp (setq arg (car args)))
      (cl-case arg
        (:read-only (setq read-only (cadr args)))
        (:properties (setq properties (cadr args)))
        (:inherit-properties
         (setq insert-fun (if (cadr args) #'insert-and-inherit #'insert)))
        (otherwise
         (error "Keyword not one of `:read-only', `:properties', `:inherit-properties' (`%s')" arg)))
      (setq args (cddr args)))
    (setq properties (append (when read-only '(read-only t))
                             properties))
    (apply insert-fun (mapcar (lambda (s)
                           (prog1 s
                             (when properties
                               (add-text-properties
                                0 (length s) properties s))))
                         args))))

(defun jupyter-repl-newline ()
  "Insert a read-only newline into the `current-buffer'."
  (jupyter-repl-insert "\n"))

(defun jupyter-repl-insert-html (html)
  "Parse and insert the HTML string using `shr-insert-document'."
  (jupyter-repl-insert
   ;; `save-excursion' is necessary here since it seems that `with-temp-buffer'
   ;; moves the REPL window's `point' when it is visible
   (save-excursion
     (with-temp-buffer
       (insert html)
       (let ((xml (libxml-parse-html-region
                   (point-min) (point-max))))
         (erase-buffer)
         (goto-char (point-min))
         (shr-insert-document xml))
       (string-trim (buffer-string))))))

(defun jupyter-repl-insert-markdown (text)
  (jupyter-repl-insert
   (let ((md (jupyter-repl-fontify-according-to-mode 'markdown-mode text)))
     (if (display-graphic-p)
         (with-current-buffer (jupyter-repl-get-fontify-buffer 'markdown-mode)
           (markdown-display-inline-images)
           (buffer-string))
       md))))

(defun jupyter-repl-insert-latex (tex)
  "Generate and insert a LaTeX image based on TEX.

Note that this uses `org-format-latex' to generate the LaTeX
image."
  (require 'org)
  (let (beg end)
    (setq beg (point))
    (jupyter-repl-insert tex)
    (setq end (point))
    (org-format-latex
     "jupyter-repl" beg end "jupyter-repl"
     'overlays "Creating LaTeX image...%s"
     'forbuffer
     ;; Use the default method for creating image files
     org-preview-latex-default-process)))

(defun jupyter-repl-insert-data (data)
  (let ((mimetypes (seq-filter #'keywordp data)))
    (cond
     ((memq :image/png mimetypes)
      (insert-image
       (create-image
        (base64-decode-string
         (plist-get data :image/png))
        nil 'data)
       (propertize " " 'read-only t)))
     ((and (memq :image/svg+xml mimetypes) (image-type-available-p 'svg))
      (insert-image
       (create-image
        (plist-get data :image/svg+xml) 'svg)
       (propertize " " 'read-only t)))
     ((memq :text/html mimetypes)
      (let ((html (plist-get data :text/html)))
        (when (string-match-p "^<img" html)
          (jupyter-repl-newline))
        (jupyter-repl-insert-html html)
        (jupyter-repl-newline)))
     ((memq :text/latex mimetypes)
      (jupyter-repl-insert-latex (plist-get data :text/latex)))
     ((and (memq :text/markdown mimetypes) (require 'markdown-mode nil t))
      (jupyter-repl-insert-markdown (plist-get data :text/markdown)))
     ((memq :text/plain mimetypes)
      (let ((text (xterm-color-filter (plist-get data :text/plain))))
        (jupyter-repl-add-font-lock-properties 0 (length text) text)
        (jupyter-repl-insert text)
        (jupyter-repl-newline)))
     (t (error "No supported mimetype found %s" mimetypes)))))

;;; Prompt

(defun jupyter-repl--prompt-display-value (str face)
  (list '(margin left-margin)
        (propertize
         (concat (make-string
                  (- jupyter-repl-prompt-margin-width
                     (length str))
                  ? )
                 str)
         'fontified t
         'font-lock-face face)))

(defun jupyter-repl--insert-prompt (str face)
  (jupyter-repl-newline)
  (let ((ov (make-overlay (1- (point)) (point) nil t))
        (md (jupyter-repl--prompt-display-value str face)))
    (overlay-put ov 'after-string (propertize " " 'display md))
    (overlay-put ov 'evaporate t)
    ov))

(defun jupyter-repl-insert-prompt (&optional type)
  "Insert a REPL promp in CLIENT's buffer according to type.
If TYPE is nil or `in' insert a new input prompt. If TYPE is
`out' insert a new output prompt."
  (setq type (or type 'in))
  (unless (memq type '(in out continuation))
    (error "Prompt type can only be (`in', `out', or `continuation')"))
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t)
        ov props)
    (cond
     ((eq type 'in)
      (let ((count (oref jupyter-repl-current-client execution-count)))
        (setq ov (jupyter-repl--insert-prompt
                  (format "In [%d]:" count) 'jupyter-repl-input-prompt)
              props (list 'jupyter-cell (list 'beginning count)
                          'rear-nonsticky t))))
     ((eq type 'out)
      ;; Output is normally inserted by first going to the end of the output
      ;; for the request. The end of the ouput for a request is at the
      ;; beginning of the next cell after the request which is why `escape' is
      ;; needed here.
      (let ((count (jupyter-repl-cell-count 'escape)))
        (setq ov (jupyter-repl--insert-prompt
                  (format "Out [%d]:" count) 'jupyter-repl-output-prompt)
              props (list 'jupyter-cell (list 'out count)))))
     ((eq type 'continuation)
      (setq ov (jupyter-repl--insert-prompt
                ":" 'jupyter-repl-input-prompt)
            props (list 'read-only nil 'rear-nonsticky t))))
    (add-text-properties (overlay-start ov) (overlay-end ov) props)))

(defun jupyter-repl-cell-update-prompt (str)
  "Update the current input prompt string.
STR should be the text which will replace the current input
prompt."
  (let ((ov (car (overlays-at (jupyter-repl-cell-beginning-position)))))
    (when ov
      (overlay-put ov 'after-string
                   (propertize
                    " " 'display (jupyter-repl--prompt-display-value
                                  str 'jupyter-repl-input-prompt))))))

(defun jupyter-repl-cell-mark-busy ()
  "Mark the current cell as busy.
The changes the current input prompt to \"In [*]:\""
  (jupyter-repl-cell-update-prompt "In [*]:"))

(defun jupyter-repl-cell-unmark-busy ()
  "Un-mark the current cell as busy.
This changes the current input prompt to \"In [N]:\" where N is
the execution count of the cell."
  (jupyter-repl-cell-update-prompt
   (format "In [%d]:" (jupyter-repl-cell-count))))

(defun jupyter-repl-cell-count (&optional escape)
  "Get the cell count of the current cell at `point'.
If ESCAPE is non-nil and `point' is already at the beginning of a
cell, return the cell count of the cell before the current one."
  (let ((pos (if (and (not escape) (jupyter-repl-cell-beginning-p)) (point)
               (save-excursion
                 (jupyter-repl-previous-cell)
                 (point)))))
    (nth 1 (get-text-property pos 'jupyter-cell))))

(defun jupyter-repl-cell-request ()
  "Get the `jupyter-request' of the current cell."
  (get-text-property (jupyter-repl-cell-beginning-position) 'jupyter-request))

;;; Cell motions

(defun jupyter-repl-cell-beginning-position ()
  "Return the cell beginning position of the current cell.
If `point' is already at the beginning of the current cell,
return `point'. Note that if the end of a cell is found before
the beginning of a cell, i.e. when `point' is somewhere inside
the output of a cell, raise an error. If the beginning of the
buffer is found before the beginning of a cell, raise a
`beginning-of-buffer' error."
  (let ((pos (point)))
    (while (not (jupyter-repl-cell-beginning-p pos))
      (setq pos (or (previous-single-property-change pos 'jupyter-cell)
                    ;; Edge case when `point-min' is the beginning of a cell
                    (and (jupyter-repl-cell-beginning-p (point-min))
                         (point-min))))
      (if pos (when (jupyter-repl-cell-end-p pos)
                (error "Found end of previous cell"))
        (signal 'beginning-of-buffer nil)))
    pos))

(defun jupyter-repl-cell-end-position ()
  "Return the cell ending position of the current cell.
This is similar to `jupyter-repl-cell-beginning-position' except
the position at the end of the current cell is returned and an
error is raised if the beginning of a cell is found before an
end. Note that if the current cell is the last cell in the
buffer, `point-max' is considered the end of the cell."
  (let ((pos (point)))
    (catch 'unfinalized
      (while (not (jupyter-repl-cell-end-p pos))
        (setq pos (next-single-property-change pos 'jupyter-cell))
        (if pos (when (jupyter-repl-cell-beginning-p pos)
                  (error "Found beginning of next cell"))
          ;; Any unfinalized cell must be at the end of the buffer.
          (throw 'unfinalized (point-max))))
      pos)))

(defun jupyter-repl-cell-code-beginning-position ()
  "Return the beginning of the current cell's code.
The code beginning position is

   `jupyter-repl-cell-beginning-position' + 1"
  (let ((pos (jupyter-repl-cell-beginning-position)))
    (if pos (1+ pos)
      (point-min))))

(defun jupyter-repl-cell-code-end-position ()
  "Return the end of the current cell's code.
The code ending position is

   `jupyter-repl-cell-end-position' - 1"
  (let ((pos (jupyter-repl-cell-end-position)))
    (if (= pos (point-max)) (point-max)
      (1- pos))))

(defun jupyter-repl-next-cell (&optional N)
  "Go to the start of the next cell.
Optional argument N is the number of times to move to the next
cell. N defaults to 1."
  (interactive "N")
  (setq N (or N 1))
  (let ((dir (if (> N 0) 'forward 'backward)))
    (if (> N 0) (setq fun #'next-single-property-change)
      (setq N (abs N)
            fun #'previous-single-property-change))
    (catch 'done
      (while (> N 0)
        (let ((pos (funcall fun (point) 'jupyter-cell)))
          (while (and pos (not (jupyter-repl-cell-beginning-p pos)))
            (setq pos (funcall fun pos 'jupyter-cell)))
          (if pos (progn
                    (goto-char pos)
                    (setq N (1- N)))
            (goto-char (if (eq dir 'forward) (point-max)
                         (point-min)))
            (throw 'done t)))))
    N))

(defun jupyter-repl-previous-cell (&optional N)
  "Go to the start of the previous cell.
Optional argument N is the number of times to move to the
previous cell. N defaults to 1."
  (interactive "N")
  (setq N (or N 1))
  (jupyter-repl-next-cell (- N)))

;;; Predicates to determine what kind of line point is in

(defun jupyter-repl-cell-beginning-p (&optional pos)
  "Is POS the beginning of a cell?
POS defaults to `point'."
  (setq pos (or pos (point)))
  (eq (nth 0 (get-text-property pos 'jupyter-cell)) 'beginning))

(defun jupyter-repl-cell-end-p (&optional pos)
  "Is POS the end of a cell?
POS defaults to `point'."
  (setq pos (or pos (point)))
  (eq (nth 0 (get-text-property pos 'jupyter-cell)) 'end))

(defun jupyter-repl-multiline-p (text)
  "Is TEXT a multi-line string?"
  (string-match "\n" text))

(defun jupyter-repl-cell-line-p ()
  "Is the current line a cell input line?"
  ;; TODO: Better predicate
  (or (overlays-at (point-at-eol))
      (when (>= (1- (point-at-bol)) (point-min))
        (and (overlays-at (1- (point-at-bol)))
             (not (eq (car (get-text-property (1- (point-at-bol))
                                              'jupyter-cell))
                      'out))))))

;;; Getting/manipulating the code of a cell

(defun jupyter-repl-cell-code ()
  "Get the code of the current cell."
  (if (= (point-min) (point-max)) ""
    (let (lines)
      (save-excursion
        (goto-char (jupyter-repl-cell-code-beginning-position))
        (push (buffer-substring-no-properties (point-at-bol) (point-at-eol))
              lines)
        (while (and (line-move-1 1 'noerror)
                    (jupyter-repl-cell-line-p))
          (push (buffer-substring-no-properties (point-at-bol) (point-at-eol))
                lines))
        (mapconcat #'identity (nreverse lines) "\n")))))

(defun jupyter-repl-cell-code-position ()
  "Get the position that `point' is at relative to the contents of the cell.
The first character of the cell code corresponds to position 1."
  (unless (jupyter-repl-cell-line-p)
    (error "Not in code of cell"))
  (- (point) (jupyter-repl-cell-beginning-position)))

(defun jupyter-repl-finalize-cell (req)
  "Finalize the current cell.
REQ is the `jupyter-request' to associate with the current cell.
Finalizing a cell involves the following steps:

- Associate REQ with the cell
- Show the cell as busy
- Move `point' to the location where the next input cell can be
  inserted
- Add the text property which marks the end of a cell
- Make the cell read-only"
  (let ((beg (jupyter-repl-cell-beginning-position))
        (count (jupyter-repl-cell-count)))
    (remove-text-properties beg (1+ beg) '(rear-nonsticky))
    (jupyter-repl-cell-mark-busy)
    (goto-char (point-max))
    (jupyter-repl-newline)
    (add-text-properties
     (1- (point)) (point) (list 'jupyter-cell (list 'end count)))
    (add-text-properties beg (1+ beg) (list 'jupyter-request req))
    ;; font-lock-multiline to avoid improper syntactic elements from
    ;; spilling over to the rest of the buffer.
    (add-text-properties beg (point) '(read-only t font-lock-multiline t))))

(defun jupyter-repl-replace-cell-code (new-code)
  "Replace the current cell code with NEW-CODE."
  (delete-region (jupyter-repl-cell-code-beginning-position)
                 (jupyter-repl-cell-code-end-position))
  (jupyter-repl-insert :read-only nil new-code))

(defun jupyter-repl-truncate-buffer ()
  "Truncate the `current-buffer' based on `jupyter-repl-maximum-size'.
The `current-buffer' is assumed to be a Jupyter REPL buffer. If
the `current-buffer' is larger than `jupyter-repl-maximum-size'
lines then truncate it to something less than
`jupyter-repl-maximum-size' lines."
  (save-excursion
    (when (= (forward-line (- jupyter-repl-maximum-size)) 0)
      (jupyter-repl-next-cell)
      (delete-region (point-min) (point)))))

;;; Handlers

(defun jupyter-repl-goto-cell (req)
  "Go to the cell beginning position of REQ.
REQ should be a `jupyter-request' that corresponds to one of the
`jupyter-execute-request's created by a cell in the
`current-buffer'. Note that the `current-buffer' is assumed to be
a Jupyter REPL buffer."
  (goto-char (point-max))
  (condition-case nil
      (goto-char (jupyter-repl-cell-beginning-position))
    ;; Handle error when seeing the end of the previous cell
    (error (jupyter-repl-previous-cell)))
  (let (cell-req)
    (while (and (not (eq (setq cell-req (jupyter-repl-cell-request)) req))
                (not (= (point) (point-min))))
      (jupyter-repl-previous-cell))
    ;; Unless the request was found, assume it is for the current un-finalized
    ;; cell
    (unless (eq cell-req req)
      (goto-char (point-max)))))

(defun jupyter-repl-history-add-input (code)
  (ring-insert jupyter-repl-history code))

(cl-defmethod jupyter-execute-request ((client jupyter-repl-client)
                                       &key code
                                       (silent nil)
                                       (store-history t)
                                       (user-expressions nil)
                                       (allow-stdin t)
                                       (stop-on-error nil))
  (with-jupyter-repl-buffer client
    (jupyter-repl-truncate-buffer)
    (if code (cl-call-next-method)
      (setq code (jupyter-repl-cell-code))
      ;; Handle empty code cells as just an update of the prompt number
      (if (= (length code) 0)
          (setq silent t)
        ;; Needed by the prompt insertion below
        (oset client execution-count (1+ (oref client execution-count)))
        (jupyter-repl-history-add-input code))
      (let ((req (cl-call-next-method
                  client :code code :silent silent :store-history store-history
                  :user-expressions user-expressions :allow-stdin allow-stdin
                  :stop-on-error stop-on-error))
            ;; Don't insert a continuation prompt when inserting a new input
            ;; prompt
            (inhibit-modification-hooks t))
        (jupyter-repl-finalize-cell req)
        (jupyter-repl-insert-prompt 'in)
        req))))

;; TODO: Proper cleanup of pager buffer
;;
;; TODO: Define a minor mode for the pager
(defun jupyter-repl-pager-payload (text &optional line)
  (setq line (or line 0))
  (let (buf)
    (setq buf (get-buffer " *jupyter-repl-pager*"))
    (unless buf
      (setq buf (get-buffer-create " *jupyter-repl-pager*"))
      (with-current-buffer buf
        (special-mode)
        (local-set-key "q" #'quit-window)
        (local-set-key (kbd "SPC") #'scroll-down)
        (local-set-key (kbd "<backtab>") #'scroll-up)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq text (xterm-color-filter text))
        (jupyter-repl-add-font-lock-properties 0 (length text) text)
        (insert text)
        (goto-char (point-min))
        (forward-line line)))
    (display-buffer buf '(display-buffer-at-bottom
                          (pop-up-windows . t)))
    (fit-window-to-buffer (get-buffer-window buf))))

(defun jupyter-repl-set-next-input-payload (text)
  (goto-char (point-max))
  (jupyter-repl-previous-cell)
  (jupyter-repl-replace-cell-code text)
  (goto-char (point-max)))

(defun jupyter-repl-edit-payload (file line)
  (with-current-buffer (find-file-other-window file)
    (forward-line line)))

(cl-defmethod jupyter-handle-execute-reply ((client jupyter-repl-client)
                                            req
                                            execution-count
                                            user-expressions
                                            payload)
  (oset client execution-count (1+ execution-count))
  (with-jupyter-repl-buffer client
    (save-excursion
      (jupyter-repl-goto-cell req)
      (jupyter-repl-cell-unmark-busy))
    (when payload
      (cl-loop
       for pl in payload
       do (pcase (plist-get pl :source)
            ("page"
             (jupyter-repl-pager-payload
              (plist-get (plist-get pl :data) :text/plain)
              (plist-get pl :start)))
            ((or "edit" "edit_magic")
             (jupyter-repl-edit-payload
              (plist-get pl :filename)
              (plist-get pl :line_number)))
            ("set_next_input"
             (jupyter-repl-set-next-input-payload
              (plist-get pl :text))))))))

(cl-defmethod jupyter-handle-execute-input ((client jupyter-repl-client)
                                            req
                                            code
                                            execution-count)
  (oset client execution-count (1+ execution-count)))

(cl-defmethod jupyter-handle-execute-result ((client jupyter-repl-client)
                                             req
                                             execution-count
                                             data
                                             metadata)
  (jupyter-repl-do-at-request client req
    (jupyter-repl-insert-prompt 'out)
    (jupyter-repl-insert-data data)))

(cl-defmethod jupyter-handle-display-data ((client jupyter-repl-client)
                                           req data metadata transient)
  (jupyter-repl-do-at-request client req
    (jupyter-repl-insert-data data)))

(cl-defmethod jupyter-handle-stream ((client jupyter-repl-client) req name text)
  (jupyter-repl-do-at-request client req
    (let ((s (xterm-color-filter text)))
      (jupyter-repl-add-font-lock-properties 0 (length s) s)
      (jupyter-repl-insert s))))

(cl-defmethod jupyter-handle-error ((client jupyter-repl-client)
                                    req ename evalue traceback)
  (jupyter-repl-do-at-request client req
    (save-excursion
      ;; `point' is at the cell beginning of the next cell after REQ,
      ;; `jupyter-repl-previous-cell' will take us back to the start of the
      ;; cell corresponding to REQ.
      (jupyter-repl-previous-cell)
      (jupyter-repl-cell-unmark-busy))
    (let ((s (mapconcat #'xterm-color-filter traceback "\n")))
      (jupyter-repl-add-font-lock-properties 0 (length s) s)
      (jupyter-repl-insert s))
    (jupyter-repl-newline)))

(cl-defmethod jupyter-handle-input-reply ((client jupyter-repl-client) req prompt password)
  (jupyter-repl-do-at-request client req
    (let ((value (cl-call-next-method)))
      (jupyter-repl-previous-cell)
      (goto-char (1+ (jupyter-repl-cell-end-position)))
      (jupyter-repl-newline)
      (jupyter-repl-insert (concat prompt value)))))

(defun jupyter-repl-history-next (n)
  (interactive "p")
  (if (cl-loop
       repeat (or n 1)
       thereis (eq (ring-ref jupyter-repl-history -1) 'jupyter-repl-history)
       do (ring-insert
           jupyter-repl-history (ring-remove jupyter-repl-history -1)))
      (cond
       ((equal (jupyter-repl-cell-code)
               (ring-ref jupyter-repl-history 0))
        (jupyter-repl-replace-cell-code ""))
       ((equal (jupyter-repl-cell-code) "")
        (message "End of history"))
       (t))
    (jupyter-repl-replace-cell-code
     (ring-ref jupyter-repl-history 0))))

(defun jupyter-repl-history-previous (n)
  (interactive "p")
  (if (not (equal (jupyter-repl-cell-code)
                  (ring-ref jupyter-repl-history 0)))
      (jupyter-repl-replace-cell-code (ring-ref jupyter-repl-history 0))
    (if (cl-loop
         repeat (or n 1)
         thereis (eq (ring-ref jupyter-repl-history 1) 'jupyter-repl-history)
         do (ring-insert-at-beginning
             jupyter-repl-history (ring-remove jupyter-repl-history 0)))
        (message "End of history")
      (jupyter-repl-replace-cell-code
       (ring-ref jupyter-repl-history 0)))))

(cl-defmethod jupyter-handle-history-reply ((client jupyter-repl-client) req history)
  (with-jupyter-repl-buffer client
    (cl-loop for (session line-number input-output) in history
             do (ring-insert jupyter-repl-history input-output))))

(cl-defmethod jupyter-handle-is-complete-reply ((client jupyter-repl-client) req status indent)
  (with-jupyter-repl-buffer client
    (pcase status
      ("complete"
       (jupyter-execute-request client))
      ("incomplete"
       (jupyter-repl-newline)
       (if (= (length indent) 0) (jupyter-repl-indent-line)
         (jupyter-repl-insert :read-only nil indent)))
      ("invalid"
       ;; Force an execute to produce a traceback
       (jupyter-execute-request client))
      ("unknown"))))

(defun jupyter-repl-ret (arg)
  (interactive "P")
  (let ((client jupyter-repl-current-client)
        (last-cell-pos (save-excursion
                         (goto-char (point-max))
                         (jupyter-repl-cell-beginning-position))))
    (if (< (point) last-cell-pos)
        (goto-char (point-max))
      (if (not jupyter-repl-use-builtin-is-complete)
          (let ((res (jupyter-wait-until-received :is-complete-reply
                       (jupyter-is-complete-request client
                         :code (jupyter-repl-cell-code)))))
            (unless res
              (setq-local jupyter-repl-use-builtin-is-complete t)
              (jupyter-repl-ret arg)))
        (let ((complete-p (equal
                           (save-excursion
                             (goto-char (jupyter-repl-cell-code-end-position))
                             (buffer-substring-no-properties
                              (line-beginning-position) (point)))
                           "")))
          (jupyter-handle-is-complete-reply client
            nil (if complete-p "complete" "incomplete") ""))))))

(defun jupyter-repl-indent-line ()
  "Indent the line according to the language of the REPL."
  (let ((spos (jupyter-repl-cell-code-beginning-position))
        (pos (jupyter-repl-cell-code-position))
        (code (jupyter-repl-cell-code)))
    (jupyter-repl-replace-cell-code
     (with-jupyter-repl-lang-buffer
       (insert code)
       (goto-char pos)
       (indent-according-to-mode)
       (setq pos (point))
       (buffer-string)))
    (goto-char (+ pos spos))))

;;; Buffer change functions

(defun jupyter-repl-after-buffer-change (beg end len)
  "Insert line continuation prompts in `jupyter-repl-mode' buffers.
BEG, END, and LEN have the same meaning as for
`after-change-functions'. If the change corresponds to text being
inserted and the beginning of the insertion is on a
`jupyter-repl-cell-line-p', insert line continuation prompts if
the inserted text is multi-line."
  (when (eq major-mode 'jupyter-repl-mode)
    (cond
     ;; Insertions only
     ((= len 0)
      (goto-char beg)
      (when (jupyter-repl-cell-line-p)
        (while (search-forward "\n" end 'noerror)
          (delete-char -1)
          (jupyter-repl-insert-prompt 'continuation)))
      (goto-char end)))))

(defun jupyter-repl-kill-buffer-query-function ()
  "Ask before killing a Jupyter REPL buffer.
If the REPL buffer is killed, stop the client and possibly the
kernel that the REPL buffer is connected to."
  (not (and (eq major-mode 'jupyter-repl-mode)
            (or (jupyter-kernel-alive-p jupyter-repl-kernel-manager)
                (jupyter-channels-running-p jupyter-repl-current-client))
            (if (y-or-n-p
                 (format "Jupyter REPL (%s) still connected. Kill it? "
                         (buffer-name (current-buffer))))
                (prog1 nil
                  (kill-buffer jupyter-repl-lang-buffer)
                  (jupyter-stop-channels jupyter-repl-current-client)
                  (when jupyter-repl-kernel-manager
                    (jupyter-shutdown-kernel jupyter-repl-kernel-manager)))
              t))))

(add-hook 'kill-buffer-query-functions #'jupyter-repl-kill-buffer-query-function)

;; FIXME: Sometimes when using packages like `perspective', upon switching back
;; to a perspective which has a REPL buffer visible, the margins will
;; disappear. It doesn't happen all the time though. Work around this by
;; setting the margins after a window configuration change.
(defun jupyter-repl-preserve-window-margins ()
  (unless (window-parameter nil 'min-margins)
    (set-window-parameter
     nil 'min-margins (cons jupyter-repl-prompt-margin-width 0))
    (set-window-margins nil jupyter-repl-prompt-margin-width)))

;;; Completion

(defun jupyter-repl-company-normalize-position (pos prefix)
  "Normalize POS based on the length of PREFIX for the current context.
If the `major-mode' is `jupyter-repl-mode' then POS is relative
to the contents of the current code cell."
  (if (eq major-mode 'jupyter-repl-mode)
      (cl-decf pos (- (point) (jupyter-repl-cell-code-beginning-position)))
    pos))

(defun jupyter-repl-code-context-at-point (type &optional prefix)
  "Return a cons cell, (CODE . POS), for the context around `point'.
Returns the required context depending on TYPE which can be
either `inspect' or `complete'. If TYPE is `inspect' return an
appropriate context for an inspect request. If TYPE is `complete'
return an appropriate context for a completion request.

The context also depends on the `major-mode' of the
`current-buffer'. If the `current-buffer' is a
`jupyter-repl-mode' buffer, CODE is the contents of the entire
code cell. Otherwise its either the line up to `point' if TYPE is
`complete' or the entire line TYPE is `inspect'."
  ;; FIXME: Remove the need for PREFIX it is currently used because the Julia
  ;; kernel doesn't return the right completions in the following scenario
  ;;
  ;;     readline(ST|)
  ;;
  ;; If the entire line is sent with the code position at |, then the kernel
  ;; just dumps all available completions without a prefix when clearly we want
  ;; the ones that start with ST.
  (unless (memq type '(complete inspect))
    (error "Type not `compete' or `inspect' (%s)" type))
  (if (eq major-mode 'jupyter-repl-mode)
      (if (and prefix (eq jupyter-repl-lang-mode 'julia-mode))
          (cons prefix (length prefix))
        (let ((code (jupyter-repl-cell-code))
              (pos (jupyter-repl-cell-code-position)))
          ;; Consider the case when completing
          ;;
          ;;     In [1]: foo|
          ;;
          ;; The cell code position will be at position 4, i.e. where the cursor
          ;; is at, but the cell code will only be 3 characters long. This is the
          ;; reason for the check on pos.
          (cons code (if (> pos (length code)) (length code) pos))))
    (cons (buffer-substring (line-beginning-position)
                            (cl-case type
                              (inspect (line-end-position))
                              (otherwise (point))))
          (- (point) (line-beginning-position)))))

(defun jupyter-repl-completion-prefix ()
  "Return the prefix for the current completion context.
Note that the prefix returned is not the content sent to the
kernel. The prefix is the symbol (including punctuation) just
before `point'. See `jupyter-repl-code-context-at-point' for what
is actually sent to the kernel."
  (when jupyter-repl-current-client
    (let ((lang-mode (with-jupyter-repl-buffer
                         jupyter-repl-current-client
                       jupyter-repl-lang-mode)))
      (and (memq major-mode `(,lang-mode jupyter-repl-mode))
           ;; No completion in finalized cells
           (not (get-text-property (point) 'read-only))
           (if (or (looking-at "\\_>")
                   (looking-back "\\.\\|->\\|::" 2))
               (buffer-substring
                (save-excursion
                  (skip-syntax-backward "w_.")
                  (point))
                (point))
             (unless (and (char-after)
                          (memq (char-syntax (char-after))
                                '(?w ?_ ?.)))
               ""))))))

;; FIXME: start and end are actually not currently used. What would be the most
;; general way of using them.
(defun jupyter-repl-construct-completion-candidates (prefix matches metadata start end)
  "Construct candidates for `company-mode' completion.
PREFIX is the prefix used to start the current completion.
MATCHES are the completion matches returned by the kernel,
METADATA is any extra data associated with MATCHES and is
currently used for adding annotations to each candidate. START
and END are the start and end of text in the current
`jupyter-repl-company-context' that should be replaced by the
elements of MATCHES."
  ;; TODO: Handle cases in the Jupyter repl when
  ;; `company-minimum-prefix-length' is 1 and the prefix is '='
  (let ((types (plist-get metadata :_jupyter_types_experimental)))
    (let ((matches matches) match)
      (while (setq match (car matches))
        ;; TODO: Maybe set the match property when it doesn't have the prefix,
        ;; indicating that it should replace part of the prefix?
        (unless (string-prefix-p prefix match)
          ;; FIXME: Note that prefix is not the code cent to the kernel in some
          ;; cases, but the symbol behind point
          (setcar matches (concat (seq-subseq prefix (- end start))
                                  (car matches))))
        ;; (put-text-property 0 1 'match match-start (car matches))
        (setq matches (cdr matches))))
    (when types
      (let ((max-len (apply #'max (mapcar #'length matches))))
        (cl-mapcar
         (lambda (match meta)
           (put-text-property
            0 1 'annot
            (concat (make-string (1+ (- max-len (length match))) ? )
                    (plist-get meta :type))
            match)
           match)
         matches types)))
    matches))

(defun company-jupyter-repl (command &optional arg &rest _)
  "`company-mode' backend using a `jupyter-repl-client'.
COMMAND and ARG have the same meaning as the elements of
`company-backends'."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-jupyter-repl))
    (sorted t)
    (prefix (or (jupyter-repl-completion-prefix) 'stop))
    (candidates (cons
                 :async
                 (lambda (cb)
                   (cl-destructuring-bind (code . pos)
                       (jupyter-repl-code-context-at-point 'complete arg)
                     (jupyter-add-callback
                         ;; Ignore errors during completion
                         (jupyter-request-inhibit-handlers
                          (jupyter-complete-request
                              jupyter-repl-current-client
                            :code code :pos pos))
                       :complete-reply
                       (lambda (msg)
                         (cl-destructuring-bind (&key status
                                                      matches metadata
                                                      cursor_start cursor_end
                                                      &allow-other-keys)
                             (jupyter-message-content msg)
                           (funcall
                            cb (when (equal status "ok")
                                 (jupyter-repl-construct-completion-candidates
                                  arg matches metadata cursor_start cursor_end))))))))))
    (annotation (get-text-property 0 'annot arg))
    (doc-buffer (let ((doc (jupyter-repl--inspect
                            arg (length arg) company-async-timeout)))
                  (remove-text-properties 0 (length doc) '(read-only) doc)
                  (when doc (company-doc-buffer doc))))))

;;; The mode

(defvar jupyter-repl-mode-map (let ((map (make-sparse-keymap)))
                                (define-key map (kbd "RET") #'jupyter-repl-ret)
                                (define-key map (kbd "C-n") #'jupyter-repl-history-next)
                                (define-key map (kbd "C-p") #'jupyter-repl-history-previous)
                                map))

(define-derived-mode jupyter-repl-mode fundamental-mode
  "Jupyter-REPL"
  "A major mode for interacting with a Jupyter kernel."
  (setq-local indent-line-function #'jupyter-repl-indent-line)
  (setq-local company-backends (cons 'company-jupyter-repl company-backends))
  (add-hook 'after-change-functions 'jupyter-repl-after-buffer-change nil t)
  (add-hook 'window-configuration-change-hook 'jupyter-repl-preserve-window-margins nil t))

(defun jupyter-repl-initialize-fontification ()
  (let (fld)
    (with-jupyter-repl-lang-buffer
      (setq fld font-lock-defaults))
    (setq font-lock-defaults fld)
    (font-lock-mode)))

(defun jupyter-repl-insert-banner (banner)
  "Insert BANNER into the `current-buffer'.
Make the text of BANNER read only and apply the `shadow' face to
it."
  (let ((start (point))
        (inhibit-modification-hooks t))
    (jupyter-repl-insert banner)
    (jupyter-repl-newline)
    (add-text-properties start (point) '(font-lock-face shadow fontified t))))

(defun jupyter-repl-sync-execution-count ()
  "Synchronize the execution count of `jupyter-repl-current-client'.
Set the execution-count slot of `jupyter-repl-current-client' to
1+ the execution count of the client's kernel."
  (let* ((client jupyter-repl-current-client)
         (req (jupyter-execute-request client :code "" :silent t)))
    (jupyter-request-inhibit-handlers req)
    (jupyter-add-callback req
      :execute-reply (lambda (msg)
                       (oset client execution-count
                             (1+ (jupyter-message-get msg :execution_count)))))
    (jupyter-wait-until-idle req)))

(defun run-jupyter-repl (kernel-name)
  "Run a Jupyter REPL connected to a kernel named KERNEL-NAME.
KERNEL-NAME can be the prefix of an available kernel name, in
this case the first kernel in `jupyter-available-kernelspecs'
that has KERNEL-NAME as a prefix will be used to start a new
kernel."
  (interactive
   (list
    (completing-read
     "kernel: " (mapcar #'car (jupyter-available-kernelspecs)) nil t)))
  (message "Starting %s kernel..." kernel-name)
  (cl-destructuring-bind (km . kc)
      (jupyter-start-new-kernel kernel-name 'jupyter-repl-client)
    (oset kc buffer (generate-new-buffer
                     (format "*jupyter-repl[%s]*" (oref km name))))
    (with-jupyter-repl-buffer kc
      (cl-destructuring-bind (&key language_info banner &allow-other-keys)
          (oref km kernel-info)
        (cl-destructuring-bind (&key name file_extension &allow-other-keys)
            language_info
          (erase-buffer)
          (jupyter-repl-mode)
          (setq-local left-margin-width jupyter-repl-prompt-margin-width)
          (setq-local jupyter-repl-current-client kc)
          (setq-local jupyter-repl-kernel-manager km)
          (setq-local jupyter-repl-history
                      (make-ring (1+ jupyter-repl-history-maximum-length)))
          ;; The sentinel value keeps track of the newest/oldest elements of
          ;; the history since next/previous navigation is implemented by
          ;; rotations on the ring.
          (ring-insert jupyter-repl-history 'jupyter-repl-history)
          (setq-local jupyter-repl-lang-buffer
                      (get-buffer-create (format " *jupyter-repl-lang-%s*" name)))
          (let (mode)
            (with-jupyter-repl-lang-buffer
              (let ((buffer-file-name
                     (concat "jupyter-repl-lang" file_extension)))
                (set-auto-mode)
                (setq mode major-mode)))
            (setq-local jupyter-repl-lang-mode mode))
          (jupyter-history-request kc :n 100 :raw nil :unique t)
          (jupyter-repl-initialize-fontification)
          (jupyter-repl-insert-banner banner)
          (jupyter-repl-sync-execution-count)
          (jupyter-repl-insert-prompt 'in))))
    (pop-to-buffer (oref kc buffer))))

(provide 'jupyter-repl-client)

;;; jupyter-repl-client.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
