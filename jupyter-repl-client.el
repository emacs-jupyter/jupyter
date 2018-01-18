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

(defgroup jupyter-repl nil
  "A Jupyter REPL client"
  :group 'jupyter)

(require 'jupyter-base)
(require 'jupyter-client)
(require 'jupyter-kernel-manager)
(require 'xterm-color)
(require 'shr)
(require 'ring)

;; TODO: Read up on how method tags can be used, see
;; https://ericabrahamsen.net/tech/2016/feb/bbdb-eieio-object-oriented-elisp.html

;; TODO: Fallbacks for when the language doesn't have a major mode installed.


;;; User variables

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
  "Face used for the output prompt."
  :group 'jupyter-repl)

(defcustom jupyter-repl-maximum-size 1024
  "Maximum number of lines before the buffer is truncated."
  :group 'jupyter-repl)

(defcustom jupyter-repl-maximum-is-complete-timeout 2
  "Maximum number of seconds to wait for a is-complete reply."
  :group 'jupyter-repl)

(defcustom jupyter-repl-history-maximum-length 100
  "The maximum number of history elements to keep track of."
  :group 'jupyter-repl)

(defcustom jupyter-repl-prompt-margin-width 12
  "The width of the margin which displays prompt strings."
  :group 'jupyter-repl)

;;; Implementation

(defclass jupyter-repl-client (jupyter-kernel-client)
  ((buffer :type buffer :initarg :buffer)
   (execution-state :type string :initform "idle")
   (execution-count :type integer :initform 1)))


(defvar jupyter-repl-lang-buffer nil
  "A buffer with the `major-mode' set to the REPL language's `major-mode'.")

(defvar jupyter-repl-current-client nil
  "The `jupyter-repl-client' for the `current-buffer'.")
(put 'jupyter-repl-current-client 'permanent-local t)

(defvar jupyter-repl-kernel-manager nil
  "The `jupyter-kernel-manager' for the `current-buffer' (if available).
When the kernel that the `jupyter-repl-client' of the
`current-buffer' is connected to was started as a subprocess of
the current Emacs process, this variable will hold the
`jupyter-kernel-manager' used.")
(put 'jupyter-repl-kernel-manager 'permanent-local t)

(defvar jupyter-repl-lang-mode nil
  "The `major-mode' corresponding to the kernel's language.")

(defvar jupyter-repl-history nil
  "The history of the current Jupyter REPL.")

(defvar jupyter-repl-fontify-buffers nil
  "An alist of (MODE . BUFFER) pairs used for fontification.
See `jupyter-repl-fontify-according-to-mode'.")

(defvar jupyter-repl-use-builtin-is-complete nil
  "Whether or not to send is_complete_request's to a kernel.
If a Jupyter kernel does not respond to an is_complete_request,
the buffer local value of this variable is set to t and code in a
cell is considered complete if the last line in a code cell is a
blank line, i.e. if RET is pressed twice in a row.")

;;; Macros

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

(defmacro jupyter-repl-without-continuation-prompts (&rest body)
  "Run BODY without inserting continuation prompts.
Normally a continuation prompt is inserted for every newline
inserted into the REPL buffer through a function in
`after-change-functions'. Prevent the function from running while
executing BODY."
  `(let ((inhibit-modification-hooks t))
     ,@body))

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
     (jupyter-repl-without-continuation-prompts
      (save-excursion
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

(defun jupyter-repl-get-doc-buffer (name)
  "Return the REPL documentation buffer for NAME.
A REPL documentation buffer has the following characteristics:

- `major-mode' is `special-mode'

- local keybindings to quit the window (q), and scroll the
  window (SPC and <backtab>).

The buffer returned will have a `buffer-name' with the form

    \"*jupyter-repl-NAME*\""
  (let* ((bname (format "*jupyter-repl-%s*" name))
         (buffer (get-buffer bname)))
    (unless buffer
      (setq buffer (get-buffer-create bname))
      (with-current-buffer buffer
        (special-mode)
        (local-set-key "q" #'quit-window)
        (local-set-key (kbd "SPC") #'scroll-down)
        (local-set-key (kbd "<backtab>") #'scroll-up)))
    buffer))

(defmacro with-jupyter-repl-doc-buffer (name &rest body)
  "With the REPL documentation buffer corresponding to NAME, run BODY.
NAME should be a string representing the purpose of the
documentation buffer. The buffer corresponding to NAME will be
obtained by a call to `juptyer-repl-get-doc-buffer'. Before
running BODY, the doc buffer is set as the
`other-window-scroll-buffer' and the contents of the buffer are
erased."
  (declare (indent 1))
  (let ((buffer (make-symbol "buffer")))
    `(let ((,buffer (jupyter-repl-get-doc-buffer ,name)))
       (with-current-buffer ,buffer
         (let ((other-window-scroll-buffer nil)
               (inhibit-read-only t))
           (erase-buffer)
           (setq other-window-scroll-buffer (current-buffer))
           ,@body)))))

;;; Text insertion

(defun jupyter-repl-add-font-lock-properties (start end &optional object)
  "Add font lock text properties between START and END in the `current-buffer'.
START, END, and OBJECT have the same meaning as in
`add-text-properties'. The properties added are the ones that
mark the text between START and END as fontified according to
font lock. Any text between START and END that does not have a
`font-lock-face' property will have the `default' face filled in
for the property."
  (add-text-properties
   start end '(fontified t font-lock-fontified t font-lock-multiline t) object)
  (font-lock-fillin-text-property
   start end 'font-lock-face 'default object))

;; Adapted from `org-src-font-lock-fontify-block'
(defun jupyter-repl-fixup-font-lock-properties ()
  "Fixup the text properties in the `curren-buffer'.
Fixing the text properties of the current buffer involves
substiuting any face properties with font-lock-face for insertion
into the REPL buffer and also adds handles
`font-lock-extra-managed-props'. Note that if text does not have
a face property, then a face of default is added to it."
  (let ((pos (point-min)) next)
    (catch 'done
      (while (setq next (or (next-property-change pos) (point-max)))
        ;; Handle additional properties from font-lock, so as to
        ;; preserve, e.g., composition.
        (dolist (prop (cons 'face font-lock-extra-managed-props))
          (let ((new-prop (get-text-property pos prop)))
            (put-text-property
             pos next
             (if (eq prop 'face) 'font-lock-face prop)
             (if (eq prop 'face) (or new-prop 'default)
               new-prop))))
        (setq pos next)
        (when (= pos (point-max))
          (throw 'done t))))))

(defun jupyter-repl-get-fontify-buffer (mode)
  "Get the cached buffer used to fontify text for MODE.
Consult the `jupyter-repl-fontify-buffers' alist for a buffer to
use for fontification according to MODE and return the buffer
found. If no buffer exists for MODE: create a new buffer, set its
`major-mode' to MODE, add it to `juptyer-repl-fontify-buffers',
and return the buffer."
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
MODE has the same meaning as in
`jupyter-repl-get-fontify-buffer'. STR is a string that will be
fontified according to MODE by inserting it into the buffer
returned by `jupyter-repl-get-fontify-buffer' (erasing any
contents of the buffer before insertion).

In addition to fontifying STR, if MODE has a non-default
`fill-forward-paragraph-function', STR will be filled using
`fill-region'."
  (with-current-buffer (jupyter-repl-get-fontify-buffer mode)
    (let ((inhibit-modification-hooks nil))
      (erase-buffer)
      (insert str)
      (font-lock-ensure)
      ;; FIXME: This adds a font-lock-face of default if text doesn't have a
      ;; font-lock-face and so does `jupyter-repl-add-font-lock-properties'
      (jupyter-repl-fixup-font-lock-properties))
    (jupyter-repl-add-font-lock-properties (point-min) (point-max))
    (when (not (memq fill-forward-paragraph-function
                     '(forward-paragraph)))
      (fill-region (point-min) (point-max)))
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
         (shr-insert-document xml))
       (jupyter-repl-fixup-font-lock-properties)
       (string-trim (buffer-string))))))

(defun jupyter-repl-insert-markdown (text)
  "Insert TEXT, fontifying it using `markdown-mode' first."
  (jupyter-repl-insert
   (let ((markdown-hide-markup t)
         (markdown-hide-urls t)
         (markdown-fontify-code-blocks-natively t))
     (jupyter-repl-fontify-according-to-mode 'markdown-mode text))))

(defun jupyter-repl-insert-latex (tex)
  "Generate and insert a LaTeX image based on TEX.

Note that this uses `org-format-latex' to generate the LaTeX
image."
  (require 'org)
  ;; FIXME: Getting a weird error when killing the temp buffers created by
  ;; `org-format-latex'. When generating the image, it seems that the temp
  ;; buffers created have the same major mode and local variables as the REPL
  ;; buffer which causes the query function to ask to kill the kernel client
  ;; when the temp buffers are killed!
  (let ((kill-buffer-query-functions nil)
        (org-format-latex-options
         `(:foreground
           default
           :background default :scale 2.0
           :matchers ,(plist-get org-format-latex-options :matchers)))
        beg end)
    (setq beg (point))
    (jupyter-repl-insert tex)
    (setq end (point))
    (org-format-latex
     "jupyter-repl" beg end "jupyter-repl"
     'overlays "Creating LaTeX image...%s"
     'forbuffer
     ;; Use the default method for creating image files
     org-preview-latex-default-process)
    (goto-char end)))

(defun jupyter-repl-insert-ansi-coded-text (text)
  "Insert TEXT, converting ANSI color codes to font lock faces."
  (setq text (xterm-color-filter text))
  (jupyter-repl-add-font-lock-properties 0 (length text) text)
  (jupyter-repl-insert text))

(defun jupyter-repl-insert-data (data)
  (let ((mimetypes (cl-loop for (k d) on data by #'cddr
                            when (and d (not (equal d ""))) collect k)))
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
      (jupyter-repl-insert-latex (plist-get data :text/latex))
      (jupyter-repl-newline))
     ((and (memq :text/markdown mimetypes) (require 'markdown-mode nil t))
      (jupyter-repl-insert-markdown (plist-get data :text/markdown)))
     ((memq :text/plain mimetypes)
      (jupyter-repl-insert-ansi-coded-text
       (plist-get data :text/plain))
      (jupyter-repl-newline))
     (t (warn "No supported mimetype found %s" mimetypes)))))

;;; Prompt

(defun jupyter-repl--prompt-display-value (str face)
  "Return the margin display value for a prompt.
STR is the string used for the display value and FACE is the
`font-lock-face' to use for STR."
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
  "Insert a new prompt at `point'.
STR is the prompt string displayed in the `left-margin' using
FACE as the `font-lock-face'. A newline is inserted before adding
the prompt. The prompt string is inserted as a `display' text
property in the `after-string' property of the overlay and the
overlay is added to the newline character just inserted."
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
  "Update the current cell's input prompt.
STR is the replacement prompt string."
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
      (setq pos (previous-single-property-change pos 'jupyter-cell))
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
  (1+ (jupyter-repl-cell-beginning-position)))

(defun jupyter-repl-cell-code-end-position ()
  "Return the end of the current cell's code.
The code ending position is

   `jupyter-repl-cell-end-position' - 1

In the case of the last cell in the REPL buffer, i.e. an
unfinalized cell, the code ending position is `point-max'."
  (let ((pos (jupyter-repl-cell-end-position)))
    (if (= pos (point-max)) (point-max)
      (1- pos))))

(defun jupyter-repl-next-cell (&optional N)
  "Go to the start of the next cell.
Optional argument N is the number of times to move to the next
cell. N defaults to 1."
  (or N (setq N 1))
  (catch 'done
    (while (> N 0)
      (let ((pos (next-single-property-change (point) 'jupyter-cell)))
        (while (and pos (not (jupyter-repl-cell-beginning-p pos)))
          (setq pos (next-single-property-change pos 'jupyter-cell)))
        (unless (when pos (goto-char pos) (setq N (1- N)))
          (goto-char (point-max))
          (throw 'done t)))))
  N)

(defun jupyter-repl-previous-cell (&optional N)
  "Go to the start of the previous cell.
Optional argument N is the number of times to move to the
previous cell. N defaults to 1."
  (or N (setq N 1))
  (catch 'done
    (while (> N 0)
      (let ((pos (previous-single-property-change (point) 'jupyter-cell)))
        (while (and pos (not (jupyter-repl-cell-beginning-p pos)))
          (setq pos (previous-single-property-change pos 'jupyter-cell)))
        (unless (when pos (goto-char pos) (setq N (1- N)))
          (goto-char (point-min))
          (throw 'done t)))))
  N)

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

(defun jupyter-repl-forward-cell (&optional arg)
  "Move to the code beginning of the cell after the current one.
ARG is the number of cells to move and defaults to 1."
  (interactive "^p")
  (or arg (setq arg 1))
  (jupyter-repl-next-cell arg)
  (goto-char (jupyter-repl-cell-code-beginning-position)))

(defun jupyter-repl-backward-cell (&optional arg)
  "Move to the code beginning of the cell before the current one.
ARG is the number of cells to move and defaults to 1."
  (interactive "^p")
  (or arg (setq arg 1))
  ;; `jupyter-previous-cell' only goes to the start of the current cell if
  ;; `point' is greater than `jupyter-repl-cell-beginning-p' so move there when
  ;; at the beginning of the current cell code so that we can escape to the
  ;; previous cell.
  (when (= (jupyter-repl-cell-code-beginning-position) (point))
    (goto-char (jupyter-repl-cell-beginning-position)))
  (jupyter-repl-previous-cell arg)
  (goto-char (jupyter-repl-cell-code-beginning-position)))

;;; Predicates

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

(defun jupyter-repl-cell-finalized-p ()
  "Has the current cell been finalized?
A cell is considered finalized when `jupyter-repl-finalize-cell'
has been previously called for it. After a call to
`jupyter-repl-finalize-cell', `jupyter-repl-cell-end-p' will
return a non-nil value for the `jupyter-repl-cell-end-position'."
  (jupyter-repl-cell-end-p
   (jupyter-repl-cell-end-position)))

;;; Buffer text manipulation

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
- Move `point' to the location where the next input cell can be
  inserted
- Add the text property which marks the end of a cell
- Make the cell read-only"
  (let ((beg (jupyter-repl-cell-beginning-position))
        (count (jupyter-repl-cell-count)))
    (remove-text-properties beg (1+ beg) '(rear-nonsticky))
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
  ;; Prevent wrapping with `inhibit-read-only' so that an error is thrown when
  ;; trying to replace a finalized cell.
  (goto-char (jupyter-repl-cell-code-beginning-position))
  (delete-region (point) (jupyter-repl-cell-code-end-position))
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

(defun jupyter-repl-history-add-input (code)
  "Add CODE as the newest element in the REPL history."
  ;; Ensure the newest element is actually the newest element and not the most
  ;; recently navigated history element.
  (while (not (eq (ring-ref jupyter-repl-history -1) 'jupyter-repl-history))
    (ring-insert jupyter-repl-history (ring-remove jupyter-repl-history)))
  ;; Remove the second to last element when the ring is full to preserve the
  ;; sentinel.
  (when (eq (ring-length jupyter-repl-history)
            (ring-size jupyter-repl-history))
    (ring-remove jupyter-repl-history -2))
  (ring-remove+insert+extend jupyter-repl-history code))

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
                  :stop-on-error stop-on-error)))
        (jupyter-repl-without-continuation-prompts
         (jupyter-repl-cell-mark-busy)
         (jupyter-repl-finalize-cell req)
         (jupyter-repl-insert-prompt 'in))
        req))))

(cl-defmethod jupyter-handle-execute-reply ((client jupyter-repl-client)
                                            req
                                            execution-count
                                            user-expressions
                                            payload)
  (oset client execution-count (1+ execution-count))
  (with-jupyter-repl-buffer client
    (save-excursion
      (jupyter-repl-goto-cell req)
      (jupyter-repl-cell-unmark-busy)
      (when payload
        (cl-loop
         for pl in payload
         do (pcase (plist-get pl :source)
              ("page"
               (let ((text (plist-get (plist-get pl :data) :text/plain))
                     (line (or (plist-get pl :start) 0))))
               (with-jupyter-repl-doc-buffer "pager"
                 (setq text (xterm-color-filter text))
                 (jupyter-repl-add-font-lock-properties 0 (length text) text)
                 (insert text)
                 (goto-char (point-min))
                 (forward-line line)
                 (display-buffer (current-buffer) '(display-buffer-at-bottom
                                                    (pop-up-windows . t)))
                 (fit-window-to-buffer (get-buffer-window))))
              ((or "edit" "edit_magic")
               (with-current-buffer (find-file-other-window
                                     (plist-get pl :filename))
                 (forward-line (plist-get pl :line_number))))
              ("set_next_input"
               (goto-char (point-max))
               (jupyter-repl-previous-cell)
               (jupyter-repl-replace-cell-code (plist-get pl :text))
               (goto-char (point-max)))))))))

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

(cl-defmethod jupyter-handle-status ((client jupyter-repl-client) req execution-state)
  (oset client execution-state execution-state))

(cl-defmethod jupyter-handle-stream ((client jupyter-repl-client) req name text)
  (jupyter-repl-do-at-request client req
    (jupyter-repl-insert-ansi-coded-text text)))

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
      (jupyter-repl-insert (concat prompt value))
      (jupyter-repl-newline))))

(defun jupyter-repl-history-next (&optional n no-replace)
  "Go to the next history element.
Navigate through the REPL history to the next (newer) history
element and insert it as the last code cell. For N positive move
forward in history that many times. If N is negative, move to
older history elements.

If NO-REPLACE is non-nil, don't insert the history element in the
REPL buffer."
  (interactive "p")
  (or n (setq n 1))
  (if (< n 0) (jupyter-repl-history-previous (- n) no-replace)
    (goto-char (point-max))
    (if (cl-loop
         repeat n
         thereis (eq (ring-ref jupyter-repl-history -1) 'jupyter-repl-history)
         do (ring-insert
             jupyter-repl-history (ring-remove jupyter-repl-history -1)))
        (cond
         ((equal (jupyter-repl-cell-code)
                 (ring-ref jupyter-repl-history 0))
          (jupyter-repl-replace-cell-code ""))
         ((equal (jupyter-repl-cell-code) "")
          (error "End of history"))
         (t))
      (unless no-replace
        (jupyter-repl-replace-cell-code
         (ring-ref jupyter-repl-history 0))))))

(defun jupyter-repl-history-previous (&optional n no-replace)
  "Go to the previous history element.
Similar to `jupyter-repl-history-next' but for older history
elements. If N is negative in this case, move to newer history
elements.

If NO-REPLACE is non-nil, don't insert the history element in the
REPL buffer."
  (interactive "p")
  (or n (setq n 1))
  (if (< n 0) (jupyter-repl-history-next (- n) no-replace)
    (goto-char (point-max))
    (if (not (equal (jupyter-repl-cell-code)
                    (ring-ref jupyter-repl-history 0)))
        (jupyter-repl-replace-cell-code (ring-ref jupyter-repl-history 0))
      (if (cl-loop
           repeat n
           thereis (eq (ring-ref jupyter-repl-history 1) 'jupyter-repl-history)
           do (ring-insert-at-beginning
               jupyter-repl-history (ring-remove jupyter-repl-history 0)))
          (error "Beginning of history")
        (unless no-replace
          (jupyter-repl-replace-cell-code
           (ring-ref jupyter-repl-history 0)))))))

(cl-defmethod jupyter-handle-history-reply ((client jupyter-repl-client) req history)
  (with-jupyter-repl-buffer client
    (cl-loop for (_session _line-number input-output) in history
             do (ring-remove+insert+extend jupyter-repl-history input-output))))

(cl-defmethod jupyter-handle-is-complete-reply ((client jupyter-repl-client) _req status indent)
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

(cl-defmethod jupyter-handle-shutdown-reply ((client jupyter-repl-client) _req restart)
  (with-jupyter-repl-buffer client
    (goto-char (point-max))
    (add-text-properties (jupyter-repl-cell-beginning-position)
                         (jupyter-repl-cell-end-position)
                         '(read-only t))
    (jupyter-repl-without-continuation-prompts
     (jupyter-repl-newline)
     (jupyter-repl-newline)
     ;; TODO: Add a slot mentioning that the kernel is shutdown so that we can
     ;; block sending requests or delay until it has restarted.
     (jupyter-repl-insert
      (propertize (concat "kernel " (if restart "restart" "shutdown"))
                  'font-lock-face 'warning))
     (jupyter-repl-newline))))

(defun jupyter-repl-ret (&optional force)
  "Send the current cell code to the kernel.
If `point' is before the last cell in the REPL buffer move to
`point-max', i.e. move to the last cell. Otherwise if `point' is
at some position within the last cell of the REPL buffer, either
insert a newline or ask the kernel to execute the cell code
depending on the kernel's response to an is_complete_request. If
FORCE is non-nil, force the kernel to execute the current cell
code without sending the is_complete_request. See
`jupyter-repl-use-builtin-is-complete' for yet another way to
execute the current cell."
  (interactive "P")
  (if (< (point) (save-excursion
                   (goto-char (point-max))
                   (jupyter-repl-cell-beginning-position)))
      (goto-char (point-max))
    (unless (or (and jupyter-repl-kernel-manager
                     (jupyter-kernel-alive-p jupyter-repl-kernel-manager))
                (jupyter-hb-beating-p
                 (oref jupyter-repl-current-client hb-channel)))
      (error "Kernel not alive"))
    ;; NOTE: kernels allow execution requests to queue up, but we prevent
    ;; sending a request when the kernel is busy because of the is-complete
    ;; request. Some kernels don't respond to this request when the kernel is
    ;; busy.
    (unless (member (oref jupyter-repl-current-client execution-state)
                    '("starting" "idle"))
      (error "Kernel busy"))
    (if force (jupyter-execute-request jupyter-repl-current-client)
      (if (not jupyter-repl-use-builtin-is-complete)
          (let ((res (jupyter-wait-until-received
                         :is-complete-reply
                       (jupyter-is-complete-request
                           jupyter-repl-current-client
                         :code (jupyter-repl-cell-code))
                       jupyter-repl-maximum-is-complete-timeout)))
            (unless res
              (message "Kernel did not respond to is-complete-request, using built-in is-complete")
              (setq-local jupyter-repl-use-builtin-is-complete t)
              (jupyter-repl-ret force)))
        (goto-char (point-max))
        (let ((complete-p (equal (buffer-substring
                                  (line-beginning-position) (point))
                                 "")))
          (jupyter-handle-is-complete-reply
              jupyter-repl-current-client
            nil (if complete-p "complete" "incomplete") ""))))))

(defun jupyter-repl-indent-line ()
  "Indent the line according to the language of the REPL."
  (let* ((spos (jupyter-repl-cell-code-beginning-position))
         (pos (jupyter-repl-cell-code-position))
         (code (jupyter-repl-cell-code))
         (replacement (with-jupyter-repl-lang-buffer
                        (insert code)
                        (goto-char pos)
                        (indent-according-to-mode)
                        (setq pos (point))
                        (buffer-string))))
    ;; Don't modify the buffer when unnecessary, this allows
    ;; `company-indent-or-complete-common' to work.
    (unless (equal code replacement)
      (jupyter-repl-replace-cell-code replacement)
      (goto-char (+ pos spos)))))

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

;; FIXME: This is necessary due to some interaction with other packages (I
;; think). Sometimes the margins will disappear after the window configuration
;; changes which is why `window-configuration-change-hook' is not used.
(defun jupyter-repl-preserve-window-margins (&optional window)
  "Ensure that the margins of a REPL window are present.
This function is added as a hook to `pre-redisplay-functions' to
ensure that a REPL windows margins are present. If WINDOW is
showing a REPL buffer and the margins are not set to
`jupyter-repl-prompt-margin-width', set them to the proper
value."
  (when (and (eq major-mode 'jupyter-repl-mode)
             (let ((margins (window-margins window)))
               (not (and (consp margins)
                         (car margins)
                         (= (car margins) jupyter-repl-prompt-margin-width)))))
    (set-window-buffer window (current-buffer))))

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
                   ;; TODO: What about other operators like :: and ->, this
                   ;; most likely will depend on the kernel in use.
                   ;; `jupyter-repl-lang-mode' can be used here with some alist
                   ;; mapping modes to operators.
                   (looking-back "\\." 2))
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
    (let ((matches matches)
          ;; TODO: This may not be the most general way to use start and end
          (prefix (seq-subseq prefix 0 (- (length prefix)
                                          (- end start))))
          match)
      (while (setq match (car matches))
        ;; TODO: Maybe set the match property when it doesn't have the prefix,
        ;; indicating that it should replace part of the prefix?
        (unless (string-prefix-p prefix match)
          ;; FIXME: Note that prefix is not the code sent to the kernel in some
          ;; cases, but the symbol behind point
          (setcar matches (concat prefix (car matches))))
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
    (candidates
     (cons
      :async
      (lambda (cb)
        (let* ((ctx (jupyter-repl-code-context-at-point 'complete arg))
               (code (car ctx))
               (pos (cdr ctx)))
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
    (ignore-case t)
    (annotation (get-text-property 0 'annot arg))
    (doc-buffer (let* ((inhibit-read-only t)
                       (buf (jupyter-repl--inspect
                             arg (length arg) (company-doc-buffer)
                             company-async-timeout)))
                  (when buf
                    (with-current-buffer buf
                      (remove-text-properties
                       (point-max) (point-min) '(read-only)))
                    buf)))))

;;; Inspection

(defun jupyter-repl--inspect (code pos &optional buffer timeout)
  "Send an inspect request to a Jupyter kernel.
CODE and POS are the code to send and the position within the
code, respectively. TIMEOUT is how long to wait (in seconds) for
the kernel to respond. If the kernel does not respond within
TIMEOUT, return nil. Otherwise if a reply was received within
TIMEOUT, then return either the inspection text or BUFFER after
inserting the inspection text in BUFFER. In both cases, the
inspection text will already be in a form ready for display."
  (let* ((jupyter-inhibit-handlers t)
         (msg (jupyter-wait-until-received :inspect-reply
                (jupyter-inspect-request jupyter-repl-current-client
                  :code code :pos pos))
               timeout)))
    (when msg
      (cl-destructuring-bind (&key status found data &allow-other-keys)
          (jupyter-message-content msg)
        (when (and (equal status "ok") found)
          (if buffer (with-current-buffer buffer
                       (jupyter-repl-insert-data data)
                       buffer)
            (with-temp-buffer
              (jupyter-repl-insert-data data)
              (buffer-string))))))))

(defun jupyter-repl-inspect-at-point ()
  (interactive)
  (cl-destructuring-bind (code . pos)
      (jupyter-repl-code-context-at-point 'inspect)
    ;; Set the default value of the client for
    ;; `jupyter-jupyter-repl-doc-buffer'
    (let ((client jupyter-repl-current-client))
      (with-jupyter-repl-doc-buffer "inspect"
        ;; TODO: Cleanup how `jupyter-repl--inspect' works, make it more clear
        ;; that `current-buffer' here means to insert the inspected result in
        ;; the current buffer.
        ;;
        ;; TODO: Figure out why setting this outside
        ;; `with-jupyter-repl-doc-buffer' doesn't work. Possibly to do with
        ;; this variable being `permanent-local'?
        (let ((jupyter-repl-current-client client))
          (if (not (jupyter-repl--inspect code pos (current-buffer)))
              (message "Inspect timed out")
            ;; TODO: Customizable action
            (display-buffer (current-buffer))
            (set-window-start (get-buffer-window) (point-min))))))))

;;; Evaluation

(defun jupyter-repl-eval-string (str &optional silently)
  "Evaluate STR with the `jupyter-repl-current-client'.
The contents of the last cell in the REPL buffer will be replaced
with STR and the last cell executed with the
`juptyer-repl-current-client'. After execution, the execution
result is echoed to the *Message* buffer or a new buffer showing
the result is open if the result output is larger than 10 lines
long."
  (interactive)
  (unless (buffer-local-value
           'jupyter-repl-current-client (current-buffer))
    (user-error "No `jupyter-repl-current-client' set, see `jupyter-repl-associate-buffer'"))
  (with-jupyter-repl-buffer jupyter-repl-current-client
    (goto-char (point-max))
    (unless (= (save-excursion (jupyter-repl-previous-cell)) 0)
      (jupyter-repl-insert-prompt 'in))
    (setq str (strim-trim str))
    (let* ((code (if silently (string-trim str)
                   (prog1 nil
                     (jupyter-repl-replace-cell-code str))))
           (req (jupyter-execute-request jupyter-repl-current-client
                  :code code)))
      (setf (jupyter-request-run-handlers-p req) (not silently))
      (jupyter-add-callback req
        :error (lambda (msg)
                 (cl-destructuring-bind (&key ename evalue &allow-other-keys)
                     (jupyter-message-content msg)
                   (message "jupyter (%s): %s" ename
                            (xterm-color-filter evalue))))
        :execute-result
        (lambda (msg)
          (let ((res (jupyter-message-data msg :text/plain)))
            (when res
              (if (and (jupyter-repl-multiline-p res)
                       (cl-loop
                        with nlines = 0
                        for c across res when (eq c ?\n) do (cl-incf nlines)
                        thereis (> nlines 10)))
                  (with-current-buffer
                      (get-buffer-create "*jupyter-repl-result*")
                    (erase-buffer)
                    (insert res)
                    (goto-char (point-min))
                    (pop-to-buffer (current-buffer)))
                (if (equal res "") (message "jupyter: eval done")
                  (message res)))))))
      req)))

(defun jupyter-repl-eval-file (file)
  "Send the contents of FILE using `jupyter-repl-current-client'."
  (interactive
   (list (read-file-name "File name: " nil nil nil
                         (file-name-nondirectory
                          (or (buffer-file-name) "")))))
  (message "Evaluating %s..." file)
  (setq file (expand-file-name file))
  (if (file-exists-p file)
      (let ((buf (find-buffer-visiting file)))
        (jupyter-repl-eval-string
         (if buf (with-current-buffer buf
                   (buffer-string))
           (with-current-buffer (delay-mode-hooks (find-file-noselect file))
             (prog1 (buffer-string)
               (kill-buffer))))
         'silently))
    (error "Not a file (%s)" file)))

(defun jupyter-repl-eval-region (beg end &optional silently)
  "Evaluate a region with the `jupyter-repl-current-client'.
BEG and END are the beginning and end of the region to evaluate.
See `jupyter-repl-eval-string' for how the results of evaluation
are displayed."
  (interactive "r")
  (jupyter-repl-eval-string
   (buffer-substring-no-properties beg end) silently))

(defun jupyter-repl-eval-line-or-region ()
  "Evaluate the current line or region with the `jupyter-repl-current-client'.
If the current region is active send the current region using
`jupyter-repl-eval-region', otherwise send the current line."
  (interactive)
  (if (use-region-p)
      (jupyter-repl-eval-region (region-beginning) (region-end))
    (jupyter-repl-eval-region (line-beginning-position) (line-end-position))))

;;; Kernel management

(defun jupyter-repl-interrupt-kernel ()
  "Interrupt the kernel if possible.
A kernel can be interrupted if it was started using a
`jupyter-kernel-manager'. See `jupyter-start-new-kernel'."
  (interactive)
  (if jupyter-repl-kernel-manager
      (with-jupyter-repl-buffer jupyter-repl-current-client
        (message "Interrupting kernel")
        (jupyter-interrupt-kernel jupyter-repl-kernel-manager))
    (user-error "Cannot interrupt non-subprocess kernels")))

;; TODO: Make timeouts configurable
;; TODO: Handle all consequences of a shutdown
(defun jupyter-repl-restart-kernel (shutdown)
  "Restart the kernel.
With a prefix argument, SHUTDOWN the kernel completely instead."
  (interactive "P")
  (unless shutdown
    ;; Gets reset to default value in
    ;; `jupyter-repl-insert-prompt-when-starting'
    (jupyter-set
     jupyter-repl-current-client
     'jupyter-include-other-output
     (list (jupyter-get
            jupyter-repl-current-client
            'jupyter-include-other-output)))
    (setq-local jupyter-include-other-output
                (list jupyter-include-other-output))
    ;; This may have been set to t due to a non-responsive kernel so make sure
    ;; that we try again when restarting.
    (setq-local jupyter-repl-use-builtin-is-complete nil))
  (if jupyter-repl-kernel-manager
      (if (jupyter-kernel-alive-p jupyter-repl-kernel-manager)
          (progn
            (message "%s kernel..." (if shutdown "Shutting down" "Restarting"))
            (jupyter-shutdown-kernel jupyter-repl-kernel-manager (not shutdown)))
        (message "Starting dead kernel...")
        (jupyter-start-kernel jupyter-repl-kernel-manager))
    (when (null (jupyter-wait-until-received :shutdown-reply
                  (jupyter-shutdown-request jupyter-repl-current-client
                    (not shutdown))))
      (message "Kernel did not respond to shutdown request"))))

(defun jupyter-repl-display-kernel-buffer ()
  "Display the kernel processes stdout."
  (interactive)
  (if jupyter-repl-kernel-manager
      (display-buffer (process-buffer (oref jupyter-repl-kernel-manager kernel)))
    (user-error "Kernel not a subprocess")))

(defun jupyter-repl-restart-channels ()
  (interactive)
  (message "Restarting client channels...")
  (jupyter-stop-channels jupyter-repl-current-client)
  (jupyter-start-channels jupyter-repl-current-client))

;;; Isearch
;; Adapted from isearch in `comint', see `comint-history-isearch-search' for
;; details

(defun jupyter-repl-isearch-setup ()
  (setq-local isearch-search-fun-function
              #'jupyter-repl-history-isearch-search)
  (setq-local isearch-wrap-function
              #'jupyter-repl-history-isearch-wrap)
  (setq-local isearch-push-state-function
              #'jupyter-repl-history-isearch-push-state))

;; Adapted from `comint-history-isearch-search'
(defun jupyter-repl-history-isearch-search ()
  (lambda (string bound noerror)
    (let ((search-fun (isearch-search-fun-default)) found)
      (unless isearch-forward (goto-char (point-max)))
      (or
       ;; 1. First try searching in the initial cell text
       (funcall search-fun string
                (if isearch-forward bound
                  (jupyter-repl-cell-code-beginning-position))
                noerror)
       ;; 2. If the above search fails, start putting next/prev history
       ;; elements in the cell successively, and search the string in them. Do
       ;; this only when bound is nil (i.e. not while lazy-highlighting search
       ;; strings in the current cell text).
       (unless bound
         (condition-case err
             (progn
               (while (not found)
                 (cond (isearch-forward
                        ;; See the comment in
                        ;; `jupyter-repl-history-isearch-wrap'
                        (if (eq (ring-ref jupyter-repl-history -1)
                                'jupyter-repl-history)
                            (error "End of history")
                          (jupyter-repl-history-next))
                        (goto-char (jupyter-repl-cell-code-beginning-position)))
                       (t
                        (jupyter-repl-history-previous)
                        (goto-char (point-max))))
                 (setq isearch-barrier (point) isearch-opoint (point))
                 ;; After putting the next/prev history element, search the
                 ;; string in them again, until an error is thrown at the
                 ;; beginning/end of history.
                 (setq found (funcall search-fun string
                                      (unless isearch-forward
                                        (jupyter-repl-cell-code-beginning-position))
                                      noerror)))
               ;; Return point of the new search result
               (point))
           ;; Return nil on the error "no next/preceding item"
           (error nil)))))))

(defun jupyter-repl-history-isearch-wrap ()
  (condition-case nil
      (if isearch-forward
          (jupyter-repl-history-next (ring-length jupyter-repl-history) t)
        (jupyter-repl-history-previous (ring-length jupyter-repl-history) t))
    (error nil))
  (jupyter-repl-replace-cell-code (ring-ref jupyter-repl-history 0))
  (goto-char (if isearch-forward (jupyter-repl-cell-code-beginning-position)
               (point-max))))

(defun jupyter-repl-history-isearch-push-state ()
  (let ((elem (ring-ref jupyter-repl-history 0)))
    (lambda (_cmd)
      (while (not (eq (ring-ref jupyter-repl-history 0) elem))
        (if isearch-forward (jupyter-repl-history-next 1 t)
          (jupyter-repl-history-previous 1 t)))
      (jupyter-repl-replace-cell-code (ring-ref jupyter-repl-history 0)))))

;;; `jupyter-repl-mode'

(defvar jupyter-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" nil)
    (define-key map [remap backward-sentence] #'jupyter-repl-backward-cell)
    (define-key map [remap forward-sentence] #'jupyter-repl-forward-cell)
    (define-key map (kbd "RET") #'jupyter-repl-ret)
    (define-key map (kbd "C-n") #'jupyter-repl-history-next)
    (define-key map (kbd "C-p") #'jupyter-repl-history-previous)
    (define-key map (kbd "M-n") #'jupyter-repl-history-next)
    (define-key map (kbd "M-p") #'jupyter-repl-history-previous)
    map))

(put 'jupyter-repl-mode 'mode-class 'special)
(define-derived-mode jupyter-repl-mode fundamental-mode
  "Jupyter-REPL"
  "A major mode for interacting with a Jupyter kernel."
  (setq-local indent-line-function #'jupyter-repl-indent-line)
  (setq-local left-margin-width jupyter-repl-prompt-margin-width)
  (setq-local jupyter-repl-history
              (make-ring (1+ jupyter-repl-history-maximum-length)))
  ;; The sentinel value keeps track of the newest/oldest elements of
  ;; the history since next/previous navigation is implemented by
  ;; rotations on the ring.
  (ring-insert jupyter-repl-history 'jupyter-repl-history)
  (erase-buffer)
  (jupyter-repl-interaction-mode)
  (jupyter-repl-isearch-setup)
  (add-hook 'kill-buffer-query-functions #'jupyter-repl-kill-buffer-query-function nil t)
  (add-hook 'after-change-functions 'jupyter-repl-after-buffer-change nil t)
  (add-hook 'pre-redisplay-functions 'jupyter-repl-preserve-window-margins nil t))

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
  (jupyter-repl-without-continuation-prompts
   (let ((start (point)))
     (jupyter-repl-insert banner)
     (jupyter-repl-newline)
     (add-text-properties start (point) '(font-lock-face shadow fontified t)))))

(defun jupyter-repl-sync-execution-state ()
  "Synchronize the execution count of `jupyter-repl-current-client'.
Set the execution-count slot of `jupyter-repl-current-client' to
1+ the execution count of the client's kernel."
  (let* ((client jupyter-repl-current-client)
         (req (jupyter-execute-request client :code "" :silent t)))
    (setf (jupyter-request-run-handlers-p req) nil)
    (jupyter-add-callback req
      :status (lambda (msg)
                (oset client execution-state
                      (jupyter-message-get msg :execution_state)))
      :execute-reply (lambda (msg)
                       (oset client execution-count
                             (1+ (jupyter-message-get msg :execution_count)))))
    (jupyter-wait-until-idle req)))

;;; `jupyter-repl-interaction-mode'

(defun jupyter-repl-pop-to-buffer ()
  "Switch to the REPL buffer associated with `jupyter-repl-current-client'."
  (interactive)
  (with-jupyter-repl-buffer jupyter-repl-current-client
    (goto-char (point-max))
    (pop-to-buffer (current-buffer))))

(defun jupyter-repl-available-repl-buffers (&optional mode)
  "Get a list of REPL buffers that are connected to live kernels.
If MODE is non-nil, return REPL buffers connected to MODE's
language. MODE should be the `major-mode' used to edit files of
one of the Jupyter kernel languages."
  (delq nil (mapcar (lambda (b)
                 (with-current-buffer b
                   (and (eq major-mode 'jupyter-repl-mode)
                        (if mode (eq mode jupyter-repl-lang-mode) t)
                        (or (condition-case nil
                                ;; Check if the kernel is local
                                (jupyter-kernel-alive-p
                                 (oref jupyter-repl-current-client
                                       parent-instance))
                              (error nil))
                            (jupyter-hb-beating-p
                             (oref jupyter-repl-current-client hb-channel)))
                        (buffer-name b))))
               (buffer-list))))

;;;###autoload
(defun jupyter-repl-associate-buffer (client)
  "Associate the `current-buffer' with a REPL CLIENT.
The `current-buffer's `major-mode' must be the
`jupyter-repl-lang-mode' of the CLIENT. CLIENT can either be a
`jupyter-repl-client' or a buffer with a non-nil
`jupyter-repl-current-client'. The buffer-local value of
`jupyter-repl-current-client' in the `current-buffer' is set to
that of CLIENT."
  (interactive
   (list
    (completing-read
     "jupyter-repl: "
     (or (jupyter-repl-available-repl-buffers major-mode)
         (error "No live REPL for `current-buffer's `major-mode'"))
     nil t)))
  (setq client (if (or (bufferp client) (stringp client))
                   (with-current-buffer client
                     jupyter-repl-current-client)
                 client))
  (cl-check-type client jupyter-repl-client)
  (setq-local jupyter-repl-current-client client)
  (setq-local jupyter-repl-kernel-manager (with-jupyter-repl-buffer client
                                            jupyter-repl-kernel-manager))
  (jupyter-repl-interaction-mode))

(defvar jupyter-repl-interaction-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'jupyter-repl-eval-line-or-region)
    (define-key map (kbd "C-c C-f") #'jupyter-repl-inspect-at-point)
    (define-key map (kbd "C-c C-r") #'jupyter-repl-restart-kernel)
    (define-key map (kbd "C-c R") #'jupyter-repl-restart-channels)
    (define-key map (kbd "C-c C-i") #'jupyter-repl-interrupt-kernel)
    (define-key map (kbd "C-c C-z") #'jupyter-repl-pop-to-buffer)
    map))

(define-minor-mode jupyter-repl-interaction-mode
  "Minor mode for interacting with a Jupyter REPL."
  :group 'jupyter-repl
  :lighter " JuPy"
  :init-value nil
  :keymap jupyter-repl-interaction-map
  (if jupyter-repl-interaction-mode
      (when (boundp 'company-mode)
        (unless (cl-find-if
                 (lambda (x) (or (and (listp x) (memq 'company-jupyter-repl x))
                            (eq x 'company-jupyter-repl)))
                 company-backends)
          (setq-local company-backends
                      (cons 'company-jupyter-repl company-backends))))
    (when (boundp 'company-mode)
      (setq-local company-backends
                  (delq 'company-jupyter-repl company-backends)))))

;;;###autoload
(defun run-jupyter-repl (kernel-name &optional associate-buffer)
  "Run a Jupyter REPL connected to a kernel with name, KERNEL-NAME.
KERNEL-NAME can be the prefix of an available kernel name, in
this case the first kernel in `jupyter-available-kernelspecs'
that has KERNEL-NAME as a prefix will be used to start a new
kernel.

Optional argument ASSOCIATE-BUFFER, if non-nil, means to enable
`jupyter-repl-interaction-mode' in the `current-buffer' and
associate it with the REPL created. When called interactivel,
ASSOCIATE-BUFFER is set to t unless `current-prefix-arg' is
non-nil. If the `current-buffer's `major-mode' does not
correspond to the language of the kernel started,
ASSOCIATE-BUFFER has no effect."
  (interactive (list (jupyter-completing-read-kernelspec)
                     (not current-prefix-arg)))
  (message "Starting %s kernel..." kernel-name)
  (setq kernel-name
        (or (and (called-interactively-p 'interactive)
                 (car kernel-name))
            (or (car (jupyter-find-kernelspec kernel-name))
                (error "No kernel found for prefix (%s)" kernel-name))))
  (cl-destructuring-bind (km . kc)
      (jupyter-start-new-kernel kernel-name 'jupyter-repl-client)
    (oset kc buffer (generate-new-buffer
                     (format "*jupyter-repl[%s]*" (oref km name))))
    (with-jupyter-repl-buffer kc
      (cl-destructuring-bind (&key language_info banner &allow-other-keys)
          (oref km info)
        (cl-destructuring-bind (&key name file_extension &allow-other-keys)
            language_info
          (jupyter-repl-mode)
          (jupyter-set kc 'jupyter-include-other-output t)
          (setq-local jupyter-repl-current-client kc)
          (setq-local jupyter-repl-kernel-manager km)
          (setq-local jupyter-repl-lang-buffer
                      (get-buffer-create
                       (format " *jupyter-repl-lang-%s*" name)))
          (let (mode syntax)
            (with-jupyter-repl-lang-buffer
              (let ((buffer-file-name
                     (concat "jupyter-repl-lang" file_extension)))
                (set-auto-mode)
                (setq mode major-mode)
                (setq syntax (syntax-table))))
            (setq-local jupyter-repl-lang-mode mode)
            (set-syntax-table syntax))
          (jupyter-history-request kc :n 100 :raw nil :unique t)
          (jupyter-repl-initialize-fontification)
          (jupyter-repl-insert-banner banner)
          (jupyter-repl-sync-execution-state)
          (jupyter-repl-insert-prompt 'in))))
    (when (and associate-buffer
               (memq (oref kc buffer) (jupyter-repl-available-repl-buffers
                                       major-mode)))
      (jupyter-repl-associate-buffer kc))
    (pop-to-buffer (oref kc buffer))))

(provide 'jupyter-repl-client)

;;; jupyter-repl-client.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
