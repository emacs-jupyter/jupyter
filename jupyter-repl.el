;;; jupyter-repl-client.el --- A Jupyter REPL client -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 08 Jan 2018
;; Version: 0.0.1

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

;; A Jupyter REPL for Emacs.
;;
;; The main entry points are `run-jupyter-repl' and `connect-jupyter-repl'.
;;
;; When called interactively, `run-jupyter-repl' asks for a kernel to start
;; (based on the kernels found using `jupyter-available-kernelspecs'), connects
;; a `jupyter-repl-client' to the selected kernel, and pops up a REPL buffer.
;; On the other hand, if `connect-jupyter-repl' is called interactively, it
;; will ask for the JSON file that contains the kernel's connection info.
;;
;; Additionally, `jupyter-repl-associate-buffer' associates the
;; `current-buffer' with a REPL client appropriate for the buffer's
;; `major-mode'. Associating a buffer with a REPL client enables the minor mode
;; `jupyter-repl-interaction-mode' and, if `company-mode' is installed, enables
;; auto-completion using the associated REPL client.
;;
;; `jupyter-repl-interaction-mode' adds the following keybindings for
;; interacing a REPL client:
;;
;;     C-c C-c `jupyter-repl-eval-line-or-region'
;;     C-c C-l `jupyter-repl-eval-file'
;;     C-c C-f `jupyter-repl-inspect-at-point'
;;     C-c C-r `jupyter-repl-restart-kernel'
;;     C-c C-i `jupyter-repl-interrupt-kernel'
;;     C-c C-z `jupyter-repl-pop-to-buffer'

;;; Code:

(defgroup jupyter-repl nil
  "A Jupyter REPL client"
  :group 'jupyter)

(require 'jupyter-base)
(require 'jupyter-client)
(require 'jupyter-widget-client)
(require 'jupyter-kernel-manager)
(require 'shr)
(require 'ring)

(declare-function company-begin-backend "company" (backend &optional callback))
(declare-function company-doc-buffer "company" (&optional string))
(declare-function org-format-latex "org" (prefix &optional beg end dir overlays msg forbuffer processing-type))

;; TODO: Read up on how method tags can be used, see
;; https://ericabrahamsen.net/tech/2016/feb/bbdb-eieio-object-oriented-elisp.html

;; TODO: Fallbacks for when the language doesn't have a major mode installed.

;; TODO: Define `jupyter-kernel-manager-after-restart-hook' to update the
;; execution count after a restart. More generally, define more ways to hook
;; into differnt events of the client/kernel interaction.

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
  :type 'integer
  :group 'jupyter-repl)

(defcustom jupyter-repl-maximum-is-complete-timeout 2
  "Maximum number of seconds to wait for an is-complete reply.
When no is-complete reply is received from the kernel within this
timeout, the built-in is-complete handler is used."
  :type 'integer
  :group 'jupyter-repl)

(defcustom jupyter-repl-history-maximum-length 100
  "The maximum number of history elements to keep track of."
  :type 'integer
  :group 'jupyter-repl)

(defcustom jupyter-repl-prompt-margin-width 12
  "The width of the margin which displays prompt strings."
  :type 'integer
  :group 'jupyter-repl)

;;; Implementation

(defclass jupyter-repl-client (jupyter-widget-client)
  ((buffer :type buffer)
   (wait-to-clear
    :type boolean :initform nil
    :documentation "Whether or not we should wait to clear the
current output of the cell. Set when the kernel sends a
`:clear-output' message.")
   (kernel-info :type json-plist :initform nil)
   (execution-state :type string :initform "idle")
   (execution-count :type integer :initform 1)))

(defvar jupyter-repl-lang-buffer nil
  "A buffer with the `major-mode' set to the REPL language's `major-mode'.")

(defvar jupyter-repl-current-client nil
  "The `jupyter-repl-client' for the `current-buffer'.")
(put 'jupyter-repl-current-client 'permanent-local t)

(defvar jupyter-repl-lang-mode nil
  "The `major-mode' corresponding to the kernel's language.")

(defvar jupyter-repl-history nil
  "The history of the current Jupyter REPL.")

;; TODO: Proper cleanup of these buffers when done with a client
(defvar jupyter-repl-fontify-buffers nil
  "An alist of (MODE . BUFFER) pairs used for fontification.
See `jupyter-repl-fontify-according-to-mode'.")

(defvar jupyter-repl-use-builtin-is-complete nil
  "Whether or not to send is_complete_request's to a kernel.
If a Jupyter kernel does not respond to an is_complete_request,
the buffer local value of this variable is set to t and code in a
cell is considered complete if the last line in a code cell is a
blank line, i.e. if RET is pressed twice in a row.")

(defvar jupyter-repl-display-ids nil
  "A hash table of display IDs.
Display IDs are implemented by setting the text property,
`jupyter-display', to the display ID requested by a
`:display-data' message. When a display is updated from an
`:update-display-data' message, the display ID from the initial
`:display-data' is retrieved from this table and used to find the
display in the REPL buffer. See `jupyter-repl-update-display'.")

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
  "Switch to CLIENT's buffer, move to the end of REQ, and run BODY.
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
`jupyter-repl-cell-code-beginning-position' and
`jupyter-repl-cell-code-end-position'. When BODY is run, `point' will
be at the `jupyter-repl-cell-code-beginning-position'. Note that
this assumes that the `current-buffer' is a Jupyter REPL buffer."
  (declare (indent 0) (debug (&rest form)))
  `(save-excursion
     (save-restriction
       (narrow-to-region (jupyter-repl-cell-code-beginning-position)
                         (jupyter-repl-cell-code-end-position))
       (goto-char (jupyter-repl-cell-code-beginning-position))
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

;;; Convenience functions

(defsubst jupyter-repl-language-mode (client)
  "Get the `major-mode' of CLIENT's kernel language."
  (with-jupyter-repl-buffer client
    jupyter-repl-lang-mode))

(cl-defmethod jupyter-repl-language ((client jupyter-repl-client))
  (plist-get (plist-get (oref client kernel-info) :language_info) :name))

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
substituting any `face' property with `font-lock-face' for
insertion into the REPL buffer and adding
`font-lock-extra-managed-props' to the text."
  (let ((start (point-min))
        (pos (point-min)) next)
    (catch 'done
      (while (setq next (or (next-property-change pos) (point-max)))
        ;; Handle additional properties from font-lock, so as to
        ;; preserve, e.g., composition.
        (dolist (prop (cons 'face font-lock-extra-managed-props))
          (let ((new-prop (get-text-property pos prop)))
            (put-text-property
             (+ start (1- pos)) (1- (+ start next))
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
      (fill-region (point-min) (point-max) t 'nosqueeze))
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

;;; Handling rich output

(defvar jupyter-repl-graphic-mimetypes '(:image/png :image/svg+xml :text/latex)
  "Mimetypes that display graphics in the REPL buffer.")

(defun jupyter-repl-graphic-data-p (msg)
  "Check to see if MSG has mimetypes for graphics."
  (cl-loop
   with graphic-types = jupyter-repl-graphic-mimetypes
   for (mimetype _value) on (jupyter-message-get msg :data) by #'cddr
   thereis (memq mimetype graphic-types)))

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

;; Markdown integration

(defvar markdown-hide-markup)
(defvar markdown-hide-urls)
(defvar markdown-fontify-code-blocks-natively)

(defvar jupyter-repl-markdown-mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'jupyter-repl-markdown-follow-link-at-point)
    (define-key map [follow-link] 'mouse-face)
    (define-key map [mouse-2] 'jupyter-repl-markdown-follow-link-at-point)
    map))

(defun jupyter-repl-markdown-follow-link-at-point ()
  "Handle markdown links specially."
  (interactive)
  (let ((link (markdown-link-at-pos (point))))
    ;; TODO: How to generalize this to kernels which do not utilize markdown in
    ;; their docstrings? Maybe if you do M-RET on a symbol it will call the
    ;; help function on it. For example in the python kernel, you can pull up
    ;; help on a symbol by calling the help function on it or appending a
    ;; question mark at the end of the symbol.
    (if (and (string= (nth 3 link) "@ref")
             (eq jupyter-repl-lang-mode 'julia-mode))
        ;; Links have the form `fun`
        (let ((fun (substring (nth 2 link) 1 -1)))
          (goto-char (point-max))
          (jupyter-repl-replace-cell-code (concat "?" fun))
          (jupyter-repl-ret))
      (markdown-follow-link-at-point))))

(defun jupyter-repl-insert-markdown (text)
  "Insert TEXT, fontifying it using `markdown-mode' first."
  (let ((pos (point)))
    (jupyter-repl-insert
     (let ((markdown-hide-markup t)
           (markdown-hide-urls t)
           (markdown-fontify-code-blocks-natively t))
       (jupyter-repl-fontify-according-to-mode 'markdown-mode text)))
    ;; Update keymaps
    (let ((limit (point)) next)
      (setq pos (next-single-property-change pos 'keymap nil limit))
      (while (/= pos limit)
        (setq next (next-single-property-change pos 'keymap nil limit))
        (when (eq (get-text-property pos 'keymap) markdown-mode-mouse-map)
          (put-text-property pos next 'keymap jupyter-repl-markdown-mouse-map))
        (setq pos next)))))

(defvar org-format-latex-options)

(defun jupyter-repl-insert-latex (tex)
  "Generate and insert a LaTeX image based on TEX.

Note that this uses `org-format-latex' to generate the LaTeX
image."
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
     ;; FIXME: Possibly make a resource directory for the REPL
     "ltx" beg end org-babel-jupyter-resource-directory
     'overlays "Creating LaTeX image...%s"
     'forbuffer
     ;; Use the default method for creating image files
     org-preview-latex-default-process)
    (goto-char end)))

(defun jupyter-repl-insert-ansi-coded-text (text)
  "Insert TEXT, converting ANSI color codes to font lock faces."
  (setq text (ansi-color-apply text))
  (jupyter-repl-add-font-lock-properties 0 (length text) text)
  ;; NOTE: Mark text with a specific syntax class so that string characters do
  ;; not get registered as strings. This requires
  ;; `parse-sexp-lookup-properties' to be non-nil.
  (add-text-properties 0 (length text) '(syntax-table (3)) text)
  (jupyter-repl-insert text))

(defun jupyter-repl-insert-data (data metadata)
  "Insert DATA into the REPL buffer in order of decreasing richness.
DATA should be plist mapping mimetypes to their content. Attempt
to insert a recognized mimetype, trying each one in order of
decreasing richness of the mimetype. The current order is

- text/html
- text/markdown (only if `markdown-mode' is available)
- text/latex
- image/png
- image/svg+xml
- text/plain

When no valid mimetype is present in DATA, a warning is shown.

METADATA is a plist similar to data, but with values describing
extra information for inserting each kind of mimetype. For
example the value of `image/png' can be a plist with the keys
`:width', `:height'."
  (let ((mimetypes (cl-loop
                    for (k d) on data by #'cddr
                    when
                    (and d (not (equal d ""))
                         (or (display-graphic-p)
                             (not (memq k jupyter-repl-graphic-mimetypes))))
                    collect k)))
    (cond
     ((and (memq :text/html mimetypes)
           (functionp 'libxml-parse-html-region))
      (let ((html (plist-get data :text/html)))
        (when (string-match-p "^<img" html)
          (jupyter-repl-newline))
        (jupyter-repl-insert-html html)
        (jupyter-repl-newline)))
     ((and (memq :text/markdown mimetypes)
           (require 'markdown-mode nil t))
      (jupyter-repl-insert-markdown (plist-get data :text/markdown)))
     ((and (memq :text/latex mimetypes)
           (require 'org nil t))
      (jupyter-repl-insert-latex (plist-get data :text/latex))
      (jupyter-repl-newline))
     ((memq :image/png mimetypes)
      (cl-destructuring-bind (&key width height)
          (plist-get metadata :image/png)
        (let* ((data (base64-decode-string (plist-get data :image/png)))
               (img (create-image data nil 'data :width width :height height)))
          (insert-image img (propertize " " 'read-only t)))))
     ((and (memq :image/svg+xml mimetypes)
           (image-type-available-p 'svg))
      (cl-destructuring-bind (&key width height)
          (plist-get metadata :image/svg+xml)
        (let* ((data (plist-get data :image/svg+xml))
               (img (create-image data 'svg nil :width width :height height)))
          (insert-image img (propertize " " 'read-only t)))))
     ((memq :text/plain mimetypes)
      (jupyter-repl-insert-ansi-coded-text
       (plist-get data :text/plain))
      (jupyter-repl-newline))
     (t (warn "No supported mimetype found %s" mimetypes)))))

(defun jupyter-repl-insert-data-with-id (display-id data metadata)
  "Associate DISPLAY-ID with DATA when inserting DATA.
DATA and METADATA have the same meaning as in
`jupyter-repl-insert-data'."
  (unless jupyter-repl-display-ids
    (setq-local jupyter-repl-display-ids
                (make-hash-table :test #'equal
                                 :weakness 'value)))
  (let ((beg (point))
        (id (gethash display-id jupyter-repl-display-ids)))
    (or id (setq id (puthash display-id
                             display-id
                             jupyter-repl-display-ids)))
    (jupyter-repl-insert-data data metadata)
    (put-text-property beg (point) 'jupyter-display id)))

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
  (jupyter-repl-without-continuation-prompts
   (let ((inhibit-read-only t)
         ov props)
     (cond
      ((eq type 'in)
       (let ((count (oref jupyter-repl-current-client execution-count)))
         (setq ov (jupyter-repl--insert-prompt
                   (format "In [%d]:" count) 'jupyter-repl-input-prompt)
               props (list 'jupyter-cell (list 'beginning count))))
       ;; Insertion of an invisible character is to prevent the prompt overlay
       ;; from inheriting the text properties of code at the beginning of a
       ;; cell similarly for the output prompt.
       ;;
       ;; The front-sticky property is so that `point' will not get trapped in
       ;; the middle of the newline inserted by `jupyter-repl--insert-prompt'
       ;; and the invisible character.
       ;;
       ;; Finally the field property is so that text motions will stop at the
       ;; start of the code for a cell instead of moving past this invisible
       ;; character.
       (jupyter-repl-insert
        :properties '(invisible t rear-nonsticky t front-sticky t field t) " "))
      ((eq type 'out)
       ;; Output is normally inserted by first going to the end of the output
       ;; for the request. The end of the ouput for a request is at the
       ;; beginning of the next cell after the request which is why `escape' is
       ;; needed here.
       (let ((count (jupyter-repl-cell-count 'escape)))
         (setq ov (jupyter-repl--insert-prompt
                   (format "Out [%d]:" count) 'jupyter-repl-output-prompt)
               props (list 'jupyter-cell (list 'out count))))
       ;; Prevent the overlay from inheriting text properties. Front sticky to
       ;; prevent inserting text at the beginning of the output cell before the
       ;; insivible character.
       (jupyter-repl-insert
        :properties '(invisible t front-sticky t) " "))
      ((eq type 'continuation)
       (setq ov (jupyter-repl--insert-prompt
                 ":" 'jupyter-repl-input-prompt)
             props (list 'read-only nil 'rear-nonsticky t))))
     (add-text-properties (overlay-start ov) (overlay-end ov) props))))

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
        (if (jupyter-repl-cell-beginning-p (point-min))
            (setq pos (point-min))
          (signal 'beginning-of-buffer nil))))
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

   `jupyter-repl-cell-beginning-position' + 2

There is an extra invisible character after the prompt."
  (+ (jupyter-repl-cell-beginning-position) 2))

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
  "Go to the start of the current or previous cell.
If `point' is already at the start of the current cell, go to the
start of the previous cell. Otherwise go to the start of the
current cell. Optional argument N is the number of times to move
to the previous cell. N defaults to 1."
  (or N (setq N 1))
  (catch 'done
    (while (> N 0)
      (let ((pos (previous-single-property-change (point) 'jupyter-cell)))
        (while (and pos (not (jupyter-repl-cell-beginning-p pos)))
          (setq pos (previous-single-property-change pos 'jupyter-cell)))
        (unless (when pos (goto-char pos) (setq N (1- N)))
          (goto-char (point-min))
          ;; Handle edge case when the first cell is at the beginning of the
          ;; buffer. This happens, for example, when erasing the buffer.
          (when (jupyter-repl-cell-beginning-p (point))
            (setq N (1- N)))
          (throw 'done t)))))
  N)

(defun jupyter-repl-goto-cell (req)
  "Go to the cell beginning position of REQ.
REQ should be a `jupyter-request' that corresponds to one of the
`jupyter-send-execute-request's created by a cell in the
`current-buffer'. Note that the `current-buffer' is assumed to be
a Jupyter REPL buffer."
  (goto-char (point-max))
  (unless (catch 'done
            (while (= (jupyter-repl-previous-cell) 0)
              (when (eq (jupyter-repl-cell-request) req)
                (throw 'done t))))
    (error "Cell for request not found")))

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
  ;; Ignore the case when `point' is in the output of a cell, in this case
  ;; `jupyter-repl-previous-cell' will go to the previous cell.
  (ignore-errors (goto-char (jupyter-repl-cell-beginning-position)))
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
  (let ((pos (point)))
    (ignore-errors
      (save-excursion
        (unless (= pos (jupyter-repl-cell-beginning-position))
          (jupyter-repl-previous-cell))
        (<= (jupyter-repl-cell-code-beginning-position)
            pos
            (jupyter-repl-cell-code-end-position))))))

(defun jupyter-repl-cell-finalized-p ()
  "Has the current cell been finalized?
A cell is considered finalized when `jupyter-repl-finalize-cell'
has been previously called for it. `jupyter-repl-finalize-cell'
is responsible for adding the text properties which cause
`jupyter-repl-cell-end-p' to return non-nil."
  (jupyter-repl-cell-end-p (jupyter-repl-cell-end-position)))

(defun jupyter-repl-client-has-manager-p ()
  "Does the `jupyter-repl-current-client' have a `jupyter-kernel-manager'?
Checks to see if the REPL client of the `current-buffer' has a
kernel manager as its manager slot."
  (and jupyter-repl-current-client
       (oref jupyter-repl-current-client manager)))

(defun jupyter-repl-connected-p ()
  "Determine if the `jupyter-repl-current-client' is connected to its kernel."
  (when jupyter-repl-current-client
    (or (and (jupyter-repl-client-has-manager-p)
             ;; Check if the kernel is local
             (jupyter-kernel-alive-p
              (oref jupyter-repl-current-client manager)))
        (let ((hb (oref jupyter-repl-current-client hb-channel)))
          (and (jupyter-channel-alive-p hb)
               (jupyter-hb-beating-p hb))))))

;;; Buffer text manipulation

(defun jupyter-repl-cell-code ()
  "Get the code of the current cell."
  (if (= (point-min) (point-max)) ""
    (let (lines)
      (save-excursion
        (goto-char (jupyter-repl-cell-code-beginning-position))
        (push (buffer-substring-no-properties (point) (point-at-eol))
              lines)
        (while (and (line-move-1 1 'noerror)
                    (jupyter-repl-cell-line-p))
          (push (buffer-substring-no-properties (point-at-bol) (point-at-eol)) lines))
        (mapconcat #'identity (nreverse lines) "\n")))))

(defun jupyter-repl-cell-code-position ()
  "Get the position that `point' is at relative to the contents of the cell.
The first character of the cell code corresponds to position 1."
  (unless (jupyter-repl-cell-line-p)
    (error "Not in code of cell"))
  (1+ (- (point) (jupyter-repl-cell-code-beginning-position))))

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
    (goto-char (point-max))
    (jupyter-repl-newline)
    (put-text-property (1- (point)) (point) 'jupyter-cell `(end ,count))
    (put-text-property beg (1+ beg) 'jupyter-request req)
    ;; Remove this property so that text can't be inserted at the start of the
    ;; cell or after any continuation prompts. See
    ;; `jupyter-repl-insert-prompt'.
    (remove-text-properties beg (point) '(rear-nonsticky))
    ;; font-lock-multiline to avoid improper syntactic elements from
    ;; spilling over to the rest of the buffer.
    ;; TODO: I don't think this is a proper use of this text property
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

(cl-defmethod jupyter-send-execute-request ((client jupyter-repl-client)
                                            &key code
                                            (silent nil)
                                            (store-history t)
                                            (user-expressions nil)
                                            (allow-stdin t)
                                            (stop-on-error nil))
  (with-jupyter-repl-buffer client
    (jupyter-repl-truncate-buffer)
    (if code (cl-call-next-method)
      (setq code (string-trim (jupyter-repl-cell-code)))
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

(defun jupyter-repl--handle-payload (payload)
  "Do the client actions in PAYLOAD."
  (cl-loop
   for pl across payload
   do (pcase (plist-get pl :source)
        ("page"
         (let ((text (plist-get (plist-get pl :data) :text/plain))
               (line (or (plist-get pl :start) 0)))
           (with-jupyter-repl-doc-buffer "pager"
             (jupyter-repl-insert-ansi-coded-text text)
             (goto-char (point-min))
             (forward-line line)
             (display-buffer (current-buffer)))))
        ((or "edit" "edit_magic")
         (with-current-buffer (find-file-other-window
                               (plist-get pl :filename))
           (forward-line (plist-get pl :line_number))
           (set-window-start (selected-window) (point))))
        ("set_next_input"
         (goto-char (point-max))
         (jupyter-repl-previous-cell)
         (jupyter-repl-replace-cell-code (plist-get pl :text))))))

(cl-defmethod jupyter-handle-execute-reply ((client jupyter-repl-client)
                                            req
                                            execution-count
                                            _user-expressions
                                            payload)
  (oset client execution-count (1+ execution-count))
  (with-jupyter-repl-buffer client
    (save-excursion
      (jupyter-repl-goto-cell req)
      (jupyter-repl-cell-unmark-busy))
    (when payload
      (jupyter-repl--handle-payload payload))))

(cl-defmethod jupyter-handle-execute-input ((client jupyter-repl-client)
                                            _req
                                            _code
                                            execution-count)
  (oset client execution-count (1+ execution-count)))

(cl-defmethod jupyter-handle-execute-result ((client jupyter-repl-client)
                                             req
                                             _execution-count
                                             data
                                             metadata)
  ;; Only handle our results
  (when req
    (jupyter-repl-do-at-request client req
      (jupyter-repl-insert-prompt 'out)
      (jupyter-repl-insert-data data metadata))))

(defun jupyter-repl-next-display-with-id (id)
  "Move `point' to the start of the next display matching ID.
Return non-nil if successful. If no display with ID is found,
return nil without moving `point'."
  (let ((pos (next-single-property-change (point) 'jupyter-display)))
    (while (and pos (not (eq (get-text-property pos 'jupyter-display) id)))
      (setq pos (next-single-property-change pos 'jupyter-display)))
    (and pos (goto-char pos))))

(defun jupyter-repl-update-display (id data metadata)
  "Update the display with ID using DATA.
DATA and METADATA have the same meaning as in a `:display-data'
message.

Updating a display involves finding and clearing the data that is
currently associated with the ID and inserting DATA at the same
location. If multiple locations have the same display ID, all of
them are updated. Raise an error if no display with ID could be
found."
  (save-excursion
    (goto-char (point-min))
    (let (str)
      (while (jupyter-repl-next-display-with-id id)
        (or str (setq str (with-temp-buffer
                            (jupyter-repl-insert-data data metadata)
                            (put-text-property
                             (point-min) (point-max) 'jupyter-display id)
                            (buffer-string))))
        (delete-region (point) (next-single-property-change
                                (point) 'jupyter-display))
        (let ((beg (point)) ov)
          (insert str)
          (setq ov (make-overlay (1+ beg) (point)))
          (overlay-put ov 'face 'secondary-selection)
          (run-at-time 0.3 nil (lambda () (delete-overlay ov)))))
      (when (= (point) (point-min))
        (error "No display matching id (%s)" id)))))

;; NOTE: Info on display_id
;; https://github.com/jupyter/jupyter_client/issues/209
(cl-defmethod jupyter-handle-display-data ((client jupyter-repl-client)
                                           req
                                           data
                                           metadata
                                           transient)
  (let ((clear (prog1 (oref client wait-to-clear)
                 (oset client wait-to-clear nil)))
        widget)
    (cond
     ((setq widget (plist-get data :application/vnd.jupyter.widget-view+json))
      (jupyter-widgets-display-model client (plist-get widget :model_id)))
     ;; ((eq (jupyter-message-parent-message-type
     ;;       (jupyter-request-last-message req))
     ;;      :comm-msg)
     ;;  (with-current-buffer (get-buffer-create "*jupyter-repl-output*")
     ;;    (when clear (erase-buffer))
     ;;    (jupyter-repl-insert-data data)
     ;;    (pop-to-buffer (current-buffer))))
     (t
      (let ((req (if (eq (jupyter-message-parent-type
                          (jupyter-request-last-message req))
                         :comm-msg)
                     ;; For comm messages which produce a display_data, the
                     ;; request is assumed to be the most recently completed
                     ;; one.
                     ;;
                     ;; TODO: Handle display_id, display_id is supposed to be
                     ;; used such that any individual output produced by a cell
                     ;; can be referenced whereas the output of the whole cell
                     ;; is referenced by the request msg_id.
                     (with-jupyter-repl-buffer client
                       (save-excursion
                         (goto-char (point-max))
                         (jupyter-repl-previous-cell 2)
                         (jupyter-repl-cell-request)))
                   req)))
        (jupyter-repl-do-at-request client req
          (cl-destructuring-bind (&key display_id &allow-other-keys)
              transient
            (if display_id
                (jupyter-repl-insert-data-with-id display_id data metadata)
              (let ((inhibit-redisplay t))
                (when clear (jupyter-repl-clear-last-cell-output client))
                (jupyter-repl-insert-data data metadata)
                ;; Prevent slight flickering of prompt margin and text, this is
                ;; needed in addition to `inhibit-redisplay'. It also seems
                ;; that it can be placed anywhere within this let and it will
                ;; prevent flickering.
                (sit-for 0.1 t))))))))))

(cl-defmethod jupyter-handle-update-display-data ((client jupyter-repl-client)
                                                  _req
                                                  data
                                                  metadata
                                                  transient)
  (cl-destructuring-bind (&key display_id &allow-other-keys)
      transient
    (unless display_id
      (error "No display ID in `:update-display-data' message"))
    (with-jupyter-repl-buffer client
      (let ((id (gethash display_id jupyter-repl-display-ids)))
        (unless id
          (error "Display ID not found (%s)" id))
        (jupyter-repl-update-display id data metadata)))))

(defun jupyter-repl-clear-last-cell-output (client)
  "In CLIENT's REPL buffer, clear the output of the last completed cell."
  (with-jupyter-repl-buffer client
    (goto-char (point-max))
    (jupyter-repl-previous-cell 2)
    (delete-region (1+ (jupyter-repl-cell-end-position))
                   (progn
                     (jupyter-repl-next-cell)
                     (point)))))

(cl-defmethod jupyter-handle-clear-output ((client jupyter-repl-client)
                                           req
                                           wait)
  ;; TODO: Tale into account json-false elsewhere
  (unless (oset client wait-to-clear (eq wait t))
    (cond
     ((eq (jupyter-message-parent-type
           (jupyter-request-last-message req))
          :comm-msg)
      (with-current-buffer (get-buffer-create "*jupyter-repl-output*")
        (erase-buffer)))
     (t
      (jupyter-repl-clear-last-cell-output client)))))

(cl-defmethod jupyter-handle-status ((client jupyter-repl-client) _req execution-state)
  (oset client execution-state execution-state))

(defvar jupyter-repl--output-marker nil)

(defun jupyter-repl-display-other-output (client stream text)
  "Display output not originating from CLIENT.
STREAM is the name of a stream which will be used to select the
buffer to display TEXT."
  (let* ((bname (buffer-name (oref client buffer)))
         (inhibit-read-only t)
         (stream-buffer
          (concat (substring bname 0 (1- (length bname)))
                  "-" stream "*")))
    (with-current-buffer (get-buffer-create stream-buffer)
      (unless jupyter-repl--output-marker
        (setq-local jupyter-repl--output-marker (set-marker (make-marker) (point-max))))
      (goto-char jupyter-repl--output-marker)
      (let ((pos (point)))
        (jupyter-repl-insert-ansi-coded-text text)
        (fill-region pos (point)))
      (set-marker jupyter-repl--output-marker (point))
      (display-buffer (current-buffer) '(display-buffer-pop-up-window
                                         (pop-up-windows . t))))))

(cl-defmethod jupyter-handle-stream ((client jupyter-repl-client) req name text)
  (if (null req)
      ;; Otherwise the stream request is due to someone else, pop up a buffer.
      ;; TODO: Make this configurable so that we can just ignore output.
      (jupyter-repl-display-other-output client name text)
    (cond
     ((eq (jupyter-message-parent-type
           (jupyter-request-last-message req))
          :comm-msg)
      (with-current-buffer (get-buffer-create "*jupyter-repl-output*")
        (jupyter-repl-insert-ansi-coded-text text)))
     (t
      (jupyter-repl-do-at-request client req
        (jupyter-repl-insert-ansi-coded-text text))))))

(defun jupyter-repl-fix-python-traceback-spacing (ename)
  "Add spacing between the first occurance of ENAME and \"Traceback\".
Do this for the current cell."
  (save-excursion
    (jupyter-repl-previous-cell)
    (when (and (search-forward ename nil t)
               (looking-at "Traceback"))
      (let ((len (- fill-column
                    jupyter-repl-prompt-margin-width
                    (- (point) (line-beginning-position))
                    (- (line-end-position) (point)))))
        (jupyter-repl-insert
         (make-string (if (> len 4) len 4) ? ))))))

(cl-defmethod jupyter-handle-error ((client jupyter-repl-client)
                                    req ename evalue traceback)
  (when req
    (setq traceback (concat (mapconcat #'identity traceback "\n") "\n"))
    (cond
     ((eq (jupyter-message-parent-type
           (jupyter-request-last-message req))
          :comm-msg)
      (with-jupyter-repl-doc-buffer "traceback"
        (jupyter-repl-insert-ansi-coded-text traceback)
        (pop-to-buffer (current-buffer))))
     (t
      (jupyter-repl-do-at-request client req
        (jupyter-repl-insert-ansi-coded-text traceback)
        (when (equal (jupyter-repl-language client) "python")
          (jupyter-repl-fix-python-traceback-spacing ename)))))))

(cl-defmethod jupyter-handle-input-reply ((client jupyter-repl-client) req prompt _password)
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
    (when (cl-loop
           repeat n
           thereis (eq (ring-ref jupyter-repl-history -1) 'jupyter-repl-history)
           do (ring-insert
               jupyter-repl-history (ring-remove jupyter-repl-history -1)))
      ;; When the next history element is the sentinel, handle some edge cases
      (cond
       ((equal (jupyter-repl-cell-code)
               (ring-ref jupyter-repl-history 0))
        ;; If the cell code is the last history item, erase it
        (jupyter-repl-replace-cell-code "")
        (setq no-replace t))
       ((equal (jupyter-repl-cell-code) "")
        (error "End of history"))))
    (unless no-replace
      (jupyter-repl-replace-cell-code
       (ring-ref jupyter-repl-history 0)))))

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
    (when (not (equal (jupyter-repl-cell-code)
                      (ring-ref jupyter-repl-history 0)))
      (setq n (1- n)))
    (if (or (and (= n 0) (eq (ring-ref jupyter-repl-history 0) 'jupyter-repl-history))
            (cl-loop
             repeat n
             thereis (eq (ring-ref jupyter-repl-history 1) 'jupyter-repl-history)
             do (ring-insert-at-beginning
                 jupyter-repl-history (ring-remove jupyter-repl-history 0))))
        (error "Beginning of history")
      (unless no-replace
        (jupyter-repl-replace-cell-code
         (ring-ref jupyter-repl-history 0))))))

(cl-defmethod jupyter-handle-history-reply ((client jupyter-repl-client) _req history)
  (with-jupyter-repl-buffer client
    (cl-loop for elem across history
             for input-output = (aref elem 2)
             do (ring-remove+insert+extend jupyter-repl-history input-output))))

(cl-defmethod jupyter-handle-is-complete-reply ((client jupyter-repl-client) _req status indent)
  (with-jupyter-repl-buffer client
    (pcase status
      ("complete"
       (jupyter-send-execute-request client))
      ("incomplete"
       (jupyter-repl-newline)
       (if (= (length indent) 0) (jupyter-repl-indent-line)
         (jupyter-repl-insert :read-only nil indent)))
      ("invalid"
       ;; Force an execute to produce a traceback
       (jupyter-send-execute-request client))
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
depending on the kernel's response to an `:is-complete-request'.
If FORCE is non-nil, force the kernel to execute the current cell
code without sending the `:is-complete-request'. See
`jupyter-repl-use-builtin-is-complete' for yet another way to
execute the current cell."
  (interactive "P")
  (condition-case nil
      (let ((cell-beginning (save-excursion
                              (goto-char (point-max))
                              (jupyter-repl-cell-beginning-position))))
        (if (< (point) cell-beginning)
            (goto-char (point-max))
          (unless (jupyter-repl-connected-p)
            (error "Kernel not alive"))
          ;; NOTE: kernels allow execution requests to queue up, but we prevent
          ;; sending a request when the kernel is busy because of the
          ;; is-complete request. Some kernels don't respond to this request
          ;; when the kernel is busy.
          (unless (member (oref jupyter-repl-current-client execution-state)
                          '("starting" "idle"))
            (jupyter-repl-sync-execution-state)
            (error "Kernel busy"))
          (if force (jupyter-send-execute-request jupyter-repl-current-client)
            (if (not jupyter-repl-use-builtin-is-complete)
                (let* ((jupyter-inhibit-handlers '(:status))
                       (res (jupyter-wait-until-received :is-complete-reply
                              (jupyter-send-is-complete-request
                                  jupyter-repl-current-client
                                :code (jupyter-repl-cell-code))
                              jupyter-repl-maximum-is-complete-timeout)))
                  (unless res
                    (message "Kernel did not respond to is-complete-request, using built-in is-complete.
Reset `jupyter-repl-use-builtin-is-complete' to nil if this is only temporary.")
                    (setq-local jupyter-repl-use-builtin-is-complete t)
                    (jupyter-repl-ret force)))
              (goto-char (point-max))
              (let ((complete-p (equal (buffer-substring
                                        (line-beginning-position) (point))
                                       "")))
                (jupyter-handle-is-complete-reply
                    jupyter-repl-current-client
                  nil (if complete-p "complete" "incomplete") ""))))))
    (beginning-of-buffer
     ;; No cells in the current buffer, just insert one
     (jupyter-repl-insert-prompt 'in))))

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
  (when (eq major-mode 'jupyter-repl-mode)
    (if (not (jupyter-channels-running-p jupyter-repl-current-client)) t
      (when (y-or-n-p
             (format "Jupyter REPL (%s) still connected. Kill it? "
                     (buffer-name (current-buffer))))
        ;; TODO: Handle case when multiple clients are connected, i.e. do we
        ;; want to also delete a kernel if this is the last client connected.
        ;; See `eieio-instance-tracker'.
        (prog1 t
          (jupyter-stop-channels jupyter-repl-current-client)
          (destructor jupyter-repl-current-client)
          (when (jupyter-repl-client-has-manager-p)
            (jupyter-shutdown-kernel (oref jupyter-repl-current-client manager))
            (destructor (oref jupyter-repl-current-client manager)))
          (cl-loop
           with client = jupyter-repl-current-client
           for buffer in (buffer-list)
           do (with-current-buffer buffer
                (when (eq jupyter-repl-current-client client)
                  (jupyter-repl-interaction-mode -1)))))))))

(defun jupyter-repl-preserve-window-margins (&optional window)
  "Ensure that the margins of a REPL window are present.
This function is added as a hook to `pre-redisplay-functions' to
ensure that a REPL windows margins are present.

If WINDOW is showing a REPL buffer and the margins are not set to
`jupyter-repl-prompt-margin-width', set them to the proper
value."
  ;; NOTE: Sometimes the margins will disappear after the window configuration
  ;; changes which is why `window-configuration-change-hook' is not used.
  (when (and (eq major-mode 'jupyter-repl-mode)
             (let ((margins (window-margins window)))
               (not (and (consp margins)
                         (car margins)
                         (= (car margins) jupyter-repl-prompt-margin-width)))))
    (set-window-buffer window (current-buffer))))

;;; Completion

(defun jupyter-repl-code-context-at-point (type)
  "Return a cons cell, (CODE . POS), for the context around `point'.
CODE is the required context for TYPE (either `inspect' or
`complete') and POS is the relative position of `point' within
CODE. The context also depends on the `major-mode' of the
`current-buffer'. If the `major-mode' is `jupyter-repl-mode',
CODE is the contents of the entire code cell. Otherwise its
either the line up to `point' if TYPE is `complete' or the entire
line if TYPE is `inspect'."
  (unless (memq type '(complete inspect))
    (error "Type not `complete' or `inspect' (%s)" type))
  (let (code pos)
    (cl-case type
      (inspect
       (setq code
             ;; TODO: This still needs work
             (save-excursion
               ;; Ignore the invisible characters of a prompt
               (when (and (eq major-mode 'jupyter-repl-mode)
                          (field-at-pos (point)))
                 (goto-char (1+ (field-end))))
               (buffer-substring
                (line-beginning-position)
                (line-end-position)))
             ;; NOTE: The +1 is because normally, when inspecting code, `point'
             ;; is on a character of the symbol being inspected, this is in
             ;; contrast to completing code where `point' is after the last
             ;; character of the prefix. This fixes an edge case where `point'
             ;; is at the first character of a symbol.
             pos (1+ (- (point) (line-beginning-position)))))
      (complete
       (if (eq major-mode 'jupyter-repl-mode)
           (setq code (jupyter-repl-cell-code)
                 pos (1- (jupyter-repl-cell-code-position)))
         (setq code (buffer-substring (line-beginning-position) (point))
               pos (- (point) (line-beginning-position))))))
    (list code pos)))

(defun jupyter-repl-completion-prefix ()
  "Return the prefix for the current completion context.
Note that the prefix returned is not the content sent to the
kernel. See `jupyter-repl-code-context-at-point' for what is
actually sent to the kernel."
  (when jupyter-repl-current-client
    (let ((lang-mode (jupyter-repl-language-mode jupyter-repl-current-client)))
      (and (memq major-mode `(,lang-mode jupyter-repl-mode))
           ;; No completion in finalized cells
           (not (get-text-property (point) 'read-only))
           (or (when (looking-at "\\_>")
                 (let* ((beg (save-excursion
                               (+ (point) (skip-syntax-backward "w_"))))
                        (char (char-before beg)))
                   ;; Handle LaTeX in the Julia kernel.
                   ;;
                   ;; TODO: Generalize this. Note that in julia-mode \ has
                   ;; a punctuation syntax class.
                   (and char (= char ?\\)
                        (buffer-substring (1- beg) (point)))))
               (let* ((s (company-grab-symbol-cons "\\.\\|::\\|->" 2)))
                 ;; Do not complete on floating point numbers
                 (unless (and (consp s)
                              (string= (car s) "")
                              (= (char-before (point)) ?.)
                              (<= ?0 (char-before (1- (point))) ?9))
                   s)))))))

(defun jupyter-repl-construct-completion-candidates
    (prefix matches metadata start end)
  "Construct candidates for completion.
PREFIX is the prefix used to start the current completion.
MATCHES are the completion matches returned by the kernel,
METADATA is any extra data associated with MATCHES that was
supplied by the kernel. START and END are the start and end of
text that the elements of MATCHES will replace. Note that START
and END are relative to the `jupyter-repl-code-context-at-point'
and not to PREFIX. See `jupyter-repl-completion-prefix' for the
value that PREFIX takes.

This function constructs candidates assuming that `company-mode'
is used for completion."
  (let* ((matches (append matches nil))
         (tail matches)
         (types (append (plist-get metadata :_jupyter_types_experimental) nil))
         ;; TODO: Handle the case when the matches are method signatures in the
         ;; Julia kernel. This information would be useful for doing some kind
         ;; of eldoc like feature.
         (match-prefix-len (- (- end start) (length prefix))))
    ;; FIXME: How to complete things like 000|? In the python kernel,
    ;; completions will return matchs to append like and, or, ... but the
    ;; prefix 000 was provided so company will replace 000 with the match if it
    ;; is accepted instead of appending it. In this case end == start ==
    ;; (length prefix). How can we append the match? The only way I can see is
    ;; to add 000 as a prefix to every match.
    (cond
     ((> match-prefix-len 0)
      ;; Remove the beginning characters of a match when len > 0. This happens
      ;; when completing things like foo.ba, the python kernel will return
      ;; matches like foo.bar, foo.baz, but we only want the bar or baz part.
      (while (car tail)
        (setcar tail (substring (car tail) match-prefix-len))
        (setq tail (cdr tail))))
     ((< match-prefix-len 0)
      ;; Add the prefix when necessaey
      ;; (while (car tail)
      ;;   (put-text-property 0 1 'match (abs match-prefix-len) (car tail))
      ;;   (setq tail (cdr tail)))

      ))
    ;; When a type is supplied add it as an annotation
    ;; TODO: Customize annotation types, when an annotation type "function"
    ;; appears, substitute "λ".
    (when types
      (let ((max-len (apply #'max (mapcar #'length matches))))
        (cl-mapc
         (lambda (match meta)
           (let* ((prefix (make-string (1+ (- max-len (length match))) ? ))
                  (annot (concat prefix (plist-get meta :type))))
             (put-text-property 0 1 'annot annot match)))
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
    (prefix (jupyter-repl-completion-prefix))
    (candidates
     (cons
      :async
      (lambda (cb)
        (cl-destructuring-bind (code pos)
            (jupyter-repl-code-context-at-point 'complete)
          (jupyter-add-callback
              ;; Ignore errors during completion
              (let ((jupyter-inhibit-handlers t))
                (jupyter-send-complete-request
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
code, respectively.

If BUFFER is non-nil then it should be the buffer in which to
insert the inspection text returned from the kernel. After the
inserting the text into BUFFER, BUFFER is returned. If BUFFER is
nil, just return the inspection text. In both cases the
inspection text is already in a form suitable for display.

TIMEOUT is how long to wait (in seconds) for the kernel to
respond before returning nil."
  (let* ((jupyter-inhibit-handlers '(:status))
         (msg (jupyter-wait-until-received :inspect-reply
                (jupyter-send-inspect-request jupyter-repl-current-client
                  :code code :pos pos)
                timeout)))
    (when msg
      (cl-destructuring-bind (&key status found data metadata &allow-other-keys)
          (jupyter-message-content msg)
        (when (and (equal status "ok") found)
          (if buffer
              (with-current-buffer buffer
                (prog1 buffer
                  (jupyter-repl-insert-data data metadata)
                  (goto-char (point-min))))
            (with-temp-buffer
              (jupyter-repl-insert-data data metadata)
              (buffer-string))))))))

(defun jupyter-repl-inspect-at-point ()
  "Inspect the code at point.
Send an inspect request to the `jupyter-repl-current-client' of
the `current-buffer' and display the results in a buffer."
  (interactive)
  (cl-destructuring-bind (code pos)
      (jupyter-repl-code-context-at-point 'inspect)
    (let ((buf (current-buffer)))
      ;; TODO: Reset this to nil when the inspect buffer is closed.
      (with-jupyter-repl-doc-buffer "inspect"
        (let ((jupyter-repl-current-client
               (buffer-local-value 'jupyter-repl-current-client buf)))
          ;; FIXME: Better way of inserting documentation into a buffer.
          ;; Currently the way text is inserted is by inserting in a temp
          ;; buffer and returning the string, but in cases where overlays may
          ;; be inserted in the buffer (markdown), this fails. A better way
          ;; would be to supply the buffer in which to insert text like what is
          ;; done here, but how to make it more general for all insertion
          ;; types?
          (if (not (jupyter-repl--inspect code pos (current-buffer)))
              (message "Inspect timed out")
            ;; TODO: Customizable action
            (display-buffer (current-buffer))
            (set-window-start (get-buffer-window) (point-min)))))
      (setq other-window-scroll-buffer (get-buffer "*jupyter-repl-inspect*")))))

;;; Evaluation

(defun jupyter-repl-eval-string (str &optional silently)
  "Evaluate STR with the `jupyter-repl-current-client'.
The contents of the last cell in the REPL buffer will be replaced
with STR and the last cell executed with the
`juptyer-repl-current-client'. After execution, the execution
result is echoed to the *Message* buffer or a new buffer showing
the result is opened if the result output is larger than 10 lines
long.

If optional argument SILENTLY is non-nil, do not replace the
contents of the last cell and do not run any of the
`jupyter-repl-client' handlers. All that occurs is that STR is
sent to the kernel for execution and the results of the execution
displayed without anything showing up in the REPL buffer."
  (interactive (list (read-string "Jupyter Eval: ") current-prefix-arg))
  (unless (buffer-local-value
           'jupyter-repl-current-client (current-buffer))
    (user-error "No `jupyter-repl-current-client' set, see `jupyter-repl-associate-buffer'"))
  (with-jupyter-repl-buffer jupyter-repl-current-client
    (goto-char (point-max))
    (unless (= (save-excursion (jupyter-repl-previous-cell)) 0)
      (jupyter-repl-insert-prompt 'in))
    (setq str (string-trim str))
    (let* ((jupyter-inhibit-handlers
            (or silently '(:execute-reply :execute-result)))
           (req (jupyter-send-execute-request jupyter-repl-current-client
                  :code (if silently (string-trim str)
                          (prog1 nil
                            (jupyter-repl-replace-cell-code str))))))
      (jupyter-add-callback req
        :execute-reply (lambda (msg)
                         (cl-destructuring-bind (&key status ename evalue
                                                      &allow-other-keys)
                             (jupyter-message-content msg)
                           (unless (equal status "ok")
                             (message "jupyter (%s): %s" ename
                                      (ansi-color-apply evalue)))))
        :execute-result
        (lambda (msg)
          (let ((res (jupyter-message-data msg :text/plain))
                (inhibit-read-only t))
            ;; Prioritize the text representation
            (if res
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
                      (display-buffer (current-buffer)))
                  (if (equal res "") (message "jupyter: eval done")
                    (message res)))
              (with-current-buffer
                  (get-buffer-create "*jupyter-repl-result*")
                (erase-buffer)
                (jupyter-repl-insert-data
                 (jupyter-message-get msg :data)
                 (jupyter-message-get msg :metadata))
                (goto-char (point-min))
                (switch-to-buffer-other-window (current-buffer)))))))
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
SILENTLY has the same meaning as in `jupyter-repl-eval-string'."
  (interactive "rP")
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

(defun jupyter-repl-on-kernel-restart (client msg)
  "Update the REPL buffer after CLIENT restarts.
If MSG is a startup message, insert the banner of the kernel,
syncrhronize the execution state, and insert a new input prompt."
  (prog1 nil
    (with-jupyter-repl-buffer client
      (when (jupyter-message-status-starting-p msg)
        ;; FIXME: Don't assume `jupyter-include-other-output' was previously nil
        (jupyter-set jupyter-repl-current-client 'jupyter-include-other-output nil)
        (jupyter-repl-without-continuation-prompts
         (goto-char (point-max))
         (jupyter-repl-previous-cell)
         (unless (jupyter-repl-cell-finalized-p)
           (jupyter-repl-finalize-cell nil)
           (jupyter-repl-newline)
           (jupyter-repl-insert-banner
            (plist-get (oref client kernel-info) :banner))
           (jupyter-repl-sync-execution-state)
           (jupyter-repl-insert-prompt 'in)))))))

(defun jupyter-repl-interrupt-kernel ()
  "Interrupt the kernel if possible.
A kernel can be interrupted if it was started using a
`jupyter-kernel-manager'. See `jupyter-start-new-kernel'."
  (interactive)
  (if (not (jupyter-repl-client-has-manager-p))
      (user-error "Cannot interrupt non-subprocess kernels")
    (message "Interrupting kernel")
    (jupyter-interrupt-kernel
     (oref jupyter-repl-current-client manager))))

;; TODO: Make timeouts configurable
(defun jupyter-repl-restart-kernel (&optional shutdown)
  "Restart the kernel.
With a prefix argument, SHUTDOWN the kernel completely instead."
  (interactive "P")
  (unless shutdown
    ;; This may have been set to t due to a non-responsive kernel so make sure
    ;; that we try again when restarting.
    (setq-local jupyter-repl-use-builtin-is-complete nil)
    (jupyter-set jupyter-repl-current-client 'jupyter-include-other-output t))
  (if (jupyter-repl-client-has-manager-p)
      (let ((manager (oref jupyter-repl-current-client manager)))
        (if (jupyter-kernel-alive-p manager)
            (progn
              (message "%s kernel..." (if shutdown "Shutting down"
                                        "Restarting"))
              (jupyter-shutdown-kernel manager (not shutdown)))
          (message "Starting dead kernel...")
          (jupyter-start-kernel manager)))
    (unless (jupyter-wait-until-received :shutdown-reply
              (jupyter-send-shutdown-request jupyter-repl-current-client
                :restart (not shutdown)))
      (jupyter-set jupyter-repl-current-client 'jupyter-include-other-output nil)
      (message "Kernel did not respond to shutdown request"))))

(defun jupyter-repl-display-kernel-buffer ()
  "Display the kernel processes stdout."
  (interactive)
  (if (jupyter-repl-client-has-manager-p)
      (let ((manager (oref jupyter-repl-current-client manager)))
        (display-buffer (process-buffer (oref manager kernel))))
    (user-error "Kernel not a subprocess")))

;;; Isearch
;; Adapted from isearch in `comint', see `comint-history-isearch-search' for
;; details

(defun jupyter-repl-isearch-setup ()
  "Setup Isearch to search through the input history."
  (setq-local isearch-search-fun-function
              #'jupyter-repl-history-isearch-search)
  (setq-local isearch-wrap-function
              #'jupyter-repl-history-isearch-wrap)
  (setq-local isearch-push-state-function
              #'jupyter-repl-history-isearch-push-state))

;; Adapted from `comint-history-isearch-search'
(defun jupyter-repl-history-isearch-search ()
  "Return a search function to search through a REPL's input history."
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
         (condition-case nil
             (progn
               (while (not found)
                 (cond (isearch-forward
                        ;; `jupyter-repl-history-next' clears the cell if the
                        ;; last element is the sentinel, prevent that.
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
  "Wrap the input history search when search fails.
Go to the newest history element for a forward search or to the
oldest history element for a backward search."
  (condition-case nil
      (if isearch-forward
          (jupyter-repl-history-next (ring-length jupyter-repl-history) t)
        (jupyter-repl-history-previous (ring-length jupyter-repl-history) t))
    (error nil))
  (jupyter-repl-replace-cell-code (ring-ref jupyter-repl-history 0))
  (goto-char (if isearch-forward (jupyter-repl-cell-code-beginning-position)
               (point-max))))

(defun jupyter-repl-history-isearch-push-state ()
  "Save a function restoring the state of input history search.
Save the element at index 0 in `jupyter-repl-history'. When
restoring the state, the `jupyter-repl-history' ring is rotated,
in the appropriate direction, to the saved element."
  (let ((elem (ring-ref jupyter-repl-history 0)))
    (lambda (_cmd)
      (while (not (eq (ring-ref jupyter-repl-history 0) elem))
        (if isearch-forward (jupyter-repl-history-next 1 t)
          (jupyter-repl-history-previous 1 t)))
      (jupyter-repl-replace-cell-code (ring-ref jupyter-repl-history 0)))))

;;; `jupyter-repl-mode'

(defun jupyter-repl-scratch-buffer ()
  "Display a scratch buffer associated with the current REPL buffer."
  (interactive)
  (if (jupyter-repl-connected-p)
      (let ((client jupyter-repl-current-client))
        (with-current-buffer (get-buffer-create
                              (concat "*jupyter-scratch*"))
          (funcall (jupyter-repl-language-mode client))
          (jupyter-repl-associate-buffer client)
          (pop-to-buffer (current-buffer))))
    (error "Not in a valid REPL buffer")))

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

;; TODO: Gaurd against a major mode change
(put 'jupyter-repl-mode 'mode-class 'special)
(define-derived-mode jupyter-repl-mode fundamental-mode
  "Jupyter-REPL"
  "A Jupyter REPL major mode."
  (cl-check-type jupyter-repl-current-client jupyter-repl-client)
  (setq-local indent-line-function #'jupyter-repl-indent-line)
  (setq-local left-margin-width jupyter-repl-prompt-margin-width)
  ;; Initialize a buffer using the major-mode correponding to the kernel's
  ;; language. This will be used for indentation and to capture font lock
  ;; properties.
  (let* ((info (oref jupyter-repl-current-client kernel-info))
         (language-info (plist-get info :language_info)))
    (cl-destructuring-bind (mode syntax)
        (jupyter-repl-kernel-language-mode-properties language-info)
      (setq-local jupyter-repl-lang-mode mode)
      (setq-local jupyter-repl-lang-buffer
                  (get-buffer-create
                   (format " *jupyter-repl-lang-%s*"
                           (plist-get language-info :name))))
      (set-syntax-table syntax)
      (with-jupyter-repl-lang-buffer
        (unless (eq major-mode mode)
          (funcall mode)))))
  ;; Get history from kernel
  (setq-local jupyter-repl-history
              (make-ring (1+ jupyter-repl-history-maximum-length)))
  ;; The sentinel value keeps track of the newest/oldest elements of the
  ;; history since next/previous navigation is implemented by rotations on the
  ;; ring.
  (ring-insert jupyter-repl-history 'jupyter-repl-history)
  (let ((jupyter-inhibit-handlers '(:status)))
    (jupyter-send-history-request jupyter-repl-current-client
      :n jupyter-repl-history-maximum-length :raw nil :unique t))
  (erase-buffer)
  ;; Add local hooks
  (add-hook 'kill-buffer-query-functions #'jupyter-repl-kill-buffer-query-function nil t)
  (add-hook 'after-change-functions 'jupyter-repl-after-buffer-change nil t)
  (add-hook 'pre-redisplay-functions 'jupyter-repl-preserve-window-margins nil t)
  ;; Initialize the REPL
  (buffer-disable-undo)
  ;; TODO: Rename to initialize-jupyter-hooks
  (jupyter-repl-initialize-hooks)
  (jupyter-repl-initialize-fontification)
  (jupyter-repl-isearch-setup)
  (jupyter-repl-sync-execution-state)
  (jupyter-repl-interaction-mode))

(defun jupyter-repl-initialize-hooks ()
  "Initialize startup hooks.
When the kernel restarts, insert a new prompt."
  ;; NOTE: This hook will only run if `jupyter-include-other-output' is non-nil
  ;; during the restart.
  (jupyter-add-hook jupyter-repl-current-client 'jupyter-iopub-message-hook
    (apply-partially
     #'jupyter-repl-on-kernel-restart jupyter-repl-current-client)))

(defun jupyter-repl-initialize-fontification ()
  "Initialize fontification for the current REPL buffer.
Extract `font-lock-defaults' from the `jupyter-repl-lang-buffer',
set it as the `font-lock-defaults' of the `current-buffer' and
call the function `font-lock-mode'."
  (let (fld sff)
    (with-jupyter-repl-lang-buffer
      (setq fld font-lock-defaults
            sff font-lock-syntactic-face-function))
    (setq
     font-lock-defaults
     (apply #'list (nth 0 fld) (nth 1 fld) (nth 2 fld) (nth 3 fld) (nth 4 fld)
            (append
             (nthcdr 5 fld)
             (list
              (cons 'font-lock-syntactic-face-function
                    ;; Only fontify syntactically when the text does
                    ;; not have a font-lock-face property
                    (lambda (state)
                      (unless (get-text-property
                               (nth 8 state) 'font-lock-face)
                        (when sff (funcall sff state)))))))))
    (font-lock-mode)
    ;; Special case since `js2-mode' does not use `font-lock-defaults' for
    ;; highlighting.
    (when (and (eq jupyter-repl-lang-mode 'js2-mode)
               (null (nth 0 font-lock-defaults)))
      (add-hook 'after-change-functions
                (lambda (beg end len)
                  (unless (jupyter-repl-cell-finalized-p)
                    (save-restriction
                      (narrow-to-region
                       (jupyter-repl-cell-code-beginning-position)
                       (jupyter-repl-cell-code-end-position))
                      (js2-parse)
                      (js2-mode-apply-deferred-properties))))
                t t))))

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
  "Synchronize the state of the kernel in `jupyter-repl-current-client'.
Set the execution-count slot of `jupyter-repl-current-client' to
1+ the execution count of the client's kernel. Block until the
kernel goes idle for our request."
  (let* ((client jupyter-repl-current-client)
         (req (let ((jupyter-inhibit-handlers t))
                (jupyter-send-execute-request client :code "" :silent t))))
    (jupyter-add-callback req
      :status (lambda (msg)
                (oset client execution-state
                      (jupyter-message-get msg :execution_state)))
      :execute-reply (lambda (msg)
                       (oset client execution-count
                             (1+ (jupyter-message-get msg :execution_count)))))
    ;; FIXME: Waiting longer here to account for initial startup of the Jupyter
    ;; kernel. Sometimes the idle message won't be received if another long
    ;; running execute request is sent right after.
    (jupyter-wait-until-idle req 2)))

;;; `jupyter-repl-interaction-mode'

(defun jupyter-repl-pop-to-buffer ()
  "Switch to the REPL buffer associated with the `current-buffer'.
Switch to the REPL buffer of the `jupyter-repl-current-client'
for the `current-buffer'."
  (interactive)
  (if jupyter-repl-current-client
      (with-jupyter-repl-buffer jupyter-repl-current-client
        (goto-char (point-max))
        (pop-to-buffer (current-buffer)))
    (error "Buffer not associated with a REPL, see `jupyter-repl-associate-buffer'")))

(defun jupyter-repl-available-repl-buffers (&optional mode)
  "Get a list of REPL buffers that are connected to live kernels.
If MODE is non-nil, return all REPL buffers whose
`jupyter-repl-lang-mode' is MODE. MODE should be the `major-mode'
used to edit files of one of the Jupyter kernel languages."
  (delq
   nil
   (mapcar (lambda (b)
        (with-current-buffer b
          (and (eq major-mode 'jupyter-repl-mode)
               (if mode (eq mode jupyter-repl-lang-mode) t)
               (jupyter-repl-connected-p)
               (buffer-name b))))
      (buffer-list))))

;;;###autoload
(defun jupyter-repl-associate-buffer (client)
  "Associate the `current-buffer' with a REPL CLIENT.
The `current-buffer's `major-mode' must be the
`jupyter-repl-lang-mode' of the CLIENT. CLIENT can either be a
`jupyter-repl-client' or a buffer with a non-nil
`jupyter-repl-current-client'.

Associating a buffer with CLIENT involves setting the
buffer-local value of `jupyter-repl-current-client' to CLIENT and
enabling `jupyter-repl-interaction-mode'."
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
  (jupyter-repl-interaction-mode))

(defvar jupyter-repl-interaction-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'jupyter-repl-eval-line-or-region)
    (define-key map (kbd "C-c C-l") #'jupyter-repl-eval-file)
    (define-key map (kbd "C-c C-f") #'jupyter-repl-inspect-at-point)
    (define-key map (kbd "C-c C-r") #'jupyter-repl-restart-kernel)
    ;; TODO: Change this keybinding since C-i is actually TAB and there may be
    ;; a more conventional command to place here.
    (define-key map (kbd "C-c C-i") #'jupyter-repl-interrupt-kernel)
    (define-key map (kbd "C-c C-z") #'jupyter-repl-pop-to-buffer)
    map))

(defun jupyter-repl-propagate-client (orig-fun buffer-or-name &rest args)
  "Propgate the `jupyter-repl-current-client' to other buffers."
  (when jupyter-repl-interaction-mode
    (let ((client jupyter-repl-current-client)
          (buf (get-buffer buffer-or-name))
          (mode major-mode))
      (when buf
        (with-current-buffer buf
          (when (and (eq mode major-mode)
                     (not jupyter-repl-interaction-mode))
            (jupyter-repl-associate-buffer client))))))
  (apply orig-fun buffer-or-name args))

(advice-add 'switch-to-buffer :around #'jupyter-repl-propagate-client)

(define-minor-mode jupyter-repl-interaction-mode
  "Minor mode for interacting with a Jupyter REPL.
Note that for buffers with `jupyter-repl-interaction-mode'
enabled, any new buffer opened with the same `major-mode' will
automatically have its buffer associated with the REPL."
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
                      (cons '(company-jupyter-repl
                              ;; FIXME: These are too useful to give up but
                              ;; seems more like a personal preference.
                              :with
                              company-dabbrev-code
                              company-gtags
                              company-etags
                              company-keywords)
                            company-backends))))
    (unless (eq major-mode 'jupyter-repl-mode)
      (kill-local-variable 'jupyter-repl-current-client))
    (when (boundp 'company-mode)
      (setq-local company-backends
                  (delq 'company-jupyter-repl company-backends)))))

(defun jupyter-repl-kernel-language-mode-properties (language-info)
  "Get the `major-mode' info of a kernel's language.
LANGUAGE-INFO should be the plist of the `:language_info' key in
a kernel's kernel-info. The `major-mode' is found by consulting
`auto-mode-alist' using the language's file extension found in
LANGUAGE-INFO. Return a list

     (MODE SYNTAX-TABLE)

Where MODE is the `major-mode' to use for syntax highlighting
purposes and SYNTAX-TABLE is the syntax table of MODE."
  (cl-destructuring-bind (&key file_extension &allow-other-keys)
      language-info
    (let (mode syntax)
      (with-temp-buffer
        (let ((buffer-file-name
               (concat "jupyter-repl-lang" file_extension)))
          (delay-mode-hooks (set-auto-mode))
          (setq mode major-mode)
          (setq syntax (syntax-table))))
      (list mode syntax))))

(defun jupyter-repl--new-repl (client)
  "Initialize a new REPL buffer based on CLIENT.
CLIENT is a `jupyter-repl-client' already connected to its
kernel and should have a non-nil kernel-info slot.

A new REPL buffer communicating with CLIENT's kernel is created
and set as CLIENT'sthis case, if MANAGER will be the buffer slot.
If CLIENT already has a non-nil buffer slot, raise an error."
  (if (slot-boundp client 'buffer) (error "Client already has a REPL buffer")
    (unless (ignore-errors (oref client kernel-info))
      (error "Client needs to have valid kernel-info"))
    (cl-destructuring-bind (&key language_info
                                 banner
                                 &allow-other-keys)
        (oref client kernel-info)
      (let ((language-name (plist-get language_info :name))
            (language-version (plist-get language_info :version)))
        (oset client buffer
              (generate-new-buffer
               (format "*jupyter-repl[%s]*"
                       (concat language-name " " language-version))))
        (with-jupyter-repl-buffer client
          (setq-local jupyter-repl-current-client client)
          (jupyter-repl-mode)
          (jupyter-repl-insert-banner banner)
          (jupyter-repl-insert-prompt 'in))))))

;;;###autoload
(defun run-jupyter-repl (kernel-name &optional associate-buffer)
  "Run a Jupyter REPL connected to a kernel with name, KERNEL-NAME.
KERNEL-NAME will be passed to `jupyter-find-kernelspecs' and the
first kernel found will be used to start the new kernel.

Optional argument ASSOCIATE-BUFFER, if non-nil, means to enable
`jupyter-repl-interaction-mode' in the `current-buffer' and
associate it with the REPL created. When called interactively,
ASSOCIATE-BUFFER is set to t. If the `current-buffer's
`major-mode' does not correspond to the language of the kernel
started, ASSOCIATE-BUFFER has no effect.

When called interactively, display the new REPL buffer.
Otherwise, in a non-interactive call, return the
`jupyter-repl-client' connect to the kernel."
  (interactive (list (car (jupyter-completing-read-kernelspec
                           nil current-prefix-arg))
                     t))
  (unless (called-interactively-p 'interactive)
    (setq kernel-name (caar (jupyter-find-kernelspecs kernel-name))))
  (unless kernel-name
    (error "No kernel found for prefix (%s)" kernel-name))
  ;; The manager is set as the client's manager slot in
  ;; `jupyter-start-new-kernel'
  (cl-destructuring-bind (_manager client info)
      (jupyter-start-new-kernel kernel-name 'jupyter-repl-client)
    (oset client kernel-info info)
    (jupyter-repl--new-repl client)
    (when (and associate-buffer
               (eq major-mode (jupyter-repl-language-mode client)))
      (jupyter-repl-associate-buffer client))
    (if (called-interactively-p 'interactive)
        (pop-to-buffer (oref client buffer))
      client)))

;;;###autoload
(defun connect-jupyter-repl (file-or-plist &optional associate-buffer)
  "Run a Jupyter REPL using a kernel's connection FILE-OR-PLIST.
FILE-OR-PLIST can be either a file holding the connection
information or a property list of connection information.
ASSOCIATE-BUFFER has the same meaning as in `run-jupyter-repl'.

Return the `jupyter-repl-client' connected to the kernel. When
called interactively, display the new REPL buffer as well."
  (interactive (list (read-file-name "Connection file: ") t))
  (let ((client (make-instance 'jupyter-repl-client)))
    (jupyter-initialize-connection client file-or-plist)
    (jupyter-start-channels client)
    (message "Requesting kernel info...")
    (let* ((jupyter-inhibit-handlers t)
           (info (jupyter-wait-until-received :kernel-info-reply
                   (jupyter-send-kernel-info-request client)
                   5)))
      (unless info
        (destructor client)
        (error "Kernel did not respond to kernel-info request"))
      (oset client kernel-info (jupyter-message-content info))
      (jupyter-repl--new-repl client)
      (when (and associate-buffer
                 (eq major-mode (jupyter-repl-language-mode client)))
        (jupyter-repl-associate-buffer client))
      (when (called-interactively-p 'interactive)
        (pop-to-buffer (oref client buffer)))
      client)))

(provide 'jupyter-repl)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; jupyter-repl.el ends here
