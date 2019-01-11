;;; jupyter-repl-client.el --- A Jupyter REPL client -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 08 Jan 2018
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

;; A Jupyter REPL for Emacs.
;;
;; The main entry points are `jupyter-run-repl' and `jupyter-connect-repl'.
;;
;; When called interactively, `jupyter-run-repl' asks for a kernel to start
;; (based on the kernels found using `jupyter-available-kernelspecs'), connects
;; a `jupyter-repl-client' to the selected kernel, and pops up a REPL buffer.
;; The main difference of `jupyter-connect-repl' is that it will obtain the
;; kernel's connection info by asking for the JSON file containing it to start
;; connection to a kernel.
;;
;; Additionally, `jupyter-repl-associate-buffer' associates the
;; `current-buffer' with a REPL client appropriate for the buffer's
;; `major-mode'. Associating a buffer with a REPL client enables the minor mode
;; `jupyter-repl-interaction-mode' and, if `company-mode' is installed, enables
;; code completion using the associated REPL client.
;;
;; `jupyter-repl-interaction-mode' adds the following keybindings for
;; interacing a REPL client:
;;
;;     C-c C-c `jupyter-eval-line-or-region'
;;     C-c C-l `jupyter-eval-file'
;;     C-c C-f `jupyter-inspect-at-point'
;;     C-c C-r `jupyter-repl-restart-kernel'
;;     C-c C-i `jupyter-repl-interrupt-kernel'
;;     C-c C-z `jupyter-repl-pop-to-buffer'

;;; Code:

(defgroup jupyter-repl nil
  "A Jupyter REPL client"
  :group 'jupyter)

(require 'jupyter-base)
(require 'jupyter-mime)
(require 'jupyter-client)
(require 'jupyter-widget-client)
(require 'jupyter-kernel-manager)
(require 'ring)

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

(defface jupyter-repl-traceback
  '((((class color) (min-colors 88) (background light))
     :background "darkred")
    (((class color) (min-colors 88) (background dark))
     :background "firebrick"))
  "Face used for a traceback."
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

(defclass jupyter-repl-client (jupyter-widget-client jupyter-kernel-client)
  ((buffer
    :type (or null buffer)
    :initform nil
    :documentation "The REPL buffer whose
`jupyter-current-client' is this client.")
   (wait-to-clear
    :type boolean
    :initform nil
    :documentation "Whether or not we should wait to clear the
current output of the cell. Set when the kernel sends a
`:clear-output' message.")
   (execution-count
    :type integer
    :initform 1
    :documentation "The current execution count of the kernel.")))

(defvar-local jupyter-repl-lang-buffer nil
  "A buffer with the `major-mode' set to the REPL language's `major-mode'.")

(defvar-local jupyter-repl-lang-mode nil
  "The `major-mode' corresponding to the REPL's language.")

(defvar-local jupyter-repl-history nil
  "The history of the current Jupyter REPL.")

(defvar-local jupyter-repl-use-builtin-is-complete nil
  "Whether or not to send `:is-complete-request's to a kernel.
If a Jupyter kernel does not respond to an is_complete_request,
the buffer local value of this variable is set to t and code in a
cell is considered complete if the last line in a code cell is a
blank line, i.e. if RET is pressed twice in a row.")

(cl-generic-define-context-rewriter jupyter-repl-mode (mode &rest modes)
  `(jupyter-repl-lang-mode (derived-mode ,mode ,@modes)))

;;; Macros

(defmacro jupyter-with-repl-buffer (client &rest body)
  "Switch to CLIENT's REPL buffer before running BODY.
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
  (declare (debug (&rest form)))
  `(let ((inhibit-modification-hooks t))
     ,@body))

(defmacro jupyter-repl-append-output (client req &rest body)
  "Switch to CLIENT's buffer, move to the end of REQ, and run BODY.
REQ is a `jupyter-request' previously made using CLIENT, a REPL
client.

`point' is moved to the `jupyter-repl-cell-beginning-position' of
the cell *after* REQ, this position is where any newly generated
output of REQ should be inserted.

Also handles any terminal control codes in the appended output."
  (declare (indent 2) (debug (symbolp &rest form)))
  `(jupyter-with-repl-buffer ,client
     (let ((buffer-undo-list t))
       (save-excursion
         (jupyter-repl-goto-cell ,req)
         (jupyter-repl-next-cell)
         (jupyter-with-insertion-bounds
             beg end (jupyter-with-control-code-handling ,@body)
           (put-text-property beg end 'read-only t)
           (set-buffer-modified-p nil))))))

(defmacro jupyter-with-repl-lang-buffer (&rest body)
  "Run BODY in the `jupyter-repl-lang-buffer' of the `current-buffer'.
The contents of `jupyter-repl-lang-buffer' is erased before
running BODY."
  (declare (indent 0) (debug (&rest form)))
  (let ((client (make-symbol "clientvar")))
    `(let ((,client jupyter-current-client))
       (with-current-buffer jupyter-repl-lang-buffer
         (let ((inhibit-read-only t)
               (jupyter-current-client ,client))
           (erase-buffer)
           ,@body)))))

(defmacro jupyter-with-repl-cell (&rest body)
  "Narrow to the current cell, run BODY, then widen.
The cell is narrowed to the region between and including
`jupyter-repl-cell-code-beginning-position' and
`jupyter-repl-cell-code-end-position'."
  (declare (indent 0) (debug (&rest form)))
  `(save-excursion
     (save-restriction
       (narrow-to-region (jupyter-repl-cell-code-beginning-position)
                         (jupyter-repl-cell-code-end-position))
       ,@body)))

;;; Convenience functions

(defun jupyter-repl-language-mode (client)
  "Return the `major-mode' of CLIENT's kernel language."
  (jupyter-with-repl-buffer client
    jupyter-repl-lang-mode))

;;; Text insertion

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

- `:inherit' :: A non-nil value will use
  `insert-and-inherit' instead of `insert' for the function used
  to insert the text. This is nil by default."
  (let ((arg nil)
        (read-only t)
        (properties nil)
        (insert-fun #'insert)
        (buffer-undo-list t))
    (while (keywordp (setq arg (car args)))
      (cl-case arg
        (:read-only (setq read-only (cadr args)))
        (:properties (setq properties (cadr args)))
        (:inherit
         (setq insert-fun (if (cadr args) #'insert-and-inherit #'insert)))
        (otherwise
         (error "Keyword not one of `:read-only', `:properties', `:inherit-' (`%s')" arg)))
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

(cl-defmethod jupyter-insert :around (mime-or-plist
                                      &context (major-mode jupyter-repl-mode) &rest _)
  "If MIME was inserted, mark the region that was inserted as read only.
Do this only when the `major-mode' is `jupyter-repl-mode'."
  (if (listp mime-or-plist) (cl-call-next-method)
    (jupyter-with-insertion-bounds
        beg end (cl-call-next-method)
      (add-text-properties beg end '(read-only t)))))

(cl-defmethod jupyter-insert ((_mime (eql :application/vnd.jupyter.widget-view+json)) data
                              &context ((and (require 'websocket nil t)
                                             (require 'simple-httpd nil t)
                                             (and jupyter-current-client
                                                  (object-of-class-p
                                                   jupyter-current-client
                                                   'jupyter-widget-client))
                                             t)
                                        (eql t))
                              &optional _metadata)
  (jupyter-widgets-display-model jupyter-current-client (plist-get data :model_id)))

;;; Prompt

(defun jupyter-repl--prompt-display-value (str face)
  "Return the margin display value for a prompt STR.
FACE is the `font-lock-face' to use for STR."
  (list '(margin left-margin)
        (propertize
         (concat (make-string
                  (- jupyter-repl-prompt-margin-width
                     (length str))
                  ? )
                 str)
         'fontified t
         'font-lock-face face)))

(defun jupyter-repl--make-prompt (str face props)
  "Make a prompt overlay for the character at `point'.
STR is used as the prompt string and FACE is its
`font-lock-face'. Add PROPS as text properties to the character."
  (let ((ov (make-overlay (1- (point)) (point) nil t))
        (md (jupyter-repl--prompt-display-value str face)))
    (overlay-put ov 'after-string (propertize " " 'display md))
    (overlay-put ov 'evaporate t)
    (add-text-properties (overlay-start ov) (overlay-end ov) props)
    (overlay-recenter (point))))

(defun jupyter-repl-insert-prompt (&optional type)
  "Insert a REPL prompt according to TYPE.
TYPE can either be `in', `out', or `continuation'. A nil TYPE is
interpreted as `in'."
  (setq type (or type 'in))
  (unless (memq type '(in out continuation))
    (error "Prompt type can only be (`in', `out', or `continuation')"))
  (jupyter-repl-without-continuation-prompts
   (let ((inhibit-read-only t))
     ;; The newline that `jupyter-repl--make-prompt' will overlay.
     (jupyter-repl-newline)
     (cond
      ((eq type 'in)
       (let ((count (oref jupyter-current-client execution-count)))
         (jupyter-repl--make-prompt
          (format "In [%d] " count) 'jupyter-repl-input-prompt
          `(jupyter-cell (beginning ,count))))
       ;; Prevent prompt overlay from inheriting text properties of code at the
       ;; beginning of a cell.
       ;;
       ;; rear-nonsticky is to prevent code inserted after this character to
       ;; inherit any of its text properties.
       ;;
       ;; front-sticky is to prevent `point' from being trapped between the
       ;; newline of the prompt overlay and this invisible character.
       ;;
       ;; field is so that text motions will not move past this invisible
       ;; character.
       (jupyter-repl-insert
        :properties '(invisible t rear-nonsticky t front-sticky t) " ")
       ;; The insertion of a new prompt starts a new cell, don't consider the
       ;; buffer modified anymore. This is also an indicator for when undo's
       ;; can be made in the buffer.
       (set-buffer-modified-p nil))
      ((eq type 'out)
       ;; Output is normally inserted by first going to the end of the output
       ;; for the request. The end of the ouput for a request is at the
       ;; beginning of the next cell after the request which is why we get the
       ;; cell count of the previous cell
       (let ((count (jupyter-repl-previous-cell-count)))
         (jupyter-repl--make-prompt
          (format "Out [%d] " count) 'jupyter-repl-output-prompt
          `(jupyter-cell (out ,count))))
       ;; See the note above about the invisible character for input prompts
       (jupyter-repl-insert
        :properties '(invisible t front-sticky t) " "))
      ((eq type 'continuation)
       (jupyter-repl--make-prompt
        ;; This needs to be two characters wide for some
        ;; reason, otherwise the continuation prompts will
        ;; be missing one character.
        "  " 'jupyter-repl-input-prompt
        `(read-only nil rear-nonsticky t)))))))

(defun jupyter-repl-prompt-string ()
  "Return the prompt string of the current input cell."
  (let ((ov (car (overlays-at (jupyter-repl-cell-beginning-position)))))
    (when ov
      (cadr (get-text-property 0 'display (overlay-get ov 'after-string))))))

(defun jupyter-repl-cell-reset-prompt ()
  "Reset the current prompt back to its default."
  (jupyter-repl-cell-update-prompt
   (format "In [%d] " (jupyter-repl-cell-count))))

(defun jupyter-repl-cell-update-prompt (str &optional face)
  "Update the current cell's input prompt.
STR is the replacement prompt string. If FACE is non-nil, it
should be a face that the prompt will use and defaults to
`jupyter-repl-input-prompt'."
  (let ((ov (car (overlays-at (jupyter-repl-cell-beginning-position)))))
    (when ov
      (overlay-put
       ov 'after-string
       (propertize
        " " 'display (jupyter-repl--prompt-display-value
                      str (or face 'jupyter-repl-input-prompt)))))))

(defun jupyter-repl-cell-mark-busy ()
  "Mark the current cell as busy."
  ;; FIXME: Have a way of determining if the input prompt should be marked
  ;; busy. Languages like Julia have REPL modes which can change the prompt
  ;; string, and we emulate that here. In those cases, the prompt should be
  ;; kept how it was without marking it busy.
  (let ((str (jupyter-repl-prompt-string)))
    (when (equal (string-trim str)
                 (format "In [%d]" (jupyter-repl-cell-count)))
      (jupyter-repl-cell-update-prompt "In [*] "))))

(defun jupyter-repl-cell-unmark-busy ()
  "Un-mark the current cell as busy."
  (let ((str (jupyter-repl-prompt-string)))
    (when (equal (string-trim str) "In [*]")
      (jupyter-repl-cell-update-prompt
       (format "In [%d] " (jupyter-repl-cell-count))))))

(defun jupyter-repl-update-cell-count (n)
  "Set the current cell count to N."
  (when (or (jupyter-repl-cell-beginning-p)
            (zerop (save-excursion (jupyter-repl-previous-cell))))
    (setf (nth 1 (get-text-property
                  (jupyter-repl-cell-beginning-position)
                  'jupyter-cell))
          n)
    (jupyter-repl-cell-reset-prompt)))

(defun jupyter-repl-cell-count ()
  "Return the cell count of the cell at `point'."
  (let ((pos (if (jupyter-repl-cell-beginning-p) (point)
               (save-excursion
                 (jupyter-repl-previous-cell)
                 (point)))))
    (nth 1 (get-text-property pos 'jupyter-cell))))

(defun jupyter-repl-previous-cell-count ()
  "Return the cell count of the previous cell before `point'."
  (save-excursion
    (jupyter-repl-previous-cell)
    (jupyter-repl-cell-count)))

(defun jupyter-repl-cell-request ()
  "Return the `jupyter-request' of the current cell."
  (get-text-property (jupyter-repl-cell-beginning-position) 'jupyter-request))

;;; Cell motions

(defun jupyter-repl-cell-beginning-position ()
  "Return the cell beginning position of the current cell.
If `point' is already at the beginning of the current cell,
return `point'.

If the end of a cell is found before the beginning of one, i.e.
when `point' is somewhere inside the output of a cell, raise an
error.

If the beginning of the buffer is found before the beginning of a
cell, raise a `beginning-of-buffer' error."
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
end.

Note: If the current cell is the last cell in the buffer,
`point-max' is considered the end of the cell."
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
In the case of the last cell in the REPL buffer, i.e. an
unfinalized cell, the code ending position is `point-max'."
  (jupyter-repl-cell-end-position))

(defun jupyter-repl-next-cell (&optional N)
  "Go to the beginning of the next cell.
Move N times where N defaults to 1. Return the count of cells
left to move."
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
  "Go to the beginning of the previous cell.
Move N times where N defaults to 1. Return the count of cells
left to move.

Note, if `point' is not at the beginning of the current cell, the
first move is to the beginning of the current cell."
  (or N (setq N 1))
  (catch 'done
    (let ((starting-pos (point)))
      (while (> N 0)
        (let ((pos (previous-single-property-change (point) 'jupyter-cell)))
          (while (and pos (not (jupyter-repl-cell-beginning-p pos)))
            (setq pos (previous-single-property-change pos 'jupyter-cell)))
          (unless (when pos (goto-char pos) (setq N (1- N)))
            (goto-char (point-min))
            ;; Handle edge case when the first cell is at the beginning of the
            ;; buffer. This happens, for example, when erasing the buffer.
            (when (and (/= (point) starting-pos)
                       (jupyter-repl-cell-beginning-p (point)))
              (setq N (1- N)))
            (throw 'done t))))))
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
  "Go to the code beginning of the cell after the current one.
ARG is the number of cells to move and defaults to 1."
  (interactive "^p")
  (or arg (setq arg 1))
  (jupyter-repl-next-cell arg)
  (goto-char (jupyter-repl-cell-code-beginning-position)))

(defun jupyter-repl-backward-cell (&optional arg)
  "Go to the code beginning of the cell before the current one.
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
  (or (= pos (point-max))
      (eq (nth 0 (get-text-property pos 'jupyter-cell)) 'end)))

(defun jupyter-repl-multiline-p (text)
  "Is TEXT a multi-line string?"
  (string-match-p "\n" text))

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
  "Has the current cell been finalized?"
  (or (not (jupyter-repl-cell-line-p))
      (/= (jupyter-repl-cell-end-position) (point-max))))

(defun jupyter-repl-client-has-manager-p ()
  "Return non-nil if the `jupyter-current-client' has a kernel manager."
  (and jupyter-current-client
       (oref jupyter-current-client manager)))

(defun jupyter-repl-connected-p ()
  "Is the `jupyter-current-client' connected to its kernel?"
  (when jupyter-current-client
    (or (and (jupyter-repl-client-has-manager-p)
             ;; Check if the kernel is local
             (jupyter-kernel-alive-p
              (oref jupyter-current-client manager)))
        (jupyter-hb-beating-p jupyter-current-client))))

;;; Modifying cell code, truncating REPL buffer

(defun jupyter-repl-cell-output ()
  "Return the output of the current cell."
  (unless (jupyter-repl-cell-finalized-p)
    (error "Cell not finalized"))
  (let ((beg (jupyter-repl-cell-end-position))
        (end (save-excursion
               (jupyter-repl-next-cell)
               (jupyter-repl-cell-beginning-position))))
    (buffer-substring beg end)))

(defun jupyter-repl-cell-code ()
  "Return the code of the current cell."
  (buffer-substring
   (jupyter-repl-cell-code-beginning-position)
   (jupyter-repl-cell-code-end-position)))

(defun jupyter-repl-cell-code-position ()
  "Return the relative position of `point' with respect to the cell code."
  (unless (jupyter-repl-cell-line-p)
    (error "Not in code of cell"))
  (1+ (- (point) (jupyter-repl-cell-code-beginning-position))))

(defun jupyter-repl-finalize-cell (req)
  "Finalize the current cell.
REQ is the `jupyter-request' to associate with the current cell."
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
    (add-text-properties beg (point) '(read-only t font-lock-multiline t))
    ;; reset the undo list so that a completed cell doesn't get undone.
    (setq buffer-undo-list '((t . 0)))))

(defun jupyter-repl-replace-cell-code (new-code)
  "Replace the current cell code with NEW-CODE."
  (goto-char (jupyter-repl-cell-code-beginning-position))
  (delete-region (point) (jupyter-repl-cell-code-end-position))
  (jupyter-repl-insert :inherit t :read-only nil new-code))

(defun jupyter-repl-truncate-buffer ()
  "Truncate the `current-buffer' based on `jupyter-repl-maximum-size'.
The `current-buffer' is assumed to be a Jupyter REPL buffer. If
the `current-buffer' is larger than `jupyter-repl-maximum-size'
lines, truncate it to something less than
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
  (if code (cl-call-next-method)
    (jupyter-with-repl-buffer client
      (jupyter-repl-truncate-buffer)
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
           (jupyter-with-display-buffer "pager" 'reset
             (jupyter-insert-ansi-coded-text text)
             (goto-char (point-min))
             (forward-line line)
             (display-buffer (current-buffer)))))
        ((or "edit" "edit_magic")
         (with-current-buffer (find-file-other-window
                               (plist-get pl :filename))
           (goto-char (point-min))
           (forward-line (plist-get pl :line_number))
           (set-window-start (selected-window) (point))))
        ("set_next_input"
         (goto-char (point-max))
         (jupyter-repl-previous-cell)
         (jupyter-repl-replace-cell-code (plist-get pl :text))))))

(cl-defmethod jupyter-handle-execute-reply ((client jupyter-repl-client)
                                            _req
                                            _status
                                            execution-count
                                            _user-expressions
                                            payload)
  (oset client execution-count (1+ execution-count))
  (jupyter-with-repl-buffer client
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
    (jupyter-repl-append-output client req
      (jupyter-repl-insert-prompt 'out)
      (jupyter-insert data metadata))))

(cl-defmethod jupyter-handle-display-data ((client jupyter-repl-client)
                                           req
                                           data
                                           metadata
                                           transient)
  (let ((clear (prog1 (oref client wait-to-clear)
                 (oset client wait-to-clear nil)))
        (req (if (eq (jupyter-message-parent-type
                      (jupyter-request-last-message req))
                     :comm-msg)
                 ;; For comm messages which produce a `:display-data' message,
                 ;; the request is assumed to be the most recently completed
                 ;; one.
                 (jupyter-with-repl-buffer client
                   (save-excursion
                     (goto-char (point-max))
                     (jupyter-repl-previous-cell 2)
                     (jupyter-repl-cell-request)))
               req)))
    (jupyter-repl-append-output client req
      (cl-destructuring-bind (&key display_id &allow-other-keys)
          transient
        (if display_id
            (jupyter-insert display_id data metadata)
          (let ((inhibit-redisplay (not debug-on-error)))
            (when clear
              (jupyter-repl-clear-last-cell-output client)
              ;; Prevent slight flickering of prompt margin and text, this is
              ;; needed in addition to `inhibit-redisplay'. It also seems that
              ;; it can be placed anywhere within this let and it will prevent
              ;; flickering.
              (sit-for 0.1 t))
            (jupyter-insert data metadata)))))))

(cl-defmethod jupyter-handle-update-display-data ((client jupyter-repl-client)
                                                  _req
                                                  data
                                                  metadata
                                                  transient)
  (cl-destructuring-bind (&key display_id &allow-other-keys)
      transient
    (unless display_id
      (error "No display ID in `:update-display-data' message"))
    (jupyter-with-repl-buffer client
      (jupyter-update-display display_id data metadata))))

(defun jupyter-repl-clear-last-cell-output (client)
  "In CLIENT's REPL buffer, clear the output of the last completed cell."
  (jupyter-with-repl-buffer client
    (goto-char (point-max))
    (jupyter-repl-previous-cell 2)
    (delete-region (1+ (jupyter-repl-cell-end-position))
                   (progn
                     (jupyter-repl-next-cell)
                     (point)))))

(cl-defmethod jupyter-handle-clear-output ((client jupyter-repl-client)
                                           req
                                           wait)
  (unless (oset client wait-to-clear (eq wait t))
    (cond
     ((eq (jupyter-message-parent-type
           (jupyter-request-last-message req))
          :comm-msg)
      (with-current-buffer (jupyter-get-buffer-create "output")
        (erase-buffer)))
     (t
      (jupyter-repl-clear-last-cell-output client)))))

(cl-defmethod jupyter-handle-status ((client jupyter-repl-client) req execution-state)
  (when (equal execution-state "idle")
    (jupyter-with-repl-buffer client
      (save-excursion
        (when (ignore-errors
                (progn (jupyter-repl-goto-cell req) t))
          (jupyter-repl-cell-unmark-busy))
        ;; Update the cell count and reset the prompt
        (goto-char (point-max))
        (jupyter-repl-update-cell-count (oref client execution-count)))))
  (force-mode-line-update))

(defun jupyter-repl-display-other-output (client stream text)
  "Display output not originating from CLIENT.
STREAM is the name of a stream which will be used to select the
buffer to display TEXT."
  (let* ((bname (buffer-name (oref client buffer)))
         (stream-buffer
          (concat (substring bname 0 (1- (length bname))) "-" stream "*")))
    ;; FIXME: Reset this on the next request
    (jupyter-with-display-buffer stream-buffer nil
      (let ((pos (point)))
        (jupyter-insert-ansi-coded-text text)
        (fill-region pos (point)))
      (display-buffer (current-buffer) '(display-buffer-pop-up-window
                                         (pop-up-windows . t))))))

(cl-defmethod jupyter-handle-stream ((client jupyter-repl-client) req name text)
  (if (null req)
      (jupyter-repl-display-other-output client name text)
    (cond
     ((eq (jupyter-message-parent-type
           (jupyter-request-last-message req))
          :comm-msg)
      (jupyter-with-display-buffer "output" req
        (jupyter-insert-ansi-coded-text text)
        (display-buffer (current-buffer))))
     (t
      (jupyter-repl-append-output client req
        (jupyter-insert-ansi-coded-text text))))))

(cl-defmethod jupyter-handle-error ((client jupyter-repl-client)
                                    req _ename _evalue traceback)
  (when req
    (cond
     ((eq (jupyter-message-parent-type
           (jupyter-request-last-message req))
          :comm-msg)
      (jupyter-display-traceback traceback))
     (t
      (jupyter-repl-append-output client req
        (jupyter-with-insertion-bounds
            beg end (jupyter-insert-ansi-coded-text
                     (concat (mapconcat #'identity traceback "\n") "\n"))
          (font-lock-prepend-text-property
           beg end 'font-lock-face 'jupyter-repl-traceback)))))))

(defun jupyter-repl-history--next (n)
  "Helper function for `jupyter-repl-history-next'.
Rotates `jupyter-repl-history' N times in the forward direction,
towards newer history elements and returns the Nth history
element in that direction relative to the current REPL history.
If the sentinel value is found before rotating N times, return
nil."
  (if (> n 0)
      (unless (eq (ring-ref jupyter-repl-history -1) 'jupyter-repl-history)
        (ring-insert jupyter-repl-history
                     (ring-remove jupyter-repl-history -1))
        (jupyter-repl-history--next (1- n)))
    (ring-ref jupyter-repl-history 0)))

(defun jupyter-repl-history-next (&optional n)
  "Go to the next history element.
Navigate through the REPL history to the next (newer) history
element and insert it as the last code cell. For N positive move
forward in history that many times. If N is negative, move to
older history elements."
  (interactive "p")
  (or n (setq n 1))
  (if (< n 0) (jupyter-repl-history-previous (- n))
    (goto-char (point-max))
    (let ((code (jupyter-repl-history--next n)))
      (if (and (null code) (equal (jupyter-repl-cell-code) ""))
          (error "End of history")
        (if (null code)
            ;; When we have reached the last history element in the forward
            ;; direction and the cell code is not empty, make it empty.
            (jupyter-repl-replace-cell-code "")
          (jupyter-repl-replace-cell-code code))))))

(defun jupyter-repl-history--previous (n)
  "Helper function for `jupyter-repl-history-previous'.
Rotates `jupyter-repl-history' N times in the backward direction,
towards older history elements and returns the Nth history
element in that direction relative to the current REPL history.
If the sentinel value is found before rotating N times, return
nil."
  (if (> n 0)
      (unless (eq (ring-ref jupyter-repl-history 1) 'jupyter-repl-history)
        (ring-insert-at-beginning
         jupyter-repl-history (ring-remove jupyter-repl-history 0))
        (jupyter-repl-history--previous (1- n)))
    (unless (eq (ring-ref jupyter-repl-history 0) 'jupyter-repl-history)
      (ring-ref jupyter-repl-history 0))))

(defun jupyter-repl-history-previous (&optional n)
  "Go to the previous history element.
Similar to `jupyter-repl-history-next' but for older history
elements. If N is negative in this case, move to newer history
elements."
  (interactive "p")
  (or n (setq n 1))
  (if (< n 0) (jupyter-repl-history-next (- n))
    (goto-char (point-max))
    (unless (equal (jupyter-repl-cell-code)
                   (ring-ref jupyter-repl-history 0))
      (setq n (1- n)))
    (let ((code (jupyter-repl-history--previous n)))
      (if (null code)
          (error "Beginning of history")
        (jupyter-repl-replace-cell-code code)))))

(cl-defmethod jupyter-handle-history-reply ((client jupyter-repl-client) _req history)
  (jupyter-with-repl-buffer client
    (cl-loop for elem across history
             for input-output = (aref elem 2)
             do (ring-remove+insert+extend jupyter-repl-history input-output))))

(cl-defmethod jupyter-handle-is-complete-reply ((client jupyter-repl-client) _req status indent)
  (jupyter-with-repl-buffer client
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
  (jupyter-with-repl-buffer client
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
at some position within the last cell, either insert a newline or
ask the kernel to execute the cell code depending on the kernel's
response to an `:is-complete-request'.

If a prefix argument is given, FORCE the kernel to execute the
current cell code without sending an `:is-complete-request'. See
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
          (unless (member (jupyter-execution-state jupyter-current-client)
                          '("starting" "idle"))
            (jupyter-repl-sync-execution-state)
            (error "Kernel busy"))
          (if force (jupyter-send-execute-request jupyter-current-client)
            (if (not jupyter-repl-use-builtin-is-complete)
                (let ((res (jupyter-wait-until-received :is-complete-reply
                             (let ((jupyter-inhibit-handlers '(:status)))
                               (jupyter-send-is-complete-request
                                   jupyter-current-client
                                 :code (jupyter-repl-cell-code)))
                             jupyter-repl-maximum-is-complete-timeout)))
                  (unless res
                    (message "Kernel did not respond to is-complete-request, using built-in is-complete.
Reset `jupyter-repl-use-builtin-is-complete' to nil if this is only temporary.")
                    (setq jupyter-repl-use-builtin-is-complete t)
                    (jupyter-repl-ret force)))
              (goto-char (point-max))
              (let ((complete-p (equal (buffer-substring-no-properties
                                        (line-beginning-position) (point))
                                       "")))
                (jupyter-handle-is-complete-reply
                    jupyter-current-client
                  nil (if complete-p "complete" "incomplete") ""))))))
    (beginning-of-buffer
     ;; No cells in the current buffer, just insert one
     (jupyter-repl-insert-prompt 'in))))

(cl-defgeneric jupyter-indent-line ()
  (call-interactively #'indent-for-tab-command))

(defun jupyter-repl-indent-line ()
  "Indent the line according to the language of the REPL."
  (let* ((pos (jupyter-repl-cell-code-position))
         (code (jupyter-repl-cell-code))
         (replacement
          ;; TODO: Convert to using indirect buffers if
          ;; they are faster. We can keep an indirect
          ;; buffer around with the languages major mode.
          ;; This way we avoid copying the buffer strings
          ;; back and forth between buffers.
          (jupyter-with-repl-lang-buffer
            (insert code)
            (goto-char pos)
            (let ((tick (buffer-chars-modified-tick)))
              (jupyter-indent-line)
              (unless (eq tick (buffer-chars-modified-tick))
                (setq pos (point))
                (buffer-string))))))
    ;; Don't modify the buffer when unnecessary, this allows
    ;; `company-indent-or-complete-common' to work.
    (when replacement
      (jupyter-repl-replace-cell-code replacement)
      (goto-char (+ pos (jupyter-repl-cell-code-beginning-position))))))

;;; Buffer change functions

(defun jupyter-repl-insert-continuation-prompts (bound)
  "Insert continuation prompts if needed, stopping at BOUND.
Return the new BOUND since inserting continuation prompts may add
more characters than were initially in the buffer."
  (setq bound (set-marker (make-marker) bound))
  (set-marker-insertion-type bound t)
  (while (and (< (point) bound)
              (search-forward "\n" bound 'noerror))
    (delete-char -1)
    (jupyter-repl-insert-prompt 'continuation))
  (prog1 (marker-position bound)
    (set-marker bound nil)))

(defun jupyter-repl-mark-as-cell-code (beg end)
  "Add the field property to text between (BEG . END) if within a code cell."
  ;; Handle field boundary at the front of the cell code
  (when (= beg (jupyter-repl-cell-code-beginning-position))
    (put-text-property beg (1+ beg) 'front-sticky t))
  (when (text-property-not-all beg end 'field 'cell-code)
    (font-lock-fillin-text-property beg end 'field 'cell-code)))

(defun jupyter-repl-do-after-change (beg end len)
  "Call `jupyter-repl-after-change' when the current cell code is changed.
`jupyter-repl-after-change' is only called when BEG is a position
on a `jupyter-repl-cell-line-p'. BEG, END, and LEN have the same
meaning as in `after-change-functions'."
  (when (eq major-mode 'jupyter-repl-mode)
    (with-demoted-errors "Jupyter error after buffer change: %S"
      (when (save-excursion
              (goto-char beg)
              (jupyter-repl-cell-line-p))
        (cond
         ((= len 0)
          (jupyter-repl-after-change 'insert beg end))
         ((and (= beg end) (not (zerop len)))
          (jupyter-repl-after-change 'delete beg len)))))))

(cl-defgeneric jupyter-repl-after-change (_type _beg _end-or-len)
  "Called from the `after-change-functions' of a REPL buffer.
Modify the text just inserted or deleted. TYPE is either insert
or delete to signify if the change was due to insertion or
deletion of text. BEG is always the beginning of the insertion or
deletion. END-OR-LEN is the end of the insertion when TYPE is
insert and is the length of the deleted text when TYPE is delete.

The `after-change-functions' of the REPL buffer are only called
for changes to input cells and not for output generated by the
kernel.

Note, the overriding method should call `cl-call-next-method'."
  nil)

(cl-defmethod jupyter-repl-after-change ((_type (eql insert)) beg end)
  (goto-char beg)
  ;; Avoid doing anything on self insertion
  (unless (and (= (point) (1- end))
               (not (eq (char-after) ?\n)))
    (setq end (jupyter-repl-insert-continuation-prompts end)))
  (jupyter-repl-mark-as-cell-code beg end)
  (goto-char end))

(defun jupyter-repl-kill-buffer-query-function ()
  "Ask before killing a Jupyter REPL buffer.
If the REPL buffer is killed, stop the client. If the REPL client
is connected to a kernel with a kernel manager, kill the kernel.

In addition, exit `jupyter-repl-interaction-mode' in all buffers
associated with the REPL. See `jupyter-repl-associate-buffer'."
  (when (eq major-mode 'jupyter-repl-mode)
    (if (not (jupyter-channels-running-p jupyter-current-client)) t
      (when (y-or-n-p
             (format "Jupyter REPL (%s) still connected. Kill it? "
                     (buffer-name (current-buffer))))
        ;; TODO: Handle case when multiple clients are connected, i.e. do we
        ;; want to also delete a kernel if this is the last client connected.
        ;; See `eieio-instance-tracker'.
        (prog1 t
          (jupyter-stop-channels jupyter-current-client)
          (cl-loop
           with client = jupyter-current-client
           for buffer in (buffer-list)
           do (with-current-buffer buffer
                (when (eq jupyter-current-client client)
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

(cl-defmethod jupyter-code-context ((_type (eql completion))
                                    &context (major-mode jupyter-repl-mode))
  (list (jupyter-repl-cell-code)
        (1- (jupyter-repl-cell-code-position))))

(cl-defmethod jupyter-completion-prefix (&context (major-mode jupyter-repl-mode))
  (and (not (get-text-property (point) 'read-only))
       (cl-call-next-method)))

;;; Evaluation

(cl-defmethod jupyter-read-expression (&context ((and
                                                  jupyter-current-client
                                                  (object-of-class-p
                                                   jupyter-current-client
                                                   'jupyter-repl-client)
                                                  t)
                                                 (eql t)))
  (jupyter-with-repl-buffer jupyter-current-client
    (jupyter-set jupyter-current-client 'jupyter-eval-expression-history
                 (ring-elements jupyter-repl-history))
    (cl-call-next-method)))

;;; Kernel management

(defun jupyter-repl-on-kernel-restart (client msg)
  "Update the REPL buffer after CLIENT restarts.
If MSG is a startup message, insert the banner of the kernel,
synchronize the execution state, and insert a new input prompt."
  (prog1 nil
    (when (jupyter-message-status-starting-p msg)
      (jupyter-with-repl-buffer client
        ;; FIXME: Don't assume `jupyter-include-other-output' was previously nil
        (jupyter-set jupyter-current-client 'jupyter-include-other-output nil)
        (jupyter-repl-without-continuation-prompts
         (goto-char (point-max))
         (jupyter-repl-previous-cell)
         (jupyter-repl-finalize-cell nil)
         (jupyter-repl-newline)
         (jupyter-repl-insert-banner
          (plist-get (jupyter-kernel-info client) :banner))
         (jupyter-repl-sync-execution-state)
         (jupyter-repl-insert-prompt 'in))))))

(defun jupyter-repl-interrupt-kernel ()
  "Interrupt the kernel if possible.
A kernel can be interrupted if it was started using a kernel
manager. See `jupyter-start-new-kernel'."
  (interactive)
  (if (not (jupyter-repl-client-has-manager-p))
      (user-error "Cannot interrupt non-subprocess kernels")
    (message "Interrupting kernel")
    (jupyter-interrupt-kernel
     (oref jupyter-current-client manager))))

;; TODO: Make timeouts configurable
(defun jupyter-repl-restart-kernel (&optional shutdown)
  "Restart the kernel.
With a prefix argument, SHUTDOWN the kernel completely instead."
  (interactive "P")
  (unless shutdown
    ;; This may have been set to t due to a non-responsive kernel so make sure
    ;; that we try again when restarting.
    (setq jupyter-repl-use-builtin-is-complete nil)
    ;; When restarting, the startup message is not associated with any request
    ;; so ensure that we are able to capture it.
    (jupyter-set jupyter-current-client 'jupyter-include-other-output t))
  (if (jupyter-repl-client-has-manager-p)
      (let ((manager (oref jupyter-current-client manager)))
        (cond
         ((jupyter-kernel-alive-p manager)
          (message "%s kernel..." (if shutdown "Shutting down"
                                    "Restarting"))
          (jupyter-shutdown-kernel manager (not shutdown)))
         (t
          (message "Starting dead kernel...")
          (jupyter-start-kernel manager))))
    (unless (jupyter-wait-until-received :shutdown-reply
              (jupyter-send-shutdown-request jupyter-current-client
                :restart (not shutdown)))
      (jupyter-set jupyter-current-client 'jupyter-include-other-output nil)
      (message "Kernel did not respond to shutdown request"))))

(defun jupyter-repl-display-kernel-buffer ()
  "Display the kernel processes stdout."
  (interactive)
  (if (jupyter-repl-client-has-manager-p)
      (let ((manager (oref jupyter-current-client manager)))
        (if (jupyter-kernel-alive-p manager)
            (display-buffer (process-buffer (oref manager kernel)))
          (error "Kernel is not alive")))
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
      (setq isearch-lazy-highlight-start-limit
            (jupyter-repl-cell-beginning-position))
      (or
       ;; 1. First try searching in the initial cell text
       (funcall search-fun string
                (or bound
                    (unless isearch-forward
                      (jupyter-repl-cell-code-beginning-position)))
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
                 ;; After putting the next/prev history element, search the
                 ;; string in them again, until an error is thrown at the
                 ;; beginning/end of history.
                 (setq found (funcall search-fun string
                                      (unless isearch-forward
                                        (jupyter-repl-cell-code-beginning-position))
                                      'noerror)))
               ;; Return point of the new search result
               (point))
           (error
            (unless noerror
              (signal (car err) (cdr err))))))))))

(defun jupyter-repl-history-isearch-wrap ()
  "Wrap the input history search when search fails.
Go to the oldest history element for a forward search or to the
newest history element for a backward search."
  (if isearch-forward
      (jupyter-repl-history--previous (ring-length jupyter-repl-history))
    (jupyter-repl-history--next (ring-length jupyter-repl-history)))
  (jupyter-repl-replace-cell-code (ring-ref jupyter-repl-history 0))
  (goto-char (if isearch-forward (jupyter-repl-cell-code-beginning-position)
               (point-max))))

(defun jupyter-repl-history-isearch-push-state ()
  "Save a function restoring the state of input history search.
Save the element at index 0 in `jupyter-repl-history'. When
restoring the state, the `jupyter-repl-history' ring is rotated,
in the appropriate direction, to the saved element."
  (let ((code (jupyter-repl-cell-code)))
    (cond
     ((equal code (ring-ref jupyter-repl-history 0))
      (let ((elem (ring-ref jupyter-repl-history 0)))
        (lambda (_cmd)
          (when isearch-wrapped
            (if isearch-forward
                (jupyter-repl-history--next (ring-length jupyter-repl-history))
              (jupyter-repl-history--previous (ring-length jupyter-repl-history))))
          (while (not (eq (ring-ref jupyter-repl-history 0) elem))
            (if isearch-forward
                (jupyter-repl-history--previous 1)
              (jupyter-repl-history--next 1)))
          (jupyter-repl-replace-cell-code (ring-ref jupyter-repl-history 0)))))
     (t
      (let ((elem code))
        (lambda (_cmd)
          (jupyter-repl-replace-cell-code elem)))))))

;;; `jupyter-repl-mode'

(defun jupyter-repl-scratch-buffer ()
  "Display a scratch buffer associated with the current REPL buffer."
  (interactive)
  (if (jupyter-repl-connected-p)
      (let* ((client jupyter-current-client)
             (name (format "*jupyter-scratch[session=%s]*"
                           (truncate-string-to-width
                            (jupyter-session-id (oref client session))
                            9 nil nil "…"))))
        (unless (get-buffer name)
          (with-current-buffer (get-buffer-create name)
            (funcall (jupyter-repl-language-mode client))
            (jupyter-repl-associate-buffer client)
            (insert
             (substitute-command-keys
              "Jupyter scratch buffer for evaluation.
\\[jupyter-eval-line-or-region] to evaluate the line or region.
\\[jupyter-eval-buffer] to evaluate the whole buffer.
\\[jupyter-repl-pop-to-buffer] to show the REPL buffer."))
            (comment-region (point-min) (point-max))
            (insert "\n\n")))
        (switch-to-buffer-other-window name))
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
  (cl-check-type jupyter-current-client jupyter-repl-client)
  ;; This is a better setting when rendering HTML tables
  (setq-local truncate-lines t)
  (setq-local indent-line-function #'jupyter-repl-indent-line)
  (setq-local left-margin-width jupyter-repl-prompt-margin-width)
  ;; Initialize a buffer using the major-mode correponding to the kernel's
  ;; language. This will be used for indentation and to capture font lock
  ;; properties.
  (let* ((info (jupyter-kernel-info jupyter-current-client))
         (language-info (plist-get info :language_info))
         (language (plist-get language-info :name)))
    (jupyter-load-language-support jupyter-current-client)
    (cl-destructuring-bind (mode syntax)
        (jupyter-repl-kernel-language-mode-properties language-info)
      (setq jupyter-repl-lang-mode mode)
      (setq jupyter-repl-lang-buffer
            (get-buffer-create
             (format " *jupyter-repl-lang-%s*" language)))
      (set-syntax-table syntax)
      (jupyter-with-repl-lang-buffer
        (unless (eq major-mode mode)
          (funcall mode))))
    ;; Get history from kernel
    (setq jupyter-repl-history
          (make-ring (1+ jupyter-repl-history-maximum-length)))
    ;; The sentinel value keeps track of the newest/oldest elements of the
    ;; history since next/previous navigation is implemented by rotations on the
    ;; ring.
    (ring-insert jupyter-repl-history 'jupyter-repl-history)
    (let ((jupyter-inhibit-handlers '(:status)))
      (jupyter-send-history-request jupyter-current-client
        :n jupyter-repl-history-maximum-length :raw nil :unique t))
    (erase-buffer)
    ;; Add local hooks
    (add-hook 'kill-buffer-query-functions #'jupyter-repl-kill-buffer-query-function nil t)
    (add-hook 'after-change-functions 'jupyter-repl-do-after-change nil t)
    (add-hook 'pre-redisplay-functions 'jupyter-repl-preserve-window-margins nil t)
    ;; Initialize the REPL
    (jupyter-repl-initialize-hooks)
    (jupyter-repl-initialize-fontification)
    (jupyter-repl-isearch-setup)
    (jupyter-repl-sync-execution-state)
    (jupyter-repl-interaction-mode)))

(cl-defgeneric jupyter-repl-after-init ()
  "Hook function called whenever `jupyter-repl-mode' is enabled/disabled.
You may override this function for a particular language using a
jupyter-lang &context specializer. For example, to do something
when the language of the REPL is python the method signature
would look like

    (cl-defmethod jupyter-repl-after-init (&context (jupyter-lang python)))"
  nil)

(add-hook 'jupyter-repl-mode-hook 'jupyter-repl-after-init)

(defun jupyter-repl-initialize-hooks ()
  "Initialize startup hooks.
When the kernel restarts, insert a new prompt."
  ;; NOTE: This hook will only run if `jupyter-include-other-output' is non-nil
  ;; during the restart.
  (jupyter-add-hook jupyter-current-client 'jupyter-iopub-message-hook
    #'jupyter-repl-on-kernel-restart))

(defun jupyter-repl-font-lock-fontify-region (fontify-fun beg end &optional verbose)
  "Use FONTIFY-FUN to fontify input cells between BEG and END.
VERBOSE has the same meaning as in
`font-lock-fontify-region-function'."
  (let ((start beg) next)
    (while (/= start end)
      (cond
       ((eq (get-text-property start 'field) 'cell-code)
        (setq next (min end (field-end start)))
        ;; It is OK that we do not update BEG and END using the return value of
        ;; this function as long as the default value of
        ;; `font-lock-extend-region-functions' is used since an input cell
        ;; always starts at the beginning of a line and ends at the end of a
        ;; line and does not use the font-lock-multiline property (2018-12-20).
        (funcall fontify-fun start next verbose))
       (t
        (setq next (or (text-property-any start end 'field 'cell-code) end))
        ;; Unfontify the region mainly to remove the font-lock-multiline
        ;; property in the output, e.g. added by markdown. These regions will
        ;; get highlighted syntactically in some scenarios.
        (font-lock-unfontify-region start next)))
      (setq start next)))
  `(jit-lock-bounds ,beg . ,end))

(defun jupyter-repl-initialize-fontification ()
  "Initialize fontification for the current REPL buffer."
  (let (fld sff spf comment)
    (jupyter-with-repl-lang-buffer
      ;; TODO: Take into account minor modes that may add to
      ;; `font-lock-keywords', e.g. `rainbow-delimiters-mode'.
      (setq fld font-lock-defaults
            frf font-lock-fontify-region-function
            sff font-lock-syntactic-face-function
            spf syntax-propertize-function
            comment comment-start))
    ;; Set `font-lock-defaults' to a copy of the font lock defaults for the
    ;; REPL language but with a modified syntactic fontification function
    (cl-destructuring-bind (kws &optional kws-only case-fold syntax-alist
                                &rest vars)
        fld
      (setq vars (append vars
                         (list
                          ;; Needed to ensure that " characters are not treated
                          ;; syntactically in cell output
                          (cons 'parse-sexp-lookup-properties t)
                          (cons 'syntax-propertize-function spf)
                          (cons 'font-lock-fontify-region-function
                                (apply-partially 'jupyter-repl-font-lock-fontify-region
                                                 'font-lock-default-fontify-region))
                          (cons 'font-lock-syntactic-face-function sff))))
      (setq-local comment-start comment)
      (setq font-lock-defaults
            (apply #'list kws kws-only case-fold syntax-alist vars)))
    (font-lock-mode)))

(defun jupyter-repl-insert-banner (banner)
  "Insert BANNER into the `current-buffer'.
Make the text of BANNER read only and apply the `shadow' face to
it."
  (jupyter-repl-without-continuation-prompts
   (let ((start (point)))
     (jupyter-repl-insert banner)
     (jupyter-repl-newline)
     (add-text-properties start (point) '(font-lock-face
                                          shadow fontified t font-lock-fontified t)))))

(defun jupyter-repl-sync-execution-state ()
  "Synchronize the `jupyter-current-client's kernel state."
  (let* ((client jupyter-current-client)
         (req (let ((jupyter-inhibit-handlers t))
                (jupyter-send-execute-request client :code "" :silent t))))
    (jupyter-add-callback req
      :execute-reply (lambda (msg)
                       (oset client execution-count
                             (1+ (jupyter-message-get msg :execution_count)))
                       (unless (equal (jupyter-execution-state client) "busy")
                         ;; Set the cell count and update the prompt
                         (jupyter-with-repl-buffer client
                           (save-excursion
                             (goto-char (point-max))
                             (jupyter-repl-update-cell-count
                              (oref client execution-count)))))))
    ;; Waiting longer here to account for initial startup of the Jupyter
    ;; kernel. Sometimes the idle message won't be received if another long
    ;; running execute request is sent right after.
    (jupyter-wait-until-idle req jupyter-long-timeout)))

;;; `jupyter-repl-interaction-mode'

(defun jupyter-repl-pop-to-buffer ()
  "Switch to the REPL buffer of the `jupyter-current-client'."
  (interactive)
  (if jupyter-current-client
      (jupyter-with-repl-buffer jupyter-current-client
        (goto-char (point-max))
        (pop-to-buffer (current-buffer)))
    (error "Buffer not associated with a REPL, see `jupyter-repl-associate-buffer'")))

(defun jupyter-repl-available-repl-buffers (&optional mode first)
  "Return a list of REPL buffers that are connected to live kernels.
If MODE is non-nil, return all REPL buffers whose
`jupyter-repl-lang-mode' is MODE.

If FIRST is non-nil, only return the first REPL buffer that matches."
  (cl-loop
   for client in (jupyter-clients)
   for match = (when (and (jupyter-repl-client-p client)
                          (buffer-live-p (oref client buffer)))
                 (with-current-buffer (oref client buffer)
                   (and (or (null mode) (eq mode jupyter-repl-lang-mode))
                        (jupyter-repl-connected-p)
                        (buffer-name))))
   if (and match first) return (oref client buffer)
   else if match collect (oref client buffer)))

;;;###autoload
(defun jupyter-repl-associate-buffer (client)
  "Associate the `current-buffer' with a REPL CLIENT.
If the `major-mode' of the `current-buffer' is the
`jupyter-repl-lang-mode' of CLIENT, enable
`jupyter-repl-interaction-mode'.

CLIENT should be a `jupyter-repl-client' or a subclass thereof.
If CLIENT is a buffer or the name of a buffer, use the
`jupyter-current-client' local to the buffer."
  (interactive
   (list
    (let ((repls (mapcar 'buffer-name (jupyter-repl-available-repl-buffers major-mode))))
      (when repls
        (with-current-buffer
            (completing-read "jupyter-repl: " repls nil t)
          jupyter-current-client)))))
  (if (not client)
      (when (y-or-n-p "No REPL for `major-mode' exists. Start one? ")
        (call-interactively #'jupyter-run-repl))
    (setq client (if (or (bufferp client) (stringp client))
                     (with-current-buffer client
                       jupyter-current-client)
                   client))
    (cl-check-type client jupyter-repl-client)
    (unless (eq (jupyter-repl-language-mode client) major-mode)
      (error "Cannot associate buffer to REPL. Wrong `major-mode'"))
    (setq-local jupyter-current-client client)
    (unless jupyter-repl-interaction-mode
      (jupyter-repl-interaction-mode))))

(defvar jupyter-repl-interaction-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-e") #'jupyter-eval-line-or-region)
    (define-key map (kbd "C-c C-c") #'jupyter-eval-line-or-region)
    (define-key map (kbd "C-M-x") #'jupyter-eval-defun)
    (define-key map (kbd "C-c C-s") #'jupyter-repl-scratch-buffer)
    (define-key map (kbd "C-c C-b") #'jupyter-eval-buffer)
    (define-key map (kbd "C-c C-l") #'jupyter-load-file)
    (define-key map (kbd "C-c M-:") #'jupyter-eval-string)
    (define-key map (kbd "M-i") #'jupyter-inspect-at-point)
    (define-key map (kbd "C-c C-r") #'jupyter-repl-restart-kernel)
    (define-key map (kbd "C-c C-i") #'jupyter-repl-interrupt-kernel)
    (define-key map (kbd "C-c C-z") #'jupyter-repl-pop-to-buffer)
    map))

(defun jupyter-repl-propagate-client (win-or-buffer &rest args)
  "Propagate the `jupyter-current-client' to other buffers.
WIN-OR-BUFFER is either a window that will display the buffer
that should be checked or the buffer itself. In the case that
WIN-OR-BUFFER is a window, the first element of ARGS is assumed
to be the buffer to check.

If the checked buffer is not in `jupyter-repl-interaction-mode'
and has the same `major-mode' as the `jupyter-current-client's
language mode, set `jupyter-current-client' in the checked buffer
to the same value as the `jupyter-current-client' of the
`current-buffer'.

If the `current-buffer' is not in
`jupyter-repl-interaction-mode', use the first buffer returned by
`jupyter-repl-available-repl-buffers' for the checked buffer's
`major-mode' as the buffer from which to propagate its value of
`jupyter-current-client'.

NOTE: Only intended to be added as advice to `switch-to-buffer',
`display-buffer', or `set-window-buffer'."
  (let* ((other-buffer (if (or (null win-or-buffer)
                               (windowp win-or-buffer))
                           (car args)
                         win-or-buffer))
         (buffer (or (and jupyter-repl-interaction-mode (current-buffer))
                     (and (buffer-live-p other-buffer)
                          (jupyter-repl-available-repl-buffers
                           (with-current-buffer other-buffer major-mode)
                           'first)))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((client jupyter-current-client)
              (mode (if (eq major-mode 'jupyter-repl-mode)
                        (jupyter-repl-language-mode jupyter-current-client)
                      major-mode)))
          (with-current-buffer other-buffer
            (when (and (eq mode major-mode)
                       (not jupyter-repl-interaction-mode))
              (jupyter-repl-associate-buffer client))))))))

(defun jupyter-repl-interaction-mode-reenable ()
  (when (and (not jupyter-repl-interaction-mode)
             (jupyter-repl-client-p jupyter-current-client)
             (eq (jupyter-repl-language-mode jupyter-current-client) major-mode))
    (jupyter-repl-interaction-mode)))

(defun jupyter-repl-interaction-mode-line ()
  "Return a mode line string with the status of the kernel.
'*' means the kernel is busy, '-' means the kernel is idle and
the REPL is connected, 'x' means the REPL is disconnected
from the kernel."
  (and (jupyter-repl-client-p jupyter-current-client)
       (concat " JuPy["
               (if (equal (jupyter-execution-state jupyter-current-client) "busy")
                   "*"
                 (if (jupyter-hb-beating-p jupyter-current-client)
                     "-"
                   "x"))
               "]")))

(define-minor-mode jupyter-repl-interaction-mode
  "Minor mode for interacting with a Jupyter REPL.
When this minor mode is enabled you may evaluate code from the
current buffer using the associated REPL (see
`jupyter-repl-associate-buffer' to associate a REPL).

In addition any new buffers opened with the same `major-mode' as
the `current-buffer' will automatically have
`jupyter-repl-interaction-mode' enabled for them.

\\{jupyter-repl-interaction-map}"
  :group 'jupyter-repl
  :lighter '(:eval (jupyter-repl-interaction-mode-line))
  :init-value nil
  :keymap jupyter-repl-interaction-map
  (cond
   (jupyter-repl-interaction-mode
    (add-hook 'completion-at-point-functions 'jupyter-completion-at-point nil t)
    (add-hook 'after-revert-hook 'jupyter-repl-interaction-mode nil t))
   (t
    (remove-hook 'completion-at-point-functions 'jupyter-completion-at-point t)
    (remove-hook 'after-revert-hook 'jupyter-repl-interaction-mode t)
    (unless (eq major-mode 'jupyter-repl-mode)
      (kill-local-variable 'jupyter-current-client)))))

;;; `jupyter-repl-persistent-mode'

(define-minor-mode jupyter-repl-persistent-mode
  "Global minor mode to persist Jupyter REPL connections.
When this minor mode is enabled, the `jupyter-current-client' of
a buffer in `jupyter-repl-interaction-mode' is propogated to any
other buffers switched to that have the same `major-mode' as the
`current-buffer'."
  :group 'jupyter-repl
  :global t
  :keymap nil
  :init-value nil
  (cond
   (jupyter-repl-persistent-mode
    (advice-add 'switch-to-buffer :before #'jupyter-repl-propagate-client)
    (advice-add 'display-buffer :before #'jupyter-repl-propagate-client)
    (advice-add 'set-window-buffer :before #'jupyter-repl-propagate-client)
    (add-hook 'after-change-major-mode-hook 'jupyter-repl-interaction-mode-reenable))
   (t
    (advice-remove 'switch-to-buffer #'jupyter-repl-propagate-client)
    (advice-remove 'display-buffer #'jupyter-repl-propagate-client)
    (advice-remove 'set-window-buffer #'jupyter-repl-propagate-client)
    (remove-hook 'after-change-major-mode-hook 'jupyter-repl-interaction-mode-reenable))))

;;; Starting a REPL

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
    (with-temp-buffer
      (let ((buffer-file-name
             (concat "jupyter-repl-lang" file_extension)))
        (delay-mode-hooks (set-auto-mode)))
      (list major-mode (syntax-table)))))

(defun jupyter-repl--new-repl (client &optional repl-name)
  "Initialize a new REPL buffer based on CLIENT.
CLIENT is a REPL client already connected to its kernel and has a
non-nil kernel-info slot.

A new REPL buffer communicating with CLIENT's kernel is created
and set as CLIENT's buffer slot. If CLIENT already has a non-nil
buffer slot, raise an error.

REPL-NAME is a string that will be used to generate the buffer
name. If nil or empty, a default will be used."
  (unless jupyter-repl-persistent-mode (jupyter-repl-persistent-mode))
  (if (oref client buffer) (error "Client already has a REPL buffer")
    (cl-destructuring-bind (&key language_info
                                 banner
                                 &allow-other-keys)
        (jupyter-kernel-info client)
      (let ((language-name (plist-get language_info :name))
            (language-version (plist-get language_info :version)))
        (oset client buffer
              (generate-new-buffer
               (format "*jupyter-repl[%s]*"
                       (if (zerop (length repl-name))
                           (concat language-name " " language-version)
                         repl-name))))
        (jupyter-with-repl-buffer client
          (setq-local jupyter-current-client client)
          (jupyter-repl-mode)
          (jupyter-repl-insert-banner banner)
          (jupyter-repl-insert-prompt 'in))))))

;;;###autoload
(defun jupyter-run-repl (kernel-name &optional repl-name associate-buffer client-class display)
  "Run a Jupyter REPL connected to a kernel with name, KERNEL-NAME.
KERNEL-NAME will be passed to `jupyter-find-kernelspecs' and the
first kernel found will be used to start the new kernel.

With a prefix argument give a new REPL-NAME for the REPL.

Optional argument ASSOCIATE-BUFFER, if non-nil, means to enable
`jupyter-repl-interaction-mode' in the `current-buffer' and
associate it with the REPL created. When called interactively,
ASSOCIATE-BUFFER is set to t. If the `current-buffer's
`major-mode' does not correspond to the language of the kernel
started, ASSOCIATE-BUFFER has no effect.

Optional argument CLIENT-CLASS is the class that will be passed
to `jupyter-start-new-kernel' and should be a subclass of
`jupyter-repl-client', which is the default.

When called interactively, DISPLAY the new REPL buffer.
Otherwise, in a non-interactive call, return the
`jupyter-repl-client' connect to the kernel."
  (interactive (list (car (jupyter-completing-read-kernelspec
                           nil current-prefix-arg))
                     (when current-prefix-arg
                       (read-string "REPL Name: "))
                     t nil t))
  (or client-class (setq client-class 'jupyter-repl-client))
  (unless (called-interactively-p 'interactive)
    (setq kernel-name (caar (jupyter-find-kernelspecs kernel-name))))
  (unless kernel-name
    (error "No kernel found for prefix (%s)" kernel-name))
  (unless (child-of-class-p client-class 'jupyter-repl-client)
    (error "Class should be a subclass of `jupyter-repl-client' (`%s')" client-class))
  (cl-destructuring-bind (_manager client)
      (jupyter-start-new-kernel kernel-name client-class)
    (jupyter-repl--new-repl client repl-name)
    ;; TODO: An alist mapping kernel languages to their
    ;; corresponding major modes in Emacs. This ways we can
    ;; error out earlier before starting the REPL. The
    ;; reason why this can't be done is because we use the
    ;; extension key of the kernel-info to get the major
    ;; mode using `auto-mode-alist'. See
    ;; `jupyter-repl-kernel-language-mode-properties'.
    (when (and associate-buffer
               (eq major-mode (jupyter-repl-language-mode client)))
      (jupyter-repl-associate-buffer client))
    (when display
      (pop-to-buffer (oref client buffer)))
    client))

;;;###autoload
(defun jupyter-connect-repl (file-or-plist &optional repl-name associate-buffer client-class display)
  "Run a Jupyter REPL using a kernel's connection FILE-OR-PLIST.
FILE-OR-PLIST can be either a file holding the connection
information or a property list of connection information.
ASSOCIATE-BUFFER has the same meaning as in `jupyter-run-repl'.

With a prefix argument give a new REPL-NAME for the REPL.

Optional argument CLIENT-CLASS is the class of the client that
will be used to initialize the REPL and should be a subclass of
`jupyter-repl-client', which is the default.

Return the `jupyter-repl-client' connected to the kernel. When
called interactively, DISPLAY the new REPL buffer as well."
  (interactive (list (read-file-name "Connection file: ")
                     (when current-prefix-arg
                       (read-string "REPL Name: "))
                     t nil t))
  (or client-class (setq client-class 'jupyter-repl-client))
  (unless (child-of-class-p client-class 'jupyter-repl-client)
    (error "Class should be a subclass of `jupyter-repl-client' (`%s')" client-class))
  (let ((client (make-instance client-class)))
    (jupyter-initialize-connection client file-or-plist)
    (jupyter-start-channels client)
    (jupyter-repl--new-repl client repl-name)
    (when (and associate-buffer
               (eq major-mode (jupyter-repl-language-mode client)))
      (jupyter-repl-associate-buffer client))
    (when display
      (pop-to-buffer (oref client buffer)))
    client))

(provide 'jupyter-repl)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; jupyter-repl.el ends here
