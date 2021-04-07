;;; jupyter-repl-client.el --- A Jupyter REPL client -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 08 Jan 2018

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
;; `major-mode'.  Associating a buffer with a REPL client enables the minor
;; mode `jupyter-repl-interaction-mode'.
;;
;; `jupyter-repl-interaction-mode' adds the following keybindings for
;; interacting with a REPL client:
;;
;;     C-c C-c `jupyter-eval-line-or-region'
;;     C-c C-l `jupyter-eval-file'
;;     M-i `jupyter-inspect-at-point'
;;     C-c C-r `jupyter-repl-restart-kernel'
;;     C-c C-i `jupyter-repl-interrupt-kernel'
;;     C-c C-z `jupyter-repl-pop-to-buffer'

;;; Code:

(defgroup jupyter-repl nil
  "A Jupyter REPL client"
  :group 'jupyter)

(eval-when-compile (require 'subr-x))
(require 'jupyter-base)
(require 'jupyter-mime)
(require 'jupyter-client)
(require 'jupyter-kernelspec)
(require 'jupyter-widget-client)
(require 'jupyter-kernel-manager)
(require 'ring)

(declare-function jupyter-start-new-kernel "jupyter-kernel-process-manager")

;; TODO: Define `jupyter-kernel-manager-after-restart-hook' to update the
;; execution count after a restart.  More generally, define more ways to hook
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
     :background "LightYellow2")
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

(defcustom jupyter-repl-cell-pre-send-hook nil
  "Hook run before sending the contents of an input cell to a kernel.
The hook is run with `point' at the cell code beginning position
and before the contents of the cell are extracted from the buffer
for sending to the kernel."
  :type 'hook
  :group 'jupyter-repl)

(defcustom jupyter-repl-cell-post-send-hook nil
  "Hook run after sending the contents of an input cell to a kernel.
The hook is run with `point' at the cell code beginning
position."
  :type 'hook
  :group 'jupyter-repl)

(defcustom jupyter-repl-allow-RET-when-busy nil
  "Allow RET to insert a newline when the kernel is busy.
Normally when the kernel is busy, pressing RET at an input cell
is disallowed.  This is because, when the kernel is busy, it does
not respond to an `:is-complete-request' message and that message
is used to avoid sending incomplete code to the kernel.

If this variable is non-nil, RET is allowed to insert a newline.
In this case, pressing RET on an empty line, i.e.  RET RET, will
send the code to the kernel."
  :type 'boolean
  :group 'jupyter-repl)

(defcustom jupyter-repl-echo-eval-p nil
  "Copy evaluation input to a REPL cell if non-nil.
If non-nil, and when calling the `jupyter-eval-*' functions like
`jupyter-eval-line-or-region', copy the input into a REPL cell.
Otherwise the evaluation request is sent to the kernel without
displaying the code of the request in the REPL.

Note, output generated by requests will always be sent to the
REPL buffer whenever this variable is non-nil.  When the REPL
buffer isn't visible, output will also be sent to pop-up buffers
as is done when this variable is nil."
  :type 'boolean
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
current output of the cell.  Set when the kernel sends a
`:clear-output' message.")))

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
  "Switch to CLIENT's REPL buffer and evaluate BODY.
`inhibit-read-only' is let bound to t while evaluating
BODY.  After evaluation, if the current buffer is visible in some
window, set the window point to the value of `point' in the
buffer."
  (declare (indent 1) (debug (symbolp &rest form)))
  `(with-current-buffer (oref ,client buffer)
     (let ((inhibit-read-only t))
       (prog1 (progn ,@body)
         (let ((win (get-buffer-window)))
           (when win (set-window-point win (point))))))))

(defvar jupyter-repl-inhibit-continuation-prompts nil
  "Non-nil when continuation prompts are suppressed.
See `jupyter-repl-insert-continuation-prompts'.")

(defmacro jupyter-repl-without-continuation-prompts (&rest body)
  "Evaluate BODY without inserting continuation prompts."
  (declare (debug (&rest form)))
  `(let ((jupyter-repl-inhibit-continuation-prompts t))
     ,@body))

(defmacro jupyter-repl-append-output (client req &rest body)
  "Switch to CLIENT's buffer, move to the end of REQ, and evaluate BODY.
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
  "Evaluate BODY in the `jupyter-repl-lang-buffer' of the `current-buffer'.
The contents of `jupyter-repl-lang-buffer' is erased before
evaluating BODY."
  (declare (indent 0) (debug (&rest form)))
  (let ((client (make-symbol "clientvar")))
    `(let ((,client jupyter-current-client))
       (with-current-buffer jupyter-repl-lang-buffer
         (let ((inhibit-read-only t)
               (jupyter-current-client ,client))
           (erase-buffer)
           ,@body)))))

(defmacro jupyter-with-repl-cell (&rest body)
  "Narrow to the current cell, evaluate BODY, then widen.
The cell is narrowed to the region between and including
`jupyter-repl-cell-code-beginning-position' and
`jupyter-repl-cell-code-end-position'."
  (declare (indent 0) (debug (&rest form)))
  `(save-excursion
     (save-restriction
       (narrow-to-region (jupyter-repl-cell-code-beginning-position)
                         (jupyter-repl-cell-code-end-position))
       ,@body)))

(defmacro jupyter-repl-inhibit-undo-when (cond &rest body)
  "Evaluate BODY, disabling undo beforehand if COND is non-nil.
Undo is re-enabled after BODY is evaluated.

Note, any changes to `buffer-undo-list' during evaluation of BODY
will not be present when undo is re-enabled if COND is non-nil."
  (declare (indent 1) (debug ([&or symbolp form] &rest form)))
  (let ((new-undo-list (make-symbol "new"))
        (disable-undo (make-symbol "disable")))
    `(let ((,disable-undo ,cond) ,new-undo-list)
       (let ((buffer-undo-list (if ,disable-undo t buffer-undo-list)))
         (unwind-protect
             (progn ,@body)
           (unless ,disable-undo
             (setq ,new-undo-list buffer-undo-list))))
       (when ,new-undo-list
         (setq buffer-undo-list ,new-undo-list)))))

(defmacro jupyter-repl-with-single-undo (&rest body)
  "Evaluate BODY, remove all undo boundaries created during its evaluation."
  (declare (indent 0) (debug (&rest form)))
  (let ((handle (make-symbol "handle")))
    `(let ((,handle (prepare-change-group)))
       (unwind-protect
           (progn
             (activate-change-group ,handle)
             ,@body)
         (undo-amalgamate-change-group ,handle)
         (accept-change-group ,handle)))))

;;; Text insertion

(defun jupyter-repl-newline ()
  "Insert a read-only newline into the `current-buffer'."
  (insert (propertize "\n" 'read-only t)))

(cl-defmethod jupyter-insert :around (mime-or-plist
                                      &context (major-mode jupyter-repl-mode) &rest _ignore)
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

;;; Util

(defun jupyter-repl-completing-read-repl-buffer (&optional mode)
  "Return a REPL buffer, selecting from all available ones.
MODE has the same meaning as in
`jupyter-repl-available-repl-buffers'."
  (let* ((buffers (or (jupyter-repl-available-repl-buffers mode)
                      (error "No REPLs available")))
         (buffer (completing-read "REPL buffer: " (mapcar #'buffer-name buffers) nil t)))
    (when (equal buffer "")
      (error "No REPL buffer selected"))
    (get-buffer buffer)))

;;; Prompt

(defconst jupyter-repl-input-prompt-format "In [%d] ")
(defconst jupyter-repl-output-prompt-format "Out [%d] ")
(defconst jupyter-repl-busy-prompt "In [*] ")

(defsubst jupyter-repl--prompt-string (ov)
  (nth 0 (overlay-get ov 'jupyter-prompt)))

(defsubst jupyter-repl--prompt-face (ov)
  (nth 1 (overlay-get ov 'jupyter-prompt)))

(defun jupyter-repl--prompt-margin-alignment (str)
  (- jupyter-repl-prompt-margin-width (length str)))

(defun jupyter-repl--prompt-display-value (str face)
  "Return the margin display value for a prompt STR.
FACE is the `font-lock-face' to use for STR."
  (list '(margin left-margin)
        (propertize
         (concat
          (make-string (jupyter-repl--prompt-margin-alignment str) ?\s) str)
         'fontified t
         'font-lock-face face)))

(defun jupyter-repl--reset-prompt-display (ov)
  (when-let* ((prompt (jupyter-repl--prompt-string ov))
              (face (or (jupyter-repl--prompt-face ov)
                        'jupyter-repl-input-prompt))
              (md (jupyter-repl--prompt-display-value prompt face)))
    (overlay-put ov 'after-string (propertize " " 'display md))))

(defun jupyter-repl--reset-prompts ()
  "Re-calculate all prompt strings in the buffer.
Also set the local value of `left-margin-width' to
`jupyter-repl-prompt-margin-width'."
  (setq-local left-margin-width jupyter-repl-prompt-margin-width)
  (dolist (ov (overlays-in (point-min) (point-max)))
    (jupyter-repl--reset-prompt-display ov)))

(defun jupyter-repl--make-prompt (str face props)
  "Make a prompt overlay for the character before POS.
STR is used as the prompt string and FACE is its
`font-lock-face'.  Add PROPS as text properties to the character."
  (when (< (jupyter-repl--prompt-margin-alignment str) 0)
    (setq-local jupyter-repl-prompt-margin-width
                (+ jupyter-repl-prompt-margin-width
                   (abs (jupyter-repl--prompt-margin-alignment str))))
    (jupyter-repl--reset-prompts))
  (let ((ov (make-overlay (1- (point)) (point) nil t)))
    (overlay-put ov 'jupyter-prompt (list str face))
    (overlay-put ov 'evaporate t)
    (jupyter-repl--reset-prompt-display ov)
    (add-text-properties (overlay-start ov) (overlay-end ov) props)
    (overlay-recenter (point))))

(defun jupyter-repl-insert-prompt (&optional type)
  "Insert a REPL prompt according to TYPE.
TYPE can either be `in', `out', or `continuation'.  A nil TYPE is
interpreted as `in'."
  (setq type (or type 'in))
  (unless (memq type '(in out continuation))
    (error "Prompt type can only be (`in', `out', or `continuation')"))
  (jupyter-repl-without-continuation-prompts
   (let ((inhibit-read-only t))
     ;; The newline that `jupyter-repl--make-prompt' will overlay.
     (insert (propertize "\n" 'read-only (not (eq type 'continuation))))
     (cond
      ((eq type 'in)
       (let ((count (oref jupyter-current-client execution-count)))
         (jupyter-repl--make-prompt
          (format jupyter-repl-input-prompt-format count)
          'jupyter-repl-input-prompt
          `(jupyter-cell (beginning ,count))))
       ;; Prevent prompt overlay from inheriting text properties of code at the
       ;; beginning of a cell.
       ;;
       ;; rear-nonsticky is to prevent code inserted after this character to
       ;; inherit any of its text properties.
       ;;
       ;; front-sticky is to prevent `point' from being trapped between the
       ;; newline of the prompt overlay and this invisible character.
       (insert (propertize " "
                           'read-only t 'invisible t
                           'rear-nonsticky t 'front-sticky t))
       ;; The insertion of a new prompt starts a new cell, don't consider the
       ;; buffer modified anymore.  This is also an indicator for when undo's
       ;; can be made in the buffer.
       (set-buffer-modified-p nil)
       (setq buffer-undo-list '((t . 0))))
      ((eq type 'out)
       ;; Output is normally inserted by first going to the end of the output
       ;; for the request.  The end of the ouput for a request is at the
       ;; beginning of the next cell after the request which is why we get the
       ;; cell count of the previous cell
       (let ((count (jupyter-repl-previous-cell-count)))
         (jupyter-repl--make-prompt
          (format jupyter-repl-output-prompt-format count)
          'jupyter-repl-output-prompt
          `(jupyter-cell (out ,count))))
       ;; See the note above about the invisible character for input prompts
       (insert (propertize " " 'read-only t 'invisible t 'front-sticky t)))
      ((eq type 'continuation)
       (jupyter-repl--make-prompt
        ;; This needs to be two characters wide for some
        ;; reason, otherwise the continuation prompts will
        ;; be missing one character.
        "  " 'jupyter-repl-input-prompt
        `(read-only nil rear-nonsticky t)))))))

(defun jupyter-repl-prompt-string ()
  "Return the prompt string of the current input cell."
  (jupyter-repl--prompt-string
   (car (overlays-at (jupyter-repl-cell-beginning-position)))))

(defun jupyter-repl-cell-reset-prompt ()
  "Reset the current prompt back to its default."
  (jupyter-repl-cell-update-prompt
   (format jupyter-repl-input-prompt-format (jupyter-repl-cell-count))))

(defun jupyter-repl-cell-update-prompt (str &optional face)
  "Update the current cell's input prompt.
STR is the replacement prompt string.  If FACE is non-nil, it
should be a face that the prompt will use and defaults to
`jupyter-repl-input-prompt'."
  (when-let* ((ov (car (overlays-at (jupyter-repl-cell-beginning-position)))))
    (overlay-put ov 'jupyter-prompt (list str face))
    (jupyter-repl--reset-prompt-display ov)))

(defun jupyter-repl-cell-mark-busy ()
  "Mark the current cell as busy."
  (when (equal (jupyter-repl-prompt-string)
               (format jupyter-repl-input-prompt-format
                       (jupyter-repl-cell-count)))
    (jupyter-repl-cell-update-prompt jupyter-repl-busy-prompt)))

(defun jupyter-repl-cell-unmark-busy ()
  "Un-mark the current cell as busy."
  (when (equal (jupyter-repl-prompt-string) jupyter-repl-busy-prompt)
    (jupyter-repl-cell-update-prompt
     (format jupyter-repl-input-prompt-format
             (jupyter-repl-cell-count)))))

(defun jupyter-repl-update-cell-count (n)
  "Set the current cell count to N."
  (when (or (jupyter-repl-cell-beginning-p)
            (zerop (save-excursion (jupyter-repl-previous-cell))))
    (setf (nth 1 (get-text-property
                  (jupyter-repl-cell-beginning-position)
                  'jupyter-cell))
          n)
    (when (string-match-p "In \\[[0-9]+\\]" (jupyter-repl-prompt-string))
      (jupyter-repl-cell-reset-prompt))))

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
Move N times where N defaults to 1.  Return the count of cells
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
Move N times where N defaults to 1.  Return the count of cells
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
            ;; buffer.  This happens, for example, when erasing the buffer.
            (when (and (/= (point) starting-pos)
                       (jupyter-repl-cell-beginning-p (point)))
              (setq N (1- N)))
            (throw 'done t))))))
  N)

(defun jupyter-repl-goto-cell (req)
  "Go to the cell beginning position of REQ.
REQ should be a `jupyter-request' that corresponds to one of the
`jupyter-send-execute-request's created by a cell in the
`current-buffer'.  Note that the `current-buffer' is assumed to be
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

(defun jupyter-repl-map-cells (beg end input output)
  "Call INPUT or OUTPUT on the corresponding cells between BEG and END.
For every input or output cell between BEG and END, call INPUT or
OUTPUT, respectively, with the buffer narrowed to the cell.
INPUT and OUTPUT are functions of no arguments.

Note the narrowed regions may not be full input/output cells if
BEG and END are within an input/output cell."
  (declare (indent 2))
  (save-excursion
    (save-restriction
      (let (next)
        (while (/= beg end)
          (widen)
          (cond
           ((eq (get-text-property beg 'field) 'cell-code)
            (setq next (min end (field-end beg t)))
            (narrow-to-region beg next)
            (funcall input))
           (t
            (setq next (or (text-property-any
                            beg end 'field 'cell-code)
                           end))
            (narrow-to-region beg next)
            (funcall output)))
          (setq beg next))))))

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

(defun jupyter-repl-connected-p ()
  "Is the `jupyter-current-client' connected to its kernel?"
  (when jupyter-current-client
    (jupyter-kernel-alive-p jupyter-current-client)))

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
  "Finalize the current input cell.
REQ is the `jupyter-request' to associate with the current cell.
Place `point' at `point-max'."
  (goto-char (point-max))
  (let ((beg (jupyter-repl-cell-beginning-position))
        (count (jupyter-repl-cell-count)))
    (jupyter-repl-newline)
    (put-text-property (1- (point)) (point) 'jupyter-cell `(end ,count))
    (put-text-property beg (1+ beg) 'jupyter-request req)
    ;; Remove this property so that text can't be inserted at the start of the
    ;; cell or after any continuation prompts.  See
    ;; `jupyter-repl-insert-prompt'.
    (remove-text-properties beg (point) '(rear-nonsticky))
    (add-text-properties beg (point) '(read-only t))
    ;; reset the undo list so that a completed cell doesn't get undone.
    (setq buffer-undo-list '((t . 0)))))

(defun jupyter-repl-replace-cell-code (new-code)
  "Replace the current cell code with NEW-CODE.
If NEW-CODE is a buffer use `replace-buffer-contents' to replace
the cell code.  Otherwise NEW-CODE should be a string, the current
cell code will be erased and NEW-CODE inserted in its place."
  (if (bufferp new-code)
      (jupyter-with-repl-cell
        (jupyter-repl-with-single-undo
          ;; Need to create a single undo step here because
          ;; `replace-buffer-contents' adds in unwanted undo boundaries.
          ;;
          ;; Tests failing on Appveyor due to `replace-buffer-contents' not
          ;; supplying the right arguments to `after-change-functions' so call
          ;; the change functions manually.  Seen on Emacs 26.1.
          ;;
          ;; For reference see https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32278
          (let ((inhibit-modification-hooks t)
                (beg (point-min))
                (end (point-max))
                (new-len (with-current-buffer new-code
                           (- (point-max) (point-min)))))
            (run-hook-with-args
             'before-change-functions beg end)
            (replace-buffer-contents new-code)
            (run-hook-with-args
             'after-change-functions
             beg (+ beg new-len) (- end beg)))))
    (goto-char (jupyter-repl-cell-code-beginning-position))
    (delete-region (point) (jupyter-repl-cell-code-end-position))
    (insert-and-inherit new-code)))

(defun jupyter-repl-truncate-buffer ()
  "Truncate the `current-buffer' based on `jupyter-repl-maximum-size'.
The `current-buffer' is assumed to be a Jupyter REPL buffer.  If
the `current-buffer' is larger than `jupyter-repl-maximum-size'
lines, truncate it to something less than
`jupyter-repl-maximum-size' lines."
  (save-excursion
    (when (= (forward-line (- jupyter-repl-maximum-size)) 0)
      (jupyter-repl-next-cell)
      (delete-region (point-min) (point)))))

(defun jupyter-repl-clear-cells ()
  "Clear the input and output cells of the current buffer."
  (interactive)
  (jupyter-repl-without-continuation-prompts
   (let ((inhibit-read-only t))
     (save-excursion
       (goto-char (point-min))
       (when (get-text-property (point) 'jupyter-banner)
         (goto-char (next-single-property-change (point) 'jupyter-banner)))
       (delete-region (point) (point-max))
       (jupyter-repl-insert-prompt 'in))))
  (goto-char (point-max)))

;;; Handlers

(defun jupyter-repl-history-add (code)
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
      (save-excursion
        (goto-char (jupyter-repl-cell-code-beginning-position))
        (run-hooks 'jupyter-repl-cell-pre-send-hook))
      (setq code (string-trim (jupyter-repl-cell-code)))
      ;; Handle empty code cells as just an update of the prompt number
      (if (= (length code) 0)
          (setq silent t)
        ;; Needed by the prompt insertion below
        (oset client execution-count (1+ (oref client execution-count)))
        (jupyter-repl-history-add code))
      (let ((req (cl-call-next-method
                  client :code code :silent silent :store-history store-history
                  :user-expressions user-expressions :allow-stdin allow-stdin
                  :stop-on-error stop-on-error)))
        (jupyter-repl-without-continuation-prompts
         (jupyter-repl-cell-mark-busy)
         (jupyter-repl-finalize-cell req)
         (jupyter-repl-insert-prompt 'in))
        (save-excursion
          (jupyter-repl-backward-cell)
          (run-hooks 'jupyter-repl-cell-post-send-hook))
        req))))

(cl-defmethod jupyter-handle-payload ((_source (eql set_next_input)) pl
                                      &context (major-mode jupyter-repl-mode))
  (goto-char (point-max))
  (jupyter-repl-previous-cell)
  (jupyter-repl-replace-cell-code (plist-get pl :text)))

(cl-defmethod jupyter-handle-execute-reply ((client jupyter-repl-client) _req msg)
  (jupyter-with-repl-buffer client
    (jupyter-with-message-content msg (payload)
      (when payload
        (jupyter-handle-payload payload)))))

(cl-defmethod jupyter-handle-execute-result ((client jupyter-repl-client) req msg)
  ;; Only handle our results
  (when req
    (jupyter-repl-append-output client req
      (jupyter-repl-insert-prompt 'out)
      (jupyter-with-message-content msg (data metadata)
        (jupyter-insert data metadata)))))

(cl-defmethod jupyter-handle-display-data ((client jupyter-repl-client) req msg)
  (let ((clear (prog1 (oref client wait-to-clear)
                 (oset client wait-to-clear nil)))
        (req (if (eq (jupyter-message-parent-type msg) :comm-msg)
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
      (jupyter-with-message-content msg (data metadata transient)
        (cl-destructuring-bind (&key display_id &allow-other-keys)
            transient
          (if display_id
              (jupyter-insert display_id data metadata)
            (let ((inhibit-redisplay (not debug-on-error)))
              (when clear
                (jupyter-repl-clear-last-cell-output client)
                ;; Prevent slight flickering of prompt margin and text, this is
                ;; needed in addition to `inhibit-redisplay'.  It also seems that
                ;; it can be placed anywhere within this let and it will prevent
                ;; flickering.
                (sit-for 0.1 t))
              (jupyter-insert data metadata))))))))

(cl-defmethod jupyter-handle-update-display-data ((client jupyter-repl-client) _req msg)
  (jupyter-with-message-content msg (data metadata transient)
    (cl-destructuring-bind (&key display_id &allow-other-keys)
        transient
      (unless display_id
        (error "No display ID in `:update-display-data' message"))
      (jupyter-with-repl-buffer client
        (jupyter-update-display display_id data metadata)))))

(defun jupyter-repl-clear-last-cell-output (client)
  "In CLIENT's REPL buffer, clear the output of the last completed cell."
  (jupyter-with-repl-buffer client
    (goto-char (point-max))
    (jupyter-repl-previous-cell 2)
    (delete-region (1+ (jupyter-repl-cell-end-position))
                   (progn
                     (jupyter-repl-next-cell)
                     (point)))))

(cl-defmethod jupyter-handle-clear-output ((client jupyter-repl-client) _req msg)
  (unless (oset client wait-to-clear
                (jupyter-with-message-content msg (wait)
                  (eq wait t)))
    (cond
     ((eq (jupyter-message-parent-type msg) :comm-msg)
      (with-current-buffer (jupyter-get-buffer-create "output")
        (erase-buffer)))
     (t
      (jupyter-repl-clear-last-cell-output client)))))

(cl-defmethod jupyter-handle-status ((client jupyter-repl-client) req msg)
  (when (equal "idle"
               (jupyter-with-message-content msg (execution_state)
                 execution_state))
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
      (jupyter-display-current-buffer-reuse-window))))

(cl-defmethod jupyter-handle-stream ((client jupyter-repl-client) req msg)
  (jupyter-with-message-content msg (name text)
    (if (null req)
        (jupyter-repl-display-other-output client name text)
      (cond
       ((eq (jupyter-message-parent-type
             (jupyter-request-last-message req))
            :comm-msg)
        (jupyter-with-display-buffer "output" req
          (jupyter-insert-ansi-coded-text text)
          (jupyter-display-current-buffer-reuse-window)))
       (t
        (jupyter-repl-append-output client req
          (jupyter-insert-ansi-coded-text text)))))))

(cl-defmethod jupyter-handle-error ((client jupyter-repl-client) req msg)
  (when req
    (jupyter-with-message-content msg (traceback)
      (cond
       ((eq (jupyter-message-parent-type msg) :comm-msg)
        (jupyter-display-traceback traceback))
       (t
        (jupyter-repl-append-output client req
          (jupyter-with-insertion-bounds
              beg end (jupyter-insert-ansi-coded-text
                       (concat (mapconcat #'identity traceback "\n") "\n"))
            (font-lock-prepend-text-property
             beg end 'font-lock-face 'jupyter-repl-traceback))))))))

(defun jupyter-repl-history--rotate (n)
  "Rotate the REPL history ring N times.
The direction of rotation is determined by the sign of N.  For N
positive rotate to newer history elements, for N negative rotate
to older elements.

Return nil if the sentinel value is found before completing the
required number of rotations, otherwise return the element
rotated to, i.e. the one at index 0."
  (let (ifun cidx ridx)
    (if (> n 0)
        (setq ifun 'ring-insert cidx -1 ridx -1)
      (setq ifun 'ring-insert-at-beginning cidx 1 ridx 0))
    (cl-loop
     repeat (abs n)
     ;; Check that the next index to rotate to is not the sentinel
     if (eq (ring-ref jupyter-repl-history cidx) 'jupyter-repl-history)
     return nil else do
     ;; if it isn't, remove an element at RIDX and insert it using IFUN back
     ;; into the history ring, thereby rotating the history
     (funcall ifun jupyter-repl-history
              (ring-remove jupyter-repl-history ridx))
     ;; after N successful rotations, return the element rotated to
     finally return (let ((el (ring-ref jupyter-repl-history 0)))
                      (unless (eq el 'jupyter-repl-history)
                        el)))))

(defun jupyter-repl-history--match-input (regexp arg)
  "Return the index of the ARGth REGEXP match.
Or nil, on failure.  If ARG is positive, search backward from the most
recent history element.  If negative, search forward through items
previously visited during this input session.  If ARG is zero, do
nothing."
  ;; Adapted from `comint-previous-matching-input-string-position'
  (let* ((direction (if (> arg 0) +1 -1))
         (i (if (= direction -1) 0 -1)) ; adjust for initial increment
         (failed (zerop arg))
         ;;
         code)
    ;; Search ARG times
    (while (not (or failed (zerop arg)))
      (while (not (or (setq code (ring-ref jupyter-repl-history
                                           (cl-incf i direction))
                            failed (eq 'jupyter-repl-history code))
                      (string-match-p regexp code))))
      (setq arg (- arg direction)))
    (unless failed i)))

(defun jupyter-repl-history-previous-matching (&optional n)
  "Search input history for the input pending before point.
On success, replace the current input with the matching code element
while preserving and returning point.  Ding on failure.  If N is negative,
find the Nth next match; if positive, the Nth previous.  If N is zero or
nil, pretend it's one."
  ;; Adapted from: `comint-previous-matching-input-from-input' and friends
  (interactive "p")
  (when (or (null n) (zerop n)) (setq n 1))
  (let ((opoint (point))
        (code (jupyter-repl-cell-code))
        (is-prev (> n 0))
        (input-string (buffer-substring
                       (jupyter-repl-cell-code-beginning-position) (point)))
        found)
    ;; Look past an initial duplicate
    (when (equal code (ring-ref jupyter-repl-history (if is-prev 0 -1)))
      (cl-incf n (if is-prev 1 -1)))
    (if (not (setq found (jupyter-repl-history--match-input
                          (concat "^" (regexp-quote input-string)) n)))
        (user-error "No %s matching input" (if is-prev "earlier" "later"))
      (setq code (ring-ref jupyter-repl-history found))
      (jupyter-repl-history--rotate (- found))
      (jupyter-repl-replace-cell-code code)
      (goto-char opoint))))

(defun jupyter-repl-history-next-matching (&optional n)
  "Search existing history session for an element matching input.
Only consider the text before point.  If N is negative, find the Nth
previous match, otherwise the Nth next.  If N is zero or nil, make it
one.  \"Existing history session\" means those history elements already
visited while forming the current input."
  (interactive "p")
  (when (or (null n) (zerop n)) (setq n 1))
  (jupyter-repl-history-previous-matching (- n)))

(defun jupyter-repl-history-next (&optional n)
  "Go to the next history element.
Navigate through the REPL history to the next (newer) history
element and insert it as the last code cell.  For N positive move
forward in history that many times.  If N is negative, move to
older history elements."
  (interactive "p")
  (or n (setq n 1))
  (if (< n 0) (jupyter-repl-history-previous (- n))
    (goto-char (point-max))
    (let ((code (jupyter-repl-history--rotate n)))
      (if (and (null code) (equal (jupyter-repl-cell-code) ""))
          (error "End of history")
        (if (null code)
            ;; When we have reached the last history element in the forward
            ;; direction and the cell code is not empty, make it empty.
            (jupyter-repl-replace-cell-code "")
          (jupyter-repl-replace-cell-code code))))))

(defun jupyter-repl-history-previous (&optional n)
  "Go to the previous history element.
Similar to `jupyter-repl-history-next' but for older history
elements.  If N is negative in this case, move to newer history
elements."
  (interactive "p")
  (or n (setq n 1))
  (if (< n 0) (jupyter-repl-history-next (- n))
    (goto-char (point-max))
    (unless (equal (jupyter-repl-cell-code)
                   (ring-ref jupyter-repl-history 0))
      (setq n (1- n)))
    (let ((code (jupyter-repl-history--rotate (- n))))
      (if (null code)
          (error "Beginning of history")
        (jupyter-repl-replace-cell-code code)))))

(cl-defmethod jupyter-handle-history-reply ((client jupyter-repl-client) _req msg)
  (jupyter-with-repl-buffer client
    (cl-loop for elem across (jupyter-with-message-content msg (history) history)
             for input-output = (aref elem 2)
             do (ring-remove+insert+extend jupyter-repl-history input-output))))

(cl-defmethod jupyter-handle-is-complete-reply ((client jupyter-repl-client) _req msg)
  (jupyter-with-repl-buffer client
    (jupyter-with-message-content msg (status indent)
      (pcase status
        ("complete"
         (jupyter-send-execute-request client))
        ("incomplete"
         (insert "\n")
         (if (= (length indent) 0) (jupyter-repl-indent-line)
           (insert indent)))
        ("invalid"
         ;; Force an execute to produce a traceback
         (jupyter-send-execute-request client))
        ("unknown"
         ;; Let the kernel decide if the code is complete
         (jupyter-send-execute-request client))))))

(defun jupyter-repl--insert-banner-and-prompt (client)
  (jupyter-with-repl-buffer client
    (goto-char (point-max))
    (unless (jupyter-repl-cell-finalized-p)
      (jupyter-repl-finalize-cell nil))
    (jupyter-repl-newline)
    (jupyter-repl-insert-banner
     (plist-get (jupyter-kernel-info client) :banner))
    (jupyter-repl-insert-prompt 'in)
    (jupyter-repl-update-cell-count 1)))

(cl-defmethod jupyter-handle-shutdown-reply ((client jupyter-repl-client) _req msg)
  (jupyter-with-repl-buffer client
    (jupyter-repl-without-continuation-prompts
     (goto-char (point-max))
     (let ((shutdown-handled-p (jupyter-repl-cell-finalized-p)))
       (unless (jupyter-repl-cell-finalized-p)
         (jupyter-repl-finalize-cell nil))
       ;; Only run the following once.  The Python kernel sends a shutdown-reply
       ;; on both the shell and iopub which is mainly the reason why this is
       ;; needed.
       (unless shutdown-handled-p
         (jupyter-repl-newline)
         (jupyter-repl-newline)
         (jupyter-with-message-content msg (restart)
           ;; TODO: Add a slot mentioning that the kernel is shutdown so that we can
           ;; block sending requests or delay until it has restarted.
           (insert (propertize (concat "kernel " (if restart "restart" "shutdown"))
                               'read-only t 'font-lock-face 'warning))
           (jupyter-repl-newline)
           (when restart
             (jupyter-repl--insert-banner-and-prompt client))))))))

(defun jupyter-repl-ret (&optional force)
  "Send the current cell code to the kernel.
If `point' is before the last cell in the REPL buffer move to
`point-max', i.e. move to the last cell.  Otherwise if `point' is
at some position within the last cell, either insert a newline or
ask the kernel to execute the cell code depending on the kernel's
response to an `:is-complete-request'.

If a prefix argument is given, FORCE the kernel to execute the
current cell code without sending an `:is-complete-request'.  See
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
          ;; is-complete request.  Some kernels don't respond to this request
          ;; when the kernel is busy.
          (when (and (jupyter-kernel-busy-p jupyter-current-client)
                     (not jupyter-repl-allow-RET-when-busy))
            (error "Kernel busy"))
          (cond
           (force (jupyter-send-execute-request jupyter-current-client))
           ((or jupyter-repl-use-builtin-is-complete
                (and jupyter-repl-allow-RET-when-busy
                     (jupyter-kernel-busy-p jupyter-current-client)))
            (goto-char (point-max))
            (let ((complete-p (equal (buffer-substring-no-properties
                                      (line-beginning-position) (point))
                                     "")))
              (jupyter-handle-is-complete-reply
                  jupyter-current-client
                nil (if complete-p "complete" "incomplete") "")))
           (t
            (let ((res (jupyter-wait-until-received :is-complete-reply
                         (let ((jupyter-inhibit-handlers '(not :is-complete-reply)))
                           (jupyter-send-is-complete-request
                               jupyter-current-client
                             :code (jupyter-repl-cell-code)))
                         jupyter-repl-maximum-is-complete-timeout)))
              (unless res
                (message "\
Kernel did not respond to is-complete-request, using built-in is-complete.
Reset `jupyter-repl-use-builtin-is-complete' to nil if this is only temporary.")
                (setq jupyter-repl-use-builtin-is-complete t)
                (jupyter-repl-ret force)))))))
    (beginning-of-buffer
     ;; No cells in the current buffer, just insert one
     (jupyter-repl-insert-prompt 'in))))

(cl-defgeneric jupyter-indent-line ()
  (call-interactively #'indent-for-tab-command))

(defun jupyter-repl-indent-line ()
  "Indent the line according to the language of the REPL."
  (when-let* ((pos (and (jupyter-repl-cell-line-p)
                        (jupyter-repl-cell-code-position)))
              (code (jupyter-repl-cell-code))
              (replacement
               (jupyter-with-repl-lang-buffer
                 (insert code)
                 (goto-char pos)
                 (let ((tick (buffer-chars-modified-tick)))
                   (jupyter-indent-line)
                   (unless (eq tick (buffer-chars-modified-tick))
                     (setq pos (point))
                     (current-buffer))))))
    ;; Don't modify the buffer when unnecessary, this allows
    ;; `company-indent-or-complete-common' to work.
    (when replacement
      (jupyter-repl-replace-cell-code replacement)
      (goto-char (+ pos (jupyter-repl-cell-code-beginning-position))))))

;;; Buffer change functions

(defun jupyter-repl-yank-handle-field-property (val beg end)
  "If VAL is not cell-code, remove the field property between BEG and END.
Yanking text into a REPL cell normally removes the field
property (see `yank-excluded-properties') but this property is
added in `jupyter-repl-after-change' to mark text in an input cell.

The problem is that the after change functions run *before*
`insert-for-yank' removes the field property.  This function is
added to `yank-handled-properties' to prevent the removal of
field when the associated text is part of the input to a REPL
cell."
  ;; Assume that text with a field value of cell-code is due to
  ;; `jupyter-repl-mark-as-cell-code'.
  (unless (eq val 'cell-code)
    (remove-text-properties beg end '(field))))

(defun jupyter-repl-insert-continuation-prompts (bound)
  "Insert continuation prompts if needed, stopping at BOUND.
Return the new BOUND since inserting continuation prompts may add
more characters than were initially in the buffer.

If `jupyter-repl-inhibit-continuation-prompts' is non-nil return
BOUND without inserting any continuation prompts."
  (if jupyter-repl-inhibit-continuation-prompts
      bound
    (setq bound (set-marker (make-marker) bound))
    (set-marker-insertion-type bound t)
    ;; Don't record these changes as it adds unnecessary undo information which
    ;; interferes with undo.
    (let ((buffer-undo-list t))
      (while (and (< (point) bound)
                  (search-forward "\n" bound 'noerror))
        ;; Delete the newline that is re-added by prompt insertion
        ;; FIXME: Why not just overlay the newline?
        (delete-char -1)
        (jupyter-repl-insert-prompt 'continuation)))
    (prog1 (marker-position bound)
      (set-marker bound nil))))

(defun jupyter-repl-mark-as-cell-code (beg end)
  "Add the field property to text between (BEG . END) if within a code cell."
  ;; Handle field boundary at the front of the cell code
  (when (< beg end)
    (when (= beg (jupyter-repl-cell-code-beginning-position))
      (put-text-property beg (1+ beg) 'front-sticky t))
    (when (text-property-not-all beg end 'field 'cell-code)
      (font-lock-fillin-text-property beg end 'field 'cell-code))))

(defun jupyter-repl-do-after-change (beg end len)
  "Call `jupyter-repl-after-change' when the current cell code is changed.
`jupyter-repl-after-change' is only called when BEG is a position
on a `jupyter-repl-cell-line-p'.  BEG, END, and LEN have the same
meaning as in `after-change-functions'."
  (when (eq major-mode 'jupyter-repl-mode)
    (with-demoted-errors "Jupyter error after buffer change: %S"
      (save-restriction
        ;; Take into account insertions that may have the buffer narrowed since
        ;; functions like `jupyter-repl-cell-code-beginning-position' need to
        ;; look at parts of the buffer not necessarily within the narrowed
        ;; region.  See #38.
        ;;
        ;; TODO: Does it really make sense to widen the buffer? To get around
        ;; this, how can functions like
        ;; `jupyter-repl-cell-code-beginning-position' and
        ;; `jupyter-repl-cell-line-p' only rely on the `field' text property?
        (widen)
        (when (save-excursion
                (goto-char beg)
                (jupyter-repl-cell-line-p))
          (cond
           ((= len 0)
            (jupyter-repl-after-change 'insert beg end))
           ((and (= beg end) (not (zerop len)))
            (jupyter-repl-after-change 'delete beg len))
           ;; Text property changes
           ((= (- end beg) len)
            ;; Revert changes made by `insert-for-yank'.  See #14.
            (when (and (= len 1)
                       (get-text-property beg 'rear-nonsticky)
                       (= end (jupyter-repl-cell-end-position)))
              (remove-text-properties beg end '(rear-nonsticky))))
           ;; Post change inserted text in the region
           ((> (- end beg) len)
            (jupyter-repl-after-change 'insert beg end))
           ;; Post change deleted text
           (t
            ;; FIXME: This is probably wrong.
            (jupyter-repl-after-change 'delete beg (- len (- end beg))))))))))

(cl-defgeneric jupyter-repl-after-change (_type _beg _end-or-len)
  "Called from the `after-change-functions' of a REPL buffer.
Modify the text just inserted or deleted.  TYPE is either insert
or delete to signify if the change was due to insertion or
deletion of text.  BEG is always the beginning of the insertion or
deletion.  END-OR-LEN is the end of the insertion when TYPE is
insert and is the length of the deleted text when TYPE is delete.

The `after-change-functions' of the REPL buffer are only called
for changes to input cells and not for output generated by the
kernel.

Note, the overriding method should call `cl-call-next-method'.

Also note, any buffer narrowing will be temporarily removed when
this method is called."
  nil)

(cl-defmethod jupyter-repl-after-change ((_type (eql insert)) beg end)
  (goto-char beg)
  ;; Avoid doing anything on self insertion
  (unless (and (= (point) (1- end))
               (not (eq (char-after) ?\n)))
    (setq end (jupyter-repl-insert-continuation-prompts end)))
  (jupyter-repl-mark-as-cell-code beg end)
  (goto-char end))

(cl-defmethod jupyter-repl-after-change ((_type (eql delete)) beg _len)
  ;; Ensure that the `front-sticky' property at the beginning of cell code is
  ;; added after deleting text at the beginning of a cell.
  (jupyter-repl-mark-as-cell-code beg (min (point-max) (+ beg 1))))

(defun jupyter-repl--deactivate-interaction-buffers ()
  (cl-loop
   with client = jupyter-current-client
   for buffer in (buffer-list)
   do (with-current-buffer buffer
        (when (and jupyter-repl-interaction-mode
                   (eq jupyter-current-client client))
          (jupyter-repl-interaction-mode -1)))))

(defun jupyter-repl-kill-buffer-query-function ()
  "Ask before killing a Jupyter REPL buffer.
If the REPL buffer is killed, stop the client's channels.  When
the client is connected to a managed kernel, ask to also shutdown
the kernel.

In addition to the above, call the function
`jupyter-repl-interaction-mode' in all buffers associated with
the REPL to disable that mode in those buffers.  See
`jupyter-repl-associate-buffer'."
  (when (eq major-mode 'jupyter-repl-mode)
    (if (not (jupyter-channels-running-p jupyter-current-client)) t
      (when (y-or-n-p (format "Jupyter REPL (%s) still connected.  Kill it? "
                              (buffer-name (current-buffer))))
        (prog1 t
          (jupyter-stop-channels jupyter-current-client)
          (when (and (jupyter-client-has-manager-p)
                     (yes-or-no-p (format "Shutdown the client's kernel? ")))
            (jupyter-shutdown-kernel (oref jupyter-current-client manager))))))))

(defun jupyter-repl-error-before-major-mode-change ()
  "Error if attempting to change the `major-mode' in a REPL buffer."
  (when (eq major-mode 'jupyter-repl-mode)
    (error "Attempting to change `major-mode' in the REPL buffer!")))

(defun jupyter-repl-preserve-window-margins (&optional window)
  "Ensure that the margins of a REPL window are present.
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
                 (delq 'jupyter-repl-history
                       (ring-elements jupyter-repl-history)))
    (let ((ex (cl-call-next-method)))
      (prog1 ex
        (jupyter-repl-history-add ex)))))

(cl-defmethod jupyter-eval-string (str &context (jupyter-current-client jupyter-repl-client)
                                       &optional beg end)
  (let (req
        cell-previous-code)
    (jupyter-with-repl-buffer jupyter-current-client
      (when jupyter-repl-echo-eval-p
        (goto-char (point-max))
        (setq cell-previous-code (jupyter-repl-cell-code))
        (jupyter-repl-replace-cell-code str)
        (setq str nil))
      (let* ((jupyter-inhibit-handlers
              ;; When copying the input to the REPL we need the handlers to
              ;; update the REPL state
              (unless jupyter-repl-echo-eval-p
                '(not :input-request))))
        (setq req (jupyter-send-execute-request jupyter-current-client
                    :code str
                    :store-history jupyter-repl-echo-eval-p))
        (if jupyter-repl-echo-eval-p
            (jupyter-repl-replace-cell-code cell-previous-code))))
    ;; Add callbacks to display evaluation output in pop-up buffers either when
    ;; we aren't copying the input to a REPL cell or, if we are, when the REPL
    ;; buffer isn't visible.
    ;;
    ;; Make sure we do this in the original buffer where STR originated from
    ;; when BEG and END are non-nil.
    (prog1 req
      (unless (and jupyter-repl-echo-eval-p
                   (get-buffer-window (oref jupyter-current-client buffer) 'visible))
        (jupyter-eval-add-callbacks req beg end)))))

;;; Kernel management

(defun jupyter-repl-interrupt-kernel ()
  "Interrupt the kernel if possible.
A kernel can be interrupted if it was started using a kernel
manager.  See `jupyter-start-new-kernel'."
  (interactive)
  (if (not (jupyter-client-has-manager-p))
      (user-error "Can only interrupt managed kernels")
    (message "Interrupting kernel")
    (jupyter-interrupt-kernel
     (oref jupyter-current-client manager))))

(defun jupyter-repl-restart-kernel (&optional shutdown client)
  "Restart the kernel `jupyter-current-client' is connected to.
With a prefix argument, SHUTDOWN the kernel completely instead.
If CLIENT is non-nil, it should by a REPL client to use instead
of `jupyter-current-client'.

If CLIENT is nil, `jupyter-current-client' will be restarted
instead.  If `jupyter-current-client' is nil or is not a REPL
client, prompt for a REPL client to restart.  Otherwise restart
the kernel `jupyter-current-client' is connected to."
  (interactive
   (list current-prefix-arg nil))
  (unless client
    (setq client
          (or jupyter-current-client
              ;; Also allow this command to be called from an Org mode buffer by
              ;; selecting a client based on the REPL buffer.
              (buffer-local-value
               'jupyter-current-client
               (jupyter-repl-completing-read-repl-buffer)))))
  (cl-check-type client jupyter-repl-client)
  (unless shutdown
    ;; This may have been set to t due to a non-responsive kernel so make sure
    ;; that we try again when restarting.
    (jupyter-with-repl-buffer client
      (setq jupyter-repl-use-builtin-is-complete nil)))
  (jupyter-hb-pause client)
  (let ((manager (oref client manager)))
    (cond
     ((and (not shutdown)
           (jupyter-client-has-manager-p client)
           (not (jupyter-kernel-alive-p manager)))
      (message "Starting dead kernel...")
      (jupyter-repl--insert-banner-and-prompt client))
     (t
      (message "%s kernel..." (if shutdown "Shutting down"
                                "Restarting"))
      (if manager (jupyter-shutdown-kernel manager (not shutdown))
        ;; NOTE: It's not possible to restart a kernel without a kernel manager
        ;; unless the kernel is able to restart on its own.
        (when (and (null (jupyter-wait-until-received :shutdown-reply
                           (let ((jupyter-inhibit-handlers '(not :shutdown-reply)))
                             (jupyter-send-shutdown-request client
                               :restart (not shutdown)))))
                   (not shutdown))
          ;; Handle the case of a restart that does not send a shutdown-reply
          ;;
          ;; TODO: Clean up the logic of when to insert a new prompt.  We insert
          ;; a new prompt before we know if the kernel is ready, but this should
          ;; be done after we know if the kernel is ready or not, e.g. on the
          ;; next status: starting message.  Generalize the stuff in
          ;; `jupyter-start-new-kernel' that handles the status: starting message
          ;; so its easier to hook into that message.
          (message "Kernel did not send shutdown-reply")
          (jupyter-repl--insert-banner-and-prompt client)))))
    (unless shutdown
      (jupyter-hb-unpause client))))

(defun jupyter-repl-display-kernel-buffer ()
  "Display the kernel processes stdout."
  (interactive)
  (if (jupyter-client-has-manager-p)
      (let ((manager (oref jupyter-current-client manager)))
        (if (jupyter-kernel-alive-p manager)
            (if (and (slot-boundp manager 'kernel)
                     (processp (oref manager kernel)))
                (display-buffer (process-buffer (oref manager kernel)))
              (error "Manager needs a kernel slot"))
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
       ;; 1.  First try searching in the initial cell text
       (funcall search-fun string
                (or bound
                    (unless isearch-forward
                      (jupyter-repl-cell-code-beginning-position)))
                noerror)
       ;; 2.  If the above search fails, start putting next/prev history
       ;; elements in the cell successively, and search the string in them.  Do
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
  (jupyter-repl-history--rotate
   (* (if isearch-forward -1 1)
      (ring-length jupyter-repl-history)))
  (jupyter-repl-replace-cell-code (ring-ref jupyter-repl-history 0))
  (goto-char (if isearch-forward (jupyter-repl-cell-code-beginning-position)
               (point-max))))

(defun jupyter-repl-history-isearch-push-state ()
  "Save a function restoring the state of input history search.
Save the element at index 0 in `jupyter-repl-history'.  When
restoring the state, the `jupyter-repl-history' ring is rotated,
in the appropriate direction, to the saved element."
  (let ((code (jupyter-repl-cell-code)))
    (cond
     ((equal code (ring-ref jupyter-repl-history 0))
      (let ((elem (ring-ref jupyter-repl-history 0)))
        (lambda (_cmd)
          (when isearch-wrapped
            (jupyter-repl-history--rotate
             (* (if isearch-forward 1 -1)
                (ring-length jupyter-repl-history))))
          (let ((dir (if isearch-forward -1 1)))
            (while (not (eq (ring-ref jupyter-repl-history 0) elem))
              (jupyter-repl-history--rotate dir)))
          (jupyter-repl-replace-cell-code (ring-ref jupyter-repl-history 0)))))
     (t
      (let ((elem code))
        (lambda (_cmd)
          (jupyter-repl-replace-cell-code elem)))))))

;;; `jupyter-repl-mode'

(defun jupyter-repl-scratch-buffer ()
  "Switch to a scratch buffer connected to the current REPL in another window.
Return the buffer switched to."
  (interactive)
  (if (jupyter-repl-connected-p)
      (let* ((client jupyter-current-client)
             (name (format "*jupyter-scratch[%s]*"
                           (jupyter-comm-id (oref client kcomm)))))
        (unless (get-buffer name)
          (with-current-buffer (get-buffer-create name)
            (funcall (jupyter-kernel-language-mode client))
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
    (define-key map (kbd "M-n") #'jupyter-repl-history-next)
    (define-key map (kbd "M-p") #'jupyter-repl-history-previous)
    (define-key map (kbd "C-c C-o") #'jupyter-repl-clear-cells)
    (define-key map (kbd "C-c M-r") #'jupyter-repl-history-previous-matching)
    (define-key map (kbd "C-c M-s") #'jupyter-repl-history-next-matching)
    map))

(put 'jupyter-repl-mode 'mode-class 'special)
(define-derived-mode jupyter-repl-mode fundamental-mode
  "Jupyter-REPL"
  "A Jupyter REPL major mode."
  (cl-check-type jupyter-current-client jupyter-repl-client)
  ;; This is a better setting for rendering language banners.
  (setq-local show-trailing-whitespace nil)
  ;; This is a better setting when rendering HTML tables
  (setq-local truncate-lines t)
  (setq-local indent-line-function #'jupyter-repl-indent-line)
  (setq-local left-margin-width jupyter-repl-prompt-margin-width)
  (setq-local yank-handled-properties
              (append '((field . jupyter-repl-yank-handle-field-property))
                      yank-handled-properties))
  (setq-local yank-excluded-properties (remq 'field yank-excluded-properties))
  ;; Initialize a buffer using the major-mode correponding to the kernel's
  ;; language.  This will be used for indentation and to capture font lock
  ;; properties.
  (let* ((info (jupyter-kernel-info jupyter-current-client))
         (language (plist-get (plist-get info :language_info) :name)))
    (jupyter-load-language-support jupyter-current-client)
    (cl-destructuring-bind (mode syntax)
        (jupyter-kernel-language-mode-properties jupyter-current-client)
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
    (add-hook 'kill-buffer-hook #'jupyter-repl--deactivate-interaction-buffers nil t)
    (add-hook 'after-change-functions 'jupyter-repl-do-after-change nil t)
    (add-hook 'pre-redisplay-functions 'jupyter-repl-preserve-window-margins nil t)
    ;; Initialize the REPL
    (jupyter-repl-initialize-fontification)
    (jupyter-repl-isearch-setup)
    (jupyter-repl-sync-execution-state)
    (jupyter-repl-interaction-mode)
    ;; Do this last so that it runs before any other `change-major-mode-hook's.
    (add-hook 'change-major-mode-hook #'jupyter-repl-error-before-major-mode-change nil t)))

(cl-defgeneric jupyter-repl-after-init ()
  "Hook function called whenever `jupyter-repl-mode' is enabled/disabled.
You may override this function for a particular language using a
jupyter-lang &context specializer.  For example, to do something
when the language of the REPL is python the method signature
would look like

    (cl-defmethod jupyter-repl-after-init (&context (jupyter-lang python)))"
  nil)

(add-hook 'jupyter-repl-mode-hook 'jupyter-repl-after-init)

(defun jupyter-repl-font-lock-fontify-region (fontify-fun beg end &optional verbose)
  "Use FONTIFY-FUN to fontify input cells between BEG and END.
VERBOSE has the same meaning as in
`font-lock-fontify-region-function'."
  (jupyter-repl-map-cells beg end
    ;; Ensure that the buffer is narrowed to the actual cell code before calling
    ;; the REPL language's `major-mode' specific fontification functions since
    ;; those functions don't know anything about input cells or output cells and
    ;; may traverse cell boundaries.
    ;;
    ;; It is OK that we do not update BEG and END using the return value of this
    ;; function as long as the default value of
    ;; `font-lock-extend-region-functions' is used since an input cell always
    ;; starts at the beginning of a line and ends at the end of a line and does
    ;; not use the font-lock-multiline property (2018-12-20).
    (lambda ()  (funcall fontify-fun (point-min) (point-max) verbose))
    ;; Unfontify the region mainly to remove the font-lock-multiline property in
    ;; the output, e.g. added by markdown.  These regions will get highlighted
    ;; syntactically in some scenarios.
    (lambda () (font-lock-unfontify-region (point-min) (point-max))))
  `(jit-lock-bounds ,beg . ,end))

(defun jupyter-repl-syntax-propertize-function (propertize-fun beg end)
  "Use PROPERTIZE-FUN to syntax propertize text between BEG and END."
  (jupyter-repl-map-cells beg end
    ;; See note in `jupyter-repl-font-lock-fontify-region' on why the buffer
    ;; should be narrowed to the input cell before calling this function.
    (lambda () (funcall propertize-fun (point-min) (point-max)))
    ;; Treat parenthesis and string characters as punctuation when parsing the
    ;; syntax of the output.  Although we don't fontify output regions,
    ;; `syntax-ppss' still looks at the whole contents of the buffer.  If there
    ;; are unmatched parenthesis or string delimiters in the output, it will
    ;; interfere with `syntax-ppss'.  Note, this requires
    ;; `parse-sexp-lookup-properties' to be non-nil so that `syntax-ppss' will
    ;; look at the `syntax-table' property.
    (lambda ()
      (goto-char (point-min))
      (skip-syntax-forward "^()\"")
      (while (not (eobp))
        (put-text-property (point) (1+ (point)) 'syntax-table '(1 . ?.))
        (forward-char)
        (skip-syntax-forward "^()\"")))))

(cl-defgeneric jupyter-repl-initialize-fontification ()
  "Initialize fontification for the current REPL buffer."
  (let (fld frf sff spf comment)
    (jupyter-with-repl-lang-buffer
      (setq fld font-lock-defaults
            frf (or font-lock-fontify-region-function #'ignore)
            sff (or font-lock-syntactic-face-function #'ignore)
            spf (or syntax-propertize-function #'ignore)
            comment comment-start))
    ;; Set `font-lock-defaults' to a copy of the font lock defaults for the
    ;; REPL language but with a modified syntactic fontification function
    (cl-destructuring-bind (kws &optional kws-only case-fold syntax-alist
                                &rest vars)
        (or fld (list nil))
      (setq vars
            (append vars
                    (list
                     ;; See `jupyter-repl-font-lock-fontify-region'
                     (cons 'parse-sexp-lookup-properties t)
                     (cons 'syntax-propertize-function
                           (apply-partially
                            #'jupyter-repl-syntax-propertize-function spf))
                     (cons 'font-lock-fontify-region-function
                           (apply-partially
                            #'jupyter-repl-font-lock-fontify-region frf))
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
   (insert (propertize banner
                       'read-only t 'jupyter-banner t
                       'font-lock-face 'shadow 'fontified t
                       'font-lock-fontified t))
   (jupyter-repl-newline)))

(defun jupyter-repl-sync-execution-state ()
  "Synchronize the `jupyter-current-client's kernel state.
Also update the cell count of the current REPL input prompt using
the updated state."
  (let* ((client jupyter-current-client)
         (req (let ((jupyter-inhibit-handlers t))
                (jupyter-send-execute-request client :code "" :silent t))))
    (jupyter-add-callback req
      :execute-reply
      (jupyter-message-lambda (execution_count)
        (oset client execution-count (1+ execution_count))
        (unless (equal (jupyter-execution-state client) "busy")
          ;; Set the cell count and update the prompt
          (jupyter-with-repl-buffer client
            (save-excursion
              (goto-char (point-max))
              (jupyter-repl-update-cell-count
               (oref client execution-count)))))))
    ;; Waiting longer here to account for initial startup of the Jupyter
    ;; kernel.  Sometimes the idle message won't be received if another long
    ;; running execute request is sent right after.
    (jupyter-wait-until-idle req jupyter-long-timeout)))

;;; `jupyter-repl-interaction-mode'

(defvar jupyter-repl-interaction-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-e") #'jupyter-eval-line-or-region)
    (define-key map (kbd "C-c C-c") #'jupyter-eval-line-or-region)
    (define-key map (kbd "C-M-x") #'jupyter-eval-defun)
    (define-key map (kbd "C-c C-o") #'jupyter-eval-remove-overlays)
    (define-key map (kbd "C-c C-s") #'jupyter-repl-scratch-buffer)
    (define-key map (kbd "C-c C-b") #'jupyter-eval-buffer)
    (define-key map (kbd "C-c C-l") #'jupyter-load-file)
    (define-key map (kbd "C-c M-:") #'jupyter-eval-string-command)
    (define-key map (kbd "M-i") #'jupyter-inspect-at-point)
    (define-key map (kbd "C-c C-r") #'jupyter-repl-restart-kernel)
    (define-key map (kbd "C-c C-i") #'jupyter-repl-interrupt-kernel)
    (define-key map (kbd "C-c C-z") #'jupyter-repl-pop-to-buffer)
    map))

(define-minor-mode jupyter-repl-interaction-mode
  "Minor mode for interacting with a Jupyter REPL.
When this minor mode is enabled you may evaluate code from the
current buffer using the associated REPL (see
`jupyter-repl-associate-buffer' to associate a REPL).

In addition any new buffers opened with the same `major-mode' as
the `current-buffer' will automatically have
`jupyter-repl-interaction-mode' enabled for them.

\\{jupyter-repl-interaction-mode-map}"
  :group 'jupyter-repl
  :lighter (:eval (jupyter-repl-interaction-mode-line))
  :init-value nil
  (cond
   (jupyter-repl-interaction-mode
    (add-hook 'completion-at-point-functions 'jupyter-completion-at-point nil t)
    (add-hook 'after-revert-hook 'jupyter-repl-interaction-mode nil t))
   (t
    (remove-hook 'completion-at-point-functions 'jupyter-completion-at-point t)
    (remove-hook 'after-revert-hook 'jupyter-repl-interaction-mode t)
    (unless (eq major-mode 'jupyter-repl-mode)
      (kill-local-variable 'jupyter-current-client)))))

(defun jupyter-repl-interaction-mode-reenable ()
  "Re-enable `jupyter-repl-interaction-mode' in the current buffer.
Do so only if possible in the `current-buffer'."
  (when (and (not jupyter-repl-interaction-mode)
             (cl-typep jupyter-current-client 'jupyter-repl-client)
             (eq major-mode
                 (jupyter-kernel-language-mode jupyter-current-client)))
    (jupyter-repl-interaction-mode)))

(defun jupyter-repl-interaction-mode-line ()
  "Return a mode line string with the status of the kernel.
'*' means the kernel is busy, '-' means the kernel is idle and
the REPL is connected, 'x' means the REPL is disconnected
from the kernel."
  (and (cl-typep jupyter-current-client 'jupyter-repl-client)
       (concat " JuPy["
               (cond
                ((not (jupyter-hb-beating-p jupyter-current-client)) "x")
                ((equal (jupyter-execution-state jupyter-current-client) "busy")
                 "*")
                (t "-"))
               "]")))

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
   for match =
   (when (and (object-of-class-p client 'jupyter-repl-client)
              (buffer-live-p (oref client buffer)))
     (with-current-buffer (oref client buffer)
       (and (or (null mode)
                (provided-mode-derived-p mode jupyter-repl-lang-mode))
            (jupyter-repl-connected-p)
            (buffer-name))))
   if (and match first) return (oref client buffer)
   else if match collect (oref client buffer)))

;;;###autoload
(defun jupyter-repl-associate-buffer (client)
  "Associate the `current-buffer' with a REPL CLIENT.
If the `major-mode' of the `current-buffer' is the
`jupyter-repl-lang-mode' of CLIENT, call the function
`jupyter-repl-interaction-mode' to enable the corresponding mode.

CLIENT should be the symbol `jupyter-repl-client' or the symbol
of a subclass.  If CLIENT is a buffer or the name of a buffer, use
the `jupyter-current-client' local to the buffer."
  (interactive
   (list
    (let ((buffer
           (when (jupyter-repl-available-repl-buffers major-mode)
             (jupyter-repl-completing-read-repl-buffer major-mode))))
      (when buffer
        (buffer-local-value 'jupyter-current-client buffer)))))
  (if (not client)
      (when (y-or-n-p "No REPL for `major-mode' exists.  Start one? ")
        (call-interactively #'jupyter-run-repl))
    (setq client (if (or (bufferp client) (stringp client))
                     (with-current-buffer client
                       jupyter-current-client)
                   client))
    (unless (object-of-class-p client 'jupyter-repl-client)
      (error "Not a REPL client (%s)" client))
    (unless (eq (jupyter-kernel-language-mode client) major-mode)
      (error "Cannot associate buffer to REPL.  Wrong `major-mode'"))
    (setq-local jupyter-current-client client)
    (unless jupyter-repl-interaction-mode
      (jupyter-repl-interaction-mode))))

(defun jupyter-repl-propagate-client (buffer &rest _)
  "Propagate the `jupyter-current-client' to BUFFER.
If BUFFER's value of the variable `jupyter-repl-interaction-mode'
is nil and the buffer has the same `major-mode' as the
`jupyter-current-client's language mode, set the buffer local
value of `jupyter-current-client' in BUFFER to the current value
of that variable."
  (when (and jupyter-current-client
             (cl-typep jupyter-current-client 'jupyter-repl-client)
             (or (bufferp buffer) (stringp buffer))
             (setq buffer (get-buffer buffer))
             (buffer-live-p buffer)
             (null (buffer-local-value 'jupyter-repl-interaction-mode buffer))
             (eq (buffer-local-value 'major-mode buffer)
                 (jupyter-kernel-language-mode jupyter-current-client)))
    (let ((client jupyter-current-client))
      (with-current-buffer buffer
        (jupyter-repl-associate-buffer client)))))

(defun jupyter-repl--before-switch-to-buffer (buffer &rest _)
  "Call `jupyter-repl-propagate-client' on BUFFER, handling a nil BUFFER.
When BUFFER is nil use `other-buffer'."
  (jupyter-repl-propagate-client (or buffer (other-buffer))))

(defun jupyter-repl--before-set-window-buffer (_ buffer &rest __)
  "Call `jupyter-repl-propagate-client' on BUFFER."
  (jupyter-repl-propagate-client buffer))

;;; `jupyter-repl-persistent-mode'

;;;###autoload
(define-minor-mode jupyter-repl-persistent-mode
  "Global minor mode to persist Jupyter REPL connections.
When the `jupyter-current-client' of the current buffer is a REPL
client, its value is propagated to all buffers switched to that
have the same `major-mode' as the client's kernel language and
`jupyter-repl-interaction-mode' is enabled in those buffers."
  :group 'jupyter-repl
  :global t
  :keymap nil
  :init-value nil
  (cond
   (jupyter-repl-persistent-mode
    (advice-add 'switch-to-buffer :before #'jupyter-repl--before-switch-to-buffer)
    (advice-add 'display-buffer :before #'jupyter-repl-propagate-client)
    (advice-add 'set-window-buffer :before #'jupyter-repl--before-set-window-buffer)
    (add-hook 'after-change-major-mode-hook 'jupyter-repl-interaction-mode-reenable))
   (t
    (advice-remove 'switch-to-buffer #'jupyter-repl--before-switch-to-buffer)
    (advice-remove 'display-buffer #'jupyter-repl-propagate-client)
    (advice-remove 'set-window-buffer #'jupyter-repl--before-set-window-buffer)
    (remove-hook 'after-change-major-mode-hook 'jupyter-repl-interaction-mode-reenable))))

;;; Starting a REPL

(cl-defgeneric jupyter-bootstrap-repl ((client jupyter-repl-client)
                                       &optional repl-name associate-buffer display)
  "Initialize a new REPL buffer based on CLIENT, return CLIENT.
CLIENT should be a REPL client already connected to its kernel.

A new REPL buffer communicating with CLIENT's kernel is created
and set as CLIENT's buffer slot.  If CLIENT already has a non-nil
buffer slot, do nothing.

REPL-NAME is a string that will be used to generate the buffer
name.  If nil or empty, a default will be used.

If ASSOCIATE-BUFFER is non-nil, attempt to \"connect\" the
`current-buffer' to the REPL (see
`jupyter-repl-associate-buffer') if it is compatible with the
underlying kernel.

If DISPLAY is non-nil, display the REPL buffer after
completing all of the above.")

(cl-defmethod jupyter-bootstrap-repl :before ((_client jupyter-repl-client)
                                              &optional _repl-name _associate-buffer _display)
  "Enable `jupyter-repl-persistent-mode' if needed."
  (unless jupyter-repl-persistent-mode (jupyter-repl-persistent-mode)))

(cl-defmethod jupyter-bootstrap-repl :after ((client jupyter-repl-client)
                                             &optional _repl-name associate-buffer display)
  (when (and associate-buffer
             (eq major-mode (jupyter-kernel-language-mode client)))
    (jupyter-repl-associate-buffer client))
  (when display
    (pop-to-buffer (oref client buffer))))

(cl-defmethod jupyter-bootstrap-repl ((client jupyter-repl-client)
                                      &optional repl-name _associate-buffer _display)
  (prog1 client
    (unless (oref client buffer)
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
                             (format "%s %s" language-name language-version)
                           repl-name))))
          (jupyter-with-repl-buffer client
            (setq-local jupyter-current-client client)
            (jupyter-repl-mode)
            (jupyter-repl-insert-banner banner)
            (jupyter-repl-insert-prompt 'in)))))))

;;;###autoload
(defun jupyter-run-repl (kernel-name &optional repl-name associate-buffer client-class display)
  "Run a Jupyter REPL connected to a kernel with name, KERNEL-NAME.
KERNEL-NAME will be passed to `jupyter-find-kernelspecs' and the
first kernel found will be used to start the new kernel.

With a prefix argument give a new REPL-NAME for the REPL.

Optional argument ASSOCIATE-BUFFER, if non-nil, means to enable
the REPL interaction mode by calling the function
`jupyter-repl-interaction-mode' in the `current-buffer' and
associate it with the REPL created.  When called interactively,
ASSOCIATE-BUFFER is set to t.  If the `current-buffer's
`major-mode' does not correspond to the language of the kernel
started, ASSOCIATE-BUFFER has no effect.

Optional argument CLIENT-CLASS is the class that will be passed
to `jupyter-start-new-kernel' and should be a class symbol like
the symbol `jupyter-repl-client', which is the default.

When called interactively, DISPLAY the new REPL buffer.
Otherwise, in a non-interactive call, return the REPL client
connected to the kernel.

Note, if `default-directory' is a remote directory, a kernel will
start on the remote host by using the \"jupyter kernel\" shell
command on the host."
  (interactive (list (car (jupyter-completing-read-kernelspec
                           nil current-prefix-arg))
                     (when current-prefix-arg
                       (read-string "REPL Name: "))
                     t nil t))
  (or client-class (setq client-class 'jupyter-repl-client))
  (jupyter-error-if-not-client-class-p client-class 'jupyter-repl-client)
  (unless (called-interactively-p 'interactive)
    (or (when-let* ((name (caar (jupyter-find-kernelspecs kernel-name))))
          (setq kernel-name name))
        (error "No kernel found for prefix (%s)" kernel-name)))
  ;; For `jupyter-start-new-kernel', we don't require this at top-level since
  ;; there are many ways to interact with a kernel, e.g. through a notebook
  ;; server, and we don't want to load any unnecessary files.
  (require 'jupyter-kernel-process-manager)
  (cl-destructuring-bind (_manager client)
      (jupyter-start-new-kernel kernel-name client-class)
    (jupyter-bootstrap-repl client repl-name associate-buffer display)))

;;;###autoload
(defun jupyter-connect-repl (file-or-plist &optional repl-name associate-buffer client-class display)
  "Run a Jupyter REPL using a kernel's connection FILE-OR-PLIST.
FILE-OR-PLIST can be either a file holding the connection
information or a property list of connection information.
ASSOCIATE-BUFFER has the same meaning as in `jupyter-run-repl'.

With a prefix argument give a new REPL-NAME for the REPL.

Optional argument CLIENT-CLASS is the class of the client that
will be used to initialize the REPL and should be a class symbol
like the symbol `jupyter-repl-client', which is the default.

Return the REPL client connected to the kernel.  When called
interactively, DISPLAY the new REPL buffer as well."
  (interactive (list (read-file-name "Connection file: ")
                     (when current-prefix-arg
                       (read-string "REPL Name: "))
                     t nil t))
  (or client-class (setq client-class 'jupyter-repl-client))
  (jupyter-error-if-not-client-class-p client-class 'jupyter-repl-client)
  (let ((client (make-instance client-class)))
    ;; FIXME: See note in `jupyter-make-client'
    (require 'jupyter-channel-ioloop-comm)
    (require 'jupyter-zmq-channel-ioloop)
    (oset client kcomm (make-instance
                        'jupyter-channel-ioloop-comm
                        :ioloop-class 'jupyter-zmq-channel-ioloop))
    (jupyter-comm-initialize client file-or-plist)
    (jupyter-start-channels client)
    (jupyter-hb-unpause client)
    (jupyter-bootstrap-repl client repl-name associate-buffer display)))

(provide 'jupyter-repl)

;;; jupyter-repl.el ends here
