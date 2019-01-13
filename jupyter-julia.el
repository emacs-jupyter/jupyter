;;; jupyter-julia.el --- Jupyter support for Julia -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 23 Oct 2018
;; Version: 0.6.0
;; X-URL: https://github.com/nathan/jupyter-julia

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

;; Support methods for integration with Julia.

;;; Code:

(require 'jupyter-repl)

(declare-function julia-latexsub-or-indent "ext:julia-mode" (arg))

(cl-defmethod jupyter-indent-line (&context (major-mode julia-mode))
  "Call `julia-latexsub-or-indent'."
  (call-interactively #'julia-latexsub-or-indent))

(cl-defmethod jupyter-load-file-code (file &context (jupyter-lang julia))
  (format "include(\"%s\");" file))

;;; Completion

(cl-defmethod jupyter-completion-prefix (&context (jupyter-lang julia))
  (let ((prefix (cl-call-next-method "\\\\\\|\\.\\|::" 2)))
    (prog1 prefix
      (when (and (consp prefix)
                 (eq (char-before (- (point) (length (car prefix)))) ?\\))
        ;; Include the \ in the prefix so it gets replaced if a canidate is
        ;; selected.
        (setcar prefix (concat "\\" (car prefix)))))))

(cl-defmethod jupyter-completion-post-completion (candidate
                                                  &context (jupyter-lang julia))
  "Insert the unicode representation of a LaTeX completion."
  (if (eq (aref candidate 0) ?\\)
      (when (get-text-property 0 'annot candidate)
        (search-backward candidate)
        (delete-region (point) (match-end 0))
        ;; Alternatively use `julia-latexsub-or-indent', but I have found
        ;; problems with that.
        (insert (string-trim (get-text-property 0 'annot candidate))))
    (cl-call-next-method)))

;;; `markdown-mode'

(cl-defmethod jupyter-markdown-follow-link (link-text url _ref-label _title-text _bang
                                                      &context (jupyter-lang julia))
  "Send a help query to the Julia REPL for LINK-TEXT if URL is \"@ref\".
If URL is \"@ref <section>\" then open a browser to the Julia
manual for <section>. Otherwise follow the link normally."
  (if (string-prefix-p "@ref" url)
      (if (string= url "@ref")
          ;; Links have the form `fun`
          (let ((fun (substring link-text 1 -1)))
            (if (not (eq major-mode 'jupyter-repl-mode))
                (jupyter-inspect fun (1- (length fun)))
              (goto-char (point-max))
              (jupyter-repl-replace-cell-code (concat "?" fun))
              (jupyter-repl-ret)))
        (let* ((ref (split-string url))
               (section (cadr ref)))
          (browse-url
           (format "https://docs.julialang.org/en/latest/manual/%s/" section))))
    (cl-call-next-method)))

;;; `jupyter-repl-after-change'

(defvar ansi-color-names-vector)

(defun jupyter-julia-update-prompt (prompt color)
  "Display PROMPT as the cell prompt using COLOR as the foreground.
Make the character after `point' invisible."
  (add-text-properties (point) (1+ (point)) '(invisible t rear-nonsticky t))
  ;; TODO: Handle `jupyter-repl-prompt-margin-width', specifically if a prompt
  ;; is longer than that width, widen the margin and right align all strings.
  (jupyter-repl-cell-update-prompt
   prompt `((:foreground ,color) jupyter-repl-input-prompt)))

(cl-defmethod jupyter-repl-after-change ((_type (eql insert)) beg _end
                                         &context (jupyter-lang julia))
  "Change the REPL prompt when a REPL mode is entered."
  (when (= beg (jupyter-repl-cell-code-beginning-position))
    (save-excursion
      (goto-char beg)
      (cl-case (char-after)
        (?\]
         ;; TODO: Get rid of the "No matching paren" message caused by
         ;; `blink-paren-function' which is called in `post-self-insert-hook'.
         ;; Maybe remap `self-insert-command' to handle this case?
         (let ((pkg-prompt (jupyter-eval "import Pkg; Pkg.REPLMode.promptf()")))
           (when pkg-prompt
             (put-text-property (point) (1+ (point)) 'syntax-table '(3 . ?_))
             (jupyter-julia-update-prompt
              (substring pkg-prompt 1 (1- (length pkg-prompt)))
              (aref ansi-color-names-vector 5))))) ; magenta
        (?\;
         (jupyter-julia-update-prompt
          "shell> " (aref ansi-color-names-vector 1))) ; red
        (?\?
         (jupyter-julia-update-prompt
          "help?> " (aref ansi-color-names-vector 3)))))) ; yellow
  (cl-call-next-method))

(cl-defmethod jupyter-repl-after-change ((_type (eql delete)) beg _len
                                         &context (jupyter-lang julia))
  "Reset the prompt if needed."
  (when (= beg (jupyter-repl-cell-code-beginning-position))
    (jupyter-repl-cell-reset-prompt)))

;;; `jupyter-org'

(cl-defmethod jupyter-org-error-location (&context (jupyter-lang julia))
  (when (and (re-search-forward "^Stacktrace:" nil t)
             (re-search-forward
              "top-level scope at In\\[[0-9]+\\]:\\([0-9]+\\)" nil t))
    (string-to-number (match-string 1))))

(cl-defmethod org-babel-jupyter-transform-code (code changelist &context (jupyter-lang julia))
  (when (plist-get changelist :dir)
    (setq code
          (format "\
let __ejupy_old_dir = pwd()
    cd(\"%s\")
    try
        %s
    finally
        cd(__ejupy_old_dir)
    end
end"
                  (plist-get changelist :dir) code)))
  code)

(provide 'jupyter-julia)

;;; jupyter-julia.el ends here
