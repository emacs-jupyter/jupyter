;;; jupyter-julia.el --- Jupyter support for Julia -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 23 Oct 2018

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

;; Support methods for integration with Julia.

;;; Code:

(eval-when-compile (require 'subr-x))
(eval-when-compile (require 'dash))
(require 'jupyter-repl)

(declare-function julia-latexsub-or-indent "ext:julia-mode" (arg))

(cl-defmethod jupyter-indent-line (&context (major-mode julia-mode))
  "Call `julia-latexsub-or-indent'."
  (call-interactively #'julia-latexsub-or-indent))

(cl-defmethod jupyter-load-file-code (file &context (jupyter-lang julia))
  (format "include(\"%s\");" file))

;;; Completion

(cl-defmethod jupyter-completion-prefix (&context (jupyter-lang julia))
  (cond
   ;; Completing argument lists
   ((and (char-before)
         (eq (char-syntax (char-before)) ?\()
         (or (not (char-after))
             (looking-at-p "\\_>")
             (not (memq (char-syntax (char-after)) '(?w ?_)))))
    (buffer-substring-no-properties
     (jupyter-completion-symbol-beginning (1- (point)))
     (point)))
   (t
    (let ((prefix (cl-call-next-method "\\\\\\|\\.\\|::?" 2)))
      (prog1 prefix
        (when (consp prefix)
          (let ((beg (- (point) (length (car prefix)))))
            (cond
             ;; Include the \ in the prefix so it gets replaced if a canidate is
             ;; selected.
             ((eq (char-before beg) ?\\)
              (setcar prefix (concat "\\" (car prefix))))
             ;; Also include : to complete symbols when used as dictionary keys
             ((and (eq (char-before beg) ?:)
                   (not (eq (char-before (1- beg)) ?:))
                   ;; Except for when it is part of range expressions like 1:len
                   (not (memq (char-syntax (char-before (1- beg))) '(?w ?_))))
              (setcar prefix (concat ":" (car prefix))))))))))))

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
manual for <section>.  Otherwise follow the link normally."
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

(defun jupyter-julia-add-prompt (prompt color)
  "Display PROMPT at the beginning of the cell using COLOR as the foreground.
Make the character after `point' invisible."
  (add-text-properties (point) (1+ (point)) '(invisible t rear-nonsticky t))
  (let ((ov (make-overlay (point) (1+ (point)) nil t))
        (md (propertize prompt
             'fontified t
             'font-lock-face `((:foreground ,color)))))
    (overlay-put ov 'after-string (propertize " " 'display md))
    (overlay-put ov 'evaporate t)))

(defun jupyter-julia-pkg-prompt ()
  "Return the Pkg prompt.
If the Pkg prompt can't be retrieved from the kernel, return
nil."
  (when-let* ((msg (jupyter-wait-until-received :execute-reply
                     (jupyter-send-execute-request jupyter-current-client
                       :code ""
                       :silent t
                       :user-expressions
                       (list :prompt "import Pkg; Pkg.REPLMode.promptf()"))
                     ;; Longer timeout to account for initial Pkg import and
                     ;; compilation.
                     jupyter-long-timeout)))
    (cl-destructuring-bind (&key prompt &allow-other-keys)
        (jupyter-message-get msg :user_expressions)
      (cl-destructuring-bind (&key status data &allow-other-keys)
          prompt
        (when (equal status "ok")
          (plist-get data :text/plain))))))

(cl-defmethod jupyter-repl-after-change ((_type (eql insert)) beg _end
                                         &context (jupyter-lang julia))
  "Change the REPL prompt when a REPL mode is entered."
  (when (= beg (jupyter-repl-cell-code-beginning-position))
    (save-excursion
      (goto-char beg)
      (when (and (bound-and-true-p blink-paren-function)
                 (eq (char-syntax (char-after)) ?\)))
        ;; Spoof `last-command-event' so that a "No matching paren" message
        ;; doesn't happen.
        (setq last-command-event ?\[))
      (cl-case (char-after)
        (?\]
         (when-let* ((pkg-prompt (jupyter-julia-pkg-prompt)))
           (jupyter-julia-add-prompt
            (substring pkg-prompt 1 (1- (length pkg-prompt)))
            (aref ansi-color-names-vector 5)))) ; magenta
        (?\;
         (jupyter-julia-add-prompt
          "shell> " (aref ansi-color-names-vector 1))) ; red
        (?\?
         (jupyter-julia-add-prompt
          "help?> " (aref ansi-color-names-vector 3)))))) ; yellow
  (cl-call-next-method))

(cl-defmethod jupyter-repl-after-change ((_type (eql delete)) beg _len
                                         &context (jupyter-lang julia))
  "Reset the prompt if needed."
  (when (= beg (jupyter-repl-cell-code-beginning-position))
    (jupyter-repl-cell-reset-prompt)))

;;; REPL font lock

(defun jupyter-julia--propertize-repl-mode-char (beg end)
  (jupyter-repl-map-cells beg end
    (lambda ()
      ;; Handle Julia package prompt so `syntax-ppss' works properly.
      (when (and (eq (char-syntax (char-after (point-min))) ?\))
                 (= (point-min)
                    (save-restriction
                      (widen)
                      ;; Looks at the position before the narrowed cell-code
                      ;; which is why the widen is needed here.
                      (jupyter-repl-cell-code-beginning-position))))
        (put-text-property
         (point-min) (1+ (point-min)) 'syntax-table '(1 . ?.))))
    #'ignore))

;;; `jupyter-repl-after-init'

(defun jupyter-julia--setup-hooks (client)
  (let ((jupyter-inhibit-handlers t))
    (jupyter-send-execute-request client
      :store-history nil
      :silent t
      ;; This is mainly for supporting the :dir header argument in `org-mode'
      ;; source blocks.  We send this after initializing the REPL and after a
      ;; kernel restart so that we can get proper line numbers when an error
      ;; occurs.
      :code "\
if !isdefined(Main, :__JUPY_saved_dir)
    Core.eval(Main, :(__JUPY_saved_dir = Ref(\"\")))
    let popdir = () -> begin
                if !isempty(Main.__JUPY_saved_dir[])
                    cd(Main.__JUPY_saved_dir[])
                    Main.__JUPY_saved_dir[] = \"\"
                end
            end
        IJulia.push_posterror_hook(popdir)
        IJulia.push_postexecute_hook(popdir)
    end
end")))

(cl-defmethod jupyter-repl-after-init (&context (jupyter-lang julia))
  (add-function
   :after (local 'syntax-propertize-function)
   #'jupyter-julia--propertize-repl-mode-char)
  (jupyter-julia--setup-hooks jupyter-current-client)
  ;; Setup hooks after restart as well
  (jupyter-add-hook jupyter-current-client 'jupyter-iopub-message-hook
    (lambda (client msg)
      (when (jupyter-message-status-starting-p msg)
        (jupyter-julia--setup-hooks client)))))

;;; `jupyter-org'

(cl-defmethod jupyter-org-error-location (&context (jupyter-lang julia))
  (when (and (re-search-forward "^Stacktrace:" nil t)
             (re-search-forward
              "top-level scope at In\\[[0-9]+\\]:\\([0-9]+\\)" nil t))
    (string-to-number (match-string 1))))

(cl-defmethod org-babel-jupyter-transform-code (code changelist &context (jupyter-lang julia))
  (when (plist-get changelist :dir)
    (setq code
          ;; Stay on one line so that tracebacks will report the right line
          ;; numbers
          (format "Main.__JUPY_saved_dir[] = pwd(); cd(\"%s\"); %s"
                  (plist-get changelist :dir) code)))
  code)

(defun jupyter-julia--try-parse-dataframe (table-as-html-str)
  "Try to parse DataFrame to transform to org table.

TABLE-AS-HTML-STR is the jupyter html representation of a DataFrame, like
<html>… <table class=data-frame>…

Returns \(cons 'ok org-table-representing-dataframe\), nil otherwise."
  (when (not (null table-as-html-str))
    (when-let* ((e (with-temp-buffer
                     (insert table-as-html-str)
                     (libxml-parse-html-region (point-min) (point-max))))
                (headers-and-rows (pcase e
                                    (`(html , _html-attribs
                                              (body , _body-attribs
                                                      (table ((class . "data-frame"))
                                                             . ,rest)))
                                     rest)))
                (first-tr (pcase (car headers-and-rows)
                            (`(thead ,_attr . ,rest)
                             (car rest))))
                (th-nodes (pcase first-tr
                            (`(tr ,_attrib . ,ths)
                             ths)))
                (col-names
                 (mapcar
                  'caddr
                  ;; inline `seq-drop-while' or`--drop-while'
                  (if-let ((pos
                            (cl-position-if
                             (pcase-lambda (`(th ,_attrib . ,col-name))
                               col-name)
                             th-nodes)))
                      (cl-subseq th-nodes pos)
                    th-nodes)))
                (second-header-row
                 (pcase (-first-item headers-and-rows)
                   (`(thead ,_attr . ,rest)
                    (cadr
                     rest))))
                (col-types
                 (mapcar 'caddr
                         (--drop-while
                          (pcase it
                            (`(th ,_attrib . ,col-name)
                             (null col-name))
                            (_
                             t))
                          second-header-row)))
                (data-rows
                 (pcase (cadr headers-and-rows)
                   (`(tbody ,_attrib (p ,_pattrib ,_table-name) . ,rest)
                    rest)))
                (extracted-data
                 (mapcar
                  (pcase-lambda (`(tr ,_tr-attrib . ,rest))
                    (let ((row-without-numbering
                           (cdr rest)))
                      (mapcar
                       (pcase-lambda (`(td ,_td-attrib . ,value))
                         (car value))
                       row-without-numbering)))
                  data-rows)))
      (cons 'ok
            (jupyter-org-scalar
             (append
              (list col-names
                    'hline)
              extracted-data))))))

(cl-defmethod jupyter-org-result ((_mime (eql :text/html))
                                  &context (jupyter-lang julia)
                                  &optional _params &rest _ignore)
  "Try to parse DataFrame to transform to org table.

Based off `jupyter-org-result' @ jupyter-python.el"
  (pcase-let* ((result (cl-call-next-method))
               (`(export-block (:type ,_export-type
                                      :value ,table-as-html-str))
                result)
               (`(ok . ,org-table)
                (jupyter-julia--try-parse-dataframe table-as-html-str)))
    (cond
     (org-table
      org-table)
     ;; else, return as-is
     (t result))))

(provide 'jupyter-julia)

;;; jupyter-julia.el ends here
