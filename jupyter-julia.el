;;; jupyter-julia.el --- Jupyter support for Julia -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 23 Oct 2018
;; Version: 0.8.0

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

(eval-when-compile (require 'subr-x))
(require 'jupyter-repl)

(declare-function julia-latexsub-or-indent "ext:julia-mode" (arg))

(cl-defmethod jupyter-indent-line (&context (major-mode julia-mode))
  "Call `julia-latexsub-or-indent'."
  (call-interactively #'julia-latexsub-or-indent))

(cl-defmethod jupyter-load-file-code (file &context (jupyter-lang julia))
  (format "include(\"%s\");" file))

;;; Support functions

;;;; Parsing identifiers

(defun jupyter-julia-id-start-char-p (c)
  "Return non-nil if C can start an identfier."
  (and c (or
          ;; Let @ start an identifier, this differs from Julia's parser, but
          ;; is useful for practical purposes
          (eq c ?@)
          (<= ?a c ?z) (<= ?A c ?Z) (eq c ?_)
          (and (not (or (< c #xa1) (> c #x10ffff)))
               (jupyter-julia-id-cat-start-char-p
                c (get-char-code-property c 'general-category))))))

(defun jupyter-julia-id-char-p (c)
  "Return non-nil if C can be a character in an identifier."
  (and c (or
          (eq c ?@)
          (<= ?a c ?z) (<= ?A c ?Z) (eq c ?_)
          (<= ?0 c ?9)
          (eq c ?!)
          (and (not (or (< c #xa1) (> c #x10ffff)))
               (or (<= #x2032 c #x2037)
                   (= c #x2057)
                   (let ((cat (get-char-code-property c 'general-category)))
                     (or (memq cat '(Mn Mc Nd Pc Sk Me No))
                         (jupyter-julia-id-cat-start-char-p c cat))))))))

(defun jupyter-julia-id-cat-start-char-p (c cat)
  "Return non-nil if C is a valid unicode identifier start character.
CAT is the unicode category of C."
  (when c
    (or (memq cat '(Lu Ll Lt Lm Lo Nl Sc))
        (and (eq cat 'So) (not (<= #x2190 c #x21ff)))
        (and (<= #x2140 c #x2a1c)
             (or (<= #x2140 c #x2144)
                 (memq c '(#x223f #x22be #x22bf #x22a4 #x22a5))
                 (and (<= #x2202 c #x2233)
                      (or (memq c '(#x2202 #x2205 #x2205
                                           #x2207 #x220e #x220f
                                           #x2210 #x2211
                                           #x221e #x221f))
                          (>= c #x222b)))
                 (<= #x22c0 c #x22c3)
                 (<= #x25f8 c #x25ff)
                 (and (>= c #x266f)
                      (or (memq c '(#x266f #x27d8 #x27d9))
                          (<= #x27c0 c #x2c71)
                          (<= #x29b0 c #x29b4)
                          (<= #x2a00 c #x2a06)
                          (<= #x2a09 c #x2a16)
                          (= c #x2a1b) (= c #x2a1c)))))
        (and (>= c #x1d6c1)
             (or (memq c '(#x1d6c1 #x1d6db
                                   #x1d6fb #x1d715
                                   #x1d735 #x1d74f
                                   #x1d76f #x1d789
                                   #x1d7a9 #x1d7c3))))
        (<= #x207a c #x207e)
        (<= #x208a c #x208e)
        (<= #x2220 c #x2222)
        (<= #x299b c #x29af)
        (= c #x2118) (= c #x212e)
        (<= #x309b c #x309c))))

(defun jupyter-julia-identifier-at-point ()
  (when (jupyter-julia-id-char-p (char-after))
    (buffer-substring-no-properties
     (save-excursion (jupyter-julia-beginning-of-identifier))
     (save-excursion (jupyter-julia-end-of-identifier)))))

(defun jupyter-julia-beginning-of-identifier ()
  (when (jupyter-julia-id-char-p (char-after))
    (while (jupyter-julia-id-char-p (char-before))
      (backward-char))
    (while (not (jupyter-julia-id-start-char-p (char-after)))
      (forward-char))))

(defun jupyter-julia-end-of-identifier ()
  (when (jupyter-julia-id-char-p (char-after))
    (while (jupyter-julia-id-char-p (char-after))
      (forward-char))))

(defun jupyter-julia-identifier-with-dots-at-point ()
  (when (or (jupyter-julia-id-char-p (char-after))
            (eq (char-after) ?.))
    (when (eq (char-after) ?.)
      (unless (bobp)
        (backward-char)))
    (let ((beg (point))
          (end (point)))
      (save-excursion
        (jupyter-julia-end-of-identifier)
        (while (and (eq (char-after (point)) ?.)
                    (jupyter-julia-id-start-char-p
                     (char-after (1+ (point)))))
          (forward-char)
          (jupyter-julia-end-of-identifier))
        (setq end (point))
        (goto-char beg)
        (jupyter-julia-beginning-of-identifier)
        (while (and (eq (char-before (point)) ?.)
                    (jupyter-julia-id-char-p
                     (char-before (1- (point)))))
          (backward-char 2)
          (jupyter-julia-beginning-of-identifier))
        (setq beg (point)))
      (buffer-substring-no-properties beg end))))

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
                   (not (eq (char-before (1- beg)) ?:)))
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

;;; XREF

(defvar jupyter-julia--bindir nil)

;; TODO: This may be unnecessary when using @functionloc since it looks like it
;; resolves paths on its own instead of relying on the LineNumber nodes.
(defun jupyter-julia--recover-local-path (path)
  (if (file-name-absolute-p path)
      (let ((begin (string-match-p (jupyter-join-path "share" "julia") path)))
        (if (or (file-exists-p path)
                (not (and begin jupyter-julia--bindir)))
            path
          (expand-file-name
           (substring path begin)
           jupyter-julia--bindir)))
    ;; Assume PATH is a file relative to base
    (expand-file-name
     path (expand-file-name
           (jupyter-join-path "share" "julia" "base")
           jupyter-julia--bindir))))

(defun jupyter-julia--xref-definitions (identifier)
  (when (oref jupyter-current-client manager)
    ;; When the client is connected to a local kernel, ensure that we know
    ;; where the base Julia library files are at.
    ;;
    ;; TODO: Generalize to remote directories using TRAMP file names (/ssh:) to
    ;; access the remote host?
    (unless jupyter-julia--bindir
      (setq jupyter-julia--bindir
            (file-name-directory
             (substring (jupyter-eval "Sys.BINDIR") 1 -1)))))
  (when-let* ((jupyter-inhibit-handlers t)
              (code (format "%s(" identifier))
              (msg (jupyter-wait-until-received :complete-reply
                     (jupyter-send-complete-request
                         jupyter-current-client
                       :code code :pos (length code))))
              (session-id (jupyter-session-id
                           (oref jupyter-current-client session)))
              (make-xref
               (lambda (x)
                 (let ((summary (get-text-property 0 'docsig x))
                       (loc-info (get-text-property 0 'location x)))
                   (and summary loc-info
                        (xref-make
                         (substring
                          (jupyter-fontify-according-to-mode
                           (jupyter-kernel-language-mode jupyter-current-client)
                           (concat "function " summary)
                           t)
                          9)
                         (jupyter-xref-file-location
                          :file (jupyter-julia--recover-local-path
                                 (car loc-info))
                          :line (1- (cdr loc-info))
                          :column (or (string-match-p identifier summary) 0)
                          :session session-id)))))))
    (jupyter-with-message-content msg
        (status matches metadata)
      (when (equal status "ok")
        (delq nil
              (sort (mapcar make-xref (jupyter-completion-construct-candidates
                                  matches metadata))
                    (lambda (a b)
                      (< (oref (oref a location) line)
                         (oref (oref b location) line)))))))))

(defun jupyter-julia--xref-apropos (pattern)
  (when-let* ((msg (jupyter-eval
                    (format "Base.Docs.apropos(\"%s\")" pattern)))))

  )

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql jupyter))
                                                &context (jupyter-lang julia))
  (jupyter-julia-identifier-with-dots-at-point))

(cl-defmethod xref-backend-definitions ((_backend (eql jupyter)) identifier
                                        &context (jupyter-lang julia))
  (jupyter-julia--xref-definitions identifier))

(cl-defmethod xref-backend-apropos ((_backend (eql jupyter)) pattern
                                    &context (jupyter-lang julia))
  (jupyter-julia--xref-apropos pattern))

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
  (jupyter-repl-cell-cond
      beg end
      ;; Handle Julia package prompt so `syntax-ppss' works properly.
      (when (and (eq (char-syntax (char-after (point-min))) ?\))
                 (= (point-min)
                    (save-restriction
                      (widen)
                      ;; Looks at the position before the narrowed cell-code
                      ;; which is why the widen is needed here.
                      (jupyter-repl-cell-code-beginning-position))))
        (put-text-property
         (point-min) (1+ (point-min)) 'syntax-table '(1 . ?.)))))

;;; `jupyter-repl-after-init'

(defun jupyter-julia--setup-hooks (client)
  (let ((jupyter-inhibit-handlers t))
    (jupyter-send-execute-request client
      :store-history nil
      :silent t
      ;; This is mainly for supporting the :dir header argument in `org-mode'
      ;; source blocks. We send this after initializing the REPL and after a
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

(provide 'jupyter-julia)

;;; jupyter-julia.el ends here
