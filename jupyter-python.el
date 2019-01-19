;;; jupyter-python.el --- Jupyter support for python -*- lexical-binding: t -*-

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

;; Support methods for integration with Python.

;;; Code:

(require 'jupyter-repl)
(require 'jupyter-org-client)

(declare-function org-babel-python-table-or-string "ob-python")

(cl-defmethod jupyter-handle-error :after ((client jupyter-repl-client)
                                           req ename _evalue _traceback
                                           &context (jupyter-lang python)
                                           (major-mode jupyter-repl-mode))
  "Add spacing between the first occurance of ENAME and \"Traceback\".
Do this only when the traceback of REQ was inserted into the REPL
buffer."
  (unless (eq (jupyter-message-parent-type
               (jupyter-request-last-message req))
              :comm-msg)
    (jupyter-with-repl-buffer client
      (save-excursion
        (jupyter-repl-goto-cell req)
        (goto-char (jupyter-repl-cell-code-end-position))
        (when (and (search-forward ename nil t)
                   (looking-at "Traceback"))
          (let ((len (- fill-column
                        jupyter-repl-prompt-margin-width
                        (- (point) (line-beginning-position))
                        (- (line-end-position) (point)))))
            (jupyter-repl-insert
             :inherit t
             (make-string (if (> len 4) len 4) ? ))))))))

(cl-defmethod jupyter-insert :around ((msg cons)
                                      &context (jupyter-lang python)
                                      &rest _)
  "Fontify docstrings after inserting inspect messages."
  (let ((mime (cl-call-next-method)))
    (prog1 mime
      (cond
       ((and (eq mime :text/plain)
             (eq (jupyter-message-type msg) :inspect-reply))
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^Docstring:" nil t)
            (jupyter-fontify-region-according-to-mode
             #'rst-mode (1+ (point))
             (or (and (re-search-forward "^\\(File\\|Type\\):" nil t)
                      (line-beginning-position))
                 (point-max))))))
       (t nil)))))

(cl-defmethod jupyter-load-file-code (file &context (jupyter-lang python))
  (concat "%run " file))

;;; XREF

(defvar jupyter-python-xref-setup-code
  "\
def __JUPY_find_definitions(references, lcode, line, col):
    import jedi
    script = jedi.Interpreter(lcode, [globals()], line=line, column=col)
    res = '('
    for d in (script.usages() if references else script.goto_definitions()):
        code = d.get_line_code().rstrip()
        path = d.module_path
        line = d.line
        col = d.column
        if code and path and line != 1:
            res += '(\"{}\" \"{}\" {} {}) '.format(code, path, line, col)
    return res + ')'")

(defvar jupyter-python-xref-definitions-code
  "__JUPY_find_definitions(%s, '''%s''', %s, %s)")

(defun jupyter-python--xref-definitions (code &optional references-p)
  (when-let* ((session-id (jupyter-session-id
                           (oref jupyter-current-client session)))
              ;; For some reason Jedi uses 0 based line numbers? It says that
              ;; it uses 1 based.
              (line (1- (line-number-at-pos)))
              (col (current-column))
              (res (jupyter-eval
                    (concat jupyter-python-xref-setup-code "\n"
                            (format jupyter-python-xref-definitions-code
                                    (if references-p "True" "False")
                                    code line col)))))
    (cl-loop
     for (summary path line column) in
     (read (substring res 1 -1))
     collect
     (xref-make
      (jupyter-fontify-according-to-mode
       (jupyter-kernel-language-mode jupyter-current-client)
       summary
       t)
      (jupyter-xref-file-location
       :file path
       :line line
       :column column
       :session session-id)))))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql jupyter))
                                                &context (jupyter-lang python))
  (python-info-current-symbol))

;; TODO: Send the code and then the identifier?
(cl-defmethod xref-backend-definitions ((_backend (eql jupyter)) _identifier
                                        &context (jupyter-lang python))
  (jupyter-python--xref-definitions
   (buffer-substring-no-properties (point-min) (point-max))))

(cl-defmethod xref-backend-references ((_backend (eql jupyter)) _identifier
                                       &context (jupyter-lang python))
  (jupyter-python--xref-definitions
   (buffer-substring-no-properties (point-min) (point-max))
   'references))

;;; `jupyter-org'

(cl-defmethod jupyter-org-result ((_mime (eql :text/plain))
                                  &context (jupyter-lang python)
                                  &optional params &rest _)
  (let ((result (cl-call-next-method)))
    (cond
     ((and (stringp result)
           (not (member "scalar" (alist-get :result-params params))))
      (org-babel-python-table-or-string result))
     (t result))))

(cl-defmethod jupyter-org-error-location (&context (jupyter-lang python))
  (and (or (save-excursion (re-search-forward "^----> \\([0-9]+\\)" nil t))
           (re-search-forward "^[\t ]*File.+line \\([0-9]+\\)$" nil t))
       (string-to-number (match-string 1))))

(cl-defmethod org-babel-jupyter-transform-code (code changelist &context (jupyter-lang python))
  (when (plist-get changelist :dir)
    (setq code
          (format "\
import os
__JUPY_saved_dir = os.getcwd()
os.chdir(\"%s\")
try:
    get_ipython().run_cell(\"\"\"%s\"\"\")
finally:
    os.chdir(__JUPY_saved_dir)"
                  (plist-get changelist :dir) code)))
  code)

(provide 'jupyter-python)

;;; jupyter-python.el ends here
