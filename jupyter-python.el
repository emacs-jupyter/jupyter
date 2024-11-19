;;; jupyter-python.el --- Jupyter support for python -*- lexical-binding: t -*-

;; Copyright (C) 2018-2024 Nathaniel Nicandro

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

;; Support methods for integration with Python.

;;; Code:

(require 'jupyter-repl)
(require 'jupyter-org-client)

(declare-function org-babel-python-table-or-string "ob-python")

(cl-defmethod jupyter-handle-error :after ((client jupyter-repl-client) req msg
                                           &context (jupyter-lang python)
                                           (major-mode jupyter-repl-mode))
  "Add spacing between the first occurance of ENAME and \"Traceback\".
Do this only when the traceback of REQ was inserted into the REPL
buffer."
  (unless (equal (jupyter-message-parent-type msg) "comm_msg")
    (jupyter-with-repl-buffer client
      (jupyter-with-message-content msg (ename)
        (save-excursion
          (jupyter-repl-goto-cell req)
          (goto-char (jupyter-repl-cell-code-end-position))
          (when (and (search-forward ename nil t)
                     (looking-at "Traceback"))
            (let ((len (- fill-column
                          jupyter-repl-prompt-margin-width
                          (- (point) (line-beginning-position))
                          (- (line-end-position) (point)))))
              (insert-and-inherit
               (propertize (make-string (if (> len 4) len 4) ? )
                           'read-only t)))))))))

(cl-defmethod jupyter-insert :around ((msg cons)
                                      &context (jupyter-lang python)
                                      &rest _ignore)
  "Fontify docstrings after inserting inspect messages."
  (let ((mime (cl-call-next-method)))
    (prog1 mime
      (cond
       ((and (eq mime :text/plain)
             (string= (jupyter-message-type msg) "inspect_reply"))
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

;;; `jupyter-org'

(cl-defmethod jupyter-org-result ((_mime (eql :text/plain)) _content params
                                  &context (jupyter-lang python))
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

(defun jupyter-python-raw-string (code)
  "Construct a Python raw string from CODE.
Return valid Python code that can be interpreted by Python as if
CODE was a raw string in Python."
  (mapconcat
   (lambda (s)
     (let ((begin (if (string-prefix-p "\"" s)
                      (if (string-prefix-p "\"\"" s)
                          2
                        1)
                    0))
           (end (if (string-suffix-p "\"" s)
                    (if (string-suffix-p "\"\"" s)
                        -2
                      -1)
                  nil)))
       (setq s (substring s begin end))
       (let ((slashes (when (string-match "\\(\\\\+\\)$" s)
                        (prog1 (match-string 1 s)
                          (setq s (substring s 0 (match-beginning 1)))))))
         (concat (cond
                  ((= begin 2) "'\"\"' + ")
                  ((= begin 1) "'\"' + ")
                  (t ""))
                 "r\"\"\"" s "\"\"\""
                 (if slashes
                     (concat " + '" (concat slashes slashes) "'")
                   "")
                 (cond
                  ((null end) "")
                  ((= end -2) " + '\"\"'")
                  ((= end -1) " + '\"'"))))))
   (split-string code "\"\\{3\\}")
   " + '\"\"\"' + "))

(cl-defmethod org-babel-jupyter-transform-code (code changelist &context (jupyter-lang python))
  (when (plist-get changelist :dir)
    (setq code
          (format "\
import os
__JUPY_saved_dir = os.getcwd()
os.chdir(\"%s\")
try:
    get_ipython().run_cell(%s)
finally:
    os.chdir(__JUPY_saved_dir)"
                  (plist-get changelist :dir)
                  (jupyter-python-raw-string code))))
  code)

(provide 'jupyter-python)

;;; jupyter-python.el ends here
