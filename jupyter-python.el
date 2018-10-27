;;; jupyter-python.el --- Jupyter support for python -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 23 Oct 2018
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

;;

;;; Code:

(require 'jupyter-repl)
(require 'jupyter-org-client)

(declare-function org-babel-python-table-or-string "ob-python")

(cl-defmethod jupyter-handle-error :after ((client jupyter-repl-client)
                                           req ename _evalue _traceback
                                           &context (jupyter-lang python))
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
             (make-string (if (> len 4) len 4) ? ))))))))

(cl-defmethod jupyter-load-file-code (file &context (jupyter-lang python))
  (concat "%run " file))

(cl-defmethod jupyter-org-transform-result (render-result
                                            &context (jupyter-lang python))
  (cond
   ((equal (car render-result) "scalar")
    (cons "scalar" (org-babel-python-table-or-string (cdr render-result))))
   (t render-result)))

(provide 'jupyter-python)

;;; jupyter-python.el ends here
