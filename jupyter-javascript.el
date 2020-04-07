;;; jupyter-javascript.el --- Jupyter support for Javascript -*- lexical-binding: t -*-

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

;; Support methods for integration with Javascript.

;;; Code:

(require 'jupyter-repl)

(declare-function js2-parse "ext:js2-mode")
(declare-function js2-mode-apply-deferred-properties "ext:js2-mode")

(cl-defmethod jupyter-repl-after-init (&context (jupyter-lang javascript)
                                                (jupyter-repl-mode js2-mode))
  "If `js2-mode' is used for Javascript kernels, enable syntax highlighting.
`js2-mode' does not use `font-lock-defaults', but their own
custom method."
  (add-hook 'after-change-functions
            (lambda (_beg _end len)
              ;; Insertions only
              (when (= len 0)
                (unless (jupyter-repl-cell-finalized-p)
                  (let ((cbeg (jupyter-repl-cell-code-beginning-position))
                        (cend (jupyter-repl-cell-code-end-position)))
                    (save-restriction
                      (narrow-to-region cbeg cend)
                      (js2-parse)
                      (js2-mode-apply-deferred-properties))))))
            t t))

(provide 'jupyter-javascript)

;;; jupyter-javascript.el ends here
