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

(require 'jupyter-org-client)

(declare-function org-babel-python-table-or-string "ob-python")

(cl-defmethod jupyter-org-transform-result (render-result
                                            &context (jupyter-lang python))
  (cond
   ((equal (car render-result) "scalar")
    (cons "scalar" (org-babel-python-table-or-string (cdr render-result))))
   (t render-result)))

(provide 'jupyter-python)

;;; jupyter-python.el ends here
