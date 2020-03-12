;;; jupyter-wolfram-language.el --- Jupyter support for Wolfram -*- lexical-binding: t -*-

;; Copyright (C) 2020 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 12 Mar 2020

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

;; Support methods for integration with Wolfram Language

;;; Code:

(require 'jupyter-mime)

(cl-defmethod jupyter-insert :around ((plist cons)
                                      &context (jupyter-lang wolfram-language)
                                      &optional metadata)
  "Fontify docstrings after inserting inspect messages."
  (cl-destructuring-bind (data metadata)
      (jupyter-normalize-data plist metadata)
    (when (and (equal (plist-get data :text/html) "")
               (plist-get data :text/plain))
      (plist-put data :text/html nil))
    (cl-call-next-method data metadata)))

(provide 'jupyter-wolfram-language)

;;; jupyter-wolfram-language.el ends here
