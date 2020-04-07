;;; jupyter-c++.el --- Jupyter support for C++ -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 12 April 2019

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

;; Support methods for integration with C++.

;;; Code:

(require 'jupyter-repl)

(cl-defmethod jupyter-repl-initialize-fontification (&context (jupyter-lang c++))
  "Copy buffer local variables used for fontification to the REPL buffer."
  (cl-loop
   with c-vars = (jupyter-with-repl-lang-buffer
                   (cl-loop
                    for var-val in (buffer-local-variables)
                    if (string-prefix-p "c-" (symbol-name (car var-val)))
                    collect var-val))
   for (var . val) in c-vars
   do (set (make-local-variable var) val))
  (cl-call-next-method))

(provide 'jupyter-c++)

;;; jupyter-c++.el ends here
