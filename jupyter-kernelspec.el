;;; jupyter-kernelspec.el --- Jupyter kernelspecs -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 17 Jan 2018
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

(defgroup jupyter-kernelspec nil
  "Jupyter kernelspecs"
  :group 'jupyter)

(require 'json)

(defvar jupyter--kernelspecs nil
  "An alist matching kernel names to their kernelspec directories.")

(defun jupyter-read-kernelspec (dir)
  "Return the kernelspec found in DIR.
If DIR contains a \"kernel.json\" file, assume that it is the
kernelspec of a kernel and return the plist created by a call to
`json-read-file'."
  (let ((json-object-type 'plist)
        (json-array-type 'list)
        (json-false nil)
        (file (expand-file-name "kernel.json" dir)))
    (if (file-exists-p file) (json-read-file file)
      (error "No kernel.json file found in %s" dir))))

(defun jupyter-available-kernelspecs (&optional refresh)
  "Get the available kernelspecs.
Return an alist mapping kernel names to (DIRECTORY . PLIST) pairs
where DIRECTORY is the resource directory of the kernel and PLIST
is its kernelspec plist. The alist is formed by a call to the
shell command

    jupyter kernelspec list

By default the available kernelspecs are cached. To force an
update of the cached kernelspecs, give a non-nil value to
REFRESH."
  (when (or (not jupyter--kernelspecs) refresh)
    (setq jupyter--kernelspecs
          (mapcar (lambda (s) (let ((s (split-string s " " 'omitnull)))
                      ;; (kernel-name . (dir . spec))
                      (cons (car s) (cons (cadr s)
                                          (jupyter-read-kernelspec (cadr s))))))
             (seq-subseq
              (split-string
               (shell-command-to-string "jupyter kernelspec list")
               "\n" 'omitnull "[ \t]+")
              1))))
  jupyter--kernelspecs)

(defun jupyter-get-kernelspec (name &optional refresh)
  "Get the kernelspec for a kernel named NAME.
If no kernelspec is found, throw an error. Otherwise return a
cons cell

    (DIRECTORY . PLIST)

where DIRECTORY is the resource directory of the kernel named
NAME and PLIST is its kernelspec plist. Optional argument REFRESH
has the same meaning as in `jupyter-available-kernelspecs'."
  (or (cdr (assoc name (jupyter-available-kernelspecs refresh)))
      (error "No kernelspec found (%s)" name)))

(defun jupyter-find-kernelspecs (prefix &optional refresh)
  "Find all kernelspecs for kernels that have names matching PREFIX.
Return a list of all the kernelspecs whose kernel names begin
with PREFIX. If no kernel matches PREFIX, return nil. Use
`jupyter-available-kernelspecs' to match against kernel names.
Each element of the returned list in the case of a match is a
cons cell

    (KERNEL-NAME . (DIRECTORY . PLIST))

where KERNEL-NAME is the name of the kernel that begins with
PREFIX and PLIST is the kernelspec PLIST read from the
\"kernel.json\" file in the kernel's resource DIRECTORY.

Optional argument REFRESH has the same meaning as in
`jupyter-available-kernelspecs'."
  (when prefix
    (delq nil (mapcar (lambda (s) (and (string-prefix-p prefix (car s)) s))
                 (jupyter-available-kernelspecs refresh)))))

(provide 'jupyter-kernelspec)

;;; jupyter-kernelspec.el ends here
