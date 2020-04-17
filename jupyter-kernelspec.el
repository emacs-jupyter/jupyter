;;; jupyter-kernelspec.el --- Jupyter kernelspecs -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 17 Jan 2018

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

;; Functions to work with kernelspecs found by the shell command
;;
;;     jupyter kernelspec list

;;; Code:

(require 'json)
(require 'jupyter-env)

(defgroup jupyter-kernelspec nil
  "Jupyter kernelspecs"
  :group 'jupyter)

(declare-function jupyter-read-plist "jupyter-base" (file))
(declare-function jupyter-read-plist-from-string "jupyter-base" (file))

(defvar jupyter--kernelspecs (make-hash-table :test #'equal :size 5)
  "An alist matching kernel names to their kernelspec directories.")

(defun jupyter-available-kernelspecs (&optional refresh)
  "Get the available kernelspecs.
Return an alist mapping kernel names to (DIRECTORY . PLIST) pairs
where DIRECTORY is the resource directory of the kernel and PLIST
is its kernelspec plist.  The alist is formed by parsing the
output of the shell command

    jupyter kernelspec list

By default the available kernelspecs are cached.  To force an
update of the cached kernelspecs, give a non-nil value to
REFRESH.

If the `default-directory' is a remote directory, return the
mapping for the kernelspecs on the remote host.  In this case,
each DIRECTORY will be a remote file name."
  (let ((host (or (file-remote-p default-directory) "local")))
    (or (and (not refresh) (gethash host jupyter--kernelspecs))
        (let ((specs (plist-get
                      (jupyter-read-plist-from-string
                       (or (jupyter-command "kernelspec" "list" "--json")
                           (error "Can't obtain kernelspecs from jupyter shell command")))
                      :kernelspecs)))
          (puthash
           host (cl-loop
                 for (name spec) on specs by #'cddr
                 for dir = (concat (unless (equal host "local") host)
                                   (plist-get spec :resource_dir))
                 collect (cons (substring (symbol-name name) 1)
                               (cons dir (plist-get spec :spec))))
           jupyter--kernelspecs)))))

(defun jupyter-get-kernelspec (name &optional refresh)
  "Get the kernelspec for a kernel named NAME.
If no kernelspec is found, return nil.  Otherwise return the
kernelspec plist for the kernel names NAME.  Optional argument
REFRESH has the same meaning as in
`jupyter-available-kernelspecs'."
  (cdr (assoc name (jupyter-available-kernelspecs refresh))))

(defun jupyter-find-kernelspecs (re &optional specs refresh)
  "Find all specs of kernels that have names matching matching RE.
RE is a regular expression use to match the name of a kernel.
Return an alist with elements of the form:

    (KERNEL-NAME . (DIRECTORY . PLIST))

where KERNEL-NAME is the name of a kernel that matches RE,
DIRECTORY is the kernel's resource directory, and PLIST is the
kernelspec propery list read from the \"kernel.json\" file in the
resource directory.

If SPECS is non-nil search SPECS, otherwise search the specs
returned by `jupyter-available-kernelspecs'.

Optional argument REFRESH has the same meaning as in
`jupyter-available-kernelspecs'."
  (cl-remove-if-not
   (lambda (s) (string-match-p re (car s)))
   (or specs (jupyter-available-kernelspecs refresh))))

(defun jupyter-guess-kernelspec (name &optional specs refresh)
  "Return the first kernelspec matching NAME.
Raise an error if no kernelspec could be found.

SPECS and REFRESH have the same meaning as in
`jupyter-find-kernelspecs'."
  (or (car (jupyter-find-kernelspecs name specs refresh))
      (error "No valid kernelspec for kernel name (%s)" name)))

(defun jupyter-completing-read-kernelspec (&optional specs refresh)
  "Use `completing-read' to select a kernel and return its kernelspec.
The returned kernelspec has the form

    (KERNEL-NAME . (DIRECTORY . PLIST))

where KERNEL-NAME is the name of the kernel, DIRECTORY is the
resource directory of the kernel, and PLIST is the kernelspec
plist.

If SPECS is non-nil then it should be a list of kernelspecs that
will be used to select from otherwise the list of kernelspecs
will be taken from `jupyter-available-kernelspecs'.

Optional argument REFRESH has the same meaning as in
`jupyter-available-kernelspecs'."
  (let* ((specs (or specs (jupyter-available-kernelspecs refresh)))
         (display-names (if (null specs) (error "No kernelspecs available")
                          (mapcar (lambda (k) (plist-get (cddr k) :display_name))
                             specs)))
         (name (completing-read "kernel: " display-names nil t)))
    (when (equal name "")
      (error "No kernelspec selected"))
    (nth (- (length display-names)
            (length (member name display-names)))
         specs)))

(provide 'jupyter-kernelspec)

;;; jupyter-kernelspec.el ends here
