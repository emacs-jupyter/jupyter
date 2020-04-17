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

(cl-defstruct jupyter-kernelspec
  (name "python"
        :type string
        :documentation "The name of the kernelspec."
        :read-only t)
  (plist nil
         :type list
         :documentation "The kernelspec as a property list."
         :read-only t)
  (resource-directory nil
   :type (or null string)
   :documentation "The resource directory."
   :read-only t))

(defvar jupyter--kernelspecs (make-hash-table :test #'equal :size 5)
  "A hash table mapping hosts to the kernelspecs available on them.")

(defun jupyter-available-kernelspecs (&optional refresh)
  "Return the available kernelspecs.
Return a list of `jupyter-kernelspec's available on the host
associated with the `default-directory'.  If `default-directory'
is a remote file name, return the list of available kernelspecs
on the remote system.  The kernelspecs on the local system are
returned otherwise.

On any system, the list is formed by parsing the output of the
shell command

    jupyter kernelspec list --json

By default the available kernelspecs are cached.  To force an
update of the cached kernelspecs, give a non-nil value to
REFRESH."
  (let* ((host (or (file-remote-p default-directory) "local"))
         (kernelspecs
          (or (and (not refresh) (gethash host jupyter--kernelspecs))
              (let ((specs
                     (plist-get
                      (jupyter-read-plist-from-string
                       (or (jupyter-command "kernelspec" "list" "--json" "--log-level" "ERROR")
                           (error "Can't obtain kernelspecs from jupyter shell command")))
                      :kernelspecs)))
                (puthash
                 host (cl-loop
                       for (kname spec) on specs by #'cddr
                       for name = (substring (symbol-name kname) 1)
                       for dir = (plist-get spec :resource_dir)
                       collect (make-jupyter-kernelspec
                                :name name
                                :resource-directory (concat
                                                     (unless (string= host "local") host)
                                                     dir)
                                :plist (plist-get spec :spec)))
                 jupyter--kernelspecs)))))
    kernelspecs))

(defun jupyter-get-kernelspec (name &optional specs refresh)
  "Get the kernelspec for a kernel named NAME.
If no kernelspec is found, return nil.  Otherwise return the
kernelspec plist for the kernel names NAME.  Optional argument
REFRESH has the same meaning as in
`jupyter-available-kernelspecs'.

If SPECS is provided, it is a list of kernelspecs that will be
searched, otherwise the kernelspecs returned by
`jupyter-available-kernelspecs' are used."
  (cl-loop
   for kernelspec in (or specs (jupyter-available-kernelspecs refresh))
   thereis (when (string= (jupyter-kernelspec-name kernelspec) name)
             kernelspec)))

(defun jupyter-find-kernelspecs (re &optional specs refresh)
  "Find all specs of kernels that have names matching RE.
RE is a regular expression use to match the name of a kernel.
Return a list of `jupyter-kernelspec' objects.

If SPECS is non-nil search SPECS, otherwise search the specs
returned by `jupyter-available-kernelspecs'.

Optional argument REFRESH has the same meaning as in
`jupyter-available-kernelspecs'."
  (cl-remove-if-not
   (lambda (kernelspec)
     (string-match-p re (jupyter-kernelspec-name kernelspec)))
   (or specs (jupyter-available-kernelspecs refresh))))

(defun jupyter-guess-kernelspec (name &optional specs refresh)
  "Return the first kernelspec starting with NAME.
Raise an error if no kernelspec could be found.

SPECS and REFRESH have the same meaning as in
`jupyter-find-kernelspecs'."
  (or (car (jupyter-find-kernelspecs (format "^%s" name) specs refresh))
      (error "No valid kernelspec for kernel name (%s)" name)))

(defun jupyter-completing-read-kernelspec (&optional specs refresh)
  "Use `completing-read' to select a kernel and return its kernelspec.

SPECS is a list of kernelspecs that will be used for completion,
if it is nil the `jupyter-available-kernelspecs' will be used.

Optional argument REFRESH has the same meaning as in
`jupyter-available-kernelspecs'."
  (let* ((specs (or specs (jupyter-available-kernelspecs refresh)))
         (display-names (if (null specs) (error "No kernelspecs available")
                          (mapcar (lambda (k)
                                    (plist-get
                                     (jupyter-kernelspec-plist k)
                                     :display_name))
                                  specs)))
         (name (completing-read "kernel: " display-names nil t)))
    (when (equal name "")
      (error "No kernelspec selected"))
    (nth (- (length display-names)
            (length (member name display-names)))
         specs)))

(provide 'jupyter-kernelspec)

;;; jupyter-kernelspec.el ends here
