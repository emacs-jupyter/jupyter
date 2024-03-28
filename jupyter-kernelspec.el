;;; jupyter-kernelspec.el --- Jupyter kernelspecs -*- lexical-binding: t -*-

;; Copyright (C) 2018-2024 Nathaniel Nicandro

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
  "A hash table mapping hosts to the kernelspecs available on them.
The top level hash-table maps hosts to nested hash-tables keyed
on virtual environment path or nil for a system-wide Jupyter
install: hosts[hash-table] -> venv[hash-table] -> kernelspecs.")

(defun jupyter-kernelspecs-ensure-cache (host)
  "Return, creating if necessary, the hash-table for HOST."
  (let ((cache (gethash host jupyter--kernelspecs)))
    (if cache cache
      (puthash host (make-hash-table :test #'equal :size 5)
               jupyter--kernelspecs))))

(defun jupyter-kernelspecs-cache-put (host kernelspecs)
  "Cache KERNELSPECS available on HOST.
This takes into account any currently active virtual
environment."
  (let ((venv (getenv "VIRTUAL_ENV")))
    (let ((cache (jupyter-kernelspecs-ensure-cache host)))
      (puthash venv kernelspecs cache))))

(defun jupyter-kernelspecs-cache-get (host)
  "Retrieve cached KERNELSPECS available on HOST.
This takes into account any currently active virtual
environment."
  (let ((venv (getenv "VIRTUAL_ENV")))
    (let ((cache (jupyter-kernelspecs-ensure-cache host)))
      (gethash venv cache))))

(defun jupyter-available-kernelspecs (&optional refresh)
  "Return the available kernelspecs.
Return a list of `jupyter-kernelspec's available on the host
associated with the `default-directory'.  If `default-directory'
is a remote file name, return the list of available kernelspecs
on the remote system.  The kernelspecs on the local system are
returned otherwise (taking into account any currently active
virtual environment).

On any system, the list is formed by parsing the output of the
shell command

    jupyter kernelspec list --json

By default the available kernelspecs are cached.  To force an
update of the cached kernelspecs, give a non-nil value to
REFRESH."
  (let* ((host (or (file-remote-p default-directory) "local"))
         (kernelspecs
          (or (and (not refresh) (jupyter-kernelspecs-cache-get host))
              (let ((specs
                     (plist-get
                      (let ((json (or (jupyter-command "kernelspec" "list"
                                                       "--json" "--log-level" "ERROR")
                                      (error "\
Can't obtain kernelspecs from jupyter shell command"))))
                        (condition-case nil
                            (jupyter-read-plist-from-string json)
                          (error
                           (error "\
Jupyter kernelspecs couldn't be parsed from

    jupyter kernelspec list --json

To investiagate further, run that command in a shell and examine
why it isn't returning valid JSON."))))
                      :kernelspecs)))
                (jupyter-kernelspecs-cache-put
                 host
                 (sort
                  (cl-loop
                   for (kname spec) on specs by #'cddr
                   for name = (substring (symbol-name kname) 1)
                   for dir = (plist-get spec :resource_dir)
                   collect (make-jupyter-kernelspec
                            :name name
                            :resource-directory (concat
                                                 (unless (string= host "local") host)
                                                 dir)
                            :plist (plist-get spec :spec)))
                  (lambda (x y)
                    (string< (jupyter-kernelspec-name x)
                             (jupyter-kernelspec-name y)))))))))
    kernelspecs))

(cl-defgeneric jupyter-kernelspecs (host &optional refresh)
  "Return a list of kernelspecs on HOST.
If REFRESH is non-nil, then refresh the list of cached
kernelspecs first.  Otherwise a cached version of the kernelspecs
may be returned.")

(cl-defmethod jupyter-kernelspecs ((host string) &optional refresh)
  (let ((default-directory host))
    (jupyter-available-kernelspecs refresh)))

(cl-defmethod jupyter-do-refresh-kernelspecs ()
  (jupyter-kernelspecs default-directory 'refresh))

;;;###autoload
(defun jupyter-refresh-kernelspecs ()
  "Refresh the list of available kernelspecs.
Execute this command if the kernelspecs seen by Emacs is out of
sync with those specified on your system or notebook server."
  (interactive)
  (message "Refreshing kernelspecs...")
  (jupyter-do-refresh-kernelspecs)
  (message "Refreshing kernelspecs...done"))

(defun jupyter-get-kernelspec (name &optional specs refresh)
  "Get the kernelspec for a kernel named NAME.
If no kernelspec is found, return nil.  Otherwise return the
kernelspec for the kernel named NAME.

If SPECS is provided, it is a list of kernelspecs that will be
searched.  Otherwise the kernelspecs associated with the
`default-directory' are used.

Optional argument REFRESH has the same meaning as in
`jupyter-kernelspecs'."
  (cl-loop
   for kernelspec in (or specs (jupyter-kernelspecs default-directory refresh))
   thereis (when (string= (jupyter-kernelspec-name kernelspec) name)
             kernelspec)))

(defun jupyter-find-kernelspecs (re &optional specs refresh)
  "Find all specs of kernels that have names matching RE.
RE is a regular expression use to match the name of a kernel.
Return a list of `jupyter-kernelspec' objects.

If SPECS is non-nil search SPECS, otherwise search the
kernelspecs associated with the `default-directory'.

Optional argument REFRESH has the same meaning as in
`jupyter-kernelspecs'."
  (cl-remove-if-not
   (lambda (kernelspec)
     (string-match-p re (jupyter-kernelspec-name kernelspec)))
   (or specs (jupyter-kernelspecs default-directory refresh))))

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
if it is nil the kernelspecs associated with the
`default-directory' will be used.

Optional argument REFRESH has the same meaning as in
`jupyter-kernelspecs'."
  (let* ((specs (or specs (jupyter-kernelspecs default-directory refresh)))
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

(defun jupyter-expand-environment-variables (var)
  "Return VAR with all environment variables expanded.
VAR is a string, if VAR contains a sequence of characters like
${ENV_VAR}, substitute it with the value of ENV_VAR in
`process-environment'."
  (let ((expanded "")
        (start 0))
    (while (string-match "\\${\\([^}]+\\)}" var start)
      (cl-callf concat expanded
        (substring var start (match-beginning 0))
        (getenv (match-string 1 var)))
      (setq start (match-end 0)))
    (cl-callf concat expanded (substring var start))))

(defun jupyter-process-environment (kernelspec)
  "Return a list of environment variables contained in KERNELSPEC.
The list of environment variables have the same form as the
entries in `process-environment'.

The environment variables returned are constructed from those in
the :env key of KERNELSPEC's property list."
  (cl-loop
   with env = (plist-get (jupyter-kernelspec-plist kernelspec) :env)
   for (k v) on env by #'cddr
   collect (format "%s=%s" (cl-subseq (symbol-name k) 1)
                   (jupyter-expand-environment-variables v))))

(defun jupyter-kernel-argv (kernelspec conn-file)
  "Return a list of process arguments contained in KERNELSPEC.
The process arguments are the ones that should be passed to
kernel processes launched using KERNELSPEC.

CONN-FILE is the file name of a connection file, containing the
IP address and ports (among other things), a
launched kernel should connect to."
  (cl-loop
   with argv = (plist-get (jupyter-kernelspec-plist kernelspec) :argv)
   for arg in (append argv nil)
   if (equal arg "{connection_file}")
   collect (file-local-name conn-file)
   else if (equal arg "{resource_dir}")
   collect (file-local-name
            (jupyter-kernelspec-resource-directory
             kernelspec))
   else collect arg))

(provide 'jupyter-kernelspec)

;;; jupyter-kernelspec.el ends here
