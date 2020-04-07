;;; jupyter-env.el --- Query the jupyter shell command for information -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 27 Jun 2019

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

;; Custom variables and functions related to calling the jupyter shell command
;; and its sub-commands for information.

;;; Code:

(require 'jupyter-base)
(eval-when-compile (require 'subr-x))

(defvar jupyter-runtime-directory nil
  "The Jupyter runtime directory.
When a new kernel is started through `jupyter-start-kernel', this
directory is where kernel connection files are written to.

This variable should not be used.  To obtain the runtime directory
call the function `jupyter-runtime-directory'.")

(defun jupyter-command (&rest args)
  "Run a Jupyter shell command synchronously, return its output.
The shell command run is

    jupyter ARGS...

If the command fails or the jupyter shell command doesn't exist,
return nil."
  (with-temp-buffer
    (when (zerop (apply #'process-file "jupyter" nil t nil args))
      (string-trim-right (buffer-string)))))

(defun jupyter-runtime-directory ()
  "Return the runtime directory used by Jupyter.
Create the directory if necessary.  If `default-directory' is a
remote directory, return the runtime directory on that remote.

As a side effect, the variable `jupyter-runtime-directory' is set
to the local runtime directory if it is nil."
  (unless jupyter-runtime-directory
    (setq jupyter-runtime-directory
          (let ((default-directory (expand-file-name "~" user-emacs-directory)))
            (jupyter-command "--runtime-dir"))))
  (let ((dir (if (file-remote-p default-directory)
                 (jupyter-command "--runtime-dir")
               jupyter-runtime-directory)))
    (unless dir
      (error "Can't obtain runtime directory from jupyter shell command"))
    (prog1 (setq dir (concat (file-remote-p default-directory) dir))
      (make-directory dir 'parents))))

(defun jupyter-locate-python ()
  "Return the path to the python executable in use by Jupyter.
If the `default-directory' is a remote directory, search on that
remote.  Raise an error if the executable could not be found.

The paths examined are the data paths of \"jupyter --paths\" in
the order specified.

This function always returns the `file-local-name' of the path."
  (let* ((remote (file-remote-p default-directory))
         (paths (mapcar (lambda (x) (concat remote x))
                   (or (plist-get
                        (jupyter-read-plist-from-string
                         (jupyter-command "--paths" "--json"))
                        :data)
                       (error "Can't get search paths"))))
         (path nil))
    (cl-loop
     with programs = '("bin/python3" "bin/python"
                       ;; Need to also check Windows since paths can be
                       ;; pointing to local or remote files.
                       "python3.exe" "python.exe")
     with pred = (lambda (dir)
                   (cl-loop
                    for program in programs
                    for spath = (expand-file-name program dir)
                    thereis (setq path (and (file-exists-p spath) spath))))
     for path in paths
     thereis (locate-dominating-file path pred)
     finally (error "No `python' found in search paths"))
    (file-local-name path)))

(cl-defmethod jupyter-write-connection-file ((session jupyter-session) (obj jupyter-finalized-object))
  "Write a connection file based on SESSION to `jupyter-runtime-directory'.
Return the path to the connection file.

Also register a finalizer on OBJ to delete the file when OBJ is
garbage collected.  The file is also deleted when Emacs exits if
it hasn't been already."
  (let* ((temporary-file-directory (jupyter-runtime-directory))
         (json-encoding-pretty-print t)
         (file (make-temp-file "emacs-kernel-" nil ".json"))
         (kill-hook (lambda () (when (and file (file-exists-p file))
                            (delete-file file)))))
    (add-hook 'kill-emacs-hook kill-hook)
    (jupyter-add-finalizer obj
      (lambda ()
        (funcall kill-hook)
        (remove-hook 'kill-emacs-hook kill-hook)))
    (prog1 file
      (with-temp-file file
        (insert (json-encode-plist
                 (jupyter-session-conn-info session)))))))

(provide 'jupyter-env)

;;; jupyter-env.el ends here
