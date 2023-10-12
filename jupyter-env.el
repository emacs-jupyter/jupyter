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

(defcustom jupyter-executable "jupyter"
  "The `jupyter` command executable."
  :type 'string
  :group 'jupyter)

(defun jupyter-command (&rest args)
  "Run a Jupyter shell command synchronously, return its output.
The shell command run is

    jupyter ARGS...

If the command fails or the jupyter shell command doesn't exist,
return nil."
  (let ((stderr-file (make-temp-file "jupyter"))
        (stdout (get-buffer-create " *jupyter-command-stdout*")))
    (unwind-protect
        (let* ((status (apply #'process-file
                              jupyter-executable
                              nil
                              (list stdout stderr-file)
                              nil
                              args))
               (buffer (find-file-noselect stderr-file)))
          (unwind-protect
              (with-current-buffer buffer
                (unless (eq (point-min) (point-max))
                  (message "jupyter-command: Content written to stderr stream")
                  (while (not (eq (point) (point-max)))
                    (message "    %s" (buffer-substring (line-beginning-position)
                                                        (line-end-position)))
                    (forward-line))))
            (kill-buffer buffer))
          (when (zerop status)
            (with-current-buffer stdout
              (string-trim-right (buffer-string)))))
      (delete-file stderr-file)
      (kill-buffer stdout))))

(defun jupyter-runtime-directory ()
  "Return the runtime directory used by Jupyter.
Create the directory if necessary.  If `default-directory' is a
remote directory, return the runtime directory on that remote.

As a side effect, the variable `jupyter-runtime-directory' is set
to the local runtime directory if it is nil."
  (unless jupyter-runtime-directory
    (setq jupyter-runtime-directory
          (let ((default-directory (expand-file-name "~" user-emacs-directory)))
            (file-name-as-directory (jupyter-command "--runtime-dir")))))
  (let ((dir (if (file-remote-p default-directory)
                 (jupyter-command "--runtime-dir")
               jupyter-runtime-directory)))
    (unless dir
      (error "Can't obtain runtime directory from jupyter shell command"))
    (setq dir (concat (file-remote-p default-directory) dir))
    (make-directory dir 'parents)
    (file-name-as-directory dir)))

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

(defun jupyter-write-connection-file (session)
  "Write a connection file based on SESSION to `jupyter-runtime-directory'.
Return the path to the connection file."
  (cl-check-type session jupyter-session)
  (let* ((temporary-file-directory (jupyter-runtime-directory))
         (json-encoding-pretty-print t)
         (file (make-temp-file "emacs-kernel-" nil ".json")))
    (prog1 file
      (with-temp-file file
        (insert (json-encode-plist
                 (jupyter-session-conn-info session)))))))

(defun jupyter-session-with-random-ports ()
  "Return a `jupyter-session' with random channel ports.
The session can be used to write a connection file, see
`jupyter-write-connection-file'."
  ;; The actual work of making the connection file is left up to the
  ;; `jupyter kernel` shell command.  This is done to support
  ;; launching remote kernels via TRAMP.  The Jupyter suite of shell
  ;; commands probably exist on the remote system, so we rely on them
  ;; to figure out a set of open ports on the remote.
  (with-temp-buffer
    ;; NOTE: On Windows, apparently the "jupyter kernel" command uses something
    ;; like an exec shell command to start the process which launches the kernel,
    ;; but exec like commands on Windows start a new process instead of replacing
    ;; the current one which results in the process we start here exiting after
    ;; the new process is launched.  We call python directly to avoid this.
    (let ((process (start-file-process
                    "jupyter-session-with-random-ports" (current-buffer)
                    (jupyter-locate-python) "-c"
                    "from jupyter_client.kernelapp import main; main()")))
      (set-process-query-on-exit-flag process nil)
      (jupyter-with-timeout
          (nil jupyter-long-timeout
               (error "`jupyter kernel` failed to show connection file path"))
        (and (process-live-p process)
             (goto-char (point-min))
             (re-search-forward "Connection file: \\(.+\\)\n" nil t)))
      (let* ((conn-file (concat
                        (save-match-data
                          (file-remote-p default-directory))
                        (match-string 1)))
            (conn-info (jupyter-read-connection conn-file)))
        ;; Tell the `jupyter kernel` process to shutdown itself and
        ;; the launched kernel.
        (interrupt-process process)
        ;; Wait until the connection file is cleaned up before
        ;; forgetting about the process completely.
        (jupyter-with-timeout
            (nil jupyter-default-timeout
                 (delete-file conn-file))
          (file-exists-p conn-file))
        (delete-process process)
        (let ((new-key (jupyter-new-uuid)))
          (plist-put conn-info :key new-key)
          (jupyter-session
           :conn-info conn-info
           :key new-key))))))

(provide 'jupyter-env)

;;; jupyter-env.el ends here
