;;; jupyter-tramp.el --- TRAMP interface to the Jupyter REST API -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 25 May 2019

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

;; Integrate the Jupyter REST API contents endpoint with Emacs' file handling
;; facilities for remote files.  Adds two new remote file methods, /jpy: and
;; /jpys:, the former being HTTP connections and the latter being HTTPS
;; connections.
;;
;; If you run a local notebook server on port 8888 then reading and writing
;; files to the server is as easy as
;;
;;     (write-region "xxxx" nil "/jpy:localhost:happy.txt")
;;
;; or
;;
;;     (find-file "/jpy:localhost:serious.py")
;;
;; To open a `dired' listing to the base directory of the notebook server
;;
;;     (dired "/jpy:localhost:/")
;;
;; You can change the default port by changing the `tramp-default-port' entry
;; of the jpy or jpys method in `tramp-methods' or you can specify a port
;; inline using something like /jpy:localhost#8888:/.
;;
;; You can also set an entry in `tramp-default-host-alist' like
;;
;;     (add-to-list 'tramp-default-host-alist (list "jpy" nil "HOST"))
;;
;; Then specifying filenames like /jpy::/foo is equivalent to /jpy:HOST:
;;
;; TODO: Same messages for implemented file operations that TRAMP and Emacs
;; give.
;;
;; TODO: How can checkpoints be used with: `auto-save-mode',
;; `diff-latest-backup-file', ...

;;; Code:

(eval-when-compile
  (require 'subr-x)
  (require 'tramp-compat))
(require 'jupyter-rest-api)
(require 'jupyter-server)
(require 'tramp)
(require 'tramp-cache)

(defgroup jupyter-tramp nil
  "TRAMP integration with the Jupyter Contents REST API"
  :group 'jupyter)

(declare-function jupyter-decode-time "jupyter-messages" (str))

(defmacro jupyter-tramp-with-api-connection (file &rest body)
  "Set `jupyter-current-server' based on FILE, evaluate BODY.
FILE must be a remote file name recognized as corresponding to a
file on a server that can be communicated with using the Jupyter
notebook REST API.

Note, BODY is wrapped with a call to
`with-parsed-tramp-file-name' so that the variables method, user,
host, localname, ..., are all bound to values parsed from FILE."
  (declare (indent 1) (debug ([&or stringp symbolp] body)))
  `(with-parsed-tramp-file-name ,file nil
     ;; FIXME: There is a dilemma here, a `jupyter-server' is a more particular
     ;; object than what we need.  There is really no reason to have it here, we
     ;; just need a `jupyter-rest-client'.  Is there a reason this needs to be
     ;; here?
     (let ((jupyter-current-server
            (jupyter-tramp-server-from-file-name ,file)))
       ,@body)))

;;; File name handler setup

;; Actual functions implemented by `jupyter-tramp' all the others are either
;; ignored or handled by the TRAMP handlers.
;;
;; jupyter-tramp-copy-file
;; jupyter-tramp-delete-directory
;; jupyter-tramp-delete-file
;; jupyter-tramp-expand-file-name
;; jupyter-tramp-file-attributes
;; jupyter-tramp-file-directory-p
;; jupyter-tramp-file-local-copy
;; jupyter-tramp-file-name-all-completions
;; jupyter-tramp-file-remote-p
;; jupyter-tramp-file-symlink-p
;; jupyter-tramp-file-writable-p
;; jupyter-tramp-make-directory-internal
;; jupyter-tramp-rename-file
;; jupyter-tramp-write-region
;;;###autoload
(defconst jupyter-tramp-file-name-handler-alist
  '(;; `access-file' performed by default handler.
    (add-name-to-file . tramp-handle-add-name-to-file)
    ;; `byte-compiler-base-file-name' performed by default handler.
    ;; `copy-directory' performed by default handler.
    (copy-file . jupyter-tramp-copy-file)
    (delete-directory . jupyter-tramp-delete-directory)
    (delete-file . jupyter-tramp-delete-file)
    ;; TODO: Use the `checkpoint' file? I think we can only create a checkpoint
    ;; or restore a file from a checkpoint so maybe we can do something with
    ;; auto-save and checkpoints?
    ;; `diff-latest-backup-file' performed by default handler.
    (directory-file-name . tramp-handle-directory-file-name)
    (directory-files . tramp-handle-directory-files)
    (directory-files-and-attributes . tramp-handle-directory-files-and-attributes)
    (dired-compress-file . ignore)
    (dired-uncache . tramp-handle-dired-uncache)
    (expand-file-name . jupyter-tramp-expand-file-name)
    (file-accessible-directory-p . tramp-handle-file-accessible-directory-p)
    (file-acl . ignore)
    (file-attributes . jupyter-tramp-file-attributes)
    (file-directory-p . jupyter-tramp-file-directory-p)
    (file-equal-p . tramp-handle-file-equal-p)
    (file-executable-p . tramp-handle-file-exists-p)
    (file-exists-p . tramp-handle-file-exists-p)
    (file-in-directory-p . tramp-handle-file-in-directory-p)
    (file-local-copy . jupyter-tramp-file-local-copy)
    (file-modes . tramp-handle-file-modes)
    (file-name-all-completions . jupyter-tramp-file-name-all-completions)
    (file-name-as-directory . tramp-handle-file-name-as-directory)
    (file-name-case-insensitive-p . tramp-handle-file-name-case-insensitive-p)
    (file-name-completion . tramp-handle-file-name-completion)
    (file-name-directory . tramp-handle-file-name-directory)
    (file-name-nondirectory . tramp-handle-file-name-nondirectory)
    ;; `file-name-sans-versions' performed by default handler.
    (file-newer-than-file-p . tramp-handle-file-newer-than-file-p)
    (file-notify-add-watch . tramp-handle-file-notify-add-watch)
    (file-notify-rm-watch . tramp-handle-file-notify-rm-watch)
    (file-notify-valid-p . tramp-handle-file-notify-valid-p)
    (file-ownership-preserved-p . ignore)
    (file-readable-p . tramp-handle-file-exists-p)
    (file-regular-p . tramp-handle-file-regular-p)
    ;; NOTE: We can't use `tramp-handle-file-remote-p' since it expects a
    ;; process to check for the connected argument whereas we are using an HTTP
    ;; connection which may or may not be as long lived as something like an
    ;; SSH connection as the liveness depends on the Keep-Alive header of an
    ;; HTTP request.
    (file-remote-p . jupyter-tramp-file-remote-p)
    (file-selinux-context . tramp-handle-file-selinux-context)
    (file-symlink-p . jupyter-tramp-file-symlink-p)
    (file-system-info . ignore)
    (file-truename . tramp-handle-file-truename)
    (file-writable-p . jupyter-tramp-file-writable-p)
    ;; TODO: Can we do something here with checkpoints on the remote?
    (find-backup-file-name . ignore)
    ;; `find-file-noselect' performed by default handler.
    ;; `get-file-buffer' performed by default handler.
    (insert-directory . tramp-handle-insert-directory)
    ;; Uses `file-local-copy' to get the contents so be sure thats implemented
    (insert-file-contents . tramp-handle-insert-file-contents)
    (load . tramp-handle-load)
    (make-auto-save-file-name . tramp-handle-make-auto-save-file-name)
    ;; `make-directory' performed by default handler.
    (make-directory-internal . jupyter-tramp-make-directory-internal)
    (make-nearby-temp-file . tramp-handle-make-nearby-temp-file)
    (make-symbolic-link . tramp-handle-make-symbolic-link)
    ;; `process-file' performed by default handler.
    (rename-file . jupyter-tramp-rename-file)
    (set-file-acl . ignore)
    (set-file-modes . ignore)
    (set-file-selinux-context . ignore)
    (set-file-times . ignore)
    (set-visited-file-modtime . tramp-handle-set-visited-file-modtime)
    ;; `shell-command' performed by default handler.
    ;; `start-file-process' performed by default handler.
    (substitute-in-file-name . tramp-handle-substitute-in-file-name)
    (temporary-file-directory . tramp-handle-temporary-file-directory)
    ;; Important that we have this so that `call-process' and friends don't try
    ;; to set a Jupyter notebook directory as a directory in which a process
    ;; should run.
    (unhandled-file-name-directory . ignore)
    (vc-registered . ignore)
    (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime)
    (write-region . jupyter-tramp-write-region))
  "Alist of handler functions for Tramp Jupyter method.
Operations not mentioned here will be handled by the default Emacs primitives.")

;;;###autoload
(defconst jupyter-tramp-methods '("jpy" "jpys")
  "Methods to connect Jupyter kernel servers.")

;;;###autoload
(with-eval-after-load 'tramp
  (mapc (lambda (method)
       (add-to-list
        'tramp-methods
        (list method
              (list 'tramp-default-port 8888)
              (list 'tramp-tmpdir "/tmp"))))
     jupyter-tramp-methods)
  (tramp-register-foreign-file-name-handler
   'jupyter-tramp-file-name-p 'jupyter-tramp-file-name-handler)
  (add-to-list 'tramp-default-host-alist
               '("\\`jpys?\\'" nil "localhost")))

;;;###autoload
(defsubst jupyter-tramp-file-name-method-p (method)
  "Return METHOD if it corresponds to a Jupyter filename method or nil."
  (and (string-match-p "\\`jpys?\\'" method) method))

;; NOTE: Needs to be a `defsubst' to avoid recursive loading.
;;;###autoload
(defsubst jupyter-tramp-file-name-p (filename)
  "If FILENAME is a Jupyter filename, return its method otherwise nil."
  (when (tramp-tramp-file-p filename)
    (jupyter-tramp-file-name-method-p
     (tramp-file-name-method (tramp-dissect-file-name filename)))))

;;;###autoload
(defun jupyter-tramp-file-name-handler (operation &rest args)
  (let ((handler (assq operation jupyter-tramp-file-name-handler-alist)))
    (if (not handler)
        (tramp-run-real-handler operation args)
      (apply (cdr handler) args))))

;;;; Converting file names to authenticated `jupyter-rest-client' instances

(defvar tramp-current-method)
(defvar tramp-current-user)
(defvar tramp-current-domain)
(defvar tramp-current-host)
(defvar tramp-current-port)

(defun jupyter-tramp-read-passwd (filename &optional prompt)
  "Read a password based off of FILENAME's TRAMP filename components.
Use PROMPT to prompt the user for the password if needed, PROMPT
defaults to \"Password:\"."
  (unless (jupyter-tramp-file-name-p filename)
    (error "Not a Jupyter filename"))
  (with-parsed-tramp-file-name filename nil
    (let ((tramp-current-method method)
          (tramp-current-user (or user user-login-name))
          (tramp-current-domain nil)
          (tramp-current-host host)
          (tramp-current-port port))
      (tramp-read-passwd nil (or prompt "Password: ")))))

;;;###autoload
(defun jupyter-tramp-file-name-from-url (url)
  "Return a Jupyter TRAMP filename for the root directory of a kernel server.
The filename is based off of URL's host and port if any."
  (let ((url (if (url-p url) url
               (url-generic-parse-url url))))
    (format "/jpy%s:%s%s:/"
            (if (equal (url-type url) "https") "s" "")
            (url-host url)
            (let ((port (url-port-if-non-default url)))
              (if port (format "#%d" port) "")))))

;;;###autoload
(defun jupyter-tramp-url-from-file-name (filename)
  "Return a URL string based off the method, host, and port of FILENAME."
  (with-parsed-tramp-file-name filename nil
    (unless port (setq port (when (functionp 'tramp-file-name-port-or-default)
                              ;; This function was introduced in Emacs 26.1
                              (tramp-file-name-port-or-default v))))
    (format "%s://%s%s" (if (equal method "jpys") "https" "http")
            host (if port (format ":%s" port) ""))))

;;;###autoload
(defun jupyter-tramp-server-from-file-name (filename)
  "Return a `jupyter-server' instance based off of FILENAME's remote components.
If the connection has not been authenticated by the server,
attempt to authenticate the connection.  Raise an error if that
fails."
  (unless (jupyter-tramp-file-name-p filename)
    (error "Not a Jupyter filename"))
  (with-parsed-tramp-file-name filename nil
    (with-tramp-connection-property v "server"
      (let* ((url (jupyter-tramp-url-from-file-name filename))
             (client (or (jupyter-find-server url)
                         (jupyter-server :url url))))
        (prog1 client
          (unless (jupyter-api-server-accessible-p client)
            (cond
             ((y-or-n-p (format "Login to %s using a token? " url))
              (jupyter-api-authenticate client 'token))
             (t
              ;; This is here so that reading a password using
              ;; `tramp-read-passwd' via `jupyter-api-read-passwd' will check
              ;; auth sources.
              (tramp-set-connection-property v "first-password-request" t)
              (jupyter-api-authenticate client
                'password
                (let ((remote (file-remote-p filename)))
                  (lambda (try)
                    (jupyter-tramp-read-passwd
                     filename (unless (zerop try)
                                (format "Password for %s " remote))))))))))))))

;;; Getting information about file models

(defalias 'jupyter-tramp-flush-file-properties
  (if (functionp 'tramp-flush-file-properties)
      ;; New in Emacs 27
      'tramp-flush-file-properties
    'tramp-flush-file-property))

(defun jupyter-tramp--get-directory-or-file-model (file localname path no-content)
  (cond
   (no-content
    (jupyter-tramp-get-file-model (file-name-directory file)))
   (t
    (condition-case err
        ;; Unset `signal-hook-function' so that TRAMP in Emacs >= 27 does not
        ;; mess with the signal data until we have a chance to look at it.
        (let (signal-hook-function)
          (jupyter-api-get-file-model jupyter-current-server localname))
      (jupyter-api-http-error
       (cl-destructuring-bind (_ code msg) err
         (if (and (eq code 404)
                  (string-match-p "No such file or directory" msg))
             (list :path path :name nil
                   ;; If a file doesn't exist we need to check if the
                   ;; containing directory is writable to determine if
                   ;; FILE is.
                   :writable (plist-get
                              (jupyter-tramp-get-file-model
                               (file-name-directory
                                (directory-file-name file))
                               'no-content)
                              :writable))
           (signal (car err) (cdr err)))))
      (error (signal (car err) (cdr err)))))))

(defun jupyter-tramp--get-file-model (file localname no-content)
  (let* ((path (jupyter-api-content-path localname))
         (model (jupyter-tramp--get-directory-or-file-model
                 file localname path no-content)))
    (or (jupyter-api-find-model path model)
        ;; We reach here when MODEL is a directory that does
        ;; not contain PATH.  PATH is writable if the
        ;; directory is.
        (list :path path :name nil
              :writable (plist-get model :writable)))))

(defun jupyter-tramp-get-file-model (file &optional no-content)
  "Return a model of FILE or raise an error.
For non-existent files the model

    (:path PATH :name nil :writable WRITABLE)

is returned, where PATH is a local path name to FILE on the
server, i.e. excludes the remote part of FILE.  WRITABLE will be t
if FILE can be created on the server or nil if PATH is outside
the base directory the server was started in.

When NO-CONTENT is non-nil, return a model for file that excludes
:content if an actual request needs to be made.  The :content key
may or may not be present in this case.  If NO-CONTENT is nil,
guarantee that we request FILE's content as well.

See `jupyter-tramp-get-file-model' for details on what a file model is."
  (setq file (expand-file-name file))
  (jupyter-tramp-with-api-connection file
    (let ((value (or (tramp-get-file-property v localname "model" nil)
                     (when no-content
                       (tramp-get-file-property v localname "nc-model" nil)))))
      (unless value
        (setq value (jupyter-tramp--get-file-model file localname no-content))
        (tramp-set-file-property
         v localname (if no-content "nc-model" "model") value))
      value)))

(defun jupyter-tramp-flush-file-and-directory-properties (filename)
  (with-parsed-tramp-file-name filename nil
    (jupyter-tramp-flush-file-properties v localname)
    (jupyter-tramp-flush-file-properties v (file-name-directory localname))))

;;; Predicates

(defun jupyter-tramp--barf-if-not-file (file)
  (unless (file-exists-p file)
    (error "No such file or directory: %s" file)))

(defun jupyter-tramp--barf-if-not-regular-file (file)
  (jupyter-tramp--barf-if-not-file file)
  (unless (file-regular-p file)
    (error "Not a file: %s" file)))

(defun jupyter-tramp--barf-if-not-directory (directory)
  (jupyter-tramp--barf-if-not-file directory)
  (unless (file-directory-p directory)
    (error "Not a directory: %s" (expand-file-name directory))))

(defun jupyter-tramp-file-writable-p (filename)
  (jupyter-tramp-with-api-connection filename
    (plist-get (jupyter-tramp-get-file-model filename 'no-content) :writable)))

;; Actually this may not be true, but there is no way to tell if a file is a
;; symlink or not
(defun jupyter-tramp-file-symlink-p (_filename)
  nil)

(defun jupyter-tramp-file-directory-p (filename)
  (jupyter-tramp-with-api-connection filename
    (equal (plist-get (jupyter-tramp-get-file-model filename 'no-content) :type)
           "directory")))

(defvar url-http-open-connections)

(defun jupyter-tramp-file-remote-p (file &optional identification connected)
  (when (file-name-absolute-p file)
    (with-parsed-tramp-file-name file nil
      (when (or (null connected)
                (let* ((port (or port (tramp-file-name-port-or-default v)))
                       (key (cons host (if (numberp port) port
                                         (string-to-number port)))))
                  (catch 'connected
                    (dolist (conn (gethash key url-http-open-connections))
                      (when (memq (process-status conn) '(run open connect))
                        (throw 'connected t))))))
        (cl-case identification
          (method method)
          (host host)
          (user user)
          (localname localname)
          (t (tramp-make-tramp-file-name
              method user domain host port "")))))))

;;; File name manipulation

(defun jupyter-tramp-expand-file-name (name &optional directory)
  ;; From `tramp-sh-handle-expand-file-name'
  (setq directory (or directory default-directory "/"))
  (unless (file-name-absolute-p name)
    (setq name (concat (file-name-as-directory directory) name)))
  (if (tramp-tramp-file-p name)
      (let ((v (tramp-dissect-file-name name)))
        (if (jupyter-tramp-file-name-method-p (tramp-file-name-method v))
            (tramp-make-tramp-file-name
             (tramp-file-name-method v)
             (tramp-file-name-user v)
             (tramp-file-name-domain v)
             (tramp-file-name-host v)
             (tramp-file-name-port v)
             (tramp-drop-volume-letter
              (tramp-run-real-handler
               'expand-file-name (list (tramp-file-name-localname v) "/")))
             (tramp-file-name-hop v))
          (let ((tramp-foreign-file-name-handler-alist
                 (remove (cons 'jupyter-tramp-file-name-p
                               'jupyter-tramp-file-name-handler)
                         tramp-foreign-file-name-handler-alist)))
            (expand-file-name name))))
    (tramp-run-real-handler 'expand-file-name (list name directory))))

;;; File operations

;; Adapted from `tramp-smb-handle-rename-file'
(defun jupyter-tramp-rename-file (filename newname &optional ok-if-already-exists)
  (setq filename (expand-file-name filename)
        newname (expand-file-name newname))

  (when (and (not ok-if-already-exists)
             (file-exists-p newname))
    (tramp-error
     (tramp-dissect-file-name
      (if (tramp-tramp-file-p filename) filename newname))
     'file-already-exists newname))

  (with-tramp-progress-reporter
      (tramp-dissect-file-name
       (if (tramp-tramp-file-p filename) filename newname))
      0 (format "Renaming %s to %s" filename newname)

    (if (and (not (file-exists-p newname))
             (tramp-equal-remote filename newname))
        ;; We can rename directly.
        (jupyter-tramp-with-api-connection filename
          ;; We must also flush the cache of the directory, because
          ;; `file-attributes' reads the values from there.
          (jupyter-tramp-flush-file-and-directory-properties filename)
          (jupyter-tramp-flush-file-and-directory-properties newname)
          (jupyter-api-rename-file jupyter-current-server
            filename newname))

      ;; We must rename via copy.
      (copy-file filename newname ok-if-already-exists)
      (if (file-directory-p filename)
          (delete-directory filename 'recursive)
        (delete-file filename)))))

;; NOTE: Deleting to trash is configured on the server.
(defun jupyter-tramp-delete-directory (directory &optional recursive _trash)
  (jupyter-tramp--barf-if-not-directory directory)
  (jupyter-tramp-with-api-connection directory
    (jupyter-tramp-flush-file-properties v localname)
    (let ((files (cl-remove-if
                  (lambda (x) (member x '("." "..")))
                  (directory-files directory nil nil t))))
      (unless (or recursive (not files))
        (error "Directory %s not empty" directory))
      (let ((deleted
             ;; Try to delete the directory, if we get an error because its not
             ;; empty, manually delete all files below and then try again.
             (condition-case err
                 (prog1 t
                   ;; Unset `signal-hook-function' so that TRAMP in Emacs >= 27
                   ;; does not mess with the signal data until we have a chance
                   ;; to look at it.
                   (let (signal-hook-function)
                     (jupyter-api-delete-file
                         jupyter-current-server
                       directory)))
               (jupyter-api-http-error
                (unless (and (= (nth 1 err) 400)
                             (string-match-p "not empty" (caddr err)))
                  (signal (car err) (cdr err))))
               (error (signal (car err) (cdr err))))))
        (unless deleted
          ;; Recursive delete, we need to do this manually since we can get a 400
          ;; error on Windows when deleting to trash and also in general when not
          ;; deleting to trash if the directory isn't empty, see
          ;; jupyter/notebook/notebook/services/contents/filemanager.py
          (while files
            (let ((file (expand-file-name (pop files) directory)))
              (if (file-directory-p file)
                  (delete-directory file recursive)
                (delete-file file))))
          (jupyter-api-delete-file jupyter-current-server directory))))
    ;; Need to uncache both the file and its directory
    (jupyter-tramp-flush-file-and-directory-properties directory)))

(defun jupyter-tramp-delete-file (filename &optional _trash)
  (jupyter-tramp--barf-if-not-regular-file filename)
  (jupyter-tramp-with-api-connection filename
    (jupyter-api-delete-file jupyter-current-server filename)
    ;; Need to uncache both the file and its directory
    (jupyter-tramp-flush-file-and-directory-properties filename)))

;; Adapted from `tramp-smb-handle-copy-file'
(defun jupyter-tramp-copy-file (filename newname &optional ok-if-already-exists
                                     keep-date _preserve-uid-gid _preserve-permissions)
  (setq filename (expand-file-name filename)
        newname (expand-file-name newname))
  (with-tramp-progress-reporter
      (tramp-dissect-file-name
       (if (tramp-tramp-file-p filename) filename newname))
      0 (format "Copying %s to %s" filename newname)

    (if (file-directory-p filename)
        (copy-directory filename newname keep-date 'parents 'copy-contents)

      (cond
       ((tramp-equal-remote filename newname)
        (jupyter-tramp-with-api-connection newname
          (when (and (not ok-if-already-exists)
                     (file-exists-p newname))
            (tramp-error v 'file-already-exists newname))
          (jupyter-api-copy-file jupyter-current-server filename newname)))
       (t
        (let ((tmpfile (file-local-copy filename)))
          (if tmpfile
              ;; Remote filename.
              (condition-case err
                  (rename-file tmpfile newname ok-if-already-exists)
                ((error quit)
                 (delete-file tmpfile)
                 (signal (car err) (cdr err))))

            ;; Remote newname.
            (when (and (file-directory-p newname)
                       (directory-name-p newname))
              (setq newname
                    (expand-file-name (file-name-nondirectory filename) newname)))

            (with-parsed-tramp-file-name newname nil
              (when (and (not ok-if-already-exists)
                         (file-exists-p newname))
                (tramp-error v 'file-already-exists newname))

              (with-temp-file newname
                (insert-file-contents-literally filename)))))))

      (when (tramp-tramp-file-p newname)
        ;; We must also flush the cache of the directory, because
        ;; `file-attributes' reads the values from there.
        (jupyter-tramp-flush-file-and-directory-properties newname)))))

(defun jupyter-tramp-make-directory-internal (dir)
  (jupyter-tramp-with-api-connection dir
    (jupyter-api-make-directory jupyter-current-server dir)
    (jupyter-tramp-flush-file-and-directory-properties dir)))

;;; File name completion

(defun jupyter-tramp-file-name-all-completions (filename directory)
  (when (jupyter-tramp-file-name-p directory)
    (all-completions
     filename (mapcar #'car (jupyter-tramp-directory-file-models directory))
     (lambda (f)
       (let ((ext (file-name-extension f t)))
         (and (or (null ext) (not (member ext completion-ignored-extensions)))
              (or (null completion-regexp-list)
                  (not (cl-loop for re in completion-regexp-list
                                thereis (not (string-match-p re f)))))))))))

;;; Insert file contents

;; XXX: WIP
(defun jupyter-tramp--recover-this-file (orig)
  "If the `current-buffer' is Jupyter file, revert back to a checkpoint.
If no checkpoints exist, revert back to the file that exists on
the server.  For any other file, call ORIG, which is the function
`recover-this-file'"
  (interactive)
  (let ((file (buffer-file-name)))
    (if (not (jupyter-tramp-file-name-p file)) (funcall orig)
      (jupyter-tramp-with-api-connection file
        (let ((checkpoint (jupyter-api-get-latest-checkpoint
                              jupyter-current-server
                            file)))
          (when checkpoint
            (jupyter-api-restore-checkpoint
                jupyter-current-server
              file checkpoint))
          (let ((tmpfile (file-local-copy file)))
            (unwind-protect
                (save-restriction
                  (widen)
                  (insert-file-contents tmpfile nil nil nil 'replace)
                  ;; TODO: What else needs to be done here
                  (set-buffer-modified-p nil))
              (delete-file tmpfile))))))))

;; TODO: Something that doesn't use advise
;; (advice-add 'recover-this-file :around 'jupyter-tramp--recover-this-file)

;; TODO: What to do about reading and writing large files?  Also the out of
;; band functions of TRAMP.
;;
;; Adapted from `tramp-sh-handle-write-region'
(defun jupyter-tramp-write-region (start end filename &optional append visit lockname mustbenew)
  (setq filename (expand-file-name filename))
  (when (and mustbenew (file-exists-p filename)
             (or (eq mustbenew 'excl)
                 (not
                  (y-or-n-p
                   (format "File %s exists; overwrite anyway? " filename)))))
    (signal 'file-already-exists (list filename)))
  (jupyter-tramp-with-api-connection filename
    ;; Ensure we don't use stale model contents
    (jupyter-tramp-flush-file-and-directory-properties filename)
    (if (and append (file-exists-p filename))
        (let* ((tmpfile (file-local-copy filename))
               (model (jupyter-tramp-get-file-model filename))
               (binary (jupyter-api-binary-content-p model))
               (coding-system-for-read (if binary 'no-conversion 'utf-8))
               (coding-system-for-write (if binary 'no-conversion 'utf-8)))
          (condition-case err
              (tramp-run-real-handler
               'write-region
               (list start end tmpfile append 'no-message lockname mustbenew))
            (error
             (delete-file tmpfile)
             (signal (car err) (cdr err))))
          (unwind-protect
              (with-temp-buffer
                (insert-file-contents-literally tmpfile)
                (jupyter-api-write-file-content
                    jupyter-current-server
                  filename (buffer-string) binary))
            (delete-file tmpfile)))
      (let ((source (if (stringp start) start
                      (if (null start) (buffer-string)
                        (buffer-substring-no-properties start end))))
            (binary (coding-system-equal
                     (or coding-system-for-write
                         (if enable-multibyte-characters 'utf-8
                           'binary))
                     'binary)))
        (jupyter-api-write-file-content
            jupyter-current-server
          filename source binary)
        ;; Adapted from `tramp-sh-handle-write-region'
        (when (or (eq visit t) (stringp visit))
          (let ((file-attr (file-attributes filename)))
            (when (stringp visit)
              (setq buffer-file-name visit))
            (set-buffer-modified-p nil)
            (set-visited-file-modtime
             ;; We must pass modtime explicitly, because FILENAME can
             ;; be different from (buffer-file-name), f.e. if
             ;; `file-precious-flag' is set.
             (tramp-compat-file-attribute-modification-time file-attr))))
        (when (and (null noninteractive)
                   (or (eq visit t) (null visit) (stringp visit)))
          (tramp-message v 0 "Wrote %s" filename))))
    ;; Another flush after writing for consistency
    ;; TODO: Figure out more exactly where these should go
    (jupyter-tramp-flush-file-and-directory-properties filename)))

;; TODO: Set `jupyter-current-server' in every buffer that visits a file, this
;; way `jupyter-current-server' will always use the right server for file
;; operations if there happen to be more than one server.
;;
;; NOTE: Not currently used since `file-local-copy' is used as a way to get
;; files from the server and then `write-region' is used to write them back.
(defun jupyter-tramp-insert-file-contents (filename &optional visit beg end replace)
  (setq filename (expand-file-name filename))
  (let ((do-visit
         (lambda ()
           (setq buffer-file-name filename)
           (set-buffer-modified-p nil))))
    (condition-case err
        (jupyter-tramp--barf-if-not-file filename)
      (error
       (and visit (funcall do-visit))
       (signal (car err) (cdr err))))
    (jupyter-tramp-with-api-connection filename
      ;; Ensure we grab a fresh model since the cached version may be out of
      ;; sync with the server.
      (jupyter-tramp-flush-file-properties v localname)
      (let ((model (jupyter-tramp-get-file-model filename)))
        (when (and visit (jupyter-api-binary-content-p model))
          (set-buffer-multibyte nil))
        (let ((pos (point)))
          (jupyter-api-insert-model-content model replace beg end)
          (and visit (funcall do-visit))
          (list filename (- (point) pos)))))))

(defun jupyter-tramp-file-local-copy (filename)
  (jupyter-tramp-with-api-connection filename
    (unless (file-exists-p filename)
      (tramp-error
       v tramp-file-missing
       "Cannot make local copy of non-existing file `%s'" filename))
    ;; Ensure we grab a fresh model since the cached version may be out of
    ;; sync with the server.
    (jupyter-tramp-flush-file-properties v localname)
    (let ((model (jupyter-tramp-get-file-model filename)))
      (when (jupyter-api-notebook-p model)
        (error "Notebooks not supported yet"))
      (let ((coding-system-for-write
             (if (jupyter-api-binary-content-p model)
                 'no-conversion
               'utf-8-auto)))
        (tramp-run-real-handler
         'make-temp-file
         (list "jupyter-tramp." nil (file-name-extension filename t)
               (with-current-buffer (jupyter-api-content-buffer model)
                 (buffer-string))))))))

;;; File/directory attributes

(defun jupyter-tramp-file-attributes-from-model (model &optional id-format)
  ;; :name is nil if the corresponding file of MODEL doesn't exist, see
  ;; `jupyter-tramp-get-file-model'.
  (when (plist-get model :name)
    (let* ((dirp (equal (plist-get model :type) "directory"))
           (last-modified (plist-get model :last_modified))
           (created (plist-get model :created))
           (mtime (or (and last-modified (jupyter-decode-time last-modified))
                      (current-time)))
           (ctime (or (and created (jupyter-decode-time created))
                      (current-time)))
           ;; Sometimes the model doesn't contain a size
           (size (or (plist-get model :size) 64))
           ;; FIXME: What to use for these two?
           (ugid (if (eq id-format 'string) "jupyter" 100))
           (mbits (format "%sr%s%s-------"
                          (if dirp "d" "-")
                          (if (plist-get model :writable) "w" "")
                          (if dirp "x" ""))))
      (list dirp 1 user-login-name ugid
            mtime mtime ctime size mbits nil -1 -1))))

(defun jupyter-tramp-file-attributes (filename &optional id-format)
  (jupyter-tramp-file-attributes-from-model
   (jupyter-tramp-with-api-connection filename
     (jupyter-tramp-get-file-model filename 'no-content))
   id-format))

(defun jupyter-tramp-directory-file-models (directory &optional full match)
  "Return the files contained in DIRECTORY as Jupyter file models.
The returned files have the form (PATH . MODEL) where PATH is
relative to DIRECTORY unless FULL is non-nil.  In that case PATH
is an absolute file name.  PATH will have an ending / character if
MODEL corresponds to a directory.

If MATCH is non-nil, it should be a regular expression.  Only
return files that match it.

If DIRECTORY does not correspond to a directory on the server,
return nil."
  (when (file-directory-p directory)
    (jupyter-tramp-with-api-connection directory
      (let ((dir-model (jupyter-tramp-get-file-model directory)))
        (cl-loop
         for model across (plist-get dir-model :content)
         for dirp = (equal (plist-get model :type) "directory")
         for name = (concat (plist-get model :name) (and dirp "/"))
         for path = (if full (expand-file-name name directory) name)
         if match when (string-match-p match name)
         collect (cons path model) into files end
         else collect (cons path model) into files
         finally return
         (let ((pdir-model (jupyter-tramp-get-file-model
                            (file-name-directory
                             (directory-file-name directory)))))
           (dolist (d (list (cons "../" pdir-model)
                            (cons "./" dir-model)))
             (when (or (null match)
                       (string-match-p match (car d)))
               (when full
                 (setcar d (expand-file-name (car d) directory)))
               (push d files)))
           files))))))

(defun jupyter-tramp-directory-files-and-attributes
    (directory &optional full match nosort id-format)
  (jupyter-tramp--barf-if-not-directory directory)
  (let ((files
         (cl-loop
          for (file . model)
          in (jupyter-tramp-directory-file-models directory full match)
          for attrs = (jupyter-tramp-file-attributes-from-model model id-format)
          collect (cons file attrs))))
    (if nosort files
      (sort files (lambda (a b) (string-lessp (car a) (car b)))))))

(provide 'jupyter-tramp)

;;; jupyter-tramp.el ends here
