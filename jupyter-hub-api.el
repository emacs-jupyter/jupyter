;;; jupyter-hub-api.el --- Jupyter Hub REST API -*- lexical-binding: t -*-

;; Copyright (C) 2019 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 21 Jun 2019
;; Version: 0.8.1

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

;; Routines for working with the Jupyter Hub REST API. Supports JupyterHub v1.0
;; and above.

;;; Code:

(require 'jupyter-server)
(eval-when-compile (require 'subr-x))

;; TODO: Set this as a client local variable as well using `jupyter-set' if
;; needed.
(defvar-local jupyter-hub-current-user nil
  "The current logged in JupyterHub user.")
(put 'jupyter-hub-current-user 'permanent-local t)

(defclass jupyter-hub-client (jupyter-rest-client)
  ((auth :initform 'token)
   (authenticator
    :initform 'jupyter-hub-pam-authenticator
    :documentation "Authentication method for logging in users.
Must be a function that takes three arguments, (HUB USER),
where USER is the username of the user to login on HUB.")))

;; TODO: The Hub should be logged in differently than this

;; NOTE: Going through the login page installs a login cookie for the user
;; whose credentials were passed to the HUB. The cookie is the means of
;; authentication and not the token.
;;
;; The PAM authenticator is more of a user authenticator rather than a HUB
;; authenticator. The HUB API is mainly accessible through a token. Logging in
;; mainly stores a login cookie and has the HUB handle the token stuff via the
;; login cookie.

;; FIXME: Have a better way to handle this case
(defvar jupyter-hub--current-user-logging-in nil)

(cl-defmethod jupyter-api-server-accessible-p ((hub jupyter-hub-client))
  (if jupyter-hub--current-user-logging-in
      (and (jupyter-hub-service-available-p
            hub "user" jupyter-hub--current-user-logging-in)
           (jupyter-hub-user-logged-in-p hub jupyter-hub--current-user-logging-in))
    (let ((url (if (url-p (oref hub url)) (oref hub url)
                 (url-generic-parse-url (oref hub url)))))
      (and (jupyter-api-auth-headers hub)
           (cl-loop
            for cookie in (url-cookie-retrieve (url-host url) "/hub/")
            thereis (equal (url-cookie-name cookie) "jupyterhub-hub-login"))))))

(defun jupyter-hub-pam-authenticator (hub user)
  "Authenticate USER using PAM authentication via HUB.
A username and password will be asked for.

Raise an error on failure."
  (let ((jupyter-hub--current-user-logging-in user))
    (jupyter-api-authenticate hub
      (lambda (hub _try)
        ;; TODO: Handle multiple users and :save-function
        (let ((matches (jupyter-api-auth-source-search hub user)) auth)
          (setq auth (if (= (length matches) 1) (car matches)
                       (error "TODO")))
          (cond
           (auth
            (let* ((user (plist-get auth :user))
                   (pass (if (functionp (plist-get auth :secret))
                             (funcall (plist-get auth :secret))
                           (plist-get auth :secret)))
                   (url-request-method "POST")
                   (url-request-extra-headers
                    (append (list (cons "Content-Type" "application/x-www-form-urlencoded"))
                            url-request-extra-headers))
                   (url-request-data
                    (concat "password=" pass "&"
                            "username=" user)))
              (condition-case err
                  (jupyter-api-login hub)
                (error
                 (when (eq (nth 2 err) 'connection-failed)
                   (signal (car err) (cdr err)))))))
           (t
            (error "Can't authenticate"))))))))

;;; Endpoints

(cl-defgeneric jupyter-hub/api/authorizations ((client jupyter-hub-client) method &rest plist)
  (declare (indent 2)))

(cl-defmethod jupyter-hub/api/authorizations ((client jupyter-hub-client) method &rest plist)
  (apply #'jupyter-api-request client method "hub" "api" "authorizations" plist))

(cl-defgeneric jupyter-hub/api/users ((client jupyter-hub-client) method &rest plist)
  (declare (indent 2)))

(cl-defmethod jupyter-hub/api/users ((client jupyter-hub-client) method &rest plist)
  (apply #'jupyter-api-request client method "hub" "api" "users" plist))

(cl-defgeneric jupyter-hub/api/groups ((client jupyter-hub-client) method &rest plist)
  (declare (indent 2)))

(cl-defmethod jupyter-hub/api/groups ((client jupyter-hub-client) method &rest plist)
  (apply #'jupyter-api-request client method "hub" "api" "groups" plist))

;;; Users

(defun jupyter-hub-current-user (hub)
  (jupyter-api-request hub "GET" "hub" "user"))

(defun jupyter-hub-get-user (hub &optional user)
  (jupyter-hub/api/users hub "GET" user))

(defun jupyter-hub-make-user (hub user)
  (jupyter-hub/api/users hub "POST" user))

(defun jupyter-hub-modify-user (hub user &rest options)
  (apply #'jupyter-hub/api/users hub "PATCH" user options))

(defun jupyter-hub-delete-user (hub user)
  (jupyter-hub/api/users hub "DELETE" user))

;;;; Tokens

(defun jupyter-hub-get-token (hub user &optional id)
  (jupyter-hub/api/users hub "GET" user "tokens" id))

(cl-defun jupyter-hub-make-token (hub user &key auth expires-in note)
  (declare (indent 2))
  (apply #'jupyter-hub/api/users hub "POST" user "tokens"
         (nconc (when auth (list :auth auth))
                (when expires-in (list :expires_in expires-in))
                (when note (list :note note)))))

(defun jupyter-hub-delete-token (hub user id)
  (jupyter-hub/api/users hub "DELETE" user "tokens" id))

;;;;;  `jupyter-hub-token-list-mode'

(defvar jupyter-hub-token-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") #'jupyter-hub-token-list-revoke)
    (define-key map (kbd "C-c C-d") #'jupyter-hub-token-list-revoke)
    (define-key map (kbd "C-<return>") #'jupyter-hub-token-list-create)
    (define-key map "r" #'revert-buffer)
    (define-key map "g" #'revert-buffer)
    map))

(defun jupyter-hub-token-list-revoke ()
  "Revoke the token under `point'."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (jupyter-hub-delete-token jupyter-current-server jupyter-hub-current-user id)
    (tabulated-list-delete-entry)))

(defun jupyter-hub-token-list-create ()
  "Create a new Jupyter Hub API token.
The value of the new token is added to the `kill-ring' and also
inserted into the *Messages* buffer."
  (interactive)
  (let* ((note (read-string "Token note: "))
         (expires-in (read-number "Expires in seconds [<= 0 means never]: " 0))
         (token (jupyter-hub-make-token
                    jupyter-current-server jupyter-hub-current-user
                  :expires-in (unless (<= expires-in 0) expires-in)
                  :note note)))
    (message (kill-new (plist-get token :token)))
    (revert-buffer)
    (goto-char (point-min))
    (while (not (equal (tabulated-list-get-id)
                       (plist-get token :id)))
      (forward-line 1))))

(define-derived-mode jupyter-hub-token-list-mode
  tabulated-list-mode "Tokens"
  "A list of tokens for a user."
  (tabulated-list-init-header)
  (tabulated-list-print)
  (let* ((inhibit-read-only t)
         (url (oref jupyter-current-server url))
         (header (format "%s [%s]" jupyter-hub-current-user url)))
    (overlay-put
     (make-overlay 1 2)
     'before-string
     (concat (propertize header 'face '(fixed-pitch default)) "\n"))))

(defun jupyter-hub--token-list-format ()
  `[("Kind" 10 nil)
    ("ID" 4 nil)
    ("Note" 35 t)
    ("Last Used" 20 t)
    ("Created/First authorized" 25 nil)])

(defun jupyter-hub--token-list-entries (user)
  (let* ((tokens (jupyter-hub-get-token jupyter-current-server user))
         (format-time
          (lambda (time)
            (propertize
             (jupyter-format-time-low-res
              (when time (jupyter-decode-time time)))
             'face 'font-lock-doc-face)))
         (format
          (lambda (token)
            (cl-destructuring-bind (&key id kind created expires_at last_activity
                                         note oauth_client
                                         &allow-other-keys)
                token
              (setq expires_at (funcall format-time expires_at))
              (setq last_activity (funcall format-time last_activity))
              (setq created (funcall format-time created))
              (setq note (or note oauth_client))
              (list id (vector kind id note last_activity created))))))
    (cl-loop
     for token across (vconcat (plist-get tokens :api_tokens)
                               (plist-get tokens :oauth_tokens))
     collect (funcall format token))))

;;;###autoload
(defun jupyter-hub-list-tokens (hub user)
  "Display a list of tokens for USER on HUB."
  (interactive
   (let ((hub (jupyter-current-server current-prefix-arg)))
     (list hub (read-string
                (format "Username [%s] (default %s): "
                        (oref hub url)
                        user-login-name)))))
  (with-current-buffer
      (jupyter-get-buffer-create
       (format "tokens:%s[%s]"
               user (oref hub url)))
    (setq jupyter-current-server hub)
    (setq jupyter-hub-current-user user)
    (if (eq major-mode 'jupyter-hub-token-list-mode)
        (revert-buffer)
      (setq tabulated-list-format
            (jupyter-hub--token-list-format)
            tabulated-list-entries
            (apply-partially #'jupyter-hub--token-list-entries user))
      (jupyter-hub-token-list-mode))
    (jupyter-display-current-buffer-reuse-window)))

;;;; Server

(defun jupyter-hub-start-server (hub user &rest options)
  (declare (indent 2))
  (apply #'jupyter-hub/api/users hub "POST" user "server" options))

(defun jupyter-hub-delete-server (hub user)
  (jupyter-hub/api/users hub "DELETE" user "server"))

;;; Authorizations

(defun jupyter-hub-user-from-cookie (client name value)
  (jupyter-hub/api/authorizations client "GET" "cookie" name value))

(defun jupyter-hub-user-from-token (client token)
  (jupyter-hub/api/authorizations client "GET" "token" token))

;;; Groups

(defun jupyter-hub-get-group (client &optional group)
  (jupyter-hub/api/groups client "GET" group))

(defun jupyter-hub-delete-group (client group)
  (jupyter-hub/api/groups client "DELETE" group))

(defun jupyter-hub-make-group (client group)
  (jupyter-hub/api/groups client "POST" group))

(defun jupyter-hub-delete-group-users (client group &rest users)
  (declare (indent 2))
  (jupyter-hub/api/groups client "DELETE" group "users" :users users))

(defun jupyter-hub-add-group-users (client group &rest users)
  (declare (indent 2))
  (jupyter-hub/api/groups client "POST" group "users" :users users))

;;; Info

(defun jupyter-hub-info (client)
  (jupyter-api-request client "GET" "hub" "info"))

;;; Client

;; TODO: Determine when a cookie is invalid.
(cl-defmethod jupyter-hub-user-logged-in-p ((hub jupyter-hub-client) user)
  (and (jupyter-api-server-accessible-p hub)
       (let ((cookie (format "jupyterhub-user-%s" user))
             (localpart (format "/user/%s/" user))
             (url (url-generic-parse-url (oref hub url))))
         (url-cookie-value
          (cl-find-if (lambda (c) (equal (url-cookie-name c) cookie))
                      (url-cookie-retrieve
                       (url-host url) localpart
                       (equal (url-type url) "https")))))
       t))

(cl-defmethod jupyter-hub-make-client ((hub jupyter-hub-client) user &optional class)
  (or class (setq class 'jupyter-rest-client))
  (jupyter-error-if-not-client-class-p class 'jupyter-rest-client)
  (let ((url (url-generic-parse-url (oref hub url))))
    (setf (url-filename url) (concat "/user/" user))
    (let ((client (make-instance class :url (url-recreate-url url) :auth t)))
      (prog1 client
        (unless (jupyter-hub-user-logged-in-p hub user)
          (funcall (oref hub authenticator) hub user))
        (jupyter-api-copy-cookies-for-websocket (oref client url))))))

;;; Utility

;; See https://jupyterhub.readthedocs.io/en/stable/reference/urls.html#special-handling-of-api-requests
(cl-defmethod jupyter-hub-service-available-p ((hub jupyter-hub-client) &rest endpoint)
  "Send a GET request to ENDPOINT at HUB and return non-nil if it was successful.
If a 503 error (service unavailable) occurs return nil. Raise an
error otherwise."
  (condition-case err
      (prog1 t (apply #'jupyter-api-request hub "GET" endpoint))
    (jupyter-api-http-error
     (unless (eq (nth 1 err) 503)       ; Service unavailable
       (signal (car err) (cdr err))))))

;; See https://jupyterhub.readthedocs.io/en/stable/reference/urls.html#hub-spawn-pending-username-servername
(defun jupyter-hub-spawn-pending-p (hub user)
  "Return non-nil if USER's server on HUB has been spawned and is pending."
  (when (jupyter-hub-service-available-p hub "user" user)
    (let ((url-max-redirections 0))
      ;; If the spawn-pending page is redirected, then the server is ready.
      (condition-case nil
          (prog1 t
            (jupyter-api-request hub "GET" "hub" "spawn-pending" user))
        (jupyter-api-http-redirect-limit nil)))))

(defun jupyter-hub-user-server (hub user)
  (jupyter-hub-make-client hub user 'jupyter-server))

(provide 'jupyter-hub-api)

;;; jupyter-hub-api.el ends here
