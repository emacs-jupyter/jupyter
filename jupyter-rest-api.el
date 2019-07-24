;;; jupyter-rest-api.el --- Jupyter REST API -*- lexical-binding: t -*-

;; Copyright (C) 2019 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 03 Apr 2019
;; Version: 0.7.3

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

;; Routines for working with the Jupyter REST API. Currently only the kernels,
;; kernelspecs, contents, and config endpoints are implemented. Functions that
;; get information from the server take the form `jupyter-api-get-*'. The lower
;; level functions that make requests have the form `jupyter-api/<endpoint>'.
;; Functions that alter the state of a kernel look like
;; `jupyter-api-interrupt-kernel'. Those that modify files have the appropriate
;; name familiar to Emacs-Lisp, e.g. to create a directory on the server there
;; is the function `jupyter-api-make-directory'. The exception are those that
;; actually read and write files, `jupyter-api-read-file-content' and
;; `jupyter-api-write-file-content' respectively.

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'jupyter-base)
(require 'websocket)
(require 'url)
(require 'url-http)

(declare-function jupyter-decode-time "jupyter-messages")

(defgroup jupyter-rest-api nil
  "Jupyter REST API"
  :group 'jupyter)

(defcustom jupyter-api-authentication-method 'ask
  "Authentication method to use for new connections."
  :type '(choice (const :tag "None" none)
                 (const :tag "Token based" token)
                 (const :tag "Password" password)
                 (const :tag "Ask" ask))
  :group 'jupyter-rest-api)

(defvar jupyter-api-max-authentication-attempts 3
  "Maximum number of retries used for authentication.
When attempting to authenticate a request, try this many times
before raising an error.")

;;; Jupyter REST API

(defmacro jupyter-api-with-subprocess-setup (&rest body)
  "Return a form to load cookies, load `jupyter-rest-api', then evaluate BODY.
The paths added to `load-path' are those necessary for proper
operation of a `jupyter-rest-client'."
  `(progn
     (require 'url)
     (url-do-setup)
     ;; Don't save any cookies or history in a subprocess
     (ignore-errors (cancel-timer url-history-timer))
     (ignore-errors (cancel-timer url-cookie-timer))
     (push ,(file-name-directory (locate-library "jupyter-base")) load-path)
     (push ,(file-name-directory (locate-library "websocket")) load-path)
     (require 'jupyter-rest-api)
     ,@body))

(defclass jupyter-rest-client ()
  ;; convert to a url field to avoid parsing this every time
  ((url
    :type string
    :initform "http://localhost:8888"
    :initarg :url)
   (ws-url
    :type string
    :initarg :ws-url
    :documentation "The WebSocket url to use.

If this slot is not bound when initializing an instance of this
class, it defaults to the value of the URL slot with the \"http\"
prefix replaced by \"ws\". ")
   (auth
    :initarg :auth
    :documentation "Indicator for authentication.

If the symbol ask, ask the user how to authenticate requests to
URL.

If a list, then its assumed to be a list of cons cells that are
the necessary HTTP headers used to authenticate requests and will
be passed along with every request made.

If the symbol password, ask for a login password to use.

If the symbol token, ask for a token to use.

If any other value, assume no steps are necessary to authenticate
requests.")))

(cl-defmethod initialize-instance ((client jupyter-rest-client) &optional _slots)
  "Set CLIENT's WS-URL slot if it isn't bound.
WS-URL will be a copy of URL with the url type equal to either ws
or wss depending on if URL has a type of http or https,
respectively."
  (cl-call-next-method)
  (unless (slot-boundp client 'auth)
    (oset client auth jupyter-api-authentication-method))
  (unless (slot-boundp client 'ws-url)
    (let ((url (url-generic-parse-url (oref client url))))
      (setf (url-type url) (if (equal (url-type url) "https") "wss" "ws"))
      (oset client ws-url (url-recreate-url url)))))

;;; Making HTTP requests

(define-error 'jupyter-api-http-error
  "Jupyter REST API error")

(define-error 'jupyter-api-http-redirect-limit
  "Redirect limit reached" 'jupyter-api-http-error)

(defvar url-http-codes)
(defvar url-http-content-type)
(defvar url-http-end-of-headers)
(defvar url-http-response-status)
(defvar url-callback-arguments)
(defvar gnutls-verify-error)

(defun jupyter-api-url-parse-response (buffer)
  "Given a URL BUFFER parse and return its response.
BUFFER should be a URL buffer as returned by, e.g.
`url-retrieve'. Return a plist representation of its JSON
content.

If the response indicates an error, signal a
`jupyter-api-http-error' otherwise return the parsed JSON or nil
if the content is not JSON.

If the maximum number of redirects are reached a
`jupyter-api-http-redirect-limit' error is raised instead."
  (with-current-buffer buffer
    (goto-char url-http-end-of-headers)
    (skip-syntax-forward "->")
    (let* ((json-object-type 'plist)
           (json-false nil)
           (resp (when (and (equal url-http-content-type "application/json")
                            (not (eobp)))
                   (json-read))))
      (cond
       ((>= url-http-response-status 400)
        (cl-destructuring-bind
            (&key reason message traceback &allow-other-keys) resp
          (when traceback
            (setq traceback
                  (format "%s (%s): %s" reason message
                          (car (last (split-string traceback "\n" t))))))
          (signal 'jupyter-api-http-error
                  (list url-http-response-status
                        (or traceback
                            (and (or reason message)
                                 (concat reason
                                         (and reason ": ")
                                         message))
                            (nth 2 (assoc url-http-response-status
                                          url-http-codes)))))))
       ;; Handle other kinds of errors, e.g. max redirects
       ((and (boundp 'url-callback-arguments)
             (plist-get (car url-callback-arguments) :error))
        (let ((err (plist-get (car url-callback-arguments) :error)))
          (if (eq (nth 1 err) 'http-redirect-limit)
              (signal 'jupyter-api-http-redirect-limit
                      (cons url-http-response-status
                            (cddr err)))
            (signal (car err) (cdr err)))))
       (t resp)))))

(defun jupyter-api-url-request (url &optional async &rest async-args)
  "Retrieve URL and return its JSON response.
ASYNC and ASYNC-ARGS have the same meaning as CALLBACK and CBARGS
of `url-retrieve'.

If ASYNC is nil, retrieve URL synchronously and return its JSON
response or signal an error when something went wrong with the
request. On success, if the response obtained by URL is not JSON,
return nil otherwise the parsed JSON is returned as a plist. On
error, either an `jupyter-api-http-error' (when
`url-http-response-status' >= 400),
`jupyter-api-http-redirect-limit' (when `url-max-redirections' is
reached), or `error' (on any other kind of URL error) is signaled.

When ASYNC is a callback function, this function does the same
thing as `url-retrieve' with its SILENT argument set to t and
INHIBIT-COOKIES set to nil."
  (let ((url-package-name "jupyter")
        (url-package-version jupyter-version)
        ;; Prevent the connection if security checks fail
        (gnutls-verify-error t)
        ;; Avoid errors when `default-directory' is a remote
        ;; directory path. `url' seems to not be able to handle it.
        (default-directory user-emacs-directory))
    (if async (url-retrieve url async async-args t)
      (let ((buffer (url-retrieve-synchronously url t nil jupyter-long-timeout)))
        (unwind-protect
            (jupyter-api-url-parse-response buffer)
          (url-mark-buffer-as-dead buffer))))))

;; See jupyter/notebook/services/api/api.yaml for HTTP
;; response codes.
(defun jupyter-api-http-request (url endpoint method &rest data)
  "Send request to URL/ENDPOINT using HTTP METHOD.
DATA is encoded into a JSON string using `json-encode-plist' and
sent as the HTTP request data. If DATA is nil, don't send any
request data."
  (declare (indent 3))
  (let ((url-request-method method)
        (url-request-data (or (and data (json-encode-plist data))
                              url-request-data))
        (url-request-extra-headers
         (append (when data
                   (list (cons "Content-Type" "application/json")))
                 url-request-extra-headers)))
    (jupyter-api-url-request (concat url "/" endpoint))))

;;; Cookies and headers

;; For more info on the XSRF header see
;; https://blog.jupyter.org/security-release-jupyter-notebook-4-3-1-808e1f3bb5e2
;; and
;; http://www.tornadoweb.org/en/stable/guide/security.html#cross-site-request-forgery-protection

(defun jupyter-api-request-xsrf-cookie (client)
  "Send a request using CLIENT to retrieve the _xsrf cookie."
  ;; Don't use `jupyter-api-request' here to avoid an infinite authentication
  ;; loop since this function is used during authentication.
  (let (url-request-extra-headers
        url-request-data)
    (jupyter-api-http-request (oref client url) "login" "GET")))

(defun jupyter-api-url-cookies (url)
  "Return the list of cookies for URL."
  (or (url-p url) (setq url (url-generic-parse-url url)))
  (url-cookie-retrieve
   (url-host url) (concat (url-filename url) "/")
   (equal (url-type url) "https")))

(defun jupyter-api-xsrf-header-from-cookies (url)
  "Return an alist containing an X-XSRFTOKEN header or nil.
Searches the cookies of URL for an _xsrf token, if found, sets
the value of the cookie as the value of the X-XSRFTOKEN header
returned."
  (cl-loop
   for cookie in (jupyter-api-url-cookies url)
   if (equal (url-cookie-name cookie) "_xsrf")
   return (list (cons "X-XSRFTOKEN" (url-cookie-value cookie)))))

(defun jupyter-api-copy-cookies-for-websocket (url)
  "Copy URL cookies so that those under HOST are accessible under HOST:PORT.
`url-retrieve-synchronously' will store cookies under HOST
whereas `websocket-open' will expect those cookies to be under
HOST:PORT when PORT is a nonstandard port for the type of URL.

The behavior of `url-retrieve-synchronously' (cookies being
stored without considering a PORT) appears to be the standard,
see RFC 6265."
  (when-let* ((url (url-generic-parse-url url))
              (host (url-host url))
              (port (url-port-if-non-default url))
              (host-port (format "%s:%s" host port)))
    (cl-loop
     for cookie in (jupyter-api-url-cookies url)
     do (pcase-let (((cl-struct url-cookie name value expires
                                localpart secure)
                     cookie))
          (url-cookie-store name value expires host-port localpart secure)))))

(defun jupyter-api-add-websocket-headers (plist)
  "Destructively modify PLIST to add a `:custom-header-alist' key.
Appends the value of `url-request-extra-headers' to the
`:custom-header-alist' key of PLIST, creating the key if
necessary. Before doing so, move past any non-keyword elements of
PLIST so as to only modify what looks like a property list.

Return the modified PLIST."
  (or plist (setq plist (list :custom-header-alist nil)))
  (let ((head plist))
    (while (and head (not (keywordp (car head))))
      (pop head))
    (setq head (or (plist-member head :custom-header-alist)
                   (setcdr (last plist)
                           (list :custom-header-alist nil))))
    (prog1 plist
      (plist-put head :custom-header-alist
                 (append
                  (plist-get head :custom-header-alist)
                  url-request-extra-headers)))))

;;; Authentication

(defvar jupyter-api-authentication-in-progress-p nil)

(define-error 'jupyter-api-login-failed
  "Login attempt failed")

(define-error 'jupyter-api-authentication-failed
  "Authentication failed")

;;;; Logging in

(defmacro jupyter-api--without-url-http-authentication-handler (&rest body)
  (declare (indent 0))
  ;; Workaround to suppress the authentication handling of `url-retrieve'.
  ;; Jupyter notebook return a 401 response without a www-authenticate header
  ;; and `url-http-handle-authentication' handles this by defaulting to
  ;; "basic" authentication which we don't want happening.
  (let ((orig (make-symbol "orig")))
    `(let ((,orig (symbol-function #'url-http-handle-authentication)))
       (cl-letf (((symbol-function #'url-http-handle-authentication)
                  (lambda (proxy &rest args)
                    ;; If there is an authenticate header, let the default
                    ;; `url-http-handle-authentication' handle it.
                    (if (mail-fetch-field
                         (if proxy "proxy-authenticate" "www-authenticate")
                         nil nil t)
                        (apply ,orig proxy args)
                      ;; Otherwise assume we are authenticated to suppress the
                      ;; "basic" authentication handling.
                      t))))
         ,@body))))

(defun jupyter-api--verify-login (status)
  (let ((err (plist-get status :error)))
    (unless
        (or (not err)
            ;; Handle HTTP 1.0. When given a POST request, 302 redirection
            ;; doesn't change the method to GET dynamically. On the Jupyter
            ;; notebook, the redirected page expects a GET and will return
            ;; 405 (invalid method).
            (and (plist-get status :redirect)
                 (= (nth 2 err) 405)))
      (signal 'jupyter-api-login-failed err))))

(defun jupyter-api-login (client)
  "Attempt to login to the server using CLIENT.
Login is attempted by sending a GET request to CLIENT's login
endpoint using `url-retrieve'. To change the login information,
set `url-request-method', `url-request-data', and
`url-request-extra-headers'.

On success, write the URL cookies to file so that they can be
used by other Emacs processes and return non-nil.

If a response isn't received in `jupyter-long-timeout' seconds,
raise an error.

If the login attempt failed, raise a `jupyter-api-login-failed'
error with the data being the error received by `url-retrieve'."
  (jupyter-api--without-url-http-authentication-handler
    (condition-case err
        (let (status done)
          (jupyter-api-url-request
           (concat (oref client url) "/login")
           (lambda (s &rest _) (setq status s done t)))
          (jupyter-with-timeout
              (nil jupyter-long-timeout
                   (error "Timeout reached during login"))
            done)
          (jupyter-api--verify-login status)
          (jupyter-api-copy-cookies-for-websocket (oref client url))
          (url-cookie-write-file)
          t)
      (error
       (when (eq (nth 2 err) 'connection-failed)
         (signal (car err) (cdr err)))))))

;;;; Authenticators

(cl-defmethod jupyter-api-server-accessible-p ((client jupyter-rest-client))
  "Return non-nil if CLIENT can access the Jupyter notebook server."
  (ignore-errors
    (prog1 t
      (let ((jupyter-api-authentication-in-progress-p t)
            url-request-data url-request-extra-headers)
        (jupyter-api-get-kernelspec client)))))

(cl-defgeneric jupyter-api-authenticate ((client jupyter-rest-client) &rest args)
  (declare (indent 1)))

(cl-defmethod jupyter-api-authenticate ((client jupyter-rest-client) (authenticator function))
  "Call AUTHENTICATOR then check if CLIENT can access the REST server.
Repeat up to `jupyter-api-max-authentication-attempts' before
signaling a `jupyter-api-authentication-failed' error if CLIENT
cannot access the server.

AUTHENTICATOR is called with zero arguments.

Before attempting to authenticate, save the value of the AUTH
slot of CLIENT and restore the AUTH slot on failure."
  (let ((jupyter-api-authentication-in-progress-p t)
        (max-tries jupyter-api-max-authentication-attempts))
    (let ((auth (oref client auth)))
      (jupyter-api-request-xsrf-cookie client)
      (let ((url-request-extra-headers
             (nconc (jupyter-api-xsrf-header-from-cookies (oref client url))
                    (jupyter-api-auth-headers client))))
        (while (and (not (progn
                           (funcall authenticator)
                           (jupyter-api-server-accessible-p client)))
                    (not (zerop (cl-decf max-tries))))))
      (when (zerop max-tries)
        (oset client auth auth)
        (signal 'jupyter-api-authentication-failed
                (list client))))))

(cl-defmethod jupyter-api-authenticate ((client jupyter-rest-client) (_auth (eql password))
                                        &optional passwd)
  "Authenticate CLIENT by asking for a password.
If PASSWD is provided it must be a function that takes zero
arguments. It will be called before each authentication attempt.
If CLIENT could not be authenticated raise an error."
  (or (functionp passwd)
      (setq passwd (lambda () (read-passwd (format "Password [%s]: "
                                              (oref client url))))))
  (jupyter-api-authenticate client
    ;; FIXME: Workaround due to the function generalizer in the base
    ;; `jupyter-api-authenticate' method only recognizing function symbols or
    ;; compiled functions since it currently uses `type-of' instead of
    ;; `cl-typep'. This wouldn't be needed for the compiled sources, but seems
    ;; to cause issues on Windows even when the sources are compiled.
    (apply-partially
     (lambda ()
       (let ((url-request-method "POST")
             (url-request-extra-headers
              (nconc (list (cons "Content-Type"
                                 "application/x-www-form-urlencoded"))
                     url-request-extra-headers))
             (url-request-data
              (concat "password=" (url-hexify-string (funcall passwd)))))
         (jupyter-api-login client)))))
  (oset client auth t))

(cl-defmethod jupyter-api-authenticate ((client jupyter-rest-client) (_auth (eql token)))
  "Authenticate CLIENT by asking for a token.
Access to server will be checked by setting the token in the
Authorization header.

Raise an error on failure."
  (jupyter-api-authenticate client
    (apply-partially
     (lambda ()
       (let ((token (read-string (format "Token [%s]: " (oref client url)))))
         (oset client auth
               (list (cons "Authorization" (concat "token " token)))))))))

;;; `jupyter-rest-client' methods

(cl-defmethod jupyter-api-ensure-authenticated :around ((_client jupyter-rest-client))
  (unless jupyter-api-authentication-in-progress-p
    (let ((jupyter-api-authentication-in-progress-p t))
      (cl-call-next-method))))

(cl-defmethod jupyter-api-ensure-authenticated ((client jupyter-rest-client))
  (with-slots (auth url) client
    (when (eq auth 'ask)
      (jupyter-api-request-xsrf-cookie client)
      (when (jupyter-api-server-accessible-p client)
        (oset client auth t)))
    (unless (or (listp auth)
                (not (memq auth '(ask token password))))
      (when (eq auth 'ask)
        (when noninteractive
          (signal 'jupyter-api-authentication-failed
                  (list "Can't authenticate non-interactively")))
        (cond
         ((y-or-n-p (format "Token authenticated [%s]? " url))
          (oset client auth 'token))
         ((y-or-n-p (format "Password needed [%s]? " url))
          (oset client auth 'password))
         (t
          (signal 'jupyter-api-authentication-failed
                  (list "Can only authenticate with password or token")))))
      (jupyter-api-authenticate client (oref client auth)))))

(cl-defmethod jupyter-api-auth-headers ((client jupyter-rest-client))
  "Return the HTTP headers CLIENT is using for authentication or nil."
  (jupyter-api-ensure-authenticated client)
  (with-slots (auth) client
    (when (listp auth)
      auth)))

;;;; Calling the REST API

(defun jupyter-api-construct-endpoint (plist)
  "Return a cons cell (ENDPOINT . REST) based on PLIST.
ENDPOINT is the API endpoint constructed from the elements at the
beginning of PLIST that are strings. REST will contain the
remainder of PLIST.

So if PLIST looks like

    '(\"api\" \"kernels\" :k1 ...)

ENDPOINT will be \"api/kernels\" and REST will be '(:k1 ...).

If there is an alist after the strings of PLIST that make up the
ENDPOINT, the alist is interpreted as the query component of
ENDPOINT. So if PLIST looks like

    '(\"api\" \"contents\" ((\"content\" . \"1\")) :k1 ...)

The returned ENDPOINT will be \"api/contents?content=1\" and REST
will be '(:k1 ...)."
  (let (endpoint)
    (while (and plist (or (null (car plist))
                          (stringp (car plist))))
      ;; Remove any trailing empty strings or nil values so that something like
      ;; ("contents?content=0" "") doesn't get turned into
      ;; "api/contents?contents=0/" below.
      (if (memq (car plist) '(nil "")) (pop plist)
        (cl-check-type (car plist) string
                       "Endpoint can only be constructed from strings")
        (push (pop plist) endpoint)))
    (setq endpoint
          ;; Allow the list of endpoint strings to contain sub-paths.
          (let ((url-unreserved-chars (cons ?/ url-unreserved-chars)))
            (mapconcat #'url-hexify-string
                       (or (nreverse endpoint) (list "")) "/")))
    (when (consp (car plist))
      (setq endpoint (concat endpoint "?"
                             (mapconcat
                              (lambda (x)
                                (cl-check-type x cons)
                                (cl-check-type (car x) string)
                                (cl-check-type (cdr x) string)
                                (concat (url-hexify-string (car x)) "="
                                        (url-hexify-string (cdr x))))
                              (pop plist)
                              "&"))))
    (cons endpoint plist)))

(cl-defgeneric jupyter-api-request ((client jupyter-rest-client) method &rest plist)
  (declare (indent 2)))

(cl-defmethod jupyter-api-request ((client jupyter-rest-client) method &rest plist)
  "Send an HTTP request using CLIENT.
METHOD is the HTTP request method and PLIST contains the request.
The elements of PLIST before the first non-string form the REST
API endpoint and the rest of the PLIST after will be encoded into
a JSON object and sent as the request data. So a call like

   \(jupyter-api-request client \"POST\" \"api\" \"kernels\" :name \"python\")

where the url slot of client is http://localhost:8888 will create
an http POST request to the url http://localhost:8888/api/kernels
using the JSON encoded from the plist (:name \"python\") as the
POST data.

Note an empty plist (after forming the endpoint) is interpreted
as no request data at all and NOT as an empty JSON dictionary.

A call to this method can also look like

   \(jupyter-api-request client \"GET\"
      \"api\" \"contents\" '((\"content\" . \"1\"))

In this case, the alist after the strings that make up the base
endpoint, but before the rest of the non-strings elements of
PLIST, will be interpreted as the query component of the
resulting endpoint. So for the above example, the resulting url
will be http://localhost:8888/api/contents?content=1.

If METHOD is \"WS\", a websocket will be opened using the REST api
url and PLIST will be used in a call to `websocket-open'."
  (jupyter-api-ensure-authenticated client)
  (let ((url-request-extra-headers
         (append (jupyter-api-auth-headers client)
                 (jupyter-api-xsrf-header-from-cookies (oref client url))
                 url-request-extra-headers)))
    (condition-case err
        (cl-destructuring-bind (endpoint . rest)
            (jupyter-api-construct-endpoint plist)
          (pcase method
            ("WS"
             (jupyter-api-copy-cookies-for-websocket (oref client url))
             (apply #'websocket-open
                    (concat (oref client ws-url) "/" endpoint)
                    (jupyter-api-add-websocket-headers rest)))
            (_
             (apply #'jupyter-api-http-request
                    (oref client url) endpoint method
                    rest))))
      (jupyter-api-http-error
       ;; Reset the `auth' state when un-authenticated so that we ask again.
       (when (and (= (cadr err) 403)
                  (equal (caddr err) "Forbidden"))
         (oset client auth 'ask))
       (signal (car err) (cdr err))))))

;;;; Endpoints

(cl-defgeneric jupyter-api/kernels ((client jupyter-rest-client) method &rest plist)
  (declare (indent 2)))

(cl-defmethod jupyter-api/kernels ((client jupyter-rest-client) method &rest plist)
  "Send an HTTP request to the api/kernels endpoint to CLIENT's url.
METHOD is the HTTP method to use. PLIST has the same meaning as
in `jupyter-api-request'."
  (apply #'jupyter-api-request client method "api" "kernels" plist))

(cl-defgeneric jupyter-api/kernelspecs ((client jupyter-rest-client) method &rest plist)
  (declare (indent 2)))

(cl-defmethod jupyter-api/kernelspecs ((client jupyter-rest-client) method &rest plist)
  "Send an HTTP request to the api/kernelspecs endpoint of CLIENT.
METHOD is the HTTP method to use. PLIST has the same meaning as
in `jupyter-api-request'."
  (apply #'jupyter-api-request client method "api" "kernelspecs" plist))

(cl-defgeneric jupyter-api/contents ((client jupyter-rest-client) method &rest plist)
  (declare (indent 2)))

(cl-defmethod jupyter-api/contents ((client jupyter-rest-client) method &rest plist)
  "Send an HTTP request to the api/contents endpoint of CLIENT.
METHOD is the HTTP method to use. PLIST has the same meaning as
in `jupyter-api-request'."
  (apply #'jupyter-api-request client method "api" "contents" plist))

(cl-defgeneric jupyter-api/config ((client jupyter-rest-client) method &rest plist)
  (declare (indent 2)))

(cl-defmethod jupyter-api/config ((client jupyter-rest-client) method &rest plist)
  "Send an HTTP request to the api/config endpoint of CLIENT.
METHOD is the HTTP method to use. PLIST has the same meaning as
in `jupyter-api-request'."
  (apply #'jupyter-api-request client method "api" "config" plist))

;;; Config

(defun jupyter-api-get-config (client section)
  "Send an HTTP request using CLIENT to get the configuration for SECTION."
  (jupyter-api/config client "GET" section))

(defun jupyter-api-update-config (client section &rest plist)
  "Send a request using CLIENT to update configuration SECTION.
PLIST is a property list that will be encoded into JSON with the
requested changes."
  (apply #'jupyter-api/config client "PATCH" section plist))

;;; Kernels API

(defun jupyter-api-get-kernel (client &optional id)
  "Send an HTTP request using CLIENT to return a plist of the kernel with ID.
If ID is nil, return models for all kernels accessible via CLIENT."
  (jupyter-api/kernels client "GET" id))

(defun jupyter-api-start-kernel (client &optional name)
  "Send an HTTP request using CLIENT to start a kernel with kernelspec NAME.
If NAME is not provided use the default kernelspec."
  (apply #'jupyter-api/kernels client "POST"
         (when name (list :name name))))

(defun jupyter-api-shutdown-kernel (client id)
  "Send the HTTP request using CLIENT to shutdown a kernel with ID."
  (jupyter-api/kernels client "DELETE" id))

(defun jupyter-api-restart-kernel (client id)
  "Send an HTTP request using CLIENT to restart a kernel with ID."
  (jupyter-api/kernels client "POST" id "restart"))

(defun jupyter-api-interrupt-kernel (client id)
  "Send an HTTP request using CLIENT to interrupt a kernel with ID."
  (jupyter-api/kernels client "POST" id "interrupt"))

;;;; Shutdown/interrupt kernel

(cl-defmethod jupyter-shutdown-kernel ((client jupyter-rest-client) kernel-id
                                       &optional restart timeout)
  "Send an HTTP request using CLIENT to shutdown the kernel with KERNEL-ID.
Optionally RESTART the kernel. If TIMEOUT is provided, it is the
timeout used for the HTTP request."
  (let ((jupyter-long-timeout (or timeout jupyter-long-timeout)))
    (if restart (jupyter-api-restart-kernel client kernel-id)
      (jupyter-api-shutdown-kernel client kernel-id))))

(cl-defmethod jupyter-interrupt-kernel ((client jupyter-rest-client) kernel-id
                                        &optional timeout)
  "Send an HTTP request using CLIENT to interrupt the kernel with KERNEL-ID.
If TIMEOUT is provided, it is the timeout used for the HTTP
request."
  (let ((jupyter-long-timeout (or timeout jupyter-long-timeout)))
    (jupyter-api-interrupt-kernel client kernel-id)))


;;;; Kernel websocket

(defun jupyter-api-get-kernel-ws (client id &rest plist)
  "Return a websocket using CLIENT's ws-url slot.
ID identifies the kernel to connect to, PLIST will be passed to
the call to `websocket-open' to initialize the websocket.

Note the `websocket-client-data' of the returned websocket will
be a plist containing ID as the value of the :id key and the
value of the :session key will be `jupyter-session' with its
`jupyter-session-id' slot set to the session ID associated with
the websocket."
  (let* ((session (jupyter-session))
         (ws (apply #'jupyter-api/kernels client "WS" id "channels"
                    `(("session_id" . ,(jupyter-session-id session)))
                    plist)))
    (prog1 ws
      (setf (websocket-client-data ws)
            (list :id id :session session)))))

;;; Kernelspec API

(defun jupyter-api-get-kernelspec (client &optional name)
  "Send an HTTP request using CLIENT to get the kernelspec with NAME.
If NAME is not provided, return a plist of all kernelspecs
available via CLIENT."
  (apply #'jupyter-api/kernelspecs client "GET"
         (when name (list :name name))))

;;; Contents API

;; TODO: Actually consider encoding/decoding
;; https://jupyter-notebook.readthedocs.io/en/stable/extending/contents.html#filesystem-entities

(defun jupyter-api--strip-slashes (path)
  (thread-last path
    (replace-regexp-in-string "^/+" "")
    (replace-regexp-in-string "/+$" "")))

;; See https://jupyter-notebook.readthedocs.io/en/stable/extending/contents.html#api-paths
(defsubst jupyter-api-content-path (file)
  "Return a sanitized path for locating FILE on a Jupyter REST server.
Note, if FILE is not an absolute file name, it is expanded into
one as if the `default-directory' where /."
  (jupyter-api--strip-slashes (expand-file-name (file-local-name file) "/")))

(defun jupyter-api-get-file-model (client file &optional no-content type)
  "Send a request using CLIENT to get a model of FILE.
If NO-CONTENT is non-nil, tell the server to return a model
excluding the FILE's contents. Otherwise a model with contents is
returned.

If TYPE is non-nil, signal an error if FILE is not of the
specified type.

Note, only the `file-local-name' of FILE is considered.

A file model is a plist that contains the following keys:

  :name - The name of the file relative to its directory
  :path - The filesystem path of the file
  :last_modified - The last time the file was modified
  :created - The time the file was created
  :content - The file's contents or, if a file is directory, a
             vector of models representing the files contained
             in the directory
  :format - The format of the file, either \"text\", \"base64\", or nil
  :mimetype - The guessed mimetype or nil
  :size - The size of the file in bytes or nil
  :writable - If the file can be written to
  :type - Either \"directory\" or \"file\""
  (declare (indent 1))
  (jupyter-api/contents client "GET"
    (jupyter-api-content-path file)
    (nconc (list (cons "content" (if no-content "0" "1")))
           (when type (cons "type" type)))))

(defun jupyter-api-delete-file (client file-or-dir)
  "Send a request using CLIENT to delete FILE-OR-DIR from the server.

Note, only the `file-local-name' of FILE-OR-DIR is considered."
  (declare (indent 1))
  (jupyter-api/contents client "DELETE"
    (jupyter-api-content-path file-or-dir)))

(defun jupyter-api-rename-file (client file newname)
  "Send a request using CLIENT to rename FILE to NEWNAME.

Note, only the `file-local-name' of FILE and NEWNAME are
considered."
  (declare (indent 1))
  (jupyter-api/contents client "PATCH"
    (jupyter-api-content-path file)
    :path (jupyter-api-content-path newname)))

;; NOTE: The Jupyter REST API doesn't allow copying directories in an easy way
(defun jupyter-api-copy-file (client file newname)
  "Send a request using CLIENT to copy FILE to NEWNAME.
NEWNAME must not be an existing file.

Note, only the `file-local-name' of FILE and NEWNAME are
considered."
  (declare (indent 1))
  (cl-destructuring-bind (&key path &allow-other-keys)
      (jupyter-api/contents client "POST"
        (jupyter-api-content-path (file-name-directory newname))
        :copy_from (jupyter-api-content-path file))
    (jupyter-api-rename-file client path newname)))

(defun jupyter-api-write-file-content (client filename content &optional binary)
  "Send a request using CLIENT to write CONTENT to FILENAME.

If CONTENT is a string, the format of the written file will be
\"text\". If CONTENT is a list, it is encoded as JSON and be sent
to the server using \"json\" as the format. If BINARY is non-nil,
as a final step encode CONTENT as a base64 string and set the
file's format to \"base64\".

Note, only the `file-local-name' of FILENAME is considered."
  (declare (indent 1))
  (jupyter-api/contents client "PUT"
    (jupyter-api-content-path filename)
    :content (if binary (base64-encode-string content) content)
    :type "file"
    :format (if binary "base64" "text")))

(defun jupyter-api-read-file-content (client file)
  "Send a request using CLIENT to read the content of FILE.

If FILE's contents are encoded, decode it first. This currently
only applies to the case where FILE's format is \"base64\".

Note, only the `file-local-name' of FILENAME is considered."
  (declare (indent 1))
  (let* ((model (jupyter-api-get-file-model client file nil "file"))
         (content (plist-get model :content)))
    (cond
     ((jupyter-api-binary-content-p model)
      (base64-decode-string content))
     (t content))))

(defun jupyter-api-make-directory (client directory)
  "Send a request using CLIENT to create DIRECTORY.

Note, only the `file-local-name' of DIRECTORY is considered."
  (declare (indent 1))
  (jupyter-api/contents client "PUT"
    (jupyter-api-content-path directory)
    :type "directory"))

;; FIXME: Extremely slow and fails often with a `wrong-type-argument' error
;; somewhere in the `url-retrieve' code. It's not uploading very much data per
;; chunk...I wonder where it goes wrong?
(defun jupyter-api-upload-large-file (client file tofile &optional format filter)
  (or format (setq format "text"))
  (zmq-start-process
   `(lambda ()
      (jupyter-api-with-subprocess-setup
       (let* ((nchunk 1)
              (chunk-size (* 1024 1024 4))
              (beg 0) (end chunk-size)
              (client (jupyter-rest-client :url ,(oref client url)))
              (file ,(expand-file-name file))
              (total 0)
              (path ,(jupyter-api-content-path tofile))
              (exit nil))
         (with-temp-buffer
           (condition-case error
               ;; Open up the connection. Note you don't want the first
               ;; HTTP request to contain large amounts of data otherwise
               ;; `url-retrieve' will hang for some reason.
               (if (not (jupyter-api-server-accessible-p client))
                   (error "Host not accessible (%s)" (oref client url))
                 (while (not exit)
                   (erase-buffer)
                   (cl-destructuring-bind (_ nbytes)
                       (insert-file-contents-literally file nil beg end)
                     ,(when (equal format "base64")
                        '(base64-encode-region (point-min) (point-max)))
                     (jupyter-api/contents client "PUT"
                       path :chunk nchunk
                       :type "file"
                       :content (buffer-string)
                       :format ,format)
                     (when (< nbytes chunk-size)
                       (setq exit 0))
                     (cl-incf total nbytes)
                     (when (zerop (mod total (* 12 chunk-size)))
                       (zmq-prin1 (list 'progress nchunk total)))
                     (cl-incf nchunk)
                     (cl-incf beg chunk-size)
                     (cl-incf end chunk-size)))
                 (when (zerop exit)
                   (jupyter-api/contents client "PUT"
                     path :chunk -1
                     :type "file"
                     :content ""
                     :format ,format)
                   (zmq-prin1 (list 'done nchunk total))))
             (error
              (zmq-prin1 (list 'error nchunk beg end error))
              (setq exit 1))))
         (kill-emacs exit))))
   :filter (or filter
               (let ((tofile (concat (oref client url) "/api/contents/"
                                     (jupyter-api-content-path tofile))))
                 (lambda (event)
                   (cl-case (car event)
                     (progress
                      (message "Uploaded %s MB of %s to %s"
                               (/ (nth 2 event) (* 1024 1024.0)) file tofile))
                     (done
                      (message "Uploaded a total of %.2f MB %s to %s"
                               (/ (nth 2 event) (* 1024 1024.0)) file tofile))
                     (t
                      (error "Error uploading file %s to %s" file tofile))))))))

;;;; Checkpoints

(defun jupyter-api-get-checkpoints (client file)
  "Send a request using CLIENT to get all checkpoints available for FILE.
Return a list of checkpoints of the form

    (:id ID :last_modified TIME)

where ID is the ID of the checkpoint and TIME is the last time
FILE was modified when the checkpoint was created."
  (declare (indent 1))
  (append
   (jupyter-api/contents client "GET"
     (jupyter-api-content-path file) "checkpoints")
   nil))

(defun jupyter-api-get-ordered-checkpoints (client file)
  "Send a request using CLIENT to get all checkpoints available for FILE.
Return a list of the checkpoints ordered most recently created first."
  (declare (indent 1))
  (sort
   (jupyter-api-get-checkpoints client file)
   (lambda (a b)
     (let ((ta (jupyter-decode-time (plist-get a :last_modified)))
           (tb (jupyter-decode-time (plist-get b :last_modified))))
       (time-less-p tb ta)))))

(defun jupyter-api-get-latest-checkpoint (client file)
  "Return the latest checkpoint for FILE on the server accessed by CLIENT.
If there are no checkpoints for FILE return nil."
  (declare (indent 1))
  (car (jupyter-api-get-ordered-checkpoints client file)))

(defun jupyter-api-make-checkpoint (client file)
  "Send a request using CLIENT to create a checkpoint for FILE.
Return a plist (:id ID :last_modified TIME) where ID is the ID of
the checkpoint and TIME is the last modified time before the
checkpoint was created in ISO 8601 format."
  (declare (indent 1))
  (jupyter-api/contents client "POST"
    (jupyter-api-content-path file) "checkpoints"))

(defun jupyter-api-restore-checkpoint (client file id)
  "Send a request using CLIENT to restore FILE to checkpoint with ID.
ID is either a string or plist containing an :id property."
  (declare (indent 1))
  (when (listp id) (setq id (plist-get id :id)))
  (jupyter-api/contents client "POST"
    (jupyter-api-content-path file) "checkpoints" id))

(defun jupyter-api-delete-checkpoint (client file id)
  "Send a request using CLIENT to delete FILE's checkpoint with ID.
ID is either a string or plist containing an :id property."
  (declare (indent 1))
  (when (listp id) (setq id (plist-get id :id)))
  (jupyter-api/contents client "DELETE"
    (jupyter-api-content-path file) "checkpoints" id))

;;;; Utility functions

(defun jupyter-api-find-model (path dir-model)
  "Find a model with PATH in DIR-MODEL.
PATH must be an API content path as returned by
`jupyter-api-content-path'. Recursively searches for a model
whose :path property is equal to PATH, searching for other models
in the :content property of DIR-MODEL until either one is found
or DIR-MODEL isn't a directory model. Returns the model if found,
otherwise nil."
  (cond
   ((equal (plist-get dir-model :path) path) dir-model)
   ((equal (plist-get dir-model :type) "directory")
    (cl-loop
     for model across (plist-get dir-model :content)
     if (equal (plist-get model :path) path) return model
     else if (equal (plist-get model :type) "directory")
     thereis (jupyter-api-find-model path model)))))

(defun jupyter-api-binary-content-p (model)
  "Return non-nil if MODEL corresponds to Base64 encoded content."
  (equal (plist-get model :format) "base64"))

(defun jupyter-api-notebook-p (model)
  "Return non-nil if MODEL corresponds to Jupyter notebook JSON."
  (equal (plist-get model :type) "notebook"))

(defun jupyter-api-insert-model-content (model &optional replace beg end)
  "Insert the content of MODEL into the current buffer.
If REPLACE is non-nil, replace the contents of the current buffer
using `replace-buffer-contents'. BEG and END are byte offsets
into the content of MODEL, only insert the portion of MODEL's
contents bounded by BEG and END. BEG and END default to
`point-min' and `point-max' respectively."
  (cl-assert (not (equal (plist-get model :type) "directory")))
  (let ((source (generate-new-buffer " *temp*")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (set-buffer-multibyte nil)
            (insert (or (plist-get model :content) ""))
            (when (jupyter-api-binary-content-p model)
              ;; TODO: Cache the result of decoding?
              (base64-decode-region (point-min) (point-max)))
            (when (or beg end)
              (narrow-to-region (or beg (point-min)) (or end (point-max)))))
          (if replace (replace-buffer-contents source)
            (insert-buffer-substring source)))
      (and (buffer-live-p source) (kill-buffer source)))))

(provide 'jupyter-rest-api)

;;; jupyter-rest-api.el ends here
