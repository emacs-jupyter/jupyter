;;; jupyter-rest-api.el --- Jupyter REST API -*- lexical-binding: t -*-

;; Copyright (C) 2019 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 03 Apr 2019
;; Version: 0.7.3

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

;; Routines for working with the Jupyter REST API. Currently only the
;; api/kernels and api/kernelspecs endpoints are implemented.

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'jupyter-base)
(require 'websocket)

(defgroup jupyter-rest-api nil
  "Jupyter REST API"
  :group 'jupyter)

(defmacro jupyter-with-api (client &rest body)
  "Bind `jupyter-api-url' to the url slot of CLIENT, evaluate BODY."
  (declare (indent 1))
  `(let ((jupyter-api-url (oref ,client url)))
     ,@body))

(defvar jupyter-api-url nil
  "The URL used for requests by `jupyter-api--request'.")

(defvar jupyter-api-max-authentication-attempts 3
  "Maximum number of retries used for authentication.
When attempting to authenticate a request, try this many times
before raising an error.")

;;; Jupyter REST API

(defvar url-http-end-of-headers)
(defvar url-http-response-status)
(defvar gnutls-verify-error)

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
    :initform 'ask
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
  (unless (slot-boundp client 'ws-url)
    (let ((url (url-generic-parse-url (oref client url))))
      (setf (url-type url) (if (equal (url-type url) "https") "wss" "ws"))
      (oset client ws-url (url-recreate-url url)))))

;;; Authentication

(defvar jupyter-api--auth-in-progress-p nil
  "Let bound in `jupyter-api--authenticate'.")

;; TODO: Be more efficient by working with url objects directly

;; For more info on the XSRF header see
;; https://blog.jupyter.org/security-release-jupyter-notebook-4-3-1-808e1f3bb5e2
;; and
;; http://www.tornadoweb.org/en/stable/guide/security.html#cross-site-request-forgery-protection
(defun jupyter-api--xsrf-header (url)
  (let ((url (url-generic-parse-url url)))
    (cl-loop for cookie in (url-cookie-retrieve (url-host url) "/" nil)
             if (equal (url-cookie-name cookie) "_xsrf")
             return (list (cons "X-XSRFTOKEN" (url-cookie-value cookie))))))

(defun jupyter-api--copy-cookies (url)
  "Copy URL cookies so that those under HOST are accessible under HOST:PORT.
`url-retrieve-synchronously' will store cookies under HOST
whereas `websocket-open' will expect those cookies to be under
HOST:PORT if PORT is a nonstandard port for the type of URL.

The behavior of `url-retrieve-synchronously' (cookies being
stored without considering a PORT) appears to be the standard,
see RFC 6265."
  (when-let* ((url (url-generic-parse-url url))
              (host (url-host url))
              (port (url-port-if-non-default url))
              (host-port (format "%s:%s" host port)))
    (cl-loop
     for cookie in (url-cookie-retrieve host "/")
     do (pcase-let (((cl-struct url-cookie name value expires
                                localpart secure)
                     cookie))
          (url-cookie-store name value expires host-port localpart secure)))))

(defun jupyter-api--password-login (client &optional empty-pw)
  "Login to Jupyter server using CLIENT.
This function attempts to login to the server located at CLIENT's
url slot by using a password and raises an error if it cannot do
so.

On success, return a non-nil value. Note, when successful the
necessary cookies will be used to authenticate requests when
`jupyter-api-request' is called.

If EMPTY-PW is on-nil, don't ask the user for a password just use
an empty string as the password. This is mainly used to set the
_xsrf cookie on password-less logins."
  (let* ((url (oref client url))
         (xsrf (jupyter-api--xsrf-header url))
         (url-request-method "POST")
         (url-request-extra-headers
          (append xsrf
                  (list (cons "Content-Type"
                              "application/x-www-form-urlencoded"))
                  url-request-extra-headers))
         (pw (if empty-pw "" (read-passwd (format "Password [%s]: " url))))
         (url-request-data (concat "password=" pw)))
    (unwind-protect
        (let* ((status nil)
               (done nil)
               (cb (lambda (s &rest _) (setq status s done t)))
               (buffer (url-retrieve (concat url "/login") cb nil t)))
          (jupyter-with-timeout
              (nil jupyter-long-timeout
                   (error "Timeout when logging in"))
            done)
          (let ((err (plist-get status :error)))
            (unless (or (not err)
                        ;; Handle unauthorized request that set the XSRF cookie
                        (and (not xsrf)
                             (= (nth 2 err) 403)
                             (jupyter-api--xsrf-header url))
                        ;; Handle HTTP 1.0. When given a POST request, 302
                        ;; redirection doesn't change the method to GET
                        ;; dynamically. On the Jupyter notebook, the redirected
                        ;; page expects a GET and will return 405 (invalid
                        ;; method).
                        (with-current-buffer buffer
                          (and (plist-get status :redirect)
                               (= url-http-response-status 302)
                               (= (nth 2 err) 405))))
              (signal (car err) (cdr err))))
          ;; Ensure that the cookies are written so that the communication
          ;; subprocesses have access to them.
          (jupyter-api--copy-cookies url)
          (url-cookie-write-file)
          t)
      (clear-string url-request-data)
      (clear-string pw))))

(defun jupyter-api--authenticate (client &optional retries)
  "Ensure requests using CLIENT are authenticated.
Allow up to RETRIES attempts at authenticating, defaulting to
`jupyter-api-max-authentication-attempts'.

See the documentation of a `jupyter-rest-client's AUTH slot for
more details."
  (unless jupyter-api--auth-in-progress-p
    (cl-check-type client jupyter-rest-client)
    (or (numberp retries) (setq retries jupyter-api-max-authentication-attempts))
    (when (zerop retries)
      (error "Maximum number of authentication tries reached"))
    (with-slots (auth url) client
      (let ((jupyter-api--auth-in-progress-p t))
        (pcase auth
          ((guard (or (listp auth)
                      (not (memq auth '(ask token password)))))
           t)
          ('ask
           (cond
            ;; Check to see if we don't need to ask, e.g. when the cookies are
            ;; already set.
            ((ignore-errors (jupyter-api-get-kernelspec client))
             ;; Get the _xsrf cookies if we don't have them already.
             (unless (jupyter-api--xsrf-header (oref client url))
               (jupyter-api--password-login client t))
             (oset client auth t))
            (t
             (cond
              ((y-or-n-p (format "Token authenticated [%s]? " url))
               (oset client auth 'token)
               (jupyter-api--authenticate client retries))
              ((y-or-n-p (format "Password needed [%s]? " url))
               (oset client auth 'password)
               (jupyter-api--authenticate client retries))
              (t (oset client auth t))))))
          ('token
           (let ((token (read-string "Token: ")))
             (oset client auth (list (cons "Authorization" (concat "token " token))))
             (unless (ignore-errors (jupyter-api-get-kernelspec client))
               (oset client auth 'token)
               (jupyter-api--authenticate client (1- retries)))))
          ('password
           (if (ignore-errors (jupyter-api--password-login client))
               (oset client auth t)
             (jupyter-api--authenticate client (1- retries)))))))))

(defun jupyter-api-http-request (url endpoint method &rest data)
  "Send request to URL/ENDPOINT using HTTP METHOD.
DATA is encoded into a JSON string using `json-encode' and sent
as the HTTP request data. If DATA is nil, don't send any
request data."
  (declare (indent 3))
  (let* ((url-request-method method)
         (url-request-data (or (and data (json-encode data))
                               url-request-data))
         (url-request-extra-headers
          (append (when data
                    (list (cons "Content Type" "application/json")))
                  url-request-extra-headers))
         ;; Prevent the connection if security checks fail
         (gnutls-verify-error t))
    (with-current-buffer (url-retrieve-synchronously
                          (concat url "/" endpoint)
                          t nil jupyter-long-timeout)
      (jupyter-api--copy-cookies url)
      (goto-char url-http-end-of-headers)
      (skip-syntax-forward "->")
      (unless (eobp)
        (let* ((json-object-type 'plist)
               (resp (ignore-errors (json-read))))
          (cond
           ((>= url-http-response-status 400)
            (cl-destructuring-bind
                (&key reason message traceback &allow-other-keys) resp
              (if traceback
                  (error "%s (%s): %s" reason message
                         (car (last (split-string traceback "\n" 'omitnulls))))
                (and reason (setq reason (concat reason ": ")))
                (if message (error "%s" (concat reason message))
                  (error "Bad Response %s" url-http-response-status)))))
           (t
            resp)))))))

;;; Calling the REST API

(defun jupyter-api--request (method &rest plist)
  "Send an HTTP request to `jupyter-api-url'.
METHOD is the HTTP request method and PLIST contains the request.
The elements of PLIST before the first keyword form the REST api
endpoint and the rest of the PLIST after will be encoded into a
JSON object and sent as the request data if METHOD is POST. So a
call like

    \(let ((jupyter-api-url \"http://localhost:8888\"))
       (jupyter-api--request \"POST\" \"kernels\" :name \"python\"))

will create an http POST request to the url
http://localhost:8888/api/kernels using the JSON encoded from the
plist (:name \"python\") as the POST data.

As a special case, if METHOD is \"WS\", a websocket will be
opened using the REST api url and PLIST will be used in a call to
`websocket-open'."
  (let (endpoint)
    (while (and plist (not (or (keywordp (car plist))
                               (null (car plist)))))
      (push (pop plist) endpoint))
    (setq endpoint (nreverse endpoint))
    (cl-assert (not (null endpoint)))
    (cl-assert (not (null jupyter-api-url)))
    (setq endpoint (mapconcat #'identity (cons "api" endpoint) "/"))
    (pcase method
      ("WS"
       (jupyter-api--copy-cookies jupyter-api-url)
       (apply #'websocket-open
              (concat jupyter-api-url "/" endpoint)
              plist))
      (_
       (apply #'jupyter-api-http-request
              jupyter-api-url endpoint method
              plist)))))

(cl-defgeneric jupyter-api-request ((client jupyter-rest-client) method &rest plist)
  (declare (indent 2)))

(cl-defmethod jupyter-api-request ((client jupyter-rest-client) method &rest plist)
  "Send an HTTP request using CLIENT.
METHOD is the HTTP request method and PLIST contains the request.
The elements of PLIST before the first keyword form the REST api
endpoint and the rest of the PLIST after will be encoded into a
JSON object and sent as the request data. So a call like

   \(jupyter-api-request client \"POST\" \"kernels\" :name \"python\")

where the url slot of client is http://localhost:8888 will create
an http POST request to the url http://localhost:8888/api/kernels
using the JSON encoded from the plist (:name \"python\") as the
POST data.

Note an empty plist (after forming the endpoint) is interpreted
as no request data at all and NOT as an empty JSON dictionary.

As a special case, if METHOD is \"WS\", a websocket will be
opened using the REST api url and PLIST will be used in a call to
`websocket-open'."
  (jupyter-api--authenticate client)
  (let* ((xsrf (jupyter-api--xsrf-header (oref client url)))
         (url-request-extra-headers url-request-extra-headers)
         (jupyter-api-url
          (slot-value
           client
           (pcase method
             ("WS"
              (prog1 'ws-url
                (let ((head plist))
                  (while (and head (not (keywordp (car head))))
                    (pop head))
                  (setq head (or (plist-member head :custom-header-alist)
                                 (setcdr (last plist)
                                         (list :custom-header-alist nil))))
                  (let ((cur (plist-get head :custom-header-alist)))
                    (plist-put head :custom-header-alist
                               (append xsrf
                                       (jupyter-api-auth-headers client)
                                       cur))))))
             (_
              (prog1 'url
                (cl-callf2 append
                    (append xsrf (jupyter-api-auth-headers client))
                    url-request-extra-headers)))))))
    (apply #'jupyter-api--request method plist)))

(cl-defmethod jupyter-api/kernels ((client jupyter-rest-client) method &rest plist)
  "Send an HTTP request to the api/kernels endpoint to CLIENT's url.
METHOD is the HTTP method to use. PLIST has the same meaning as
in `jupyter-api-request'."
  (apply #'jupyter-api-request client method "kernels" plist))

(cl-defmethod jupyter-api/kernelspecs ((client jupyter-rest-client) method &rest plist)
  "Send an HTTP request to the api/kernelspecs endpoint of CLIENT.
METHOD is the HTTP method to use. PLIST has the same meaning as
in `jupyter-api-request'."
  (apply #'jupyter-api-request client method "kernelspecs" plist))

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

;;;; Kernel websocket

(defun jupyter-api-kernel-ws (client id &rest plist)
  "Return a websocket using CLIENT's ws-url slot.
ID identifies the kernel to connect to, PLIST will be passed to
the call to `websocket-open' to initialize the websocket.

Note, ID is also stored as the `websocket-client-data' slot of
the returned websocket."
  (let ((ws (apply #'jupyter-api/kernels client "WS" id "channels" plist)))
    (prog1 ws
      (setf (websocket-client-data ws) id))))

;;; Kernelspec API

(defun jupyter-api-get-kernelspec (client &optional name)
  "Send an HTTP request using CLIENT to get the kernelspec with NAME.
If NAME is not provided, return a plist of all kernelspecs
available via CLIENT."
  (apply #'jupyter-api/kernelspecs client "GET"
         (when name (list :name name))))

;;; Shutdown/interrupt kernel

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

(provide 'jupyter-rest-api)

;;; jupyter-rest-api.el ends here
