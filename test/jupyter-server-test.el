;;; jupyter-server-test.el --- Test communication with a notebook server -*- lexical-binding: t -*-

;; Copyright (C) 2019 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 31 May 2019
;; Version: 0.0.1
;; X-URL: https://github.com/dzop/emacs-jupyter

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

;; Test `jupyter-rest-api' and `jupyter-server' related functionality.

;;; Code:

(require 'jupyter-server)
(require 'jupyter-org-client)
(require 'subr-x)

(defun jupyter-server-test-server (url)
  (or (jupyter-find-server url)
      (jupyter-server :url url)))

;;; REST API

(ert-deftest jupyter-rest-api ()
  :tags '(rest)
  (let ((client (jupyter-rest-client
                 :url "http://foo"
                 :ws-url "ws://foo"
                 :auth t)))
    (jupyter-test-rest-api
     (jupyter-api-request client "GET" "kernels")
     (should (equal url "http://foo/api/kernels"))
     (should (equal url-request-method "GET"))
     (should (equal url-request-data nil))
     (should (equal url-request-extra-headers nil)))
    (jupyter-test-rest-api
     (jupyter-api-request client "POST" "kernels" "ID" :name "bar")
     (should (equal url "http://foo/api/kernels/ID"))
     (should (equal url-request-method "POST"))
     (should (equal url-request-data (json-encode '(:name "bar"))))
     (should (equal (alist-get "Content-Type" url-request-extra-headers nil nil #'equal)
                    "application/json")))
    (cl-letf (((symbol-function #'websocket-open)
               (lambda (url &rest plist)
                 (should (equal url "ws://foo/api/kernels"))
                 (should (equal (plist-get plist :on-open) 'identity)))))
      (jupyter-api-request client "WS" "kernels" :on-open 'identity))))

(ert-deftest jupyter-api-copy-cookies-for-websocket ()
  :tags '(rest)
  (let* (url-cookie-storage
         (port (jupyter-test-ensure-notebook-server))
         (host (format "localhost:%s" port)))
    (url-cookie-store "_xsrf" "1" nil "localhost" "/")
    (url-cookie-store (format "username-login-%s" port) "2" nil "localhost" "/")
    (let ((old-cookies (url-cookie-retrieve "localhost" "/")) cookies)
      (jupyter-api-copy-cookies-for-websocket (format "http://%s" host))
      (should (setq cookies (url-cookie-retrieve host "/")))
      (cl-loop
       for cookie in old-cookies
       do (setf (url-cookie-domain cookie) host))
      (cl-loop
       for cookie in cookies
       ;; old-cookies now have the domain of the new cookies for verification
       ;; purposes
       do (should (member cookie old-cookies))))))

(ert-deftest jupyter-api-password-authenticator ()
  :tags '(rest)
  (let (cookies-copied-before-write
        cookies-written
        url-cookie-storage
        (host (format "localhost:%s" (jupyter-test-ensure-notebook-server))))
    (cl-letf (((symbol-function #'read-passwd)
               (lambda (&rest _) "foo"))
              ((symbol-function #'jupyter-api-copy-cookies-for-websocket)
               (lambda (&rest _)
                 (setq cookies-copied-before-write t)))
              ((symbol-function #'jupyter-api-server-accessible-p)
               (lambda (&rest _) t))
              ((symbol-function #'url-cookie-write-file)
               (lambda (&rest _)
                 (should cookies-copied-before-write)
                 (setq cookies-written t))))
      (url-cookie-store "_xsrf" "1" nil "localhost" "/")
      (jupyter-test-rest-api
       (let ((url-request-extra-headers
              (jupyter-api-xsrf-header-from-cookies
               (format "http://%s" host))))
         (jupyter-api-password-authenticator
             (jupyter-rest-client :url (format "http://%s" host))))
       (should (equal url-request-method "POST"))
       (should (equal url (format "http://%s/login" host)))
       (should (equal (cdr (assoc "X-XSRFTOKEN" url-request-extra-headers)) "1"))
       (should (equal (cdr (assoc "Content-Type" url-request-extra-headers))
                      "application/x-www-form-urlencoded"))
       (should (equal url-request-data "password=foo"))))
    (should cookies-written)))

(ert-deftest jupyter-api-get-kernel-ws ()
  :tags '(rest)
  (let* ((host (format "localhost:%s" (jupyter-test-ensure-notebook-server)))
         (client (jupyter-rest-client :url (format "http://%s" host))))
    (cl-destructuring-bind (&key id &allow-other-keys)
        (jupyter-api-start-kernel client)
      (unwind-protect
          (let ((ws (jupyter-api-get-kernel-ws client id)))
            (unwind-protect
                (progn
                  (should (websocket-openp ws))
                  (should (equal (websocket-client-data ws) id)))
              (websocket-close ws)))
        (jupyter-api-shutdown-kernel client id)))))


;;; Server

;; And `jupyter-server-kernel-comm'
(ert-deftest jupyter-server ()
  :tags '(server)
  (let* ((host (format "localhost:%s" (jupyter-test-ensure-notebook-server)))
         (server (jupyter-server-test-server (format "http://%s" host)))
         (kernel (jupyter-server-kernel
                  :spec (jupyter-guess-kernelspec
                         "python" (jupyter-server-kernelspecs server)))))
    (let ((id (jupyter-start-kernel kernel server)))
      (unwind-protect
          (progn
            (when (jupyter-comm-alive-p server)
              (jupyter-comm-stop server))
            (should-not (jupyter-comm-alive-p server))
            (jupyter-comm-start server)
            (should (jupyter-comm-alive-p server))
            (unwind-protect
                (progn
                  (should (jupyter-api-get-kernel server id))
                  (ert-info ("Connecting kernel comm to server")
                    (let ((kcomm (jupyter-server-kernel-comm
                                  :server server
                                  :kernel kernel)))
                      (should-not (jupyter-server-kernel-connected-p server id))
                      (jupyter-connect-client server kcomm)
                      (should (jupyter-server-kernel-connected-p server id))
                      (should (jupyter-comm-alive-p kcomm))
                      (jupyter-comm-stop kcomm)
                      (should-not (jupyter-comm-alive-p kcomm))
                      (should (jupyter-comm-alive-p server))))
                  (ert-info ("Connecting kernel comm starts server comm if necessary")
                    (let ((kcomm (jupyter-server-kernel-comm
                                  :server server
                                  :kernel kernel)))
                      (jupyter-comm-stop server)
                      (should-not (jupyter-comm-alive-p server))
                      (should-not (jupyter-server-kernel-connected-p server id))
                      (jupyter-comm-start kcomm)
                      (should (jupyter-comm-alive-p server))
                      (should (jupyter-server-kernel-connected-p server id))
                      (should (jupyter-comm-alive-p kcomm))
                      (jupyter-comm-stop kcomm))))
              (jupyter-comm-stop server)))
        (jupyter-api-shutdown-kernel server id)))))

(ert-deftest jupyter-server-kernel ()
  :tags '(kernel server)
  (let ((kernel (jupyter-server-kernel)))
    (should-not (slot-boundp kernel 'id))
    (should-not (jupyter-kernel-alive-p kernel))
    ;; TODO: How should this work? Pass the server as an argument?
    (should-error (jupyter-start-kernel kernel))
    (ert-info ("ID slot as a proxy for kernel liveness")
      (should-not (jupyter-kernel-alive-p kernel))
      (oset kernel id "foobar")
      ;; FIXME: There is actually nowhere in the code where the ID slot is made
      ;; unbound since the event handler of the server relies on the ID slot,
      ;; but if the kernel is shutdown the necessary communication layer
      ;; connections are removed.
      (should (jupyter-kernel-alive-p kernel)))
    (ert-info ("Force killing a server kernel isn't possible")
      (should-error (jupyter-kill-kernel kernel)))))

(ert-deftest jupyter-server-kernel-manager ()
  :tags '(server)
  (let* ((host (format "localhost:%s" (jupyter-test-ensure-notebook-server)))
         (server (jupyter-server-test-server (format "http://%s" host)))
         (kernel (jupyter-server-kernel
                  :spec (jupyter-guess-kernelspec
                         "python" (jupyter-server-kernelspecs server))))
         (manager (jupyter-server-kernel-manager
                   :server server
                   :kernel kernel)))
    (should-not (jupyter-kernel-alive-p manager))
    (jupyter-start-kernel manager)
    (unwind-protect
        (progn
          (should (jupyter-kernel-alive-p (oref manager kernel)))
          (should (jupyter-comm-alive-p (oref manager comm)))
          (should (jupyter-kernel-alive-p manager)))
      (jupyter-shutdown-kernel manager))))

(ert-deftest jupyter-server-start-new-kernel ()
  :tags '(server)
  (let* ((host (format "localhost:%s" (jupyter-test-ensure-notebook-server)))
         (server (jupyter-server-test-server (format "http://%s" host))))
    (cl-destructuring-bind (manager client)
        (jupyter-server-start-new-kernel server "python")
      (unwind-protect
          (let ((jupyter-current-client client))
            (should (jupyter-kernel-alive-p
                     (thread-first jupyter-current-client
                       (oref manager)
                       (oref kernel))))
            (should (jupyter-comm-alive-p (oref jupyter-current-client kcomm)))
            (should (eq (oref client manager) manager))
            (should (slot-boundp manager 'comm))
            (should (jupyter-comm-alive-p (oref manager comm)))
            (should (slot-boundp client 'kcomm))
            (should (eq (oref manager comm) (oref client kcomm)))
            (jupyter-comm-client-loop (oref client kcomm) c
              (should (eq c client)))
            (should (equal (jupyter-eval "1 + 1") "2")))
        (jupyter-shutdown-kernel manager)))))

(ert-deftest jupyter-run-server-repl ()
  :tags '(server)
  (let* ((host (format "localhost:%s" (jupyter-test-ensure-notebook-server)))
         (server (jupyter-server-test-server (format "http://%s" host))))
    (with-current-buffer
        (oref (jupyter-run-server-repl server "python") buffer)
      (unwind-protect
          (progn
            (should (jupyter-kernel-alive-p
                     (thread-first jupyter-current-client
                       (oref manager)
                       (oref kernel))))
            (should (jupyter-comm-alive-p (oref jupyter-current-client kcomm)))
            (should (equal (jupyter-eval "1 + 1") "2")))
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (_prompt) t))
                  ((symbol-function 'y-or-n-p)
                   (lambda (_prompt) t)))
          (kill-buffer (current-buffer)))))))

(ert-deftest jupyter-connect-server-repl ()
  :tags '(server)
  (let* ((host (format "localhost:%s" (jupyter-test-ensure-notebook-server)))
         (server (jupyter-server-test-server (format "http://%s" host)))
         (id (plist-get (jupyter-api-start-kernel server) :id)))
    (with-current-buffer
        (oref (jupyter-connect-server-repl server id) buffer)
      (unwind-protect
          (progn
            (should (jupyter-kernel-alive-p
                     (thread-first jupyter-current-client
                       (oref manager)
                       (oref kernel))))
            (should (jupyter-comm-alive-p (oref jupyter-current-client kcomm)))
            (should (equal (jupyter-eval "1 + 1") "2")))
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (_prompt) t))
                  ((symbol-function 'y-or-n-p)
                   (lambda (_prompt) t)))
          (kill-buffer (current-buffer)))))))

(ert-deftest org-babel-jupyter-server-session ()
  :tags '(server org)
  (require 'ob-jupyter)
  (let* ((url (format "http://localhost:%s" (jupyter-test-ensure-notebook-server)))
         (remote (file-remote-p (jupyter-tramp-file-name-from-url url)))
         (initiate-session
          (lambda (session &optional args)
            (erase-buffer)
            (insert (format "\
#+BEGIN_SRC jupyter-python :session %s %s
1 + 1
#+END_SRC" session (or args "")))
            (goto-char (point-min))
            (let ((params (nth 2 (org-babel-get-src-block-info))))
              (org-babel-jupyter-initiate-session
               (alist-get :session params) params)))))
    (with-temp-buffer
      (org-mode)
      (ert-info ("No session name")
        (should-error (funcall initiate-session remote)))
      (let ((server (jupyter-server-test-server url)))
        (ert-info ("Non-existent kernel")
          (should-error (funcall initiate-session (concat remote "py")
                                 ":kernel foo")))
        (ert-info ("Connect to an existing kernel")
          (let* ((id (plist-get (jupyter-api-start-kernel server) :id))
                 (session
                  (progn (sleep-for 1)
                         (funcall initiate-session
                                  (concat remote id)))))
            (unwind-protect
                (should (not (null session)))
              (cl-letf (((symbol-function 'yes-or-no-p)
                         (lambda (_prompt) t))
                        ((symbol-function 'y-or-n-p)
                         (lambda (_prompt) t)))
                (kill-buffer session)))))
        (ert-info ("Start a new kernel")
          (let ((session (funcall initiate-session
                                  (concat remote "py"))))
            (unwind-protect
                (should (not (null session)))
              (cl-letf (((symbol-function 'yes-or-no-p)
                         (lambda (_prompt) t))
                        ((symbol-function 'y-or-n-p)
                         (lambda (_prompt) t)))
                (kill-buffer session)))))))))


;;; jupyter-server-test.el ends here
