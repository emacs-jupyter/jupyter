;;; jupyter-server-test.el --- Test communication with a notebook server -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 31 May 2019

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

(ert-deftest jupyter-api-construct-endpoint ()
  :tags '(rest)
  (let (plist)
    (ert-info ("Basic")
      (should (equal (jupyter-api-construct-endpoint plist) '("")))
      (setq plist '("foo"))
      (should (equal (jupyter-api-construct-endpoint plist) '("foo")))
      (setq plist '("foo" "bar"))
      (should (equal (jupyter-api-construct-endpoint plist) '("foo/bar")))
      (setq plist '("foo" "bar" :))
      (should (equal (jupyter-api-construct-endpoint plist) '("foo/bar" :)))
      (setq plist '("foo" "bar" 1))
      (should (equal (jupyter-api-construct-endpoint plist) '("foo/bar" 1)))
      (setq plist '("foo" "" "bar" ?1))
      (should (equal (jupyter-api-construct-endpoint plist) '("foo/bar" ?1)))
      (setq plist '("foo" "bar" "" nil))
      (should (equal (jupyter-api-construct-endpoint plist) '("foo/bar")))
      (setq plist '("foo/bar" "baz"))
      (should (equal (jupyter-api-construct-endpoint plist) '("foo/bar/baz"))))
    (ert-info ("Query parameters")
      (setq plist '("foo" "bar" ("content")))
      (should-error (jupyter-api-construct-endpoint plist))
      (setq plist '("foo" "bar" (("content" "1"))))
      (should-error (jupyter-api-construct-endpoint plist))
      (setq plist '("foo" "bar" ((:contents . "1"))))
      (should-error (jupyter-api-construct-endpoint plist))
      (setq plist '("foo" "bar" (("content" . 1))))
      (should-error (jupyter-api-construct-endpoint plist)))
    (ert-info ("Rest of plist")
      (setq plist '("foo" "bar" :token 1))
      (should (equal (jupyter-api-construct-endpoint plist) '("foo/bar" :token 1)))
      (setq plist '("foo" "bar" (("content" . "1")) :token 1))
      (should (equal (jupyter-api-construct-endpoint plist) '("foo/bar?content=1" :token 1))))))

(ert-deftest jupyter-rest-api ()
  :tags '(rest)
  (let ((client (jupyter-rest-client
                 :url "http://foo"
                 :ws-url "ws://foo"
                 :auth t)))
    (jupyter-test-rest-api-request
     (jupyter-api-request client "GET" "api" "kernels")
     (should (equal url "http://foo/api/kernels"))
     (should (equal url-request-method "GET"))
     (should (equal url-request-data nil))
     (should (equal url-request-extra-headers nil)))
    (jupyter-test-rest-api-request
     (jupyter-api-request client "POST" "api" "kernels" "ID" :name "bar")
     (should (equal url "http://foo/api/kernels/ID"))
     (should (equal url-request-method "POST"))
     (should (equal url-request-data (json-encode '(:name "bar"))))
     (should (equal (alist-get "Content-Type" url-request-extra-headers nil nil #'equal)
                    "application/json")))
    (cl-letf (((symbol-function #'websocket-open)
               (lambda (url &rest plist)
                 (should (equal url "ws://foo/api/kernels"))
                 (should (equal (plist-get plist :on-open) 'identity)))))
      (jupyter-api-request client "WS" "api" "kernels" :on-open 'identity))))

(ert-deftest jupyter-api-add-websocket-headers ()
  :tags '(rest)
  (let ((jupyter-api-request-headers
         (list (cons "Authorization" "token 111"))))
    (should (equal (jupyter-api-add-websocket-headers nil)
                   (list :custom-header-alist
                         jupyter-api-request-headers)))
    (should (equal (jupyter-api-add-websocket-headers
                    '("foo" ?a "bar" ((a . b)) aple :foo 1))
                   (list "foo" ?a "bar" '((a . b)) 'aple :foo 1
                         :custom-header-alist
                         jupyter-api-request-headers)))
    (should (equal (jupyter-api-add-websocket-headers
                    '("foo" "bar" :foo 1 :custom-header-alist (("a" . "b"))))
                   (list "foo" "bar" :foo 1
                         :custom-header-alist
                         (append '(("a" . "b"))
                                 jupyter-api-request-headers))))))

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

(defvar url-cookie-storage)
(defvar url-cookies-changed-since-last-save)

(ert-deftest jupyter-api-delete-cookies ()
  :tags '(rest)
  (cl-letf* ((url-cookie-storage
              '(("localhost:8888"
                 [url-cookie
                  "username-localhost-8888"
                  "foo"
                  "Fri, 16 Aug 9999 06:02:50 GMT"
                  "/" "localhost:8888" nil])
                ("localhost"
                 [url-cookie
                  "username-localhost-8888"
                  "foo"
                  "Fri, 16 Aug 9999 06:02:50 GMT"
                  "/" "localhost" nil])))
             (cookies-written nil)
             ((symbol-function #'url-cookie-write-file)
              (lambda ()
                (should url-cookies-changed-since-last-save)
                (setq cookies-written t))))
    (jupyter-api-delete-cookies "http://localhost:8888")
    (should-not url-cookie-storage)
    (should cookies-written)))

(ert-deftest jupyter-api-login ()
  :tags '(rest)
  (let (cookies-copied-before-write
        cookies-written
        url-cookie-storage
        (host "localhost:8888"))
    (cl-letf (((symbol-function #'read-passwd)
               (lambda (&rest _) "foo"))
              ((symbol-function #'jupyter-api-copy-cookies-for-websocket)
               (lambda (&rest _)
                 (setq cookies-copied-before-write t)))
              ((symbol-function #'jupyter-api-request-xsrf-cookie) #'ignore)
              ((symbol-function #'url-cookie-write-file)
               (lambda (&rest _)
                 (should cookies-copied-before-write)
                 (setq cookies-written t))))
      (jupyter-test-rest-api-request
       (jupyter-api-login
        (jupyter-rest-client
         :url (format "http://%s" host)))
       (should (equal url (format "http://%s/login" host)))))
    (should cookies-copied-before-write)
    (should cookies-written)))

(ert-deftest jupyter-api-get-kernel-ws ()
  :tags '(rest)
  (jupyter-test-rest-api-with-notebook client
    (cl-destructuring-bind (&key id &allow-other-keys)
        (jupyter-api-start-kernel client)
      (unwind-protect
          (let ((kernel-id id)
                (ws (jupyter-api-get-kernel-ws client id)))
            (unwind-protect
                (cl-destructuring-bind (&key id session &allow-other-keys)
                    (websocket-client-data ws)
                  (should (websocket-openp ws))
                  (should (equal id kernel-id))
                  (should (jupyter-session-p session)))
              (websocket-close ws)))
        (jupyter-api-shutdown-kernel client id)))))

(ert-deftest jupyter-api-server-accessible-p ()
  :tags '(rest)
  (jupyter-test-rest-api-with-notebook client
    (should (jupyter-api-server-accessible-p client))))

(ert-deftest jupyter-api-request-xsrf-cookie ()
  :tags '(rest)
  (jupyter-test-rest-api-with-notebook client
    (let ((url (oref client url)))
      (should-not (jupyter-api-xsrf-header-from-cookies url))
      (jupyter-api-request-xsrf-cookie client)
      (should (jupyter-api-xsrf-header-from-cookies url)))))

(ert-deftest jupyter-api-authenticate ()
  :tags '(rest)
  (cl-letf (((symbol-function #'url-cookie-write-file) #'ignore))
    (ert-info ("Password authentication")
      (let ((jupyter-test-notebook nil)
            ;; foobar
            (password "sha1:84cbf6913f79:5df10a65c1f36cdf691bb93b089f7cae0582b20e"))
        (jupyter-test-ensure-notebook-server password)
        (jupyter-test-rest-api-with-notebook client
          (oset client auth 'password)
          (unwind-protect
              (progn
                (should-not (jupyter-api-server-accessible-p client))
                (jupyter-api-authenticate client 'password (lambda () "foobar"))
                (should (eq (oref client auth) t))
                (should (jupyter-api-server-accessible-p client)))
            (when (process-live-p (car jupyter-test-notebook))
              (delete-process (car jupyter-test-notebook)))))))
    (ert-info ("Token authentication")
      (let ((jupyter-test-notebook nil) token)
        (jupyter-test-ensure-notebook-server t)
        (with-current-buffer (process-buffer (car jupyter-test-notebook))
          (while (not (re-search-forward "\\?token=\\([a-zA-Z0-9]+\\)" nil t))
            (goto-char (point-min))
            (sleep-for 0.02))
          (setq token (match-string 1)))
        (jupyter-test-rest-api-with-notebook client
          (oset client auth 'token)
          (unwind-protect
              (cl-letf (((symbol-function #'read-string)
                         (lambda (&rest _) token)))
                (should-not (jupyter-api-server-accessible-p client))
                (jupyter-api-authenticate client 'token)
                (should (jupyter-api-server-accessible-p client))
                (should (equal (list (cons "Authorization" (concat "token " token)))
                               (oref client auth)))
                (should (equal (list (cons "Authorization" (concat "token " token)))
                               (jupyter-api-auth-headers client))))
            (when (process-live-p (car jupyter-test-notebook))
              (delete-process (car jupyter-test-notebook)))))))))

;;; Server

;; And `jupyter-server-kernel-comm'
(ert-deftest jupyter-server ()
  :tags '(server)
  (jupyter-test-with-notebook server
    (ert-info ("`jupyter-comm-layer' methods")
      (when (jupyter-comm-alive-p server)
        (jupyter-comm-stop server))
      (should-not (jupyter-comm-alive-p server))
      (jupyter-comm-start server)
      (should (jupyter-comm-alive-p server)))
    (jupyter-test-with-server-kernel server "python" kernel
      (should (oref kernel id))
      (let ((id (oref kernel id)))
        (should (jupyter-api-get-kernel server id))
        (ert-info ("Connecting kernel comm to server")
          (let ((kcomm (jupyter-server-kernel-comm
                        :kernel kernel)))
            (should-not (jupyter-server-kernel-connected-p server id))
            (jupyter-comm-add-handler server kcomm)
            (should (jupyter-server-kernel-connected-p server id))
            (should (jupyter-comm-alive-p kcomm))
            (jupyter-comm-stop kcomm)
            (should-not (jupyter-comm-alive-p kcomm))
            (should (jupyter-comm-alive-p server))))
        (ert-info ("Connecting kernel comm starts server comm if necessary")
          (let ((kcomm (jupyter-server-kernel-comm
                        :kernel kernel)))
            (jupyter-comm-stop server)
            (should-not (jupyter-comm-alive-p server))
            (should-not (jupyter-server-kernel-connected-p server id))
            (jupyter-comm-start kcomm)
            (should (jupyter-comm-alive-p server))
            (should (jupyter-server-kernel-connected-p server id))
            (should (jupyter-comm-alive-p kcomm))
            (jupyter-comm-stop kcomm)))))))

(ert-deftest jupyter-server-reauthentication ()
  :tags '(server)
  (let ((jupyter-test-notebook nil)
        (jupyter-api-authentication-method 'password)
        ;; foobar
        (password "sha1:84cbf6913f79:5df10a65c1f36cdf691bb93b089f7cae0582b20e"))
    (jupyter-test-ensure-notebook-server password)
    (unwind-protect
        (jupyter-test-with-notebook server
          (cl-letf (((symbol-function #'read-passwd)
                     (lambda (&rest _) "foobar")))
            (jupyter-api-ensure-authenticated server))
          (cl-destructuring-bind (manager client)
              (jupyter-server-start-new-kernel server "python")
            (unwind-protect
                (let ((id (oref (oref manager kernel) id))
                      (jupyter-current-client client))
                  (should (equal (jupyter-eval "1 + 1") "2"))
                  (let (url-cookie-storage)
                    (should-not (jupyter-api-server-accessible-p server))
                    (ert-info ("Can't attempt re-authentication")
                      (let ((jupyter-api-authentication-method 'none))
                        (should-error (jupyter-api-get-kernel server))))
                    (should-not (jupyter-api-server-accessible-p server))
                    (ert-info ("Do re-authentication")
                      (cl-letf (((symbol-function #'read-passwd)
                                 (lambda (&rest _) "foobar")))
                        (should (jupyter-api-get-kernel server)))
                      (should (jupyter-api-server-accessible-p server))
                      (should (jupyter-server-kernel-connected-p server id)))
                    (ert-info ("Verify clients still work after re-authentication")
                      (should (equal (jupyter-eval "1 + 1") "2")))))
              (jupyter-shutdown-kernel manager))))
      (when (process-live-p (car jupyter-test-notebook))
        (delete-process (car jupyter-test-notebook))))))

(ert-deftest jupyter-server-kernel ()
  :tags '(kernel server)
  (jupyter-test-with-notebook server
    (let ((kernel (jupyter-server-kernel
                   :server server
                   :spec (jupyter-guess-kernelspec
                          "python" (jupyter-server-kernelspecs server)))))
      (should (slot-boundp kernel 'server))
      (should (eq (oref kernel server) server))
      (should-not (slot-boundp kernel 'id))
      (should-not (jupyter-kernel-alive-p kernel))
      (jupyter-start-kernel kernel)
      (should (slot-boundp kernel 'id))
      (let ((id (oref kernel id)))
        (unwind-protect
            (progn
              (should (jupyter-api-get-kernel server id))
              (should (jupyter-kernel-alive-p kernel)))
         (jupyter-api-shutdown-kernel server id))))))

(ert-deftest jupyter-server-kernel-manager ()
  :tags '(server)
  (jupyter-test-with-notebook server
    (let* ((kernel (jupyter-server-kernel
                    :server server
                    :spec (jupyter-guess-kernelspec
                           "python" (jupyter-server-kernelspecs server))))
           (manager (jupyter-server-kernel-manager
                     :kernel kernel)))
      (should-not (jupyter-kernel-alive-p manager))
      (jupyter-start-kernel manager)
      (unwind-protect
          (progn
            (should (jupyter-kernel-alive-p (oref manager kernel)))
            (should (jupyter-comm-alive-p (oref manager comm)))
            (should (jupyter-kernel-alive-p manager)))
        (jupyter-shutdown-kernel manager)))))

(ert-deftest jupyter-server-start-new-kernel ()
  :tags '(server)
  (jupyter-test-with-notebook server
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
            (jupyter-comm-handler-loop (oref client kcomm) c
              (should (eq c client)))
            (should (equal (jupyter-eval "1 + 1") "2")))
        (jupyter-shutdown-kernel manager)))))

(ert-deftest jupyter-run-server-repl ()
  :tags '(server)
  (jupyter-test-with-notebook server
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
        (jupyter-test-kill-buffer (current-buffer))))))

(ert-deftest jupyter-connect-server-repl ()
  :tags '(server)
  (jupyter-test-with-notebook server
    (let ((id (plist-get (jupyter-api-start-kernel server) :id)))
      (sleep-for 1)
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
          (jupyter-test-kill-buffer (current-buffer)))))))

(ert-deftest org-babel-jupyter-server-session ()
  :tags '(server org)
  (require 'ob-jupyter)
  (jupyter-test-with-notebook server
    (let* ((remote (file-remote-p (jupyter-tramp-file-name-from-url (oref server url))))
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
        (ert-info ("Non-existent kernel")
          (should-error (funcall initiate-session (concat remote "py") ":kernel foo")))
        (ert-info ("Connect to an existing kernel")
          (let* ((id (plist-get (jupyter-api-start-kernel server) :id))
                 (session (funcall initiate-session (concat remote id))))
            (unwind-protect
                (should (not (null session)))
              (jupyter-test-kill-buffer session))))
        (ert-info ("Start a new kernel")
          (let ((session (funcall initiate-session (concat remote "py"))))
            (unwind-protect
                (should (not (null session)))
              (jupyter-test-kill-buffer session))))))))

;;; Naming kernels

(ert-deftest jupyter-server-cull-kernel-names ()
  :tags '(server)
  (cl-letf* ((jupyter-server-kernel-names
              '(("http://localhost:89812"
                 ("id1" . "name1")
                 ("id2" . "name2"))
                ("http://localhost:89813"
                 ("id3" . "name3"))
                ("http://localhost:89814"
                 ("id4" . "name4"))))
             (servers (list (jupyter-server :url "http://localhost:89812")
                            (jupyter-server :url "http://localhost:89813")))
             ((symbol-function #'jupyter-gc-servers) #'ignore)
             ((symbol-function #'jupyter-servers) (lambda () servers))
             ((symbol-function #'jupyter-api-get-kernel)
              (lambda (server &rest _)
                (cond
                 ((equal (oref server url) "http://localhost:89812")
                  (vector '(:id "id1")))
                 ((equal (oref server url) "http://localhost:89813")
                  nil)))))
    (jupyter-server-cull-kernel-names (car servers))
    (should (equal jupyter-server-kernel-names
                   '(("http://localhost:89812"
                      ("id1" . "name1"))
                     ("http://localhost:89813"
                      ("id3" . "name3"))
                     ("http://localhost:89814"
                      ("id4" . "name4")))))
    (jupyter-server-cull-kernel-names)
    (should (equal jupyter-server-kernel-names
                   '(("http://localhost:89812"
                      ("id1" . "name1"))
                     ("http://localhost:89813"))))))

(ert-deftest jupyter-server-kernel-name ()
  :tags '(server)
  (let ((jupyter-server-kernel-names
         '(("http://localhost:8882"
            ("id1" . "name1"))))
        (server (jupyter-server :url "http://localhost:8882")))
    (should (equal (jupyter-server-kernel-name server "id1")
                   "name1"))
    (should (null (jupyter-server-kernel-name server "id2")))))

(ert-deftest jupyter-server-kernel-id-from-name ()
  :tags '(server)
  (cl-letf (((symbol-function #'jupyter-server-cull-kernel-names) #'ignore)
            (jupyter-server-kernel-names
             '(("http://localhost:8882"
                ("id1" . "name1"))))
            (server (jupyter-server :url "http://localhost:8882")))
    (should (equal (jupyter-server-kernel-id-from-name server "name1")
                   "id1"))
    (should (null (jupyter-server-kernel-id-from-name server "name2")))))

(ert-deftest jupyter-server-name-kernel ()
  :tags '(server)
  (let ((jupyter-server-kernel-names
         '(("http://localhost:8882"
            ("id1" . "name1"))))
        (server (jupyter-server :url "http://localhost:8882")))
    (jupyter-server-name-kernel server "id2" "name2")
    (should (equal jupyter-server-kernel-names
                   '(("http://localhost:8882"
                      ("id2" . "name2")
                      ("id1" . "name1")))))
    (jupyter-server-name-kernel server "id2" "name3")
    (should (equal jupyter-server-kernel-names
                   '(("http://localhost:8882"
                      ("id2" . "name3")
                      ("id1" . "name1")))))))

(ert-deftest jupyter-server-name-client-kernel ()
  :tags '(server)
  (let* ((jupyter-server-kernel-names
          '(("http://localhost:8882"
             ("id1" . "name1"))))
         (server (jupyter-server :url "http://localhost:8882"))
         (client (jupyter-kernel-client)))
    (oset client kcomm (jupyter-server-kernel-comm
                        :kernel (jupyter-server-kernel
                                 :id "id1"
                                 :server server)))
    (jupyter-server-name-client-kernel client "foo")
    (should (equal jupyter-server-kernel-names
                   '(("http://localhost:8882"
                      ("id1" . "foo")))))))

;;; jupyter-server-test.el ends here
