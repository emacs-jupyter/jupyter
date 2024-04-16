;;; jupyter-server.el --- Support for the Jupyter kernel servers -*- lexical-binding: t -*-

;; Copyright (C) 2019-2024 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 02 Apr 2019

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

;; Overview of implementation
;;
;; A `jupyter-server' communicates with a Jupyter kernel server (either the
;; notebook or a kernel gateway) via the Jupyter REST API.  Given the URL and
;; Websocket URL for the server, the `jupyter-server' object can launch kernels
;; using the function `jupyter-server-start-new-kernel'.  The kernelspecs
;; available on the server can be accessed by calling
;; `jupyter-kernelspecs'.
;;
;; Starting REPLs
;;
;; You can launch kernels without connecting clients to them by using
;; `jupyter-server-launch-kernel'.  To connect a REPL to a launched kernel use
;; `jupyter-connect-server-repl'.  To both launch and connect a REPL use
;; `jupyter-run-server-repl'.  All of the previous commands determine the server
;; to use by using the `jupyter-current-server' function, which see.
;;
;; Managing kernels on a server
;;
;; To get an overview of all live kernels on a server you can call
;; `jupyter-server-list-kernels'.  From the buffer displayed there are a number
;; of keys bound that enable you to manage the kernels on the server.  See
;; `jupyter-server-kernel-list-mode-map'.
;;
;; TODO: Find where it would be appropriate to call `delete-instance' on a
;;`jupyter-server' that does not have any websockets open, clients connected,
;; or HTTP connections open, or is not bound to `jupyter-current-server' in any
;; buffer.

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'jupyter-repl)
(require 'jupyter-server-kernel)

(declare-function jupyter-tramp-file-name-p "jupyter-tramp" (filename))
(declare-function jupyter-tramp-server-from-file-name "jupyter-tramp" (filename))
(declare-function jupyter-tramp-file-name-from-url "jupyter-tramp" (url))

(defgroup jupyter-server nil
  "Support for the Jupyter kernel gateway"
  :group 'jupyter)

;;; Assigning names to kernel IDs

(defvar jupyter-server-kernel-names nil
  "An alist mapping URLs to alists mapping kernel IDs to human friendly names.
For example

    \((\"http://localhost:8888\"
      (\"72d92ded-1275-440a-852f-90f655197305\" . \"thermo\"))\)

You can persist this alist across Emacs sessions using `desktop',
`savehist', or any other session persistence package.  For
example, when using `savehist' you can add the following to your
init file to persist the server names across Emacs sessions.

    \(savehist-mode\)
    \(add-to-list \='savehist-additional-variables \='jupyter-server-kernel-names\).")

(defun jupyter-server-cull-kernel-names (&optional server)
  "Ensure all names in `jupyter-server-kernel-names' map to existing kernels.
If SERVER is non-nil only check the kernels on SERVER, otherwise
check all kernels on all existing servers."
  (let ((servers (if server (list server)
                   (jupyter-gc-servers)
                   (jupyter-servers))))
    (unless server
      ;; Only remove non-existing servers when culling all kernels on all
      ;; servers.
      (let ((urls (mapcar (lambda (x) (oref x url)) servers)))
        (cl-callf2 cl-remove-if-not (lambda (x) (member (car x) urls))
                   jupyter-server-kernel-names)))
    (dolist (server servers)
      (when-let* ((names (assoc (oref server url) jupyter-server-kernel-names)))
        (setf (alist-get (oref server url)
                         jupyter-server-kernel-names nil nil #'equal)
              (cl-loop
               for kernel across (jupyter-api-get-kernel server)
               for name = (assoc (plist-get kernel :id) names)
               when name collect name))))))

(defun jupyter-server-kernel-name (server id)
  "Return the associated name of the kernel with ID on SERVER.
If there is no name associated, return nil.  See
`jupyter-server-kernel-names'."
  (cl-check-type server jupyter-server)
  (let ((kernel-names (assoc (oref server url) jupyter-server-kernel-names)))
    (cdr (assoc id kernel-names))))

(defun jupyter-server-kernel-id-from-name (server name)
  "Return the ID of the kernel that has NAME on SERVER.
If NAME does not have a kernel associated, return nil.  See
`jupyter-server-kernel-names'."
  (cl-check-type server jupyter-server)
  (jupyter-server-cull-kernel-names server)
  (let ((kernel-names (assoc (oref server url) jupyter-server-kernel-names)))
    (car (rassoc name kernel-names))))

(defun jupyter-server-name-kernel (server id name)
  "NAME the kernel with ID on SERVER.
See `jupyter-server-kernel-names'."
  (cl-check-type server jupyter-server)
  (setf (alist-get id
                   (alist-get (oref server url)
                              jupyter-server-kernel-names
                              nil nil #'equal)
                   nil nil #'equal)
        name))

(defun jupyter-server-name-client-kernel (client name)
  "For the kernel connected to CLIENT associate NAME.
CLIENT must be communicating with a `jupyter-server-kernel', the
CLIENT must be communicating with a `jupyter-server-kernel', see
`jupyter-server-kernel-names'."
  (cl-check-type client jupyter-kernel-client)
  (jupyter-kernel-action client
    (lambda (kernel)
      (pcase-let (((cl-struct jupyter-server-kernel server id) kernel))
        (jupyter-server-name-kernel server id name)))))

;;; Launching notebook processes

(defvar jupyter-notebook-procs nil)

(defvar jupyter-default-notebook-port 8888)

(defun jupyter-port-available-p (port)
  "Return non-nil if PORT is available."
  (let ((proc
         (condition-case nil
             (make-network-process
              :name "jupyter-port-available-p"
              :server t
              :host "127.0.0.1"
              :service port)
           (file-error nil))))
    (when proc
      (prog1 t
        (delete-process proc)))))

(defun jupyter-launch-notebook (&optional port authentication)
  "Launch a Jupyter notebook on PORT with AUTHENTICATION.
If PORT is nil, launch the notebook on the
`jupyter-default-notebook-port' if available.  Launch the
notebook on a random port otherwise.  Return the actual port
used.

If AUTHENTICATION is t, use the default, token, authentication of
a Jupyter notebook.  If AUTHENTICATION is a string, it is
interpreted as the password to the notebook.  Any other value of
AUTHENTICATION means the notebook is not authenticated."
  (let ((port (if port
                  (if (jupyter-port-available-p port)
                      port
                    (error "Port %s not available" port))
                (if (jupyter-port-available-p jupyter-default-notebook-port)
                    jupyter-default-notebook-port
                  (car (jupyter-available-local-ports 1))))))
    (prog1 port
      (let ((buffer (generate-new-buffer "*jupyter-notebook*"))
            (args (append
                   (list "notebook" "--no-browser" "--debug"
                         (format "--NotebookApp.port=%s" port))
                   (cond
                    ((eq authentication t)
                     (list))
                    ((stringp authentication)
                     (list
                      "--NotebookApp.token=''"
                      (format "--NotebookApp.password='%s'"
                              authentication)))
                    (t
                     (list
                      "--NotebookApp.token=''"
                      "--NotebookApp.password=''"))))))
        (setq jupyter-notebook-procs
              (cl-loop for (port . proc) in jupyter-notebook-procs
                       if (process-live-p proc) collect (cons port proc)))
        (push
         (cons port
               (apply #'start-file-process
                      "jupyter-notebook" buffer "jupyter" args))
         jupyter-notebook-procs)
        (with-current-buffer buffer
          (jupyter-with-timeout ((format "Launching notebook process on port %s..." port) 5)
            (save-excursion
              (goto-char (point-min))
              (re-search-forward "Jupyter Notebook.+running at:$" nil t))))))))

(defun jupyter-notebook-process (server)
  "Return a process object for the notebook associated with SERVER.
Return nil if the associated notebook process was not launched by
Emacs."
  (let ((url (url-generic-parse-url (oref server url))))
    (cdr (assoc (url-port url) jupyter-notebook-procs))))

;;; Helpers for commands

(defun jupyter-completing-read-server-kernel (server)
  "Use `completing-read' to select a kernel on SERVER.
A model of the kernel is returned as a property list and has at
least the following keys:

- :id :: The ID used to identify the kernel on the server
- :last_activity :: The last channel activity of the kernel
- :name :: The kernelspec name used to start the kernel
- :execution_state :: The status of the kernel
- :connections :: The number of websocket connections for the kernel"
  (let* ((kernels (jupyter-api-get-kernel server))
         (display-names
          (if (null kernels) (error "No kernels @ %s" (oref server url))
            (mapcar (lambda (k)
                 (cl-destructuring-bind
                     (&key name id last_activity &allow-other-keys) k
                   (concat name " (last activity: " last_activity ", id: " id ")")))
               kernels)))
         (name (completing-read "kernel: " display-names nil t)))
    (when (equal name "")
      (error "No kernel selected"))
    (nth (- (length display-names)
            (length (member name display-names)))
         (append kernels nil))))

(define-error 'jupyter-server-non-existent
  "The server doesn't exist")

(defun jupyter-current-server (&optional ask)
  "Return an existing `jupyter-server' or ASK for a new one.
If ASK is non-nil, always ask for a URL and return the
`jupyter-server' object corresponding to it.  If no Jupyter server
at URL exists, `signal' a `jupyter-server-non-existent' error
with error data being URL.

If the buffer local value of `jupyter-current-server' is non-nil,
return its value.  If `jupyter-current-server' is nil and the
`jupyter-current-client' is communicating with a kernel behind a
kernel server, return the `jupyter-server' managing the
connection.

If `jupyter-current-client' is nil or not communicating with a
kernel behind a server and if `default-directory' is a Jupyter
remote file name, return the `jupyter-server' object
corresponding to that connection.

If all of the above fails, either return the most recently used
`jupyter-server' object if there is one or ask for one based off
a URL."
  (interactive "P")
  (let ((read-url-make-server
         (lambda ()
           ;; From the list of available server
           ;; (if (> (length jupyter--servers) 1)
           ;;     (let ((server (cdr (completing-read
           ;;                         "Jupyter Server: "
           ;;                         (mapcar (lambda (x) (cons (oref x url) x))
           ;;                            jupyter--servers)))))
           ;;   )
           (jupyter-gc-servers)
           (let* ((url (read-string "Server URL: " "http://localhost:8888"))
                  (ws-url (read-string "Websocket URL: "
                                       (let ((u (url-generic-parse-url url)))
                                         (setf (url-type u) "ws")
                                         (url-recreate-url u)))))
             (let ((server (jupyter-server :url url :ws-url ws-url)))
               (if (jupyter-api-server-exists-p server) server
                 (delete-instance server)
                 (signal 'jupyter-server-non-existent (list url))))))))
    (let ((server
           (if ask (funcall read-url-make-server)
             (cond
              (jupyter-current-server)
              ;; Server of the current kernel client
              ((and jupyter-current-client
                    (jupyter-kernel-action
                        jupyter-current-client
                      (lambda (kernel)
                        (and (jupyter-server-kernel-p kernel)
                             (jupyter-server-kernel-server kernel))))))
              ;; Server of the current TRAMP remote context
              ((and (file-remote-p default-directory)
                    (jupyter-tramp-file-name-p default-directory)
                    (jupyter-tramp-server-from-file-name default-directory)))
              ;; Most recently accessed
              (t
               (or (car jupyter--servers)
                   (funcall read-url-make-server)))))))
      (prog1 server
        (setq jupyter--servers
              (cons server (delq server jupyter--servers)))))))

;;; Commands

;;;###autoload
(defun jupyter-server-launch-kernel (server)
  "Start a kernel on SERVER.

With a prefix argument, ask to select a server if there are
mutiple to choose from, otherwise the most recently used server
is used as determined by `jupyter-current-server'."
  (interactive (list (jupyter-current-server current-prefix-arg)))
  (let* ((specs (jupyter-kernelspecs server))
         (spec (jupyter-completing-read-kernelspec specs)))
    (jupyter-api-start-kernel server (jupyter-kernelspec-name spec))))

;;; REPL

;; TODO: When closing the REPL buffer and it is the last connected client as
;; shown by the :connections key of a `jupyter-api-get-kernel' call, ask to
;; also shutdown the kernel.
(defun jupyter-server-repl (kernel &optional repl-name associate-buffer client-class display)
  (or client-class (setq client-class 'jupyter-repl-client))
  (jupyter-error-if-not-client-class-p client-class 'jupyter-repl-client)
  (jupyter-bootstrap-repl
   (jupyter-client kernel client-class)
   repl-name associate-buffer display))

;;;###autoload
(defun jupyter-run-server-repl
    (server kernel-name &optional repl-name associate-buffer client-class display)
  "On SERVER start a kernel with KERNEL-NAME.

With a prefix argument, ask to select a server if there are
mutiple to choose from, otherwise the most recently used server
is used as determined by `jupyter-current-server'.

REPL-NAME, ASSOCIATE-BUFFER, CLIENT-CLASS, and DISPLAY all have
the same meaning as in `jupyter-run-repl'."
  (interactive
   (let ((server (jupyter-current-server current-prefix-arg)))
     (list server
           (jupyter-completing-read-kernelspec
            (jupyter-kernelspecs server))
           ;; FIXME: Ambiguity with `jupyter-current-server' and
           ;; `current-prefix-arg'
           (when (and current-prefix-arg
                      (y-or-n-p "Name REPL? "))
             (read-string "REPL Name: "))
           t nil t)))
  (jupyter-server-repl
   (jupyter-kernel :server server :spec kernel-name)
   repl-name associate-buffer client-class display))

;;;###autoload
(defun jupyter-connect-server-repl
    (server kernel-id &optional repl-name associate-buffer client-class display)
  "On SERVER, connect to the kernel with KERNEL-ID.

With a prefix argument, ask to select a server if there are
mutiple to choose from, otherwise the most recently used server
is used as determined by `jupyter-current-server'.

REPL-NAME, ASSOCIATE-BUFFER, CLIENT-CLASS, and DISPLAY all have
the same meaning as in `jupyter-connect-repl'."
  (interactive
   (let ((server (jupyter-current-server current-prefix-arg)))
     (list server
           (completing-read
            "Kernel ID: "
            (mapcar (lambda (kernel)
                 (cl-destructuring-bind (&key id &allow-other-keys)
                     kernel
                   (or (jupyter-server-kernel-name server id) id)))
               (jupyter-api-get-kernel server)))
           ;; FIXME: Ambiguity with `jupyter-current-server' and
           ;; `current-prefix-arg'
           (when (and current-prefix-arg
                      (y-or-n-p "Name REPL? "))
             (read-string "REPL Name: "))
           t nil t)))
  (jupyter-server-repl
   (jupyter-kernel
    :server server
    :id (or (jupyter-server-kernel-id-from-name server kernel-id)
            kernel-id))
   repl-name associate-buffer client-class display))

;;; `jupyter-server-kernel-list'

(defun jupyter-server-kernel-list-do-shutdown ()
  "Shutdown the kernel corresponding to the current entry."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (really (yes-or-no-p
                       (format "Really shutdown %s kernel? "
                               (aref (tabulated-list-get-entry) 0)))))
    (jupyter-api-shutdown-kernel jupyter-current-server id)
    (tabulated-list-delete-entry)))

(defun jupyter-server-kernel-list-do-restart ()
  "Restart the kernel corresponding to the current entry."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (really (yes-or-no-p "Really restart kernel? ")))
    (jupyter-api-restart-kernel jupyter-current-server id)
    (revert-buffer)))

(defun jupyter-server-kernel-list-do-interrupt ()
  "Interrupt the kernel corresponding to the current entry."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (jupyter-api-interrupt-kernel jupyter-current-server id)
    (revert-buffer)))

(defun jupyter-server-kernel-list-new-repl ()
  "Connect a REPL to the kernel corresponding to the current entry."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (let ((jupyter-current-client
           (jupyter-server-repl
            (jupyter-kernel
             :server jupyter-current-server
             :id id))))
      (revert-buffer)
      (jupyter-repl-pop-to-buffer))))

(defun jupyter-server-kernel-list-launch-kernel ()
  "Launch a new kernel on the server."
  (interactive)
  (jupyter-server-launch-kernel jupyter-current-server)
  (revert-buffer))

(defun jupyter-server-kernel-list-name-kernel ()
  "Name the kernel under `point'."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (name (read-string
                     (let ((cname (jupyter-server-kernel-name
                                   jupyter-current-server id)))
                       (if cname (format "Rename %s to: " cname)
                         (format "Name kernel [%s]: " id))))))
    (when (zerop (length name))
      (jupyter-server-kernel-list-name-kernel))
    (jupyter-server-name-kernel jupyter-current-server id name)
    (revert-buffer)))

(defvar jupyter-server-kernel-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-i") #'jupyter-server-kernel-list-do-interrupt)
    (define-key map (kbd "d") #'jupyter-server-kernel-list-do-shutdown)
    (define-key map (kbd "C-c C-d") #'jupyter-server-kernel-list-do-shutdown)
    (define-key map (kbd "C-c C-r") #'jupyter-server-kernel-list-do-restart)
    (define-key map [follow-link] nil) ;; allows mouse-1 to be activated
    (define-key map [mouse-1] #'jupyter-server-kernel-list-new-repl)
    (define-key map (kbd "RET") #'jupyter-server-kernel-list-new-repl)
    (define-key map (kbd "C-RET") #'jupyter-server-kernel-list-launch-kernel)
    (define-key map (kbd "C-<return>") #'jupyter-server-kernel-list-launch-kernel)
    (define-key map (kbd "<return>") #'jupyter-server-kernel-list-new-repl)
    (define-key map "R" #'jupyter-server-kernel-list-name-kernel)
    (define-key map "r" #'revert-buffer)
    (define-key map "g" #'revert-buffer)
    map))

(define-derived-mode jupyter-server-kernel-list-mode
  tabulated-list-mode "Jupyter Server Kernels"
  "A list of live kernels on a Jupyter kernel server."
  (tabulated-list-init-header)
  (tabulated-list-print)
  (let ((inhibit-read-only t)
        (url (oref jupyter-current-server url)))
    (overlay-put
     (make-overlay 1 2)
     'before-string
     (concat (propertize url 'face '(fixed-pitch default)) "\n")))
  ;; So that `dired-jump' will visit the directory of the kernel server.
  (setq default-directory
        (jupyter-tramp-file-name-from-url
         (oref jupyter-current-server url))))

(defun jupyter-server--kernel-list-format ()
  (let* ((get-time
          (lambda (a)
            (or (get-text-property 0 'jupyter-time a)
                (let ((time (jupyter-decode-time a)))
                  (prog1 time
                    (put-text-property 0 1 'jupyter-time time a))))))
         (time-sort
          (lambda (a b)
            (time-less-p
             (funcall get-time (aref (nth 1 a) 2))
             (funcall get-time (aref (nth 1 b) 2)))))
         (conn-sort
          (lambda (a b)
            (< (string-to-number (aref (nth 1 a) 4))
               (string-to-number (aref (nth 1 b) 4))))))
    `[("Name" 17 t)
      ("ID" 38 nil)
      ("Activity" 20 ,time-sort)
      ("State" 10 nil)
      ("Conns." 6 ,conn-sort)]))

(defun jupyter-server--kernel-list-entries ()
  (cl-loop
   with names = nil
   for kernel across (jupyter-api-get-kernel jupyter-current-server)
   collect
   (cl-destructuring-bind
       (&key name id last_activity execution_state
             connections &allow-other-keys)
       kernel
     (let* ((time (jupyter-decode-time last_activity))
            (name (propertize
                   (or (jupyter-server-kernel-name jupyter-current-server id)
                       (let ((same (cl-remove-if-not
                                    (lambda (x) (string-prefix-p name x)) names)))
                         (when same (setq name (format "%s<%d>" name (length same))))
                         (push name names)
                         name))
                   'face 'font-lock-constant-face))
            (activity (propertize (jupyter-format-time-low-res time)
                                  'face 'font-lock-doc-face
                                  'jupyter-time time))
            (conns (propertize (number-to-string connections)
                               'face 'shadow))
            (state (propertize execution_state
                               'face (pcase execution_state
                                       ("busy" 'warning)
                                       ("idle" 'shadow)
                                       ("starting" 'success)))))
       (list id (vector name id activity state conns))))))

;;;###autoload
(defun jupyter-server-list-kernels (server)
  "Display a list of live kernels on SERVER.
When called interactively, ask to select a SERVER when given a
prefix argument otherwise the `jupyter-current-server' will be
used."
  (interactive (list (jupyter-current-server current-prefix-arg)))
  (if (zerop (length (jupyter-api-get-kernel server)))
      (when (yes-or-no-p (format "No kernels at %s; launch one? "
                                 (oref server url)))
        (jupyter-server-launch-kernel server)
        (jupyter-server-list-kernels server))
    (with-current-buffer
        (jupyter-get-buffer-create (format "kernels[%s]" (oref server url)))
      (setq jupyter-current-server server)
      (if (eq major-mode 'jupyter-server-kernel-list-mode)
          (revert-buffer)
        (setq tabulated-list-format (jupyter-server--kernel-list-format)
              tabulated-list-entries #'jupyter-server--kernel-list-entries
              tabulated-list-sort-key (cons "Activity" t))
        (jupyter-server-kernel-list-mode)
        ;; So that `dired-jump' will visit the directory of the kernel server.
        (setq default-directory
              (jupyter-tramp-file-name-from-url (oref server url))))
      (jupyter-display-current-buffer-reuse-window))))

(provide 'jupyter-server)

;;; jupyter-server.el ends here
