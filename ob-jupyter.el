;;; ob-jupyter.el --- Jupyter integration with org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 21 Jan 2018

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

;; Interact with a Jupyter kernel via `org-mode' src-block's.

;;; Code:

(defgroup ob-jupyter nil
  "Jupyter integration with org-mode"
  :group 'org-babel)

(require 'jupyter-env)
(require 'jupyter-kernelspec)
(require 'jupyter-org-client)
(require 'jupyter-org-extensions)
(eval-when-compile
  (require 'jupyter-repl) ; For `jupyter-with-repl-buffer'
  (require 'subr-x))

(declare-function org-in-src-block-p "org" (&optional inside))
(declare-function org-element-at-point "org-element")
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-context "org-element" (&optional element))
(declare-function org-babel-variable-assignments:python "ob-python" (params))
(declare-function org-babel-expand-body:generic "ob-core" (body params &optional var-lines))
(declare-function org-export-derived-backend-p "ox" (backend &rest backends))

(declare-function jupyter-run-server-repl "jupyter-server")
(declare-function jupyter-connect-server-repl "jupyter-server")
(declare-function jupyter-server-kernelspecs "jupyter-server")
(declare-function jupyter-server-kernel-id-from-name "jupyter-server")
(declare-function jupyter-server-name-client-kernel "jupyter-server")
(declare-function jupyter-api-get-kernel "jupyter-rest-api")

(declare-function jupyter-tramp-url-from-file-name "jupyter-tramp")
(declare-function jupyter-tramp-server-from-file-name "jupyter-tramp")
(declare-function jupyter-tramp-file-name-p "jupyter-tramp")

(defvaralias 'org-babel-jupyter-resource-directory
  'jupyter-org-resource-directory)

(defvar org-babel-jupyter-session-clients (make-hash-table :test #'equal)
  "A hash table mapping session names to Jupyter clients.
`org-babel-jupyter-src-block-session' returns a key into this
table for the source block at `point'.")

(defvar org-babel-header-args:jupyter '((kernel . :any)
                                        (async . ((yes no))))
  "Available header arguments for Jupter src-blocks.")

(defvar org-babel-default-header-args:jupyter '((:kernel . "python")
                                                (:async . "no"))
  "Default header arguments for Jupyter src-blocks.")

;;; Helper functions

(defun org-babel-jupyter--src-block-kernel-language ()
  (when (org-in-src-block-p)
    (let ((info (org-babel-get-src-block-info)))
      (save-match-data
        (string-match "^jupyter-\\(.+\\)$" (car info))
        (match-string 1 (car info))))))

(defun org-babel-jupyter-language-p (lang)
  "Return non-nil if LANG src-blocks are executed using Jupyter."
  (or (string-prefix-p "jupyter-" lang)
      ;; Check if the language has been overridden, see
      ;; `org-babel-jupyter-override-src-block'
      (advice-member-p
       'ob-jupyter (intern (concat "org-babel-execute:" lang)))))

(defun org-babel-jupyter-session-key (params)
  "Return a string that is the concatenation of the :session and :kernel PARAMS.
PARAMS is the arguments alist as returned by
`org-babel-get-src-block-info'.  The returned string can then be
used to identify unique Jupyter Org babel sessions."
  (let ((session (alist-get :session params))
        (kernel (alist-get :kernel params)))
    (unless (and session kernel
                 (not (equal session "none")))
      (error "Need a valid session and a kernel to form a key"))
    (concat session "-" kernel)))

(defun org-babel-jupyter-src-block-session ()
  "Return the session key for the current Jupyter source block.
Return nil if the current source block is not a Jupyter block or
if there is no source block at point."
  (let ((info (or (and (org-in-src-block-p)
                       (org-babel-get-src-block-info 'light))
                  (org-babel-lob-get-info))))
    (when info
      (cl-destructuring-bind (lang _ params . rest) info
        (when (org-babel-jupyter-language-p lang)
          (org-babel-jupyter-session-key params))))))

;;; `ob' integration

(defun org-babel-variable-assignments:jupyter (params &optional lang)
  "Assign variables in PARAMS according to the Jupyter kernel language.
LANG is the kernel language of the source block.  If LANG is nil,
get the kernel language from the current source block.

The variables are assigned by looking for the function
`org-babel-variable-assignments:LANG'.  If this function does not
exist or if LANG cannot be determined, assign variables using
`org-babel-variable-assignments:python'."
  (or lang (setq lang (org-babel-jupyter--src-block-kernel-language)))
  (let ((fun (when lang
               (intern (format "org-babel-variable-assignments:%s" lang)))))
    (if (functionp fun) (funcall fun params)
      (require 'ob-python)
      (org-babel-variable-assignments:python params))))

(cl-defgeneric org-babel-jupyter-transform-code (code _changelist)
  "Transform CODE according to CHANGELIST, return the transformed CODE.
CHANGELIST is a property list containing the requested changes.  The default
implementation returns CODE unchanged.

This is useful for kernel languages to extend using the
jupyter-lang method specializer, e.g. to return new code to change
directories before evaluating CODE.

See `org-babel-expand-body:jupyter' for possible changes that can
be in CHANGELIST."
  code)

(defun org-babel-expand-body:jupyter (body params &optional var-lines lang)
  "Expand BODY according to PARAMS.

BODY is the code to expand, PARAMS should be the header arguments
of the src block with BODY as its code, and VAR-LINES should be
the list of strings containing the variables to evaluate before
executing body.  LANG is the kernel language of the source block.

This function is similar to
`org-babel-variable-assignments:jupyter' in that it attempts to
find the kernel language of the source block if LANG is not
provided.

BODY is expanded by calling the function
`org-babel-expand-body:LANG'.  If this function doesn't exist or
if LANG cannot be determined, fall back to
`org-babel-expand-body:generic'.

If PARAMS has a :dir parameter, the expanded code is passed to
`org-babel-jupyter-transform-code' with a changelist that
includes the :dir parameter with the directory being an absolute
path."
  (or lang (setq lang (org-babel-jupyter--src-block-kernel-language)))
  (let* ((expander (when lang
                     (intern (format "org-babel-expand-body:%s" lang))))
         (expanded (if (functionp expander)
                       (funcall expander body params)
                     (org-babel-expand-body:generic body params var-lines)))
         (changelist nil))
    (when-let* ((dir (alist-get :dir params)))
      (setq changelist (plist-put changelist :dir (expand-file-name dir))))
    (if changelist (org-babel-jupyter-transform-code expanded changelist)
      expanded)))

(defun org-babel-edit-prep:jupyter (info)
  "Prepare the edit buffer according to INFO.
Enable `jupyter-repl-interaction-mode' in the edit buffer
associated with the session found in INFO.

If the session is a Jupyter TRAMP file name, the
`default-directory' of the edit buffer is set to the root
directory the notebook serves."
  (let* ((params (nth 2 info))
         (session (alist-get :session params))
         (client-buffer (org-babel-jupyter-initiate-session session params)))
    (jupyter-repl-associate-buffer client-buffer)
    (when (jupyter-tramp-file-name-p session)
      (setq default-directory (concat (file-remote-p session) "/")))))

(defun org-babel-jupyter--insert-variable-assignments (params)
  "Insert variable assignment lines from PARAMS into the `current-buffer'.
Return non-nil if there are variable assignments, otherwise
return nil."
  (let ((var-lines (org-babel-variable-assignments:jupyter params)))
    (prog1 var-lines
      (jupyter-repl-replace-cell-code (mapconcat #'identity var-lines "\n")))))

(defun org-babel-prep-session:jupyter (session params)
  "Prepare a Jupyter SESSION according to PARAMS."
  (with-current-buffer (org-babel-jupyter-initiate-session session params)
    (goto-char (point-max))
    (and (org-babel-jupyter--insert-variable-assignments params)
         (jupyter-send-execute-request jupyter-current-client))
    (current-buffer)))

(defun org-babel-load-session:jupyter (session body params)
  "In a Jupyter SESSION, load BODY according to PARAMS."
  (save-window-excursion
    (with-current-buffer (org-babel-jupyter-initiate-session session params)
      (goto-char (point-max))
      (when (org-babel-jupyter--insert-variable-assignments params)
        (insert "\n"))
      (insert (org-babel-expand-body:jupyter (org-babel-chomp body) params))
      (current-buffer))))

;;;; Initializing session clients

(cl-defstruct (org-babel-jupyter-session
               (:constructor org-babel-jupyter-session))
  name)

(cl-defstruct (org-babel-jupyter-remote-session
               (:include org-babel-jupyter-session)
               (:constructor org-babel-jupyter-remote-session))
  connect-repl-p)

(cl-defgeneric org-babel-jupyter-parse-session ((session string))
  "Return a parsed representation of SESSION."
  (org-babel-jupyter-session :name session))

(cl-defgeneric org-babel-jupyter-initiate-client ((_session org-babel-jupyter-session) kernel)
  "Launch SESSION's KERNEL, return a `jupyter-org-client' connected to it.
SESSION is the :session header argument of a source block and
KERNEL is the name of the kernel to launch."
  (jupyter-run-repl kernel nil nil 'jupyter-org-client))

(cl-defmethod org-babel-jupyter-initiate-client :around (session _kernel)
  "Rename the returned client's REPL buffer to include SESSION's name.
Also set `jupyter-include-other-output' to nil for the session so
that output produced by other clients do not get handled by the
client."
  (let ((client (cl-call-next-method)))
    (prog1 client
      (jupyter-set client 'jupyter-include-other-output nil)
      ;; Append the name of SESSION to the initiated client REPL's
      ;; `buffer-name'.
      (jupyter-with-repl-buffer client
        (let ((name (buffer-name)))
          (when (string-match "^\\*\\(.+\\)\\*" name)
            (rename-buffer
             (concat "*" (match-string 1 name) "-"
                     (org-babel-jupyter-session-name session)
                     "*")
             'unique)))))))

(cl-defmethod org-babel-jupyter-parse-session :extra "remote" ((session string))
  "If SESSION is a remote file name, return a `org-babel-jupyter-remote-session'.
A `org-babel-jupyter-remote-session' session is also returned if
SESSION ends in \".json\", regardless of SESSION being a remote
file name, with `org-babel-jupyter-remote-session-connect-repl-p'
set to nil.  The CONNECT-REPL-P slot indicates that a connection
file is read to connect to the session, as oppossed to launcing a
kernel."
  (let ((json-p (string-suffix-p ".json" session)))
    (if (or json-p (file-remote-p session))
        (org-babel-jupyter-remote-session
         :name session
         :connect-repl-p json-p)
      (cl-call-next-method))))

(cl-defmethod org-babel-jupyter-initiate-client :before ((session org-babel-jupyter-remote-session) _kernel)
  "Raise an error if SESSION's name is a remote file name without a local name.
The local name is used as a unique identifier of a remote
session."
  (unless (not (zerop (length (file-local-name
                               (org-babel-jupyter-session-name session)))))
    (error "No remote session name")))

(cl-defmethod org-babel-jupyter-initiate-client ((session org-babel-jupyter-remote-session) kernel)
  "Initiate a client connected to a remote kernel process."
  (pcase-let (((cl-struct org-babel-jupyter-remote-session name connect-repl-p) session))
    (if connect-repl-p
        (jupyter-connect-repl name nil nil 'jupyter-org-client)
      (let ((default-directory (file-remote-p name)))
        (org-babel-jupyter-aliases-from-kernelspecs)
        (jupyter-run-repl kernel nil nil 'jupyter-org-client)))))

(require 'jupyter-server)
(require 'jupyter-tramp)

(cl-defstruct (org-babel-jupyter-server-session
               (:include org-babel-jupyter-remote-session)
               (:constructor org-babel-jupyter-server-session)))

(cl-defmethod org-babel-jupyter-parse-session :extra "server" ((session string))
  "If SESSION is a Jupyter TRAMP file name return a
`org-babel-jupyter-server-session'."
  (if (jupyter-tramp-file-name-p session)
      (org-babel-jupyter-server-session :name session)
    (cl-call-next-method)))

(cl-defmethod org-babel-jupyter-initiate-client ((session org-babel-jupyter-server-session) kernel)
  (let* ((rsession (org-babel-jupyter-session-name session))
         (url (jupyter-tramp-url-from-file-name rsession))
         (server (jupyter-server :url url)))
    (unless (jupyter-server-has-kernelspec-p server kernel)
      (error "No kernelspec matching \"%s\" exists at %s" kernel url))
    ;; Language aliases may not exist for the kernels that are accessible on
    ;; the server so ensure they do.
    (org-babel-jupyter-aliases-from-kernelspecs
     nil (jupyter-server-kernelspecs server))
    (let ((sname (file-local-name rsession)))
      (if-let ((id (jupyter-server-kernel-id-from-name server sname)))
          ;; Connecting to an existing kernel
          (cl-destructuring-bind (&key name id &allow-other-keys)
              (or (ignore-errors (jupyter-api-get-kernel server id))
                  (error "Kernel ID, %s, no longer references a kernel at %s"
                         id (oref server url)))
            (unless (string-match-p kernel name)
              (error "\":kernel %s\" doesn't match \"%s\"" kernel name))
            (jupyter-connect-server-repl server id nil nil 'jupyter-org-client))
        ;; Start a new kernel
        (let ((client (jupyter-run-server-repl
                       server kernel nil nil 'jupyter-org-client)))
          (prog1 client
            ;; TODO: If a kernel gets renamed in the future it doesn't affect
            ;; any source block :session associations because the hash of the
            ;; session name used here is already stored in the
            ;; `org-babel-jupyter-session-clients' variable.  Should that
            ;; variable be updated on a kernel rename?
            ;;
            ;; TODO: Would we always want to do this?
            (jupyter-server-name-client-kernel client sname)))))))

(defun org-babel-jupyter-initiate-session-by-key (session params)
  "Return the Jupyter REPL buffer for SESSION.
If SESSION does not have a client already, one is created based
on SESSION and PARAMS.  If SESSION ends with \".json\" then
SESSION is interpreted as a kernel connection file and a new
kernel connected to SESSION is created.

Otherwise a kernel is started based on the `:kernel' parameter in
PARAMS which should be either a valid kernel name or a prefix of
one, in which case the first kernel that matches the prefix will
be used.

If SESSION is a remote file name, like /ssh:ec2:jl, then the
kernel starts on the remote host /ssh:ec2: with a session name of
jl.  The remote host must have jupyter installed since the
\"jupyter kernel\" command will be used to start the kernel on
the host."
  (let* ((key (org-babel-jupyter-session-key params))
         (client (gethash key org-babel-jupyter-session-clients)))
    (unless client
      (setq client (org-babel-jupyter-initiate-client
                    (org-babel-jupyter-parse-session session)
                    (alist-get :kernel params)))
      (puthash key client org-babel-jupyter-session-clients)
      (jupyter-with-repl-buffer client
        (let ((forget-client (lambda () (remhash key org-babel-jupyter-session-clients))))
          (add-hook 'kill-buffer-hook forget-client nil t))))
    (oref client buffer)))

(defun org-babel-jupyter-initiate-session (&optional session params)
  "Initialize a Jupyter SESSION according to PARAMS."
  (if (equal session "none") (error "Need a session to run")
    (org-babel-jupyter-initiate-session-by-key session params)))

;;;;  `org-babel-execute:jupyter'

;;;###autoload
(defun org-babel-jupyter-scratch-buffer ()
  "Display a scratch buffer connected to the current block's session."
  (interactive)
  (let (buffer)
    (org-babel-do-in-edit-buffer
     (setq buffer (save-window-excursion
                    (jupyter-repl-scratch-buffer))))
    (if buffer (pop-to-buffer buffer)
      (user-error "No source block at point"))))

(defvar org-bracket-link-regexp)

(defun org-babel-jupyter-cleanup-file-links ()
  "Delete the files of image links for the current source block result.
Do this only if the file exists in
`org-babel-jupyter-resource-directory'."
  (when-let*
      ((pos (org-babel-where-is-src-block-result))
       (link-re (format "^[ \t]*%s[ \t]*$" org-bracket-link-regexp))
       (resource-dir (expand-file-name org-babel-jupyter-resource-directory)))
    (save-excursion
      (goto-char pos)
      (forward-line)
      (let ((bound (org-babel-result-end)))
        ;; This assumes that `jupyter-org-client' only emits bracketed links as
        ;; images
        (while (re-search-forward link-re bound t)
          (when-let*
              ((path (org-element-property :path (org-element-context)))
               (dir (when (file-name-directory path)
                      (expand-file-name (file-name-directory path)))))
            (when (and (equal dir resource-dir)
                       (file-exists-p path))
              (delete-file path))))))))

;; TODO: What is a better way to handle discrepancies between how `org-mode'
;; views header arguments and how `emacs-jupyter' views them? Should the
;; strategy be to always try to emulate the `org-mode' behavior?
(defun org-babel-jupyter--remove-file-param (params)
  "Destructively remove the file result parameter from PARAMS.
These parameters are handled internally."
  (let* ((result-params (assq :result-params params))
         (fresult (member "file" result-params))
         (fparam (assq :file params)))
    (setcar fresult "")
    (delq fparam params)))

(defun org-babel-jupyter--execute (code async-p)
  (let ((req (jupyter-send-execute-request jupyter-current-client :code code)))
    `(,req
      ,(cond
        (async-p
         (when (bound-and-true-p org-export-current-backend)
           (jupyter-add-idle-sync-hook
            'org-babel-after-execute-hook req 'append))
         (if (jupyter-org-request-inline-block-p req)
             org-babel-jupyter-async-inline-results-pending-indicator
           ;; This returns the message ID of REQ as an indicator
           ;; for the pending results.
           (jupyter-org-pending-async-results req)))
        (t
         (jupyter-idle-sync req)
         (if (jupyter-org-request-inline-block-p req)
             ;; When evaluating a source block synchronously, only the
             ;; :execute-result will be in `jupyter-org-request-results' since
             ;; stream results and any displayed data will be placed in a separate
             ;; buffer.
             (car (jupyter-org-request-results req))
           ;; This returns an Org formatted string of the collected
           ;; results.
           (jupyter-org-sync-results req)))))))

(defvar org-babel-jupyter-current-src-block-params nil
  "The block parameters of the most recently executed Jupyter source block.")

(defconst org-babel-jupyter-async-inline-results-pending-indicator "???"
  "A string to disambiguate pending inline results from empty results.")

(defun org-babel-execute:jupyter (body params)
  "Execute BODY according to PARAMS.
BODY is the code to execute for the current Jupyter `:session' in
the PARAMS alist."
  (let ((result-params (assq :result-params params))
        (async-p (or (equal (alist-get :async params) "yes")
                     (plist-member params :async))))
    (when (member "replace" result-params)
      (org-babel-jupyter-cleanup-file-links))
    (let* ((org-babel-jupyter-current-src-block-params params)
           (jupyter-current-client
            (thread-first (alist-get :session params)
              (org-babel-jupyter-initiate-session params)
              (thread-last (buffer-local-value 'jupyter-current-client))))
           (lang (jupyter-kernel-language jupyter-current-client))
           (vars (org-babel-variable-assignments:jupyter params lang))
           (code (org-babel-expand-body:jupyter body params vars lang)))
      (pcase-let ((`(,req ,maybe-result)
                   (org-babel-jupyter--execute code async-p)))
        ;; KLUDGE: Remove the file result-parameter so that
        ;; `org-babel-insert-result' doesn't attempt to handle it while
        ;; async results are pending.  Do the same in the synchronous
        ;; case, but not if link or graphics are also result-parameters,
        ;; only in Org >= 9.2, since those in combination with file mean
        ;; to interpret the result as a file link, a useful meaning that
        ;; doesn't interfere with Jupyter style result insertion.
        ;;
        ;; Do this after sending the request since
        ;; `jupyter-generate-request' still needs access to the :file
        ;; parameter.
        (when (and (member "file" result-params)
                   (or async-p
                       (not (or (member "link" result-params)
                                (member "graphics" result-params)))))
          (org-babel-jupyter--remove-file-param params))
        (prog1 maybe-result
          ;; KLUDGE: Add the "raw" result parameter for non-inline
          ;; synchronous results because an Org formatted string is
          ;; already returned in that case and
          ;; `org-babel-insert-result' should not process it.
          (unless (or async-p
                      (jupyter-org-request-inline-block-p req))
            (nconc (alist-get :result-params params) (list "raw"))))))))

;;; Overriding source block languages, language aliases

(defvar org-babel-jupyter--babel-ops
  '(execute expand-body prep-session edit-prep
            variable-assignments load-session
            initiate))

(defvar org-babel-jupyter--babel-vars
  '(header-args default-header-args))

(defun org-babel-jupyter--babel-op-symbol (op lang)
  (if (eq op 'initiate)
      (intern (format "org-babel-%s-initiate-session" lang))
    (intern (format (format "org-babel-%s:%s" op lang)))))

(defun org-babel-jupyter--babel-var-symbol (var lang)
  (intern (format "org-babel-%s:%s" var lang)))

(defun org-babel-jupyter--babel-map (alias-action
                                     var-action)
  "Loop over Org babel function and variable symbols.
ALIAS-ACTION and VAR-ACTION are functions of one argument.

When ALIAS-ACTION is called, the argument will be a symbol that
represents an Org Babel operation that can be defined by a
language extension to Org Babel, e.g. 'execute.

Similarly VAR-ACTION is called with a symbol representing an Org
Babel variable that can be defined for a language,
e.g. 'header-args."
  (declare (indent 0))
  (dolist (op org-babel-jupyter--babel-ops)
    (funcall alias-action op))
  (dolist (var org-babel-jupyter--babel-vars)
    (funcall var-action var)))

(defun org-babel-jupyter-override-src-block (lang)
  "Override the built-in `org-babel' functions for LANG.
This overrides functions like `org-babel-execute:LANG' and
`org-babel-LANG-initiate-session' to use the machinery of
jupyter-LANG source blocks.

Also, set `org-babel-header-args:LANG' to the value of
`org-babel-header-args:jupyter-LANG', if the latter exists.  If
`org-babel-header-args:LANG' had a value, save it as a symbol
property of `org-babel-header-args:LANG' for restoring it later.
Do the same for `org-babel-default-header-args:LANG'."
  (org-babel-jupyter--babel-map
    (lambda (op)
      ;; Only override operations that are not related to a particular
      ;; language.
      (unless (memq op '(variable-assignments expand-body))
        (let ((lang-op
               (org-babel-jupyter--babel-op-symbol
                op lang))
              (jupyter-lang-op
               (org-babel-jupyter--babel-op-symbol
                op (format "jupyter-%s" lang))))
          ;; If a language doesn't have a function assigned, set one so it can
          ;; be overridden
          (unless (fboundp lang-op)
            (fset lang-op #'ignore))
          (advice-add lang-op :override jupyter-lang-op
                      '((name . ob-jupyter))))))
    (lambda (var)
      (let ((lang-var
             (org-babel-jupyter--babel-var-symbol
              var lang))
            (jupyter-lang-var
             (org-babel-jupyter--babel-var-symbol
              var (format "jupyter-%s" lang))))
        (when (boundp jupyter-lang-var)
          (when (boundp lang-var)
            (put lang-var 'jupyter-restore-value (symbol-value lang-var)))
          (set lang-var (copy-tree (symbol-value jupyter-lang-var))))))))

(defun org-babel-jupyter-restore-src-block (lang)
  "Restore the overridden `org-babel' functions for LANG.
This undoes everything that
`org-babel-jupyter-override-src-block' did."
  (org-babel-jupyter--babel-map
    (lambda (op)
      ;; Only override operations that are not related to a particular
      ;; language.
      (unless (memq op '(variable-assignments expand-body))
        (let ((lang-op
               (org-babel-jupyter--babel-op-symbol
                op lang))
              (jupyter-lang-op
               (org-babel-jupyter--babel-op-symbol
                op (format "jupyter-%s" lang))))
          (advice-remove lang-op jupyter-lang-op)
          ;; The function didn't have a definition, so
          ;; ensure that we restore that fact.
          (when (eq (symbol-function lang-op) #'ignore)
            (fmakunbound lang-op)))))
    (lambda (var)
      (let ((lang-var
             (org-babel-jupyter--babel-var-symbol
              var lang)))
        (when (boundp lang-var)
          (set lang-var (get lang-var 'jupyter-restore-value)))))))

(defun org-babel-jupyter-make-language-alias (kernel lang)
  "Similar to `org-babel-make-language-alias' but for Jupyter src-blocks.
KERNEL should be the name of the default kernel to use for kernel
LANG, the language of the kernel.

The Org Babel functions `org-babel-FN:jupyter-LANG', where FN is
one of execute, expand-body, prep-session, edit-prep,
variable-assignments, or load-session, are aliased to
`org-babel-FN:jupyter'.  Similarly,
`org-babel-jupyter-LANG-initiate-session' is aliased to
`org-babel-jupyter-initiate-session'.

If not already defined, the variable
`org-babel-default-header-args:jupyter-LANG' is set to the same
value as `org-babel-header-args:jupyter', which see.  The
variable `org-babel-default-header-args:jupyter-LANG' is also set
to

    \((:async . \"no\")
     \(:kernel . KERNEL))

if that variable does not already have a value.

If LANG has an association in `org-babel-tangle-lang-exts',
associate the same value with jupyter-LANG, if needed.
Similarly, associate the same value for LANG in
`org-src-lang-modes'."
  (org-babel-jupyter--babel-map
    (lambda (op)
      (defalias (org-babel-jupyter--babel-op-symbol
                 op (format "jupyter-%s" lang))
        (org-babel-jupyter--babel-op-symbol
         op "jupyter")))
    (lambda (var)
      (let ((jupyter-var
             (org-babel-jupyter--babel-var-symbol
              var "jupyter"))
            (jupyter-lang-var
             (org-babel-jupyter--babel-var-symbol
              var (format "jupyter-%s" lang))))
        (unless (boundp jupyter-lang-var)
          (set jupyter-lang-var (copy-tree (symbol-value jupyter-var)))
          (cond
           ((eq var 'default-header-args)
            ;; Needed since the default kernel is not language
            ;; specific and it needs to be.
            (setf (alist-get :kernel (symbol-value jupyter-lang-var)) kernel)
            (put jupyter-lang-var 'variable-documentation
                 (format
                  "Default header arguments for Jupyter %s src-blocks"
                  lang)))
           (t
            (put jupyter-lang-var 'variable-documentation
                 (get jupyter-var 'variable-documentation))))))))
  (when (assoc lang org-babel-tangle-lang-exts)
    (add-to-list 'org-babel-tangle-lang-exts
                 (cons (concat "jupyter-" lang)
                       (cdr (assoc lang org-babel-tangle-lang-exts)))))
  (add-to-list 'org-src-lang-modes
               (cons (concat "jupyter-" lang)
                     (or (cdr (assoc lang org-src-lang-modes))
                         (intern (downcase (replace-regexp-in-string
                                            "[0-9]*" "" lang)))))))

(defun org-babel-jupyter-aliases-from-kernelspecs (&optional refresh specs)
  "Make language aliases based on the available kernelspecs.
For all kernel SPECS, make a language alias for the kernel
language if one does not already exist.  The alias is created with
`org-babel-jupyter-make-language-alias'.

SPECS defaults to `jupyter-available-kernelspecs'.  Optional
argument REFRESH has the same meaning as in
`jupyter-available-kernelspecs'.

Note, spaces in the kernel language name are converted into
dashes in the language alias, e.g.

    Wolfram Language -> jupyter-Wolfram-Language

For convenience, after creating a language alias for a kernel
language LANG, set the :kernel default header argument if not
present in `org-babel-default-header-args:jupyter-LANG', see
`org-babel-header-args:jupyter'.  This allows users to set that
variable in their configurations without having to also set the
:kernel header argument since it is common for only one per
language to exist on someone's system."
  (cl-loop
   with specs = (or specs
                    (with-demoted-errors "Error retrieving kernelspecs: %S"
                      (jupyter-available-kernelspecs refresh)))
   for (kernel . (_dir . spec)) in specs
   for lang = (jupyter-canonicalize-language-string (plist-get spec :language))
   unless (member lang languages) collect lang into languages and
   do (org-babel-jupyter-make-language-alias kernel lang)
   ;; KLUDGE: The :kernel header argument is always set, even when we aren't
   ;; the ones who originally set the defaults.  This is here for convenience
   ;; since usually a user does not set :kernel directly.
   (let ((var (intern (concat "org-babel-default-header-args:jupyter-" lang))))
     (unless (alist-get :kernel (symbol-value var))
       (setf (alist-get :kernel (symbol-value var)) kernel)))))

;;; `ox' integration

(defvar org-latex-minted-langs)

(defun org-babel-jupyter-setup-export (backend)
  "Ensure that Jupyter src-blocks are integrated with BACKEND.
Currently this makes sure that Jupyter src-block languages are
mapped to their appropriate minted language in
`org-latex-minted-langs' if BACKEND is latex."
  (cond
   ((org-export-derived-backend-p backend 'latex)
    (cl-loop
     for (_kernel . (_dir . spec)) in (jupyter-available-kernelspecs)
     for lang = (plist-get spec :language)
     do (cl-pushnew (list (intern (concat "jupyter-" lang)) lang)
                    org-latex-minted-langs :test #'equal)))))

(defun org-babel-jupyter-strip-ansi-escapes (_backend)
  "Remove ANSI escapes from Jupyter src-block results in the current buffer."
  (org-babel-map-src-blocks nil
    (when (org-babel-jupyter-language-p lang)
      (when-let* ((pos (org-babel-where-is-src-block-result))
                  (ansi-color-apply-face-function
                   (lambda (beg end face)
                     ;; Could be useful for export backends
                     (when face
                       (put-text-property beg end 'face face)))))
        (goto-char pos)
        (ansi-color-apply-on-region (point) (org-babel-result-end))))))

;;; Hook into `org'

(org-babel-jupyter-aliases-from-kernelspecs)
(add-hook 'org-export-before-processing-hook #'org-babel-jupyter-setup-export)
(add-hook 'org-export-before-parsing-hook #'org-babel-jupyter-strip-ansi-escapes)

(provide 'ob-jupyter)

;;; ob-jupyter.el ends here
