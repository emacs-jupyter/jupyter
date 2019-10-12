;;; ob-jupyter.el --- Jupyter integration with org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 21 Jan 2018
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

(declare-function org-element-at-point "org-element")
(declare-function org-link-set-parameters "org" (type &rest parameters))
(declare-function org-in-src-block-p "org" (&optional inside))
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-type "org-element" (element))
(declare-function org-element-context "org-element" (&optional element))
(declare-function org-babel-variable-assignments:python "ob-python" (params))
(declare-function org-babel-expand-body:generic "ob-core" (body params &optional var-lines))
(declare-function org-export-derived-backend-p "ox" (backend &rest backends))

(declare-function jupyter-server "jupyter-server")
(declare-function jupyter-find-server "jupyter-server")
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
A key into this table can be constructed for the src-block at
`point' using `org-babel-jupyter-src-block-session'.")

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
  "Return the session key based on the keys in PARAMS.
PARAMS is the arguments alist as returned by
`org-babel-get-src-block-info' and should contain a :kernel key
and a valid :session key. The session key is used to access the
clients in `org-babel-jupyter-session-clients'."
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
LANG is the kernel language of the source block. If LANG is nil,
get the kernel language from the current source block.

The variables are assigned by looking for the function
`org-babel-variable-assignments:LANG'. If this function does not
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
CHANGELIST is a property list containing the requested changes. The default
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
executing body. LANG is the kernel language of the source block.

This function is similar to
`org-babel-variable-assignments:jupyter' in that it attempts to
find the kernel language of the source block if LANG is not
provided.

BODY is expanded by calling the function
`org-babel-expand-body:LANG'. If this function doesn't exist or
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

If the session corresponds to a connection to a notebook server,
the `default-directory' is set to the root of the directory that
the notebook serves."
  (let* ((params (nth 2 info))
         (session (alist-get :session params))
         (client-buffer (org-babel-jupyter-initiate-session session params)))
    (jupyter-repl-associate-buffer client-buffer)
    (when (jupyter-tramp-file-name-p session)
      (setq default-directory (concat (file-remote-p session) "/")))))

(defun org-babel-prep-session:jupyter (session params &optional delay-eval)
  "Prepare a Jupyter SESSION according to PARAMS.
If DELAY-EVAL is non-nil, delay the evaluation of the header
variables in PARAMS."
  (let ((buffer (org-babel-jupyter-initiate-session session params))
        (var-lines (org-babel-variable-assignments:jupyter params)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (when var-lines
        (jupyter-repl-replace-cell-code
         (mapconcat #'identity var-lines "\n"))
        ;; For `org-babel-load-session:jupyter', ensure that the loaded code
        ;; starts on a new line.
        (when delay-eval
          (insert "\n")))
      (unless delay-eval
        (jupyter-send-execute-request jupyter-current-client))
      (current-buffer))))

(defun org-babel-load-session:jupyter (session body params)
  "In a Jupyter SESSION, load BODY according to PARAMS."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:jupyter session params 'delay-eval)))
      (with-current-buffer buffer
        (insert (org-babel-expand-body:jupyter (org-babel-chomp body) params))
        (current-buffer)))))

(defun org-babel-jupyter--run-repl (session kernel)
  (let ((remote (file-remote-p session)))
    (when (and remote (zerop (length (file-local-name session))))
      (error "No remote session name"))
    (let ((default-directory (or remote default-directory)))
      (when remote
        (org-babel-jupyter-aliases-from-kernelspecs))
      (jupyter-run-repl kernel nil nil 'jupyter-org-client))))

(defun org-babel-jupyter--server-repl (session kernel)
  (require 'jupyter-server)
  (let* ((url (jupyter-tramp-url-from-file-name session))
         (server (or (jupyter-tramp-server-from-file-name session)
                     (jupyter-server :url url)))
         (localname (file-local-name session))
         (name-or-id (if (null localname) (error "No remote server session name")
                       ;; If a kernel has an associated name, get its kernel ID
                       ;; otherwise return either a name to associate with a
                       ;; newly started kernel on the server or an ID of an
                       ;; existing kernel.
                       (or (jupyter-server-kernel-id-from-name server localname)
                           localname))))
    (let ((specs (jupyter-server-kernelspecs server))
          (kmodel (ignore-errors (jupyter-api-get-kernel server name-or-id))))
      ;; Language aliases may not exist for the kernels that are accessible on
      ;; the server.
      (org-babel-jupyter-aliases-from-kernelspecs nil specs)
      (unless (jupyter-guess-kernelspec kernel specs)
        (error "No kernelspec matching \"%s\" exists at %s" kernel url))
      (if kmodel
          ;; Connecting to an existing kernel
          (cl-destructuring-bind (&key name id &allow-other-keys) kmodel
            (unless (string-match-p kernel name)
              (error "\":kernel %s\" doesn't match \"%s\"" kernel name))
            (jupyter-connect-server-repl server id nil nil 'jupyter-org-client))
        ;; If the file local name of SESSION doesn't match an existing kernel,
        ;; we are starting a new one.
        (let ((client (jupyter-run-server-repl
                       server kernel nil nil 'jupyter-org-client)))
          (prog1 client
            ;; Associate a name with the newly started kernel.
            ;;
            ;; TODO: If a kernel gets renamed in the future it doesn't affect
            ;; any source block :session associations because the hash of the
            ;; session name used here is already stored in the
            ;; `org-babel-jupyter-session-clients' variable. Should that
            ;; variable be updated on a kernel rename?
            ;;
            ;; TODO: Would we always want to do this?
            (jupyter-server-name-client-kernel client name-or-id)))))))

(defun org-babel-jupyter-initiate-session-by-key (session params)
  "Return the Jupyter REPL buffer for SESSION.
If SESSION does not have a client already, one is created based
on SESSION and PARAMS. If SESSION ends with \".json\" then
SESSION is interpreted as a kernel connection file and a new
kernel connected to SESSION is created.

Otherwise a kernel is started based on the `:kernel' parameter in
PARAMS which should be either a valid kernel name or a prefix of
one, in which case the first kernel that matches the prefix will
be used.

If SESSION is a remote file name, like /ssh:ec2:jl, then the
kernel starts on the remote host /ssh:ec2: with a session name of
jl. The remote host must have jupyter installed since the
\"jupyter kernel\" command will be used to start the kernel on
the host."
  (let* ((kernel (alist-get :kernel params))
         (key (org-babel-jupyter-session-key params))
         (client (gethash key org-babel-jupyter-session-clients)))
    (unless client
      (setq client
            (cond
             ((string-suffix-p ".json" session)
              (jupyter-connect-repl (expand-file-name session) nil nil
                                    'jupyter-org-client))
             (t
              (funcall (if (and (file-remote-p session)
                                (jupyter-tramp-file-name-p session))
                           #'org-babel-jupyter--server-repl
                         #'org-babel-jupyter--run-repl)
                       session kernel))))
      (jupyter-set client 'jupyter-include-other-output nil)
      (jupyter-with-repl-buffer client
        (let ((name (buffer-name)))
          (when (string-match "^\\*\\(.+\\)\\*" name)
            (rename-buffer
             (concat "*" (match-string 1 name) "-" session "*")
             'unique)))
        (add-hook
         'kill-buffer-hook
         (lambda ()
           (remhash key org-babel-jupyter-session-clients))
         nil t))
      (puthash key client org-babel-jupyter-session-clients))
    (oref client buffer)))

(defun org-babel-jupyter-initiate-session (&optional session params)
  "Initialize a Jupyter SESSION according to PARAMS."
  (if (equal session "none") (error "Need a session to run")
    (org-babel-jupyter-initiate-session-by-key session params)))

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
  (when-let* ((result-pos (org-babel-where-is-src-block-result))
              (link-re (format "^[ \t]*%s[ \t]*$" org-bracket-link-regexp)))
    (save-excursion
      (goto-char result-pos)
      (forward-line)
      (let ((bound (org-babel-result-end)))
        ;; This assumes that `jupyter-org-client' only emits bracketed links as
        ;; images
        (while (re-search-forward link-re bound t)
          (when-let* ((link-path
                       (org-element-property :path (org-element-context)))
                      (link-dir
                       (when (file-name-directory link-path)
                         (expand-file-name (file-name-directory link-path))))
                      (resource-dir
                       (expand-file-name org-babel-jupyter-resource-directory)))
            (when (and (equal link-dir resource-dir)
                       (file-exists-p link-path))
              (delete-file link-path))))))))

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

(defvar org-babel-jupyter-current-src-block-params nil
  "The block params of the currently executed source block.
`org-mode' merges many different sources of source block
parameters that cannot be obtained by just calling
`org-babel-log-get-info' or `org-babel-get-src-block-info' so
this variable exists to ensure `jupyter-generate-request' uses
the parameters that `org-mode' provides when evaluating a source
block.")

(defconst org-babel-jupyter-async-inline-results-pending-indicator "???"
  "A string to disambiguate pending inline results from empty results.")

(defun org-babel-execute:jupyter (body params)
  "Execute BODY according to PARAMS.
BODY is the code to execute for the current Jupyter `:session' in
the PARAMS alist."
  (let* ((org-babel-jupyter-current-src-block-params params)
         (jupyter-current-client
          (thread-first (alist-get :session params)
            (org-babel-jupyter-initiate-session params)
            (thread-last (buffer-local-value 'jupyter-current-client))))
         (kernel-lang (jupyter-kernel-language jupyter-current-client))
         (vars (org-babel-variable-assignments:jupyter params kernel-lang))
         (code (org-babel-expand-body:jupyter body params vars kernel-lang))
         (req (jupyter-send-execute-request jupyter-current-client :code code)))
    (when (member "replace" (assq :result-params params))
      (org-babel-jupyter-cleanup-file-links))
    (cond
     ((or (equal (alist-get :async params) "yes")
          (plist-member params :async))
      ;; TODO: Support :results link in this case as well. What we can do is
      ;; set `jupyter-org-request-silent-p' to "none" so that no results are
      ;; appended, but then we have to remove the ID and insert the link once
      ;; everything comes in. Maybe remove `jupyter-org-request-silent-p' and
      ;; have the meaning of `jupyter-org-request-result-type' to also include
      ;; silent results and link style results?
      (when (member "file" (assq :result-params params))
        (org-babel-jupyter--remove-file-param params))
      (cl-labels
          ((sync-on-export
            ()
            ;; Remove the hook before waiting so it doesn't get called again.
            (remove-hook 'org-babel-after-execute-hook #'sync-on-export t)
            (unless (jupyter-request-idle-received-p req)
              (while (null (jupyter-wait-until-idle req jupyter-long-timeout))))))
        ;; Ensure we convert async blocks to synchronous ones when exporting
        (when (bound-and-true-p org-export-current-backend)
          (add-hook 'org-babel-after-execute-hook #'sync-on-export t t))
        (if (jupyter-org-request-inline-block-p req)
            org-babel-jupyter-async-inline-results-pending-indicator
          (jupyter-org-pending-async-results req))))
     (t
      (let ((result-params (assq :result-params params)))
        (when (and (member "file" result-params)
                   ;; In Org >= 9.2 these mean to ignore the results and insert
                   ;; a link to file so don't remove the file parameters in
                   ;; these cases since that is useful.
                   (not (or (member "link" result-params)
                            (member "graphics" result-params))))
          (org-babel-jupyter--remove-file-param params)))
      (while (null (jupyter-wait-until-idle req jupyter-long-timeout)))
      (if (jupyter-org-request-inline-block-p req)
          ;; In the case of synchronous inline results, only the result of the
          ;; execute-result message will be added to
          ;; `jupyter-org-request-results', stream results and any display data
          ;; messages will be displayed in a separate buffer.
          (car (jupyter-org-request-results req))
        (prog1 (jupyter-org-sync-results req)
          ;; Add after since the initial result params are used in
          ;; `jupyter-org-client'
          (nconc (alist-get :result-params params) (list "raw"))))))))

;;; Overriding source block languages, language aliases

(defvar org-babel-jupyter--babel-ops
  '("execute" "expand-body" "prep-session" "edit-prep"
    "variable-assignments" "load-session"))

(defun org-babel-jupyter--override-restore-header-args (lang restore)
  "Set `org-babel-header-args:LANG' to its Jupyter equivalent.
`org-babel-header-args:LANG' is set to the value of
`org-babel-header-args:jupyter-LANG', if the latter exists, when
RESTORE is nil. If `org-babel-header-args:LANG' had a value, save
it as a symbol property of `org-babel-header-args:LANG' for
restoring it later.

If RESTORE is non-nil, set `org-babel-header-args:LANG' to its
saved value before it was overridden.

Do the same for `org-babel-default-header-args:LANG'."
  (dolist (prefix '("org-babel-header-args:"
                    "org-babel-default-header-args:"))
    (when-let* ((jupyter-var (intern-soft (concat prefix "jupyter-" lang))))
      (let ((var (intern-soft (concat prefix lang))))
        (if restore
            (set var (get var 'jupyter-restore-value))
          (if var (put var 'jupyter-restore-value (symbol-value var))
            (setq var (intern (concat prefix lang))))
          (set var (symbol-value jupyter-var)))))))

(defun org-babel-jupyter--override-restore-src-block (lang restore)
  (cl-macrolet ((override-restore
                 (sym jupyter-sym)
                 `(cond
                   (restore
                    (advice-remove ,sym ,jupyter-sym)
                    ;; The function didn't have a definition, so ensure that
                    ;; we restore that fact.
                    (when (eq (symbol-function ,sym) #'ignore)
                      (fmakunbound ,sym)))
                   (t
                    ;; If a language doesn't have a function assigned, set one
                    ;; so it can be overridden
                    (unless (fboundp ,sym)
                      (fset ,sym #'ignore))
                    (advice-add ,sym :override ,jupyter-sym
                                '((name . ob-jupyter)))))))
    (dolist (fn (cl-set-difference
                 org-babel-jupyter--babel-ops
                 '("variable-assignments" "expand-body")
                 :test #'equal))
      (let ((sym (intern (concat "org-babel-" fn ":" lang))))
        (override-restore sym (intern (concat "org-babel-" fn ":jupyter-" lang)))))
    (override-restore (intern (concat "org-babel-" lang "-initiate-session"))
                      #'org-babel-jupyter-initiate-session))
  (org-babel-jupyter--override-restore-header-args lang restore))

(defun org-babel-jupyter-override-src-block (lang)
  "Override the built-in `org-babel' functions for LANG.
This overrides functions like `org-babel-execute:LANG' and
`org-babel-LANG-initiate-session' to use the machinery of
jupyter-LANG source blocks."
  (org-babel-jupyter--override-restore-src-block lang nil))

(defun org-babel-jupyter-restore-src-block (lang)
  "Restore the overridden `org-babel' functions for LANG.
See `org-babel-jupyter-override-src-block'."
  (org-babel-jupyter--override-restore-src-block lang t))

(defun org-babel-jupyter-make-language-alias (kernel lang)
  "Similar to `org-babel-make-language-alias' but for Jupyter src-blocks.
KERNEL should be the name of the default kernel to use for kernel
LANG. All necessary org-babel functions for a language with the
name jupyter-LANG will be aliased to the Jupyter functions."
  (dolist (fn org-babel-jupyter--babel-ops)
    (let ((sym (intern-soft (concat "org-babel-" fn ":jupyter"))))
      (when (and sym (fboundp sym))
        (defalias (intern (concat "org-babel-" fn ":jupyter-" lang)) sym))))
  (defalias (intern (concat "org-babel-jupyter-" lang "-initiate-session"))
    'org-babel-jupyter-initiate-session)
  (let (var)
    (setq var (concat "org-babel-header-args:jupyter-" lang))
    (unless (intern-soft var)
      (set (intern var) org-babel-header-args:jupyter))
    (put (intern var) 'variable-documentation
         (get 'org-babel-header-args:jupyter 'variable-documentation))
    (setq var (concat "org-babel-default-header-args:jupyter-" lang))
    (unless (and (intern-soft var)
                 (boundp (intern var)))
      (set (intern var) `((:async . "no"))))
    (put (intern var) 'variable-documentation
         (format "Default header arguments for Jupyter %s src-blocks" lang))
    ;; Always set the kernel if there isn't one. Typically the default header
    ;; args for a language are set by the user in their configurations by
    ;; calling `setq', but the :kernel is typically not something the user
    ;; wants to set directly so make sure its defined in the header args.
    (setq var (intern var))
    (unless (alist-get :kernel (symbol-value var))
      (setf (alist-get :kernel (symbol-value var)) kernel)))
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
language if one does not already exist. The alias is created with
`org-babel-jupyter-make-language-alias'.

SPECS defaults to `jupyter-available-kernelspecs'. Optional
argument REFRESH has the same meaning as in
`jupyter-available-kernelspecs'."
  (cl-loop
   with specs = (or specs
                    (with-demoted-errors "Error retrieving kernelspecs: %S"
                      (jupyter-available-kernelspecs refresh)))
   for (kernel . (_dir . spec)) in specs
   for lang = (plist-get spec :language)
   unless (member lang languages) collect lang into languages and
   do (org-babel-jupyter-make-language-alias kernel lang)))

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
