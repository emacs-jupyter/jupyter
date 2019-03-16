;;; ob-jupyter.el --- Jupyter integration with org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 21 Jan 2018
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

;; Interact with a Jupyter kernel via `org-mode' src-block's.

;;; Code:

(defgroup ob-jupyter nil
  "Jupyter integration with org-mode"
  :group 'org-babel)

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

(defvaralias 'org-babel-jupyter-resource-directory
  'jupyter-org-resource-directory)

(defvar org-babel-jupyter-session-clients (make-hash-table :test #'equal)
  "A hash table mapping session names to Jupyter clients.")

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
                       (funcall expander body params var-lines)
                     (org-babel-expand-body:generic body params var-lines)))
         (changelist nil))
    (when-let* ((dir (alist-get :dir params)))
      (setq changelist (plist-put changelist :dir (expand-file-name dir))))
    (if changelist (org-babel-jupyter-transform-code expanded changelist)
      expanded)))

(defun org-babel-edit-prep:jupyter (info)
  "Prepare the edit buffer according to INFO."
  (let* ((params (nth 2 info))
         (session (alist-get :session params)))
    (jupyter-repl-associate-buffer
     (org-babel-jupyter-initiate-session session params))))

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

(defun org-babel-jupyter-initiate-session-by-key (session params)
  "Return the Jupyter REPL buffer for SESSION.
If SESSION does not have a client already, one is created based
on SESSION and PARAMS. If SESSION ends with \".json\" then
SESSION is interpreted as a kernel connection file and a new
kernel connected to SESSION is created.

Otherwise a kernel is started based on the `:kernel' parameter
in PARAMS which should be either a valid kernel name or a prefix
of one. The first kernel that is returned by
`jupyter-find-kernelspecs' when passed the value of the `:kernel'
parameter will be used."
  (let* ((kernel (alist-get :kernel params))
         (key (concat session "-" kernel))
         (client
          (or (gethash key org-babel-jupyter-session-clients)
              (let ((client
                     (if (string-suffix-p ".json" session)
                         (jupyter-connect-repl session nil nil 'jupyter-org-client)
                       (jupyter-run-repl kernel nil nil 'jupyter-org-client))))
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
                (puthash key client org-babel-jupyter-session-clients)))))
    (oref client buffer)))

(defun org-babel-jupyter-initiate-session (&optional session params)
  "Initialize a Jupyter SESSION according to PARAMS."
  (if (equal session "none") (error "Need a session to run")
    (org-babel-jupyter-initiate-session-by-key session params)))

(defun org-babel-jupyter-scratch-buffer ()
  "Display a scratch buffer connected to the current block's session."
  (interactive)
  (let (buffer)
    (org-babel-do-in-edit-buffer
     (setq buffer (jupyter-repl-scratch-buffer)))
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

(defun org-babel-execute:jupyter (body params)
  "Execute BODY according to PARAMS.
BODY is the code to execute for the current Jupyter `:session' in
the PARAMS alist."
  (let* ((jupyter-current-client (with-current-buffer
                                     (org-babel-jupyter-initiate-session
                                      (alist-get :session params) params)
                                   jupyter-current-client))
         (kernel-lang (jupyter-kernel-language jupyter-current-client))
         (vars (org-babel-variable-assignments:jupyter params kernel-lang))
         (code (org-babel-expand-body:jupyter body params vars kernel-lang))
         (req (progn
                ;; This needs to be set to the same parameter object used
                ;; internally by org-babel since insertion of results will
                ;; manipulate it.
                (oset jupyter-current-client block-params params)
                (jupyter-send-execute-request jupyter-current-client
                  :code code))))
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
            (unless (jupyter-request-idle-received-p req)
              (jupyter-wait-until-idle req most-positive-fixnum))
            (remove-hook 'org-babel-after-execute-hook #'sync-on-export t)))
        ;; Ensure we convert async blocks to synchronous ones when exporting
        (when (bound-and-true-p org-export-current-backend)
          (add-hook 'org-babel-after-execute-hook #'sync-on-export t t))
        (if (jupyter-org-request-inline-block-p req) ""
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
      (jupyter-wait-until-idle req most-positive-fixnum)
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

(defun org-babel-jupyter--override-restore-src-block (lang restore)
  (let ((fun (if restore
                 (lambda (sym _ function &rest __)
                   (advice-remove sym function))
               #'advice-add)))
    (dolist (fn (cl-set-difference
                 org-babel-jupyter--babel-ops
                 '("variable-assignments" "expand-body")
                 :test #'equal))
      (let ((sym (intern (concat "org-babel-" fn ":" lang))))
        ;; If a language doesn't have a function assigned, set one so it can be
        ;; overridden
        (unless (fboundp sym)
          (fset sym #'ignore))
        (funcall fun sym
                 :override (intern (concat "org-babel-" fn ":jupyter-" lang))
                 '((name . ob-jupyter)))))
    (funcall fun (intern (concat "org-babel-" lang "-initiate-session"))
             :override #'org-babel-jupyter-initiate-session
             '((name . ob-jupyter)))
    (dolist (prefix '("org-babel-header-args:"
                      "org-babel-default-header-args:"))
      (when-let* ((var (intern-soft (concat prefix lang))))
        (if restore
            (set var (get var 'jupyter-restore-value))
          (let ((jupyter-var (intern (concat prefix "jupyter-" lang))))
            (put var 'jupyter-restore-value (symbol-value var))
            (set var (symbol-value jupyter-var))))))))

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
  (set (intern (concat "org-babel-header-args:jupyter-" lang))
       org-babel-header-args:jupyter)
  (set (intern (concat "org-babel-default-header-args:jupyter-" lang))
       `((:kernel . ,kernel)
         (:async . "no"))))

(defun org-babel-jupyter-aliases-from-kernelspecs (&optional refresh)
  "Make language aliases based on the available kernelspecs.
For all kernels returned by `jupyter-available-kernelspecs', make
a language alias for the kernel language if one does not already
exist. The alias is created with
`org-babel-jupyter-make-language-alias'.

Optional argument REFRESH has the same meaning as in
`jupyter-available-kernelspecs'."
  (cl-loop
   for (kernel . (_dir . spec)) in (jupyter-available-kernelspecs refresh)
   for lang = (plist-get spec :language)
   unless (member lang languages) collect lang into languages and
   do (org-babel-jupyter-make-language-alias kernel lang)
   (when (assoc lang org-babel-tangle-lang-exts)
     (add-to-list 'org-babel-tangle-lang-exts
                  (cons (concat "jupyter-" lang)
                        (cdr (assoc lang org-babel-tangle-lang-exts)))))
   (add-to-list 'org-src-lang-modes
                (cons (concat "jupyter-" lang)
                      (intern (or (cdr (assoc lang org-src-lang-modes))
                                  (replace-regexp-in-string
                                   "[0-9]*" "" lang)))))))

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
    (when-let* ((jupyter-p (org-babel-jupyter-language-p lang))
                (pos (org-babel-where-is-src-block-result))
                (ansi-color-apply-face-function
                 (lambda (beg end face)
                   ;; Could be useful for export backends
                   (when face
                     (put-text-property beg end 'face face)))))
      (goto-char pos)
      (ansi-color-apply-on-region (point) (org-babel-result-end)))))

;;; Hook into `org'

(org-babel-jupyter-aliases-from-kernelspecs)
(add-hook 'org-export-before-processing-hook #'org-babel-jupyter-setup-export)
(add-hook 'org-export-before-parsing-hook #'org-babel-jupyter-strip-ansi-escapes)

(provide 'ob-jupyter)

;;; ob-jupyter.el ends here
