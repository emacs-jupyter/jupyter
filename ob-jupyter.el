;;; ob-jupyter.el --- Jupyter integration with org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 21 Jan 2018
;; Version: 0.6.0

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

;; TODO: Properly replace the source block results when multiple results are
;; appended. Currently what happens is that only the first result is removed
;; when re-execution the src block due to the behavior of
;; `org-babel-result-end'. The solution is to keep calling
;; `org-babel-result-end' moving point to the end of the results until
;; `org-babel-result-end' returns the same position twice. We may need to
;; advise this function to implement this behavior for jupyter blocks.
;;
;; An alternative is to collect all results in both async and sync cases before
;; insertion. Then if there are multiple types of data, for example images and
;; text, we can insert them all into a single RESULTS drawer.

;;; Code:

(defgroup ob-jupyter nil
  "Jupyter integration with org-mode"
  :group 'org-babel)

(require 'jupyter-org-client)
;; For `jupyter-with-repl-buffer'
(eval-when-compile (require 'jupyter-repl))

(declare-function org-element-at-point "org-element")
(declare-function org-link-set-parameters "org" (type &rest parameters))
(declare-function org-in-src-block-p "org" (&optional inside))
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-type "org-element" (element))
(declare-function org-element-context "org-element" (&optional element))
(declare-function org-babel-variable-assignments:python "ob-python" (params))
(declare-function org-babel-expand-body:generic "ob-core" (body params &optional var-lines))

(defvaralias 'org-babel-jupyter-resource-directory
  'jupyter-org-resource-directory)

(defvar org-babel-jupyter-session-clients (make-hash-table :test #'equal)
  "A hash table mapping session names to `jupyter-repl-client's.")

(defvar org-babel-header-args:jupyter '((kernel . :any)
                                        (async . ((yes no))))
  "Available header arguments for Jupter src-blocks.")

(defvar org-babel-default-header-args:jupyter '((:kernel . "python")
                                                (:async . "no"))
  "Default header arguments for Jupyter src-blocks.")

(defvar org-babel-jupyter-language-regex "^[ \t]*#\\+begin_src[ \t]+jupy-\\([^ \f\t\n\r\v]+\\)[ \t]*"
  "Regular expression used to extract a source block's language name.")

(defun org-babel-variable-assignments:jupyter (params &optional lang)
  "Assign variables in PARAMS according to the Jupyter kernel language.
LANG is the kernel language of the source block. If LANG is nil,
get the kernel language from the current source block.

The variables are assigned by looking for the function
`org-babel-variable-assignments:LANG'. If this function does not
exist or if LANG cannot be determined, assign variables using
`org-babel-variable-assignments:python'."
  (let* ((lang (or lang
                   (save-excursion
                     ;; TODO: This is not the most general case since we have
                     ;; to consider inline calls.
                     (when (and (org-in-src-block-p)
                                (re-search-backward
                                 org-babel-jupyter-language-regex nil t))
                       (match-string 1)))))
         (fun (when lang
                (intern (concat "org-babel-variable-assignments:" lang)))))
    (if (functionp fun) (funcall fun params)
      (org-babel-variable-assignments:python params))))

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
`org-babel-expand-body:generic'."
  (let* ((lang (or lang
                   (save-excursion
                     (when (re-search-backward
                            org-babel-jupyter-language-regex nil t)
                       (match-string 1)))))
         (fun (when lang
                (intern (concat "org-babel-expand-body:" lang)))))
    (if (functionp fun) (funcall fun body params var-lines)
      (org-babel-expand-body:generic body params var-lines))))

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
        (insert (org-babel-chomp body))
        (current-buffer)))))

(defun org-babel-jupyter-initiate-session-by-key (session params)
  "Return the `jupyter-repl-client' buffer for SESSION.
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

(defun org-babel-jupyter--cleanup-file-links ()
  "Delete files of image links that are being replaced for the current result.
Do this only if the file exists in
`org-babel-jupyter-resource-directory'."
  (save-restriction
    (narrow-to-region (point) (org-babel-result-end))
    ;; This assumes that `jupyter-org-client' only emits bracketed links as
    ;; images
    (let ((re (format "^[ \t]*%s[ \t]*$" org-bracket-link-regexp)))
      (while (re-search-forward re nil t)
        (let* ((link (org-element-context))
               (path (org-element-property :path link)))
          (when (and
                 (equal (expand-file-name org-babel-jupyter-resource-directory)
                        (expand-file-name (file-name-directory path)))
                 (file-exists-p path))
            (delete-file path)))))))

(defun org-babel-execute:jupyter (body params)
  "Execute BODY according to PARAMS.
BODY is the code to execute for the current Jupyter `:session' in
the PARAMS alist."
  (let* ((default-directory (or (alist-get :dir params) default-directory))
         (client (with-current-buffer
                     (org-babel-jupyter-initiate-session
                      (alist-get :session params) params)
                   jupyter-current-client))
         (kernel-lang (jupyter-kernel-language client))
         (vars (org-babel-variable-assignments:jupyter params kernel-lang))
         (code (org-babel-expand-body:jupyter body params vars kernel-lang))
         (req (progn
                ;; This needs to be set to the same parameter object used
                ;; internally by org-babel since insertion of results will
                ;; manipulate it.
                (oset client block-params params)
                (jupyter-send-execute-request client :code code))))
    (when (member "replace" (assq :result-params params))
      (let ((pos (org-babel-where-is-src-block-result)))
        (when pos
          (org-with-point-at pos
            (org-babel-jupyter--cleanup-file-links)))))
    (cond
     ((or (equal (alist-get :async params) "yes")
          (plist-member params :async))
      (jupyter-org-insert-async-id req))
     (t
      (jupyter-wait-until-idle req most-positive-fixnum)
      (prog1 (jupyter-org-sync-results req)
        ;; Add after since the initial result params are used in
        ;; `jupyter-org-client'
        (nconc (alist-get :result-params params) (list "raw")))))))

(defun org-babel-jupyter-make-language-alias (kernel lang)
  "Similar to `org-babel-make-language-alias' but for Jupyter src-blocks.
KERNEL should be the name of the default kernel to use for kernel
LANG. All necessary org-babel functions for a language with the
name jupy-LANG will be aliased to the Jupyter functions."
  (dolist (fn '("execute" "expand-body" "prep-session" "edit-prep"
                "variable-assignments" "load-session"))
    ;; Use :jupyter here for the base functions, for all the language aliases
    ;; use :jupy-LANG, this is to avoid name clashes with the ob-ipython
    ;; package.
    (let ((sym (intern-soft (concat "org-babel-" fn ":jupyter"))))
      (when (and sym (fboundp sym))
        (defalias (intern (concat "org-babel-" fn ":jupy-" lang)) sym))))
  (defalias (intern (concat "org-babel-jupy-" lang "-initiate-session"))
    'org-babel-jupyter-initiate-session)
  (set (intern (concat "org-babel-header-args:jupy-" lang))
       org-babel-header-args:jupyter)
  (set (intern (concat "org-babel-default-header-args:jupy-" lang))
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
                  (cons (concat "jupy-" lang)
                        (cdr (assoc lang org-babel-tangle-lang-exts)))))
   (add-to-list 'org-src-lang-modes
                (cons (concat "jupy-" lang)
                      (intern (or (cdr (assoc lang org-src-lang-modes))
                                  (replace-regexp-in-string
                                   "[0-9]*" "" lang)))))))

(org-babel-jupyter-aliases-from-kernelspecs)

(provide 'ob-jupyter)

;;; ob-jupyter.el ends here
