;;; ob-jupyter.el --- Jupyter integration with org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 21 Jan 2018
;; Version: 0.0.1

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

;;

;;; Code:

(defgroup ob-jupyter nil
  "Jupyter integration with org-mdoe"
  :group 'jupyter)

(require 'jupyter)
(require 'ob)

(declare-function cddar "cl" (x))
(declare-function org-element-at-point "org-element")
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-type "org-element" (element))
(declare-function org-element-context "org-element" (&optional element))
(declare-function org-babel-variable-assignments:python "ob-python" (params))
(declare-function org-babel-expand-body:generic "ob-core" (body params &optional var-lines))

(defcustom org-babel-jupyter-resource-directory "./.ob-jupyter/"
  "Directory used to store automatically generated image files.
See `org-babel-jupyter-file-name'."
  :group 'ob-jupyter)

(defvar org-babel-jupyter-session-clients (make-hash-table :test #'equal)
  "A hash table mapping session names to `jupyter-repl-client's.")

(defvar org-babel-header-args:jupyter '((kernel . :any)
                                        (async . ((yes no))))
  "Available header arguments for Jupter src-blocks.")

(defvar org-babel-default-header-args:jupyter '((:kernel . "python")
                                                (:async . "no"))
  "Default header arguments for Jupyter src-blocks.")

(defun org-babel-jupyter--get-src-block-info (light &optional context)
  "Similar to `org-babel-get-src-block-info', but handle inline-babel-call.
LIGHT and CONTEXT have the same meaning as in
`org-babel-get-src-block-info'.

If CONTEXT corresponds to an inline-babel-call, go to the named
src block and return its info. Otherwise the behavior is the same
as `org-babel-get-src-block-info'."
  (or context (setq context (org-element-context)))
  (save-excursion
    (when (eq (org-element-type context) 'inline-babel-call)
      (org-babel-goto-named-src-block
       (org-element-property :call context)))
    (org-babel-get-src-block-info light context)))

(defun org-babel-jupyter-src-block-lang (&optional element)
  "Get the kernel language of a Jupyter src-block.
ELEMENT should be a src-block org element and and defaults to the
`org-element-at-point'."
  (or element (setq element (org-element-at-point)))
  (when (and (memq (org-element-type element)
                   '(src-block inline-src-block inline-babel-call))
             (equal (org-element-property :language element) "jupyter"))
    (let ((begin (org-element-property :begin element)))
      (or (get-text-property begin 'jupyter-language)
          (let* ((args (nth 2 (org-babel-jupyter--get-src-block-info
                               'light element)))
                 (spec (cddar
                        (jupyter-find-kernelspecs (alist-get :kernel args))))
                 (lang (plist-get spec :language)))
            (put-text-property begin (1+ begin) 'jupyter-language lang))))))

(defun org-babel-jupyter--get-lang-mode (orig-fun lang &rest args)
  "Identical to `org-src--get-lang-mode' but handle jupyter blocks specially.
This is an advice function for `org-src--get-lang-mode' which
should be passed as the ORIG-FUN argument. Whenever LANG is
\"jupyter\" pass the result of `org-babel-jupyter-src-block-lang'
to ORIG-FUN as its first argument instead of \"jupyter\". ARGS
are any additional arguments to pass to ORIG-FUN. This should be
nil."
  (apply orig-fun
         (if (equal lang "jupyter")
             (org-babel-jupyter-src-block-lang)
           lang)
         args))

(advice-add 'org-src--get-lang-mode :around #'org-babel-jupyter--get-lang-mode)

(defun org-babel-variable-assignments:jupyter (params &optional element)
  "Assign variables in PARAMS according to the Jupyter kernel language.
Use `org-babel-jupyter-src-block-lang' to get the kernel language
of the src-block ELEMENT and call the variable assignment
function for the language. ELEMENT defaults to the
`org-element-at-point'. So if LANG is the kernel language, then
call

    org-babel-variable-assignments:LANG

If the above function doesn't exist or if no kernel langauge can
be found, fall back to `org-babel-variable-assignments:python'."
  (let* ((lang (org-babel-jupyter-src-block-lang element))
         (fun (when lang
                (intern (concat "org-babel-variable-assignments:" lang)))))
    (if (functionp fun) (funcall fun params)
      (org-babel-variable-assignments:python params))))

(defun org-babel-expand-body:jupyter (body params &optional var-lines element)
  "Expand BODY according to PARAMS.

BODY is the code to expand, PARAMS should be the header arguments
of the src block with BODY as its code, and VAR-LINES should be
the list of strings containing the variables to evaluate before
executing body.

This function is similar to
`org-babel-variable-assignments:jupyter' in that it finds the
kernel language of the src-block ELEMENT, defaulting to the
`org-element-at-point', to find get the kernel language of BODY.
So if LANG is the kernel language, call the function

    org-babel-expand-body:LANG

to expand BODY. If the above function doesn't exist or if no
kernel langauge can be found fall back to
`org-babel-expand-body:generic'."
  (let* ((lang (org-babel-jupyter-src-block-lang element))
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

(defun org-babel-prep-session:jupyter (session params &optional no-execute)
  "Prepare a Jupyter SESSION according to PARAMS.
If optional argument NO-EXECUTE is non-nil, do not execute any of
the header variables in PARAMS."
  (let ((buffer (org-babel-jupyter-initiate-session session params))
        (var-lines (org-babel-variable-assignments:jupyter params)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (when var-lines
        (jupyter-repl-replace-cell-code
         (mapconcat #'identity var-lines "\n"))
        ;; For `org-babel-load-session:jupyter'
        (when no-execute
          (insert "\n")))
      (unless no-execute
        (jupyter-execute-request jupyter-repl-current-client))
      (current-buffer))))

(defun org-babel-load-session:jupyter (session body params)
  "In a Jupyter SESSION, load BODY according to PARAMS."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:jupyter session params 'noexecute)))
      (with-current-buffer buffer
        (insert (org-babel-chomp body))
        (current-buffer)))))

(defun org-babel-jupyter-initiate-session-by-key (session params)
  "Return the `jupyter-repl-client' for SESSION.
If SESSION does not have a client already, one is created based
on SESSION and PARAMS. If SESSION ends with \".json\" then
SESSION is interpreted as a kernel connection file and a new
kernel connected to SESSION is created. Otherwise a kernel is run
based on the `:kernel' parameter in PARAMS which should be either
a valid kernel name or a prefix of one. The first kernel that is
returned by `jupyter-find-kernelspecs' will be used."
  (let* ((kernel (alist-get :kernel params))
         (key (concat session "-" kernel)))
    (oref (or (gethash key org-babel-jupyter-session-clients)
              (let ((client (if (string-suffix-p ".json" session)
                                (connect-jupyter-repl session)
                              (run-jupyter-repl kernel))))
                (jupyter-set client 'jupyter-include-other-output nil)
                (with-jupyter-repl-buffer client
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
                (puthash key client org-babel-jupyter-session-clients)))
          buffer)))

(defun org-babel-jupyter-initiate-session (&optional session params)
  "Initialize a Jupyter SESSION according to PARAMS."
  (if (equal session "none") (error "Need a session to run")
    (org-babel-jupyter-initiate-session-by-key session params)))

(defun org-babel-jupyter-file-name (info ext)
  "Return a file name based on src-block INFO.
INFO should be the full list returned by
`org-babel-get-src-block-info', i.e. the list returned when the
LIGHT argument is nil. If the src-block that corresponds to INFO
has a `:file' argument, then just return that. Otherwise, when no
`:file' argument exists, generate a file name by concatenating
the following:

1. `org-babel-jupyter-resource-directory'
2. The hash returned by `org-babel-sha1-hash' on INFO
3. .EXT

Where EXT should be the file extension to give the
file (excluding the dot)."
  (let ((file (alist-get :file (nth 2 info))))
    (or file
        (let ((dir (prog1 org-babel-jupyter-resource-directory
                     (unless (file-directory-p
                              org-babel-jupyter-resource-directory)
                       (make-directory org-babel-jupyter-resource-directory)))))
          (concat (file-name-as-directory dir)
                  (org-babel-sha1-hash info) "." ext)))))

(defun org-babel-jupyter-render-result (data info)
  "Return the rendered DATA.
DATA is a plist, (:mimetype1 value1 ...), which is used to render
a result which can be passed to `org-babel-insert-result'.

INFO is the list returned by `org-babel-get-src-block-info' for
the src-block whose results are contained in DATA. Note that INFO
should be the full src-block info, i.e. the LIGHT argument of
`org-babel-get-src-block-info' should be nil when obtaining INFO.

Return a cons cell (RESULT-PARAM . RESULTS) where RESULT-PARAM is
either a result parameter, i.e. one of the RESULT-PARAMS of
`org-babel-insert-result', or a key value pair which should be
added to the ARGUMENTS list of INFO before calling
`org-babel-insert-result'.

For example, if DATA only contains the mimetype `:text/markdown',
the RESULT-PARAM will be

    (:wrap . \"EXPORT markdown\")

and RESULTS will be the markdown text which should be wrapped in
an \"EXPORT markdown\" block. See `org-babel-insert-result'.

Note that INFO is only relevant if the resulting rendered DATA
ends up being an image and no `:file' argument is found in INFO.
In this case a file name is automatically generated, see
`org-babel-jupyter-file-name'."
  (let ((mimetypes (seq-filter #'keywordp data))
        (result-params (alist-get :result-params (nth 2 info)))
        param result)
    (cond
     ((memq :text/org mimetypes)
      (setq param "org"
            result (plist-get data :text/org)))
     ((memq :text/html mimetypes)
      (let ((html (plist-get data :text/html)))
        (if (string-match "^<img src=\"data:\\(.+\\);base64,\\(.+\\)\"" html)
            (let ((mimetype (intern (concat ":" (match-string 1 html)))))
              (org-babel-jupyter-render-result
               (list mimetype (match-string 2 html)) info))
          (setq param "html"
                result (plist-get data :text/html)))))
     ((memq :text/markdown mimetypes)
      (setq param '(:wrap . "EXPORT markdown")
            result (plist-get data :text/markdown)))
     ((memq :text/latex mimetypes)
      ;; TODO: Handle other cases like this for other mimetypes
      (setq param (unless (member "raw" result-params) "latex")
            result (plist-get data :text/latex)))
     ((memq :image/png mimetypes)
      (let ((file (org-babel-jupyter-file-name info "png")))
        (setq param "file"
              result (with-temp-file file
                       (let ((buffer-file-coding-system 'binary)
                             (require-final-newline nil))
                         (insert (plist-get data :image/png))
                         (base64-decode-region (point-min) (point-max)))
                       file))))
     ((memq :image/svg+xml mimetypes)
      (let ((file (org-babel-jupyter-file-name info "svg")))
        (setq param "file"
              result (with-temp-file file
                       (let ((require-final-newline nil))
                         (insert (plist-get data :image/svg+xml)))
                       file))))
     ((memq :text/plain mimetypes)
      (setq result (plist-get data :text/plain)))
     (t (warn "No supported mimetype found %s" mimetypes)))
    (list param result)))

;; TODO: Properly handle the execution-state and execution-count of the REPL
;; buffer. I don't think ob-jupyter should be responsible for updating the
;; state of the REPL buffer. What I really want to do is to be able to silence
;; any output generated by the REPL due to execute requests of a jupyter
;; src-block. Since a jupyter src-block already handles all of the output
;; anyways it doesn't seem necessary. It seems that there should be an option
;; to prevent output in the REPL buffer and just send off execute requests
;; without displaying any results and this should have the option of being
;; configured for every request since you would still like to show the output
;; when sending requests directly from the REPL buffer.
(defun org-babel-execute:jupyter (body params)
  "Execute BODY according to PARAMS.
BODY is the code to execute for the current Jupyter `:session' of
PARAMS."
  (let* ((session (alist-get :session params))
         (repl-buffer (org-babel-jupyter-initiate-session session params))
         ;; TODO: Figure out if the current context is always a source block
         ;; context for the various commands that can execute code blocks.
         ;; Since this function is only given a body and params, I am assuming
         ;; it should be written to not depend on a context.
         (element (org-element-at-point))
         (context (org-element-context element))
         (block-info (org-babel-jupyter--get-src-block-info nil context))
         (code (org-babel-expand-body:jupyter
                body params (org-babel-variable-assignments:jupyter
                             params element)
                element))
         (req (with-current-buffer repl-buffer
                (goto-char (point-max))
                (jupyter-repl-replace-cell-code code)
                ;; TODO: Should handlers be inhbited? They are inhibited to
                ;; prevent output from going into the REPL buffer when it is
                ;; redirected to the output of a Jupyter src block. It
                ;; doesn't seem to make sense to insert output in the REPL
                ;; buffer when it is redirected somewhere else.
                (prog1 (let ((jupyter-inhibit-handlers t))
                         (jupyter-execute-request jupyter-repl-current-client))
                  (when (get-buffer-window)
                    (set-window-point (get-buffer-window) (point)))))))
    ;; Setup callbacks for the request
    (let* ((result-type (alist-get :result-type params))
           (result-params (alist-get :result-params params))
           (async (equal (alist-get :async params) "yes"))
           (block-beginning
            (copy-marker org-babel-current-src-block-location))
           (finalize-execution
            (lambda (result)
              ;; Unmark the REPL cell as busy. This is needed since we set
              ;; `jupyter-inhibit-handlers' to t before executing a jupyter
              ;; src-block which means that the handler which unmark's the cell
              ;; is not run. The cell is marked as busy in
              ;; `jupyter-execute-request' for a `jupyter-repl-client'.
              (with-current-buffer repl-buffer
                (save-excursion
                  (jupyter-repl-goto-cell req)
                  (jupyter-repl-cell-unmark-busy)))
              (when async
                (org-with-point-at block-beginning
                  (let ((lang (org-babel-jupyter-src-block-lang)))
                    (org-babel-insert-result
                     result result-params block-info nil lang))))
              (set-marker block-beginning nil)))
           result output)
      (jupyter-add-callback req
        :stream
        (lambda (msg)
          (and (eq result-type 'output)
               (equal (jupyter-message-get msg :name) "stdout")
               (push (ansi-color-apply (jupyter-message-get msg :text))
                     output)))
        ;; FIXME: finalize-execution should be called when the status goes idle
        ;; so that in the case of result-type output, all of the output is
        ;; captured. See
        ;; http://jupyter-client.readthedocs.io/en/stable/messaging.html#request-reply
        ;;
        ;; The reason this is not done is due to the Julia kernel not sending
        ;; the idle message when multiple execute requests are queued. It seems
        ;; to mangle the parent message id when this is the case so that
        ;; multiple idle messages are received for the same request instead of
        ;; one for each queued request. <2018-01-22 Mon>
        :execute-reply
        (lambda (msg)
          (cl-destructuring-bind (&key status ename evalue traceback
                                       &allow-other-keys)
              (jupyter-message-content msg)
            (if (equal status "ok")
                ;; See the execute-result callback for when result-type is
                ;; value
                (when (eq result-type 'output)
                  (setq result (mapconcat #'identity (nreverse output) "\n")))
              (if (eq result-type 'value)
                  (setq result (format "%s: %s" ename (ansi-color-apply evalue)))
                (push (mapconcat #'ansi-color-apply traceback "\n") output)
                (setq result (mapconcat #'identity (nreverse output) "\n"))))
            (funcall finalize-execution result)))
        :execute-result
        (lambda (msg)
          (unless (eq result-type 'output)
            (cl-destructuring-bind (render-param res)
                (org-babel-jupyter-render-result
                 (jupyter-message-get msg :data) block-info)
              ;; Update params/result-params for `org-babel-insert-result' This
              ;; gives a way to insert the results based on its mimetype.
              ;;
              ;; NOTE: This relies on the semantics of
              ;; `org-babel-insert-result' which prioritizes the various values
              ;; of result-params. So if a user species different result-params
              ;; locally to a block, the settings here may override the user's
              ;; setting and vice versa.
              ;;
              ;; NOTE: This modifies the lists contained in params so that
              ;; synchronous evaluation will work. This seems to be the only
              ;; way to handle both cases of async and synchronous execution.
              (cond
               ((consp render-param)
                (nconc params (list render-param))
                ;; Since async results are inserted using block-info
                (cl-callf org-babel-merge-params (nth 2 block-info)
                  (list render-param)))
               ((stringp render-param)
                (nconc result-params (list render-param))))
              (setq result res)))))
      (if async (format "%s%s"
                        (if (member "raw" result-params) ": " "")
                        (jupyter-request-id req))
        (jupyter-wait-until-received :execute-reply req most-positive-fixnum)
        result))))

(provide 'ob-jupyter)

;;; ob-jupyter.el ends here
