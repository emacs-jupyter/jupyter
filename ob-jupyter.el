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
  :group 'org-babel)

(require 'jupyter)
(require 'ob)

(declare-function cddar "cl" (x))
(declare-function org-element-at-point "org-element")
(declare-function org-at-drawer-p "org")
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-type "org-element" (element))
(declare-function org-element-context "org-element" (&optional element))
(declare-function org-babel-variable-assignments:python "ob-python" (params))
(declare-function org-babel-python-table-or-string "ob-python" (results))
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

(defvar org-babel-jupyter-language-regex "^[ \t]*#\\+begin_src[ \t]+jupyter-\\([^ \f\t\n\r\v]+\\)[ \t]*"
  "Regular expression used to extract a source block's language name.")

(defun org-babel-variable-assignments:jupyter (params &optional lang)
  "Assign variables in PARAMS according to the Jupyter kernel language.
Use `org-babel-jupyter-src-block-lang' to get the kernel language
of the src-block ELEMENT and call the variable assignment
function for the language. ELEMENT defaults to the
`org-element-at-point'. So if LANG is the kernel language, then
call

    org-babel-variable-assignments:LANG

If the above function doesn't exist or if no kernel langauge can
be found, fall back to `org-babel-variable-assignments:python'."
  (let* ((lang (or lang
                   (save-excursion
                     (when (re-search-backward
                            org-babel-jupyter-language-regex nil t)
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

(defun org-babel-jupyter-file-name (data ext)
  "Return a file name based on DATA.
DATA is usually the contents of an image to create a file name
for. The generated absolute file name is based on the following:

1. The value of `org-babel-jupyter-resource-directory'
2. The `sha1' hash of DATA
3. .EXT

Where EXT should be the file extension to give the
file (excluding the dot)."
  (let ((dir (prog1 org-babel-jupyter-resource-directory
               (unless (file-directory-p
                        org-babel-jupyter-resource-directory)
                 (make-directory org-babel-jupyter-resource-directory)))))
    (concat (file-name-as-directory dir) (sha1 data) "." ext)))

(defun org-babel-jupyter--image-result (data file &optional overwrite base64-encoded)
  "Possibly write DATA to FILE.
If OVERWRITE is non-nil, overwrite FILE if it already exists.
Otherwise if FILE already exists, DATA is not written to FILE.

If BASE64-ENCODED is non-nil, the DATA is assumed to be encoded
with the base64 encoding and is first decoded before writing to
FILE.

Return the cons cell (\"file\" . FILE), see
`org-babel-jupyter-prepare-result'."
  (cons "file" (prog1 file
                 (when (or overwrite (not (file-exists-p file)))
                   (let ((buffer-file-coding-system
                          (if base64-encoded 'binary
                            buffer-file-coding-system))
                         (require-final-newline nil))
                     (with-temp-file file
                       (insert data)
                       (when base64-encoded
                         (base64-decode-region (point-min) (point-max)))))))))

(defun org-babel-jupyter-prepare-result (data metadata params)
  "Return the rendered DATA.
DATA is a plist, (:mimetype1 value1 ...), which is used to render
a result which can be passed to `org-babel-insert-result'.

METADATA is the metadata plist used to render DATA with, as
returned by the Jupyter kernel. This plist typically contains
information such as the size of an image to be rendered. The
metadata plist is currently unused.

PARAMS is the source block parameter list as passed to
`org-babel-execute:jupyter'. Currently this is only used to
extract the file name of an image file when DATA can be rendered
as an image type (either `:image/png' or `:image/svg+xml') when a
file name is passed to the code block. If no file name is given
one is generated based on DATA and the mimetype, see
`org-babel-jupyter-file-name'.

This function returns a cons cell (RESULT-PARAM . RESULT) where
RESULT-PARAM is either a result parameter, i.e. one of the result
paramters of `org-babel-insert-result', or a key value pair which
should be appended to the PARAMS list when to render RESULT.

For example, if DATA only contains the mimetype `:text/markdown',
the RESULT-PARAM will be

    (:wrap . \"EXPORT markdown\")

and RESULT will be the markdown text which should be wrapped in
an \"EXPORT markdown\" block. See `org-babel-insert-result'."
  (let ((mimetypes (cl-loop for elem in data if (keywordp elem) collect elem))
        (result-params (alist-get :result-params params)))
    (cond
     ((memq :text/org mimetypes)
      (cons (unless (member "raw" result-params) "org")
            (plist-get data :text/org)))
     ((memq :text/html mimetypes)
      (let ((html (plist-get data :text/html)))
        (save-match-data
          (if (string-match "^<img" html)
              (let* ((dom (with-temp-buffer
                            (insert html)
                            (libxml-parse-html-region (point-min) (point-max))))
                     (img (car (dom-by-tag dom 'img)))
                     (src (dom-attr img 'src)))
                ;; Regex adapted from `shr-get-image-data'
                (when (string-match
                       "\\`data:\\(\\([^/;,]+\\(/[^;,]+\\)?\\)\\(;[^;,]+\\)*\\)?,\\(.*\\)" src)
                  (let ((mimetype (intern (concat ":" (match-string 2 src))))
                        (data (url-unhex-string (match-string 5 src))))
                    (org-babel-jupyter-prepare-result
                     (list mimetype data) metadata params))))
            (cons "html" (plist-get data :text/html))))))
     ((memq :text/markdown mimetypes)
      (cons '(:wrap . "EXPORT markdown") (plist-get data :text/markdown)))
     ((memq :text/latex mimetypes)
      ;; TODO: Handle other cases like this for other mimetypes
      (cons (unless (member "raw" result-params) "latex")
            (plist-get data :text/latex)))
     ((memq :image/png mimetypes)
      (let* ((data (plist-get data :image/png))
             (overwrite (not (null (alist-get :file params))))
             (file (or (alist-get :file params)
                       (org-babel-jupyter-file-name data "png"))))
        (org-babel-jupyter--image-result data file overwrite 'b64-encoded)))
     ((memq :image/svg+xml mimetypes)
      (let* ((data (plist-get data :image/svg+xml))
             (overwrite (not (null (alist-get :file params))))
             (file (or (alist-get :file params)
                       (org-babel-jupyter-file-name data "svg"))))
        (org-babel-jupyter--image-result data file overwrite)))
     ((memq :text/plain mimetypes)
      (cons "scalar" (plist-get data :text/plain)))
     (t (warn "No supported mimetype found %s" mimetypes)))))

(defun org-babel-jupyter--inject-render-param (render-param params)
  "Destructively modify result parameters for `org-babel-insert-result'.
RENDER-PARAM is the first element of the list returned by
`org-babel-jupyter-prepare-result', PARAMS are the paramters
passed to `org-babel-execute:jupyter'.

Append RENDER-PARAM to RESULT-PARAMS if it is a string, otherwise
if RENDER-PARAM is a cons cell, (KEYWORD . STRING), append
RENDER-PARAM to the PARAMS."
  (let ((l (cond
            ((consp render-param) params)
            ((stringp render-param) (alist-get :result-params params))
            ((not (null render-param))
             (error "Render parameter unsupported (%s)" render-param)))))
    (when l (nconc l (list render-param)))))

(defun org-babel-jupyter--clear-render-param (render-param params)
  "Destructively modify result parameters.
Remove RENDER-PARAM from PARAMS or from the result parameters
found in PARAMS. If RENDER-PARAM is a cons cell, remove it from
the PARAMS list. If RENDER-PARAM is a string, remove it from the
`:result-params' of PARAMS. In all cases, `delq' is used for
removal."
  (let ((l (cond
            ((consp render-param) params)
            ((stringp render-param) (alist-get :result-params params))
            ((not (null render-param))
             (error "Render parameter unsupported (%s)" render-param)))))
    (when l (delq render-param l))))

(defun org-babel-jupyter--clear-request-id (req)
  "Delete the request id of REQ when prepending or appending results."
  (save-excursion
    (let ((start (org-babel-where-is-src-block-result)))
      (when start
        (goto-char start)
        (forward-line 1)
        (when (search-forward (jupyter-request-id req) nil t)
          (delete-region (line-beginning-position)
                         (1+ (line-end-position)))
          ;; Delete the entire drawer when there was nothing inside of it
          ;; except for the id.
          (when (and (org-at-drawer-p)
                     (progn
                       (forward-line -1)
                       (org-at-drawer-p)))
            (delete-region
             (point)
             (progn
               (forward-line 1)
               (1+ (line-end-position))))))))))

(defun org-babel-jupyter--transform-result (render-result kernel-lang)
  "Do some final transformations of RENDER-RESULT based on KERNEL-LANG.
For example, call `org-babel-python-table-or-string' on the
results when rendering scalar data for a python code block.

RENDER-RESULT should be the cons cell returned by
`org-babel-jupyter-prepare-result' and KERNEL-LANG is the kernel
language."
  (cl-destructuring-bind (render-param . result) render-result
    (cond
     ((and (equal render-param "scalar") (equal kernel-lang "python"))
      (cons "scalar" (org-babel-python-table-or-string result)))
     (t render-result))))

(defun org-babel-jupyter-insert-results (results params kernel-lang)
  "Insert RESULTS at the current source block location.
RESULTS is either a a single pair or a list of pairs with the form

    (RENDER-PARAM . RESULT)

i.e. the pairs returned by `org-babel-jupyter-prepare-result'.
PARAMS should be the parameters passed to
`org-babel-execute:jupyter'. KERNEL-LANG is the language of the
kernel for the current source block. If optional argument APPEND
is non-nil, then append RESULTS to the current results. The
results will also be appended, regardless of the value of APPEND,
if RESULTS is a list."
  ;; Unless this is a list of results
  (unless (car-safe (car results))
    (setq results (list results)))
  (cl-loop
   ;; FIXME: This is a hack that relies on `org-babel-insert-result' only caring
   ;; about the parameters of the info and not anything else.
   with info = (list nil nil params)
   with result-params = (alist-get :result-params params)
   for (render-param . result) in
   (mapcar (lambda (r) (org-babel-jupyter--transform-result r kernel-lang))
      results)
   do (org-babel-jupyter--inject-render-param render-param params)
   (cl-letf (((symbol-function 'message) #'ignore))
     (org-babel-insert-result result result-params info nil kernel-lang))
   (org-babel-jupyter--clear-render-param render-param params)))

(defun org-babel-execute:jupyter (body params)
  "Execute BODY according to PARAMS.
BODY is the code to execute for the current Jupyter `:session' of
PARAMS."
  (let* ((repl-buffer (org-babel-jupyter-initiate-session
                       (alist-get :session params) params))
         (client (with-current-buffer repl-buffer
                   jupyter-repl-current-client))
         (kernel-lang (plist-get (plist-get (oref client kernel-info)
                                            :language_info)
                                 :name))
         (code (org-babel-expand-body:jupyter
                body params (org-babel-variable-assignments:jupyter
                             params kernel-lang)
                kernel-lang))
         (req (with-jupyter-repl-buffer client
                (goto-char (point-max))
                (jupyter-repl-replace-cell-code code)
                (let ((jupyter-inhibit-handlers
                       '(:stream
                         :execute-reply :execute-result
                         :display-data :error)))
                  (jupyter-execute-request jupyter-repl-current-client)))))
    ;; Setup callbacks for the request
    (let* ((result-type (alist-get :result-type params))
           (async (equal (alist-get :async params) "yes"))
           (block-beginning
            (copy-marker org-babel-current-src-block-location))
           (id-cleared nil)
           (results nil)
           (add-result
            (lambda (result)
              ;; TODO: Figure out how to handle result-type output in the async
              ;; case. Should the output be pooled and displayed when finished?
              ;; No I don't think so. It should be appended to the current
              ;; output but for multiline output that is received this will end
              ;; up either putting it in an example block and you would have
              ;; multiple example blocks for a single output. The best bet
              ;; would be to insert it as raw text in a drawer.
              (or (consp result) (setq result (cons "scalar" result)))
              (if async
                  (org-with-point-at block-beginning
                    (unless id-cleared
                      (setq id-cleared t)
                      (org-babel-jupyter--clear-request-id req)
                      (org-babel-jupyter--inject-render-param "append" params))
                    (org-babel-jupyter-insert-results result params kernel-lang))
                (push result results)))))
      (jupyter-add-callback req
        :stream
        (lambda (msg)
          (and (eq result-type 'output)
               (equal (jupyter-message-get msg :name) "stdout")
               (funcall add-result (ansi-color-apply
                                    (jupyter-message-get msg :text)))))
        :status
        (lambda (msg)
          (when (jupyter-message-status-idle-p msg)
            (when (and async (not id-cleared))
              (org-babel-jupyter--clear-request-id req))
            (set-marker block-beginning nil)))
        :execute-reply
        (lambda (msg)
          (cl-destructuring-bind (&key status ename evalue traceback
                                       &allow-other-keys)
              (jupyter-message-content msg)
            (unless (equal status "ok")
              (if (eq result-type 'output)
                  (funcall add-result (mapconcat #'ansi-color-apply traceback "\n"))
                (funcall add-result (format "%s: %s" ename (ansi-color-apply evalue)))))))
        '(:display-data :execute-result)
        (lambda (msg)
          (unless (eq result-type 'output)
            (cl-destructuring-bind (&key data metadata &allow-other-keys)
                (jupyter-message-content msg)
              (funcall add-result (org-babel-jupyter-prepare-result
                                   data metadata params))))))
      (if async
          (concat (when (member "raw" (alist-get :result-params params)) ": ")
                  (jupyter-request-id req))
        (jupyter-wait-until-idle req most-positive-fixnum)
        ;; Finalize the list of results
        (setq results (nreverse results))
        (if (eq result-type 'output) (mapconcat #'identity results "\n")
          (let* ((result (org-babel-jupyter--transform-result
                          (car results) kernel-lang))
                 (render-param (car result))
                 (result (cdr result)))
            (org-babel-jupyter--inject-render-param render-param params)
            (prog1 result
              ;; Insert remaining results after the first one has been
              ;; inserted.
              (when (cdr results)
                (run-at-time
                 0.01 nil
                 (lambda ()
                   (org-babel-jupyter--clear-render-param render-param params)
                   (org-babel-jupyter--inject-render-param "append" params)
                   (org-babel-jupyter-insert-results
                    (cdr results) params kernel-lang)))))))))))

(defun org-babel-jupyter-make-language-alias (kernel lang)
  "Simimilar to `org-babel-make-language-alias' but for Jupyter src-blocks.
KERNEL should be the name of a the default kernel using for the
kernel LANG. All necessary org-babel functions for a language
with the name jupyter-LANG will be aliased to the jupyter
functions."
  (dolist (fn '("execute" "expand-body" "prep-session" "edit-prep"
                "variable-assignments" "load-session"))
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
   ;; Only make aliases the first time a new language appears
   unless (functionp (intern (concat "org-babel-execute:jupyter-" lang)))
   do (org-babel-jupyter-make-language-alias kernel lang)
   ;; (add-to-list 'org-babel-tangle-lang-exts
   ;;              (cons (concat "jupyter-" lang) file_extension))
   (add-to-list 'org-src-lang-modes
                (cons (concat "jupyter-" lang)
                      (intern (or (cdr (assoc lang org-src-lang-modes))
                                  (replace-regexp-in-string
                                   "[0-9]*" "" lang)))))))

(org-babel-jupyter-aliases-from-kernelspecs)

(provide 'ob-jupyter)

;;; ob-jupyter.el ends here
