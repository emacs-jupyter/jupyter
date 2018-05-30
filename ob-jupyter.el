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
  "Jupyter integration with org-mdoe"
  :group 'org-babel)

(require 'jupyter)
(require 'ob)

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
        ;; For `org-babel-load-session:jupyter', ensure that the loaded code
        ;; starts on a new line.
        (when no-execute
          (insert "\n")))
      (unless no-execute
        (jupyter-send-execute-request jupyter-repl-current-client))
      (current-buffer))))

(defun org-babel-load-session:jupyter (session body params)
  "In a Jupyter SESSION, load BODY according to PARAMS."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:jupyter session params 'noexecute)))
      (with-current-buffer buffer
        (insert (org-babel-chomp body))
        (current-buffer)))))

(defun org-babel-jupyter-initiate-session-by-key (session params)
  "Return the `jupyter-repl-client' buffer for SESSION.
If SESSION does not have a client already, one is created based
on SESSION and PARAMS. If SESSION ends with \".json\" then
SESSION is interpreted as a kernel connection file and a new
kernel connected to SESSION is created. Otherwise a kernel is run
based on the `:kernel' parameter in PARAMS which should be either
a valid kernel name or a prefix of one. The first kernel that is
returned by `jupyter-find-kernelspecs' will be used."
  (let* ((kernel (alist-get :kernel params))
         (key (concat session "-" kernel))
         (client
          (or (gethash key org-babel-jupyter-session-clients)
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

(defun org-babel-jupyter-file-name (data ext)
  "Return a file name based on DATA and EXT.
`org-babel-jupyter-default-directory' is used as the directory
name, the `sha1' hash of DATA is used as the base name, and EXT
is used as the extension."
  (let ((dir (prog1 org-babel-jupyter-resource-directory
               (unless (file-directory-p org-babel-jupyter-resource-directory)
                 (make-directory org-babel-jupyter-resource-directory))))
        (ext (if (= (aref ext 0) ?.) ext
               (concat "." ext))))
    (concat (file-name-as-directory dir) (sha1 data) ext)))

(defun org-babel-jupyter--image-result (data file &optional overwrite base64-encoded)
  "Possibly write image DATA to FILE.
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
DATA is a plist, (:mimetype1 value1 ...), containing the
different representations of a result returned by a kernel.

METADATA is the metadata plist used to render DATA with, as
returned by the Jupyter kernel. This plist typically contains
information such as the size of an image to be rendered. The
metadata plist is currently unused.

PARAMS is the source block parameter list as passed to
`org-babel-execute:jupyter'. Currently this is used to extract
the file name of an image file when DATA can be rendered as an
image. If no file name is given, one is generated based on the
image data and mimetype, see `org-babel-jupyter-file-name'.
PARAMS is also used to intelligently choose the rendering
parameter used for result insertion.

This function returns a cons cell (RENDER-PARAM . RESULT) where
RENDER-PARAM is either a result parameter, i.e. one of the result
parameters of `org-babel-insert-result', or a key value pair
which should be appended to the PARAMS list when rendering
RESULT.

For example, if DATA only contains the mimetype `:text/markdown',
the RESULT-PARAM will be

    (:wrap . \"SRC markdown\")

and RESULT will be the markdown text which should be wrapped in
an \"EXPORT markdown\" block. See `org-babel-insert-result'."
  (let ((mimetypes (cl-loop for elem in data if (keywordp elem) collect elem))
        (result-params (alist-get :result-params params))
        itype)
    (cond
     ((memq :text/org mimetypes)
      (cons (unless (member "raw" result-params) "org")
            (plist-get data :text/org)))
     ;; TODO: Insert a link which runs code to display the widget
     ((memq :application/vnd.jupyter.widget-view+json mimetypes)
      (cons "scalar" "Widget"))
     ((memq :text/html mimetypes)
      (let ((html (plist-get data :text/html)))
        (save-match-data
          ;; Allow handling of non-string data but with an html mimetype at a
          ;; higher level
          (if (and (stringp html) (string-match "^<img" html))
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
      (cons '(:wrap . "SRC markdown") (plist-get data :text/markdown)))
     ((memq :text/latex mimetypes)
      ;; TODO: Take into account result-params for other mimetypes
      (cons (unless (member "raw" result-params) "latex")
            (plist-get data :text/latex)))
     ((setq itype (cl-find-if (lambda (x) (memq x '(:image/png
                                               :image/jpg
                                               :image/svg+xml)))
                              mimetypes))
      (let* ((data (plist-get data itype))
             (overwrite (not (null (alist-get :file params))))
             (encoded (memq itype '(:image/png :image/jpg)))
             (file (or (alist-get :file params)
                       (org-babel-jupyter-file-name
                        data (cl-case itype
                               (:image/png "png")
                               (:image/jpg "jpg")
                               (:image/svg+xml "svg"))))))
        (org-babel-jupyter--image-result data file overwrite encoded)))
     ((memq :text/plain mimetypes)
      (cons "scalar" (plist-get data :text/plain)))
     (t (warn "No supported mimetype found %s" mimetypes)))))

(defun org-babel-jupyter--inject-render-param (render-param params)
  "Destructively modify result parameters for `org-babel-insert-result'.
RENDER-PARAM is the first element of the list returned by
`org-babel-jupyter-prepare-result', PARAMS are the parameters
passed to `org-babel-execute:jupyter'.

Append RENDER-PARAM to RESULT-PARAMS if it is a string, otherwise
if RENDER-PARAM is a cons cell, (KEYWORD . STRING), append
RENDER-PARAM to the PARAMS."
  (cond
   ((consp render-param)
    (nconc params (list render-param)))
   ((stringp render-param)
    (let ((rparams (alist-get :result-params params)))
      ;; `org-babel-insert-result' looks for replace first, thus we have to
      ;; remove it if we are injecting append or prepend.
      ;;
      ;; TODO: Do the inverse operation in
      ;; `org-babel-jupyter--clear-render-param'. This may not really be
      ;; necessary since this will only be injected for async results.
      (if (and (member render-param '("append" "prepend"))
               (member "replace" rparams))
          (setcar (member "replace" rparams) render-param)
        (nconc rparams (list render-param)))))
   ((not (null render-param))
    (error "Render parameter unsupported (%s)" render-param))))

(defun org-babel-jupyter--clear-render-param (render-param params)
  "Destructively modify result parameters.
Remove RENDER-PARAM from PARAMS or from the result parameters
found in PARAMS. If RENDER-PARAM is a cons cell, remove it from
the PARAMS list. If RENDER-PARAM is a string, remove it from the
`:result-params' of PARAMS. In all cases, `delq' is used for
removal."
  (cond
   ((consp render-param)
    (delq render-param params))
   ((stringp render-param)
    (delq render-param (alist-get :result-params params)))
   ((not (null render-param))
    (error "Render parameter unsupported (%s)" render-param))))

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

RENDER-RESULT is the cons cell returned by
`org-babel-jupyter-prepare-result' and KERNEL-LANG is the kernel
language."
  (let ((render-param (or (car render-result) "scalar"))
        (result (cdr render-result)))
    (cond
     ((and (equal render-param "scalar") (equal kernel-lang "python"))
      (cons "scalar" (when result (org-babel-python-table-or-string result))))
     (t
      (if (equal render-param "scalar")
          (cons "scalar" (when result (org-babel-script-escape result)))
        render-result)))))

(defun org-babel-jupyter-insert-results (results params kernel-lang)
  "Insert RESULTS at the current source block location.
RESULTS is either a single cons cell or a list of such cells,
each cell having the form

    (RENDER-PARAM . RESULT)

They should have been collected by previous calls to
`org-babel-jupyter-prepare-result'. PARAMS are the parameters
passed to `org-babel-execute:jupyter'. KERNEL-LANG is the
language of the kernel that produced RESULTS.

Note that if RESULTS is a list, the last result in the list will
be the one that eventually is shown in the org document. This is
due to how `org-babel-insert-result' works. This behavior can be
modified if the source block has an \"append\" or \"prepend\"
parameter; in this case results will either be appended or
prepended.

The current implementation of `org-babel-execute:jupyter' will
automatically add this parameter internally so under normal use
it does not need to be added by the user."
  ;; Unless this is a list of results
  (unless (car-safe (car results))
    (setq results (list results)))
  (cl-loop
   ;; FIXME: This is a hack that relies on `org-babel-insert-result' only
   ;; caring about the parameters of the info and not anything else.
   with info = (list nil nil params)
   with result-params = (alist-get :result-params params)
   for (render-param . result) in
   (mapcar (lambda (r) (org-babel-jupyter--transform-result r kernel-lang))
      results)
   do (org-babel-jupyter--inject-render-param render-param params)
   (cl-letf (((symbol-function 'message) #'ignore))
     (org-babel-insert-result result result-params info))
   (org-babel-jupyter--clear-render-param render-param params)))

(defun org-babel-execute:jupyter (body params)
  "Execute BODY according to PARAMS.
BODY is the code to execute for the current Jupyter `:session' in
the PARAMS alist."
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
         (req (with-current-buffer repl-buffer
                (goto-char (point-max))
                (jupyter-repl-replace-cell-code code)
                (let ((jupyter-inhibit-handlers
                       '(:stream
                         :execute-reply :execute-result
                         :display-data :error)))
                  (jupyter-send-execute-request client))))
         (no-results (member "none" (alist-get :result-params params))))
    (unless no-results
      (let* ((result-type (alist-get :result-type params))
             (async (equal (alist-get :async params) "yes"))
             (block-beginning (copy-marker org-babel-current-src-block-location))
             (results nil)
             (first-async-insertion t)
             (add-result
              (lambda (result)
                ;; TODO: Figure out how to handle result-type output in the
                ;; async case. Should the output be pooled and displayed when
                ;; finished? No I don't think so. It should be appended to the
                ;; current output but for multiline output that is received
                ;; this will end up either putting it in an example block and
                ;; you would have multiple example blocks for a single output.
                ;; The best bet would be to insert it as raw text in a drawer.
                (unless no-results
                  (or (consp result) (setq result (cons "scalar" result)))
                  (if async
                      (org-with-point-at block-beginning
                        (when first-async-insertion
                          (setq first-async-insertion nil)
                          (org-babel-jupyter--clear-request-id req)
                          (org-babel-jupyter--inject-render-param "append" params))
                        (org-babel-jupyter-insert-results result params kernel-lang))
                    (push result results))))))
        ;; TODO: Handle stream output and errors similar to ob-ipython
        (jupyter-add-callback req
          :stream
          (lambda (msg)
            (and (eq result-type 'output)
                 (equal (jupyter-message-get msg :name) "stdout")
                 (funcall add-result (ansi-color-apply
                                      (jupyter-message-get msg :text)))))
          :status
          (lambda (msg)
            (when (and async (jupyter-message-status-idle-p msg))
              (set-marker block-beginning nil)
              (when first-async-insertion
                (org-babel-jupyter--clear-request-id req))))
          :execute-reply
          (lambda (msg)
            (cl-destructuring-bind (&key status ename evalue traceback
                                         &allow-other-keys)
                (jupyter-message-content msg)
              (unless (equal status "ok")
                ;; HACK: Prevent insertion of a file when an error happens
                (let ((params (member "file" (alist-get :result-params params))))
                  (and params (setcar params "scalar")))
                (if (eq result-type 'output)
                    (funcall add-result (mapconcat #'ansi-color-apply traceback "\n"))
                  (funcall add-result (format "%s: %s" ename (ansi-color-apply evalue)))))
              (when async
                ;; Run the hooks here instead of in the status message to prevent
                ;; any delays
                (org-with-point-at block-beginning
                  (run-hooks 'org-babel-after-execute-hook)))))
          '(:display-data :execute-result)
          (lambda (msg)
            (unless (eq result-type 'output)
              (cl-destructuring-bind (&key data metadata &allow-other-keys)
                  (jupyter-message-content msg)
                (funcall add-result (org-babel-jupyter-prepare-result
                                     data metadata params))))))
        ;; Prevent showing the request ID as a file link for async results
        (let ((fresult (member "file" (assq :result-params params))))
          (when (and async fresult)
            (let ((fparam (assq :file params)))
              (setcar fresult "scalar")
              (when fparam (delq fparam params))
              (cl-labels
                  ((reset-file-param
                    ()
                    (setcar fresult "file")
                    (when fparam (nconc params (list fparam)))
                    (remove-hook 'org-babel-after-execute-hook #'reset-file-param t)))
                (add-hook 'org-babel-after-execute-hook #'reset-file-param nil t)))))
        (if async
            ;; TODO: Use `org-babel-after-execute-hook' to make the id
            ;; read-only.
            (concat (when (member "raw" (assq :result-params params)) ": ")
                    (jupyter-request-id req))
          (jupyter-wait-until-idle req most-positive-fixnum)
          ;; Finalize the list of results
          (setq results (nreverse results))
          (cl-destructuring-bind (render-param . result)
              (org-babel-jupyter--transform-result (car results) kernel-lang)
            (org-babel-jupyter--inject-render-param render-param params)
            (prog1 result
              ;; Insert remaining results after the first one has been
              ;; inserted.
              (when (cdr results)
                ;; TODO: Prevent running the hooks until all results have been
                ;; inserted. Think harder about how to insert a list of
                ;; results.
                (run-at-time
                 0.01 nil
                 (lambda ()
                   (org-with-point-at block-beginning
                     (org-babel-jupyter--clear-render-param render-param params)
                     (org-babel-jupyter--inject-render-param "append" params)
                     (org-babel-jupyter-insert-results (cdr results) params kernel-lang)
                     (set-marker block-beginning nil))))))))))))

(defun org-babel-jupyter-make-language-alias (kernel lang)
  "Simimilar to `org-babel-make-language-alias' but for Jupyter src-blocks.
KERNEL should be the name of the default kernel to use for kernel
LANG. All necessary org-babel functions for a language with the
name jupyter-LANG will be aliased to the jupyter functions."
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
   unless (member lang languages) collect lang into languages and
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
