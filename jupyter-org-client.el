;;; jupyter-org-client.el --- Org integration -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 02 Jun 2018
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

;;

;;; Code:

(require 'jupyter-repl)
(require 'ob)

(declare-function org-at-drawer-p "org")
(declare-function org-in-regexp "org" (regexp &optional nlines visually))
(declare-function org-in-src-block-p "org" (&optional inside))
(declare-function org-element-at-point "org-element")
(declare-function org-element-property "org-element")
(declare-function org-babel-python-table-or-string "ob-python" (results))
(declare-function org-babel-jupyter-initiate-session "ob-jupyter" (&optional session params))

(defcustom jupyter-org-resource-directory "./.ob-jupyter/"
  "Directory used to store automatically generated image files.
See `jupyter-org-image-file-name'."
  :group 'ob-jupyter
  :type 'string)

(defconst jupyter-org-mime-types '(:text/org
                                   ;; Prioritize images over html
                                   :image/svg+xml :image/jpeg :image/png
                                   :text/html :text/markdown
                                   :text/latex :text/plain)
  "MIME types handled by Jupyter Org.")

(defclass jupyter-org-client (jupyter-repl-client)
  ((block-params
    :initform nil
    :documentation "The parameters of the most recently executed
source code block. Set by `org-babel-execute:jupyter'.")))

(cl-defstruct (jupyter-org-request
               (:include jupyter-request)
               (:constructor nil)
               (:constructor jupyter-org-request))
  result-type
  block-params
  results
  silent
  id-cleared-p
  marker
  async)

;;; Predicates

(defun jupyter-org-file-header-arg-p (req)
  "Determine if the source block of REQ specifies a file header argument."
  (let ((params (jupyter-org-request-block-params req)))
    (member "file" (assq :result-params params))))

;;; `jupyter-kernel-client' interface

(cl-defmethod jupyter-generate-request ((client jupyter-org-client) _msg
                                        &context (major-mode (eql org-mode)))
  "Return a `jupyter-org-request' for the current source code block."
  (let* ((block-params (oref client block-params))
         (result-params (alist-get :result-params block-params)))
    (jupyter-org-request
     :marker (copy-marker org-babel-current-src-block-location)
     :result-type (alist-get :result-type block-params)
     :block-params block-params
     :async (equal (alist-get :async block-params) "yes")
     :silent (car (or (member "none" result-params)
                      (member "silent" result-params))))))

(cl-defmethod jupyter-drop-request ((_client jupyter-org-client)
                                    (req jupyter-org-request))
  (set-marker (jupyter-org-request-marker req) nil))

(cl-defmethod jupyter-handle-stream ((_client jupyter-org-client)
                                     (req jupyter-org-request)
                                     name
                                     text)
  (and (eq (jupyter-org-request-result-type req) 'output)
       (equal name "stdout")
       (jupyter-org--add-result req (ansi-color-apply text))))

(cl-defmethod jupyter-handle-status ((_client jupyter-org-client)
                                     (req jupyter-org-request)
                                     execution-state)
  (when (and (jupyter-org-request-async req)
             (equal execution-state "idle"))
    (jupyter-org--clear-request-id req)
    (run-hooks 'org-babel-after-execute-hook)))

(cl-defmethod jupyter-handle-error ((_client jupyter-org-client)
                                    (req jupyter-org-request)
                                    ename
                                    evalue
                                    traceback)
  ;; Clear the file parameter to prevent showing the error as a file link
  (when (jupyter-org-file-header-arg-p req)
    (let ((params (jupyter-org-request-block-params req)))
      (setcar (member "file" (assq :result-params params)) "scalar")))
  (let ((emsg (format "%s: %s" ename (ansi-color-apply evalue))))
    (jupyter-with-output-buffer "traceback" 'reset
      (jupyter-insert-ansi-coded-text
       (mapconcat #'identity traceback "\n"))
      (goto-char (line-beginning-position))
      (pop-to-buffer (current-buffer)))
    (jupyter-org--add-result req emsg)))

(cl-defmethod jupyter-handle-execute-result ((_client jupyter-org-client)
                                             (req jupyter-org-request)
                                             _execution-count
                                             data
                                             metadata)
  (unless (eq (jupyter-org-request-result-type req) 'output)
    (jupyter-org--add-result req (jupyter-org-result req data metadata))))

(cl-defmethod jupyter-handle-display-data ((_client jupyter-org-client)
                                           (req jupyter-org-request)
                                           data
                                           metadata
                                           ;; TODO: Add request objects as text
                                           ;; properties of source code blocks
                                           ;; to implement display IDs. Or how
                                           ;; can #+NAME be used as a display
                                           ;; ID?
                                           _transient)
  (unless (eq (jupyter-org-request-result-type req) 'output)
    (jupyter-org--add-result req (jupyter-org-result req data metadata))))

(cl-defmethod jupyter-handle-execute-reply ((client jupyter-org-client)
                                            (_req jupyter-org-request)
                                            _status
                                            execution-count
                                            _user-expressions
                                            payload)
  ;; TODO: Re-use the REPL's handler somehow?
  (oset client execution-count (1+ execution-count))
  (when payload
    (jupyter-repl--handle-payload payload)))


;;; Completions in code blocks

(cl-defmethod jupyter-completion-prefix (&context (major-mode org-mode))
  (when (org-in-src-block-p 'inside)
    (let* ((el (org-element-at-point))
           (lang (org-element-property :language el))
           info params syntax client)
      (when (string-prefix-p "jupyter-" lang)
        (setq info (org-babel-get-src-block-info el)
              params (nth 2 info)
              client (with-current-buffer
                         (org-babel-jupyter-initiate-session
                          (alist-get :session params) params)
                       (setq syntax (syntax-table))
                       jupyter-current-client))
        ;; KLUDGE: Remove the need for setting
        ;; `jupyter-current-client', its needed so
        ;; that `jupyter-completion-prefetch' will use the
        ;; right client, similarly for the less specialized
        ;; `jupyter-completion-prefix'
        (setq jupyter-current-client client)
        ;; Use the syntax table of the language when
        ;; retrieving the prefix
        (with-syntax-table syntax
          (cl-call-next-method))))))

(cl-defmethod jupyter-code-context ((_type (eql inspect))
                                    &context (major-mode org-mode))
  (when (org-in-src-block-p 'inside)
    (jupyter-line-context)))

(cl-defmethod jupyter-code-context ((_type (eql completion))
                                    &context (major-mode org-mode))
  (when (org-in-src-block-p 'inside)
    (jupyter-line-context)))

(defun jupyter-org-enable-completion ()
  "Enable autocompletion in Jupyter source code blocks."
  (add-hook 'completion-at-point-functions 'jupyter-completion-at-point nil t))

(add-hook 'org-mode-hook 'jupyter-org-enable-completion)

;;; Inserting results

(defun jupyter-org-image-file-name (data ext)
  "Return a file name based on DATA and EXT.
`jupyter-org-resource-directory' is used as the directory name of
the file, the `sha1' hash of DATA is used as the base name, and
EXT is used as the extension."
  (let ((dir (prog1 jupyter-org-resource-directory
               (unless (file-directory-p jupyter-org-resource-directory)
                 (make-directory jupyter-org-resource-directory))))
        (ext (if (= (aref ext 0) ?.) ext
               (concat "." ext))))
    (concat (file-name-as-directory dir) (sha1 data) ext)))

(defun jupyter-org--image-result (mime params data)
  "Return a cons cell (\"file\" . FILENAME).
MIME is the image mimetype, PARAMS is the
`jupyter-org-request-block-params' that caused this result to be
returned and DATA is the image DATA.

If PARAMS contains a :file key, it is used as the FILENAME.
Otherwise a file name is created, see
`jupyter-org-image-file-name'. Note, when PARAMS contains a :file
key any existing file with the file name will be overwritten when
writing the image data. This is not the case when a new file name
is created."
  (let* ((overwrite (not (null (alist-get :file params))))
         (base64-encoded (memq mime '(:image/png :image/jpeg)))
         (file (or (alist-get :file params)
                   (jupyter-org-image-file-name
                    data (cl-case mime
                           (:image/png "png")
                           (:image/jpeg "jpg")
                           (:image/svg+xml "svg"))))))
    (when (or overwrite (not (file-exists-p file)))
      (let ((buffer-file-coding-system
             (if base64-encoded 'binary
               buffer-file-coding-system))
            (require-final-newline nil))
        (with-temp-file file
          (insert data)
          (when base64-encoded
            (base64-decode-region (point-min) (point-max))))))
    (cons "file" file)))

(cl-defmethod jupyter-org-result ((req jupyter-org-request) data &optional metadata)
  "For REQ, return the rendered DATA.
DATA is a plist, (:mimetype1 value1 ...), containing the
different representations of a result returned by a kernel.

METADATA is the metadata plist used to render DATA with, as
returned by the Jupyter kernel. This plist typically contains
information such as the size of an image to be rendered. The
metadata plist is currently unused.

Using the `jupyter-org-request-block-params' of REQ, loop over
the MIME types in `jupyter-org-mime-types' calling

    (jupyter-org-result MIME PARAMS DATA METADATA)

for each MIME type. Return the result of the first iteration in
which the above call returns a non-nil value. PARAMS is the REQ's
`jupyter-org-request-block-params', DATA and METADATA are the
data and metadata of the current MIME type."
  (cl-assert data json-plist)
  (let* ((params (jupyter-org-request-block-params req)))
    (or (jupyter-loop-over-mime
            jupyter-org-mime-types mime data metadata
          (jupyter-org-result mime params data metadata))
        (prog1 nil
          (warn "No valid mimetype found %s"
                (cl-loop for (k _v) on data by #'cddr collect k))))))

(cl-defgeneric jupyter-org-result (_mime _params _data &optional _metadata)
  "Return a pair, (RENDER-PARAM . RESULT), used to display MIME DATA.
RENDER-PARAM is either a result parameter, i.e. one of the result
parameters of `org-babel-insert-result', or a key value pair
which will be appended to PARAMS when rendering RESULT in the
`org-mode' buffer.

DATA is the data representing the MIME type and METADATA is any
associated metadata associated with the MIME DATA.

As an example, if DATA only contains the mimetype
`:text/markdown', then RESULT-PARAM should be something like

    (:wrap . \"SRC markdown\")

and RESULT will be the markdown text which should be wrapped in
an \"EXPORT markdown\" block. See `org-babel-insert-result'."
  (ignore))

(cl-defmethod jupyter-org-result ((_mime (eql :application/vnd.jupyter.widget-view+json))
                                  _params _data &optional _metadata)
  (cons "scalar" "Widget"))

(cl-defmethod jupyter-org-result ((_mime (eql :text/org)) params data
                                  &optional _metadata)
  (let ((result-params (alist-get :result-params params)))
    (cons (if (member "raw" result-params) "scalar" "org") data)))

(cl-defmethod jupyter-org-result ((mime (eql :image/png)) params data
                                  &optional _metadata)
  ;; TODO: Add ATTR_ORG with the width and height. This can be done for example
  ;; by adding a function to `org-babel-after-execute-hook'.
  (jupyter-org--image-result mime params data))

(cl-defmethod jupyter-org-result ((mime (eql :image/jpeg)) params data
                                  &optional _metadata)
  (jupyter-org--image-result mime params data))

(cl-defmethod jupyter-org-result ((mime (eql :image/svg+xml)) params data
                                  &optional _metadata)
  (jupyter-org--image-result mime params data))

(cl-defmethod jupyter-org-result ((_mime (eql :text/markdown)) _params data
                                  &optional _metadata)
  (cons '(:wrap . "SRC markdown") data))

(cl-defmethod jupyter-org-result ((_mime (eql :text/latex)) params data
                                  &optional _metadata)
  (let ((result-params (alist-get :result-params params)))
    (cons (if (member "raw" result-params) "scalar" "latex") data)))

(cl-defmethod jupyter-org-result ((_mime (eql :text/html)) _params data
                                  &optional _metadata)
  (cons "html" data))

;; NOTE: The order of :around methods is that the more specialized wraps the
;; more general, this makes sense since it is how the primary methods work as
;; well.
;;
;; Using an :around method to attempt to guarantee that this is called as the
;; outer most method. Kernel languages should extend the primary method.
(cl-defmethod jupyter-org-result :around ((_mime (eql :text/plain)) &rest _)
  "Do some final transformations of the result.
Call the next method, if it returns \"scalar\" results, return a
new \"scalar\" result with the result of calling
`org-babel-script-escape' on the old result."
  (let ((result (cl-call-next-method)))
    (cond
     ((and (equal (car result) "scalar")
           (stringp (cdr result)))
      (cons "scalar" (org-babel-script-escape (cdr result))))
     (t result))))

(cl-defmethod jupyter-org-result ((_mime (eql :text/plain)) _params data
                                  &optional _metadata)
  (cons "scalar" data))

;; NOTE: The parameters are destructively added to the result parameters passed
;; to `org-babel-insert-result' in order to avoid advising
;; `org-babel-execute-src-block', but this might be the way to go to avoid
;; depending on the the priority of result parameters in
;; `org-babel-insert-result'.
(defun jupyter-org--inject-render-param (render-param params)
  "Destructively modify result parameters for `org-babel-insert-result'.
RENDER-PARAM is the first element of the cons cell returned by
`jupyter-org-result', PARAMS are the parameters passed to
`org-babel-execute:jupyter'.

Append RENDER-PARAM to the :result-params of PARAMS if it is a
string. Otherwise, if RENDER-PARAM is a cons cell

    (KEYWORD . STRING)

append RENDER-PARAM to PARAMS."
  (cond
   ((consp render-param)
    (nconc params (list render-param)))
   ((stringp render-param)
    (let ((rparams (alist-get :result-params params)))
      ;; `org-babel-insert-result' looks for replace first, thus we have to
      ;; remove it if we are injecting append or prepend.
      ;;
      ;; TODO: Do the inverse operation in
      ;; `jupyter-org--clear-render-param'. This may not really be
      ;; necessary since this will only be injected for async results.
      (if (and (member render-param '("append" "prepend"))
               (member "replace" rparams))
          (setcar (member "replace" rparams) render-param)
        (nconc rparams (list render-param)))))
   ((not (null render-param))
    (error "Render parameter unsupported (%s)" render-param))))

(defun jupyter-org--clear-render-param (render-param params)
  "Destructively modify result parameters.
Remove RENDER-PARAM from PARAMS or from the result parameters
found in PARAMS. If RENDER-PARAM is a cons cell, remove it from
the PARAMS list. If RENDER-PARAM is a string, remove it from the
`:result-params' of PARAMS."
  (cond
   ((consp render-param)
    (delq render-param params))
   ((stringp render-param)
    (delq render-param (alist-get :result-params params)))
   ((not (null render-param))
    (error "Render parameter unsupported (%s)" render-param))))

(defun jupyter-org--clear-request-id (req)
  "Delete the ID of REQ in the `org-mode' buffer if present."
  (unless (jupyter-org-request-id-cleared-p req)
    (org-with-point-at (jupyter-org-request-marker req)
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
    (setf (jupyter-org-request-id-cleared-p req) t)))

(defun jupyter-org--add-result (req result)
  "For a request made with CLIENT, add RESULT.
REQ is a `jupyter-org-request' and if the request is a
synchronous request, RESULT will be pushed to the list of results
in the request's results slot. Otherwise, when the request is
asynchronous, RESULT is inserted at the location of the code
block for the request."
  ;; TODO: Figure out how to handle result-type output in the
  ;; async case. Should the output be pooled and displayed when
  ;; finished? No I don't think so. It should be appended to the
  ;; current output but for multiline output that is received
  ;; this will end up either putting it in an example block and
  ;; you would have multiple example blocks for a single output.
  ;; The best bet would be to insert it as raw text in a drawer.
  (unless (equal (jupyter-org-request-silent req) "none")
    (or (consp result) (setq result (cons "scalar" result))))
  (if (jupyter-org-request-silent req)
      (unless (equal (jupyter-org-request-silent req) "none")
        ;; TODO: Process the result before displaying
        (message "%s" (cdr result)))
    (if (jupyter-org-request-async req)
        (let ((params (jupyter-org-request-block-params req)))
          (org-with-point-at (jupyter-org-request-marker req)
            (jupyter-org--clear-request-id req)
            (jupyter-org--insert-results result params))
          (unless (member "append" (assq :result-params params))
            (jupyter-org--inject-render-param "append" params)))
      (push result (jupyter-org-request-results req)))))

(defun jupyter-org--insert-results (results params)
  "Insert RESULTS at the current source block location.
RESULTS is either a single cons cell or a list of such cells,
each cell having the form

    (RENDER-PARAM . RESULT)

As is returned by `jupyter-org-result'. PARAMS are the parameters
passed to `org-babel-execute:jupyter'.

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
   ;; NOTE: This relies on `org-babel-insert-result' only
   ;; caring about the parameters of the info and not
   ;; anything else.
   with info = (list nil nil params)
   with result-params = (alist-get :result-params params)
   for (render-param . result) in results
   do (jupyter-org--inject-render-param render-param params)
   (cl-letf (((symbol-function 'message) #'ignore))
     (org-babel-insert-result result result-params info))
   (jupyter-org--clear-render-param render-param params)))

(defun jupyter-org-insert-sync-results (client req)
  "For CLIENT, insert the results of REQ.
Meant to be used as the return value of `org-babel-execute:jupyter'."
  (let ((results (nreverse (jupyter-org-request-results req)))
        (params (jupyter-org-request-block-params req)))
    (cl-destructuring-bind (render-param . result) (car results)
      (jupyter-org--inject-render-param render-param params)
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
             (org-with-point-at (jupyter-org-request-marker req)
               (let ((params (jupyter-org-request-block-params req))
                     (jupyter-current-client client))
                 (jupyter-org--clear-render-param render-param params)
                 (jupyter-org--inject-render-param "append" params)
                 (jupyter-org--insert-results (cdr results) params)
                 (set-marker (jupyter-org-request-marker req) nil))))))))))

(provide 'jupyter-org-client)

;;; jupyter-org-client.el ends here
