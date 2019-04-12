;;; jupyter-R.el --- Jupyter support for R -*- lexical-binding: t -*-

;; Copyright (C) 2019 Nathaniel Nicandro

;; Author: Jack Kamm <jackkamm@gmail.com>, Nathaniel Nicandro <nathanielnicandro@gmail.com>

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

;; Support methods for integration with R.

;;; Code:

(require 'jupyter-repl)

(cl-defmethod jupyter-repl-initialize-fontification (&context (jupyter-lang R))
  (when (featurep 'ess)
      (setq-local ess-font-lock-keywords 'ess-R-font-lock-keywords))
  (cl-call-next-method))

(cl-defmethod jupyter-org-result ((_mime (eql :text/html)) params data
                                  &context (jupyter-lang R)
                                  &optional metadata)
  "Parse and convert DATA of type 'text/html' into a prettier form.
If _METADATA has ':isolated' tag (e.g. DT::datatable()), save it to a file
and open in a browser. Otherwise, if libxml is available and DATA
is of type <table>, <ol>, <ul>, or <dl>, try to render it nicely."
  (if (plist-get metadata :isolated)
      ;; handle html with "isolated" property
      (let ((file (or (alist-get :file params)
                      (jupyter-org-image-file-name data ".html"))))
        (with-temp-file file
          (insert data))
        (browse-url-of-file file)
        (jupyter-org-file-link file))
    ;; handle html without "isolated" property
    (if (functionp 'libxml-parse-html-region)
        ;; parse with libxml
        (with-temp-buffer
          (insert data)
          (let* ((parsed-html (libxml-parse-html-region
                               (point-min) (point-max)))
                 (inner-element
                  (jupyter-org-result--R-html-inner-element parsed-html))
                 (inner-element-type (car inner-element)))
            (cl-case (car inner-element)
              (table
               (jupyter-org-result--R-html-table parsed-html))
              ((ol dl ul)
               (jupyter-org-result--R-html-list parsed-html))
              (t (cl-call-next-method)))))
      ;; libxml not available, call next method
      (cl-call-next-method))))

(defun jupyter-org-result--R-html-table (parsed-html)
  "Convert PARSED-HTML into an org-element table."
  (with-temp-buffer
    (shr-insert-document parsed-html)
    (org-table-convert-region (point-min) (point-max) nil)
    (org-table-insert-hline)
    (let* ((tbl (org-table-to-lisp))
           (head (jupyter-org-result--R-html-table-fix-header (car tbl)))
           (body (cdr tbl)))
      (jupyter-org-scalar `(,head . ,body)))))

(defun jupyter-org-result--R-html-table-fix-header (header)
  "Shifts column names to the right if last element of HEADER is empty string.
This fixes the header column alignment when the table has row names."
  (if (equal "" (car (last header)))
      `("" . ,(butlast header))
    header))

(defun jupyter-org-result--R-html-list (parsed-html)
  "Renders list-like PARSED-HTML.
Since these can have arbitrary nested structure, we don't try to parse it into
an org-element; instead we just render the html.  Note this is still a bit
uglier than ':display plain', which should be preferred for these outputs."
  (with-temp-buffer
    (shr-insert-document parsed-html)
    (buffer-string)))

(defun jupyter-org-result--R-html-inner-element (parsed-html)
  "If PARSED-HTML consists of a body with 1 element, return that element.
Otherwise, return nil."
  (let ((html-subnodes (cdr (cdr parsed-html))))
    ;; check there is 1 html-subnode and it is the body
    (when (eq 1 (length html-subnodes))
      (let ((body (car html-subnodes)))
        (when (eq (car body) 'body)
          ;; check the body has 1 subnode and return it
          (let ((body-subnodes (cdr (cdr body))))
            (when (eq 1 (length body-subnodes))
              (car body-subnodes))))))))

(provide 'jupyter-R)

;;; jupyter-R.el ends here
