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
If METADATA has ':isolated' tag (e.g. DT::datatable()), save it to a file
and open in a browser. Otherwise, try to use pandoc or libxml to convert
DATA to a prettier form."
  (cond
   ((plist-get metadata :isolated)
      (let ((file (or (alist-get :file params)
                      (jupyter-org-image-file-name data ".html"))))
        (with-temp-file file
          (insert data))
        (browse-url-of-file file)
        (jupyter-org-file-link file)))
   ((functionp 'pandoc-convert-stdio)
    (jupyter-org-raw-string (pandoc-convert-stdio data "html" "org")))
   ((functionp 'libxml-parse-html-region)
    (with-temp-buffer
      (insert data)
      (let ((parsed-html (libxml-parse-html-region
                          (point-min) (point-max))))
        (erase-buffer)
        (shr-insert-document parsed-html)
        (buffer-string))))
   (t (cl-call-next-method))))

(provide 'jupyter-R)

;;; jupyter-R.el ends here
