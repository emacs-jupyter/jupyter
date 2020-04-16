;;; jupyter-R.el --- Jupyter support for R -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Nathaniel Nicandro

;; Author: Jack Kamm <jackkamm@gmail.com>
;;         Nathaniel Nicandro <nathanielnicandro@gmail.com>

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

;; Support methods for integration with R.

;;; Code:

(require 'jupyter-repl)
(require 'jupyter-org-client)
(require 'jupyter-mime)

(defvar ess-font-lock-keywords)

(cl-defmethod jupyter-repl-initialize-fontification (&context (jupyter-lang R))
  (when (featurep 'ess)
    (setq-local ess-font-lock-keywords 'ess-R-font-lock-keywords))
  (cl-call-next-method))

(cl-defmethod jupyter-org-result ((_mime (eql :text/html)) content params
                                  &context (jupyter-lang R))
  "If html DATA is an iframe, save it to a separate file and open in browser.
Otherwise, parse it as normal."
  (if (plist-get (plist-get content :metadata) :isolated)
      (let* ((data (plist-get content :data))
             (file (or (alist-get :file params)
                       (jupyter-org-image-file-name data ".html"))))
        (with-temp-file file
          (insert data))
        (browse-url-of-file file)
        (jupyter-org-file-link file))
    (cl-call-next-method)))

(cl-defmethod jupyter-insert ((_mime (eql :text/html)) data
                              &context (jupyter-lang R)
                              &optional metadata)
  (if (plist-get metadata :isolated)
      (jupyter-browse-url-in-temp-file data)
    (cl-call-next-method)))

(provide 'jupyter-R)

;;; jupyter-R.el ends here
