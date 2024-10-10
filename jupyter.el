;;; jupyter.el --- Jupyter -*- lexical-binding: t -*-

;; Copyright (C) 2018-2024 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 11 Jan 2018
;; Version: 1.0
;; Package-Requires: ((emacs "27") (cl-lib "0.5") (org "9.1.6") (zmq "0.10.10") (simple-httpd "1.5.0") (websocket "1.9"))
;; URL: https://github.com/emacs-jupyter/jupyter

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

;; An interface for communicating with Jupyter kernels.

;;; Code:

(defgroup jupyter nil
  "Jupyter"
  :group 'processes)

(require 'jupyter-base)
(require 'jupyter-client)
(require 'jupyter-kernelspec)
(require 'jupyter-server)
(require 'jupyter-repl)

(provide 'jupyter)

;;; jupyter.el ends here
