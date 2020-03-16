;;; jupyter-server-ioloop-comm.el --- Async support for Jupyter kernel servers -*- lexical-binding: t -*-

;; Copyright (C) 2020 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 11 Mar 2020

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

;; Async support for Jupyter kernel servers. Requires ZMQ.

;;; Code:

(require 'jupyter-server)
(require 'jupyter-ioloop-comm)
(require 'jupyter-server-ioloop)

(defgroup jupyter-server-ioloop-comm nil
  "Async support for Jupyter kernel servers (requires ZMQ)."
  :group 'jupyter)

(defclass jupyter-server-ioloop-comm (jupyter-server jupyter-ioloop-comm)
  ())

(defclass jupyter-server-ioloop-kernel-comm (jupyter-server-abstract-kcomm)
  ())

(provide 'jupyter-server-ioloop-comm)

;;; jupyter-server-ioloop-comm.el ends here
