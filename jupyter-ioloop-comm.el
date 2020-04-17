;;; jupyter-ioloop-comm.el --- Communication layer using jupyter-ioloop -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 27 Jun 2019

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

;; Implement the `jupyter-comm-layer' interface on-top of a `jupyter-ioloop'.
;; Note this class only implements a subset of the `jupyter-comm-layer'
;; interface needed for a `jupyter-kernel-client' and is usually sub-classed to
;; be usable by a `jupyter-kernel-client'.  See `jupyter-channel-ioloop-comm'.

;;; Code:

(require 'jupyter-comm-layer)
(require 'jupyter-ioloop)

(defclass jupyter-ioloop-comm (jupyter-comm-layer)
  ((ioloop :type jupyter-ioloop))
  :abstract t)

(cl-defmethod jupyter-send ((comm jupyter-ioloop-comm) &rest event)
  (apply #'jupyter-send (oref comm ioloop) event))

(cl-defmethod jupyter-comm-start ((comm jupyter-ioloop-comm))
  (with-slots (ioloop) comm
    (unless (jupyter-ioloop-alive-p ioloop)
      (jupyter-ioloop-start
       ioloop (lambda (event)
                (jupyter-event-handler comm event))))))

(cl-defmethod jupyter-comm-stop ((comm jupyter-ioloop-comm))
  (with-slots (ioloop) comm
    (when (jupyter-ioloop-alive-p ioloop)
      (jupyter-ioloop-stop ioloop))))

(cl-defmethod jupyter-comm-alive-p ((comm jupyter-ioloop-comm))
  (and (slot-boundp comm 'ioloop)
       (jupyter-ioloop-alive-p (oref comm ioloop))))

(provide 'jupyter-ioloop-comm)

;;; jupyter-ioloop-comm.el ends here
