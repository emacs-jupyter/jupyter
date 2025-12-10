;;; jupyter-seq.el --- Sequence interface -*- lexical-binding: t -*-

;; Copyright (C) 2025 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 5 Dec 2025

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

;;; Code:

(require 'seq)

(cl-defstruct (jupyter-seq
               (:constructor nil)
               (:constructor
                jupyter-seq
                (&key (stream nil))))
  (stream nil :read-only t))

(defun jupyter-message-seq (req)
  (jupyter-seq
   :stream (jupyter-request-message-stream req)))

(cl-defmethod seqp ((_seq jupyter-seq)) t)

(cl-defmethod seq-copy ((seq jupyter-seq))
  (jupyter-seq :stream (jupyter-seq-stream seq)))

(cl-defmethod seq-length ((seq jupyter-seq))
  (let ((i 0)
        (s (jupyter-seq-stream seq)))
    (while (not (jupyter-null-stream-p s))
      (setq s (jupyter-stream-cdr s))
      (cl-incf i))
    i))

(cl-defmethod seq-elt ((seq jupyter-seq) n)
  "Return the Nth element of SEQUENCE."
  (let ((i 0)
        (s (jupyter-seq-stream seq)))
    (while (and (< i n)
                (not (jupyter-null-stream-p s)))
      (setq s (jupyter-stream-cdr s)
            i (1+ i)))
    (if (jupyter-null-stream-p s)
        (error "Stream ended before %dth element." n)
      (jupyter-stream-car s))))

(cl-defmethod seq-do (function (seq jupyter-seq))
  (let ((s (jupyter-seq-stream seq)))
    (while (not (jupyter-null-stream-p s))
      (funcall function (jupyter-stream-car s))
      (setq s (jupyter-stream-cdr s)))))

(cl-defmethod seq-into-sequence ((seq jupyter-seq))
  (seq-map #'identity seq))

(cl-defmethod seq-into ((seq jupyter-seq) type)
  (pcase type
     (`vector (vconcat (seq-into-sequence seq)))
     (`list (seq-into-sequence seq))
     (_ (error "Not a valid sequence type name: %S" type))))

(cl-defmethod seq-subseq ((seq jupyter-seq) start &optional end)
  (let ((s (jupyter-seq-stream seq))
        (result nil))
    (cl-macrolet ((skip-to-start ()
                    `(while (not (zerop start))
                       (cl-decf start)
                       (setq s (jupyter-stream-cdr s))))
                  (collect-until (cond)
                    `(progn
                       (while (not ,cond)
                         (push (jupyter-stream-car s) result)
                         (setq s (jupyter-stream-cdr s)))
                       (nreverse result))))
      (when (< start 0)
        (setq start (+ start (seq-length s))))
      (when (< start 0)
        (error "Start of subsequence out of bounds"))
      (cond
       (end
        (when (< end 0)
          (setq end (+ end (seq-length s))))
        (when (< end 0)
          (error "End of subsequence out of bounds"))
        (skip-to-start)
        (collect-until
         (prog1 (or (jupyter-null-stream-p s)
                    (<= end 0))
           (cl-decf end))))
       (t
        (skip-to-start)
        (collect-until (jupyter-null-stream-p s)))))))

(provide 'jupyter-seq)

;;; jupyter-seq.el ends here
