;;; jupyter-org-extensions.el --- Jupyter Org Extensions -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Carlos Garcia C. <carlos@binarycharly.com>
;; Created: 01 March 2019
;; Version: 0.7.1

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

;; Functions that extend the functionality of Org mode to interact with
;; jupyter source blocks.

;;; Code:

(require 'org)
(require 'jupyter-org-client)

(declare-function org-babel-jupyter-initiate-session "ob-jupyter" (&optional session params))
(declare-function avy-with nil)
(declare-function avy-jump nil)
(declare-function ivy-read nil)

;;;###autoload
(defun jupyter-org-insert-src-block (&optional below)
  "Insert a src block above the current point.
With prefix arg BELOW, insert it below the current point.

If point is in a block, copy the header to the new block"
  (interactive "P")
  (if (org-in-src-block-p)
      (let* ((src (org-element-context))
             (start (org-element-property :begin src))
             (end (org-element-property :end src))
             (lang (org-element-property :language src))
             (switches (org-element-property :switches src))
             (parameters (org-element-property :parameters src))
             location)
        (if below
            (progn
              (goto-char start)
              (setq location (org-babel-where-is-src-block-result))
              (if (not location)
                  (goto-char end)
                (goto-char location)
                (goto-char (org-element-property :end (org-element-context))))
              (insert
               (org-element-interpret-data
                (org-element-put-property
                 (jupyter-org-src-block lang parameters "\n" switches)
                 :post-blank 1)))
              (forward-line -3))
          ;; after current block
          (goto-char (org-element-property :begin src))
          (insert
           (org-element-interpret-data
            (org-element-put-property
             (jupyter-org-src-block lang parameters "\n" switches)
             :post-blank 1)))
          (forward-line -3)))

    ;; not in a src block, insert a new block, query for jupyter kernel
    (beginning-of-line)
    (let ((kernelspec (jupyter-completing-read-kernelspec)))
      (insert
       (org-element-interpret-data
        (org-element-put-property
         (jupyter-org-src-block
             (format "jupyter-%s" (plist-get (cddr kernelspec) :language)) "" "\n")
         :post-blank 0))))
    (forward-line -2)))

;;;###autoload
(defun jupyter-org-split-src-block (&optional below)
  "Split the current src block with point in upper block.

With a prefix BELOW move point to lower block."
  (interactive "P")
  (unless (org-in-src-block-p)
    (error "Not in a source block"))
  (beginning-of-line)
  (org-babel-demarcate-block)
  (if below
      (progn
        (org-babel-next-src-block)
        (forward-line)
        (open-line 1))
    (forward-line -2)
    (end-of-line)))

;;;###autoload
(defun jupyter-org-execute-and-next-block (&optional new)
  "Execute his block and jump or add a new one.

If a new block is created, use the same language, switches and parameters.
With prefix arg NEW, always insert new cell."
  (interactive "P")
  (unless (org-in-src-block-p)
      (error "Not in a source block"))
  (org-babel-execute-src-block)
  ;; add a new src block if there is no next one to jump to
  (unless (or new (ignore-errors (org-babel-next-src-block)))
    ;; FIXME: without this, the new block is added as part of the results of the last block
    (sleep-for 0.2)
    (jupyter-org-insert-src-block t)))

;;;###autoload
(defun jupyter-org-execute-to-point ()
  "Execute all the src blocks that start before point."
  (interactive)
  (let ((p (point)))
    (save-excursion
      (goto-char (point-min))
      (while (and (org-babel-next-src-block) (< (point) p))
        (org-babel-execute-src-block)))))

;;;###autoload
(defun jupyter-org-restart-kernel-execute-block ()
  "Restart the kernel of the source block where point is and execute it."
  (interactive)
  (jupyter-org-with-src-block-client
   (jupyter-repl-restart-kernel))
  (org-babel-execute-src-block-maybe))

;;;###autoload
(defun jupyter-org-restart-and-execute-to-point ()
  "Kill the kernel and run src-blocks to point."
  (interactive)
  (jupyter-org-with-src-block-client
   (jupyter-repl-restart-kernel))
  (jupyter-org-execute-to-point))

;;;###autoload
(defun jupyter-org-restart-kernel-execute-buffer ()
  "Restart kernel and execute buffer."
  (interactive)
  (jupyter-org-with-src-block-client
   (jupyter-repl-restart-kernel))
  (org-babel-execute-buffer))

;;;###autoload
(defun jupyter-org-jump-to-block (&optional N)
  "Jump to a block in the buffer.
If narrowing is in effect, only a block in the narrowed region.
Use a numeric prefix N to specify how many lines of context to use.
Defaults to 3."
  (interactive "p")
  (unless (require 'ivy nil t)
    (error "Package `ivy' not installed"))
  (let ((p '()))
    (when (= 1 N) (setq N 3))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-babel-src-block-regexp nil t)
        (push (list (format "line %s:\n%s"
                            (line-number-at-pos (match-beginning 0))
                            (save-excursion
                              (goto-char (match-beginning 0))
                              (let ((s (point)))
                                (forward-line N)
                                (buffer-substring s (point)))))
                    (line-number-at-pos (match-beginning 0)))
              p)))
    (ivy-read "block: " (reverse p)
              :action (lambda (candidate)
                        (goto-char (point-min))
                        (forward-line (1- (nth 1 candidate)))
                        (ignore-errors (outline-show-entry))
                        (recenter)))))

;;;###autoload
(defun jupyter-org-jump-to-visible-block ()
  "Jump to a visible src block with avy."
  (interactive)
  (unless (require 'avy nil t)
    (error "Package `avy' not installed"))
  (avy-with #'jupyter-org-jump-to-block
            (avy-jump "#\\+begin_src" :beg (point-min) :end (point-max))))

;;;###autoload
(defun jupyter-org-edit-header ()
  "Edit the src-block header in the minibuffer."
  (interactive)
  (let ((src-info (org-babel-get-src-block-info 'light)))
    (unless src-info
      (error "Not in a source block"))
    (let* ((header-start (nth 5 src-info))
           (header-end (save-excursion (goto-char header-start)
                                       (line-end-position))))
      (setf (buffer-substring header-start header-end)
            (read-string "Header: "
                         (buffer-substring header-start header-end))))))

(defun jupyter-org-select-block-and-results ()
  "Return the region that contains a source block and its results (if any).

Return the region as (BEGIN . END)"
  (unless (org-in-src-block-p)
    (error "Not in a source block"))
  (let* ((src (org-element-context))
         (results-start (org-babel-where-is-src-block-result))
         (results-end
          (when results-start
            (save-excursion
              (goto-char results-start)
              (goto-char (org-babel-result-end))
              (point)))))
    `(,(org-element-property :begin src) .
      ,(or results-end (jupyter-org-element-end-before-blanks src)))))

;;;###autoload
(defun jupyter-org-kill-block-and-results ()
  "Kill the block and its results."
  (interactive)
  (let ((region (jupyter-org-select-block-and-results)))
    (kill-region (car region) (cdr region))))

;;;###autoload
(defun jupyter-org-copy-block-and-results ()
  "Copy the src block at the current point and its results."
  (interactive)
  (let ((region (jupyter-org-select-block-and-results)))
    (kill-new (buffer-substring (car region) (cdr region)))))

;;;###autoload
(defun jupyter-org-clone-block (&optional below)
  "Clone the block above the current block.

If BELOW is non-nil, add the cloned block below."
  (interactive "P")
  (let* ((src (org-element-context))
         (code (org-element-property :value src)))
    (unless (org-in-src-block-p)
      (error "Not in a source block"))
    (jupyter-org-insert-src-block below)
    (delete-char 1)
    (insert code)
    ;; move to the end of the last line of the cloned block
    (forward-line -1)
    (end-of-line)))

;;;###autoload
(defun jupyter-org-merge-blocks ()
  "Merge the current block with the next block."
  (interactive)
  (unless (org-in-src-block-p)
    (error "Not in a source block"))
  (let ((current-src-block (org-element-context)))
    (org-babel-remove-result)
    (org-babel-next-src-block)
    (let* ((next-src-block (prog1 (org-element-context)
                   (org-babel-remove-result)))
           (next-src-block-beg (set-marker
                      (make-marker)
                      (org-element-property :begin next-src-block)))
           (next-src-block-end (set-marker
                      (make-marker)
                      (jupyter-org-element-end-before-blanks next-src-block))))
      (goto-char (jupyter-org-element-end-before-blanks current-src-block))
      (forward-line -1)
      (insert
       (delete-and-extract-region
        (save-excursion
          (goto-char (jupyter-org-element-begin-after-affiliated next-src-block))
          (forward-line 1)
          (point))
        (save-excursion
          (goto-char next-src-block-end)
          (forward-line -1)
          (point))))
      ;; delete a leftover space
      (save-excursion
        (goto-char next-src-block-end)
        (when (looking-at-p "[[:space:]]*$")
          (set-marker next-src-block-end (+ (point-at-eol) 1))))
      (delete-region next-src-block-beg next-src-block-end)
      (set-marker next-src-block-beg nil)
      (set-marker next-src-block-end nil)))
  ;; move to the end of the last line
  (forward-line -1)
  (end-of-line))

;;;###autoload
(defun jupyter-org-move-src-block (&optional below)
  "Move source block before of after another.

If BELOW is non-nil, move the block down, otherwise move it up."
  (interactive)
  (unless (org-in-src-block-p)
    (error "Not in a source block"))
  ;; throw error if there's no previous or next source block
  (when (ignore-errors
          (save-excursion
            (if below
                (org-babel-next-src-block)
              (org-babel-previous-src-block))))
    (let* ((region (jupyter-org-select-block-and-results))
           (block (delete-and-extract-region (car region) (cdr region))))
      ;; if there is an empty line remaining, take that line as part of the
      ;; ... block
      (when (and (looking-at-p "[[:space:]]*$") (/= (point) (point-max)))
        (delete-region (point-at-bol) (+ (point-at-eol) 1))
        (setq block (concat block "\n")))
      (if below
          ;; if below, move past the next source block or its result
          (let ((next-src-block-head (org-babel-where-is-src-block-head)))
            (if next-src-block-head
                (goto-char next-src-block-head)
              (org-babel-next-src-block))
            (let ((next-src-block (org-element-context))
                  (next-results-start (org-babel-where-is-src-block-result)))
              (if next-results-start
                  (progn
                    (goto-char next-results-start)
                    (goto-char (org-babel-result-end))
                    (when (looking-at-p "[[:space:]]*$")
                      (forward-line 1)))
                (goto-char (org-element-property :end next-src-block)))))
        ;; else, move to the begining of the previous block
        (org-babel-previous-src-block))
      ;; keep cursor where the insertion takes place
      (save-excursion (insert block)))))

;;;###autoload
(defun jupyter-org-clear-all-results ()
  "Clear all results in the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (org-babel-next-src-block)
      (org-babel-remove-result))))

(defun jupyter-org-hydra/body ()
  "Hack to bind a hydra only if the hydra package exists."
  (interactive)
  (unless (require 'hydra nil t)
    (error "Package `hydra' not installed"))
  ;; unbinding this function and define the hydra
  (fmakunbound 'jupyter-org-hydra/body)
  (eval `(defhydra jupyter-org-hydra (:color blue :hint nil)
    "
          Execute                 Navigate       Edit             Misc
------------------------------------------------------------------------------
    _<return>_: current           _p_: previous  _C-p_: move up     _/_: inspect
  _C-<return>_: current to next   _n_: next      _C-n_: move down   _l_: clear result
  _M-<return>_: to point          _g_: visible     _x_: kill        _L_: clear all
  _S-<return>_: Restart/block     _G_: any         _c_: copy
_S-C-<return>_: Restart/to point  ^ ^              _o_: clone
_S-M-<return>_: Restart/buffer    ^ ^              _m_: merge
           _r_: Goto repl         ^ ^              _s_: split
           ^ ^                    ^ ^              _+_: insert above
           ^ ^                    ^ ^              _=_: insert below
           ^ ^                    ^ ^              _h_: header"
    ("<return>" org-ctrl-c-ctrl-c :color red)
    ("C-<return>" jupyter-org-execute-and-next-block :color red)
    ("M-<return>" jupyter-org-execute-to-point)
    ("S-<return>" jupyter-org-restart-kernel-execute-block)
    ("S-C-<return>" jupyter-org-restart-and-execute-to-point)
    ("S-M-<return>" jupyter-org-restart-kernel-execute-buffer)
    ("r" org-babel-switch-to-session)

    ("p" org-babel-previous-src-block :color red)
    ("n" org-babel-next-src-block :color red)
    ("g" jupyter-org-jump-to-visible-block)
    ("G" jupyter-org-jump-to-block)

    ("C-p" jupyter-org-move-src-block :color red)
    ("C-n" (jupyter-org-move-src-block t) :color red)
    ("x" jupyter-org-kill-block-and-results)
    ("c" jupyter-org-copy-block-and-results)
    ("o" (jupyter-org-clone-block t))
    ("m" jupyter-org-merge-blocks)
    ("s" jupyter-org-split-src-block)
    ("+" jupyter-org-insert-src-block)
    ("=" (jupyter-org-insert-src-block t))
    ("l" org-babel-remove-result)
    ("L" jupyter-org-clear-all-results)
    ("h" jupyter-org-edit-header)

    ("/" jupyter-inspect-at-point)))
  (call-interactively #'jupyter-org-hydra/body))

(define-key jupyter-org-interaction-mode-map (kbd "C-c h") #'jupyter-org-hydra/body)

(provide 'jupyter-org-extensions)
;;; jupyter-org-extensions.el ends here
