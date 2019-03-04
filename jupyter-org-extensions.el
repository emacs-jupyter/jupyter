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
  "Execute this block and jump or add a new one.

If a new block is created, use the same language, switches and parameters.
With prefix arg NEW, always insert new cell."
  (interactive "P")
  (unless (org-in-src-block-p)
      (error "Not in a source block"))
  (org-babel-execute-src-block)
  ;; add a new src block if there is no next one to jump to
  (unless (or new (condition-case nil
                       (org-babel-next-src-block)
                     (error nil)))
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
(defun jupyter-org-restart-and-execute-to-point ()
  "Kill the kernel and run src-blocks to point."
  (interactive)
  ;; FIXME: restaring the kernel does not work
  (call-interactively #'jupyter-repl-restart-kernel)
  (jupyter-org-execute-to-point))

;;;###autoload
(defun jupyter-org-restart-kernel-execute-buffer ()
  "Restart kernel and execute buffer."
  (interactive)
  ;; FIXME: restaring the kernel does not work
  (call-interactively #'jupyter-repl-restart-kernel)
  (org-babel-execute-buffer))

;;;###autoload
(defun jupyter-org-jump-to-block (&optional N)
  "Jump to a block in the buffer.
If narrowing is in effect, only a block in the narrowed region.
Use a numeric prefix N to specify how many lines of context to use.
Defaults to 3."
  (interactive "p")
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
                        (forward-line (1- (second candidate)))
                        (outline-show-entry)
                        (recenter)))))

;;;###autoload
(defun jupyter-org-jump-to-visible-block ()
  "Jump to a visible src block with avy."
  (interactive)
  (avy-with jupyter-org-jump-to-block
            (avy-jump "#\\+begin_src" :beg (point-min) :end (point-max))))

;;;###autoload
(defun jupyter-org-edit-header ()
  "Edit the src-block header in the minibuffer."
  (interactive)
  (let ((src-info (org-babel-get-src-block-info 'light)))
    (unless src-info
      (error "Not in a source block"))
    (let* ((header-start (sixth src-info))
           (header-end (save-excursion (goto-char header-start)
                                       (line-end-position))))
      (setf (buffer-substring header-start header-end)
            (read-string "Header: "
                         (buffer-substring header-start header-end))))))

(defun jupyter-org-select-block-and-results ()
  "Return the region that covers a source block and its results (if any).

Return the region as (REGION_START . REGION_END)"
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
    ;; return the region as (REGION_START . REGION_END)
    `(,(org-element-property :begin src) .
      ,(or results-end
           (- (org-element-property :end src)
              (org-element-property :post-blank src))))))

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
  (let ((current-src-block (org-element-context))
        (next-src-block (save-excursion
                          (org-babel-next-src-block)
                          (org-element-context))))
    (let ((merged-code (concat (org-element-property :value current-src-block)
                               (org-element-property :value next-src-block)))
          (lang (org-element-property :language current-src-block))
          (switches (or (org-element-property :switches current-src-block) ""))
          (parameters (or (org-element-property :parameters current-src-block) "")))
      ;; Remove source blocks
      (mapc (lambda (src-block)
              (goto-char (org-element-property :begin src-block))
              (org-babel-remove-result)
              (setf (buffer-substring (org-element-property :begin src-block)
                                      (org-element-property :end src-block))
                    ""))
            (list next-src-block current-src-block))
      ;; Now create the merged block, point is where the current block was
      (insert
       (org-element-interpret-data
        (org-element-put-property
         (jupyter-org-src-block lang parameters merged-code switches)
         :post-blank 1)))
      ;; (insert (format "#+BEGIN_SRC %s %s %s\n%s#+END_SRC\n\n"
      ;;                 lang switches parameters merged-code))
      (forward-line -3)
      (end-of-line))))

;;;###autoload
(defun jupyter-org-move-src-block (&optional below)
  "Move source block before of after another.

If BELOW is non-nil, move the block down, otherwise move it up."
  (interactive)
  (unless (org-in-src-block-p)
    (error "Not in a source block"))
  ;; throw error if there's no previous or next source block
  (when (condition-case nil
            (save-excursion
              (if below
                  (org-babel-next-src-block)
                (org-babel-previous-src-block)))
          (error nil))
    (let* ((region (jupyter-org-select-block-and-results))
           (block (delete-and-extract-region (car region) (cdr region))))
      ;; if there is an empty line remaining, take that line as part of the
      ;; ... block
      (when (looking-at-p "[[:space:]]*$")
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

;;;###autoload
(defun jupyter-org-restart-kernel-execute-block ()
  "Restart the kernel of the source block where point is and execute it."
  (interactive)
  (jupyter-org-with-src-block-client
   (jupyter-repl-restart-kernel))
  (org-babel-execute-src-block-maybe))


(defun jupyter-org-hydra/body ()
  "Hack to bind a hydra only if the hydra package exists."
  (interactive)
  (unless (require 'hydra nil t)
    (error "Package `hydra' not installed"))
  ;; unbinding this function and define the hydra
  (fmakunbound 'jupyter-org-hydra/body)
  (eval `(defhydra jupyter-org-hydra (:color blue :hint nil)
    "
        Execute                   Navigate       Edit             Misc
----------------------------------------------------------------------
    _<return>_: current           _i_: previous  _w_: move up     _/_: inspect
  _S-<return>_: current to next   _k_: next      _s_: move down   _l_: clear result
_S-M-<return>_: to point          _g_: visible   _x_: kill        _L_: clear all
  _s-<return>_: Restart/block     _G_: any       _n_: copy
_M-s-<return>_: Restart/to point  ^ ^            _c_: clone
  _H-<return>_: Restart/buffer    ^ ^            _m_: merge
           _r_: Goto repl         ^ ^            _-_: split
           ^ ^                    ^ ^            _+_: insert above
           ^ ^                    ^ ^            _=_: insert below
           ^ ^                    ^ ^            _h_: header"
    ("<return>" org-ctrl-c-ctrl-c :color red)
    ("S-<return>" jupyter-org-execute-and-next-block :color red)
    ("S-M-<return>" jupyter-org-execute-to-point)
    ("s-<return>" jupyter-org-restart-kernel-execute-block)
    ("M-s-<return>" jupyter-org-restart-and-execute-to-point)
    ("H-<return>" jupyter-org-restart-kernel-execute-buffer)
    ("r" org-babel-switch-to-session)

    ("i" org-babel-previous-src-block :color red)
    ("k" org-babel-next-src-block :color red)
    ("g" jupyter-org-jump-to-visible-block)
    ("G" jupyter-org-jump-to-block)

    ("w" jupyter-org-move-src-block :color red)
    ("s" (jupyter-org-move-src-block t) :color red)
    ("x" jupyter-org-kill-block-and-results)
    ("n" jupyter-org-copy-block-and-results)
    ("c" (jupyter-org-clone-block t))
    ("m" jupyter-org-merge-blocks)
    ("-" jupyter-org-split-src-block)
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
