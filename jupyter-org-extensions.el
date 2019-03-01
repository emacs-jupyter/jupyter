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
              (setq location (org-babel-where-is-src-block-result nil nil))
              (if (not  location)
                  (goto-char end)
                (goto-char location)
                (goto-char (org-element-property :end (org-element-context))))
              (insert (format "#+BEGIN_SRC %s\n\n#+END_SRC\n\n"
                              (concat lang (and lang " ")
                                      switches (and switches " ")
                                      parameters (and parameters " "))))
              (forward-line -3))
          ;; after current block
          (goto-char (org-element-property :begin (org-element-context)))
          (insert (format "#+BEGIN_SRC %s\n\n#+END_SRC\n\n"
                          (concat lang (and lang " ")
                                  switches (and switches " ")
                                  parameters (and parameters " "))))
          (forward-line -3)))

    ;; Not in a src block, just insert a block
    (beginning-of-line)
    (insert (format "#+BEGIN_SRC %s\n\n#+END_SRC\n" (completing-read "Language: " (mapcar 'car org-babel-load-languages))))
    (forward-line -1)))

;;;###autoload
(defun jupyter-org-split-src-block (&optional below)
  "Split the current src block with point in upper block.

With a prefix BELOW move point to lower block."
  (interactive "P")
  (let* ((src-block (org-element-context)))
    (unless (eq (org-element-type src-block) 'src-block)
      (error "Not in a source block"))
    (let ((lang (org-element-property :language src-block))
          (switches (org-element-property :switches src-block))
          (parameters (org-element-property :parameters src-block)))
      (beginning-of-line)
      (insert (format "#+END_SRC\n\n#+BEGIN_SRC %s\n"
                      (concat lang (and lang " ")
                              switches (and switches " ")
                              parameters (and parameters " "))))
      (unless below
        (beginning-of-line)
        (forward-line -3)
        (forward-char -1)))))

;;;###autoload
(defun jupyter-org-execute-and-next-block (&optional new)
  "Execute this block and jump or add a new one.

If a new block is created, use the same language, switches and parameters.
With prefix arg NEW, always insert new cell."
  (interactive "P")
  (org-babel-execute-src-block)
  ;; we ignore-errors here because when there is no next block it is a
  ;; ... user-error, not nil.
  (let* ((lang (car (org-babel-get-src-block-info t)))
         (next-block (ignore-errors
                       (save-excursion
                         (catch 'block
                           (while (setq next-block (org-babel-next-src-block))
                             (when (string= lang (org-element-property :language (org-element-context)))
                               (throw 'block next-block))))))))
    (if (or new (not next-block))
        (jupyter-org-insert-src-block t)
      (goto-char (match-beginning 0)))))

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
            (avy-jump "#\\+BEGIN_SRC" :beg (point-min) :end (point-max))))

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

;;;###autoload
(defun jupyter-org-kill-block-and-results ()
  "Kill the block and its results."
  (interactive)
  (let ((src (org-element-context))
        (result-start (org-babel-where-is-src-block-result))
        end)
    (if result-start
        (save-excursion
          (goto-char result-start)
          (setq end (org-element-property :end src)))
      (setq end (org-element-property :end src)))
    (kill-region
     (org-element-property :begin src)
     end)))

;;;###autoload
(defun jupyter-org-copy-block-and-results ()
  "Copy the src block at the current point and its results."
  (interactive)
  (let ((src (org-element-context))
        (result-start (org-babel-where-is-src-block-result))
        end)
    (if result-start
        (save-excursion
          (goto-char result-start)
          (setq end (org-babel-result-end)))
      (setq end (org-element-property :end src)))
    (kill-new
     (buffer-substring
      (org-element-property :begin src)
      end))))

;;;###autoload
(defun jupyter-org-clone-block (&optional below)
  "Clone the block above the current block.

If BELOW is non-nil, add the cloned block below."
  (interactive "P")
  (let* ((src (org-element-context))
         (code (org-element-property :value src)))
    (unless (eq (org-element-type src) 'src-block)
      (error "Not in a source block"))
    (jupyter-org-insert-src-block (not below))
    (delete-char 1)
    (insert code)
    ;; jump back to start of new block
    (org-babel-previous-src-block)
    (org-babel-next-src-block)))

;;;###autoload
(defun jupyter-org-merge-blocks ()
  "Merge the current block with the next block."
  (interactive)
  (let ((current-src-block (org-element-context))
        (next-src-block (save-excursion
                          (org-babel-next-src-block)
                          (org-element-context))))
    (unless (eq (org-element-type current-src-block) 'src-block)
      (error "Not in a source block"))
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
      (insert (format "#+BEGIN_SRC %s %s %s\n%s#+END_SRC\n\n"
                      lang switches parameters merged-code))
      (forward-line -3)
      (end-of-line))))

;;;###autoload
(defun jupyter-org-move-src-block (&optional below)
  "Move source block before of after another.

If BELOW is non-nil, move the block down, otherwise move it up."
  (interactive)
  ;;skip all this if there are no previous or next source block
  (when (condition-case nil
            (save-excursion
              (if below
                  (org-babel-next-src-block)
                (org-babel-previous-src-block)))
          (error nil))
    (let* ((src (org-element-context))
           (results-start (org-babel-where-is-src-block-result))
           (results-end
            (when results-start
              (save-excursion
                (goto-char results-start)
                (goto-char (org-babel-result-end))
                ;; if line is empty, take that empty line by moving down
                (when (looking-at-p "[[:space:]]*$")
                  (forward-line 1))
                (point)))))
      ;; kill from the start of the source block to the end of the results
      ;; ... if there are no results, to the end of the source block
      (kill-region
       (org-element-property :begin src)
       (or results-end (org-element-property :end src))))
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
    (save-excursion (org-yank)))) ; keep cursor where the yank takes place

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
  "Restart kernel and execute block."
  (interactive)
  (let* ((params (car jupyter-org--src-block-cache))
         (jupyter-current-client
          (buffer-local-value 'jupyter-current-client
                              (org-babel-jupyter-initiate-session
                               (alist-get :session params) params))))
    ;; FIXME: retarting the kernel does not work
    (jupyter-repl-restart-kernel))
  (org-babel-execute-src-block-maybe))

(with-eval-after-load 'hydra
  (defhydra jupyter-org-hydra (:color blue :hint nil)
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

    ("w" jupyter-org-ob-move-src-block :color red)
    ("s" (jupyter-org-move-src-block t) :color red)
    ("x" jupyter-org-kill-block-and-results)
    ("n" jupyter-org-copy-block-and-results)
    ("c" jupyter-org-clone-block)
    ("m" jupyter-org-merge-blocks)
    ("-" jupyter-org-split-src-block)
    ("+" jupyter-org-insert-src-block)
    ("=" (jupyter-org-insert-src-block t))
    ("l" org-babel-remove-result)
    ("L" jupyter-org-clear-all-results)
    ("h" jupyter-org-edit-header)

    ("/" jupyter-inspect-at-point)))

;; FIXME: find a better map? We want the hydra to be available outside of jupyter blocks
(with-eval-after-load 'hydra
  (define-key jupyter-org-interaction-mode-map (kbd "C-c h") #'jupyter-org-hydra/body))

(provide 'jupyter-org-extensions)
;;; jupyter-org-extensions.el ends here
