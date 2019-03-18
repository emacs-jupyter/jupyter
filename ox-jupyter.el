(require 'jupyter-messages)             ; For `jupyter--empty-dict'
(require 'jupyter-kernelspec)
(eval-when-compile
  (require 'ob-core); For `org-babel-map-src-blocks'
  (require 'subr-x))

(org-export-define-derived-backend 'jupyter 'md
  :translate-alist
  '((src-block . jupyter-notebook-src-block)
    (paragraph . jupyter-notebook-paragraph)
    (section . jupyter-notebook-section)
    (fixed-width . jupyter-notebook-cell)
    (drawer . jupyter-notebook-cell)
    (inner-template . jupyter-notebook-json))
  :menu-entry
  '(?j "Export to Jupyter Notebook"
       ((?J "As Notebook" jupyter-org-export-as-notebook))))

(defsubst jupyter-notebook--markdown-cell-p (info)
  (equal "markdown" (plist-get (jupyter-notebook--current-cell info) :cell_type)))

(defsubst jupyter-notebook--code-cell-p (info)
  (equal "code" (plist-get (jupyter-notebook--current-cell info) :cell_type)))

(defsubst jupyter-notebook--result-drawer-p (elem)
  (and (eq (org-element-type elem) 'drawer)
       (equal "RESULTS"
              (upcase (org-element-property :drawer-name elem)))))

;; From the `org-mode' documentation
;;
;; A section contains directly any greater element or element. Only a headline
;; can contain a section. As an exception, text before the first headline in
;; the document also belongs to a section.
;; (document
;;  (section)
;;  (headline
;;   (section)
;;   (headline)
;;   (headline
;;    (headline))))
(defun jupyter-notebook-section (data contents _info)
  contents)

(defun jupyter-notebook-paragraph (elem contents info)
  (if (jupyter-notebook--markdown-cell-p info)
      (let* ((cell (jupyter-notebook--current-cell info))
             (source (plist-get cell :source)))
        (plist-put cell :source
                   (nconc source
                          ;; TODO: Be able to control when to create a new
                          ;; markdown cell? At what level of subtree should
                          ;; markdown cells be created? For top-level subtrees
                          ;; or have a subtree property that states when to
                          ;; create a subtree?
                          (list "\n\n")
                          (jupyter-notebook-split-source contents))))
    (jupyter-notebook--begin-cell
     (jupyter-notebook-markdown-cell
      :source (jupyter-notebook-split-source contents))
     info))
  "")

(defun jupyter-notebook--complete-cell (elem info)
  (when elem
    (let ((cells (plist-get info :jupyter-cells)))
      (push elem cells)
      (plist-put info :jupyter-cells cells)
      (plist-put info :jupyter-current-cell nil))))

(defun jupyter-notebook--begin-cell (elem info)
  (when (plist-get info :jupyter-current-cell)
    (jupyter-notebook--complete-cell
     (plist-get info :jupyter-current-cell) info))
  (plist-put info :jupyter-current-cell elem))

(defun jupyter-notebook--current-cell (info)
  (plist-get info :jupyter-current-cell))

(defun jupyter-notebook-src-block (src-block contents info)
  (jupyter-notebook--begin-cell
   (jupyter-notebook-code-cell
    ;; TODO: Global execution count
    :execution-count 0
    :source (string-trim-right (org-element-property :value src-block)))
   info)
  ""
  ;; TODO: (1) If INFO contains intermediate output, complete the previous cell
  ;; object
  ;;
  ;; TODO: (2) Start a source block object and store it in INFO as the
  ;; :jupyter-intermediate object. On the next element transcoded, if it
  ;; doesn't contain a RESULTS post-affiliated keyword, complete the source
  ;; block as not containing any outputs.
  ;;
  ;; TODO: (3) So source blocks act as boundaries on when to complete objects.
  )

(defun jupyter-notebook--result-p (elem info)
  "Return non-nil if ELEM corresponds to the results of a source block.
ELEM is the results of a source block if the current cell being
processed in INFO is a code cell, ELEM has a RESULTS affiliated
keyword, and ELEM passes `jupyter-org-babel-result-p'."
  (and (jupyter-notebook--code-cell-p info)
       (org-element-property :results elem)
       (or (jupyter-org-babel-result-p elem)
           ;; TODO: Isn't a result drawer also a babel result?
           (jupyter-notebook--result-drawer-p elem))))

;; TODO: Remember that the export process is depth first, so the higher level
;; nodes like drawers and sections will already have parsed contents of the
;; elements they contain. What I need to do is tag with text properties parts
;; of the buffer that correspond to notebook elements, then during the export
;; process, check those properties to parse them differently.
;;
;; 1. Add a hook function to org-export-before-parsing-hook that marks org
;;    elements that correspond to source block output as such.
;;
;; 2. During export, if an element has been marked as a source block output,
;;    add it to the outputs of the current source block cell.
(defun jupyter-notebook-cell (elem contents info)
  (if (jupyter-notebook--result-p elem info)
      (let ((cell (jupyter-notebook--current-cell info)))
        (save-excursion
          (goto-char (org-element-property :begin elem))
          (plist-put cell :outputs (jupyter-notebook-parse-outputs))
          (jupyter-notebook--complete-cell cell info)
          ""))
    (org-export-data-with-backend elem 'md info)
    ))

(defvar jupyter-notebook--kernelspec nil)

(defun jupyter-notebook-verify-kernelspec (backend)
  "When BACKEND is jupyter, ensure all source blocks use the same kernelspec.
Raise an error if a source block uses a different kernel,
otherwise return the kernelspec used for the notebook."
  (when (eq backend 'jupyter)
    (let (spec)
      (org-babel-map-src-blocks nil
        (when (org-babel-jupyter-language-p lang)
          (let* ((info (org-babel-get-src-block-info))
                 (block-spec
                  (car (jupyter-find-kernelspecs
                        (alist-get :kernel (nth 2 info))))))
            (or spec (setq spec block-spec))
            ;; TODO: Go to error location
            (unless (equal spec block-spec)
              (user-error "Using different kernels in same notebook")))))
      (setq jupyter-notebook--kernelspec
            (cl-destructuring-bind (name _ . spec) spec
              (list :name name
                    :display_name (plist-get spec :display_name)
                    :language (plist-get spec :language)))))))

(add-hook 'org-export-before-processing-hook #'jupyter-notebook-verify-kernelspec)

(defun jupyter-notebook-json (_ info)
  "From the Jupyter related properties in INFO, return the notebook JSON."
  ;; Finish up the last cell
  (jupyter-notebook--complete-cell
   (jupyter-notebook--current-cell info) info)
  (json-encode
   (list :cells (apply #'vector (nreverse (plist-get info :jupyter-cells)))
         :metadata (list :kernelspec (prog1 (or jupyter-notebook--kernelspec
                                                jupyter--empty-dict)
                                       (setq jupyter-notebook--kernelspec nil))
                         :language_info jupyter--empty-dict
                         :widgets jupyter--empty-dict)
         :nbformat 4
         :nbformat_minor 1)))

