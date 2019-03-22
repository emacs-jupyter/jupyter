;; TODO: Encapsulate the notebook state in some struct so it is easier to
;; reason about which parts are stateful.

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
    (headline . jupyter-notebook-headline)
    (fixed-width . jupyter-notebook-cell)
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
;;
;; Once a section is complete, the headline nodes gets called so I need to have
;; some state that allows me to put the headline at the front.


;; Take the current cells and put them in a section, the node that gets called
;; right after this one is the headline node which is then responsible for
;; modifying the head of the section to add the headline
(defun jupyter-notebook-section (_data contents info)
  (message "section %s" contents)
  (let ((cells (plist-get info :jupyter-cells)))
    (plist-put info :jupyter-sections
               (cons cells (plist-get info :jupyter-sections)))
    (plist-put info :jupyter-cells nil))
  "")

;; TODO: Figure out when this gets called
(defun jupyter-notebook-headline (data contents info)
  (message "headline %s" contents)
  (let* ((cell (jupyter-notebook--current-cell info))
         (source (plist-get cell :source)))
    (plist-put cell :source
               (nconc source
                      ;; TODO: Be able to control when to create a new markdown
                      ;; cell? At what level of subtree should markdown cells
                      ;; be created? For top-level subtrees or have a subtree
                      ;; property that states when to create a subtree?
                      (list "\n\n")
                      (list (concat (make-string (org-element-property :level data) ?#) " "
                                    (org-element-property :raw-value data))))))
  "")

(defun jupyter-notebook-paragraph (elem contents info)
  (message "paragraph %s" contents)
  (let (context)
    (cond
     ((jupyter-notebook--markdown-cell-p info)
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
                          (jupyter-notebook-split-source contents)))))
     ((jupyter-notebook--result-p
       (save-excursion
         (goto-char (jupyter-org-element-begin-after-affiliated elem))
         ;; To handle links we need to get the context
         (setq context (org-element-context)))
       info)
      ;; TODO: Rename this function to better represent what it does. It is a
      ;; node in the parse tree that gets a source block's results.
      (jupyter-notebook-cell context nil info))
     (t
      (jupyter-notebook--begin-cell
       (jupyter-notebook-markdown-cell
        :source (jupyter-notebook-split-source contents))
       info))))
  "")

(defun jupyter-notebook--complete-cell (info)
  (when-let* ((elem (plist-get info :jupyter-current-cell)))
    ;; TODO: Handle the execute-result output. The last output should be
    ;; translated into an execute result if need be. This will be somewhat
    ;; difficult to do since there is no way to distinguish between an execute
    ;; result or a stream or display-data result. So don't really know what the
    ;; strategy should be.
    (let ((cells (plist-get info :jupyter-cells)))
      (push elem cells)
      (plist-put info :jupyter-cells cells)
      (plist-put info :jupyter-current-cell nil))))

(defun jupyter-notebook--begin-cell (elem info)
  (when (plist-get info :jupyter-current-cell)
    (jupyter-notebook--complete-cell info))
  (plist-put info :jupyter-current-cell elem))

(defun jupyter-notebook--current-cell (info)
  (plist-get info :jupyter-current-cell))

(defun jupyter-notebook-src-block (src-block contents info)
  (message "src-block %s" contents)
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
       ;; If ELEM is associated with a #+RESULTS: keyword, it is considered a
       ;; result
       (cl-loop
        with el = elem
        while el thereis (org-element-property :results el)
        do (setq el (org-element-property :parent el)))
       (or (jupyter-org-babel-result-p elem)
           ;; TODO: Isn't a result drawer also a babel result?
           (jupyter-notebook--result-drawer-p elem))))

(defun jupyter-notebook--mime-bundle (elem)
  ;; TODO: Is there a better way?
  ;; NOTE: All elements are initially considered as display-data output, when
  ;; the corresponding source block cell is completed, the last element in the
  ;; :outputs key is converted into an execute-result if it should be.
  (cl-case (org-element-type elem)
    (table
     (save-restriction
       (narrow-to-region (org-element-property :begin elem)
                         (org-element-property :end elem))
       (let ((org-html-format-table-no-css t))
         (jupyter-notebook-display-data-output
          :data (list :text/html (org-export-as 'html nil nil 'body-only))))))
    ;; Ambiguity between stream results and final result output
    ((fixed-width example-block)
     (jupyter-notebook-stream-output
      ;; TODO: How to distinguish between stdout and stderr reliably? Maybe
      ;; hijack the switches of an example block or add as affiliated keywords
      ;; like ATTR_JUPYTER.
      ;;
      ;; The example block way would be
      ;;
      ;; #+BEGIN_EXAMPLE :stream err
      ;; #+END_EXAMPLE
      :name "stdout"
      :text (org-element-property :value elem)))
    ;; Image links
    (link
     (let* ((path (org-element-property :path elem))
            (ext (file-name-extension path))
            ;; TODO: Generalize this into a function and also use in
            ;; `jupyter-org-client.el'
            (mime (pcase ext
                    ("png" :image/png)
                    ("jpg" :image/jpeg)
                    ("svg" :image/svg+xml)
                    (_ (error "Unsupported file name extension (%s)" ext)))))
       (list mime (with-temp-buffer
                    (insert-file-contents-literally path)
                    (when (memq mime '(:image/png :image/jpeg))
                      (base64-encode-region (point-min) (point-max) 'no-line-breaks))
                    (buffer-string))))
     )
    (_
     ;; A result that passes `jupyter-org-babel-result-p'
     ;; TODO: One issue here is that results that look like tables get
     ;; converted into `org-mode' tables and information is lost there.
     ;; Probably the best option here is to convert into an `html' table.

     )))

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
  (message "cell %s" contents)
  (if (jupyter-notebook--result-p elem info)
      (let ((cell (jupyter-notebook--current-cell info)))
        (plist-put
         cell :outputs (vconcat
                        (plist-get cell :outputs)
                        (list (jupyter-notebook--mime-bundle elem))))
        "")
    (let (org-export-with-toc)
      (org-export-data-with-backend elem 'md info))))

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
  (jupyter-notebook--complete-cell info)
  (json-encode
   (list :cells (apply #'vector (nreverse (plist-get info :jupyter-cells)))
         :metadata (list :kernelspec (prog1 (or jupyter-notebook--kernelspec
                                                jupyter--empty-dict)
                                       (setq jupyter-notebook--kernelspec nil))
                         :language_info jupyter--empty-dict
                         :widgets jupyter--empty-dict)
         :nbformat 4
         :nbformat_minor 1)))

