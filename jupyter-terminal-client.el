(require 'jupyter-client)
(require 'xterm-color)

;; TODO: Read up on how method tags can be used, see
;; https://ericabrahamsen.net/tech/2016/feb/bbdb-eieio-object-oriented-elisp.html

;; TODO: Still need to figure out how to remove requests from the client's
;; request table.

;; TODO: Fallbacks for when the language doesn't have a major mode installed.

;; TODO: Add a text property to all output depending on which handler it came
;; from. Then it would make it easier to traverse the buffer and collect
;; information to save the REPL to a notebook format. But what about the text
;; properties from `xterm-color-filter'? Maybe I can use overlays, this way the
;; text is preserved but color is still there.

(defface jupyter-repl-input-prompt
  '((((class color) (min-colors 88) (background light))
     :foreground "darkseagreen2")
    (((class color) (min-colors 88) (background dark))
     :foreground "darkolivegreen"))
  "Face used for the input prompt."
  :group 'jupyter-repl)

(defface jupyter-repl-output-prompt
  '((((class color) (min-colors 88) (background light))
     :foreground "indianred3")
    (((class color) (min-colors 88) (background dark))
     :foreground "darkred"))
  "Face used for the input prompt."
  :group 'jupyter-repl)

(defcustom jupyter-repl-maximum-size 1024
  "Maximum number of lines before the buffer is truncated."
  :group 'jupyter-repl)

(defclass jupyter-repl-client (jupyter-kernel-client)
  ((kernel-info)
   (buffer :initarg :buffer)
   (execution-count :initform 1)
   (input-start-marker :initform (make-marker))))

(defvar-local jupyter-repl-lang-buffer nil
  "A buffer with the current major mode set to the language of
  the REPL.")

(defvar-local jupyter-repl-current-client nil
  "The `jupyter-repl-client' for the `current-buffer'.")

(defvar-local jupyter-repl-kernel-manager nil
  "If the REPL is connected to a kernel which was started as a
 subprocess. This will contain the kernel manager used to control
 the lifetime of the kernel.")

(defvar-local jupyter-repl-lang-mode nil
  "The major mode corresponding to the kernel's language.")

(defmacro with-jupyter-repl-buffer (client &rest body)
  (declare (indent 1) (debug (symbolp &rest form)))
  `(with-current-buffer (oref ,client buffer)
     (let ((inhibit-read-only t))
       (prog1 (progn ,@body)
         (let ((win (get-buffer-window)))
           (when win (set-window-point win (point))))))))

(defmacro with-jupyter-repl-lang-buffer (&rest body)
  (declare (indent 0) (debug (&rest form)))
  `(with-current-buffer jupyter-repl-lang-buffer
     (let ((inhibit-read-only t))
       (erase-buffer)
       ,@body)))

(defmacro with-jupyter-repl-cell (&rest body)
  "Narrow the buffer to the current code cell and widen afterwards."
  (declare (indent 0) (debug (&rest form)))
  `(cl-destructuring-bind (beg . end)
       (jupyter-repl-cell-bounds)
     (save-restriction
       (narrow-to-region beg end)
       ,@body)))

(defun jupyter-repl-insert (&rest args)
  "Insert text into the `current-buffer', possibly with text properties.

This acts like `insert' except that the leading elements of ARGS
can contain the following keywords along with their values:

- `:read-only' :: A non-nil value makes the text to be inserted,
  read only. This is t by default, so to make text editable you
  will have to do something like:
    (jupyter-repl-insert :read-only nil \"<editable text>\")

- `:properties' :: A list of text properties and their values to
  be added to the inserted text. This defaults to an empty list.

- `:inherit-properties' :: A non-nil value will use
  `insert-and-inherit' instead of `insert' for the function used
  to insert the text. This is nil by default."
  (let ((arg nil)
        (read-only t)
        (properties nil)
        (insert-fun #'insert))
    (while (keywordp (setq arg (car args)))
      (cl-case arg
        (:read-only (setq read-only (cadr args)))
        (:properties (setq properties (cadr args)))
        (:inherit-properties
         (setq insert-fun (if (cadr args) #'insert-and-inherit #'insert)))
        (otherwise
         (error "Keyword not one of `:read-only', `:properties', `:inherit-properties' (`%s')." arg)))
      (setq args (cddr args)))
    (when read-only
      (if properties
          (setq properties (append '(read-only t) properties))
        (setq properties '(read-only t))))
    (apply insert-fun (mapcar (lambda (s)
                           (prog1 s
                             (when properties
                               (add-text-properties
                                0 (length s) properties s))))
                         args))))

;;; Prompt

(defun jupyter-repl-insert-output-prompt (count)
  (let ((props (list 'fontified t
                     'font-lock-face 'jupyter-repl-output-prompt
                     'field (list 'jupyter-cell 'out count))))
    ;; Handled by `jupyter-repl-do-request-output'
    ;; (jupyter-repl-insert "\n")
    (jupyter-repl-insert :properties props (format "Out[%d]:" count))
    (jupyter-repl-insert :read-only nil :properties props " ")))

(defun jupyter-repl-insert-input-prompt (count)
  (let ((props (list 'fontified t
                     'font-lock-face 'jupyter-repl-input-prompt
                     'field (list 'jupyter-cell 'in count))))
    (jupyter-repl-insert "\n")
    (jupyter-repl-insert
     :properties (append '(front-sticky t) props)
     (let ((str (format "In [%d]:" count)))
       (add-text-properties 0 1 '(jupyter-cell beginning) str)
       str))
    (jupyter-repl-insert
     :properties props (propertize " " 'rear-nonsticky t))))

(defun jupyter-repl-insert-continuation-prompt (field)
  (let ((props (list 'fontified t
                     'font-lock-face 'jupyter-repl-input-prompt
                     'field field
                     'cursor-intangible t)))
    ;; (jupyter-repl-insert :read-only nil "\n")
    (jupyter-repl-insert
     :properties (append '(front-sticky t) props)
     (let ((len (- (jupyter-repl-cell-prompt-width) 2)))
       (concat (make-string len ? ) ":")))
    (jupyter-repl-insert
     ;; Deleting this space, deletes the whole continuation prompt. See
     ;; `jupyter-repl-before-buffer-change'
     :read-only nil
     :properties props (propertize  " " 'rear-nonsticky t))))

(defun jupyter-repl-insert-prompt (&optional type)
  "Insert a REPL promp in CLIENT's buffer according to type.
If TYPE is nil or `in' insert a new input prompt. If TYPE is
`out' insert a new output prompt."
  (setq type (or type 'in))
  (unless (memq type '(in out busy continuation))
    (error "Prompt type can only be (`in', `out', `busy', or `continuation')"))
  (let ((count (oref jupyter-repl-current-client execution-count))
        (inhibit-modification-hooks (memq type '(in out))))
    (cond
     ((eq type 'in)
      (jupyter-repl-insert-input-prompt count))
     ((eq type 'out)
      (jupyter-repl-insert-output-prompt count))
     ((eq type 'continuation)
      (jupyter-repl-insert-continuation-prompt
       (field-at-pos (1+ (jupyter-repl-cell-beginning-position))))))))

(defun jupyter-repl-finalize-cell ()
  "Make the current cell un-editable."
  (add-text-properties
   (oref jupyter-repl-current-client input-start-marker)
   (point-max)
   ;; font-lock-multiline to avoid improper syntactic elements from
   ;; spilling over to the rest of the buffer.
   '(read-only t font-lock-multiline t)))

;; Stolen from `gnus-remove-text-with-property'
(defun jupyter-remove-text-with-property (prop)
  (let ((start (point-min))
        end)
    (unless (get-text-property start prop)
      (setq start (next-single-property-change start prop)))
    (while start
      (setq end (text-property-any start (point-max) prop nil))
      (delete-region start (or end (point-max)))
      (setq start (when end
                    (next-single-property-change start prop))))))

(defun jupyter-repl-cell-code ()
  "Get the code of the cell without prompts."
  (if (= (point-min) (point-max)) ""
    (let ((code (buffer-substring
                 (oref jupyter-repl-current-client
                       input-start-marker)
                 (point-max))))
      (with-temp-buffer
        (insert code)
        (jupyter-remove-text-with-property 'jupyter-prompt)
        (buffer-string)))))

(defun jupyter-repl-prompt-width ()
  "Return the width of the prompt at the start of the line where `point' is at."
  (- (or (next-single-property-change (point-at-bol) 'jupyter-prompt)
         ;; Handle edge case when
         (point-at-eol))
     (point-at-bol)))

(defun jupyter-repl-cell-bounds ()
  "Get the bounds of the current input cell."
  (let ((pos (point)) beg end)
    (goto-char (point-at-bol))
    (unless (get-text-property (point) 'jupyter-prompt)
      (error "Not in a code cell."))
    (while (get-text-property (point) 'jupyter-prompt)
      (forward-line -1))
    (setq beg (1+ (point-at-eol)))
    (goto-char pos)
    (while (and (not (= (point-at-eol) (point-max)))
                (get-text-property (1+ (point-at-eol)) 'jupyter-prompt))
      (forward-line 1))
    (setq end (point-at-eol))
    (goto-char pos)
    (cons beg end)))

(defun jupyter-repl-cell-position ()
  "Get the position that `point' is at relative to the contents of the cell.
FIXME: If `point' is within a prompt return nil."
  (unless (get-text-property (point) 'jupyter-prompt)
    (with-jupyter-repl-cell
      (- (point) (point-min) (* (line-number-at-pos (point))
                                (jupyter-repl-prompt-width))))))

(defun jupyter-repl-replace-cell-code (new-code)
  (let ((mark (oref jupyter-repl-current-client input-start-marker)))
    (delete-region mark (point-max))
    (jupyter-repl-insert :read-only nil new-code)))

(defvar-local jupyter-repl-request-time nil)

;; TODO: Clean this up what about edge cases?
(defun jupyter-repl-next-prompt ()
  (beginning-of-line)
  ;; Go to the line that does not start with a prompt
  (while (and (get-text-property (point) 'jupyter-prompt)
              (= (forward-line) 0)))
  ;; Find the start of the next prompt
  (while (and (not (get-text-property (point) 'jupyter-prompt))
              (= (forward-line) 0)))
  ;; Move to the start of the input when a prompt was found
  (when (get-text-property (point) 'jupyter-prompt)
    (let ((pos (next-single-property-change (point) 'jupyter-prompt)))
      ;; Return non-nil when we made it
      (and pos (goto-char pos)))))

(defun jupyter-repl-truncate-buffer ()
  (save-excursion
    (when (= (forward-line (- jupyter-repl-maximum-size)) 0)
      (jupyter-repl-next-prompt)
      (beginning-of-line)
      (delete-region (point-min) (point)))))

(cl-defmethod jupyter-request-execute ((client jupyter-repl-client)
                                       &key code
                                       (silent nil)
                                       (store-history t)
                                       (user-expressions '(:home "eval(JULIA_HOME)"))
                                       (allow-stdin t)
                                       (stop-on-error nil))
  (setq jupyter-repl-request-time (current-time))
  (if code (cl-call-next-method)
    (with-jupyter-repl-buffer client
      (let ((code (jupyter-repl-cell-code)))
        ;; Don't store empty code
        (when (= (length code) 0)
          (setq silent t))
        (jupyter-repl-finalize-cell)
        (jupyter-repl-insert-prompt 'busy)
        (jupyter-repl-truncate-buffer)
        (cl-call-next-method
         client :code code :silent silent :store-history store-history
         :user-expressions user-expressions :allow-stdin allow-stdin
         :stop-on-error stop-on-error)))))

(cl-defmethod jupyter-handle-execute ((client jupyter-repl-client)
                                      req
                                      execution-count
                                      user-expressions
                                      payload)
  (oset client execution-count (1+ execution-count))
  (with-jupyter-repl-buffer client
    (jupyter-repl-insert-prompt 'in)))

(cl-defmethod jupyter-handle-execute-input ((client jupyter-repl-client)
                                            req
                                            code
                                            execution-count)
  (oset client execution-count (1+ execution-count)))

;; TODO: Do like how org-mode does it and cache buffers which have the
;; major mode enabled already
(defun jupyter-repl-fontify (mode str)
  "Fontify STR according to MODE and return the fontified string."
  ;; Adapted from `org-src-font-lock-fontify-block'
  (with-temp-buffer
    (let ((inhibit-modification-hooks nil))
      (funcall mode)
      (insert str)
      (font-lock-ensure)
      (let ((pos (point-min)) next)
        (while (setq next (next-property-change pos))
          ;; Handle additional properties from font-lock, so as to
          ;; preserve, e.g., composition.
          (dolist (prop (cons 'face font-lock-extra-managed-props))
            (let ((new-prop (get-text-property pos prop)))
              (put-text-property
               (+ 1 (1- pos)) (1- (+ 1 next))
               (if (eq prop 'face) 'font-lock-face
                 prop)
               (if (eq prop 'face) (or new-prop 'default)
                 new-prop))))
          (setq pos next))))
    (add-text-properties
     (point-min) (point-max) '(font-lock-multiline t fontified t))
    (buffer-string)))

(cl-defmethod jupyter-handle-execute-result ((client jupyter-repl-client)
                                             req
                                             execution-count
                                             data
                                             metadata)
  (oset client execution-count execution-count)
  (with-jupyter-repl-buffer client
    (let ((mimetypes (seq-filter #'keywordp data)))
      (catch 'done
        (when (memq :text/html mimetypes)
          (let ((html (plist-get data :text/html)))
            (when (string-match
                   "<img src=\"data:image/png;base64,\\(.+\\)\"" html)
              (jupyter-repl-insert-prompt 'out)
              (let ((img (create-image
                          (base64-decode-string (match-string 1 html))
                          nil 'data)))
                (insert-image img (propertize " " 'read-only t)))
              (jupyter-repl-insert "\n")
              (throw 'done t))))
        (when (memq :text/latex mimetypes)
          (require 'org)
          (jupyter-repl-insert-prompt 'out)
          (let (beg end)
            (setq beg (point))
            (jupyter-repl-insert (plist-get data :text/latex))
            (setq end (point))
            (org-format-latex
             "jupyter-repl" beg end "jupyter-repl"
             'overlays "Creating LaTeX image...%s"
             'forbuffer
             ;; Use the default method for creating image files
             org-preview-latex-default-process))
          (throw 'done t))
        (when (memq :text/markdown mimetypes)
          (jupyter-repl-insert-prompt 'out)
          (let ((inhibit-read-only t))
            (jupyter-repl-insert
             "\n" (jupyter-repl-fontify
                   #'markdown-mode (plist-get data :text/markdown))))
          (throw 'done t))
        (when (memq :text/plain mimetypes)
          (jupyter-repl-insert-prompt 'out)
          (let ((multiline (string-match "\n" (plist-get data :text/plain))))
            (jupyter-repl-insert
             (concat
              (when multiline "\n")
              (xterm-color-filter (plist-get data :text/plain)) "\n"))))))))

(cl-defmethod jupyter-handle-stream ((client jupyter-repl-client) req name text)
  (with-jupyter-repl-buffer client
    (goto-char (point-max))
    (jupyter-repl-insert (xterm-color-filter text))))

(cl-defmethod jupyter-handle-error ((client jupyter-repl-client)
                                    req ename evalue traceback)
  (with-jupyter-repl-buffer client
    (dolist (s traceback)
      (add-text-properties 0 (length s) '(font-lock-face default fontified t font-lock-fontified t) s)
      (jupyter-repl-insert "\n" (xterm-color-filter s)))
    (jupyter-repl-insert "\n")
    (jupyter-repl-insert-prompt 'in)))

(cl-defmethod jupyter-handle-input ((client jupyter-repl-client) req prompt password)
  (with-jupyter-repl-buffer client
    (let ((value (cl-call-next-method)))
      (jupyter-repl-insert (concat "\n" prompt value)))))

(defvar-local jupyter-repl-history nil
  "The history of the current Jupyter REPL.")

;; (defun jupyter-repl-history-next (n)
;;   (interactive "N")
;;   (setq n (or n 1))
;;   (let ((pos (ring-member jupyter-repl-history
;;                           (jupyter-repl-current-cell-code
;;                            jupyter-repl-current-client
;;                            'with-prompts))))
;;     (when pos
;;       (jupyter-repl-replace-current-cell
;;        (ring-ref jupyter-repl-history (+ pos n))))))

;; (defun jupyter-repl-history-previous (n)
;;   (interactive "N")
;;   (setq n (or n 1))
;;   ;; TODO: Fix this, have a current history index or something
;;   (let ((pos (ring-member jupyter-repl-history
;;                           (jupyter-repl-current-cell-code
;;                            jupyter-repl-current-client
;;                            'with-prompts))))
;;     (when pos
;;       (jupyter-repl-replace-current-cell
;;        (ring-ref jupyter-repl-history (- pos n))))))

(cl-defmethod jupyter-handle-history ((client jupyter-repl-client) req history)
  (with-jupyter-repl-buffer client
    (delete-region (oref client input-start-marker) (point-max))
    (let ((lines (split-string (nth 2 (car (last history))) "\n")))
      (jupyter-repl-insert (car lines))
      (dolist (s (cdr lines))
        (jupyter-repl-insert-prompt 'continuation)
        (jupyter-repl-insert s)))))

(cl-defmethod jupyter-handle-is-complete ((client jupyter-repl-client) req status indent)
  (with-jupyter-repl-buffer client
    (pcase status
      ("complete"
       (jupyter-request-execute client)
       (goto-char (point-max))
       (jupyter-repl-insert "\n"))
      ("incomplete"
       (jupyter-repl-insert-prompt 'continuation)
       ;; Indentation is editable
       (insert indent))
      ("invalid"
       ;; Force an execute to produce a traceback
       (jupyter-request-execute client))
      ("unknown"))))

(defun jupyter-repl-ret (arg)
  (interactive "P")
  (let* ((client jupyter-repl-current-client)
         (mark (oref client input-start-marker)))
    (if (not (marker-position mark))
        (jupyter-wait-until-idle
         client (jupyter-request-execute client :code ""))
      (if (< (point) mark)
          (goto-char (point-max))
        ;; TODO: Not all kernels will respond to an is_complete_request. The
        ;; jupyter console will switch to its own internal handler when the
        ;; request times out.
        (let* ((code (with-jupyter-repl-buffer client
                       (jupyter-repl-cell-code)))
               (res (jupyter-wait-until-received
                        client 'is-complete-reply
                      (jupyter-request-is-complete client :code code))))
          ;; If the kernel responds to an is-complete request then the
          ;; is-complete handler takes care of executing the code.
          (unless res
            ;; TODO: Indent or send (on prefix arg)?
            )
          )))))

(defun jupyter-repl-tab (arg)
  (interactive "P")
  (with-jupyter-repl-buffer jupyter-repl-current-client
    (let ((prompt-width nil))
      (narrow-to-region
       (save-excursion
         (goto-char (oref jupyter-repl-current-client input-start-marker))
         (setq prompt-width (- (point) (point-at-bol)))
         (point-at-bol))
       (point-max))
      (let* ((spoint (point))
             (offset (- (point) (point-min)
                        (* (line-number-at-pos)
                           prompt-width)))
             (code (jupyter-repl-cell-code)))
        (jupyter-repl-replace-cell-code
         (with-jupyter-repl-lang-buffer
           (insert code)
           (goto-char (1+ offset))
           (indent-according-to-mode)
           (setq offset (- (point) (point-at-bol)))
           (buffer-string)))
        (goto-char (+ spoint offset))
        (widen)))))

;; TODO: Before change function to delete prompt when attempting to delete
;; past the code/prompt boundary.

(defun jupyter-repl-after-buffer-change (beg end len)
  (when (eq major-mode 'jupyter-repl-mode)
    (cond
     ;; Don't do anything when just typing normally
     ((and (> (- end beg) 1)
           ;; Only for insertions
           (= len 0))
      ;; If the text changed is part of a code cell make sure to add line
      ;; continuations for multiline text
      (when (get-text-property beg 'jupyter-prompt)
        (save-excursion
          (goto-char beg)
          (goto-char (point-at-bol))
          (let ((inhibit-read-only t)
                (p1 nil))
            (while (and (= (forward-line) 0)
                        (< (point) end))
              (setq p1 (point))
              (delete-char -1)
              (jupyter-repl-insert-prompt 'continuation)
              (setq end (+ end (- (point) p1)))))))))))

(defun jupyter-repl-before-buffer-change (beg end)
  (when (eq major-mode 'jupyter-repl-mode)
    (message "b %s e %s" beg end)))

(add-hook 'after-change-functions 'jupyter-repl-after-buffer-change nil t)
(add-hook 'before-change-functions 'jupyter-repl-before-buffer-change nil t)

(defun jupyter-repl-delete-backward-char (n &optional killflag)
  (interactive "p\nP")
  (condition-case err
      (call-interactively #'delete-backward-char)
    (text-read-only
     (let ((input-marker (oref jupyter-repl-current-client
                               input-start-marker))
           (lbp (point-at-bol)))
       (if (and (get-text-property lbp 'jupyter-prompt)
                (> (point) input-marker))
           (let ((inhibit-read-only t)
                 (prompt-end (or (next-single-property-change
                                  lbp 'jupyter-prompt)
                                 (point-max))))
             (delete-region lbp prompt-end)
             (join-line))
         (signal (car err) (cdr err)))))))

(defvar jupyter-repl-mode-map (let ((map (make-sparse-keymap)))
                                (define-key map (kbd "RET") #'jupyter-repl-ret)
                                (define-key map (kbd "DEL") #'jupyter-repl-delete-backward-char)
                                (define-key map (kbd "TAB") #'jupyter-repl-tab)
                                (define-key map (kbd "C-n") #'jupyter-reply-history-next)
                                (define-key map (kbd "C-p") #'jupyter-reply-history-previous)
                                map))

(defun jupyter-repl-kill-buffer-query-function ()
  (or (not (eq major-mode 'jupyter-repl-mode))
      (not (or (jupyter-kernel-alive-p jupyter-repl-kernel-manager)
               (jupyter-channels-running-p jupyter-repl-current-client)))
      (not (and (y-or-n-p
                 (format "Jupyter REPL (%S) still connected. Kill it? "
                         (buffer-name (current-buffer))))
                (prog1 t
                  (jupyter-stop-channels jupyter-repl-current-client)
                  (jupyter-stop-kernel jupyter-repl-kernel-manager))))))

(add-hook 'kill-buffer-query-functions #'jupyter-repl-kill-buffer-query-function)

;;; Completion

(defun company-jupyter-repl (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-jupyter-repl))
    (prefix (and (eq major-mode 'jupyter-repl-mode)
                 (not (get-text-property (point) 'read-only))
                 ;; Just grab a symbol, we will just send the whole code cell
                 (company-grab-symbol)))
    (candidates
     (cons
      :async
      (lexical-let ((arg arg))
        (lambda (cb)
          (let ((client jupyter-repl-current-client))
            (with-jupyter-repl-buffer client
              (jupyter-add-receive-callback
                  client 'complete-reply
                  (jupyter-request-complete
                   client
                   :code (jupyter-repl-cell-code)
                   ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Text-Representations.html
                   ;; Buffer positions are measured in character units
                   :pos (jupyter-repl-cell-position))
                (apply-partially
                 (lambda (cb msg)
                   (if (equal (plist-get
                               (jupyter-message-content msg) :status)
                              "ok")
                       (cl-destructuring-bind (&key matches &allow-other-keys)
                           (jupyter-message-content msg)
                         (funcall cb matches))
                     (funcall cb '())))
                 cb))))))))
    (sorted t)
    (doc-buffer
     (let* ((client jupyter-repl-current-client)
            (msg (jupyter-wait-until-received client 'inspect-reply
                   (jupyter-request-inspect
                    client :code arg :pos (length arg))))
            (doc nil))
       (when (and msg (equal (plist-get
                              (jupyter-message-content msg) :status)
                             "ok"))
         (cl-destructuring-bind (&key found data &allow-other-keys)
             (jupyter-message-content msg)
           (when found
             ;; TODO: Generalize this
             (setq doc (or (and (plist-get data :text/markdown)
                                (jupyter-repl-fontify-string
                                 (plist-get data :text/markdown)
                                 #'markdown-mode))
                           (plist-get data :text/plain)))
             (with-current-buffer (company-doc-buffer doc)
               (unless (plist-get data :text/markdown)
                 (ansi-color-apply-on-region (point-min) (point-max)))
               (current-buffer)))))))))

;;; The mode

(define-derived-mode jupyter-repl-mode fundamental-mode
  "Jupyter-REPL"
  "A major mode for interacting with a Jupyter kernel."
  (buffer-disable-undo))

(defun run-jupyter-repl (kernel-name)
  (interactive
   (list
    (completing-read
     "kernel: " (mapcar #'car (jupyter-available-kernelspecs)) nil t)))
  ;; TODO: kernel existence
  (let ((km (jupyter-kernel-manager :name kernel-name))
        (client nil))
    (message "Starting %s kernel..." kernel-name)
    ;; Populate connection info
    (jupyter-start-kernel km)
    (setq client (jupyter-repl-client :kernel-manager km))
    (oset client buffer (generate-new-buffer
                         (format "*jupyter-repl[%s]*" (oref km name))))
    (jupyter-start-channels client)
    (sleep-for 1)
    ;; TODO: Move this over to starting a kernel
    ;; TODO: Separate channel implementation to a blocking and event based channel.
    ;;
    ;; TODO: Put the kernel manager as a process item with process-put and when
    ;; the process dies, do cleanup in the process sentinel. Similarly for the
    ;; client.
    (oset km kernel-info
          (let ((info (cl-loop
                       ;; try sending the request multiple times to account for
                       ;; kernel startup
                       repeat 2
                       for info = (jupyter-wait-until-received client 'kernel-info-reply
                                    (jupyter-request-kernel-info client) 10)
                       when info return info)))
            (when info (plist-get info :content))))
    (unless (oref km kernel-info)
      (error "Kernel did not respond to kernel-info request."))
    (with-jupyter-repl-buffer client
      (erase-buffer)
      (jupyter-repl-mode)
      (setq jupyter-repl-current-client client)
      (setq jupyter-repl-kernel-manager km)
      ;; (setq-local jupyter-repl-history (make-ring 100))
      ;; (jupyter-request-history client :n 100)
      (cl-destructuring-bind (&key language_info banner &allow-other-keys)
          (oref km kernel-info)
        (let ((name (plist-get language_info :name))
              (ext (plist-get language_info :file_extension))
              (fld nil))
          ;; Set up the REPL language buffer and extract `font-lock-defaults'
          (setq jupyter-repl-lang-buffer (get-buffer-create
                                          (format " *jupyter-repl-lang-%s*"
                                                  name)))
          (with-jupyter-repl-lang-buffer
            (setq buffer-file-name (concat "jupyter-repl-lang" ext))
            (set-auto-mode)
            (setq buffer-file-name nil)
            (setq fld font-lock-defaults))
          ;; Set the `font-lock-defaults' to those of the REPL language
          (setq font-lock-defaults fld)
          (font-lock-mode))
        ;; Insert the REPL banner
        (jupyter-repl-insert banner "\n")
        (let ((inhibit-modification-hooks t))
          (add-text-properties
           (point-min) (point) '(font-lock-face shadow fontified t)))
        (jupyter-wait-until-idle
         client (jupyter-request-execute
                 client :code "" :silent t))))
    (pop-to-buffer (oref client buffer))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
