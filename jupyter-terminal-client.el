(require 'jupyter-client)
(require 'xterm-color)
(require 'shr)

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

(defvar-local jupyter-repl-history nil
  "The history of the current Jupyter REPL.")

;;; Convenience macros

(defmacro with-jupyter-repl-buffer (client &rest body)
  (declare (indent 1) (debug (symbolp &rest form)))
  `(with-current-buffer (oref ,client buffer)
     (let ((inhibit-read-only t))
       (prog1 (progn ,@body)
         (let ((win (get-buffer-window)))
           (when win (set-window-point win (point))))))))

(defmacro jupyter-repl-do-at-request (client req &rest body)
  "Set `point' to the end of the output for REQ and run BODY.

BODY is executed with `point' at the proper location to insert
any output for REQ. If REQ already has output, `point' will be at
the end of the output so that any text inserted by BODY will be
appended to the current output for REQ."
  (declare (indent 2) (debug (symbolp &rest form)))
  `(with-jupyter-repl-buffer ,client
     (save-excursion
       (let ((inhibit-modification-hooks t))
         (jupyter-repl-goto-request req)
         (jupyter-repl-next-cell)
         ,@body))))

(defmacro with-jupyter-repl-lang-buffer (&rest body)
  (declare (indent 0) (debug (&rest form)))
  `(with-current-buffer jupyter-repl-lang-buffer
     (let ((inhibit-read-only t))
       (erase-buffer)
       ,@body)))

(defmacro with-jupyter-repl-cell (&rest body)
  "Narrow the buffer to the current code cell and widen afterwards."
  (declare (indent 0) (debug (&rest form)))
  `(save-excursion
     (save-restriction
       (narrow-to-region (jupyter-repl-cell-beginning-position)
                         (jupyter-repl-cell-end-position))
       ,@body)))

;;; Inserting text into the REPL buffer

;; TODO: Do like how org-mode does it and cache buffers which have the
;; major mode enabled already
(defun jupyter-repl-fontify-according-to-mode (mode str)
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

(defun jupyter-repl-newline ()
  (jupyter-repl-insert "\n"))

(defun jupyter-repl-insert-html (html)
  (let ((start (point)))
    (shr-insert-document
     (with-temp-buffer
       (insert html)
       (libxml-parse-html-region (point-min) (point-max))))
    (add-text-properties start (point) '(read-only t))))

(defun jupyter-repl-insert-latex (tex)
  (require 'org)
  (let (beg end)
    (setq beg (point))
    (jupyter-repl-insert (plist-get data :text/latex))
    (setq end (point))
    (org-format-latex
     "jupyter-repl" beg end "jupyter-repl"
     'overlays "Creating LaTeX image...%s"
     'forbuffer
     ;; Use the default method for creating image files
     org-preview-latex-default-process)))

(defun jupyter-repl-insert-data (data)
  (let ((mimetypes (seq-filter #'keywordp data)))
    (cond
     ((memq :image/png mimetypes)
      (jupyter-repl-newline)
      (insert-image
       (create-image
        (base64-decode-string
         (plist-get data :image/png))
        nil 'data)
       (propertize " " 'read-only t)))
     ((memq :text/html mimetypes)
      ;; TODO: If this can fail handle the execute request again but with
      ;; the html key removed from the data plist
      (jupyter-repl-insert-html (plist-get data :text/html))
      (jupyter-repl-newline))
     ((memq :text/latex mimetypes)
      (jupyter-repl-insert-latex (plist-get data :text/latex)))
     ((memq :text/markdown mimetypes)
      (jupyter-repl-newline)
      (jupyter-repl-insert
       (jupyter-repl-fontify-according-to-mode
        'markdown-mode (plist-get data :text/markdown))))
     ((memq :text/plain mimetypes)
      (let ((text (xterm-color-filter (plist-get data :text/plain))))
        (add-text-properties
         0 (length text) '(fontified t font-lock-fontified t
                                     font-lock-multiline t)
         text)
        (font-lock-fillin-text-property
         0 (length text) 'font-lock-face 'default text)
        (when (jupyter-repl-multiline-p text)
          (jupyter-repl-newline))
        (jupyter-repl-insert text)))
     (t (error "No supported mimetype found %s" mimetypes)))))

;;; Prompt

(defun jupyter-repl--prompt-display-value (str face)
  (list '(margin left-margin)
        (propertize
         (concat (make-string
                  (- jupyter-repl-prompt-margin-width
                     (length str))
                  ? )
                 str)
         'fontified t
         'font-lock-face face)))

(defun jupyter-repl--insert-prompt (str face)
  (jupyter-repl-newline)
  (let ((ov (make-overlay (1- (point)) (point) nil t))
        (md (jupyter-repl--prompt-display-value str face)))
    (overlay-put ov 'after-string (propertize " " 'display md))
    (overlay-put ov 'evaporate t)
    ov))

(defun jupyter-repl-insert-prompt (&optional type)
  "Insert a REPL promp in CLIENT's buffer according to type.
If TYPE is nil or `in' insert a new input prompt. If TYPE is
`out' insert a new output prompt."
  (setq type (or type 'in))
  (unless (memq type '(in out continuation))
    (error "Prompt type can only be (`in', `out', or `continuation')"))
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t)
        ov props)
    (cond
     ((eq type 'in)
      (let ((count (oref jupyter-repl-current-client execution-count)))
        (setq ov (jupyter-repl--insert-prompt
                  (format "In [%d]:" count) 'jupyter-repl-input-prompt)
              props (list 'jupyter-cell (list 'beginning count)
                          'rear-nonsticky t))))
     ((eq type 'out)
      (let ((count (jupyter-repl-cell-count)))
        (setq ov (jupyter-repl--insert-prompt
                  (format "Out [%d]:" count) 'jupyter-repl-output-prompt)
              props (list 'jupyter-cell (list 'out count)))))
     ((eq type 'continuation)
      (setq ov (jupyter-repl--insert-prompt
                ":" 'jupyter-repl-input-prompt)
            props (list 'read-only nil 'rear-nonsticky t))))
    (add-text-properties (overlay-start ov) (overlay-end ov) props)))

(defun jupyter-repl-update-cell-prompt (str)
  (let ((ov (car (overlays-at (jupyter-repl-cell-beginning-position)))))
    (when ov
      (overlay-put ov 'after-string
                   (propertize
                    " " 'display (jupyter-repl--prompt-display-value
                                  str 'jupyter-repl-input-prompt))))))

(defun jupyter-repl-mark-cell-busy ()
  (jupyter-repl-update-cell-prompt "In [*]:"))

(defun jupyter-repl-unmark-cell-busy ()
  (jupyter-repl-update-cell-prompt
   (format "In [%d]:" (jupyter-repl-cell-count))))

(defun jupyter-repl-cell-count (&optional cross-boundary)
  (let ((pos (if (jupyter-repl-cell-beginning-p) (point)
               (save-excursion
                 (jupyter-repl-previous-cell)
                 (point)))))
    (nth 1 (get-text-property pos 'jupyter-cell))))

(defun jupyter-repl-cell-request ()
  (get-text-property (jupyter-repl-cell-beginning-position) 'jupyter-request))

;;; Cell motions

(defun jupyter-repl-cell-beginning-position ()
  (let ((pos (point)))
    (while (not (jupyter-repl-cell-beginning-p pos))
      (setq pos (or (previous-single-property-change pos 'jupyter-cell)
                    ;; Edge case when `point-min' is the beginning of a cell
                    (and (jupyter-repl-cell-beginning-p (point-min))
                         (point-min))))
      (if pos (when (jupyter-repl-cell-end-p pos)
                (error "Found end of previous cell."))
        (signal 'beginning-of-buffer nil)))
    pos))

(defun jupyter-repl-cell-end-position ()
  (let ((pos (point)))
    (catch 'unfinalized
      (while (not (jupyter-repl-cell-end-p pos))
        (setq pos (next-single-property-change pos 'jupyter-cell))
        (if pos (when (jupyter-repl-cell-beginning-p pos)
                  (error "Found beginning of next cell."))
          ;; Any unfinalized cell must be at the end of the buffer.
          (throw 'unfinalized (point-max))))
      pos)))

(defun jupyter-repl-cell-code-beginning-position ()
  (let ((pos (jupyter-repl-cell-beginning-position)))
    (if pos (1+ pos)
      (point-min))))

(defun jupyter-repl-cell-code-end-position ()
  (let ((pos (jupyter-repl-cell-end-position)))
    (if (= pos (point-max)) (point-max)
      (1- pos))))

(defun jupyter-repl-next-cell (&optional N)
  "Go to the start of the next cell.
Returns the count of cells left to move."
  (interactive "N")
  (setq N (or N 1))
  (let ((dir (if (> N 0) 'forward 'backward)))
    (if (> N 0) (setq fun #'next-single-property-change)
      (setq N (abs N)
            fun #'previous-single-property-change))
    (catch 'done
      (while (> N 0)
        (let ((pos (funcall fun (point) 'jupyter-cell)))
          (while (and pos (not (jupyter-repl-cell-beginning-p pos)))
            (setq pos (funcall fun pos 'jupyter-cell)))
          (if pos (progn
                    (goto-char pos)
                    (setq N (1- N)))
            (goto-char (if (eq dir 'forward) (point-max)
                         (point-min)))
            (throw 'done t)))))
    N))

(defun jupyter-repl-previous-cell (&optional N)
  "Go to the start of the previous cell.
Returns the count of cells left to move."
  (interactive "N")
  (setq N (or N 1))
  (jupyter-repl-next-cell (- N)))

;;; Predicates to determine what kind of line point is in

(defun jupyter-repl-cell-beginning-p (&optional pos)
  (setq pos (or pos (point)))
  (eq (nth 0 (get-text-property pos 'jupyter-cell)) 'beginning))

(defun jupyter-repl-cell-end-p (&optional pos)
  (setq pos (or pos (point)))
  (eq (nth 0 (get-text-property pos 'jupyter-cell)) 'end))

(defun jupyter-repl-multiline-p (text)
  (string-match "\n" text))

(defun jupyter-repl-cell-line-p ()
  "Is the current line an input cell line?"
  (or (overlays-at (point-at-eol))
      (when (>= (1- (point-at-bol)) (point-min))
        (and (overlays-at (1- (point-at-bol)))
             (not (eq (car (get-text-property (1- (point-at-bol))
                                              'jupyter-cell))
                      'out))))))

;;; Getting/manipulating the code of a cell

(defun jupyter-repl-cell-code ()
  "Get the code of the cell without prompts."
  (if (= (point-min) (point-max)) ""
    (let (lines)
      (save-excursion
        (goto-char (jupyter-repl-cell-code-beginning-position))
        (push (buffer-substring-no-properties (point-at-bol) (point-at-eol))
              lines)
        (while (and (line-move-1 1 'noerror)
                    (jupyter-repl-cell-line-p))
          (push (buffer-substring-no-properties (point-at-bol) (point-at-eol))
                lines))
        (mapconcat #'identity (nreverse lines) "\n")))))

(defun jupyter-repl-cell-code-position ()
  "Get the position that `point' is at relative to the contents of the cell.
The first character of the cell code corresponds to position 1."
  (unless (jupyter-repl-cell-line-p)
    (error "Not in code of cell."))
  (- (point) (jupyter-repl-cell-beginning-position)))

(defun jupyter-repl-finalize-cell (req)
  "Make the current cell read only."
  (let* ((beg (jupyter-repl-cell-beginning-position))
         (count (jupyter-repl-cell-count)))
    (jupyter-repl-mark-cell-busy)
    (goto-char (point-max))
    (jupyter-repl-newline)
    (add-text-properties
     (1- (point)) (point) (list 'jupyter-cell (list 'end count)))
    (add-text-properties beg (1+ beg) (list 'jupyter-request req))
    ;; font-lock-multiline to avoid improper syntactic elements from
    ;; spilling over to the rest of the buffer.
    (add-text-properties beg (point) '(read-only t font-lock-multiline t))))

(defun jupyter-repl-replace-cell-code (new-code)
  (delete-region (jupyter-repl-cell-code-beginning-position)
                 (jupyter-repl-cell-code-end-position))
  (jupyter-repl-insert :read-only nil new-code))

(defun jupyter-repl-truncate-buffer ()
  (let ((inhibit-modification-hooks t))
    (save-excursion
      (when (= (forward-line (- jupyter-repl-maximum-size)) 0)
        (jupyter-repl-next-cell)
        (beginning-of-line)
        (delete-region (point-min) (point))))))

;;; Handlers

(defun jupyter-repl-goto-request (req)
  (goto-char (point-max))
  (condition-case nil
      (goto-char (jupyter-repl-cell-beginning-position))
    ;; Handle error when seeing the end of the previous cell
    (error (jupyter-repl-previous-cell)))
  (let (cell-req)
    (while (and (not (eq (setq cell-req (jupyter-repl-cell-request)) req))
                (not (= (point) (point-min))))
      (jupyter-repl-previous-cell))
    ;; Unless the request was found, assume it is for the current un-finalized
    ;; cell
    (unless (eq cell-req req)
      (goto-char (point-max)))))

(defun jupyter-repl-history-add-input (code)
  (ring-insert jupyter-repl-history code))

(cl-defmethod jupyter-request-execute ((client jupyter-repl-client)
                                       &key code
                                       (silent nil)
                                       (store-history t)
                                       (user-expressions nil)
                                       (allow-stdin t)
                                       (stop-on-error nil))
  (with-jupyter-repl-buffer client
    (jupyter-repl-truncate-buffer)
    (if code (cl-call-next-method)
      (setq code (jupyter-repl-cell-code))
      ;; Handle empty code cells as just an update of the prompt number
      (if (= (length code) 0)
          (setq silent t)
        ;; Needed by the prompt insertion below
        (oset client execution-count (1+ (oref client execution-count))))
      (let ((req (cl-call-next-method
                  client :code code :silent silent :store-history store-history
                  :user-expressions user-expressions :allow-stdin allow-stdin
                  :stop-on-error stop-on-error))
            ;; Don't insert a continuation prompt when inserting a new input
            ;; prompt
            (inhibit-modification-hooks t))
        (jupyter-repl-finalize-cell req)
        (jupyter-repl-insert-prompt 'in)
        req))))

(cl-defmethod jupyter-handle-execute ((client jupyter-repl-client)
                                      req
                                      execution-count
                                      user-expressions
                                      payload)
  (oset client execution-count (1+ execution-count))
  (with-jupyter-repl-buffer client
    (let ((pos (point)))
      ;; KLUDGE: Clean this up. It seems the semantics of
      ;; `jupyter-repl-goto-request' depends on a lot of assumptions. This
      ;; condition case is necessary because of the initial state of the
      ;; buffer.
      (when (condition-case nil
                (progn
                  (jupyter-repl-goto-request req)
                  t)
              ;; Handle the initial state of the buffer when starting and
              ;; possibly other cases
              (beginning-of-buffer
               (goto-char (point-max))
               (jupyter-repl-insert-prompt 'in)
               nil))
        (jupyter-repl-unmark-cell-busy)
        (goto-char pos)))))

(cl-defmethod jupyter-handle-execute-input ((client jupyter-repl-client)
                                            req
                                            code
                                            execution-count)
  (oset client execution-count (1+ execution-count)))

(cl-defmethod jupyter-handle-execute-result ((client jupyter-repl-client)
                                             req
                                             execution-count
                                             data
                                             metadata)
  (oset client execution-count execution-count)
  (jupyter-repl-do-at-request client req
    (jupyter-repl-insert-prompt 'out)
    (jupyter-repl-insert-data data)))

(cl-defmethod jupyter-handle-display-data ((client jupyter-repl-client)
                                           req data metadata transient)
  (jupyter-repl-do-at-request client req
    (jupyter-repl-insert-data data)))

(cl-defmethod jupyter-handle-stream ((client jupyter-repl-client) req name text)
  (jupyter-repl-do-at-request client req
    (jupyter-repl-insert (xterm-color-filter text))))

(cl-defmethod jupyter-handle-error ((client jupyter-repl-client)
                                    req ename evalue traceback)
  (jupyter-repl-do-at-request client req
    (save-excursion
      ;; `point' is at the cell beginning of the next cell after REQ,
      ;; `jupyter-repl-previous-cell' will take us back to the start of the
      ;; cell corresponding to REQ.
      (jupyter-repl-previous-cell)
      (jupyter-repl-unmark-cell-busy))
    (let ((s (mapconcat #'xterm-color-filter traceback "\n")))
      (add-text-properties
       0 (length s) '(fontified t font-lock-fontified t font-lock-multiline t) s)
      (font-lock-fillin-text-property
       0 (length s) 'font-lock-face 'default s)
      (jupyter-repl-insert s))
    (jupyter-repl-newline)))

(cl-defmethod jupyter-handle-input ((client jupyter-repl-client) req prompt password)
  (jupyter-repl-do-at-request client req
    (let ((value (cl-call-next-method)))
      (jupyter-repl-previous-cell)
      (goto-char (1+ (jupyter-repl-cell-end-position)))
      (jupyter-repl-newline)
      (jupyter-repl-insert (concat prompt value)))))

(defun jupyter-repl-history-next (n)
  (interactive "p")
  (setq n (or n 1))
  (cl-loop repeat n
           do (ring-insert
               jupyter-repl-history (ring-remove jupyter-repl-history -1)))
  (jupyter-repl-replace-cell-code
   (ring-ref jupyter-repl-history -1)))

(defun jupyter-repl-history-previous (n)
  (interactive "p")
  (setq n (or n 1))
  (cl-loop repeat n
           do (ring-insert-at-beginning
               jupyter-repl-history (ring-remove jupyter-repl-history 0)))
  (jupyter-repl-replace-cell-code
   (ring-ref jupyter-repl-history -1)))

(cl-defmethod jupyter-handle-history ((client jupyter-repl-client) req history)
  (with-jupyter-repl-buffer client
    (cl-loop for (session line-number input-output) in history
             do (ring-insert jupyter-repl-history input-output))))

(cl-defmethod jupyter-handle-is-complete ((client jupyter-repl-client) req status indent)
  (with-jupyter-repl-buffer client
    (pcase status
      ("complete"
       (jupyter-request-execute client))
      ("incomplete"
       (jupyter-repl-newline)
       (if (= (length indent) 0) (jupyter-repl-indent-line)
         (jupyter-repl-insert :read-only nil indent)))
      ("invalid"
       ;; Force an execute to produce a traceback
       (jupyter-request-execute client))
      ("unknown"))))

(defun jupyter-repl-ret (arg)
  (interactive "P")
  (let (current-cell-pos)
    (setq current-cell-pos
          (save-excursion
            (goto-char (point-max))
            (condition-case nil
                (jupyter-repl-cell-beginning-position)
              ;; No way to find end of previous cell if we go to `point-max'
              (beginning-of-buffer nil))))
    (if current-cell-pos
        (if (< (point) current-cell-pos)
            (goto-char (point-max))
          ;; TODO: Not all kernels will respond to an is_complete_request. The
          ;; jupyter console will switch to its own internal handler when the
          ;; request times out.
          (let* ((code (jupyter-repl-cell-code))
                 (res (jupyter-wait-until-received
                          jupyter-repl-current-client
                          'is-complete-reply
                        (jupyter-request-is-complete
                         jupyter-repl-current-client
                         :code code))))
            ;; If the kernel responds to an is-complete request then the
            ;; is-complete handler takes care of executing the code.
            (unless res
              ;; TODO: Indent or send (on prefix arg)?
              )))
      (let ((inhibit-read-only t))
        (jupyter-repl-insert-prompt 'in)))))

;; TODO: Before change function to delete prompt when attempting to delete
;; past the code/prompt boundary.
(defun jupyter-repl-indent-line ()
  "Indent the line according to the language of the REPL."
  (let* ((spos (jupyter-repl-cell-code-beginning-position))
         (pos (jupyter-repl-cell-code-position))
         (code (jupyter-repl-cell-code))
         (inhibit-read-only t))
    (jupyter-repl-replace-cell-code
     (with-jupyter-repl-lang-buffer
       (insert code)
       (goto-char pos)
       (indent-according-to-mode)
       (setq pos (point))
       (buffer-string)))
    (goto-char (+ pos spos))))

;;; Buffer change functions

(defun jupyter-repl-after-buffer-change (beg end len)
  (when (eq major-mode 'jupyter-repl-mode)
    (cond
     ;; Insertions only
     ((= len 0)
      (goto-char beg)
      (when (jupyter-repl-cell-line-p)
        (while (search-forward "\n" end 'noerror)
          (delete-char -1)
          (jupyter-repl-insert-prompt 'continuation)))
      (goto-char end)))))

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
                   :pos (jupyter-repl-cell-code-position))
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

(defvar jupyter-repl-mode-map (let ((map (make-sparse-keymap)))
                                (define-key map (kbd "RET") #'jupyter-repl-ret)
                                (define-key map (kbd "C-n") #'jupyter-repl-history-next)
                                (define-key map (kbd "C-p") #'jupyter-repl-history-previous)
                                map))

(define-derived-mode jupyter-repl-mode fundamental-mode
  "Jupyter-REPL"
  "A major mode for interacting with a Jupyter kernel."
  (setq-local indent-line-function #'jupyter-repl-indent-line)
  (setq-local company-backends (cons 'company-jupyter-repl company-backends))
  (add-hook 'after-change-functions 'jupyter-repl-after-buffer-change nil t))

(defun jupyter-repl-initialize-fontification (ext)
  (let (fld)
    (with-jupyter-repl-lang-buffer
      (setq buffer-file-name (concat "jupyter-repl-lang" ext))
      (set-auto-mode)
      (setq buffer-file-name nil)
      (setq fld font-lock-defaults))
    ;; Set the `font-lock-defaults' to those of the REPL language
    (setq font-lock-defaults fld)
    (font-lock-mode)))

(defun jupyter-repl-insert-banner (banner)
  (let ((start (point))
        (inhibit-modification-hooks t))
    (jupyter-repl-insert banner)
    (jupyter-repl-newline)
    (add-text-properties start (point) '(font-lock-face shadow fontified t))))

(defun jupyter-repl-update-execution-counter ()
  (jupyter-add-receive-callback jupyter-repl-current-client
      'execute-reply
      (jupyter-request-execute
       jupyter-repl-current-client :code "" :silent t)
    (apply-partially
     (lambda (client msg)
       (oset client execution-count
             (plist-get (jupyter-message-content msg) :execution_count))
       (with-jupyter-repl-buffer client
         (jupyter-repl-insert-prompt 'in))
       ;; Don't pass to handlers
       nil)
     jupyter-repl-current-client)))

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
    ;; hb channel starts in a paused state
    ;; (jupyter-hb-unpause (oref client hb-channel))
    ;;
    ;; TODO: Move this over to starting a kernel
    (oset km kernel-info
          (let ((info (cl-loop
                       ;; try sending the request multiple times to account for
                       ;; kernel startup
                       repeat 2
                       for info = (jupyter-wait-until-received
                                      client 'kernel-info-reply
                                    (jupyter-request-kernel-info client) 10)
                       when info return info)))
            (when info (plist-get info :content))))
    (unless (oref km kernel-info)
      (error "Kernel did not respond to kernel-info request."))
    (with-jupyter-repl-buffer client
      (cl-destructuring-bind (&key language_info banner &allow-other-keys)
          (oref km kernel-info)
        (cl-destructuring-bind (&key name file_extension &allow-other-keys)
            language_info
          (erase-buffer)
          (jupyter-repl-mode)
          (setq jupyter-repl-current-client client)
          (setq jupyter-repl-kernel-manager km)
          (setq jupyter-repl-history (make-ring 100))
          (setq left-margin-width jupyter-repl-prompt-margin-width)
          ;; TODO: Cleanup of buffers created by jupyter-repl
          (setq jupyter-repl-lang-buffer (get-buffer-create
                                          (format " *jupyter-repl-lang-%s*" name)))
          (jupyter-request-history client :n 100 :raw nil :unique t)
          (jupyter-repl-initialize-fontification file_extension)
          (jupyter-repl-insert-banner banner)
          (jupyter-repl-update-execution-counter))))
    (pop-to-buffer (oref client buffer))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
