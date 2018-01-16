(require 'json)

(defvar jupyter--kernelspecs nil
  "An alist matching kernel names to their kernelspec
  directories.")

(defun jupyter-read-kernelspec (dir)
  "Return the kernelspec found in DIR.
If DIR contains a kernel.json file, assume that it is the
kernelspec of a kernel and return the plist created by a call to
`json-read-file'."
  (let ((json-object-type 'plist)
        (json-array-type 'list)
        (json-false nil)
        (file (expand-file-name "kernel.json" dir)))
    (if (file-exists-p file) (json-read-file file)
      (error "No kernel.json file found in %s" dir))))

(defun jupyter-available-kernelspecs (&optional force-new)
  "Get the available kernelspecs.
Return an alist mapping kernel names to their kernelspec
directories. The alist is formed by a call to the shell command

    jupyter kernelspec list

By default the available kernelspecs are cached. To force an
update of the cached kernelspecs set FORCE-NEW to a non-nil
value."
  (when (or (not jupyter--kernelspecs) force-new)
    (setq jupyter--kernelspecs
          (mapcar (lambda (s) (let ((s (split-string s " " 'omitnull)))
                      (cons (car s) (jupyter-read-kernelspec (cadr s)))))
             (seq-subseq
              (split-string
               (shell-command-to-string "jupyter kernelspec list")
               "\n" 'omitnull "[ \t]+")
              1))))
  jupyter--kernelspecs)

(defun jupyter-get-kernelspec (name &optional force-new)
  "Get the kernelspec for a kernel named NAME.
If no kernelspec is found for the kernel that has a name of NAME,
throw an error. Otherwise return the kernelspec plist. Optional
argument FORCE-NEW has the same meaning as in
`jupyter-available-kernelspecs'."
  (or (cdr (assoc name (jupyter-available-kernelspecs force-new)))
      (error "No kernelspec found (%s)" name)))

(defun jupyter-find-kernelspec (prefix &optional force-new)
  "Find the first kernelspec for the kernel that matches PREFIX.
From the available kernelspecs returned by
`jupyter-available-kernelspecs' return a cons cell

    (KERNEL-NAME . PLIST)

where KERNEL-NAME is the name of the kernel that begins with
PREFIX and PLIST is the kernelspec PLIST read from the
\"kernel.json\" file in the kernel's kernelspec directory.

If no kernelspec was found that matches PREFIX, return nil.

Optional argument FORCE-NEW has the same meaning as in
`jupyter-available-kernelspecs'."
  (when prefix
    (cl-find-if
     (lambda (s) (string-prefix-p prefix (car s)))
     (jupyter-available-kernelspecs force-new))))

(provide 'jupyter-kernelspec)
