(require 'json)

(defvar jupyter--kernelspec-dirs nil
  "An alist matching kernel names to their kernelspec
  directories.")

(defun jupyter-available-kernelspecs (&optional force-new)
  "Get the available kernelspecs.
Return an alist mapping kernel names to their kernelspec
directories. The alist is formed by a call to the shell command

    jupyter kernelspec list

By default the available kernelspecs are cached. To force an
update of the cached kernelspecs set FORCE-NEW to a non-nil
value."
  (unless (or jupyter--kernelspec-dirs force-new)
    (setq jupyter--kernelspec-dirs
          (mapcar (lambda (s) (let ((s (split-string s " " 'omitnull)))
                      (cons (car s) (cadr s))))
             (seq-subseq
              (split-string
               (shell-command-to-string "jupyter kernelspec list")
               "\n" 'omitnull "[ \t]+")
              1))))
  jupyter--kernelspec-dirs)

(defun jupyter-find-kernelspec (prefix)
  "Find the first kernelspec for the kernel that matches PREFIX.
From the available kernelspecs returned by
`jupyter-available-kernelspecs' return a cons cell

    (KERNEL-NAME . PLIST)

where KERNEL-NAME is the name of the kernel that begins with
PREFIX and PLIST is the kernelspec PLIST read from the
\"kernel.json\" file in the kernel's kernelspec directory."
  (when prefix
    (let ((kname-path (cl-find-if
                       (lambda (s) (string-prefix-p prefix (car s)))
                       (jupyter-available-kernelspecs)))
          (json-object-type 'plist)
          (json-array-type 'list)
          (json-false nil))
      (when kname-path
        (cons (car kname-path)
              (json-read-file
               (expand-file-name "kernel.json" (cdr kname-path))))))))

(provide 'jupyter-kernelspec)
