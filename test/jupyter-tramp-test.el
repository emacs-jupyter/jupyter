;;; jupyter-tramp-test.el --- Tests for the contents REST API integration with TRAMP -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 28 May 2019

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

;; Test integration of Jupyter REST contents API with TRAMP.

;;; Code:

(require 'jupyter-tramp)

(ert-deftest jupyter-tramp-file-name-p ()
  :tags '(tramp)
  (should-not (jupyter-tramp-file-name-p "foobar"))
  (should-not (jupyter-tramp-file-name-p "/foobar"))
  (should-not (jupyter-tramp-file-name-p "/ssh::foobar"))
  (should (equal (jupyter-tramp-file-name-p "/jpy::foobar") "jpy"))
  (should (equal (jupyter-tramp-file-name-p "/jpy::/foobar") "jpy"))
  (should (equal (jupyter-tramp-file-name-p "/jpys::/foobar") "jpys")))

(ert-deftest jupyter-tramp-file-directory-p ()
  :tags '(tramp)
  (jupyter-test-at-temporary-directory
    (let* ((tfile (make-temp-file "file-directory-p"))
           (tdir (make-temp-file "file-directory-p" 'directory))
           (jpyfile (expand-file-name (file-name-nondirectory tfile) "/jpy::/"))
           (jpydir (expand-file-name (file-name-nondirectory tdir) "/jpy::/")))
      (unwind-protect
          (progn
            (should (file-exists-p jpyfile))
            (should (file-exists-p jpydir))
            (should-not (file-directory-p jpyfile))
            (should (file-directory-p jpydir)))
        (delete-directory tdir)
        (delete-file tfile)))))

(ert-deftest jupyter-tramp-file-writable-p ()
  :tags '(tramp)
  (jupyter-test-at-temporary-directory
    (let* ((tname (make-temp-name "file-writable-p"))
           (jpyfile (expand-file-name tname "/jpy::/")))
      (should-not (file-exists-p tname))
      ;; TODO: To test this fully we would have to start the Jupyter server in
      ;; a less privileged state than the current user.
      (should (file-writable-p jpyfile)))))

(ert-deftest jupyter-tramp-make-directory ()
  :tags '(tramp)
  (jupyter-test-at-temporary-directory
    (let* ((tdir (make-temp-name "make-directory"))
           (jpydir (expand-file-name tdir "/jpy::/")))
      (should-not (file-exists-p tdir))
      (should-not (file-directory-p tdir))
      (unwind-protect
          (progn
            (make-directory jpydir)
            (should (file-exists-p tdir))
            (should (file-directory-p tdir)))
        (when (file-directory-p tdir)
          (delete-directory tdir))))))

(ert-deftest jupyter-tramp-file-local-copy ()
  :tags '(tramp)
  (jupyter-test-at-temporary-directory
    (let* ((tfile (make-temp-file "file-local-copy"))
           (jpyfile (expand-file-name (file-name-nondirectory tfile) "/jpy::/")))
      (unwind-protect
          (let ((contents (concat "αβ" (jupyter-new-uuid) "λ")))
            (with-temp-file tfile
              (setq buffer-file-coding-system 'utf-8-auto)
              (insert contents))
            (let ((lfile (file-local-copy jpyfile)))
              (unwind-protect
                  (with-temp-buffer
                    (should-not (file-remote-p lfile))
                    (should-not (equal lfile tfile))
                    (let ((coding-system-for-read 'utf-8-auto))
                      (insert-file-contents lfile))
                    (should (equal (buffer-string) contents)))
                (delete-file lfile))))
        (delete-file tfile)))))

(ert-deftest jupyter-tramp-rename-file ()
  :tags '(tramp)
  (jupyter-test-at-temporary-directory
    (let* ((tfile (make-temp-file "rename-file"))
           (tnewname (jupyter-new-uuid))
           (jpyfile (expand-file-name (file-name-nondirectory tfile) "/jpy::/"))
           (jpynewname (expand-file-name tnewname "/jpy::/")))
      (ert-info ("Remote to same remote")
        (should-not (file-exists-p tnewname))
        (unwind-protect
            (let ((contents (jupyter-new-uuid)))
              (with-temp-file tfile
                (insert contents))
              (rename-file jpyfile jpynewname)
              (should (file-exists-p tnewname))
              (unwind-protect
                  (with-temp-buffer
                    (insert-file-contents tnewname)
                    (should (equal (buffer-string) contents)))
                (ignore-errors (delete-file tnewname))))
          (ignore-errors (delete-file tfile))))
      (ert-info ("Local to remote")
        (unwind-protect
            (let ((contents (jupyter-new-uuid)))
              (should-not (file-exists-p tfile))
              (should-not (file-exists-p jpyfile))
              (with-temp-file tfile
                (insert contents))
              (should (file-exists-p tfile))
              (rename-file tfile jpynewname)
              (should-not (file-exists-p tfile))
              (should (file-exists-p tnewname)))
          (ignore-errors (delete-file tnewname))
          (ignore-errors (delete-file tfile)))))))

(ert-deftest jupyter-tramp-copy-file ()
  :tags '(tramp)
  (jupyter-test-at-temporary-directory
    (cl-macrolet
        ((file-contents
          (f) `(with-temp-buffer
                 (insert-file-contents ,f)
                 (buffer-string)))
         (check-copy
          (f1 f2 c)
          `(progn
             (should-not (file-exists-p ,f1))
             (write-region ,c nil ,f1)
             (should (file-exists-p ,f1))
             (unwind-protect
                 (unwind-protect
                     (progn
                       (copy-file ,f1 ,f2)
                       (should (file-exists-p ,f2))
                       (should (equal ,c (file-contents ,f2))))
                   (ignore-errors (delete-file (file-name-nondirectory ,f2))))
               (ignore-errors (delete-file (file-name-nondirectory ,f1)))))))
      (ert-info ("Local to remote")
        (let ((tf1 (make-temp-name "copy-file"))
              (jpy1 (expand-file-name (make-temp-name "copy-file") "/jpy::/"))
              (c1 (jupyter-new-uuid)))
          (check-copy tf1 jpy1 c1)))
      (ert-info ("Remote to local")
        (let ((tf1 (make-temp-name "copy-file"))
              (jpy1 (expand-file-name (make-temp-name "copy-file") "/jpy::/"))
              (c1 (jupyter-new-uuid)))
          (check-copy jpy1 tf1 c1)))
      (ert-info ("Remote to remote")
        (let ((jpy1 (expand-file-name (make-temp-name "copy-file") "/jpy::/"))
              (jpy2 (expand-file-name (make-temp-name "copy-file") "/jpy::/"))
              (c1 (jupyter-new-uuid)))
          (check-copy jpy1 jpy2 c1))))))

(ert-deftest jupyter-tramp-delete-file ()
  :tags '(tramp)
  (jupyter-test-at-temporary-directory
    (let* ((tfile (make-temp-file "delete-file"))
           (tdir (make-temp-file "delete-file" 'directory))
           (jpyfile (expand-file-name (file-name-nondirectory tfile) "/jpy::/"))
           (jpydir (expand-file-name (file-name-nondirectory tdir) "/jpy::/")))
      (should (file-exists-p tfile))
      (should (file-exists-p jpyfile))
      (should (file-exists-p tdir))
      (should (file-exists-p jpydir))
      (unwind-protect
          (progn
            (ert-info ("Error when attempting to delete a directory")
              (should-error (delete-file jpydir)))
            (ert-info ("Delete a file")
              (delete-file jpyfile)
              (should-not (file-exists-p tfile))
              (ert-info ("Ensure cache is cleared")
                (should-not (file-exists-p jpyfile)))))
        (when (file-exists-p tfile)
          (delete-file tfile))
        (when (file-exists-p tdir)
          (delete-directory tdir))))))

(ert-deftest jupyter-delete-directory ()
  :tags '(tramp)
  (jupyter-test-at-temporary-directory
    (let* ((tfile (make-temp-file "delete-directory"))
           (tdir (make-temp-file "delete-directory" 'directory))
           (jpyfile (expand-file-name (file-name-nondirectory tfile) "/jpy::/"))
           (jpydir (expand-file-name (file-name-nondirectory tdir) "/jpy::/")))
      (should (file-exists-p tfile))
      (should (file-exists-p jpyfile))
      (should (file-exists-p tdir))
      (should (file-exists-p jpydir))
      (unwind-protect
          (progn
            (ert-info ("Error when attempting to delete a file")
              (should-error (delete-directory jpyfile)))
            (ert-info ("Delete a directory")
              (let ((tfile2 (expand-file-name "foobar" jpydir)))
                (write-region "xxx" nil tfile2)
                (unwind-protect
                    (progn
                      (ert-info ("Error when directory contains files")
                        (should-error (delete-directory jpydir)))
                      (ert-info ("Unless recusrive is specifed")
                        (delete-directory jpydir t)
                        (should-not (file-exists-p tfile2))
                        (should-not (file-directory-p tdir))))
                  (when (file-exists-p tfile2)
                    (delete-file tfile2))))
              (should-not (file-exists-p tdir))
              (ert-info ("Ensure cache is cleared")
                (should-not (file-exists-p jpydir)))))
        (when (file-exists-p tfile)
          (delete-file tfile))
        (when (file-exists-p tdir)
          (delete-directory tdir t))))))

(ert-deftest jupyter-tramp-file-attributes ()
  :tags '(tramp)
  (jupyter-test-at-temporary-directory
    (let* ((file (make-temp-file "file-attributes"))
           (jpyfile (expand-file-name
                     (file-name-nondirectory file) "/jpy::/")))
      (set-file-modes file (string-to-number "600" 8))
      (write-region (make-string (1+ (random 100)) ?x) nil file)
      (unwind-protect
          (let ((attrs (file-attributes file 'string))
                (jpyattrs (file-attributes jpyfile 'string)))
            (should-not (or (null (file-attribute-size attrs))
                            (zerop (file-attribute-size attrs))))
            ;; Remove the usec and psec resolution
            (dolist (s '(2 3))
              (setf (nth s (file-attribute-modification-time attrs)) 0)
              (setf (nth s (file-attribute-status-change-time attrs)) 0)
              (setf (nth s (file-attribute-modification-time jpyattrs)) 0)
              (setf (nth s (file-attribute-status-change-time jpyattrs)) 0))
            (should (equal (nth 0 attrs) (nth 0 jpyattrs)))
            (dolist (item '(file-attribute-modification-time
                            file-attribute-status-change-time
                            ;; We always use the mode 600 since the file modes
                            ;; are not accessible by a user. The file should
                            ;; always be writable when testing since the server
                            ;; is started by the current Emacs process.
                            ;; file-attribute-modes
                            file-attribute-size))
              (should (equal (funcall item attrs)
                             (funcall item jpyattrs)))))
        (delete-file file)))))

(ert-deftest jupyter-tramp-expand-file-name ()
  :tags '(tramp)
  (should (equal "/foo" (tramp-drop-volume-letter (expand-file-name "/foo" "/jpy:h:/foo"))))
  (should (equal "~/foo" (abbreviate-file-name (expand-file-name "~/foo" "/jpy:h:/foo"))))
  (should (equal "/jpy:h:/foo/bar" (expand-file-name "bar" "/jpy:h:/foo")))
  (should (equal "/jpy:h:/foo/bar" (expand-file-name "bar" "/jpy:h:/foo/")))
  (should (equal "/jpy:h:/foo/bar" (expand-file-name "/jpy:h:/foo/bar")))
  (should (equal "/jpy:h:/foo/bar" (expand-file-name "/jpy:h:foo/bar")))
  (let ((default-directory "/jpy:h:/"))
    (should (equal "/jpy:h:/foo" (expand-file-name "foo"))))
  (let ((default-directory nil))
    (should (equal "/foo" (tramp-drop-volume-letter
                           (jupyter-tramp-expand-file-name "foo"))))))

;; TODO
(ert-deftest jupyter-tramp-file-name-all-completions ()
  :tags '(tramp))

;; TODO
(ert-deftest jupyter-tramp-file-remote-p ()
  :tags '(tramp))

(ert-deftest jupyter-tramp-write-region ()
  :tags '(tramp)
  (jupyter-test-at-temporary-directory
    (let* ((file (make-temp-file "write-region"))
           (jpyfile (expand-file-name
                     (file-name-nondirectory file) "/jpy::/")))
      (unwind-protect
          (cl-macrolet ((file-contents
                         () `(with-temp-buffer
                               (insert-file-contents-literally file)
                               (buffer-string))))
            (should-error (write-region "foo" nil jpyfile nil nil nil 'excl))
            (ert-info ("Basic write")
              (write-region "foo" nil jpyfile)
              (should (equal (file-contents) "foo"))
              (write-region "foλo" nil jpyfile)
              (should (equal (encode-coding-string (file-contents) 'utf-8)
                             (encode-coding-string "foλo" 'utf-8)))
              (with-temp-buffer
                (insert "foo")
                (write-region nil nil jpyfile)
                (should (buffer-modified-p))
                (should (equal (file-contents) "foo"))
                (insert "bar")
                (write-region nil "" jpyfile)
                (should (buffer-modified-p))
                (should (equal (file-contents) "foobar"))
                (should-error (write-region 1 nil jpyfile))
                (should-error (write-region (list 1) 1 jpyfile))
                (write-region 2 4 jpyfile)
                (should (buffer-modified-p))
                (should (equal (file-contents) "oo"))))
            (ert-info ("Base64 encode binary")
              (let ((coding-system-for-write 'binary))
                (write-region "\0\1\2\3\4\5\6" nil jpyfile)
                (should (equal (file-contents) "\0\1\2\3\4\5\6"))))
            (ert-info ("Append")
              (write-region "x" nil jpyfile)
              (should (equal (file-contents) "x"))
              (write-region "y" nil jpyfile t)
              (should (equal (file-contents) "xy"))
              (write-region "z" nil jpyfile t)
              (should (equal (file-contents) "xyz"))
              (write-region "a" nil jpyfile 1)
              (should (equal (file-contents) "xaz"))
              (write-region "β" nil jpyfile 6)
              (should (equal (encode-coding-string (file-contents) 'utf-8)
                             (encode-coding-string "xaz\0\0\0β" 'utf-8))))
            (ert-info ("File visiting")
              (with-temp-buffer
                (insert "foo")
                (write-region nil nil jpyfile nil t)
                (should-not (buffer-modified-p))
                (should (equal jpyfile (buffer-file-name)))
                (insert "bar")
                (write-region nil nil jpyfile nil "foo")
                (should-not (buffer-modified-p))
                (should (equal "foo" (file-name-nondirectory
                                      (buffer-file-name)))))))
        (delete-file file)))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; jupyter-tramp-test.el ends here
