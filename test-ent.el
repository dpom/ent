;;; test-ent.el --- Tests for the ent build‑tool -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)   ; for cl-loop, cl-remove-if-*
(require 'ert)

(defvar project-home )
(setq project-home (file-name-directory (if load-file-name load-file-name buffer-file-name)))

(load-file (expand-file-name "ent.el" project-home))


;;; Helpers ------------------------------------------------------------

(defun ent--make-test-tree (root)
  "Create a predictable directory structure under ROOT.
Return a list of all created paths (directories *and* files)."
  (let* ((dir-paths
          (list
           (expand-file-name "src" root)
           (expand-file-name "src/module" root)
           (expand-file-name "docs" root)))
         (file-paths
          (list
           (expand-file-name "foo.el" root)
           (expand-file-name "bar.txt" root)
           (expand-file-name "foo.el" (expand-file-name "src" root))
           (expand-file-name "baz.el" (expand-file-name "src/module" root))
           (expand-file-name "readme.org" (expand-file-name "docs" root)))))
    ;; Create directories first
    (dolist (d dir-paths)
      (make-directory d t))
    ;; Create files
    (dolist (f file-paths)
      (write-region "" nil f t))
    ;; Return the complete list (dirs + files)
    (append dir-paths file-paths)))


;;; ent-walk -----------------------------------------------------------

(ert-deftest ent-walk-files-test ()
  "Verify that `ent-walk` visits only files whose names match the regexp."
  (let* ((tmp (make-temp-file "ent-test-" t))
         (paths (ent--make-test-tree tmp))
         (seen  '())
         (files (cl-remove-if #'file-directory-p paths)))
    (unwind-protect
        (progn
          (dolist (p files)
            (ent-walk p "\\.el$" (lambda (f) (push f seen))))
          (should (equal (sort seen #'string<)
                         (sort (cl-remove-if-not
                                (lambda (p) (string-match-p "\\.el$" p))
                                files)
                               #'string<))))
      (delete-directory tmp t))))




;;; ent-dir-walk -------------------------------------------------------

(ert-deftest ent-dir-walk-test ()
  "Verify that `ent-dir-walk` visits only directories whose names match the regexp."
  (let* ((tmp (make-temp-file "ent-test-" t))
         (paths (ent--make-test-tree tmp))
         (seen  '())
         (dirs  (cl-remove-if-not #'file-directory-p paths)))
    (unwind-protect
        (progn
          (ent-dir-walk tmp "/src/?" (lambda (d) (push d seen)))
          (should (equal (sort seen #'string<)
                         (sort (cl-remove-if-not
                                (lambda (d) (string-match-p "/src/?" d))
                                dirs)
                               #'string<))))
      (delete-directory tmp t))))



;;; Integration: ent-clean-action ------------------------------------


(ert-deftest ent-clean-action-removes-target ()
  "Test that `ent-clean-action` deletes the expected files."
  (let* ((tmp  (make-temp-file "ent-clean-test-" t))
         (paths (ent--make-test-tree tmp))
         (to-remove (cl-remove-if-not (lambda (p) (string-match-p "\\.el$" p)) paths)))
    (unwind-protect
        (progn
          ;; Run the clean action
          (let ((removed (ent-clean-action tmp)))
            (should (= removed (length to-remove)))
            ;; Verify that the files are really gone
            (dolist (f to-remove)
              (should-not (file-exists-p f)))))
      (delete-directory tmp t))))



;;; Summary -------------------------------------------------------------

(provide 'test-ent)
;;; test-ent.el ends here
