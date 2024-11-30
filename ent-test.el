; -*- mode: emacs-lisp; lexical-binding: t -*-

(require 'ert)
(require 'dash)

(defvar project-home )
(setq project-home (file-name-directory (if load-file-name load-file-name buffer-file-name)))


(ert-deftest ent-test-find-project-file ()
  (load-file (expand-file-name "ent.el" project-home))
  (should (equal (expand-file-name ".ent.el" project-home) (ent-find-project-file))))

(ert-deftest ent-test-tasks ()
  (load (expand-file-name "test/ent-config1.el" project-home))
  (should (= 100 (length ent-tasks)))
  (should (equal [elispbuild task env init clean mcopy tangle genautoload help info dirclean] (seq-remove (lambda (x) (eq x 0)) ent-tasks))))

(provide 'ent-test)
;;; ent-test.el ends here
