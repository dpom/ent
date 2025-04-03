;;; ent-test.el --- Project tests -*- mode: emacs-lisp; lexical-binding: t -*-

;; Package-Requires: ((emacs "25.1"))

;;; Code:

(require 'ert)
(require 'dash)

(defvar project-home )
(setq project-home (file-name-directory (if load-file-name load-file-name buffer-file-name)))


(ert-deftest ent-test-find-project-file ()
  (load-file (expand-file-name "ent.el" project-home))
  (should (equal (expand-file-name ".ent.el" project-home) (ent-find-project-file))))

(provide 'ent-test)
;;; ent-test.el ends here
