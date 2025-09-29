;;; .ent.el --- local ent config file -*- lexical-binding: t; -*-

;; project settings
(setq ent-project-home (file-name-directory (if load-file-name load-file-name buffer-file-name)))
(setq ent-project-name "ent")
(setq ent-clean-regexp "~$\\|\\.tex$")

;; local functions
;; (add-to-list 'load-path nil)

(load-file "ent.el")

;; tasks

(task :eldev-clean '(:clean) "clean tmp files" "eldev --color=never clean")

(task :lint '(:eldev-clean) "emacs lisp lint" "eldev --color=never lint")

(task :build '() "build package" "eldev --color=never build")

(task :test '() "run tests" "emacs -batch -l ert -l test-ent.el -f ert-run-tests-batch-and-exit")

(task :du '() "check async" "du -h /")

(task :exec-dirs '() "check execpath" (lambda (x) (insert (format "current-dir: %s\nexec-path: %s\n" x exec-path))))

(provide '.ent)
;;; .ent.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
