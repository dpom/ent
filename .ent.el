;;; .ent.el --- local ent config file -*- lexical-binding: t; -*-

;; project settings
(setq ent-project-home (file-name-directory (if load-file-name load-file-name buffer-file-name)))
(setq ent-project-name "ent")
(setq ent-clean-regexp "~$\\|\\.tex$")

;; local functions
;; (add-to-list 'load-path nil)

(load-file "ent.el")

;; tasks

(task :eldev-clean '(:clean) "clean tmp files" "eldev clean")

(task :lint '(:eldev-clean) "emacs lisp lint" "eldev lint")

(task :build '() "build package" "eldev build")

(task :test '() "run tests" "eldev test")


(provide '.ent)
;;; .ent.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
