;;; .ent.el --- local ent config file -*- lexical-binding: t; -*-

;; project settings
(setq ent-project-home (file-name-directory (if load-file-name load-file-name buffer-file-name)))
(setq ent-project-name "ent")
(setq ent-clean-regexp "~$\\|\\.tex$")

;; local functions
;; (add-to-list 'load-path nil)

(load-file "ent.el")

(load-file "ent-elisp-tasks.el")

(ent-tasks-init)
(ent-elisp-tasks-init)

;; tasks

(task 'clean '() "clean tmp files" '(lambda (&optional x) "eldev clean" ))

(task 'lint '(clean) "emacs lisp lint" '(lambda (&optional x) "eldev lint"))

(task 'build '() "build package" '(lambda (&optional x) "eldev build"))

(task 'test '() "run tests" '(lambda (&optional x) "eldev test"))


(provide '.ent)
;;; .ent.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
