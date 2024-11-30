;;; .ent.el --- local ent config file -*- lexical-binding: t; -*-

;; project settings
(setq ent-project-home (file-name-directory (if load-file-name load-file-name buffer-file-name)))
(setq ent-project-name "ent-test")
(setq ent-clean-regexp "~$\\|\\.tex$")

;; local functions
;; (add-to-list 'load-path nil)

(load-file "../ent.el")

(load-file "../ent-elisp-tasks.el")

(ent-tasks-init)
(ent-elisp-tasks-init)

;; tasks


(task 'init  '() "initialize test" '(lambda (&optional x) (concat  "mkdir tmp"
                                                                   "; touch test1.tex"
                                                                   "; touch test2~")))


(task 'task '(init) "test task function" '(lambda (&optional x) "ls -la" ))

(provide '.ent)
;;; .ent.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
