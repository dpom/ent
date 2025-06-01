;;; .ent.el --- local ent config file -*- lexical-binding: t; -*-

;; project settings
(setq ent-project-home (file-name-directory (if load-file-name load-file-name buffer-file-name)))
(setq ent-project-name "ent-test")
(setq ent-clean-regexp "~$\\|\\.tex$")

;; local functions
;; (add-to-list 'load-path nil)

(load-file "../ent.el")

(load-file "../ent-elisp-tasks.el")

(ent-add-elisp-tasks)

;; tasks

(task :init  '() "initialize test" "touch test1.tex; touch test2~")

(task :list '() "list current directory" "ls -la" )

(task :test '(:env :init :list :clean :list :cleadir) "test tasks" (lambda (x) (insert (format "Test the ent project"))))

(provide '.ent)
;;; .ent.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
