;;; .ent.el --- local ent config file -*- lexical-binding: t; -*-

;; project settings
(setq ent-project-home (file-name-directory (if load-file-name load-file-name buffer-file-name)))
(setq ent-project-name "ent")

(ent-load-default-tasks)


;; tasks

(task "lint"
      :doc "Lint the source code"
      :action (concat
               "cd .github"
               " && direnv allow"
               " && emacs --script ./run-shim.el -- lint"))

(task "test"
      :doc "Test the source code"
      :action (concat
               "cd .github"
               " && direnv allow"
               " && emacs --script ./run-shim.el -- test"))

(provide '.ent)
;;; .ent.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
