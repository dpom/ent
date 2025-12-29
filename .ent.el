;;; .ent.el --- local ent config file -*- lexical-binding: t; -*-

;; project settings
(setq ent-project-home (file-name-directory (if load-file-name load-file-name buffer-file-name)))
(setq ent-project-name "ent")

;; tasks


(task "lint"
      :doc "Lint the source code"
      :action "nix develop .github# && \"emacs\" --quick --script .github/run-shim.el -- lint")





(provide '.ent)
;;; .ent.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
