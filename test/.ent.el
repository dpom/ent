;;; .ent.el --- local ent config file -*- lexical-binding: t; -*-


;;; Commentary:
;; Test file

;;; Code:

(setq ent2-project-home (file-name-directory (if load-file-name load-file-name buffer-file-name)))
(setq ent2-project-name "ent-test")
(setq ent2-clean-regexp "~$\\|\\.tex$")

(load-file "../lisp/ent2.el")

(task "test1"
      :doc "Test task 1"
      :shell "ls -la")

(ent2-log "ent2-tasks: %s\n" ent2-tasks)

(provide '.ent)
;;; .ent.el ends here
