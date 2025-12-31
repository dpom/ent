;;; .ent.el --- local ent config file -*- lexical-binding: t; -*-


;;; Commentary:
;; Test file

;;; Code:


(setq ent-project-home (file-name-directory (if load-file-name load-file-name buffer-file-name)))
(setq ent-project-name "ent-test")

(ent-load-default-tasks)

(task "test1"
      :doc "Test shell"
      :action "ls -laC /nix/store")

(task "test2"
      :doc "Test elisp"
      :action (lambda ()
               (let  ((cucu (completing-read "Cucu: " '("abc" "def") nil t)))
                 (message "Cucu este %s" cucu)
                 )))

(task "test3"
      :deps "test1 test2"
      :action "nix-collect-garbage")

(task "test4"
      :doc "Test shell"
      :action "ls -laC")

(task "test5"
      :doc "Test deps"
      :deps "test4 test3 test2")

(provide '.ent)
;;; .ent.el ends here
