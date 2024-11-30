;;; ent-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ent" "ent.el" (0 0 0 0))
;;; Generated autoloads from ent.el

(autoload 'ent "ent" "\
Main entry point for ent.
You could specify the TASKNAME.

\(fn &optional TASKNAME)" t nil)

(autoload 'ent-visit-build-file "ent" "\
Visit the ent project file." t nil)

(autoload 'ent-visit-config-file "ent" "\
Visit the project specific config file, ent-project-config-filename." t nil)

(autoload 'ent-find-file "ent" "\
Find a file from the project." t nil)

(defvar ent-prefix-map (let ((map (make-sparse-keymap))) (define-key map "x" 'ent) (define-key map "c" 'ent-visit-config-file) (define-key map "b" 'ent-visit-build-file) (define-key map "f" 'ent-find-file) map))

(fset 'ent-prefix-map ent-prefix-map)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ent" '("ent-" "task")))

;;;***

;;;### (autoloads nil "ent-lein-tasks" "ent-lein-tasks.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from ent-lein-tasks.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ent-lein-tasks" '("ent-lein-" "make-image-tag")))

;;;***

;;;### (autoloads nil nil ("ent-elisp-tasks.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ent-autoloads.el ends here
