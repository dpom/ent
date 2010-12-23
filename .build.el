(setq ent-project-home (file-name-directory (if load-file-name load-file-name buffer-file-name)))
(add-to-list 'load-path (concat ent-project-home "src/main/lisp/"))
(require 'ent)
(setq debug-on-error t)


;; project settings
(defvar generated-autoload-file)
(defvar deploy-dir)

(setq ent-project-name "ent")

(setq deploy-dir (expand-dir-name ent-project-name "~/.emacs.d/contrib/"))

(setq ent-mcopy-list (list
                      (list (expand-dir-name ent-elisp-default-src-dir ent-project-home)
                            deploy-dir
                            "\.elc?$")))

;in this project the specific config file is the build file
(setq ent-project-config-filename ".build.el") 
(setq ent-clean-regexp "~$\\|\.elc$\\|semantic\.cache")                    

 
(ent-init)

;; local functions

(defun generate-autoload ()
  (require 'autoload)
  (setq generated-autoload-file (expand-file-name (concat ent-project-name "-autoloads.el") (expand-dir-name ent-elisp-src-dir ent-project-home)))
  (with-temp-file generated-autoload-file 
    (insert (concat ";;; " ent-project-name "-autoloads.el --- autoloads for " ent-project-name "
;;
;;; Code:
")))
  (update-directory-autoloads ent-elisp-src-dir)
  (find-file generated-autoload-file)
  (goto-char (point-max))
  (insert ?\n)
  (insert (concat "(provide '" ent-project-name "-autoloads)
;;; " ent-project-name "-autoloads.el ends here
;;
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
"))
  (insert ?\n)
  (save-buffer 0)
  (kill-buffer (current-buffer)))

;; local tasks


(task 'genautoload () "generate project autoloads file" '(lambda (&optional x)
                                                           (ent-emacs "generate-autoload" x)))

(task 'install '(clean genautoload elispbuild mcopy) "install the package")