;;; ent-elisp-tasks.el --- useful elisp dev tasks -*- lexical-binding: t -*-

;; Copyright (C) 2019, 2025  Dan Pomohaci

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;; Package-Requires: ((emacs "25.1") cl-lib)

;;; Commentary:

;;; Code:

(require 'ent)

(defvar ent-project-orgfile ""
  "Project org file.")

;; genautoload

(defun ent-genautoload-task ()
  "Generate autoload file for the current project."
  (require 'autoload)
  (setq generated-autoload-file (ent-expand-file-name (concat ent-project-name "-autoloads.el") (expand-dir-name ent-elisp-src-dir ent-project-home)))
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


;; info

(defun ent-info-task (&optional arg)
  "Copy all info files (ARG is only for compatibility)."
  (ent-walk ent-elisp-src-dir "\\.info$"
            '(lambda (x)
               (message "copy %s" x)
               (copy-file x info-dir t)
               (call-process "sudo" nil t t "ginstall-info"(concat "--infodir=" (ent-expand-dir-name info-dir)) x ))))

;; tangle

(defun ent-tangle-task (&optional arg)
  "Tangle org file (ARG is only for compatibility)."
  (require 'org)(require 'ob-exp)(require 'ob)(require 'ob-tangle)
  (find-file (expand-file-name ent-project-orgfile ent-project-home))
  (org-babel-tangle)
  (kill-buffer))


;; initialization

(defun ent-add-elisp-tasks ()
  "Add elisp tasks (ARG is only for compatibility)."
  (task :genautoload '() "Generate project autoloads file" 'ent-genautoload-task)
  (task :info '() "Install info file"  'ent-info-task)
  (task :tangle '() "Tangle config file" 'ent-tangle-task))



(provide 'ent-elisp-tasks)

;;; ent-elisp-tasks.el ends here
