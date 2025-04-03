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

;; Package-Requires: ((emacs "25.1"))

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

(defun ent-genautoload-init ()
  "Initialise the generate autoload task."
  (task 'genautoload () "generate project autoloads file" '(lambda (&optional x)
                                                           (ent-emacs "ent-genautoload-task" x))))

;; info


(defun ent-info-task ()
  "Copy all info files."
  (ent-walk ent-elisp-src-dir "\\.info$"
            '(lambda (x)
               (message "copy %s" x)
               (copy-file x info-dir t)
               (call-process "sudo" nil t t "ginstall-info"(concat "--infodir=" (ent-expand-dir-name info-dir)) x ))))

(defun ent-info-init ()
  "Initialize info task."
  (task 'info () "install info file" '(lambda (&optional x)
                                      (ent-emacs "ent-info-task" x))))
;; tangle

(defun ent-tangle-task ()
  "Tangle org file."
  (require 'org)(require 'ob-exp)(require 'ob)(require 'ob-tangle)
  (find-file (expand-file-name ent-project-orgfile ent-project-home))
  (org-babel-tangle)
  (kill-buffer))

(defun ent-tangle-init ()
  "Initialize tangle task."
  (task 'tangle '() "tangle config file" '(lambda (&optional x) (ent-emacs "ent-tangle-task" (expand-file-name ent-file-name ent-project-home)))))

;; initialization

(defun ent-elisp-tasks-init ()
  (ent-genautoload-init)
  (ent-info-init)
  (ent-tangle-init))


(provide 'ent-elisp-tasks)

;;; ent-elisp-tasks.el ends here
