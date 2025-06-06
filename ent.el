;;; ent.el --- Emacs build tool -*- mode: emacs-lisp; lexical-binding: t -*-

;; Copyright (C) 2019, 2025  Dan Pomohaci

;; Author: Dan Pomohaci
;; Version: 2.2
;; Homepage: https://gitlab.com/dpom/ent
;; Keywords: elisp tools maint
;; Package-Requires: ((emacs "26.1") cl-lib  (seq))

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; a task tool similar to ant but with elisp syntax

;;; Code:

(require 'seq)
(require 'dired)
(require 'bytecomp)
(require 'cl-lib)
(require 'ansi-color)
(require 'shell)

;; Custom variables

(defgroup ent nil
  "Emacs build tool."
  :version "2.0"
  :group 'tools
  :group '
  processes)

(defcustom ent-emacs-exec "emacs"
  "Emacs executable."
  :type '(string)
  :group 'ent)

(defcustom ent-default-task :clean
  "Default task."
  :type '(symbol)
  :group 'ent)

(defcustom ent-dirclean-default-regexp "target"
  "Default regular expression to match garbage directories."
  :type '(string)
  :group 'ent)

(defcustom ent-clean-default-regexp "~$"
  "Default regular expression to match garbage files."
  :type '(string)
  :group 'ent)

(defcustom ent-elisp-default-src-dir "./"
  "Default Lisp source directory."
  :type '(string)
  :group 'ent)

(defcustom ent-project-config-filename ".ent.el"
  "Project specific config file name."
  :type '(string)
  :group 'ent)


(defcustom ent-default-exclude-dir-list (list "*.git" "*target")
  "The list of directories to be excluded in file search."
  :type '(list string)
  :group 'ent)

;; Global variables

(defvar ent-project-name ""
  "Project name, must be set in the project init file.")

(defvar ent-project-home ""
  "Project home directory.")

(defvar ent-tasks ()
  "Local tasks list.")

(defvar ent-clean-regexp nil
  "Regular expression to match garbage files.")

(defvar ent-dirclean-regexp nil
  "Regular expression to match garbage directories.")

(defvar ent-elisp-src-dir nil
  "Lisp source directory.")

(defvar ent-mcopy-list ()
  "Multiple copy list each element is \(SRC DEST PATTERN\).
The SRC and DEST must be absolute path.")

(defvar ent-generated-autoload-file "")

(defvar ent-exclude-dir-list ()
  "Directories to be excluded from file search.")

;; Utility functions

(defun ent-expand-dir-name (dir &optional path)
  "Expand a local DIR name using his PATH."
  (file-name-as-directory (expand-file-name dir path)))

(defun ent-walk (dir regexp function)
  "Walk DIR recursively and execute FUNCTION for REGEXP match files."
  (cond
   ((file-regular-p dir) (and (string-match regexp dir)
                              (funcall function dir)))
   ((file-directory-p dir) (mapc #'(lambda (x)
                                     (ent-walk x regexp function))
                                 (directory-files (expand-file-name dir)
                                                  t "[^.]$")))))

(defun ent-dir-walk (dir regexp function)
  "Walk DIR recursively and execute FUNCTION for REGEXP match directories."
  (if (file-directory-p dir)
      (if (string-match regexp dir) (funcall function dir)
        (mapc #'(lambda (x) (ent-dir-walk x regexp function))
              (directory-files (expand-file-name dir) t "[^.]$")))))

(defun ent-rcopy (src dest regexp)
  "Recursive copy REGEXP files from SRC to DEST."
  (ent-walk src regexp #'(lambda (x)
                           (message "copy %s to %s" x dest)
                           (make-directory dest t)
                           (copy-file x dest t))))

(defun ent-mcopy ()
  "Multiple recursive copy using ent-mcopy-list."
  (mapc #'(lambda (y)
            (message "%s" y)
            (apply 'ent-rcopy y))
        ent-mcopy-list))


(cl-defstruct task name doc deps action)

(defun ent-plist-keys (plist)
  "Get just the keys of a PLIST."
  (cl-loop for key in plist by #'cddr collecting key))

(defun ent-plist-values (plist)
  "Get just the values of a PLIST."
  (cl-loop for (_prop val) on plist by #'cddr collecting val))

(defun ent-symbol-to-string (symbol)
  "Convert a SYMBOL in string."
  (format "%s" symbol))

(defun ent-time-iso-format ()
  "Get date time in ISO format."
  (format-time-string "%FT%T.%N"))

(defun task (name deps doc &optional action)
  "Create a task and add it to the tasks."
  (let ((tsk (make-task :name name
                        :doc doc
                        :deps (mapcar 'ent-symbol-to-string deps)
                        :action action)))
    (setq ent-tasks (plist-put ent-tasks (ent-symbol-to-string name) tsk))))

(defun ent-find-project-file ()
  "Return the project file absolute path of the current project."
  (expand-file-name ent-project-config-filename (locate-dominating-file default-directory ent-project-config-filename)))

(defun ent-run-task (tsk tasks dir out-buffer)
  "Run a TSK from TASKS in DIR, results displayed in OUT-BUFFER."
  (insert (format "Start %s (%s)\n" (task-name tsk) (ent-time-iso-format)))
  (when (task-deps tsk)
    (dolist (dt (task-deps tsk))
      (funcall #'ent-run-task (plist-get tasks (ent-symbol-to-string dt)) tasks dir out-buffer)))
  (when-let* ((action (task-action tsk)))
    (if (functionp action)
        (funcall action dir)
      (if (stringp action)
          (progn
            (shell-cd dir)
            (start-process-shell-command "ent" out-buffer action))
        (insert "no action\n"))))
  (insert (format "End %s (%s)\n\n" (task-name tsk) (ent-time-iso-format))))


;;;; Global tasks

;; clean
(defun ent-clean-action (dir)
  "Remove all files matching ent-clean-regexp from DIR recursively."
  (let ((acc 0)
        (regexp (or ent-clean-regexp ent-clean-default-regexp)))
    (displaying-byte-compile-warnings
     (insert (format "Clean: %s from %s\n" regexp dir))
     (ent-walk dir regexp #'(lambda (x)
                              (delete-file (expand-file-name x))
                              (setq acc (+ acc 1))
                              (insert (format "clean: %s deleted\n" (expand-file-name x)))))
     (insert (format "Clean: command terminated %d files removed\n" acc))
     acc)))


;; dirclean
(defun ent-dirclean-action (dir)
  "Remove all directories ent-dirclean-regexp from DIR recursively."
  (let ((acc 0)
        (regexp (or ent-dirclean-regexp ent-dirclean-default-regexp)))
    (displaying-byte-compile-warnings
     (insert (format "DirClean: %s from %s\n" regexp dir))
     (ent-dir-walk dir regexp #'(lambda (x)
                                  (dired-delete-file (expand-file-name x) 'always)
                                  (setq acc (+ acc 1))
                                  (insert (format  "%s deleted\n" (expand-file-name x)))))
     (insert (format  "DirClean: %d directories removed\n" acc))
     acc)))

;; help
(defun ent-help-action (&optional _arg)
  "Display tasks description (ARG is only for compatibility)."
  (displaying-byte-compile-warnings
   (dolist (task (ent-plist-values ent-tasks))
     (insert (format  "%-10s\t- %s\n" (task-name task) (task-doc task))))))

;; env
(defun ent-env-action (&optional arg)
  "Display environment variables (ARG is only for compatibility)."
  (displaying-byte-compile-warnings
   (insert (format  "\nEnv:\n"))
   (insert (format  "- load-path: %s\n" load-path))
   (insert (format  "- arg: %s\n" arg))
   (insert (format  "- project name: %s\n" ent-project-name))
   (insert (format  "- project dir: %s\n" ent-project-home))
   (insert (format  "- project tasks: %s\n" (ent-plist-keys ent-tasks)))))

;; elispbuild
(defun ent-elispbuild-action (dir)
  "Build an elisp project from DIR (ARG is only for compatibility)."
  (displaying-byte-compile-warnings
   (insert (format  "Project %s\n" ent-project-name))
   (insert (format  "Build: compile el files found in %s\n" dir))
   (byte-recompile-directory dir 0)))

;; mcopy
(defun ent-mcopy-action  (&optional _arg)
  "Multiple copy using ent-mcopy-list recursively."
  (displaying-byte-compile-warnings
   (ent-mcopy)))

;;;; Commands

(defun ent-add-default-tasks ()
  "Add default tasks."
  (task :clean ()  (documentation 'ent-clean-action) 'ent-clean-action)
  (task :dirclean ()  (documentation 'ent-dirclean-action) 'ent-dirclean-action)
  (task :help ()  (documentation 'ent-help-action) 'ent-help-action)
  (task :env ()  (documentation 'ent-env-action) 'ent-env-action)
  (task :elispbuild () (documentation 'ent-elispbuild-action) 'ent-elispbuild-action)
  (task :mcopy ()  (documentation 'ent-mcopy-action) 'ent-mcopy-action))


;;;###autoload
(defun ent-run (&optional taskname)
  "Main entry point for ent.
You could specify the TASKNAME."
  (interactive)
  (let ((initfile (ent-find-project-file))
        (dir (locate-dominating-file default-directory ent-project-config-filename))
        (out-buffer (generate-new-buffer "*ent-out*" )))
    (setq ent-tasks ())
    (ent-add-default-tasks)
    (switch-to-buffer out-buffer)
    (compilation-mode)
    (let ((inhibit-read-only t))
      (shell-cd dir)
      (load initfile)
      (if (not taskname)
          (setq taskname (ido-completing-read  "Command: "
                                               (ent-plist-keys ent-tasks)
                                               nil t)))
      (ent-run-task (plist-get ent-tasks taskname) ent-tasks dir out-buffer)
      (ansi-color-apply-on-region (point-min) (point-max)))))

;;;###autoload
(defun ent-visit-build-file ()
  "Visit the ent project file."
  (interactive)
  (find-file-other-window (ent-find-project-file)))

(defmacro ent-in-project (&rest body)
  "Execute BODY for the curent project."
  `(if (file-exists-p (ent-find-project-file))
       (progn
         (load (ent-find-project-file))
         ,@body)
     (message "Not in a project!")))

;;;###autoload
(defun ent-visit-config-file ()
  "Visit the project specific config file, ent-project-config-filename."
  (interactive)
  (ent-in-project
   (find-file-other-window (expand-file-name ent-project-config-filename ent-project-home))))

;;;###autoload
(defun ent-find-file ()
  "Find a file from the project."
  (interactive)
  (ent-in-project
   (let (project-files tbl)
     (setq project-files
           (split-string
            (shell-command-to-string
             (concat "find "
                     ent-project-home
                     " \\( "
                     (mapconcat (function (lambda (x) (concat "-name \"" x "\""))) (or ent-exclude-dir-list ent-default-exclude-dir-list) " -o  ")
                     " \\) -prune -o -type f -print | grep -v \"" (or ent-clean-regexp ent-clean-default-regexp) "\""))
            "\n"))
     ;; populate hash table (display repr => path)
     (setq tbl (make-hash-table :test 'equal))
     (let (ido-list)
       (mapc (lambda (path)
               (let (key)
                 ;; format path for display in ido list
                 (setq key (replace-regexp-in-string "\\(.*?\\)\\([^/]+?\\)$" "\\2|\\1" path))
                 ;; strip project root
                 (setq key (replace-regexp-in-string ent-project-home "" key))
                 ;; remove trailing | or /
                 (setq key (replace-regexp-in-string "\\(|\\|/\\)$" "" key))
                 (puthash key path tbl)
                 (push key ido-list)))
             project-files)
       (find-file (gethash (ido-completing-read "project-files: " ido-list) tbl))))))

;; Key map

;;;###autoload
(defvar ent-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "x" 'ent-run)
    (define-key map "c" 'ent-visit-config-file)
    (define-key map "b" 'ent-visit-build-file)
    (define-key map "f" 'ent-find-file)
    map))

;;;###autoload
(fset 'ent-prefix-map ent-prefix-map)

(provide 'ent)

;;; ent.el ends here
