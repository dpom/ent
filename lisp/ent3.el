;;; ent3.el --- Emacs build tool -*- mode: emacs-lisp; lexical-binding: t -*-

;; Copyright (C) 2025  Dan Pomohaci

;; Author: Dan Pomohaci
;; Version: 2.2
;; Homepage: https://gitlab.com/dpom/ent
;; Keywords: elisp tools project
;; Package-Requires: ((emacs "28.1") cl-lib f (seq))

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

;; A task tool similar to ant but with elisp syntax

;; This is created using `elisp-repo-kit'.  Reload the commands or
;; run the tests with `erk-reload-project-package' and
;; `erk-ert-project'.  Have fun!

;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'shell)
(require 'subr-x)


;; Custom variables

(defgroup ent3 nil
  "Emacs build tool."
  :package-version "3.0"
  :group 'tools)

(defcustom ent3-emacs-exec "emacs"
  "Emacs executable."
  :type '(string)
  :group 'ent3)

(defcustom ent3-default-task :clean
  "Default task."
  :type '(symbol)
  :group 'ent3)

(defcustom ent3-dirclean-default-regexp "target"
  "Default regular expression to match garbage directories."
  :type '(string)
  :group 'ent3)

(defcustom ent3-clean-default-regexp "~$"
  "Default regular expression to match garbage files."
  :type '(string)
  :group 'ent3)

(defcustom ent3-elisp-default-src-dir "./"
  "Default Lisp source directory."
  :type '(string)
  :group 'ent3)

(defcustom ent3-project-config-filename ".ent.el"
  "Project specific config file name."
  :type '(string)
  :group 'ent3)


(defcustom ent3-default-exclude-dir-list (list "*.git" "*target")
  "The list of directories to be excluded in file search."
  :type '(list string)
  :group 'ent3)

;; Global variables

(defvar ent3-source-dir (expand-file-name "src")
  "Root of the source tree.")
(defvar ent3-dist-dir (expand-file-name "dist")
  "Root of the distribution tree.")
(defvar ent3-log-buffer "*ent3-log*"
  "Ent log buffer.")
(defvar ent3-shell-result nil
  "The result of the last shell command.")
(defvar ent3-lock-p nil
  "Lock the task.")
(defvar ent3-project-name ""
  "Project name, must be set in the project init file.")
(defvar ent3-project-home ""
  "Project home directory.")
(defvar ent3-tasks (make-hash-table :test 'equal)
  "Local tasks list.")
(defvar ent3-clean-regexp nil
  "Regular expression to match garbage files.")
(defvar ent-dirclean-regexp nil
  "Regular expression to match garbage directories.")


(defun ent3-log (fmt &rest args)
  "Append a formatted (FMT) line with ARGS to `ent3-log-buffer`."
  (with-current-buffer (get-buffer-create ent3-log-buffer)
    (goto-char (point-max))
    (insert (apply #'format (cons fmt args)) "\n")))

(defun ent3-time-iso-format ()
  "Get date time in ISO format."
  (format-time-string "%FT%T.%N"))

(defun ent3-walk (dir regex action)
  "Traverse DIR, applying ACTION to each file that matches REGEX."
  (cl-loop for sub in (directory-files dir t nil t)
           if (and (eq (file-attribute-type sub) 'file)
                   (string-match-p regex (file-name-base sub)))
           do (funcall action sub)))

(defun ent3-files (regex)
  "Return a list of files in `ent3-source-dir` that match REGEX."
  (cl-remove-if-not
   (lambda (f) (string-match-p regex (file-name-base f)))
   (directory-files ent3-source-dir t nil t)))

(cl-defstruct ent3-task
  name
  doc
  deps
  shell
  elisp)


(cl-defun task (name &optional &key doc deps elisp shell)
  "Create a ENT3-TASK with NAME, optional DOC, DEPS, SHELL, and ELISP."
  (puthash name
           (make-ent3-task :name  name
                           :doc   doc
                           :deps  deps
                           :shell shell
                           :elisp elisp)
           ent3-tasks))


(defun ent3-run-shell-command (command)
  "Run COMMAND asynchronously.

Write output/error to `ent3-log-buffer' and store exit code
in `ent3-shell-result'."
  (ent3-log "[%s] Running command: %s\n" (ent3-time-iso-format) command)
  (setq ent3-shell-result nil)

  ;; Create a process to run the shell command
  (make-process
   :name "ent3-shell"
   :buffer ent3-log-buffer
   :command (split-string command)
   :sentinel (lambda (proc status)
               (ent3-log "Exit code: %s\n" status)
               (ent3-log "Proc: %s\n" proc)
               (ent3-log "[%s] End task" (ent3-time-iso-format))
               (setq ent3-shell-result status))))


(defun ent3-run-task (name)
  "Run ent3-task with the NAME."
  (let ((task (gethash name ent3-tasks)))
    (unless task (error "Task %S not defined" name))
    (ent3-log "[%s] Start task: %s\n" (ent3-time-iso-format) name)
    (ent3-run-deps task '())
    (cond
     ((ent3-task-elisp task) (funcall (ent3-task-elisp task)))
     ((ent3-task-shell task) (ent3-run-shell-command (ent3-task-shell task))))))

(defun ent3-run-deps (task visited)
  "Run recursively all TASK dependencies.  VISITED contain the tasks already run."
  (let ((n (ent3-task-name task)))
    (when (member n visited)
      (error "Circular dependency at %S" n))
    (dolist (dep (ent3-task-deps task))
      (ent3-run-deps (gethash dep ent3-tasks) (cons n visited)))))

;; Example tasks
(task "clean"
  :doc "Clean the"
  :elisp (lambda ()
          (ent3-walk ent3-dist-dir ".*~" #'delete-file)
          (ent3-log "Cleaned distribution directory")))

(task "build"
      :doc ""
      :deps '("clean")
      :elisp (lambda ()
              (ent3-walk ent3-source-dir ".*\\.el$"
                         (lambda (src)
                           (let ((dst (concat ent3-dist-dir "/" (file-name-base src) ".el")))
                             (copy-file src dst t)
                             (ent3-log "Built %s" dst))))))

(defun ent3-find-project-file ()
  "Return the project file absolute path of the current project."
  (expand-file-name ent3-project-config-filename (locate-dominating-file default-directory ent3-project-config-filename)))

(defun ent3-init-run ()
  "Initialize the global variables for anew run."
  (setq ent3-tasks (make-hash-table :test 'equal))
  (switch-to-buffer ent3-log-buffer)
  (erase-buffer)
  )

;;;###autoload
(defun ent3-run (&optional taskname)
  "Main entry point for ent.
You could specify the TASKNAME."
  (interactive)
  (let ((initfile (ent3-find-project-file))
        (dir (locate-dominating-file default-directory ent3-project-config-filename)))
    (ent3-init-run)
    (compilation-mode)
    (let ((inhibit-read-only t))
      (shell-cd dir)
      (load initfile)
      (if (not taskname)
          (setq taskname (ido-completing-read  "Command: "
                                               (hash-table-keys ent3-tasks)
                                               nil t)))
      (ent3-run-task taskname)
      (ansi-color-apply-on-region (point-min) (point-max)))))


(provide 'ent3)
;;; ent3.el ends here
