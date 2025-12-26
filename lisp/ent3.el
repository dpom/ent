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

(defcustom ent3-shell "bash"
  "Shell to run in shell commands."
  :type '(string)
  :group 'ent3)

(defcustom ent3-project-config-filename ".ent3.el"
  "Project specific config file name."
  :type '(string)
  :group 'ent3)

;; Global internal variables

(defvar ent3--lock-p nil
  "Lock the task.")
(defvar ent3--log-buffer "*ent3-log*"
  "Ent log buffer.")
(defvar ent3--shell-result nil
  "The result of the last shell command.")
(defvar ent3--tasks (make-hash-table :test #'equal)
  "Internal hash table of registered tasks.")

;; Global public variables

(defvar ent3-project-home ""
  "Project home directory.")
(defvar ent3-project-name ""
  "Project name, must be set in the project init file.")

;;; Logging -----------------------------------------------------------------

(defun ent3-log (format-string &rest args)
  "Append a log message to `ent3--log-buffer'.

FORMAT-STRING and ARGS follow the same convention as `format'.
The message is written as a single line terminated by a newline."
  (with-current-buffer (get-buffer-create ent3--log-buffer)
    (goto-char (point-max))
    (insert (apply #'format (cons format-string args)) "\n")))

(defun ent3--clear-log ()
  "Delete all contents of the log buffer."
  (with-current-buffer (get-buffer-create ent3--log-buffer)
    (erase-buffer)))

(defun ent3-time-iso-format ()
  "Get date time in ISO format."
  (format-time-string "%FT%T.%N"))

;;; Task registration --------------------------------------------------------

(cl-defstruct ent3-task name doc deps shell elisp)

(defun ent3--ensure-task-exists (name)
  "Ensure that a task named NAME is registered.
If not, signal an error."
  (unless (gethash name ent3--tasks)
    (error "Task `%s' is not defined" name)))

(cl-defun task (name &optional &key doc deps elisp shell)
  "Create a ENT3-TASK with NAME, optional DOC, DEPS, SHELL, and ELISP."
  (puthash name (make-ent3-task :name  name
                                :doc   doc
                                :deps  deps
                                :shell shell
                                :elisp elisp)
           ent3--tasks))

;;; Process helpers ---------------------------------------------------------

(defun ent3-task-names ()
  "Return a list of all task names currently defined."
  (cl-loop for key being the hash-keys of ent3--tasks
           collect key))

(defun ent3--run-task-recursively (name &optional visited)
  "Run the task NAME, running all of its dependencies first.
VISITED is a list of task names already executed in the current
recursive chain; it is used to detect and abort circular
dependencies.
\(fn NAME &optional VISITED)"
  (unless (member name visited)
    (let* ((task (gethash name ent3--tasks)))
      (unless task
        (error "Task %S not defined" name))

      ;; 1. Run dependencies first
      (when (ent3-task-deps task)
        (dolist (dep (split-string (ent3-task-deps task)))
          (ent3--run-task-recursively dep (cons name visited))))

      ;; 2. Run the task itself
      (ent3-log "[%s] Start task: %s\n"
                (ent3-time-iso-format) name)
      (cond ((ent3-task-elisp task)
             (funcall (ent3-task-elisp task))
             (ent3-log "[%s] End task: %s\n"
                       (ent3-time-iso-format) name))
            ((ent3-task-shell task)
             (ent3-run-shell-command (ent3-task-shell task)))))))


(defun ent3-run-shell-command (command)
  "Run COMMAND asynchronously.

Write output/error to `ent3-log-buffer' and store exit code
in `ent3-shell-result'."
  (ent3-log "[%s] Running command: %s\n" (ent3-time-iso-format) command)
  (setq ent3--shell-result nil)
  ;; Create a process to run the shell command
  (make-process :name "ent3-shell"
                :buffer ent3--log-buffer
                :command (list ent3-shell "-c" command)
                :sentinel (lambda (proc _status)
                                 ;; Store the exit code for later inspection.
                            (setq ent3--shell-result (process-exit-status proc))
                            (ent3-log "Exit code: %s\n" ent3--shell-result)
                            (ent3-log "[%s] End task" (ent3-time-iso-format)))))


(defun ent3-run-task (name)
  "Run ent3-task with the NAME."
  (let ((task (gethash name ent3--tasks)))
    (unless task (error "Task %S not defined" name))
    (ent3-log "[%s] Start task: %s\n" (ent3-time-iso-format) name)
    (cond ((ent3-task-elisp task)
           (progn (funcall (ent3-task-elisp task))
                  (ent3-log "[%s] End task: %s\n" (ent3-time-iso-format) name)))
          ((ent3-task-shell task)
           (ent3-run-shell-command (ent3-task-shell task))))))

(defun ent3--find-project-file ()
  "Return the project file absolute path of the current project."
  (expand-file-name ent3-project-config-filename
                    (locate-dominating-file default-directory ent3-project-config-filename)))

(defun ent3--init-run ()
  "Initialize the global variables for a new run."
  (setq ent3--tasks (make-hash-table :test 'equal))
  (setq ent3--shell-result 0)
  (ent3--clear-log)
  (switch-to-buffer ent3--log-buffer))

;;;###autoload
;; (defun ent3-run ()
;;   "Main entry point for ent."
;;   (interactive)
;;   (let ((initfile (ent3--find-project-file))
;;         (dir (locate-dominating-file default-directory ent3-project-config-filename)))
;;     (ent3--init-run)
;;     (shell-cd dir)
;;     (load initfile)
;;     (let ((taskname (completing-read  "Command: " (hash-table-keys ent3--tasks) nil t)))
;;       (ent3-run-task taskname)
;;       (ansi-color-apply-on-region (point-min) (point-max)))))

(defun ent3-run ()
  "Run the selected task and all of its dependencies recursively.

When invoked interactively, `ent3-run` first loads the project‑specific
init file (via `ent3-init`).  It then asks the user to pick a task
from the list of registered tasks, resolves that task’s
`:deps` field recursively, and finally executes the task itself."
  (interactive)
  (let* ((initfile (ent3--find-project-file))
        (dir (locate-dominating-file default-directory ent3-project-config-filename)))
    (ent3--init-run)
    (shell-cd dir)
    (load initfile)
    (let* ((task-name (completing-read "Task: " (ent3-task-names))))
      (ent3--run-task-recursively task-name))))


(provide 'ent3)
;;; ent3.el ends here
