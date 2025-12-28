;;; ent.el --- Emacs build tool -*- mode: emacs-lisp; lexical-binding: t -*-

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

(defgroup ent nil
  "Emacs build tool."
  :package-version "3.0"
  :group 'tools)

(defcustom ent-shell "bash"
  "Shell to run in shell commands."
  :type '(string)
  :group 'ent)

(defcustom ent-project-config-filename ".ent.el"
  "Project specific config file name."
  :type '(string)
  :group 'ent)

;; Global internal variables

(defvar ent--log-buffer "*ent-log*"
  "Ent log buffer.")

(defvar ent--tasks (make-hash-table :test #'equal)
  "Internal hash table of registered tasks.")

;; Global public variables

(defvar ent-project-home ""
  "Project home directory.")

(defvar ent-project-name ""
  "Project name, must be set in the project init file.")

;;; Logging -----------------------------------------------------------------

(defun ent-log (format-string &rest args)
  "Append a log message to `ent--log-buffer'.

FORMAT-STRING and ARGS follow the same convention as `format'.
The message is written as a single line terminated by a newline."
  (with-current-buffer (get-buffer-create ent--log-buffer)
    (goto-char (point-max))
    (insert (apply #'format (cons format-string args)) "\n")))

(defun ent--clear-log ()
  "Delete all contents of the log buffer."
  (with-current-buffer (get-buffer-create ent--log-buffer)
    (erase-buffer)))

(defun ent-time-iso-format ()
  "Get date time in ISO format."
  (format-time-string "%FT%T.%N"))

;;; Task registration --------------------------------------------------------

(cl-defstruct ent-task name doc deps shell elisp)

(defun ent--ensure-task-exists (name)
  "Ensure that a task named NAME is registered.
If not, signal an error."
  (unless (gethash name ent--tasks)
    (error "Task `%s' is not defined" name)))

(cl-defun task (name &optional &key doc deps elisp shell)
  "Create a ENT-TASK with NAME, optional DOC, DEPS, SHELL, and ELISP."
  (puthash name (make-ent-task :name  name
                                :doc   doc
                                :deps  deps
                                :shell shell
                                :elisp elisp)
           ent--tasks))

;;; Process helpers ---------------------------------------------------------

(defun ent-task-names ()
  "Return a list of all task names currently defined."
  (cl-loop for key being the hash-keys of ent--tasks
           collect key))

(defun ent--get-all-deps (all-deps task-name)
  "Return a list with all dependencies of the TASK-NAME.

ALL-DEPS is a list of task names, initial is empty."
  (if (member task-name all-deps)
      all-deps
    (let* ((task (gethash task-name ent--tasks))
           (deps (ent-task-deps task))
           (new-deps (if deps
                           (cl-reduce 'ent-get-all-deps (split-string deps "[[:space:]]+" t) :initial-value all-deps)
                         all-deps)))
       (push task-name new-deps))))

(defun ent-run-shell-command (command task-list)
  "Run COMMAND asynchronously.

TASK-LIST contains the name of the tasks not executed yet.
Write output/error to `ent-log-buffer'."
  (ent-log "[%s] Running command: %s" (ent-time-iso-format) command)
  (make-process
   :name (format "ent-shell-%s" (md5 command))
   :buffer ent--log-buffer
   :command (list ent-shell "-c" command)
   :sentinel (lambda (proc _status) (ent--sentinel command task-list proc))))

(defun ent--sentinel (command task-list proc)
  "After COMMAND exited, call next task from TASK-LIST if PROC exit was ok."
  (let ((exit-status (process-exit-status proc)))
    (ent-log "[%s] Command: %s exit with %d" (ent-time-iso-format) command exit-status)
    (ent--start-next-task (if (eql exit-status 0) task-list '()))))

;; --------------------------------------------------------------------------
;; Task execution
;; --------------------------------------------------------------------------

(defun ent--start-next-task (task-list)
  "Start the first task from the TASK-LIST."
  (if (null task-list)
      (ent-log "[%s] Done" (ent-time-iso-format))
    (let* ((task-name (pop task-list))
           (task (gethash task-name ent--tasks)))
      (cond
       ((ent-task-elisp task) (progn
                                (funcall (ent-task-elisp task))
                                (ent--start-next-task task-list)))
        ((ent-task-shell task) (ent-run-shell-command (ent-task-shell task) task-list))
        ('t (ent--start-next-task task-list))))))


(defun ent--find-project-file ()
  "Return the project file absolute path of the current project."
  (expand-file-name ent-project-config-filename
                    (locate-dominating-file default-directory ent-project-config-filename)))


(defun ent--init-run ()
  "Initialize the global variables for a new run."
  (setq ent--tasks (make-hash-table :test 'equal))
  (ent--clear-log)
  (switch-to-buffer ent--log-buffer))

;;;###autoload
(defun ent-run ()
  "Main command, run the selected task and all of its dependencies recursively.

When invoked interactively, `ent-run` first loads the project‑specific
config file (`ent-project-config-filename').  It then asks the user to pick
a task from the list of registered tasks, resolves that task’s
`:deps` field recursively, and finally executes the task itself."
  (interactive)
  (let* ((initfile (ent--find-project-file))
         (dir (locate-dominating-file default-directory ent-project-config-filename)))
    (ent--init-run)
    (shell-cd dir)
    (load initfile)
    (let* ((task-name (completing-read "Task: " (ent-task-names) nil t))
           (task-list (reverse (ent--get-all-deps '() task-name))))
      (ent--start-next-task task-list))))

(provide 'ent)
;;; ent.el ends here
