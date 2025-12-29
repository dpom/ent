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

(defconst ent--log-buffer "*ent-log*"
  "Ent log buffer.")

(defvar ent--log-proc nil
  "Log dummy process.")

(defvar ent-tasks (make-hash-table :test #'equal)
  "Internal hash table of registered tasks.")


;; Global public variables

(defvar ent-project-home ""
  "Project home directory.")

(defvar ent-project-name ""
  "Project name, must be set in the project init file.")


;;; Logging -----------------------------------------------------------------

(defun ent--create-log-buffer ()
  "Create a comint-like buffer with no interactive input."
  (let ((buffer (get-buffer-create ent--log-buffer)))
    (switch-to-buffer buffer)
    (erase-buffer)
    (unless (derived-mode-p 'shell-mode)
      (shell-mode))
    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
    (setq comint-prompt-regexp "^OUT> ")
    (setq comint-use-prompt-regexp t)
    ;; Disable interactive input
    (setq comint-input-sender
          (lambda (proc cmd)
            (comint-output-filter  proc (concat "INPUT NOT SENT: " cmd "\n"))))
    ;;  Start a dummy process
    (setq ent--log-proc (start-process "ent" buffer "sh"))))


(defun ent-time-iso-format ()
  "Get date time in ISO format."
  (format-time-string "%FT%T.%N"))



(defun ent-log* (format-string &rest args)
  "Append a log message to `ent--log-buffer'.

FORMAT-STRING and ARGS follow the same convention as `format'.
The message is written as a single line terminated by a newline."
  (with-current-buffer ent--log-buffer
    (comint-goto-process-mark)
    (insert (apply #'format (cons format-string args)) "\n")
    (comint-set-process-mark)))


(defun ent-log (format-string &rest args)
  "Append a timed prefixed log message to `ent--log-buffer'.

FORMAT-STRING and ARGS follow the same convention as `format'.
The message is written as a single line terminated by a newline."
(apply #'ent-log* (cons (concat "\n[%s] " format-string) (push (ent-time-iso-format) args))))

(defun ent--clear-log ()
  "Delete all contents of the log buffer."
  (with-current-buffer ent--log-buffer
    (erase-buffer)))


(defun ent--colorize-log ()
  "Delete all contents of the log buffer."
  (with-current-buffer ent--log-buffer
    ;; (kill-process ent--log-proc)
    (let ((inhibit-read-only t)
          (modified (buffer-modified-p)))
      (ansi-color-apply-on-region (point-min) (point-max) t)
      (set-buffer-modified-p modified))))


;;; Task registration --------------------------------------------------------

(cl-defstruct ent-task name doc deps action)


(defun ent--ensure-task-exists (name)
  "Ensure that a task named NAME is registered.
If not, signal an error."
  (unless (gethash name ent-tasks)
    (error "Task `%s' is not defined" name)))


(cl-defun task (name &optional &key doc deps action)
  "Create a ENT-TASK with NAME, optional DOC, DEPS, and ACTION."
  (puthash name (make-ent-task :name  name
                                :doc   doc
                                :deps  deps
                                :action action)
           ent-tasks))


;;; Process helpers ---------------------------------------------------------

(defun ent-task-names ()
  "Return a list of all task names currently defined."
  (cl-loop for key being the hash-keys of ent-tasks
           collect key))


(defun ent--get-all-deps (all-deps task-name)
  "Return a list with all dependencies of the TASK-NAME.

ALL-DEPS is a list of task names, initial is empty."
  (if (member task-name all-deps)
      all-deps
    (let* ((task (gethash task-name ent-tasks))
           (deps (ent-task-deps task))
           (new-deps (if deps
                           (cl-reduce 'ent--get-all-deps (split-string deps "[[:space:]]+" t) :initial-value all-deps)
                         all-deps)))
       (push task-name new-deps))))


(defun ent-run-shell-command (command task-list)
  "Run COMMAND asynchronously.

TASK-LIST contains the name of the tasks not executed yet.
Write output/error to `ent-log-buffer'."
  (ent-log "Running command: %s" command)
  (make-process
   :name (format "ent-shell-%s" (md5 command))
   :buffer ent--log-buffer
   :command (list ent-shell "-c" command)
   :sentinel (lambda (proc _status) (ent--sentinel command task-list proc))))


(defun ent--sentinel (command task-list proc)
  "After COMMAND exited, call next task from TASK-LIST if PROC exit was ok."
  (let ((exit-status (process-exit-status proc)))
    (ent-log "Command: %s exit with %d" command exit-status)
    (ent--start-next-task (if (zerop exit-status) task-list '()))))


;;; Task execution ----------------------------------------------------------

(defun ent--start-next-task (task-list)
  "Start the first task from the TASK-LIST."
  (if (null task-list)
      (ent-log "Done")
    (let* ((task-name (pop task-list))
           (task (gethash task-name ent-tasks))
           (action (ent-task-action task)))
      (ent-log "Start %s task" task-name)
      (cond
       ((functionp action) (progn
                             (funcall action)
                             (ent-log "End %s task" task-name)
                             (ent--start-next-task task-list)))
        ((stringp action) (ent-run-shell-command action task-list))
        ('t (ent--start-next-task task-list))))))


(defun ent--find-project-file ()
  "Return the project file absolute path of the current project."
  (expand-file-name ent-project-config-filename
                    (locate-dominating-file default-directory ent-project-config-filename)))


(defun ent--init-run ()
  "Initialize the global variables for a new run."
  (setq ent-tasks (make-hash-table :test 'equal))
  (ent--create-log-buffer)
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
      (ent--start-next-task task-list)
      ;; (ent--colorize-log)
      )))


(provide 'ent)
;;; ent.el ends here
