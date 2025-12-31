;;; ent-default-tasks.el --- Default tasks -*- mode: emacs-lisp; lexical-binding: t -*-

;; Copyright (C) 2025  Dan Pomohaci

;; Author: Dan Pomohaci
;; Version: 2.2
;; Homepage: https://gitlab.com/dpom/ent
;; Keywords: elisp tools project
;; Package-Requires: ((emacs "28.1"))

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

;; Default tasks

;; Add these tasks with:
;; `(require 'ent-default-tasks)'

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'ent)


(defcustom ent-dirclean-default-regexp "target"
  "Default regular expression to match garbage directories."
  :type '(string)
  :group 'ent)

(defcustom ent-clean-default-regexp "~$"
  "Default regular expression to match garbage files."
  :type '(string)
  :group 'ent)

(defvar ent-clean-regexp nil
  "Regular expression to match garbage files.")

(defvar ent-dirclean-regexp nil
  "Regular expression to match garbage directories.")


;;; Utils functions ---------------------------------------------------------

(defun ent-expand-dir-name (dir &optional path)
  "Expand a local DIR name using his PATH."
  (file-name-as-directory (expand-file-name dir path)))

(cl-defun ent-walk (root regexp fn)
  "Recursively walk ROOT and invoke FN on each *file* that matches REGEXP.
ROOT may be a file or a directory.  FN is a function that receives the
file path as its sole argument.

If ROOT is a directory, the traversal is depth‑first: child directories are
walked first, then the file itself is processed."
  (cond
   ;; 1️⃣  ROOT is a directory – recurse into it
   ((file-directory-p root)
    (cl-loop for child in (directory-files root t "[^.]")
             do (ent-walk child regexp fn)))

   ;; 2️⃣  ROOT is a file – apply FN only if it matches the regexp
   ((and (file-regular-p root) (string-match regexp root))
    (funcall fn root))))

(cl-defun ent-dir-walk (root pattern callback)
  "Recursively walk ROOT and invoke CALLBACK on each dir that matches PATTERN.

CALLBACK is a function that receives the directory path as its sole argument.

The traversal is depth‑first: child directories are visited *before*
the directory itself.  This guarantees that a callback that removes a
directory never runs on a child that has already been deleted."
  (when (file-directory-p root)
    ;; Recurse into all children first
    (cl-loop for child in (directory-files root t "[^.]")
             do (ent-dir-walk child pattern callback))
    ;; Now process the root itself, if it matches
    (when (string-match pattern root)
      (funcall callback root))))


(defun ent--help-action ()
  "List active tasks."
  (let ((task-names (sort (ent-task-names))))
    (ent-log* "Project name: %s" ent-project-name)
    (ent-log* "Project dir: %s" ent-project-home)
    (ent-log* "Project tasks:")
    (dolist (name task-names)
      (let ((task (gethash name ent-tasks)))
        (ent-log* "  - %-10s\t- %s" name (ent-task-doc task))))))


(defun ent--clean-action ()
  "Remove all files matching a regexp from project root directory recursively. The regexp is the `ent-clean-regexp' variable (if not set is `ent-clean-default-regexp')."
  (let ((regexp (or ent-dirclean-regexp ent-dirclean-default-regexp))
        (dir ent-project-home)
        (removed 0))
    (ent-log* "Clean: %s from %s" regexp dir)
    (ent-walk dir regexp #'(lambda (x)
                              (delete-file (expand-file-name x))
                              (cl-incf removed)
                              (ent-log* "clean: %s deleted" (expand-file-name x))))
    (ent-log* "Clean: command terminated %d files removed" removed)
    removed))


(defun ent--dirclean-action ()
  "Remove all directories matching regexp from project root dir recursively. The regexp is the `ent-dirclean-regexp' variable (if not set is `ent-dirclean-default-regexp')."
  (let ((removed 0)
        (dir ent-project-home)
        (regexp (or ent-dirclean-regexp ent-dirclean-default-regexp)))
    (ent-log* "DirClean: %s from %s" regexp dir)
    (ent-dir-walk dir regexp #'(lambda (x)
                                  (dired-delete-file (expand-file-name x) 'always)
                                  (cl-incf removed)
                                  (ent-log* "%s deleted" (expand-file-name x))))
    (ent-log*  "DirClean: %d directories removed" removed)
    removed))


(defun ent--env-action ()
  "Display environmeqnt variables."
  (ent-log* "\nEnvironment:")
  (ent-log* "load-path:\n %s\n" load-path)
  (ent-log* "initial-environment:\n %s\n" initial-environment)
  (ent-log* "process-environment:\n %s\n" process-environment))



;;; Tasks -------------------------------------------------------------------

;;;###autoload
(defun ent-load-default-tasks ()
  "Load the default tasks."
  (task "help"
        :doc (documentation 'ent--help-action)
        :action 'ent--help-action)
  (task "clean"
        :doc (documentation 'ent--clean-action)
        :action 'ent--clean-action)
  (task "dirclean"
        :doc (documentation 'ent--dirclean-action)
        :action 'ent--dirclean-action)
  (task "env"
        :doc (documentation 'ent--env-action)
        :action 'ent--env-action))

(provide 'ent-default-tasks)
;;; ent-default-tasks.el ends here
