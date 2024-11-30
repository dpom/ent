;;; ent.el --- Emacs build tool

;; Copyright (C) 2019  Dan Pomohaci

;; Author: Dan Pomohaci
;; Version: 2.0
;; Homepage: https://gitlab.com/dpom/ent
;; Keywords: elisp tools maint
;; Package-Requires: ((emacs "25.1") (dash "2.19.0") cl-lib  (seq))

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

;; Custom variables

(defgroup ent nil
  "Emacs build tool"
  :version "2.0"
  :group 'tools
  :group 'processes)


(defcustom ent-emacs-exec "emacs"
  "Emacs executable."
  :type '(string)
  :group 'ent)

(defcustom ent-default-task-number 200
  "Default task number."
  :type '(integer)
  :group 'ent)


(defcustom ent-default-task 'clean
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
  :type '(list)
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
  (ent-walk src regexp '(lambda (x)
                          (message "copy %s to %s" x dest)
                          (make-directory dest t)
                          (copy-file x dest t))))

(defun ent-mcopy ()
  "Multiple recursive copy using ent-mcopy-list."
  (mapc #'(lambda (y)
            (message "%s" y)
            (apply 'ent-rcopy y))
        ent-mcopy-list))

(defsubst ent-get-name (tsk)
  "Get TSK name."
  (symbol-name tsk))

(defsubst ent-get-dependencies (tsk)
  "Get TSK dependencies."
  (get tsk :dependencies))

(defsubst ent-get-description (tsk)
  "Get TSK description."
  (get tsk :description))

(defsubst ent-get-function (tsk)
  "Get TSK function."
  (symbol-function tsk))

(defsubst ent-put-dependencies (tsk val)
  "Put VAL as TSK dependencies."
  (put tsk :dependencies val))

(defsubst ent-put-description (tsk val)
  "Put VAL as TSK description."
  (put tsk :description val))

(defsubst ent-put-function (tsk val)
  "Put VAL as TSK function."
  (fset tsk val))

(defsubst ent-get-task (name)
  "Get task NAME."
  (intern-soft name ent-tasks))

(defsubst ent-has-function (task)
  "Return not-nil if TASK is a function."
  (fboundp task))

(defsubst ent-remove-function (task)
  "Make TASK function definition nil."
  (fmakunbound task))

(defun ent-flatten (x)
  "Flatten X."
  (cl-labels ((rec (x acc)
                   (cond ((null x) acc)
                         ((atom x) (cons x acc))
                         (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun ent-parse-dep (dep subprojects dir)
  "Parse DEP and return a dependency item \(TASK PROJECT-DIR\) using SUBPROJECTS and DIR."
  (if (listp dep)
      (list (car dep) (expand-file-name (cdr (assoc (cdr dep) subprojects)) dir))
    (list dep dir)))

(defun ent-get-dep-list (name)
  "Create a complete NAME task dependencies list."
  (let ((task (intern (symbol-name name) ent-tasks)))
    (delete-dups (ent-flatten (append (mapcar 'ent-get-dep-list  (ent-get-dependencies task)) name)))))

(defun ent-batch (dir name tasks &optional file)
  "Run the NAME task dep list in DIR directory using FILE as init file."
  (compile (concat "cd " dir ";"
                   "export EMACSLOADPATH=\"" (ent-expand-dir-name dir) ":\";"
                   (mapconcat #'(lambda (x)
                                  (let ((task (intern (symbol-name x) tasks)))
                                    (if (ent-has-function task)
                                        (funcall task file))))
                              (ent-get-dep-list name)
                              ";"))))

(defun ent-emacs (task file)
  "Run a batch Emacs loading FILE and evaluating TASK function."
  (concat ent-emacs-exec " --batch -u dan --kill -l \"" file "\" -f " task))

(defun task (name depends desc &optional func)
  "Create a task with NAME, DEPENDS, DESC and FUNC and insert it in the ent-tasks-list."
  (let ((task (intern (symbol-name name) ent-tasks)))
    (ent-put-dependencies task depends)
    (ent-put-description task desc)
    (if func (ent-put-function task func)
      (ent-remove-function task))))

(defun ent-find-project-file ()
  "Return the project file absolute path of the current project."
  (expand-file-name ent-project-config-filename (locate-dominating-file default-directory ent-project-config-filename)))

;;;; Global tasks

;; clean
(defun ent-clean-task  (&optional regexp)
  "Remove all files matching REGEXP or ent-clean-regexp from current dir recursively."
  (let ((acc 0)
        (dir default-directory)
        (regexp (or regexp ent-clean-regexp ent-clean-default-regexp)))
    (displaying-byte-compile-warnings
     (message "\nClean: %s from %s" regexp dir)
     (ent-walk dir regexp #'(lambda (x)
                              (delete-file (expand-file-name x))
                              (setq acc (+ acc 1))
                              (message "%s deleted" (expand-file-name x))))
     (message "Clean: command terminated %d files removed" acc)
     acc)))

(defun ent-clean-init ()
  "Initialise the clean task."
  (task 'clean ()  (documentation 'ent-clean-task) '(lambda (&optional file)
                                                      (ent-emacs "ent-clean-task" file))))

;; dirclean
(defun ent-dirclean-task  (&optional regexp)
  "Remove all directories matching REGEXP or ent-dirclean-regexp from current dir recursively."
  (let ((acc 0)
        (dir default-directory)
        (regexp (or regexp ent-dirclean-regexp ent-dirclean-default-regexp)))
    (displaying-byte-compile-warnings
     (message "\nDirClean: %s from %s" regexp dir)
     (ent-dir-walk dir regexp #'(lambda (x)
                                  (dired-delete-file (expand-file-name x) 'always)
                                  (setq acc (+ acc 1))
                                  (message "%s deleted" (expand-file-name x))))
     (message "DirClean: command terminated %d directories removed" acc)
     acc)))

(defun ent-dirclean-init ()
  "Initialise the dirclean task."
  (task 'dirclean ()  (documentation 'ent-dirclean-task) '(lambda (&optional file)
                                                            (ent-emacs "ent-dirclean-task" file))))


;; help
(defun ent-help-task (&optional arg)
  "Display tasks description.
ARG is only for compatibility with the other functions."
  (displaying-byte-compile-warnings
   (message "\nHelp:")
   (mapatoms #'(lambda (x) (message "%s\t- %s" (ent-get-name x) (ent-get-description x)))
             ent-tasks)))

(defun ent-help-init ()
  "Initialise the help task."
  (task 'help ()  (documentation 'ent-help-task) '(lambda (&optional file)
                                                    (ent-emacs "ent-help-task" file))))
;; env
(defun ent-env-task (&optional arg)
  "Display environment variables.
ARG is only for compatibility with the other functions."
  (displaying-byte-compile-warnings
   (message "\nEnv:")
   (message "load-path: %s" load-path)
   (message "arg: %s" arg)
   (message "project name: %s" ent-project-name)
   (message "project dir: %s" ent-project-home)
   (message "project tasks: %s" (seq-remove (lambda (elt) (and (numberp elt) (zerop elt))) ent-tasks))

   ))

(defun ent-env-init ()
  "Initialise the help task."
  (task 'env ()  (documentation 'ent-env-task) '(lambda (&optional file)
                                                  (ent-emacs "ent-env-task" file))))


;; elispbuild
(defun ent-elispbuild-task (&optional arg)
  "Build an elisp project.
ARG is only for compatibility with the other fs functions."
  (let ((dir (expand-file-name (or ent-elisp-src-dir ent-elisp-default-src-dir))))
    (displaying-byte-compile-warnings
     (message "Project %s\n" ent-project-name)
     (message "Build: compile el files found in %s" dir)
     (byte-recompile-directory dir 0)
     (message "\nBuild: bin-compile command terminated"))))

(defun ent-elispbuild-init ()
  "Initialise the elispbuild task."
  (task 'elispbuild () (documentation 'ent-elispbuild-task) '(lambda (&optional file)
                                                               (ent-emacs "ent-elispbuild-task" file))))

;; mcopy
(defun ent-mcopy-task  (&optional regexp)
  "Multiple copy using REGEXP or ent-mcopy-list recursively."
  (let ((dir default-directory))
    (displaying-byte-compile-warnings
     (message "\nMCopy")
     (ent-mcopy)
     (message "\nMCcopy: command terminated"))))

(defun ent-mcopy-init ()
  "Initialise mcopy task."
  (task 'mcopy ()  (documentation 'ent-mcopy-task) '(lambda (&optional file)
                                                      (ent-emacs "ent-mcopy-task" file))))

;;;; Tasks initialization
(defun ent-tasks-init (&optional maxtasks)
  "Initialize the global variables with default values.
Optional you could set MAXTASKS."
  (if (not maxtasks) (setq maxtasks ent-default-task-number))
  (setq ent-tasks (make-vector maxtasks 0))
  (ent-clean-init)
  (ent-dirclean-init)
  (ent-help-init)
  (ent-env-init)
  (ent-elispbuild-init)
  (ent-mcopy-init))

;;;; Commands

;;;###autoload
(defun ent (&optional taskname)
  "Main entry point for ent.
You could specify the TASKNAME."
  (interactive)
  (let ((initfile (ent-find-project-file))
        (dir (locate-dominating-file default-directory ent-project-config-filename)))
    (load initfile)
    (if (not taskname)
        (setq taskname (ido-completing-read  "Command: "
                                             (mapcar 'ent-get-name (seq-filter 'symbolp ent-tasks))
                                             nil t)))
    (ent-batch dir (ent-get-task taskname) ent-tasks initfile)))

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
    (define-key map "x" 'ent)
    (define-key map "c" 'ent-visit-config-file)
    (define-key map "b" 'ent-visit-build-file)
    (define-key map "f" 'ent-find-file)
    map))

;;;###autoload
(fset 'ent-prefix-map ent-prefix-map)

(provide 'ent)

;;; ent.el ends here
