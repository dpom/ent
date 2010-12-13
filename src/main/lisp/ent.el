;;; ent.el --- emacs build tool
;; Copyright (C) 2009 Dan Pomohaci (dpom)

;; Author: Dan Pomohaci <dan.pomohaci@gmail.com>
;; Version: 0.1
;; Keywords: build ant task 

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License.

;;; Commentary:


;;; Code:

(provide 'ent)


;;;; Custom variables

(defgroup ent nil
  "Emacs build tool"
  :version "1.0"
  :group 'tools
  :group 'processes)

(defcustom ent-file-name ".build.el"
  "Local ent file name"
  :type '(string)
  :group 'ent)


(defcustom ent-init-file "~/.emacs.d/init-ent.el"
  "Global ent init file name"
  :type '(string)
  :group 'ent)

;;;; Global variables

(defvar ent-project-name "" "Project name, must be set in the project init file.")

(defvar ent-project-home "" "Project home directory")

;;;; Auxiliary functions

(provide 'ent)

;;; ent.el ends here

