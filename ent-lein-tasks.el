;;; ent-lein-tasks.el --- leiningen tasks -*- lexical-binding: t -*-

;; Copyright (C) 2019  Dan Pomohaci

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


;;; Commentary:

;;; Code:

(require 'ent)

(defvar ent-lein-project-version ""
  "Lein project version.")

(defvar ent-lein-opts ""
  "Lein options.")


(defun ent-lein-get-version (&optional projname)
  "Get version from project.clj."
  (with-temp-buffer
    (insert-file-contents (expand-file-name  "project.clj" ent-project-home))
    (goto-char (point-min)) ;; From the beginning...
    (if (re-search-forward (concat "defproject " (or projname ent-project-name) " "  "\\(.*\\)") nil t 1)
        (match-string 1))))

(defun make-image-tag (&optional version)
  "Make docker image tag using ent variables."
  (concat ent-project-name ":" (or version ent-lein-project-version)))

(provide 'ent-lein-tasks)

;;; ent-lein-tasks.el ends here
