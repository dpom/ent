<!-- !!!THIS FILE HAS BEEN GENERATED!!! Edit README.org -->

<!-- a href="https://melpa.org/#/erk"><img src="https://melpa.org/packages/erk-badge.svg" alt="melpa package"></a> <a href="https://stable.melpa.org/#/erk"><img src="https://stable.melpa.org/packages/erk-badge.svg" alt="melpa stable package"></a> -->
<a href="https://github.com/dpom/ent/actions/?workflow=CI"><img src="https://github.com/dpom/ent/actions/workflows/ci.yml/badge.svg" alt="CI workflow status"></a>
<a href="https://github.com/dpom/ent/actions/?workflow=Developer+Certificate+of+Origin"><img src="https://github.com/dpom/ent/actions/workflows/dco.yml/badge.svg" alt="DCO Check"></a>


# Ent

`ent.el` is a **tiny build‑automation** framework written in pure Emacs‑Lisp.

Its goal is to discover a *project* (identified by a dedicated
`ent‑project‑config‑filename` file), load the project‑specific build
file, create a set of **tasks** (default and user‑defined) and run a
selected task while reporting the output in a *compilation* buffer.


# Install ent

    ;; update this after you publish your new package!
    ;; (use-package ent) ; vanilla, assuming you have MELPA configured
    
    ;; package-vc
    (package-vc-install
     '(ent :url "https://github.com/dpom/ent.git"
           :lisp-dir "lisp"
           :doc "doc/ent.texi"))
    
    ;; using elpaca's with explicit recipe
    (use-package ent
      :elpaca (ent :host github :repo "dpom/ent"))
    
    ;; straight with explicit recipe
    (use-package ent
      :straight (ent :type git :host github :repo "dpom/ent"))
    
    ;; for my emacs config (twix) a recipe file:
    (ent :fetcher github :repo "dpom/ent"
         :files ("lisp/*.el"))


# Usage

First of all you need to create a build file `.ent.el` in the project root directory.

The minimal form of this file is:

    ;;; .ent.el --- local ent config file -*- lexical-binding: t; -*-
    
    ;;; Commentary:
    
    ;;; Code:
    
    ;; project settings
    (setq ent-project-home (file-name-directory (if load-file-name load-file-name buffer-file-name)))
    (setq ent-project-name "project name")
    
    (ent-load-default-tasks)
    
    (provide '.ent)
    ;;; .ent.el ends here
    
    ;; Local Variables:
    ;; no-byte-compile: t
    ;; no-update-autoloads: t
    ;; End:

To run a task run the `ent-run` command and select the task from the
list of active tasks. In minimal form only the default tasks are
active (help, clean, dirclean, env) but the big advantage of `ent` is
the ease with which you can create new tasks.

A new task is created with the function:

    (task "nume-task"
          :doc "Explain the functionality of the task."
          :deps "A list of task names on which the task depends separated by space."
          :action "Action to execute")

All the three key parameters (`doc`, `deps` and `action`) are optional but of course a task without any of them makes no sense.

The `:action` parameter can be a string or an elisp function. If it is a string then it is a shell command that executes in the root directory of the project. Example:

    (task "shell-example"
          :doc "List the files in the project root directory after it has been cleaned."
          :deps "clean dirclean"
          :action "ls -la")

The elisp function is a function without any parameters and can be interactive or not. Example:

    (task "elisp-example"
          :doc "Display a greeting message."
          :action (lambda ()
                   (let  ((greeting (completing-read "Greeting: " '("Hello" "Buna" "Bonjour") nil t)))
                     (message "%s Dan" greeting)
                     )))

**Note** To have the functionality of a compilation output buffer set the log file to `compilation-mode` after task execution.

