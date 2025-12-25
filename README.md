<!-- !!!THIS FILE HAS BEEN GENERATED!!! Edit README.org -->

<!-- a href="https://melpa.org/#/erk"><img src="https://melpa.org/packages/erk-badge.svg" alt="melpa package"></a> <a href="https://stable.melpa.org/#/erk"><img src="https://stable.melpa.org/packages/erk-badge.svg" alt="melpa stable package"></a> -->
<a href="https://github.com/dpom/ent/actions/?workflow=CI"><img src="https://github.com/dpom/ent/actions/workflows/ci.yml/badge.svg" alt="CI workflow status"></a>
<a href="https://github.com/dpom/ent/actions/?workflow=Developer+Certificate+of+Origin"><img src="https://github.com/dpom/ent/actions/workflows/dco.yml/badge.svg" alt="DCO Check"></a>


# Ent

`ent.el` is a **tiny build‑automation** framework written in pure Emacs‑Lisp.  
Its goal is to discover a *project* (identified by a dedicated `ent‑project‑config‑filename` file), load the project‑specific build file, create a set of **tasks** (default and user‑defined) and run a selected task while reporting the output in a *compilation* buffer.


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
    
    ;; or use manual load-path & require, you brave yak shaver


# Table of Contents

-   [Tasks](#org4c1aeb5)
    -   [Backlog](#org9548b47)
    -   [1.0 Initial version](#org61a4b6d)
    -   [2.0 Dash version](#orge8ef35c)
-   [Contributing](#org00ed356)


# Tasks


## Backlog


## 1.0 Initial version


### DONE create project layout


### DONE refactor project to work with straight


## 2.0 Dash version


### INPROGRESS build a manual test plan


### fix flatten bug


# Contributing

Because this repository is created from [elisp-repo-kit](https://github.com/positron-solutions/elisp-repo-kit), please file any issues
or make relevant pull requests there unless the changes are specific to this
repo.

See the [CONTRIBUTING](./CONTRIBUTING.md) guide for help making changes to this project.

For turn-key contribution to the software ecosystem that keeps you moving, see
the [funding links](https://github.com/sponsors/positron-solutions).

