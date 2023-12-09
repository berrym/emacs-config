;;; Commentary:
;;;berrym-pacakges.el --- Required packages for my sanity

;; Copyright (c) 2023 Michael Berry

;; Author: Michael Berry <trismegustis@gmail.com>
;; URL: https://github.com/berrym/emacs-config

;; This file is not part of GNU Emacs.

;; License: GPLv3

;;; Code:

;; Setup straight
(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Prefer using straight.el use-package over package.el version
(straight-use-package 'use-package)

;; Use encryption always
(use-package gnutls
  :defer t
  :custom
  (gnutls-verify-error t))

;; Automatically update pacakges
(use-package auto-package-update
  :straight t
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; Clean up the native compilation cache
(use-package no-littering
  :straight t
  :ensure t)

;; Need to set this up to use :bind in use-package
(use-package bind-key
  :straight t
  :ensure t
  :config
  (add-to-list 'same-window-buffer-names "*Personal Keybindings*"))

;; Hide most minor-modes from the mode-line
(use-package delight
  :straight t
  :ensure t)

;; Which-key mode
(use-package which-key
  :straight t
  :ensure t
  :config
  (which-key-mode))

;; Auto revert buffers
(use-package autorevert
  :straight t
  :ensure t
  :delight auto-revert-mode
  :config (global-auto-revert-mode t))

;; Benchmark init
(use-package benchmark-init
  :straight t
  :ensure t
  :delight
  :hook (after-init . benchmark-init/deactivate)
  :config
  (benchmark-init/activate))

;; Garbage Collector Magic Hack
(use-package gcmh
  :straight t
  :ensure t
  :delight
  :hook (emacs-startup . gcmh-mode))

;; Use all-the-icons
(use-package all-the-icons
  :straight t
  :ensure t
  :delight
  :if (display-graphic-p))

;; Use all-the-icons in dired mode
(use-package all-the-icons-dired
  :straight t
  :ensure t)

;; Use auto-complete
(use-package auto-complete
  :straight t
  :ensure t
  :delight
  :config
  (ac-config-default)
  (setq ac-comphist-file
        (expand-file-name "ac-comphist.dat" save-files-dir)))

;; Use counsel
(use-package counsel
  :straight t
  :delight)

;; Expand selected regions around point
(use-package expand-region
  :straight t
  :ensure t
  :delight
  :bind
  ("C-=" . er/expand-region))

;; Enhanced menu navigation
(use-package helm
  :straight t
  :ensure t
  :delight
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1)
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-split-window-in-side-p t
        helm-move-to-line-cycle-in-source t
        helm-ff-search-library-in-sexp t
        helm-scroll-amount 8
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line t
        helm-autoresize-max-height 0
        helm-autoresize-min-height 20)
  :bind
  (("C-c h m"     . helm-mini)
   ("M-x"         . helm-M-x)
   ("C-x C-f"     . helm-find-files)
   ("C-x C-b"     . helm-buffers-list)
   ("C-x b"       . helm-buffers-list)
   ("M-i"         . helm-imenu)
   ("M-y"         . helm-show-kill-ring)
   ("C-c h M-s o" . helm-occur)
   ("C-c h a"     . helm-apropos)
   ([f3]          . helm-buffers-list)
   ([f2]          . helm-recentf)))

;; Use helm-ag
(use-package helm-ag
  :straight t
  :ensure t
  :delight)

;; Project wide management functions
(use-package projectile
  :straight t
  :ensure t
  :delight '(:eval (concat " " (projectile-project-name))) ; only show project name
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p"   . 'projectile-command-map)
              ("C-c p" . 'projectile-command-map))
  :config
  (setq projectile-completion-system 'helm
        projectile-known-projects-file (expand-file-name
                                        "projectile-bookmarks.eld"
                                        save-files-dir)))

;; Use helm for projectile command completion
(use-package helm-projectile
  :straight t
  :ensure t
  :delight
  :config
  (helm-projectile-on))

;; Use helm with slime
(use-package helm-slime
  :straight t
  :ensure t
  :delight)

;; Superior Lisp Interaction Mode for Emacs
(use-package slime
  :straight t
  :ensure t
  :delight
  :init
  (setq inferior-lisp-program "sbcl")
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  :config
  (slime-setup '(helm-slime slime-fancy slime-quicklisp slime-asdf slime-banner))
  (setq slime-complete-symbol*-fancy t)
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
  (global-helm-slime-mode))

;; Use parentheses highlights
(use-package highlight-parentheses
  :straight t
  :ensure t
  :delight)

;; Mode for editing meson build files
(use-package meson-mode
  :straight t
  :ensure t
  :delight)

;; Eclpise like project browser
(use-package treemacs
  :straight t
  :ensure t
  :delight
  :bind
  (("C-c t" . treemacs)
   ("s-a"   . treemacs)))

;; NERDTree like file browser
(use-package neotree
  :straight t
  :ensure t
  :delight
  :bind
  ("<f8>" . neotree)
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; Projectile support for treemacs
(use-package treemacs-projectile
  :straight t
  :ensure t)

;; imenu
(use-package imenu
  :straight t
  :ensure t
  :bind
  ("C-x i" . imenu))

;; use minimap of buffer
(use-package minimap
  :straight t
  :ensure t
  :delight
  :config
  (setq minimap-window-location 'right)
  :bind
  ([f10] . minimap-mode))

;; Cleanup whitespace automatically on save
(use-package whitespace-cleanup-mode
  :straight t
  :ensure t
  :delight
  :config
  (global-whitespace-cleanup-mode t))

;; Undo tree instead of default ring
(use-package undo-tree
  :straight t
  :ensure t
  :delight
  :hook
  (after-init . undo-tree-mode))

;; Show number of search matches
(use-package anzu
  :straight t
  :ensure t
  :delight
  :hook
  (after-init . anzu-mode))

;; Use org mode
(use-package org
  :straight t
  :ensure t
  :config
  (setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE"))))

;; Another completion backend
(use-package company
  :straight t
  :ensure t
  :delight
  :init
  (setq company-idle-delay t
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t)
  :hook
  (after-init . global-company-mode)
  (text-mode  . company-mode)
  (prog-mode  . company-mode)

  :config
  (setq company-backends
        '((company-files                 ; files & directory
           company-keywords              ; keywords
           company-capf)                 ; completion-at-point-functions
          (company-abbrev company-dabbrev)
          company-dabbrev-other-buffers t
          company-dabbrev-code-other-buffers t))
  :bind
  ("C-i"   . company-indent-or-complete-common)
  ("C-M-i" . counsel-company))

;; Company quick help
(use-package company-quickhelp
  :straight t
  :ensure t
  :delight
  :config
  (company-quickhelp-mode))

;; Use the Language Server Protocol
(use-package lsp-mode
  :straight t
  :ensure t
  :delight
  :commands
  (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((before-save . lsp-organize-imports)
   (python-mode . lsp-deferred))
  :bind
  (("C-c d"   . lsp-describe-thing-at-point)
   ("C-c e n" . flymake-goto-next-error)
   ("C-c e p" . flymake-goto-prev-error)
   ("C-c e r" . lsp-find-references)
   ("C-c e R" . lsp-rename)
   ("C-c e i" . lsp-find-implementation)
   ("C-c e t" . lsp-find-type-definition)))

;; Provides visual help in the buffer
(use-package lsp-ui
  :straight t
  :ensure t
  :defer t
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-delay 2)
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
          ("C-c i" . lsp-ui-imenu)))

;; Helm integration with lsp
(use-package helm-lsp
  :straight t
  :ensure t
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

;; lsp integration with treemacs
(use-package lsp-treemacs
  :straight t
  :ensure t)

;; Integration with the debug server
(use-package dap-mode
  :straight t
  :ensure t
  :defer t
  :delight
  :after lsp-mode
  :config
  (dap-auto-configure-mode))

;; Another completion backend
(use-package racer
  :straight t
  :ensure t
  :delight
  :init
  (setq company-tooltip-align-annotations t)
  :hook
  (racer-mode-hook . company-mode))

;; Rust programming language mode
(use-package rustic
  :straight t
  :ensure t
  :delight
  :init
  (setq rustic-lsp-server 'rust-analyzer)
  :config
  (setq rustic-analyzer-command '("~/.cargo/bin/rust-analyzer"))
  :hook
  ((rustic-mode . (lambda ()
                    (company-mode)
                    (set (make-local-variable 'company-backends)
                         '((company-capf company-files :with company-yasnippet)
                           (company-dabbrev-code company-dabbrev))))))
  :custom
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
  (rustic-rustfmt-config-alist '((edition . "2021")))
  :config
  (setq rust-indent-method-chain t)
  (setq rustic-lsp-format t)
  (setq rustic-format-on-save t))

;; Git porcelain
(use-package magit
  :straight t
  :ensure t
  :delight
  :bind
  (("C-x g"   . magit-status)
   ("C-x M-g" . magit-dispatch)))

;; Git statistics
(use-package magit-stats
  :straight t
  :ensure t
  :bind
  ("C-x M-s" . magit-stats))

;; More git helpers
(use-package git-messenger
  :straight t
  :ensure t
  :delight
  :bind
  ("C-x G" . git-messenger:popup-message)
  :config
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t))

;; Ruby programming language mode
(use-package ruby-mode
  :straight t
  :delight
  :mode "\\.rb\\'"
  :interpreter "ruby")

;; Python programming language mode
(use-package elpy
  :straight t
  :ensure t
  :delight
  :init
  (elpy-enable))

;; Auto format Python files using the uncompromising formatter
(use-package blacken
  :straight t
  :ensure t
  :delight
  :hook (python-mode . blacken-mode)
  :config
  (setq blacken-line-length '79))

;; Language server for Python
(use-package lsp-pyright
  :straight t
  :ensure t
  :defer t
  :config
  (setq lsp-pyright-disable-language-service nil
    lsp-pyright-disable-organize-imports nil
    lsp-pyright-auto-import-completions t
    lsp-pyright-use-library-code-for-types t)
  :hook ((python-mode . (lambda ()
                          (require 'lsp-pyright) (lsp-deferred)))))

;; Built-in Python utilities
(use-package python
  :ensure t
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil)
  ;; Use IPython when available or fall back to regular Python
  (cond
   ((executable-find "ipython")
    (progn
      (setq python-shell-buffer-name "IPython")
      (setq python-shell-interpreter "ipython")
      (setq python-shell-interpreter-args "-i --simple-prompt")))
   ((executable-find "python3")
    (setq python-shell-interpreter "python3"))
   ((executable-find "python2")
    (setq python-shell-interpreter "python2"))
   (t
    (setq python-shell-interpreter "python"))))

;; Use pydoc
(use-package pydoc
  :straight t
  :ensure t)

;; Helm navigation of pydoc
(use-package helm-pydoc
  :straight t
  :ensure t)

;; Required to easily switch virtual envs
(use-package pyvenv
  :straight t
  :ensure t
  :config
  ;; Display virtual envs in the menu bar
  (setq pyvenv-menu t)
  ;; Restart the python process when switching environments
  (add-hook 'pyvenv-post-activate-hooks (lambda ()
                                          (pyvenv-restart-python)))
  :hook (python-mode . pyvenv-mode))

;; Python mode docstring handler
(use-package python-docstring
  :straight t
  :ensure t
  :delight
  :hook (python-mode . python-docstring-mode))

;; Haskell programming language mode
(use-package haskell-mode
  :straight t
  :ensure t
  :delight
  :init
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode))

;; Virtual terminal emulater
(use-package vterm
  :straight t
  :ensure t
  :delight
  :bind
  ([f1] . vterm))

(message "berrym-packages: module loaded successfully.")

(provide 'berrym-packages)

;;; berrym-packages.el ends here
