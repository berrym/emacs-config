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

(use-package bind-key                   ; Need to set this up to use :bind in use-package
  :straight t
  :config
  (add-to-list 'same-window-buffer-names "*Personal Keybindings*"))

;; Hide most minor-modes from the mode-line
(use-package diminish
  :straight t)

(use-package benchmark-init
  :straight t
  :diminish t
  :hook (after-init . benchmark-init/deactivate)
  :config
  (benchmark-init/activate))

(use-package gcmh
  :straight t
  :diminish t
  :hook (emacs-startup . gcmh-mode))

;; Use encryption always
(use-package gnutls
  :defer t
  :custom
  (gnutls-verify-error t))

;; Use all-the-icons
(use-package all-the-icons
  :straight t
  :diminish t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :straight t)

;; Use auto-complete
(use-package auto-complete
  :straight t
  :diminish t
  :config
  (ac-config-default)
  (setq ac-comphist-file
        (expand-file-name "ac-comphist.dat" save-files-dir)))

;; Use counsel
(use-package counsel
  :straight t
  :diminish t)

;; Expand selected regions around point
(use-package expand-region
  :straight t
  :diminish t
  :bind
  ("C-=" . er/expand-region))

;; Enhanced menu navigation
(use-package helm
  :straight t
  :diminish t
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
  :diminish t)

;; Project wide management functions
(use-package projectile
  :straight t
  :diminish t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p"    . 'projectile-command-map)
              ("C-c p" . 'projectile-command-map))
  :config
  (setq projectile-completion-system 'helm
        projectile-known-projects-file (expand-file-name
                                        "projectile-bookmarks.eld"
                                        save-files-dir)))

;; Use helm for projectile command completion
(use-package helm-projectile
  :straight t
  :diminish t
  :config
  (helm-projectile-on))

;; Use helm with slime
(use-package helm-slime
  :straight t
  :diminish t)

;; Superior Lisp Interaction Mode for Emacs
(use-package slime
  :straight t
  :diminish t
  :init
  (load "~/.roswell/helper.el")
  (setq inferior-lisp-program "ros -Q run")
  :config
  (slime-setup '(helm-slime))
  (global-helm-slime-mode))

;; Use parentheses highlights
(use-package highlight-parentheses
  :straight t
  :diminish t)

;; Mode for editing meson build files
(use-package meson-mode
  :straight t
  :diminish t)

;; Eclpise like project browser
(use-package treemacs
  :straight t
  :diminish t
  :bind
  (("C-c t" . treemacs)
   ("s-a"   . treemacs)))

;; use minimap of buffer
(use-package minimap
  :straight t
  :diminish t
  :config
  (setq minimap-window-location 'right)
  :bind
  ([f10] . minimap-mode))

;; Cleanup whitespace automatically on save
(use-package whitespace-cleanup-mode
  :straight t
  :diminish t
  :config
  (global-whitespace-cleanup-mode t))

;; Undo tree instead of default ring
(use-package undo-tree
  :straight t
  :diminish t
  :hook
  (after-init . undo-tree-mode))

;; Show number of search matches
(use-package anzu
  :straight t
  :diminish t
  :hook
  (after-init . anzu-mode))

;; Another completion backend
(use-package company
  :straight t
  :diminish t
  :init
  (setq company-idle-delay t
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t)
  :hook
  (after-init . global-company-mode)
  :config
  (setq company-backends
        '((company-files                 ; files & directory
           company-keywords              ; keywords
           company-capf)                 ; completion-at-point-functions
          (company-abbrev company-dabbrev)))
  :bind
  ("C-i"   . company-indent-or-complete-common)
  ("C-M-i" . counsel-company))

;; Company quick help
(use-package company-quickhelp
  :straight t
  :diminish t
  :config
  (company-quickhelp-mode))

;; Use the Language Server Protocol
(use-package lsp-mode
  :straight t
  :diminish t
  :commands lsp
  :hook
  ((before-save . lsp-format-buffer)
   (before-save . lsp-organize-imports))
  :bind
  (("C-c d"   . lsp-describe-thing-at-point)
   ("C-c e n" . flymake-goto-next-error)
   ("C-c e p" . flymake-goto-prev-error)
   ("C-c e r" . lsp-find-references)
   ("C-c e R" . lsp-rename)
   ("C-c e i" . lsp-find-implementation)
   ("C-c e t" . lsp-find-type-definition)))

;; Another completion backend
(use-package racer
  :straight t
  :diminish t
  :init
  (setq company-tooltip-align-annotations t)
  :hook
  (racer-mode-hook . company-mode))

;; Rust programming language mode
(use-package rustic
  :straight t
  :diminish t
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
  :config
  (setq rust-indent-method-chain t)
  (setq rustic-lsp-format t)
  (setq rustic-format-on-save t))

;; Git porcelain
(use-package magit
  :straight t
  :diminish t
  :bind
  (("C-x g"   . magit-status)
   ("C-x M-g" . magit-dispatch)))

;; Git statistics
(use-package magit-stats
  :straight t
  :bind
  ("C-x M-s" . magit-stats))

;; More git helpers
(use-package git-messenger
  :straight t
  :diminish t
  :bind
  ("C-x G" . git-messenger:popup-message)
  :config
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t))

;; Ruby programming language mode
(use-package ruby-mode
  :straight t
  :diminish t
  :mode "\\.rb\\'"
  :interpreter "ruby")

;; Python programming language mode
(use-package elpy
  :straight t
  :diminish t
  :init
  (elpy-enable))

;; Auto format Python files
(use-package blacken
  :straight t
  :diminish t
  :hook (python-mode . blacken-mode)
  :config
  (setq blacken-line-length '79))

;; Python mode docstring handler
(use-package python-docstring
  :straight t
  :diminish t
  :hook (python-mode . python-docstring-mode))

;; Haskell programming language mode
(use-package haskell-mode
  :straight t
  :diminish t
  :init
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode))

;; Virtual terminal emulater
(use-package vterm
  :straight t
  :diminish t
  :bind
  ([f1] . vterm))

(message "berrym-packages: module loaded successfully.")

(provide 'berrym-packages)

;;; berrym-packages.el ends here
