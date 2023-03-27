;;; Commentary:
;;;berrym-editor.el --- Configure Editor Behaviors

;; Copyright (c) 2023 Michael Berry

;; Author: Michael Berry <trismegustis@gmail.com>
;; URL: https://github.com/berrym/emacs-config

;; This file is not part of GNU Emacs.

;; License: GPLv3

;;; Code:

;; Define a C programming style
(defconst berrym-c-style
  '((c-basic-offset                 . 4)
    (c-tab-always-indent            . t)
    (c-comment-only-line-offset     . 0)
    (c-echo-syntactic-information-p . t)
    (c-report-syntactic-errors      . t)
    (indent-tabs-mode               . nil)
    (case-fold-search               . nil)
    (c-hanging-braces-alist         . ((brace-list-open)
                       (brace-list-intro)
                       (brace-list-close)
                       (brace-entry-open)
                       (substatement-open after)
                       (block-close . c-snug-do-while)
                       (arglist-cont-nonempty)
                       (defun-open after)
                       (class-open after)
                       (class-close berfore after)
                       (inexpr-class-open after)
                       (inexpr-class-close before)
                       (namespace-open after)
                       (inline-open after)
                       (inline-close before after)
                       (block-open after)
                       (extern-lang-open after)
                       (extern-lang-close after)
                       (statement-case-open after)))
    (c-hanging-colons-alist         . ((case-label)
                           (label after)
                           (access-label after)
                           (member-init-intro before)
                           (inher-intro)))
    (c-cleanup-list                 . ((brace-else-brace)
                       (brace-elseif-brace)
                       (brace-catch-brace)
                       (empty-defun-braces)
                       (defun-close-semi)
                       (scope-operator)))
    (c-offsets-alist                . ((statement-block-intro . +)
                       (statement-case-intro  . +)
                       (knr-argdecl-intro     . 0)
                       (substatement-open     . 0)
                       (substatement-label    . 0)
                       (label                 . 0)
                       (statement-cont        . +)
                       (inline-open           . 0)
                       (inexpr-class          . 0)))
    (c-hanging-semi&comma-criteria
     . (c-semi&comma-no-newlines-for-oneline-inliners
        c-semi&comma-inside-parenlist
        c-semi&comma-no-newlines-before-nonblanks)))
  "Michael Berry C Programming Style.")
(c-add-style "berrym" berrym-c-style)
(add-hook 'c-mode-common-hook (lambda () (c-set-style "berrym")))

;; reload files automatically if externally modified
(global-auto-revert-mode t)

;; tabs are 4 spaces
(setq-default tab-width 4
              indent-tabs-mode nil)

;;; Package configurations

(use-package diminish                   ; Hide most minor-modes from the mode-line
  :ensure t)

(use-package gnutls                     ; Use encryption always
  :defer t
  :custom
  (gnutls-verify-error t))

(use-package auto-complete              ; Use auto-complete
  :config
  (ac-config-default)
  (setq ac-comphist-file
      (expand-file-name "ac-comphist.dat" save-files-dir)))

(use-package expand-region              ; Expand selected regions around point
  :bind
  ("C-=" . er/expand-region))

(use-package helm                       ; Enhanced menu navigation
  :straight t
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

(use-package treemacs                   ; Eclpise like project browser
  :bind
  (("C-c t" . treemacs)
   ("s-a"   . treemacs)))

(use-package minimap
  :config                               ; Minimap of en
  (setq minimap-window-location 'right)
  :bind
  ([f10] . minimap-mode))

(use-package projectile                 ; Project wide management functions
  :ensure t
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

(use-package helm-projectile            ; Use helm for projectile command completion
  :config
  (helm-projectile-on))

(use-package whitespace-cleanup-mode    ; Cleanup whitespace
  :hook
  ((after-init       . whitespace-cleanup-mode)
   (before-save-hook . whitespace-cleanup)))

(use-package undo-tree                  ; Undo tree
  :hook
  (after-init . undo-tree-mode))

(use-package anzu                       ; Show number of search matches
  :hook
  (after-init . anzu-mode))

(use-package company                    ; Another completion backend
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

(use-package company-quickhelp          ; Company quick help
    :config
    (company-quickhelp-mode))

(use-package lsp-mode                   ; Language server protocol
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

(use-package racer                      ; Another completion backend
  :init
  (setq company-tooltip-align-annotations t)
  :hook
  (racer-mode-hook . company-mode))

(use-package rustic                     ; Rust programming language mode
  :init
  (setq rustic-lsp-server 'rust-analyzer)
  :hook
  ((rustic-mode . (lambda ()
                    (company-mode)
                    (set (make-local-variable 'company-backends)
                         '((company-capf company-files :with company-yasnippet)
                           (company-dabbrev-code company-dabbrev))))))
  :config
  (setq rust-indent-method-chain t)
  (setq rustic-lsp-format t)
  (setq rustic-format-on-save t))

(use-package magit                      ; Git porcelain
  :bind
  (("C-x g"   . magit-status)
   ("C-x M-g" . magit-dispatch)))

(use-package magit-stats
  :bind
  ("C-x M-s" . magit-stats))

(use-package git-messenger              ; More git helpers
  :bind
  ("C-x G" . git-messenger:popup-message)
  :config
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t))

(use-package ruby-mode                  ; Ruby programming language mode
  :mode "\\.rb\\'"
  :interpreter "ruby")

(use-package elpy                       ; Python programming language mode
  :init
  (elpy-enable))

(use-package blacken                    ; Auto format Python files
  :hook (python-mode . blacken-mode)
  :config
  (setq blacken-line-length '79))

(use-package python-docstring           ; Python mode docstring handler
  :diminish
  :hook (python-mode . python-docstring-mode))

(use-package haskell-mode               ; Haskell programming language mode
  :init
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode))

(require 'slime)
(use-package slime                      ; Superior Lisp Interaction Mode for Emacs
  :init
  (load "~/.roswell/helper.el")
  (setq inferior-lisp-program "ros -Q run")
  :config
  (slime-setup '(helm-slime))
  (global-helm-slime-mode))
               
(use-package vterm                      ; Virtual terminal emulater
  :bind
  ([f1] . vterm))

(message "berrym-editor: module loaded successfully.")

(provide 'berrym-editor)

;;; berrym-editor.el ends here
