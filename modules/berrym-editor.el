;;; Commentary:
;;;berrym-editor.el --- Configure Editor Behaviors

;; Copyright (c) 2022 Michael Berry

;; Author: Michael Berry <trismegustis@gmail.com>
;; URL: https://github.com/berrym/emacs-config

;; This file is not part of GNU Emacs.

;; License: GPLv3

;;; Code:

(defun repeat-last-search-forward ()
  "Repeat last search forward."
  (interactive)
  (search-forward (car search-ring)))

(defun repeat-last-search-backward ()
  "Repeat last search backward."
  (interactive)
  (search-backward (car search-ring)))

(defun backward-kill-line ()
  "Kill from point to start of line, if beginning of line delete newline."
  (interactive)
  (if (bolp)
      (backward-delete-char 1)
    (kill-line 0)))

(defun duplicate-line ()
  "Create a duplicate of current line."
  (interactive)
  (save-mark-and-excursion
    (beginning-of-line)
    (insert (thing-at-point 'line t))))

(defun transpose-line-down ()
  "Transpose current line down."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun transpose-line-up ()
  "Transpose current line up."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (forward-line -1)
    (move-to-column col)))

(defun print-elements-in-list (l)
  "Print each element in a list L."
  (interactive)
  (while l
    (print (car l))
    (setq l (cdr l))))

;; define a c programming style
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

(use-package auto-complete
  :diminish
  :config
  (ac-config-default)
  (setq ac-comphist-file
      (expand-file-name "ac-comphist.dat" save-files-dir)))

(use-package expand-region
  :diminish
  :bind
  (("C-=" . er/expand-region)))

;; enhanced menu navigation
(use-package helm
  :diminish
  :config
  (require 'helm-config)
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
  (("C-c h m"     . 'helm-mini)
   ("M-x"         . 'helm-M-x)
   ("C-x C-f"     . 'helm-find-files)
   ("C-x C-b"     . 'helm-buffers-list)
   ("C-x b"       . 'helm-buffers-list)
   ("M-i"         . 'helm-imenu)
   ("M-y"         . 'helm-show-kill-ring)
   ("C-c h M-s o" . 'helm-occur)
   ("C-c h a"     . 'helm-apropos)
   ([f10]         . 'helm-buffers-list)
   ([S-M-f10]     . 'helm-recentf)))

;; NERDTree like file browser
(use-package treemacs
  :diminish
  :bind
  (("C-c t" . treemacs)
   ("s-a"   . treemacs)))

;; minimap
(use-package minimap
  :diminish
  :config
  (setq minimap-window-location 'right)
  :bind
  (([f10] . minimap-mode)))

;; project management
(use-package projectile
  :diminish
  :bind
  ("C-c p" . projectile-command-map)
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm
        projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" save-files-dir)))

(use-package helm-projectile
  :diminish
  :config
  (helm-projectile-on))

;; cleanup whitespace
(use-package whitespace-cleanup-mode
  :diminish
  :hook
  ((after-init       . whitespace-cleanup-mode)
   (before-save-hook . whitespace-cleanup)))

;; undo tree
(use-package undo-tree
  :diminish
  :hook
  (after-init . undo-tree-mode))

;; show number of search matches
(use-package anzu
  :diminish
  :hook
  (after-init . anzu-mode))

(use-package company
  :diminish
  :init
  (setq company-idle-delay t
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t)
  :hook
  (after-init . global-company-mode)
  :config
  (setq company-backends
        '((company-files          ; files & directory
           company-keywords       ; keywords
           company-capf)  ; completion-at-point-functions
          (company-abbrev company-dabbrev)))
  :bind
  ("C-i"   . company-indent-or-complete-common)
  ("C-M-i" . counsel-company))

(use-package company-quickhelp
    :config
    (company-quickhelp-mode))

(use-package lsp-mode
  :diminish
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

(use-package racer
  :diminish
  :init
  (setq company-tooltip-align-annotations t)
  :hook
  (racer-mode-hook . company-mode))

(use-package rustic
  :diminish
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


;; git porcelain
(use-package magit
  :diminish
  :bind
  (("C-x g"   . magit-status)
   ("C-x M-g" . magit-dispatch)))

(use-package git-messenger
  :diminish
  :bind
  (("C-x G" . git-messenger:popup-message))
  :config
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t))

;; language modes
(use-package ruby-mode
  :diminish
  :mode "\\.rb\\'"
  :interpreter "ruby")

(use-package elpy
  :diminish
  :init
  (elpy-enable))

(use-package blacken
  :diminish
  :hook (python-mode . blacken-mode)
  :config
  (setq blacken-line-length '79))

(use-package python-docstring
  :diminish
  :hook (python-mode . python-docstring-mode))

(use-package vterm
  :diminish)

(use-package haskell-mode
  :diminish
  :init
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode))

(setq inferior-lisp-program "ros -Q run")

(message "berrym-editor: module loaded successfully.")

(provide 'berrym-editor)

;;; berrym-editor.el ends here
