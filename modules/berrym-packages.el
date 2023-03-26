;;; Commentary:
;;;berrym-pacakges.el --- Required packages for my sanity

;; Copyright (c) 2023 Michael Berry

;; Author: Michael Berry <trismegustis@gmail.com>
;; URL: https://github.com/berrym/emacs-config

;; This file is not part of GNU Emacs.

;; License: GPLv3

;;; Code:

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(setq package-user-dir (expand-file-name "packages" user-emacs-directory))

(setq package-selected-packages
      '(all-the-icons
        all-the-icons-dired
        all-the-icons-fonts
        anzu
        auto-complete
        benchmark-init
        blacken
        cargo
        company
        company-quickhelp
        counsel
        dashboard
        diminish
        doom-themes
        elpy
        expand-region
        fira-code-mode
        fontaine
        gcmh
        git-messenger
        haskell-mode
        helm
        helm-ag
        helm-projectile
        helm-slime
        highlight-parentheses
        lsp-mode
        magit
        meson-mode
        minimap
        on
        powerline
        projectile
        python-docstring
        rustic
        slime
        treemacs
        undo-tree
        vi-tilde-fringe
        vterm
        volatile-highlights
        whitespace-cleanup-mode))

(package-initialize)

(package-install-selected-packages)

(setq use-package-compute-statistics t)

(use-package bind-key                   ; Need to set this up to use :bind in use-package
  :demand t
  :config
  (add-to-list 'same-window-buffer-names "*Personal Keybindings*"))

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

(use-package benchmark-init
  :demand t
  :hook (after-init . benchmark-init/deactivate)
  :config
  (benchmark-init/activate))

(use-package gcmh
  :demand t
  :hook (emacs-startup . gcmh-mode))

(message "berrym-packages: module loaded successfully.")

(provide 'berrym-packages)

;;; berrym-packages.el ends here
