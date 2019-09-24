;;; Commentary:
;;;berrym-pacakges.el --- Required packages for my sanity

;; Copyright (c) 2019 Michael Berry

;; Author: Michael Berry <trismegustis@gmail.com>
;; URL: https://bitbucket.org/berrym/emacs-config

;; This file is not part of GNU Emacs.

;; License: GPLv3

;;; Code:

(require 'package)

;; (if (and (version< emacs-version "26.3") (>= libgnutls-version 30604))
;;     (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; (add-to-list 'package-archives
;; 	     '("elpy" . "http://jorgenschaefer.github.io/packages/"))
;; (add-to-list 'package-archives
;;           '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(setq package-user-dir (expand-file-name "packages" user-emacs-directory))

(unless package-archive-contents
  (package-refresh-contents))

(setq package-selected-packages
  '(anzu
    auto-complete
    company
    diminish
    dumb-jump
    elpy
    expand-region
    flycheck
    google-this
    haskell-mode
    helm
    helm-ag
    helm-projectile
    highlight-parentheses
    ir-black-theme
    jedi
    meson-mode
    monky
    neotree
    powerline
    projectile
    python-mode
    ;; rustic
    slime
    solarized-theme
    spacemacs-theme
    spaceline
    undo-tree
    volatile-highlights
    whitespace-cleanup-mode
    zenburn-theme))

(package-initialize)

(package-install-selected-packages)

(message "berrym-packages: module loaded successfully.")

(provide 'berrym-packages)

;;; berrym-packages.el ends here
