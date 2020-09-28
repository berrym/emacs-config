;;; Commentary:
;;;berrym-pacakges.el --- Required packages for my sanity

;; Copyright (c) 2020 Michael Berry

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
  '(anzu
    auto-complete
    diminish
    elpy
    fira-code-mode
    helm
    helm-ag
    helm-projectile
    highlight-parentheses
    ir-black-theme
    lsp-mode
    magit
    meson-mode
    minimap
    powerline
    projectile
    rustic
    slime
    solarized-theme
    spacemacs-theme
    spaceline
    treemacs
    undo-tree
    use-package
    volatile-highlights
    whitespace-cleanup-mode
    zenburn-theme))

(package-initialize)

(package-install-selected-packages)

(message "berrym-packages: module loaded successfully.")

(provide 'berrym-packages)

;;; berrym-packages.el ends here
