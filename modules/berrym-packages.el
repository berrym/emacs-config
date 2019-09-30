;;; Commentary:
;;;berrym-pacakges.el --- Required packages for my sanity

;; Copyright (c) 2019 Michael Berry

;; Author: Michael Berry <trismegustis@gmail.com>
;; URL: https://bitbucket.org/berrym/emacs-config

;; This file is not part of GNU Emacs.

;; License: GPLv3

;;; Code:

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(setq package-user-dir (expand-file-name "packages" user-emacs-directory))

(unless package-archive-contents
  (package-refresh-contents))

(setq package-selected-packages
  '(anzu
    auto-complete
    diminish
    elpy
    helm
    helm-ag
    helm-projectile
    highlight-parentheses
    ir-black-theme
    meson-mode
    monky
    powerline
    projectile
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
