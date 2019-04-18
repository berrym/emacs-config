;;; Commentary:
;;;berrym-global-keybindings.el --- Global keyboard shortcuts.

;; Copyright (c) 2019 Michael Berry

;; Author: Michael Berry <trismegustis@gmail.com>
;; URL: https://bitbucket.org/berrym/emacs-config

;; This file is not part of GNU Emacs.

;; License: GPLv3

;;; Code:
(global-set-key (kbd "M-s") 'repeat-last-search-forward)
(global-set-key (kbd "M-r") 'repeat-last-search-backward)
(global-set-key (kbd "C-S-k") 'backward-kill-line)
(global-set-key (kbd "<f5>") 'linum-mode)
(global-set-key (kbd "<f8>") 'neotree-toggle)
(global-set-key (kbd "<f11>") 'package-show-package-list)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

(message "berrym-global-keybindings: module loaded successfully.")

(provide 'berrym-global-keybindings)

;;; berrym-global-keybindings.el ends here
