;;; Commentary:
;;;berrym-global-keybindings.el --- Global keyboard shortcuts.

;; Copyright (c) 2024 Michael Berry

;; Author: Michael Berry <trismegustis@gmail.com>
;; URL: https://github.com/berrym/emacs-config

;; This file is not part of GNU Emacs.
;; License: GPLv3
;;; Code:

(global-set-key (kbd "M-s")   #'repeat-last-search-forward)
(global-set-key (kbd "M-r")   #'repeat-last-search-backward)
(global-set-key (kbd "C-M-k") #'backward-kill-line)
(global-set-key (kbd "C-S-d") #'duplicate-line)
(global-set-key (kbd "C-S-j") #'transpose-line-down)
(global-set-key (kbd "C-S-k") #'transpose-line-up)
(global-set-key (kbd "C-x k") #'kill-this-buffer)
(global-set-key (kbd "<f11>") #'package-show-package-list)

(message "berrym-global-keybindings: module loaded successfully.")

(provide 'berrym-global-keybindings)

;;; berrym-global-keybindings.el ends here
