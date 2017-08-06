;;; berrym-global-keybindings.el --- Global keyboard shortucuts.
;;
;; Copyright (c) 2013-2017 Michael Berry

(global-set-key (kbd "M-s") 'repeat-last-search-forward)
(global-set-key (kbd "M-r") 'repeat-last-search-backward)
(global-set-key (kbd "C-c '") 'backward-kill-line)
(global-set-key (kbd "<f5>") 'linum-mode)
(global-set-key (kbd "<f8>") 'whitespace-mode)
(global-set-key (kbd "<f11>") 'package-show-package-list)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-=") 'er/expand-region)

(message "berrym-global-keybindings: module loaded successfully.")

(provide 'berrym-global-keybindings)

;;; berrym-global-keybindings.el ends here
