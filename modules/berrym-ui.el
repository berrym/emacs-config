;;; Commentary:
;;;berrym-ui.el --- User Interface Configuration

;; Copyright (c) 2021 Michael Berry

;; Author: Michael Berry <trismegustis@gmail.com>
;; URL: https://github.com/berrym/emacs-config

;; This file is not part of GNU Emacs.

;; License: GPLv3

;;; Code:

(defconst save-files-dir (expand-file-name "save-files" user-emacs-directory)
  "Directory for storing autosave and backup files.")

(defadvice show-paren-function
  (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the echo area.
Has no effect if the character before point is not of the syntax class ')'."
  (interactive)
    (let* ((cb (char-before (point)))
	   (matching-text
	    (and cb
		 (char-equal (char-syntax cb) ?\) )
		 (blink-matching-open))))))

(show-paren-mode t)

(defvar highlight-parentheses-mode)
(define-globalized-minor-mode
  global-highlight-parentheses-mode highlight-parentheses-mode
  (lambda () (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)
(diminish 'highlight-parentheses-mode)

;; mode line settings
(unless (window-system)
  (progn
    (line-number-mode t)
    (column-number-mode t)
    (size-indication-mode t)))

;; disable some ui components
(when (window-system)
  (mapc
   (lambda (mode)
     (when (fboundp mode)
       (funcall mode 0)))
   '(tool-bar-mode
     menu-bar-mode
     scroll-bar-mode
     ;; fringe-mode
     blink-cursor-mode)))

(use-package volatile-highlights
  :diminish
  :hook
  (after-init . volatile-highlights-mode))

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; enable recent files
(recentf-mode t)

;; disable beep
(setq visible-bell t)

;; no text in scratch buffer
(setq initial-scratch-message "")

;; set the frame title format
(setq-default
 frame-title-format
 '(:eval
   (format "%s@%s:%s"
           (or (file-remote-p default-directory 'user) user-login-name)
           (or (file-remote-p default-directory 'host) system-name)
           (file-name-nondirectory (or (buffer-file-name) default-directory)))))

;; disable startup screen
(setq inhibit-startup-screen t)

;; set default font and color theme
(defun font-exists-p (font)
  "Check if a FONT exists.  Return T if found NIL if not."
  (if (null (x-list-fonts font)) nil t))

(use-package vi-tilde-fringe
  :diminish
  :init
  (global-vi-tilde-fringe-mode))

(use-package linum
  :diminish
  :init
  (global-linum-mode))

(use-package minimap
  :diminish
  :init
  (minimap-mode))

;; change font, change frame size, load a color theme
(when (window-system)
  (progn
    (if (font-exists-p "Source Code Pro 10")
	(progn
	  (set-frame-font "Source Code Pro 10")
	  (message "Using Source Code Pro font."))
      (message "Source Code Pro font not found.  Using default font."))
    (use-package fira-code-mode
      :diminish
      :custom
      (fira-code-mode-disabled-ligatures '())
      :hook
      (prog-mode))
    (use-package spaceline-config
      :diminish
      :hook
      ((after-init . spaceline-spacemacs-theme)
       (spaceline-helm-mode)
       (spaceline-info-mode)))
    (set-frame-size (selected-frame) 122 50)
    (load-theme 'solarized-gruvbox-dark t))) ;; whiteboard t)))

(message "berrym-ui: module loaded successfully.")

(provide 'berrym-ui)

;;; berrym-ui.el ends here
