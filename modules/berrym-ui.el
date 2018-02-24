;;; berrym-ui.el --- User Interface Configuration
;;
;; Copyright (c) 2013-2018 Michael Berry
;;
;; Author: Michael Berry <trismegustis@gmail.com>
;; URL: https://bitbucket.org/berrym/emacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; fancy parenthesis matching
(defadvice show-paren-function
  (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
echo area. Has no effect if the character before point is not of
the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
	 (matching-text (and cb
			     (char-equal (char-syntax cb) ?\) )
			     (blink-matching-open))))
    (when matching-text (message matching-text))))

(show-paren-mode t)

(define-globalized-minor-mode
  global-highlight-parentheses-mode highlight-parentheses-mode
  (lambda () (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)
(diminish 'highlight-parentheses-mode)

;; mode line settings
(unless (display-graphic-p)
  (progn
    (line-number-mode t)
    (column-number-mode t)
    (size-indication-mode t)))

;; shrink the fringe but don't disable it
(when (fboundp 'fringe-mode)
       (fringe-mode 4))

;; disable some gui components
(if (display-graphic-p)
    (mapc
     (lambda (mode)
       (when (fboundp mode)
	 (funcall mode 0)))
     '(
       tool-bar-mode
       menu-bar-mode
       ;; scroll-bar-mode
       ;; fringe-mode
       blink-cursor-mode
       )))

;; cleanup whitespace
(require 'whitespace-cleanup-mode)
(diminish 'whitespace-cleanup-mode)

;; undo tree
(global-undo-tree-mode t)
(diminish 'undo-tree-mode)

;; show number of search matches
(require 'anzu)
(global-anzu-mode t)
(diminish 'anzu-mode)

;; auto completion
(require 'auto-complete-config)
(ac-config-default)
(diminish 'auto-complete-mode)
(setq ac-comphist-file (expand-file-name "ac-comphist.dat" *save-files-dir*))

;; enhanced menu navigation
(require 'helm)
(require 'helm-config)
(diminish 'helm-mode)

;; project tools
(projectile-global-mode t)
(setq projectile-known-projects-file
      (expand-file-name "projectile-bookmarks.eld" *save-files-dir*))
(diminish 'projectile-mode)

;; use spaceline powerline config
(if (display-graphic-p)
    (progn
      (require 'spaceline-config)
      (spaceline-spacemacs-theme)
      (spaceline-helm-mode t)
      (spaceline-info-mode t)))

(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

;; vim nerdtree like package
(require 'neotree)

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
  "Check if a font exists. Return t if found nill if not."
  (if (null (x-list-fonts font))
      nil
    t))

;; only change the font (if it exists) if in grpahical mode
(if (display-graphic-p)
    (progn
      (if (font-exists-p "Source Code Pro")
	  (progn
	    (set-frame-font "Source Code Pro 11")
	    (message "Using Source Code Pro font."))
	(message "Source Code Pro font not found. Using default font.")))
  (message "Using default font."))

(load-theme 'spacemacs-dark t)

(message "berrym-ui: module loaded successfully.")

(provide 'berrym-ui)

;;; berrym-ui.el ends here
