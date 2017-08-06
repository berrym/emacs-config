;;; berrym-ui.el --- User Interface Configuration
;;
;; Copyright (c) 2013-2017 Michael Berry

(global-undo-tree-mode t)
(diminish 'undo-tree-mode)

(require 'auto-complete-config)
(ac-config-default)
(diminish 'auto-complete-mode)
(setq ac-comphist-file (expand-file-name "ac-comphist.dat" *save-files-dir*))

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

(require 'helm)
(require 'helm-config)
(helm-mode t)
(diminish 'helm-mode)

(projectile-global-mode t)
(setq projectile-known-projects-file
      (expand-file-name "projectile-bookmarks.eld" *save-files-dir*))
(diminish 'projectile-mode)

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

;; use spaceline powerline config
(if (display-graphic-p)
    (progn
      (setq powerline-default-separator 'utf-8)
      (require 'spaceline-config)
      (spaceline-spacemacs-theme)
      (spaceline-helm-mode t)
      (spaceline-info-mode t)
      (spaceline-toggle-hud-on)
      (spaceline-toggle-version-control-on)
      (spaceline-toggle-window-number-on)))

(require 'python-mode)

(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; enable recent files
(recentf-mode t)

;; disable beep
(setq visible-bell t)

;; No text in scratch buffer
(setq initial-scratch-message "")

(setq-default
 frame-title-format
 '(:eval
   (format "%s@%s:%s"
           (or (file-remote-p default-directory 'user) user-login-name)
           (or (file-remote-p default-directory 'host) system-name)
           (file-name-nondirectory (or (buffer-file-name) default-directory)))))

;; disable startup screen
(setq inhibit-startup-screen t)

;; load a color theme
(if (display-graphic-p)
    (load-theme 'spacemacs-dark t)
  (load-theme 'ir-black t))

;; pretty lambdas
(pretty-lambda-for-modes)

(message "berrym-ui: module loaded successfully.")

(provide 'berrym-ui)

;;; berrym-ui.el ends here
