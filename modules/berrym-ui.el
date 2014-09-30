;;; berrym-ui.el --- User Interface Configuration
;;
;; Copyright (c) 2013 Michael Berry

(global-undo-tree-mode t)
(diminish 'undo-tree-mode)
(require 'auto-complete-config)
(ac-config-default)
(diminish 'auto-complete-mode)
(setq ac-comphist-file (expand-file-name "ac-comphist.dat" save-files-dir))
(define-globalized-minor-mode
  global-highlight-parentheses-mode highlight-parentheses-mode
  (lambda () (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)
(diminish 'highlight-parentheses-mode)
(ido-mode t)
(ido-ubiquitous-mode t)
(flx-ido-mode t)
(setq ido-faces nil)
(helm-mode t)
(diminish 'helm-mode)
(projectile-global-mode t)
(setq projectile-known-projects-file
      (expand-file-name "projectile-bookmarks.eld" save-files-dir))
(diminish 'projectile-mode)
(show-paren-mode t)
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
(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)
(require 'whitespace-cleanup-mode)
(diminish 'whitespace-cleanup-mode)

;; shrink the fringe but don't disable it
(when (fboundp 'fringe-mode)
  (fringe-mode 4))

(mapc
 (lambda (mode)
   (when (fboundp mode)
     (funcall mode 0)))
 '(
   ;; tool-bar-mode
   ;; menu-bar-mode
   ;; scroll-bar-mode
   ;; fringe-mode
   blink-cursor-mode
   ))

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; enable recent files
(recentf-mode t)

;; disable beep
(setq visible-bell t)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

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
(load-theme 'whiteboard t)

;; pretty lambdas
(pretty-lambda-for-modes)

(message "berrym-ui: module loaded successfully.")

(provide 'berrym-ui)

;;; berrym-ui.el ends here
