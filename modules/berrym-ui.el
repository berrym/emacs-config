;;; Commentary:
;;;berrym-ui.el --- User Interface Configuration

;; Copyright (c) 2019 Michael Berry

;; Author: Michael Berry <trismegustis@gmail.com>
;; URL: https://bitbucket.org/berrym/emacs-config

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
(unless (display-graphic-p)
  (progn
    (line-number-mode t)
    (column-number-mode t)
    (size-indication-mode t)))

;; shrink the fringe but don't disable it
(when (fboundp 'fringe-mode)
       (fringe-mode 4))

;; disable some gui components
;;(if (display-graphic-p)
(mapc
 (lambda (mode)
   (when (fboundp mode)
     (funcall mode 0)))
 '(tool-bar-mode
   menu-bar-mode
   ;; scroll-bar-mode
   ;; fringe-mode
   blink-cursor-mode
   )) ;;)

;; cleanup whitespace
(require 'whitespace-cleanup-mode)
(diminish 'whitespace-cleanup-mode)

;; undo tree
(require 'undo-tree)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;; show number of search matches
(require 'anzu)
(global-anzu-mode t)
(diminish 'anzu-mode)

;; auto completion
(ac-config-default)
(diminish 'auto-complete-mode)
(setq ac-comphist-file
      (expand-file-name "ac-comphist.dat" save-files-dir))

(require 'company)
(global-company-mode)
(diminish 'global-company-mode)

;; enhanced menu navigation
(require 'helm-config)
(helm-mode)
(diminish 'helm-mode)

(require 'projectile)
(projectile-mode t)
(setq projectile-known-projects-file
      (expand-file-name "projectile-bookmarks.eld" save-files-dir))
(diminish 'projectile-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(require 'helm-projectile)
(helm-projectile-on)

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
  (if (null (x-list-fonts font))
      nil
    t))

;; only change the font (if it exists) if in grpahical mode
(if (display-graphic-p)
   (progn
     (if (font-exists-p "Fira Code 10")
	  (progn
	    (set-frame-font "Fira Code 10")
	    (message "Using Fira Code font."))
	(message "Fira Code Variable font not found.  Using default font.")))
 (message "Using default font."))

(require 'use-package)
(use-package composite
	     :defer t
	     :init
	     (defvar composition-ligature-table (make-char-table nil))
	     :hook
	     (((prog-mode conf-mode nxml-mode markdown-mode help-mode)
	       . (lambda () (setq-local composition-function-table composition-ligature-table))))
	     :config
	     ;; support ligatures, some toned down to prevent hang
	     (when (version<= "27.0" emacs-version)
	       (let ((alist
		      '((33 . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
			(35 . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
			(36 . ".\\(?:\\(>\\)>?\\)")
			(37 . ".\\(?:\\(%\\)%?\\)")
			(38 . ".\\(?:\\(&\\)&?\\)")
			(42 . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
			;; (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
			(43 . ".\\(?:\\([>]\\)>?\\)")
			;; (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
			(45 . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
			;; (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
			(46 . ".\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
			(47 . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
			;; (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
			(48 . ".\\(?:\\(x[a-fA-F0-9]\\).?\\)")
			(58 . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
			(59 . ".\\(?:\\(;\\);?\\)")
			(60 . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
			(61 . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
			(62 . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
			(63 . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
			(91 . ".\\(?:\\(|\\)[]|]?\\)")
			;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
			(94 . ".\\(?:\\(=\\)=?\\)")
			(95 . ".\\(?:\\(|_\\|[_]\\)_?\\)")
			(119 . ".\\(?:\\(ww\\)w?\\)")
			(123 . ".\\(?:\\(|\\)[|}]?\\)")
			(124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
			(126 . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))))
		 (dolist (char-regexp alist)
		   (set-char-table-range composition-ligature-table (car char-regexp)
					 `([,(cdr char-regexp) 0 font-shape-gstring]))))
	       (set-char-table-parent composition-ligature-table composition-function-table))
	     )

(load-theme 'spacemacs-dark t)
;; (load-theme 'solarized-dark t)
;; (load-theme 'ir-black t)
;; (load-theme 'zenburn t)

(message "berrym-ui: module loaded successfully.")

(provide 'berrym-ui)

;;; berrym-ui.el ends here
