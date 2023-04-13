;;; Commentary:
;;;berrym-ui.el --- User Interface Configuration

;; Copyright (c) 2023 Michael Berry

;; Author: Michael Berry <trismegustis@gmail.com>
;; URL: https://github.com/berrym/emacs-config

;; This file is not part of GNU Emacs.

;; License: GPLv3

;;; Code:

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

;; mode line settings
(unless (window-system)
  (progn
    (line-number-mode t)
    (column-number-mode t)
    (size-indication-mode t)))

;; disable some ui components
;; (when (window-system)
;;   (mapc
;;    (lambda (mode)
;;      (when (fboundp mode)
;;        (funcall mode 0)))
;;    '(tool-bar-mode
;;      menu-bar-mode
;;      scroll-bar-mode)))
     ;; fringe-mode
     ;; blink-cursor-mode)))

(use-package emacs
  :config
  ;; Configure tab-bar-mode
  (set-face-attribute 'tab-bar nil
                      :inherit 'mode-line)
  (set-face-attribute 'tab-bar-tab nil
                      :inherit 'mode-line)
  (set-face-attribute 'tab-bar-tab-inactive nil
                      :inherit 'mode-line)
  (setq tab-bar-show t
        tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-bar-new-tab-choice "*scratch*"
        tab-bar-close-tab-select 'recent
        tab-bar-new-tab-to 'right
        tab-bar-tab-hints nil
        tab-bar-separator " "
        tab-bar-menu-bar-button "𝜦"
        tab-bar-format '(tab-bar-format-menu-bar tab-bar-format-tabs tab-bar-separator
                                                 tab-bar-format-align-right
                                                 tab-bar-format-global))
  :custom
  (cursor-type 'bar)
  (visible-bell t)
  (recentf-mode t)
  (frame-inhibit-implied-resize t)
  (tab-bar-mode t)
  (global-tab-line-mode nil)
  (tab-line-mode nil))

(use-package frame
  :config
  (blink-cursor-mode -1)
  (set-frame-size (selected-frame) 122 50)
  (setq-default
   frame-title-format
   '(:eval
     (format "%s@%s:%s"
             (or (file-remote-p default-directory 'user) user-login-name)
             (or (file-remote-p default-directory 'host) system-name)
             (file-name-nondirectory (or (buffer-file-name) default-directory))))))

(use-package scroll-bar
  :config
  (scroll-bar-mode -1))

(use-package tool-bar
  :config
  (tool-bar-mode -1))

(use-package menu-bar
  :config
  (menu-bar-mode -1))

(setq use-dialog-box 0)

(use-package volatile-highlights
  :straight t
  :delight
  :hook
  (after-init . volatile-highlights-mode))

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(use-package "startup"
  :custom
  (initial-scratch-message "")
  (inhibit-startup-screen t)
  (initial-major-mode 'fundamental-mode))

(use-package vi-tilde-fringe
  :straight t
  :delight
  :init
  (global-vi-tilde-fringe-mode))

(use-package linum
  :delight
  :bind
  ("<f5>" . linum-mode))

(use-package minimap
  :straight t
  :delight
  :bind
  ("<f6>" . minimap-mode))

(when (window-system)                   ; Change font, and use a dashboard when using window system
  (use-package fontaine
    :straight t
    :delight
    :custom
    (fontaine-presets
     `((regular
        :default-weight semilight
        :default-height 110
        :variable-pitch-family "Sans"
        :bold-weight extrabold)
       (presentation
        :default-height 160)
       (t ;; defaults
        :default-family
        ,(cond
          ((find-font (font-spec :name "Fira Code"))
           "Fira Code Retina")
          ("Monospace")))))
  :config
  (fontaine-set-preset (or fontaine-current-preset 'regular)))

  (use-package dashboard
    :straight t
    :delight
    :config
    (dashboard-setup-startup-hook)
    (setq dashboard-items '((recents . 5)
                            (projects . 15)
                            (bookmarks . 5)
                            (agenda . 10))
          dashboard-startup-banner 'logo
          dashboard-set-navigator t
          dashboard-center-content t
          dashboard-set-heading-icons t
          dashboard-set-file-icons t
          dashboard-projects-backend 'projectile)))

(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-xcode t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package powerline
  :straight t
  :delight
  :config
  (powerline-default-theme))

(message "berrym-ui: module loaded successfully.")

(provide 'berrym-ui)

;;; berrym-ui.el ends here
