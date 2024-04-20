;;; Commentary:
;;;berrym-ui.el --- User Interface Configuration

;; Copyright (c) 2024 Michael Berry

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
                      :foreground "gray60")
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
  ; use emacs 29 new pixel based scrolling
  (setq pixel-scroll-precision-mode t)
  (setq pixel-scroll-precision-use-momentum t)
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

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(use-package "startup"
  :custom
  (initial-scratch-message "")
  (inhibit-startup-screen t)
  (initial-major-mode 'fundamental-mode))

(use-package linum
  :delight
  :bind
  ("<f5>" . linum-mode))

(use-package volatile-highlights
  :straight t
  :delight
  :hook
  (after-init . volatile-highlights-mode))

(use-package vi-tilde-fringe
  :straight t
  :demand t
  :delight
  :init
  (global-vi-tilde-fringe-mode))

(use-package minimap
  :straight t
  :delight
  :bind
  ("<f6>" . minimap-mode))

(use-package doom-modeline
  :straight t
  :demand t
  :delight
  :config
  ;; If non-nil, cause imenu to see `doom-modeline' declarations.
  ;; This is done by adjusting `lisp-imenu-generic-expression' to
  ;; include support for finding `doom-modeline-def-*' forms.
  ;; Must be set before loading doom-modeline.
  (setq doom-modeline-support-imenu t)

  ;; How tall the mode-line should be. It's only respected in GUI.
  ;; If the actual char height is larger, it respects the actual height.
  (setq doom-modeline-height 25)

  ;; How wide the mode-line bar should be. It's only respected in GUI.
  (setq doom-modeline-bar-width 4)

  ;; Whether to use hud instead of default bar. It's only respected in GUI.
  (setq doom-modeline-hud t)

  ;; The limit of the window width.
  ;; If `window-width' is smaller than the limit, some information won't be
  ;; displayed. It can be an integer or a float number. `nil' means no limit."
  (setq doom-modeline-window-width-limit 79)

  ;; How to detect the project root.
  ;; nil means to use `default-directory'.
  ;; The project management packages have some issues on detecting project root.
  ;; e.g. `projectile' doesn't handle symlink folders well, while `project' is unable
  ;; to hanle sub-projects.
  ;; You can specify one if you encounter the issue.
  (setq doom-modeline-project-detection 'auto)

  ;; Determines the style used by `doom-modeline-buffer-file-name'.
  ;;
  ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   auto => emacs/l/comint.el (in a project) or comint.el
  ;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
  ;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
  ;;   truncate-with-project => emacs/l/comint.el
  ;;   truncate-except-project => ~/P/F/emacs/l/comint.el
  ;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
  ;;   truncate-all => ~/P/F/e/l/comint.el
  ;;   truncate-nil => ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   relative-from-project => emacs/lisp/comint.el
  ;;   relative-to-project => lisp/comint.el
  ;;   file-name => comint.el
  ;;   buffer-name => comint.el<2> (uniquify buffer name)
  ;;
  ;; If you are experiencing the laggy issue, especially while editing remote files
  ;; with tramp, please try `file-name' style.
  ;; Please refer to https://github.com/bbatsov/projectile/issues/657.
  (setq doom-modeline-buffer-file-name-style 'auto)

  ;; Whether display icons in the mode-line.
  ;; While using the server mode in GUI, should set the value explicitly.
  (setq doom-modeline-icon t)

  ;; Whether display the icon for `major-mode'. It respects `doom-modeline-icon'.
  (setq doom-modeline-major-mode-icon t)

  ;; Whether display the colorful icon for `major-mode'.
  ;; It respects `nerdg-icons-color-icons'.
  (setq doom-modeline-major-mode-color-icon t)

  ;; Whether display the icon for the buffer state. It respects `doom-modeline-icon'.
  (setq doom-modeline-buffer-state-icon t)

  ;; Whether display the modification icon for the buffer.
  ;; It respects `doom-modeline-icon' and `doom-modeline-buffer-state-icon'.
  (setq doom-modeline-buffer-modification-icon t)

  ;; Whether display the time icon. It respects variable `doom-modeline-icon'.
  (setq doom-modeline-time-icon t)

  ;; Whether to use unicode as a fallback (instead of ASCII) when not using icons.
  (setq doom-modeline-unicode-fallback nil)

  ;; Whether display the buffer name.
  (setq doom-modeline-buffer-name t)

  ;; Whether highlight the modified buffer name.
  (setq doom-modeline-highlight-modified-buffer-name t)

  ;; Whether display the minor modes in the mode-line.
  (setq doom-modeline-minor-modes nil)

  ;; If non-nil, a word count will be added to the selection-info modeline segment.
  (setq doom-modeline-enable-word-count nil)

  ;; Major modes in which to display word count continuously.
  ;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
  ;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
  ;; remove the modes from `doom-modeline-continuous-word-count-modes'.
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))

  ;; Whether display the buffer encoding.
  (setq doom-modeline-buffer-encoding t)

  ;; Whether display the indentation information.
  (setq doom-modeline-indent-info nil)

  ;; If non-nil, only display one number for checker information if applicable.
  (setq doom-modeline-checker-simple-format t)

  ;; The maximum number displayed for notifications.
  (setq doom-modeline-number-limit 99)

  ;; The maximum displayed length of the branch name of version control.
  (setq doom-modeline-vcs-max-length 12)

  ;; Whether display the workspace name. Non-nil to display in the mode-line.
  (setq doom-modeline-workspace-name t)

  ;; Whether display the perspective name. Non-nil to display in the mode-line.
  (setq doom-modeline-persp-name t)

  ;; If non nil the default perspective name is displayed in the mode-line.
  (setq doom-modeline-display-default-persp-name nil)

  ;; If non nil the perspective name is displayed alongside a folder icon.
  (setq doom-modeline-persp-icon t)

  ;; Whether display the `lsp' state. Non-nil to display in the mode-line.
  (setq doom-modeline-lsp t)

  ;; Whether display the GitHub notifications. It requires `ghub' package.
  (setq doom-modeline-github nil)

  ;; The interval of checking GitHub.
  (setq doom-modeline-github-interval (* 30 60))

  ;; Whether display the modal state.
  ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
  (setq doom-modeline-modal nil)

  ;; Whether display the modal state icon.
  ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
  (setq doom-modeline-modal-icon t)

  ;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
  ;; (setq doom-modeline-mu4e nil)
  ;; ;; also enable the start of mu4e-alert
  ;; (mu4e-alert-enable-mode-line-display)

  ;; Whether display the gnus notifications.
  (setq doom-modeline-gnus nil)

  ;; Whether gnus should automatically be updated and how often (set to 0 or smaller than 0 to disable)
  (setq doom-modeline-gnus-timer nil)

  ;; Wheter groups should be excludede when gnus automatically being updated.
  (setq doom-modeline-gnus-excluded-groups '("dummy.group"))

  ;; Whether display the IRC notifications. It requires `circe' or `erc' package.
  (setq doom-modeline-irc nil)

  ;; Function to stylize the irc buffer names.
  (setq doom-modeline-irc-stylize 'identity)

  ;; Whether display the battery status. It respects `display-battery-mode'.
  (setq doom-modeline-battery t)

  ;; Whether display the time. It respects `display-time-mode'.
  (setq doom-modeline-time t)

  ;; Whether display the misc segment on all mode lines.
  ;; If nil, display only if the mode line is active.
  (setq doom-modeline-display-misc-in-all-mode-lines t)

  ;; Whether display the environment version.
  (setq doom-modeline-env-version t)
  ;; Or for individual languages
  (setq doom-modeline-env-enable-python t)
  (setq doom-modeline-env-enable-ruby t)
  (setq doom-modeline-env-enable-perl t)
  (setq doom-modeline-env-enable-go t)
  (setq doom-modeline-env-enable-elixir t)
  (setq doom-modeline-env-enable-rust t)

  ;; Change the executables to use for the language version string
  (setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
  (setq doom-modeline-env-ruby-executable "ruby")
  (setq doom-modeline-env-perl-executable "perl")
  (setq doom-modeline-env-go-executable "go")
  (setq doom-modeline-env-elixir-executable "iex")
  (setq doom-modeline-env-rust-executable "rustc")

  ;; What to display as the version while a new one is being loaded
  (setq doom-modeline-env-load-string "...")

  ;; By default, almost all segments are displayed only in the active window. To
  ;; display such segments in all windows, specify e.g.
  (setq doom-modeline-always-visible-segments '(mu4e irc))

  ;; Hooks that run before/after the modeline version string is updated
  (setq doom-modeline-before-update-env-hook nil)
  (setq doom-modeline-after-update-env-hook nil)
  :init
  (doom-modeline-mode t))

(when (display-graphic-p) ; Change font, and use a dashboard
  (use-package fontaine
    :straight t
    :ensure t
    :delight
    :custom
    (fontaine-presets
     `((regular
        :default-weight regular
        :default-height 110
        :variable-pitch-family "JetBrainsMono Nerd Font"
        :bold-weight bold)
       (presentation
        :default-height 160)
       (t ;; defaults
        :default-family
        ,(cond
          ((find-font (font-spec :name "JetBrainsMono Mono Nerd Font"))
           "Regular")
          ("Monospace")))))
  :config
  (fontaine-set-preset (or fontaine-current-preset 'regular)))

  (use-package dashboard
    :straight t
    :ensure t
    :delight
    :init
    (dashboard-setup-startup-hook)
    :config
    (setq dashboard-items '((recents . 5)
                            (projects . 5)
                            ;; (bookmarks . 5)
                            (agenda . 10))
          dashboard-banner-logo-title "Welcome to Lusus Naturae's Emacs\nHere be Dragons!"
          dashboard-startup-banner (expand-file-name "images/ancient_mage.gif" user-emacs-directory) ;;'logo
          dashboard-set-navigator t
          dashboard-center-content t
          dashboard-set-heading-icons t
          dashboard-heading-icons '((recents   . "history")
                                    (bookmarks . "bookmark")
                                    (agenda    . "calendar")
                                    (projects  . "rocket")
                                    (registers . "database"))
          dashboard-icon-type 'all-the-icons
          dashboard-set-file-icons t
          dashboard-projects-backend 'projectile))

  ;; (use-package powerline
  ;;   :straight t
  ;;   :ensure t
  ;;   :delight
  ;;   :config
  ;;   (powerline-default-theme))

  (use-package doom-themes
    :straight t
    :ensure t
    :demand t
    :config
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
          doom-themes-enable-italic t) ; if nil, italics is universally disabled
    (load-theme 'doom-Iosvkem t)

    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)
    ;; Enable custom neotree theme (all-the-icons must be installed!)
    (doom-themes-neotree-config)
    ;; or for treemacs users
    (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-atom" for a more minimal icon theme
    (doom-themes-treemacs-config)
    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config)))

(message "berrym-ui: module loaded successfully.")

(provide 'berrym-ui)

;;; berrym-ui.el ends here
