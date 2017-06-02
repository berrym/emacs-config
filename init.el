;;; init.el --- GNU Emacs Configuration
;;
;; Copyright (c) 2013-2015 Michael Berry


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(when (version< emacs-version "24.1")
  (error "This configuration only works with GNU Emacs >= 24.1"))

(defvar *modules-dir* (expand-file-name "modules" user-emacs-directory)
  "Directory for modules altering the standard emacs environment.")

(defvar *save-files-dir* (expand-file-name "save-files" user-emacs-directory)
  "Directory for storing autosave and backup files.")
(unless (file-exists-p *save-files-dir*)
  (make-directory *save-files-dir*))
(setq backup-directory-alist `(("." . , *save-files-dir*)))
(setq auto-save-list-file-prefix (expand-file-name "saves-" *save-files-dir*))
(setq auto-save-file-name-transforms `(("." ,*save-files-dir* t)))
(setq tramp-auto-save-directory (file-name-as-directory *save-files-dir*))
(setq custom-file (expand-file-name "custom.el" (file-name-as-directory
						 *save-files-dir*)))
(if (file-exists-p custom-file)
    (load custom-file))

(add-to-list 'load-path *modules-dir*)
(require 'berrym-packages)
(require 'berrym-ui)
(require 'berrym-editor)
(require 'berrym-global-keybindings)

(unless (dired-nondirectory-p "personal-lisp")
    (mapc 'load (directory-files "personal-lisp" 't "^[^#].*el[^~]*$")))

;;; init.el ends here
