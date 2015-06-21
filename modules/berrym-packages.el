;;; berrym-pacakges.el --- Required packages for my sanity.
;;
;; Copyright (c) 2013-2015 Michael Berry

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(setq package-user-dir (expand-file-name "packages" user-emacs-directory))
(package-initialize)

(defvar required-packages
  '(auto-complete
    diminish
    elpy
    expand-region
    flx-ido
    flycheck
    google-this
    haskell-mode
    helm
    highlight-parentheses
    ido-ubiquitous
    ir-black-theme
    jedi
    pretty-lambdada
    projectile
    slime
    solarized-theme
    undo-tree
    volatile-highlights
    whitespace-cleanup-mode
    zenburn-theme)
  "A list of required packages for this configuration.")

(defvar packages-needing-installed ()
  "A list of required packages that need to be installed.")

(defun check-required-packages-are-installed ()
  "Check that all required packages are installed."
  (mapc
   (lambda (package)
     (or (package-installed-p package)
	 (add-to-list 'packages-needing-installed package)))
   required-packages))

(defun install-packages-needing-installed ()
  "Install all required packages not yet installed."
  (package-refresh-contents)
  (mapc
   (lambda (package)
     (message "berrym-packages: Trying to install package %s..." package)
     (package-install package))
   packages-needing-installed))

(defun berrym-package-menu ()
  "Get/Create a buffer then load the package menu and refresh it's contents."
  (interactive)
  (message "berrym-packages: switching to package-menu-buffer.")
  (let ((package-menu-buffer (get-buffer-create "package-menu-buffer")))
    (switch-to-buffer package-menu-buffer)
    (package-menu-mode)
    (package-menu-refresh)))

(check-required-packages-are-installed)

(if (not packages-needing-installed)
    (message "berrym-packages: All required packages are installed.")
  (progn
    (message "berrym-packages: Some packages need installed...")
    (install-packages-needing-installed)))

(message "berrym-packages: module loaded successfully.")

(provide 'berrym-packages)

;;; berrym-pacakges.el ends here
