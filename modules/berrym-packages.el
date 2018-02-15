;;; berrym-pacakges.el --- Required packages for my sanity.
;;
;; Copyright (c) 2013-2017 Michael Berry

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(setq package-user-dir (expand-file-name "packages" user-emacs-directory))
(package-initialize)

(defvar *required-packages*
  '(anzu
    auto-complete
    diminish
    elpy
    expand-region
    flycheck
    google-this
    haskell-mode
    helm
    helm-projectile
    highlight-parentheses
    ir-black-theme
    jedi
    powerline
    python-mode
    slime
    solarized-theme
    spacemacs-theme
    spaceline
    undo-tree
    volatile-highlights
    whitespace-cleanup-mode
    zenburn-theme)
  "A list of required packages for this configuration.")

(defvar *packages-needing-installed* ()
  "A list of required packages that need to be installed.")

(defun check-required-packages-are-installed ()
  "Check that all required packages are installed."
  (mapc
   (lambda (package)
     (or (package-installed-p package)
	 (add-to-list '*packages-needing-installed* package)))
   *required-packages*))

(defun install-packages-needing-installed ()
  "Install all required packages not yet installed."
  (package-refresh-contents)
  (mapc
   (lambda (package)
     (message "berrym-packages: Trying to install package %s..." package)
     (package-install package))
   *packages-needing-installed*))

(check-required-packages-are-installed)

(if (not *packages-needing-installed*)
    (message "berrym-packages: All required packages are installed.")
  (progn
    (message "berrym-packages: Some packages need installed...")
    (install-packages-needing-installed)))

(message "berrym-packages: module loaded successfully.")

(provide 'berrym-packages)

;;; berrym-pacakges.el ends here
