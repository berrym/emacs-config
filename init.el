;;; init.el --- GNU Emacs Configuration
;;
;; Copyright (c) 2013-2018 Michael Berry
;;
;; Author: Michael Berry <trismegustis@gmail.com>
;; URL: https://bitbucket.org/berrym/emacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

(when (version< emacs-version "24.4")
  (error "This configuration only works with GNU Emacs >= 24.4"))

(defvar *modules-dir* (expand-file-name "modules" user-emacs-directory)
  "Directory for modules altering the standard emacs environment.")

(add-to-list 'load-path *modules-dir*)

(require 'berrym-core)
(require 'berrym-packages)
(require 'berrym-editor)
(require 'berrym-global-keybindings)
(require 'berrym-ui)

;;; init.el ends here
