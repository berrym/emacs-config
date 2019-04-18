;;; Commentary:
;;;init.el --- GNU Emacs Configuration

;; Copyright (c) 2019 Michael Berry

;; Author: Michael Berry <trismegustis@gmail.com>
;; URL: https://bitbucket.org/berrym/emacs-config

;; This file is not part of GNU Emacs.

;; License: GPLv3

;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

;;; Code:
(when (version< emacs-version "25.1")
  (error "This configuration only works with GNU Emacs >= 25.1"))

(defconst modules-dir (expand-file-name "modules" user-emacs-directory)
  "Directory for modules altering the standard Emacs environment.")

(add-to-list 'load-path modules-dir)

(require 'berrym-core)
(require 'berrym-packages)
(require 'berrym-editor)
(require 'berrym-global-keybindings)
(require 'berrym-ui)

;;; init.el ends here
