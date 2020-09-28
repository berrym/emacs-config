;;; Commentary:
;;;init.el --- GNU Emacs Configuration

;; Copyright (c) 2020 Michael Berry

;; Author: Michael Berry <trismegustis@gmail.com>
;; URL: https://github.com/berrym/emacs-config

;; This file is not part of GNU Emacs.

;; License: GPLv3

;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

;;; Code:
(when (version< emacs-version "27.0")
  (error "This configuration only works with GNU Emacs >= 27.0"))

(defconst modules-dir (expand-file-name "modules" user-emacs-directory)
  "Directory for modules altering the standard Emacs environment.")

(add-to-list 'load-path modules-dir)

(require 'berrym-core)
(require 'berrym-packages)
(require 'berrym-editor)
(require 'berrym-global-keybindings)
(require 'berrym-ui)

(require 'server)
(if (not (server-running-p)) (server-start))

;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)
