;;; Commentary:
;;;init.el --- GNU Emacs Configuration

;; Copyright (c) 2023 Michael Berry

;; Author: Michael Berry <trismegustis@gmail.com>
;; URL: https://github.com/berrym/emacs-config

;; This file is not part of GNU Emacs.

;; License: GPLv3

;;; Code:

(when (version< emacs-version "29.0")
  (error "This configuration only works with GNU Emacs >= 29.0"))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(defconst save-files-dir (expand-file-name "save-files" user-emacs-directory)
  "Directory for storing autosave and backup files.")

(defconst berrym-modules-dir (expand-file-name "modules" user-emacs-directory)
  "Directory for modules altering the standard Emacs environment.")

(add-to-list 'load-path berrym-modules-dir)

(require 'berrym-core)
(require 'berrym-misc-funcs)
(require 'berrym-packages)
(require 'berrym-editor)
(require 'berrym-global-keybindings)
(require 'berrym-ui)

(require 'server)
(if (not (server-running-p)) (server-start))

;;; init.el ends here
