;;; Commentary:
;;;early-init.el --- Early settings before emacs is loaded

;; Copyright (c) 2023 Michael Berry

;; Author: Michael Berry <trismegustis@gmail.com>
;; URL: https://github.com/berrym/emacs-config

;; This file is not part of GNU Emacs.

;; License: GPLv3

;;; Code:

(setq package-enable-at-startup nil)
(setq package-quickstart nil)

(push '(menu-bar-lines . nil) default-frame-alist)
(push '(tool-bar-lines . nil) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

(setq frame-inhibit-implied-resize t)

(setq comp-deferred-compilation nil)

(setq inhibit-compacting-font-caches t)

(setq byte-compile-warnings 'nil)

;;; early-init.el ends here
