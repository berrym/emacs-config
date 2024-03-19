;;; Commentary:
;;;berrym-editor.el --- Configure Editor Behaviors

;; Copyright (c) 2024 Michael Berry

;; Author: Michael Berry <trismegustis@gmail.com>
;; URL: https://github.com/berrym/emacs-config

;; This file is not part of GNU Emacs.

;; License: GPLv3

;;; Code:

;; Define a C programming style
(c-add-style "berrym"
             '((c-basic-offset                 . 4)
               (c-tab-always-indent            . t)
               (c-comment-only-line-offset     . 0)
               (c-echo-syntactic-information-p . t)
               (c-report-syntactic-errors      . t)
               (indent-tabs-mode               . nil)
               (case-fold-search               . nil)
               (c-hanging-braces-alist         . ((brace-list-open)
                                                  (brace-list-intro)
                                                  (brace-list-close)
                                                  (brace-entry-open)
                                                  (substatement-open after)
                                                  (block-close . c-snug-do-while)
                                                  (arglist-cont-nonempty)
                                                  (defun-open after)
                                                  (class-open after)
                                                  (class-close berfore after)
                                                  (inexpr-class-open after)
                                                  (inexpr-class-close before)
                                                  (namespace-open after)
                                                  (inline-open after)
                                                  (inline-close before after)
                                                  (block-open after)
                                                  (extern-lang-open after)
                                                  (extern-lang-close after)
                                                  (statement-case-open after)))
               (c-hanging-colons-alist         . ((case-label)
                                                  (label after)
                                                  (access-label after)
                                                  (member-init-intro before)
                                                  (inher-intro)))
               (c-cleanup-list                 . ((brace-else-brace)
                                                  (brace-elseif-brace)
                                                  (brace-catch-brace)
                                                  (empty-defun-braces)
                                                  (defun-close-semi)
                                                  (scope-operator)))
               (c-offsets-alist                . ((statement-block-intro . +)
                                                  (statement-case-intro  . +)
                                                  (knr-argdecl-intro     . 0)
                                                  (substatement-open     . 0)
                                                  (substatement-label    . 0)
                                                  (label                 . 0)
                                                  (statement-cont        . +)
                                                  (inline-open           . 0)
                                                  (inexpr-class          . 0)))
               (c-hanging-semi&comma-criteria
                . (c-semi&comma-no-newlines-for-oneline-inliners
                   c-semi&comma-inside-parenlist
                   c-semi&comma-no-newlines-before-nonblanks))))

(add-hook 'c-mode-common-hook (lambda () (c-set-style "berrym")))
(add-hook 'c-mode-common-hook (lambda () (c-toggle-comment-style -1))) ; use c++ style comments
(add-hook 'c-mode-common-hook 'lsp)

;; tabs are 4 spaces and tab indents
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(message "berrym-editor: module loaded successfully.")

(provide 'berrym-editor)

;;; berrym-editor.el ends here
