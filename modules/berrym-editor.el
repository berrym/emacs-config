;;; Commentary:
;;;berrym-editor.el --- Configure Editor Behaviors

;; Copyright (c) 2020 Michael Berry

;; Author: Michael Berry <trismegustis@gmail.com>
;; URL: https://github.com/berrym/emacs-config

;; This file is not part of GNU Emacs.

;; License: GPLv3

;;; Code:
(defun repeat-last-search-forward ()
  "Repeat last search forward."
  (interactive)
  (search-forward (car search-ring)))

(defun repeat-last-search-backward ()
  "Repeat last search backward."
  (interactive)
  (search-backward (car search-ring)))

(defun backward-kill-line ()
  "Kill from point to start of line, if beginning of line delete newline."
  (interactive)
  (if (bolp)
      (backward-delete-char 1)
    (kill-line 0)))

(defun duplicate-line ()
  "Create a duplicate of current line."
  (interactive)
  (save-mark-and-excursion
    (beginning-of-line)
    (insert (thing-at-point 'line t))))

(defun transpose-line-down ()
  "Transpose current line down."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun transpose-line-up ()
  "Transpose current line up."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (forward-line -1)
    (move-to-column col)))

(defun print-elements-in-list (l)
  "Print each element in a list L."
  (interactive)
  (while l
    (print (car l))
    (setq l (cdr l))))

;; define a c programming style
(defconst berrym-c-style
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
        c-semi&comma-no-newlines-before-nonblanks)))
  "Michael Berry C Programming Style.")
(c-add-style "berrym" berrym-c-style)
(add-hook 'c-mode-common-hook (lambda () (c-set-style "berrym")))
(add-hook 'rustic-mode-hook (lambda () (c-set-style "berrym")))

(use-package ruby-mode
  :diminish
  :mode "\\.rb\\'"
  :interpreter "ruby")

(use-package python
  :diminish
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

(use-package elpy
  :diminish
  :hook (after-init . elpy-enable))

(setq inferior-lisp-program "sbcl")

(message "berrym-editor: module loaded successfully.")

(provide 'berrym-editor)

;;; berrym-editor.el ends here
