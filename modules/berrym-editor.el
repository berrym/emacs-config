;;; berrym-editor.el --- Configure editing behaviors.
;;
;; Copyright (c) 2013-2015 Michael Berry

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

;; basic hooks for all programming modes that derive from prog mode
(add-hook 'prog-mode-hook (lambda ()
                            (define-key prog-mode-map
                              (kbd "C-m") 'newline-and-indent)
			    (define-key prog-mode-map
			      (kbd "RET") 'newline-and-indent)
			    (linum-mode t)
			    (whitespace-cleanup-mode t)))

;; define a c programming style
(defconst berrym-c-style
  '((c-basic-offset                 . 4)
    (c-indent-tabs-mode             . nil)
    (c-tab-always-indent            . nil)
    (c-comment-only-line-offset     . 0)
    (c-echo-syntactic-information-p . t)
    (c-report-syntactic-errors      . t)
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

;; configure python-mode
(setq python-python-command "python")
(elpy-enable)

;; configure lisp-mode to use sbcl and setup SLIME
(setq inferior-lisp-program "clisp")
(slime-setup '(slime-fancy slime-banner))

;; turn on some helpers for haskell-mode
(add-hook 'haskell-mode-hook (lambda ()
			       (turn-on-haskell-doc-mode)
			       (turn-on-haskell-indent)))

(message "berrym-editor: module loaded successfully.")

(provide 'berrym-editor)

;;; berrym-editor.el ends here
