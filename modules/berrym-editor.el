;;; berrym-editor.el --- Configure editing behaviors.
;;
;; Copyright (c) 2013 Michael Berry

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

;; basic hooks for all programming modes
(add-hook 'prog-mode-hook (lambda ()
			    (define-key prog-mode-map
			      (kbd "RET") 'newline-and-indent)
			    (linum-mode t)
			    (whitespace-cleanup-mode t)))

;; define a berrym c programming style
(defconst berrym-c-style
  '((c-basic-offset                 . 4)
    (c-indent-tabs-mode             . nil)
    (c-tab-always-indent            . t)
    (c-comment-only-line-offset     . 0)
    (c-require-final-newline        . t)
    (c-echo-syntactic-information-p . t)
    (c-report-syntactic-errors      . t)
    (c-hanging-braces-alist         . ((brace-list-open)
				       (brace-list-intro)
				       (brace-list-close)
				       (brace-entry-open)
				       (substatement-open after)
				       (block-close . c-snug-do-while)
				       (arglist-cont-nonempty)))
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
				       (inexpr-class          . 0))))
  "Michael Berry C Programming Style.")

;; load berrym style for certain modes
(c-add-style "berrym" berrym-c-style)
(mapc (lambda (mode)
	(add-hook mode (lambda ()
			 (c-set-style "berrym")
			 (define-key c-mode-base-map
			   (kbd "RET") 'newline-and-indent)
			 (setq case-fold-search nil))))
      '(c-mode-hook c++-mode-hook objc-mode-hook))

;; configure python-mode to use ipython3 with elpy
(setq python-python-command "ipython3")
(elpy-enable)
(elpy-use-ipython "ipython3")
(elpy-clean-modeline)

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
