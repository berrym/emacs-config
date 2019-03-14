;;; berrym-core.el --- Basic settings for GNU Emacs
;;
;; Copyright (c) 2013-2018 Michael Berry
;;
;; Author: Michael Berry <trismegustis@gmail.com>
;; URL: https://bitbucket.org/berrym/emacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar *save-files-dir* (expand-file-name "save-files" user-emacs-directory)
  "Directory for storing autosave and backup files.")

(unless (file-exists-p *save-files-dir*)
  (make-directory *save-files-dir*))

(setq backup-directory-alist `((".*" . , *save-files-dir*)))
(setq auto-save-list-file-prefix (expand-file-name "saves-" *save-files-dir*))
(setq auto-save-file-name-transforms `((".*" ,*save-files-dir* t)))
(setq tramp-auto-save-directory (file-name-as-directory *save-files-dir*))
(setq custom-file (expand-file-name "custom.el" (file-name-as-directory
						 *save-files-dir*)))
(if (file-exists-p custom-file)
    (load custom-file))

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable.
Set it to match that used by the user's shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

(message "berrym-core: module loaded successfully.")

(provide 'berrym-core)

;;; berrym-core.el ends here
