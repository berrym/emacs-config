;;; Commentary:
;;;berrym-core.el --- Basic settings for GNU Emacs

;; Copyright (c) 2024 Michael Berry

;; Author: Michael Berry <trismegustis@gmail.com>
;; URL: https://github.com/berrym/emacs-config

;; This file is not part of GNU Emacs.

;; License: GPLv3

;;; Code:

(defconst save-files-dir (expand-file-name "save-files" user-emacs-directory)
  "Directory for storing autosave and backup files.")

(unless (file-exists-p save-files-dir)
  (make-directory save-files-dir))

(setq backup-directory-alist `((".*" . , save-files-dir)))
(setq auto-save-list-file-prefix (expand-file-name "saves-" save-files-dir))
(setq auto-save-file-name-transforms `((".*" ,save-files-dir t)))
(setq custom-file (expand-file-name "custom.el" (file-name-as-directory
						 save-files-dir)))
(defvar tramp-auto-save-directory (file-name-as-directory save-files-dir)
  "Set all tramp backups to this save-files-dir.")

(if (file-exists-p custom-file)
    (load custom-file))

(put 'dired-find-file-other-buffer 'disabled t)

(savehist-mode t)
(setq history-length 10000)

(message "berrym-core: module loaded successfully.")

(provide 'berrym-core)

;;; berrym-core.el ends here
