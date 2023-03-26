;;; Commentary:
;;;berrym-misc-funcs.el --- Miscellaneous functions

;; Copyright (c) 2023 Michael Berry

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

(defun font-exists-p (font)
  "Check if a FONT exists.  Return T if found NIL if not."
  (if (null (x-list-fonts font)) nil t))

(message "berrym-misc-funcs loaded successfully")

(provide 'berrym-misc-funcs)
