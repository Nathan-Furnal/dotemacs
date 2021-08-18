;;; handy.el --- Useful functions taken from here and there I'll probably use. -*- lexical-binding: t -*-

;;; Commentary:

;; A bunch of useful functions.

;;; Code:

(defun handy-find-user-init-file ()
  "Opens the `user-init-file' in another window.
Taken from https://github.com/bbatsov/crux/blob/master/crux.el"
  (interactive)
  (find-file-other-window user-init-file))

(provide 'handy)
;;; handy.el ends here
