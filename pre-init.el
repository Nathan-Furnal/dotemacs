;;; pre-init.el --- Init -*- no-byte-compile: t; lexical-binding: t; -*-

(set-face-attribute 'default nil :family "Iosevka Term Curly" :height 140 :weight 'regular)
(set-face-attribute 'fixed-pitch nil :family "Iosevka Term" :height 140 :weight 'medium)
(set-face-attribute 'variable-pitch nil :family "Iosevka Aile" :height 135 :weight 'medium)
(put 'narrow-to-region 'disabled nil) ; Allows narrowing bound to C-x n n (region) and C-x n w (widen)

(use-package emacs
  :bind (("C-z" . undo)
         ("C-x C-z" . nil)
         ("C-h h" . nil)
	     ;; AZERTY bindings
	     ("C-x &" . delete-other-windows)
	     ("C-x é" . split-window-below)
	     ("C-x \"" . split-window-right)
	     ("C-x à" . delete-window)
	     ("M-ù" . xref-find-definitions)
	     ("M-µ" . xref-find-references)
	     ("M-ç" . text-scale-increase)
	     ("M-à" . text-scale-decrease)
	     ("C-l" . duplicate-dwim)))

;; Own

(delete-selection-mode t)

;; Thanks Prot'
;; https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/
(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

 When the region is active, disable it.
 When a minibuffer is open, but not focused, close the minibuffer.
 When the Completions buffer is selected, close it.
 In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)
