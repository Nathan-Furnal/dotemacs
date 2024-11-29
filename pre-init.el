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
