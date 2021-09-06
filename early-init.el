;;; early-init.el --- early in the morning -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides early initialization for Emacs 27.1+ Do not initialize the
;; package manager.  This is done in `init.el'.  The optimization of
;; the early init comes from both Doom Emacs' config as well as Prot's
;; config.
;; See https://github.com/hlissner/doom-emacs/blob/develop/early-init.el
;; See https://protesilaos.com/dotemacs/

;;; Code:

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. We want to keep from loading at startup.
(setq package-enable-at-startup nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(mode-line-format . 0) default-frame-alist)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode 1)
;; Create a package-quickstart.el
(setq package-quickstart t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

(setq native-comp-async-report-warnings-errors nil) 	; Stop showing compilation warnings on startup

;;; early-init ends here
