;;; init.el --- Fun stuff all around -*- lexical-binding: t; -*-

;;; Commentary:
;; This file aims to provide a lightweight Emacs experience, it's heavily inspired from Prot's config as explained in the README.

;;; Code:

(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize the packages, avoiding a re-initialization.

(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

;; Make sure `use-package' is available.

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure `use-package' prior to loading it.

(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t)
  (setq use-package-compute-statistics nil)
  ;; The following is VERY IMPORTANT.  Write hooks using their real name
  ;; instead of a shorter version: after-init ==> `after-init-hook'.
  ;;
  ;; This is to empower help commands with their contextual awareness,
  ;; such as `describe-symbol'.
  (setq use-package-hook-name-suffix nil))

(eval-when-compile
  (require 'use-package))

(use-package diminish :ensure t :after use-package) ;; if you use :diminish
(use-package bind-key :ensure t :after use-package) ;; if you use any :bind variant

;;;========================================
;;; Useful defaults
;;;========================================

(use-package emacs
  :init
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode 1)
  (set-face-attribute 'default nil :family "Roboto Mono" :height 95)
  (set-face-attribute 'fixed-pitch nil :family "Roboto Mono" :height 95)
  (set-face-attribute 'variable-pitch nil :family "Roboto Regular" :height 100)
  
  :config
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8-unix)
  
  (setq-default cursor-type 'bar)               ; Line-style cursor similar to other text editors
  (setq inhibit-startup-screen t)               ; Disable startup screen
  (setq initial-scratch-message "")	        ; Make *scratch* buffer blank
  (setq-default frame-title-format '("%b"))     ; Make window title the buffer name
  (setq confirm-kill-processes nil)		; Stop confirming the killing of processes
  (setq ring-bell-function 'ignore)             ; Disable bell sound
  (fset 'yes-or-no-p 'y-or-n-p)                 ; y-or-n-p makes answering questions faster
  (show-paren-mode t)                           ; Visually indicates pair of matching parentheses
  (delete-selection-mode t)                     ; Start writing straight after deletion
  (put 'narrow-to-region 'disabled nil)	        ; Allows narrowing bound to C-x n n (region) and C-x n w (widen)
  (setq read-process-output-max (* 1024 1024))  ; Increase the amount of data which Emacs reads from the process
  (global-hl-line-mode 1)			; Highlight the current line to make it more visible
  (setq create-lockfiles nil)                   ; lock files kill `npm start'
  (setq-default fill-column 80)		        ; Set fill column to 80 rather than 70, in all cases. 

  ;; Speed up startup High garbage collection at startup needs to be
  ;; reset at some point then we defer the work to `gcmh'.
  (add-hook 'emacs-startup-hook
	    (lambda ()
	      (setq gc-cons-threshold 16777216 ; 16mb
		    gc-cons-percentage 0.1)))

  :bind (("C-z" . undo)
         ("C-x C-z" . nil)
         ("C-h h" . nil)
	 ;; AZERTY bindings instead of numbers
	 ("C-x &" . delete-other-windows)
	 ("C-x é" . split-window-below)
	 ("C-x \"" . split-window-right)
	 ("C-x à" . delete-window))
  :hook (text-mode-hook . auto-fill-mode))

;; Adopt a sneaky garbage collection strategy of waiting until idle
;; time to collect; staving off the collector while the user is
;; working.  Thanks Doom -
;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly
(use-package gcmh
  :ensure t
  :defer nil
  :config
  (setq gcmh-mode 1
	gcmh-idle-delay 5
	gcmh-high-cons-threshold (* 16 1024 1024)))

(use-package elec-pair
  :ensure nil
  :defer t
  :config

  (defun nf/electric-pair-local-text-mode ()
    "Advise and wrap electric pairs in text mode."
    (add-function :before-until electric-pair-inhibit-predicate
		  (lambda (c) (eq c ?<)))
    (electric-pair-local-mode))
  
  :hook ((prog-mode-hook . electric-pair-local-mode)
	 (text-mode-hook . nf/electric-pair-local-text-mode)))

(use-package recentf
  ;; Loads after 2 second of idle time.
  :defer 2)

;; Feature that provides the ability to browse Emacs kill ring in autocomplete style popup menu.

(use-package popup-kill-ring
  :ensure t
  :defer 2
  :bind ("M-y" . popup-kill-ring))

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-startup-banner 'logo
	dashboard-show-shortcuts t)
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)
                          (agenda . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (dashboard-setup-startup-hook))

;;;========================================
;;; Themes
;;;========================================

(use-package modus-themes
  :ensure t
  :pin melpa
  :init
  (setq modus-themes-org-blocks 'greyscale)
  (setq modus-themes-completions 'opinionated)
  (setq modus-themes-fringes 'subtle)
  (setq modus-themes-scale-headings t
	modus-themes-slanted-constructs t
	modus-themes-bold-constructs t
	modus-themes-syntax 'alt-syntax
	modus-themes-intense-hl-line nil
	modus-themes-variable-pitch-headings t
	modus-themes-paren-match 'intense
	modus-themes-headings 'section)

  (setq modus-themes-scale-1 1.05
	modus-themes-scale-2 1.1
	modus-themes-scale-3 1.15
	modus-themes-scale-4 1.2
	modus-themes-scale-5 1.3)

  (setq modus-themes-headings
	'((1 . section)
          (2 . section-no-bold)
          (3 . rainbow-line)
          (t . rainbow-line-no-bold))))

;; Running modus-themes depending on the time of the day.

(use-package solar
  :config
  (setq calendar-latitude 50.85
        calendar-longitude 4.35))

(use-package circadian
  :ensure t
  :after solar
  :config
  (setq circadian-themes '((:sunrise . modus-operandi)
                           (:sunset  . modus-vivendi)))
  (circadian-setup))

;; More useful modeline

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;;;========================================
;;; Completion & Navigation
;;;========================================

;; Tabs navigation and groups

(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (setq centaur-tabs-set-bar 'under)         ; Display underline for selected tab
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

;; Selectrum for completion and prescient keys

(use-package selectrum
  :ensure t
  :commands (selectrum-mode selectrum-prescient-mode prescient-persist-mode)
  :init
  (selectrum-mode)
  (selectrum-prescient-mode)
  (prescient-persist-mode))

(use-package selectrum-prescient
  :ensure t
  :after selectrum)

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :ensure t
  :defer 2
  :config
  (marginalia-mode)
  (setq marginalia-annotators '(marginalia-annotators-light)))

(use-package imenu
  :config
  (setq imenu-use-markers t)
  (setq imenu-auto-rescan t)
  (setq imenu-auto-rescan-maxout 600000)
  (setq imenu-max-item-length 100)
  (setq imenu-use-popup-menu nil)
  (setq imenu-eager-completion-buffer t)
  (setq imenu-space-replacement " ")
  (setq imenu-level-separator "/"))

(use-package imenu-list
  :ensure t
  :defer t
  :bind ("C-c i" . imenu-list))

(use-package flimenu
  :ensure t
  :defer t
  :config
  (flimenu-global-mode 1))

(use-package ctrlf
  :ensure t
  :commands (ctrlf-mode ctrlf-local-mode)
  :init (ctrlf-mode)
  :config
  (defun nf/ctrlf-hook ()
    (if (eq major-mode 'pdf-view-mode)
	(ctrlf-local-mode -1)))
  :hook (pdf-view-mode-hook . nf/ctrlf-hook))

(use-package which-key
  :ensure t
  :defer 4
  :diminish which-key-mode
  :config
  (which-key-mode 1))

(use-package company
  :ensure t
  :defer t
  :diminish
  :defines (company-dabbrev-other-buffers
	    company-dabbrev-code-other-buffers
	    company-dabbrev-downcase
	    company-dabbrev-ignore-case)
  :config
  (setq company-dabbrev-other-buffers t
        company-dabbrev-code-other-buffers t

        ;; M-<num> to select an option according to its number.
        company-show-numbers t

        ;; Only 2 letters required for completion to activate.
        company-minimum-prefix-length 3

        ;; Do not downcase completions by default.
        company-dabbrev-downcase nil

        ;; Even if I write something with the wrong case,
        ;; provide the correct casing.
	
        company-dabbrev-ignore-case t

        ;; Don't way before completion.
        company-idle-delay 0

	;; No company-mode in shell & eshell
	company-global-modes '(not eshell-mode shell-mode))
  
  :hook ((text-mode-hook . company-mode)
         (prog-mode-hook . company-mode)))

;;;========================================
;;; Windows & movement
;;;========================================

(use-package windmove
  :ensure nil
  :defer t
  :config
  (setq windmove-create-window nil)     ; Emacs 27.1
  :bind (("C-c <up>" . windmove-up)
         ("C-c <right>" . windmove-right)
         ("C-c <down>" . windmove-down)
         ("C-c <left>" . windmove-left)))

(use-package transpose-frame
  :ensure t
  :defer t
  :commands (transpose-frame
             flip-frame
             flop-frame
             rotate-frame
             rotate-frame-clockwise
             rotate-frame-anticlockwise)
  :bind (("C-c f" . flop-frame)
         ("C-c r" . rotate-frame-clockwise)))

;;;========================================
;;; Spell checking
;;;========================================

;; Requires aspell, aspell-en & aspell-fr to work

(use-package flyspell
  :ensure t
  :defer t
  :hook ((text-mode-hook . flyspell-mode)
	 (prog-mode-hook . flyspell-prog-mode))
  
  :config
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-program-name "aspell")
  (setq ispell-dictionary "en_US")

  (defvar nf/ispell-dicts
    '(("English" . "en_US")
      ("Français" . "fr"))
    "Alist of languages dictionaries")

  (defun nf/ispell-dictionaries-complete ()
    "Select an item from `nf/ispell-dicts'."
    (interactive)
    (let* ((dicts (mapcar #'car nf/ispell-dicts))
           (choice (completing-read "Select dictionary: " dicts nil t))
           (key (cdr (assoc `,choice nf/ispell-dicts))))
      (ispell-change-dictionary key)
      (message "Switched to %s" key)))

  :bind ("C-x C-;" . nf/ispell-dictionaries-complete))

;; Syntax checking for GNU Emacs

(use-package flycheck
  :ensure t
  :defer t
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save)) ; Check on save instead of running constantly
  :hook ((prog-mode-hook text-mode-hook) . flycheck-mode))
;;;========================================
;;; Org-mode
;;;========================================

(use-package org
  :ensure t
  :pin elpa
  :config
  (setq org-imenu-depth 7)
  (setq org-fontify-done-headline nil
	org-fontify-quote-and-verse-blocks t
	org-fontify-whole-heading-line nil
	org-fontify-whole-block-delimiter-line t)
  (setq org-confirm-babel-evaluate nil                  ; Don't prompt before running code in org
	org-src-fontify-natively t                      ; Use syntax highlighting in source blocks while editing
	org-src-tab-acts-natively t                     ; Tabs act as 4 spaces in source blocks
	org-src-preserve-indentation t                  ; Preserving indentation in source blocks
	org-highlight-latex-and-related '(latex))       ; Coloring latex code in org mode
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))   ; Open PDF's with Emacs
  (setq org-latex-prefer-user-labels t)	                ; Prefer user names and labels for references

  ;; Set :scale to 2 instead of 1 when org mode renders LaTeX
  (setq org-format-latex-options '(:foreground default
					       :background default
					       :scale 2.0
					       :html-foreground "Black"
					       :html-background "Transparent"
					       :html-scale 1.0
					       :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

  (defun nf/toggle-presentation ()
    "Toggle between presentation and regular `org-mode'.
    The modes used are `olivetti-mode' and
    `org-tree-slide-mode'."
    (interactive)
    (if (bound-and-true-p olivetti-mode)
	(olivetti-mode -1)
      (olivetti-mode 1))
    (if (bound-and-true-p org-tree-slide-mode)
	(org-tree-slide-mode -1)
      (org-tree-slide-mode 1)))

  :bind (:map org-mode-map
	      ("C-c i" . imenu-list)
	      ("C-x p" . nf/toggle-presentation)))


;; Custome LaTeX templates
;; Requires a full LaTeX install, usually called `texlive'.
;; The arch wiki https://wiki.archlinux.org/index.php/TeX_Live details how to use it
;; latex compilation found at https://github.com/jkitchin/org-ref/blob/master/org-ref.org
;; Better latexmk for glossaries with a ~/.latexmkrc file. Explained at `https://tex.stackexchange.com/a/44316/223017'.

(use-package auctex
  :ensure t
  :defer t)

;; LaTeX config and use PDF-tools to view PDF files
(use-package tex
  :ensure nil
  :defer t
  :defines TeX-view-program-selection
  :config
  (setq TeX-view-program-selection
	'(((output-dvi has-no-display-manager)  "dvi2tty")
	  ((output-dvi style-pstricks)   "dvips and gv")
	  (output-dvi "xdvi")
	  (output-pdf "PDF Tools")
	  (output-html "xdg-open"))))

(use-package template
  :after org ox-latex
  :load-path (lambda () (concat user-emacs-directory "latex")))

(use-package ox-latex
  :defer t
  :after org
  :config
  (add-to-list 'org-latex-packages-alist
	       '("AUTO" "polyglossia" t ("xelatex" "lualatex")))
  (setq
   org-latex-listings 'minted
   org-latex-minted-options '(("linenos=true") ("bgcolor=gray!10!white") ("breaklines=true")))

  (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")))

(use-package cdlatex
  :ensure t
  :defer t
  :hook ((org-mode-hook . turn-on-org-cdlatex)    ; Enable cdlatex by default
	 (LaTex-mode-hook . turn-on-cdlatex)
	 (latex-mode-hook . turn-on-cdlatex))
  :bind (:map org-cdlatex-mode-map
	      ("`" . nil)))

(use-package org-ref
  :ensure t
  :defer 2
  :after org)

(use-package shackle
  :ensure t
  :defer t
  :hook (org-mode-hook . shackle-mode)
  :config
  (setq shackle-rules
	'((pdf-view-mode :align right)))) ; Ensure PDF view opens on the right

;;;========================================
;;; Note taking
;;;========================================

;; Roam based workflow for org-mode

(use-package org-roam
  :ensure t
  :diminish "-Ω-"
  :hook
  (after-init-hook . org-roam-mode)
  :custom
  (org-roam-directory (expand-file-name "~/org-roam"))
  (org-roam-db-location (expand-file-name "~/org-roam/org-roam.db"))
  :config

  (setq org-roam-capture-templates
	'(("d" "default" plain
           (function org-roam-capture--get-point)
           "%?"
           :file-name "fleeting/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\n"
           :unnarrowed t)
	  ("c" "concept" plain
	   (function org-roam-capture--get-point)
	   "%?"
	   :file-name "concepts/${slug}"
	   :head "#+latex_class: notes_en\n#+title: ${title}\n#+author: Nathan Furnal\n#+roam_tags:\n#+created: %U\n#+las_modified: %U\n\n"
	   :unnarrowed t)
	  ("l" "literature" plain
	   (function org-roam-capture--get-point)
	   "%?"
	   :file-name "literature/${slug}"
	   :head "#+latex_class: notes_en\n#+title: ${title}\n#+author: Nathan Furnal\n#+roam_tags:\n#+created: %U\n#+las_modified: %U\n\n"
	   :unnarrowed t)))
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

;; Adding Deft an easy way to go through files and create notes on the fly
;; Source : https://jblevins.org/projects/deft/

(use-package deft
  :ensure t
  :defer t
  :config (setq deft-default-extension "org"
	        deft-directory "~/projects/notes"
		deft-use-filter-string-for-filename t
		deft-recursive t                                   ; Allows searching through sub-directories
		deft-use-filename-as-title t)                      ; use filename instead of first line of doc
  :bind ("C-c d" . deft))

;; Take screenshots

(use-package org-download
  :ensure t
  :defer t)

;;;========================================
;;; Presentation
;;;========================================

(use-package org-tree-slide
  :ensure t
  :defer t
  :diminish
  :config
  (setq org-tree-slide-breadcrumbs nil)
  (setq org-tree-slide-header nil)
  (setq org-tree-slide-slide-in-effect nil)
  (setq org-tree-slide-heading-emphasis nil)
  (setq org-tree-slide-cursor-init t)
  (setq org-tree-slide-modeline-display nil)
  (setq org-tree-slide-skip-done nil)
  (setq org-tree-slide-skip-comments t)
  (setq org-tree-slide-fold-subtrees-skipped t)
  (setq org-tree-slide-skip-outline-level 8)
  (setq org-tree-slide-never-touch-face t)
  (setq org-tree-slide-activate-message
        (format "Presentation %s" (propertize "ON" 'face 'success)))
  (setq org-tree-slide-deactivate-message
        (format "Presentation %s" (propertize "OFF" 'face 'error)))
  :bind (:map org-tree-slide-mode-map
         ("<C-down>" . org-tree-slide-display-header-toggle)
         ("<C-right>" . org-tree-slide-move-next-tree)
         ("<C-left>" . org-tree-slide-move-previous-tree)))

;;;========================================
;;; Focus
;;;========================================

(use-package olivetti
  :ensure t
  :defer t
  :diminish
  :config
  (setq olivetti-body-width 0.7)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t))

;;;========================================
;;; Reading
;;;========================================

(use-package pdf-tools
  :ensure t
  :mode  ("\\.pdf\\'" . pdf-view-mode)
  :hook (TeX-after-compilation-finished-hook . TeX-revert-document-buffer)
  :defines pdf-annot-activate-created-annotations
  :config
  (setq-default pdf-view-display-size 'fit-page)

  ;; more fine-grained zooming
  
  (setq pdf-view-resize-factor 1.05)

  ;; create annotation on highlight
  
  (setq pdf-annot-activate-created-annotations t)
   
  (pdf-tools-install :no-query)
  (require 'pdf-occur)
  :bind (:map pdf-view-mode-map
	      ("C-s" . isearch-forward)
	      ("C-r" . isearch-backward)))

;;;========================================
;;; Agenda & Organization
;;;========================================

;; Empty for now

;;;========================================
;;; Markdown
;;;========================================

(use-package markdown-mode
  :ensure t
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t)
  :init (setq markdown-command "pandoc")
  ;; I use those bindings for window movement
  :bind (:map markdown-mode-map
	      ("C-c <left>" . nil)
	      ("C-c <right>" . nil)
	      ("C-c <up>" . nil)
	      ("C-c <down>" . nil)))

;;;========================================
;;; Exporting text files
;;;========================================

(use-package pandoc-mode
  :ensure t
  :defer t)

;;;========================================
;;; Version control
;;;========================================

;; Git integration for emacs
;; Requires git

(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status))

;;;========================================
;;; Editing
;;;========================================

(use-package multiple-cursors
  :ensure t
  :defer t
  :bind   (("C-S-m" . mc/edit-lines)
	   ("C->" . mc/mark-next-like-this)
	   ("C-<" . mc/mark-previous-like-this)
	   ("C-c C-<" . mc/mark-all-like-this)))

(use-package iedit
  :ensure t
  :defer t
  :bind ("C-:" . iedit-mode))

(use-package rect
  :ensure nil
  :defer t
  :bind ("C-x <SPC>" . rectangle-mark-mode))

;;;========================================
;;; Navigation
;;;========================================

(use-package treemacs
  :ensure t
  :defer t
  :config
  (setq treemacs-no-png-images t
	treemacs-width 24)
  :bind ("C-c t" . treemacs))

;;;========================================
;;; Project management
;;;========================================

(use-package projectile
  :ensure t
  :defer t
  :bind ("M-p" . projectile-mode)
  (:map projectile-mode-map
	("C-c p" . projectile-command-map)))

(use-package treemacs-projectile
  :ensure t
  :after treemacs projectile
  :defer t)

;;;========================================
;;; Developement with LSP
;;;========================================

(use-package lsp-mode
  :ensure t
  :defer t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :defines (lsp-clients-clangd-args
	    lsp-sqls-server)
  :hook ((css-mode-hook . lsp-deferred)
	 (web-mode-hook . lsp-deferred)
	 (js2-mode-hook . lsp-deferred)
	 (c++-mode-hook . lsp-deferred)
	 (c-mode-hook . lsp-deferred)
	 (java-mode-hook . lsp-deferred)
	 (python-mode-hook . lsp-deferred)
	 (sql-mode-hook . lsp-deferred)
	 (rust-mode-hook . lsp-deferred)
	 (clojure-mode-hook . lsp-deferred)
	 (lsp-mode-hook . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :bind (:map lsp-mode-map
	      ("M-<RET>" . lsp-execute-code-action))
  :config
  (setq lsp-keep-workspace-alive nil
	lsp-auto-guess-root nil)

  ;; C++ config
  (setq lsp-clients-clangd-args '("--clang-tidy"))

  ;; SQL config

  (setq lsp-sqls-server "~/go/bin/sqls"))

(use-package lsp-ui
  :ensure t
  :defer t
  :config
  (setq lsp-ui-sideline-enable nil
	lsp-ui-doc-delay 2)
  :hook (lsp-mode-hook . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
	      ("C-c i" . lsp-ui-imenu)))

;; LSP integration with treemacs

(use-package lsp-treemacs
  :ensure t
  :defer t
  :after lsp)

;; Debugger

(use-package dap-mode
  :ensure t
  :defer t
  :after lsp-mode lsp-treemacs
  :config
  (dap-auto-configure-mode))

;;;========================================
;;; (E)Lisp development
;;;========================================

(use-package buttercup
  :ensure t
  :defer t)

(use-package package-lint
  :ensure t
  :defer t)

(use-package elisp-lint
  :ensure t
  :defer t)

;; Emacs shell

(use-package eshell
  :defer t)

;; Better shell

(use-package vterm
  :ensure t
  :defer t
  :bind ("C-$" . vterm))

;;;========================================
;;; Common Lisp
;;;========================================

(use-package sly
  :ensure t
  :defer t
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package paredit
  :ensure t
  :defer t
  :hook ((emacs-lisp-mode-hook lisp-mode-hook racket-mode-hook) . paredit-mode))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (lisp-mode-hook . rainbow-delimiters-mode))

;;;========================================
;;; Clojure
;;;========================================

(use-package cider
  :ensure t
  :defer t)

(use-package clojure-mode
  :ensure t
  :defer t
  :defines lsp-completion-enable
  :config
  (setq lsp-completion-enable nil) ; use cider completion
  :hook (clojure-mode-hook . (lambda ()
			       (cider)
			       (paredit-mode))))

;;;========================================
;;; Scheme & Racket
;;;========================================

(use-package scheme
  :defer t
  :ensure nil
  :mode ("\\.scm$\\'")
  :hook (scheme-mode-hook . paredit-mode))

(use-package geiser
  :ensure t
  :defer t
  :defines geiser-guile-binary
  :functions geiser-impl--set-buffer-implementation
  :commands (geiser run-geiser)
  :config
  ;; Send the argument of `run-geiser' to
  ;; `geiser-impl--set-buffer-implementation' BEFORE `run-geiser' is
  ;; ran. As I had to set the Scheme implementation by hand otherwise
  ;; with `geiser-set-scheme'
  (advice-add 'run-geiser :before #'geiser-impl--set-buffer-implementation)
  (setq geiser-guile-binary "/usr/bin/guile3"))

(use-package racket-mode
  :defer t
  :ensure t
  :bind (:map racket-mode-map
	      ("C-c C-c" . racket-run)
	      ("M-<RET>" . racket-eval-last-sexp))
  :hook (racket-mode-hook . racket-xp-mode))

;;;========================================
;;; Julia
;;;========================================

;; Requires a Julia install

(use-package julia-mode
  :ensure t
  :defer t
  :defines inferior-julia-program
  :mode ("\\.jl\\'" . julia-mode)
  :init
  (setq inferior-julia-program "/usr/bin/julia")
  (setenv "JULIA_NUM_THREADS" "16")
  :hook (julia-mode-hook . julia-repl-mode))

(use-package julia-repl
  :requires julia-mode
  :ensure t
  :defer t
  :functions (julia-repl-set-terminal-backend)
  :config
  (julia-repl-set-terminal-backend 'vterm))

(use-package lsp-julia
  :ensure t
  :defer t
  :defines lsp-julia-default-environment
  :config
  (setq lsp-julia-default-environment "~/.julia/environments/v1.6")
  :hook (julia-mode-hook . (lambda ()
			     (require 'lsp-julia) (lsp-deferred))))

;; ========================================
;; Python
;; ========================================

(use-package python
  :ensure t
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil)
  ;; Use IPython when available or fall back to regular Python
  (cond
   ((executable-find "ipython")
    (progn
      (setq python-shell-buffer-name "IPython")
      (setq python-shell-interpreter "ipython")
      (setq python-shell-interpreter-args "-i --simple-prompt")))
   ((executable-find "python3")
    (setq python-shell-interpreter "python3"))
   ((executable-find "python2")
    (setq python-shell-interpreter "python2"))
   (t
    (setq python-shell-interpreter "python"))))

;; Hide the modeline for inferior python processes
(use-package inferior-python-mode
  :ensure nil
  :hook ((inferior-python-mode-hook . hide-mode-line-mode)
	 (inferior-ess-r-mode-hook . hide-mode-line-mode)))

(use-package hide-mode-line
  :ensure t
  :defer t)

(use-package poetry
  :ensure t
  :defer t
  :hook (python-mode-hook . poetry-tracking-mode))

(use-package lsp-pyright
  :ensure t
  :defer t
  :defines (lsp-clients-python-library-directories
	    lsp-pyright-disable-language-service)
  :config
  (setq lsp-pyright-disable-language-service nil
	lsp-pyright-disable-organize-imports nil
	lsp-pyright-auto-import-completions t
	lsp-pyright-use-library-code-for-types t)
  :hook ((python-mode-hook . (lambda ()
			       (require 'lsp-pyright) (lsp-deferred)))))

(use-package yapfify
  :ensure t
  :defer t
  :hook (python-mode-hook . yapf-mode))

;;;========================================
;;; Jupyter & notebooks
;;;========================================

;; Org babel allows to evaluate code block in org-mode
;; jupyter HAS to be last to work, per : https://github.com/nnicandro/emacs-jupyter#org-mode-source-blocks

;; R support
;; To install the R Kernel once R is installed, go in the R console and :
;; install.packages('IRkernel')
;; IRkernel::installspec()
;; To add linting, one has to install the "lintr" package and
;; Create a ~/.R/lintr_cache/ directory

(use-package ess
  :ensure t
  :defer t
  :mode ("\\.R\\'" . ess-r-mode))

;; Julia support
;; (For notebooks and jupyter support)
;; First you need to add IJulia to the packages and run using IJulia
;; Then run either notebook() or jupyterlab()

(use-package jupyter
  :ensure t
  :defer t
  :defines (org-babel-default-header-args:jupyter-python
	    org-babel-default-header-args:jupyter-julia
	    org-babel-default-header-args:jupyter-R)
  :after (ob)
  :init
  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                       (:session . "py")))
  
  (setq org-babel-default-header-args:jupyter-julia '((:async . "yes")
                                                      (:session . "jl")
                                                      (:kernel . "julia-1.6")))
  
  (setq org-babel-default-header-args:jupyter-R '((:async . "yes")
						  (:session . "R")
						  (:kernel . "ir"))))

(use-package ob
  :after org
  :defer nil
  :defines (org-ditaa-jar-path org-plantuml-jar-path)
  :config
  (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar"
	org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
  (setq org-babel-load-languages
	'((latex . t)
	  (python . t)
	  (R . t)
	  (shell . t)
	  (sql . t)
	  (emacs-lisp . t)
	  (maxima . t)
	  (java . t)
	  (ditaa . t)
	  (plantuml . t)
	  (gnuplot . t)
	  (jupyter . t)))
  
  (org-babel-do-load-languages
   'org-babel-load-languages
   org-babel-load-languages))

;;;========================================
;;; Java
;;;========================================

;; Requires lsp-mode

(use-package lsp-java
  :ensure t
  :defer t
  :defines c-label-offset
  :after lsp
  :mode ("\\.java\\'")
  :config
  (setq lsp-java-format-on-type-enabled nil)
  (defun nf/java-mode-hook ()
    (setq c-basic-offset 2
          c-label-offset 0
          tab-width 2
          indent-tabs-mode nil
	  require-final-newline nil))
  :hook (java-mode-hook . (lsp nf/java-mode-hook)))

;; Requires dap-mode

(use-package dap-java
  :ensure nil
  :defer t
  :after (lsp-java))

;;;========================================
;;; Web development
;;;========================================


;; LSP requirements on the server
;; sudo npm i -g typescript-language-server; sudo npm i -g typescript
;; sudo npm i -g javascript-typescript-langserver
;; sudo npm install -g prettier ; it's a linter/formatter
;; -> checkout https://youtu.be/0zuYCEzrchk

(use-package rjsx-mode
  :ensure t
  :defer t
  :mode ("\\.js\\'" . rjsx-mode)
  :hook (rjsx-mode-hook . prettier-js-mode))

(use-package prettier-js
  :ensure t
  :defer t
  :after (rjsx-mode))

;; Importantly, I have to setup a jsconfig.json in the root folder of the project, see https://github.com/ananthakumaran/tide#javascript
;; Here is a template :
;; {
;;   "compilerOptions": {
;;     "target": "es2017",
;;     "allowSyntheticDefaultImports": true,
;;     "noEmit": true,
;;     "checkJs": true,
;;     "jsx": "react",
;;     "lib": [ "dom", "es2017" ]
;;   }
;; }

(use-package tide
  :ensure t
  :defer t
  :commands flycheck-add-next-checker
  :after (rjsx-mode flycheck company)
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  ;; configure javascript-tide checker to run after your default javascript checker
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
  :hook
  ((rjsx-mode-hook . setup-tide-mode)
   (typescript-mode-hook . tide-setup)
   (typescript-mode-hook . tide-hl-identifier-mode)
   (before-save-hook . tide-format-before-save))

  :bind (:map tide-mode-map
	      ("M-j" . tide-jsdoc-template)))

(use-package js2-refactor
  :ensure t
  :defer t
  :after js2-mode
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m")
  :hook (js2-mode-hook . js2-refactor-mode))

(use-package json-mode
  :ensure t
  :defer t)

;; Requires node : sudo apt install nodejs

(use-package web-mode
  :ensure t
  :defer t
  :mode ("\\.html\\'" "\\.php\\'")
  :bind (:map web-mode-map
	      ("C-c C-v" . browse-url-of-buffer))
  :config
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq js-indent-level 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-css-colorization t)
  :hook (web-mode-hook . electric-pair-mode))

(use-package css-mode
  :ensure nil
  :defer t
  :mode "\\.css\\'"
  :hook (css-mode-hook . emmet-mode))

;; Useful tool to have live html documents update
;; To use : M-x httpd-start
;; One has to visit http://localhost:8080/imp/ (which I bookmarked) to display the file in browser

(use-package impatient-mode
  :ensure t
  :defer t
  :hook (web-mode-hook . impatient-mode))

;; Evaluate JS from a REPL

(use-package nodejs-repl
  :ensure t
  :defer t
  :commands nodejs-repl
  :bind (:map js2-mode-map
	      ("C-x C-e" . nodejs-repl-send-last-expression)
	      ("C-c C-j" . nodejs-repl-send-line)
	      ("C-c C-r" . nodejs-repl-send-region)
	      ("C-c C-c" . nodejs-repl-send-buffer)
	      ("C-c C-l" . nodejs-repl-load-file)
	      ("C-c C-z" . nodejs-repl-switch-to-repl)))

;; Useful cheat-sheet https://docs.emmet.io/cheat-sheet/

(use-package emmet-mode
  :ensure t
  :defer t
  :init
  (setq emmet-indentation 2)
  (setq emmet-move-cursor-between-quotes t)
  ;; Auto-start on any markup modes
  :hook ((sgml-mode-hook . emmet-mode)
	 (web-mode-hook . emmet-mode)))

;;;========================================
;;; Rust
;;;========================================

(use-package rustic
  :ensure t
  :defer t
  :config
  (setq rustic-cargo-bin (expand-file-name "~/.cargo/bin/cargo")))

(use-package cargo
  :ensure t
  :defer t
  :hook (rust-mode-hook . cargo-minor-mode))

;;;========================================
;;; Code snippets and skeletons
;;;========================================

(use-package yasnippet
  :ensure t
  :defer 3
  :config
  (yas-global-mode))

(use-package yasnippet-snippets
  :ensure t
  :defer t
  :after yasnippet)

;;;========================================
;;; Sharing
;;;========================================

(use-package gif-screencast
  :ensure t
  :defer t)

;;;========================================
;;; Mathematics
;;;========================================

(use-package maxima
  :ensure t
  :defer t)

;;;========================================
;;; Diverse text formats editing
;;;========================================

(use-package yaml-mode
  :ensure t
  :defer t
  :mode ("\\.yml\\'"))

;;;========================================
;;; Diagrams & Graphs
;;;========================================

(use-package plantuml-mode
  :ensure t
  :defer t
  :config
  (setq plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar))

(use-package gnuplot
  :ensure t
  :defer t)

;;;========================================
;;; Assembly language
;;;========================================

(use-package nasm-mode
  :defines nasm-basic-offset
  :mode ("\\.asm\\'" . nasm-mode)
  :ensure t
  :defer t
  :config
  (setq nasm-basic-offset 4))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(poetry modus-themes racket-mode yasnippet-snippets yapfify yaml-mode which-key web-mode vterm use-package treemacs-projectile transpose-frame tide sly shackle selectrum-prescient rustic rjsx-mode rainbow-delimiters prettier-js popup-kill-ring plantuml-mode paredit pandoc-mode org-tree-slide org-roam org-ref org-download olivetti nodejs-repl nasm-mode maxima marginalia magit lsp-ui lsp-pyright lsp-julia lsp-java jupyter julia-repl json-mode js2-refactor impatient-mode imenu-list iedit hide-mode-line gnuplot gif-screencast geiser gcmh flimenu ess emmet-mode elisp-lint doom-modeline diminish deft dashboard ctrlf company circadian cider centaur-tabs cdlatex cargo buttercup auctex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
