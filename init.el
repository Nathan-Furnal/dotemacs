;;; init.el --- Fun stuff all around -*- lexical-binding: t; -*-

;; Package-Requires : ((emacs "29.060"))

;;; Commentary:
;; This file aims to provide a lightweight Emacs experience, it's heavily
;; inspired from Prot's config as explained in the README.  I also try use Emacs
;; internals as well as possible.

;;; Code:

(require 'use-package)

(use-package use-package
  :custom
  (use-package-hook-name-suffix nil))

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("elpa" . "https://elpa.gnu.org/packages/")))

(unless (bound-and-true-p package--initialized)
  (package-initialize))

(use-package diminish :ensure t :after use-package) ;; if you use :diminish
(use-package bind-key :ensure t :after use-package) ;; if you use any :bind variant
(use-package delight :ensure t :after use-package)  ;; Use delighting for modes

;;;========================================
;;; Useful defaults
;;;========================================

(use-package emacs
  :init
  (set-face-attribute 'default nil :family "Roboto Mono" :height 130 :weight 'regular)
  (set-face-attribute 'fixed-pitch nil :family "Roboto Mono" :height 130 :weight 'medium)
  (set-face-attribute 'variable-pitch nil :family "Roboto Regular" :height 130 :weight 'medium)
  :config
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8-unix)
  
  (setq-default cursor-type 'bar)               ; Line-style cursor similar to other text editors
  (setq inhibit-startup-screen t)               ; Disable startup screen
  (setq initial-scratch-message "")	        ; Make *scratch* buffer blank
  (setq-default frame-title-format '("%b"))     ; Make window title the buffer name
  (setq confirm-kill-processes nil)		; Stop confirming the killing of processes
  (setq use-short-answers t)                    ; y-or-n-p makes answering questions faster
  (show-paren-mode t)                           ; Visually indicates pair of matching parentheses
  (delete-selection-mode t)                     ; Start writing straight after deletion
  (put 'narrow-to-region 'disabled nil)	        ; Allows narrowing bound to C-x n n (region) and C-x n w (widen)
  (setq read-process-output-max (* 1024 1024))  ; Increase the amount of data which Emacs reads from the process
  (global-hl-line-mode 1)			; Highlight the current line to make it more visible
  (setq create-lockfiles nil)                   ; lock files kill `npm start'
  (setq-default fill-column 80)		        ; Set fill column to 80 rather than 70, in all cases.
  (pixel-scroll-precision-mode 1)	        ; Precision scrolling

  (setq backup-directory-alist
	`(("." . ,(concat user-emacs-directory "backups"))))
  
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
	 ("M-à" . text-scale-decrease))
  :hook (text-mode-hook . auto-fill-mode))
;; Adopt a sneaky garbage collection strategy of waiting until idle
;; time to collect; staving off the collector while the user is
;; working.  Thanks Doom -
;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly
(use-package gcmh
  :ensure t
  :diminish gcmh-mode
  :custom
  (gcmh-mode 1)
  (gcmh-idle-delay 10)
  (gcmh-high-cons-threshold (* 32 1024 1024))
  (gc-cons-percentage 0.8))

(use-package elec-pair
  :ensure nil
  :defer t
  :config
  (defun nf-electric-pair-local-text-mode ()
    "Advise and wrap electric pairs in text mode."
    (add-function :before-until electric-pair-inhibit-predicate
		  (lambda (c) (eq c ?<)))
    (electric-pair-local-mode))
  :hook ((prog-mode-hook . electric-pair-local-mode)
	 (text-mode-hook . nf-electric-pair-local-text-mode)))

;; The README is very informative as to how to set paths and shells properly
;; https://github.com/purcell/exec-path-from-shell

(use-package exec-path-from-shell
  :ensure t
  :defer nil
  :config
  (exec-path-from-shell-copy-env "PATH")
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (when (daemonp)
  (exec-path-from-shell-initialize)))

(use-package moody
  :if (window-system)
  :ensure t
  :after emacs
  :custom
  (mode-line-compact t)
  (x-underline-at-descent-line t)
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package eldoc
  :diminish eldoc-mode)

(use-package autorevert
  :defer 2
  :delight auto-revert-mode)

(use-package recentf
  :defer 2)

;; Better shell

(use-package vterm
  :ensure t
  :defer t
  :bind ("C-$" . vterm))

(use-package vertico
  :ensure t
  :after emacs
  :init
  (vertico-mode))

;;;========================================
;;; Themes
;;;========================================

(use-package modus-themes
  :pin melpa
  :ensure t
  :init
  (setq modus-themes-org-blocks 'tinted-background
	modus-themes-italic-constructs t
	modus-themes-bold-constructs t
	modus-themes-mixed-fonts t
	modus-themes-common-palette-overrides '((border-mode-line-active unspecified)
						(border-mode-line-inactive unspecified)
						(fringe subtle)
						(bg-paren-match bg-yellow-intense)))
  (setq modus-themes-headings
        (quote ((1 . (overline variable-pitch 1.4))
                (2 . (overline variable-pitch 1.25))
                (3 . (overline 1.1))
                (t . (monochrome))))))

;; Running modus-themes depending on the time of the day.

(use-package solar
  :ensure nil
  :custom
  (calendar-latitude 50.85)
  (calendar-longitude 4.35))

(use-package circadian
  :ensure t
  :after solar
  :config
  (setq circadian-themes '((:sunrise . modus-operandi)
                           (:sunset  . modus-vivendi)))
  (circadian-setup))

;;;========================================
;;; Completion & Navigation
;;;========================================

(use-package isearch
  ;; Delete char on <DEL> instead of going back searches
  :bind (("C-c s" . isearch-forward-thing-at-point)
	 :map isearch-mode-map
	      ("<DEL>" . isearch-del-char)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(partial-completion orderless flex))
  (completion-category-defaults nil)
  (read-file-name-completion-ignore-case t)
  (completion-category-overrides '((file (styles partial-completion))
				   (minibuffer (initials orderless)))))


;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure t
  :defer 2
  :config
  (savehist-mode))

;; Enable richer annotations using the Marginalia package

(use-package marginalia
  :pin melpa
  :ensure t
  :defer 1
  :custom (marginalia-annotators '(marginalia-annotators-light))
  :config
  (marginalia-mode))

(use-package which-key
  :ensure t
  :defer 4
  :diminish which-key-mode
  :config
  (which-key-mode 1))

(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary t)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

(use-package cape
  :ensure t
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line))
  )

(use-package treemacs
  :ensure t
  :defer t
  :custom
  (treemacs-no-png-images t)
  (treemacs-width 24)
  :bind ("C-c t" . treemacs))

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
  :diminish flyspell-mode
  :hook ((text-mode-hook . flyspell-mode)
	 (prog-mode-hook . flyspell-prog-mode))
  :custom
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil)
  (ispell-program-name "aspell")
  (ispell-dictionary "en_US")
  :config
  (defvar nf-ispell-dicts
    '(("English" . "en_US")
      ("Français" . "fr"))
    "Alist of languages dictionaries")

  (defun nf-ispell-dictionaries-complete ()
    "Select an item from `nf-ispell-dicts'."
    (interactive)
    (let* ((dicts (mapcar #'car nf-ispell-dicts))
           (choice (completing-read "Select dictionary: " dicts nil t))
           (key (cdr (assoc `,choice nf-ispell-dicts))))
      (ispell-change-dictionary key)
      (message "Switched to %s" key)))
  :bind ("C-x C-;" . nf-ispell-dictionaries-complete))

;; Syntax checking for GNU Emacs

(use-package flycheck
  :diminish flycheck-mode
  :ensure t
  :defer t
  :custom
  (flycheck-check-syntax-automatically '(mode-enabled save)) ; Check on save instead of running constantly
  :hook ((prog-mode-hook text-mode-hook) . flycheck-mode))

;;;========================================
;;; Org-mode
;;;========================================

(use-package org
  :ensure nil
  :delight "Οrg"
  :custom
  (org-imenu-depth 7)
  (org-fontify-done-headline nil)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line nil)
  (org-fontify-whole-block-delimiter-line t)
  
  (org-confirm-babel-evaluate nil)         ; Don't prompt before running code in org
  (org-src-fontify-natively t)             ; Use syntax highlighting in source blocks while editing
  (org-src-tab-acts-natively t)            ; Tabs act as 4 spaces in source blocks
  (org-src-preserve-indentation t)         ; Preserving indentation in source blocks
  (org-highlight-latex-and-related '(latex))    ; Coloring latex code in mode
  (org-latex-prefer-user-labels t)         ; Prefer user names and labels for references
  (org-cite-csl-styles-dir "~/Zotero/styles") ; Use Zotero styles for CSL exports (bibliography management)
  :config
  ;; Set :scale to 2 instead of 1 when org mode renders LaTeX
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))   ; Open PDF's with Emacs
  (setq org-format-latex-options '(:foreground default
					       :background default
					       :scale 2.0
					       :html-foreground "Black"
					       :html-background "Transparent"
					       :html-scale 1.0
					       :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

  (defun nf-toggle-presentation ()
    "Toggle between presentation and regular `org-mode'.
    The modes used are `olivetti-mode' and
    `org-tree-slide-mode'."
    (interactive)
    (if (bound-and-true-p olivetti-mode)
	(olivetti-mode -1)
      (olivetti-mode 1))
    (if (bound-and-true-p org-tree-slide-mode)
	(org-tree-slide-mode -1)
      (org-tree-slide-mode 1))

    (if (bound-and-true-p hide-mode-line-mode)
	(hide-mode-line-mode -1)
      (hide-mode-line-mode 1)))

  (defun nf/parse-headline (x)
    (plist-get (cadr x) :raw-value))

  (defun nf/get-headlines ()
    (org-element-map (org-element-parse-buffer) 'headline #'nf/parse-headline))

  (defun nf/link-to-headline ()
    "Insert an internal link to a headline."
    (interactive)
    (let* ((headlines (nf/get-headlines))
	   (choice (completing-read "Headings: " headlines nil t))
	   (desc (read-string "Description: " choice)))
      (org-insert-link buffer-file-name (concat "*" choice) desc)))

  :bind (:map org-mode-map
	      ("C-x p" . nf-toggle-presentation)))

;;; Add magnificent margins and custom blocks

(use-package org-special-block-extras
  :ensure t
  :defer t
  :hook (org-mode-hook . org-special-block-extras-mode))

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
  :custom
  (TeX-view-program-selection
   '(((output-dvi has-no-display-manager)  "dvi2tty")
     ((output-dvi style-pstricks)   "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "PDF Tools")
     (output-html "xdg-open"))))

(use-package template
  :after org ox-latex
  :load-path "latex")

(use-package ox-latex
  :after org
  :config
  (add-to-list 'org-latex-packages-alist
	       '("AUTO" "polyglossia" t ("xelatex" "lualatex")))
  
  (setq org-latex-minted-options '(("linenos=true") ("bgcolor=Periwinkle!5!white") ("breaklines=true")))

  (use-package engrave-faces
    :ensure t
    :custom
    (org-latex-src-block-backend 'engraved)
    :config
    (add-to-list 'org-latex-engraved-options '("numbers" . "left")))

  (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")))

(use-package cdlatex
  :ensure t
  :defer t
  :delight " cdlatex"
  :hook ((org-mode-hook . turn-on-org-cdlatex)    ; Enable cdlatex by default
	 (LaTex-mode-hook . turn-on-cdlatex)
	 (latex-mode-hook . turn-on-cdlatex))
  :bind (:map org-cdlatex-mode-map
	      ("`" . nil)))

(use-package org-ref
  :ensure t
  :defer t
  :config
  (setq org-export-before-parsing-functions '(org-ref-glossary-before-parsing
					      org-ref-acronyms-before-parsing)))

(use-package citeproc
  :ensure t
  :defer t
  :after org-ref)

(use-package shackle
  :ensure t
  :defer t
  :hook (org-mode-hook . shackle-mode)
  :config
  (setq shackle-rules
	'((pdf-view-mode :align right)))) ; Ensure PDF view opens on the right

(use-package org-roam
  :ensure t
  :defer t
  :diminish "-Ω-"
  :defines (org-roam-capture-templates org-roam-mode-map)
  :custom
  (org-roam-directory (file-truename (expand-file-name "~/org-roam")))
  (org-roam-db-location (expand-file-name "~/org-roam/org-roam.db"))
  :config
  (setq org-roam-capture-templates
	'(("d" "default" plain "%?"
           :if-new (file+head "fleeting/%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+title: ${title}\n#+created: %U \n#+last_modified: %U\n\n")
           :unnarrowed t)
	  ("c" "concept" plain "%?"
	   :if-new (file+head "concepts/${slug}.org"
	   "#+latex_class: notes_en\n#+title: ${title}\n#+author: Nathan Furnal\n#+filetags:\n#+created: %U\n#+last_modified: %U\n\n")
	   :unnarrowed t)
	  ("l" "literature" plain "%?"
	   :if-new (file+head "literature/${slug}.org"
	   "#+latex_class: notes_en\n#+title: ${title}\n#+author: Nathan Furnal\n#+filetags:\n#+created: %U\n#+las_modified: %U\n\n")
	   :unnarrowed t)))
  (org-roam-db-autosync-enable)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture)))


;;; Replacing deft with the faster alternative : xeft.
(use-package xeft
  :load-path "site-lisp/xeft"
  :ensure nil
  :defer t
  :custom
  (xeft-directory (expand-file-name "~/projects/notes"))
  (xeft-default-extension "org")
  (xeft-ignore-extension '("iimg" "md~" "tex" "tex~" "log" "gls" "glo" "glg" "org~"
			   "odt" "bbl" "ist" "qexams" "resums" "pdf" "class" "java"
			   "docx" "mw" "png" "jpg" "defs" "fls" "toc" "out" "fdb_latexmk"
			   "aux" "" "#" "pyg" "brf" "dvi" "html" "css" "js"))
  :commands xeft)

(use-package imenu-list
  :ensure t
  :defer t
  :bind ("C-é" . imenu-list-smart-toggle))

;;; Add presentation with Reveal.js

(use-package ox-reveal
  :ensure t
  :defer 5)

;;; Modern org

(use-package org-modern
  :ensure t
  :defer t
  :pin melpa
  :custom
  (org-modern-table nil)
  :hook ((org-mode-hook . org-modern-mode)
	 (org-agenda-finalize-hook . org-modern-agenda)))

;;;========================================
;;; Presentation
;;;========================================

(use-package org-tree-slide
  :ensure t
  :defer t
  :diminish
  :custom
  (org-tree-slide-breadcrumbs nil)
  (org-tree-slide-header nil)
  (org-tree-slide-slide-in-effect nil)
  (org-tree-slide-heading-emphasis nil)
  (org-tree-slide-cursor-init t)
  (org-tree-slide-modeline-display nil)
  (org-tree-slide-skip-done nil)
  (org-tree-slide-skip-comments t)
  (org-tree-slide-fold-subtrees-skipped t)
  (org-tree-slide-skip-outline-level 8)
  (org-tree-slide-never-touch-face t)
  (org-tree-slide-activate-message (format "Presentation %s" (propertize "ON" 'face 'success)))
  (org-tree-slide-deactivate-message (format "Presentation %s" (propertize "OFF" 'face 'error)))
  :defines org-tree-slide-mode-map
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
  :custom
  (olivetti-body-width 0.7)
  (olivetti-minimum-body-width 80)
  (olivetti-recall-visual-line-mode-entry-state t))

;;;========================================
;;; Reading
;;;========================================

(use-package pdf-tools
  :ensure t
  :defer t
  :magic ("%PDF" . pdf-view-mode)
  :hook (TeX-after-compilation-finished-hook . TeX-revert-document-buffer)
  :defines pdf-annot-activate-created-annotations
  :custom
  (pdf-view-display-size 'fit-page)
  ;; more fine-grained zooming
  (pdf-view-resize-factor 1.05)
  ;; create annotation on highlight
  (pdf-annot-activate-created-annotations t)
  :config
  (pdf-tools-install :no-query)
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
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-command "pandoc")
  :defines markdown-mode-map
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

;; Git integration for Emacs
;; Requires git

(use-package magit
  :ensure t
  :defer t
  :pin melpa
  :bind ("C-x g" . magit-status))

;;;========================================
;;; Editing
;;;========================================

(use-package iedit
  :ensure t
  :defer t
  :bind ("C-:" . iedit-mode))

(use-package rect
  :ensure nil
  :defer t
  :bind ("C-x <SPC>" . rectangle-mark-mode))

;;;========================================
;;; Development with Eglot
;;;========================================

(use-package eglot
  :ensure nil
  :defer t
  :custom
  (read-process-output-max (* 1024 1024))
  (eldoc-echo-area-use-multiline-p)
  (eglot-autoshutdown t)
  :config
  ;; Python specific
  (setq-default eglot-workspace-configuration
		'((:pyright .
			    ((useLibraryCodeForTypes . t)))))
  :hook ((zig-mode-hook . eglot-ensure)
	 (python-mode-hook . (lambda ()
			       (poetry-tracking-mode)
			       (eglot-ensure)))
	 (c-mode-hook . eglot-ensure)
	 (c++-mode-hook . eglot-ensure)))

;;;========================================
;;; (E)Lisp development
;;;========================================

(use-package elisp-mode
  :config
  (delight '((emacs-lisp-mode "EL" :major)
	     (lisp-interaction-mode "λ"))))

(use-package buttercup
  :ensure t
  :defer t)

(use-package package-lint
  :ensure t
  :defer t)

(use-package elisp-lint
  :ensure t
  :defer t)

;; Converts regex to `rx' syntax (very useful)
;; https://github.com/mattiase/xr
(use-package xr
  :ensure t
  :defer t)

;;;========================================
;;; (Common) Lisp
;;;========================================

(use-package lisp-mode
  :delight "CL")

(use-package sly
  :ensure t
  :defer t
  :custom
  (inferior-lisp-program "sbcl"))

(use-package paredit
  :diminish paredit-mode
  :delight " π"
  :ensure t
  :defer t
  :hook ((emacs-lisp-mode-hook lisp-mode-hook racket-mode-hook) . paredit-mode))

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
  :commands (geiser)
  :config
  (use-package geiser-mit
    :ensure t
    :defer t))

(use-package racket-mode
  :defer t
  :ensure t
  :delight "RKT"
  :mode ("\\.rkt\\'")
  :custom
  (racket-show-functions 'racket-show-echo-area)
  :defines racket-mode-map
  :bind (:map racket-mode-map
	      ("C-c C-c" . racket-run)
	      ("M-<RET>" . racket-eval-last-sexp)
	      ("M-l" . racket-insert-lambda))
  :hook ((racket-mode-hook .  racket-xp-mode)
	 (racket-repl-mode-hook . hide-mode-line-mode)))

;;;========================================
;;; Python
;;;========================================

(use-package python
  :ensure t
  :defer t
  :delight "Py"
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil))

;; Hide the modeline for inferior python processes

(use-package hide-mode-line
  :ensure t
  :defer t
  :hook ((inferior-python-mode-hook . hide-mode-line-mode)
	 (inferior-ess-r-mode-hook . hide-mode-line-mode)
	 (pdf-view-mode-hook . hide-mode-line-mode)
	 (shell-mode-hook . hide-mode-line-mode)))

(use-package poetry
  :ensure t
  :defer t
  :commands poetry-tracking-mode
  :config
  (setq poetry-tracking-strategy 'switch-buffer))

;; Buffer formatting on save
(use-package blacken
  :ensure t
  :defer t
  :custom
  (blacken-allow-py36 t)
  (blacken-skip-string-normalization t)
  (blacken-only-if-project-is-blackened t)
  (black-fast-unsafe t)
  :hook (python-mode-hook . blacken-mode))

;; numpy docstring for python
(use-package numpydoc
  :ensure t
  :defer t
  :custom
  (numpydoc-insert-examples-block nil)
  (numpydoc-template-long nil)
  :bind (:map python-mode-map
              ("C-c C-n" . numpydoc-generate)))

;;;========================================
;;; Org-mode Babel
;;;========================================

;; Org babel allows to evaluate code block in org-mode

;; R support
;; To install the R Kernel once R is installed, go in the R console and:
;; install.packages('IRkernel')
;; IRkernel::installspec()
;; To add linting, one has to install the "lintr" package and
;; Create a ~/.R/lintr_cache/ directory

(use-package ess
  :ensure t
  :defer t
  :mode ("\\.R\\'" . ess-r-mode))

;; Loading org babel languages one by one to defer them.

(use-package ob-latex
  :defer t
  :commands (org-babel-execute:latex))

(use-package ob-python
  :defer t
  :commands (org-babel-execute:python))

(use-package ob-R
  :defer t
  :commands (org-babel-execute:R))

(use-package ob-shell
  :defer t
  :commands
  (org-babel-execute:sh
   org-babel-expand-body:sh

   org-babel-execute:bash
   org-babel-expand-body:bash))

(use-package ob-sql
  :defer t
  :commands (org-babel-execute:sql))

(use-package ob-emacs-lisp
  :defer t
  :commands (org-babel-execute:elisp
	     org-babel-expand-body:elisp

	     org-babel-execute:emacs-lisp
	     org-babel-expand-body:emacs-lisp))

(use-package ob-C
  :defer t
  :commands  (org-babel-execute:C
	      org-babel-expand-body:C

	      org-babel-execute:C++
	      org-babel-expand-body:C++))

(use-package ob-maxima
  :defer t
  :commands (org-babel-execute:maxima))

(use-package ob-java
  :defer t
  :commands (org-babel-execute:java))

(use-package ob-ditaa
  :defer t
  :custom
  (org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")
  :commands (org-babel-execute:ditaa))

(use-package ob-plantuml
  :defer t
  :custom
  (org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
  :commands (org-babel-execute:plantuml))

(use-package ob-dot
  :defer t
  :commands (org-babel-execute:dot))

(use-package ob-gnuplot
  :defer t
  :commands (org-babel-execute:gnuplot
	     org-babel-expand-body:gnuplot))

(use-package ob-makefile
  :defer t)

(use-package ob-php
  :ensure t
  :defer t
  :commands (org-babel-execute:php))

(use-package ob-js
  :ensure nil
  :defer t
  :commands (org-babel-execute:js))

(use-package ob-scheme
  :ensure nil
  :defer t
  :commands (org-babel-execute:scheme))

(use-package ob-lisp
  :ensure nil
  :defer t
  :custom (org-babel-lisp-eval-fn 'sly-eval)
  :commands (org-babel-execute:lisp))

;;;========================================
;;; Web development
;;;========================================

;; LSP requirements on the server Prefer management with `nvm'(yay -Syu nvm) and
;; then add source /usr/share/nvm/init-nvm.sh to `.bashrc'.
;; npm install -g typescript prettier typescript-languageserver javascript-typescript-langserver

(use-package rjsx-mode
  :ensure t
  :defer t
  :mode ("\\.js\\'" . rjsx-mode)
  :hook (rjsx-mode-hook . prettier-js-mode))

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
  :after (rjsx-mode flycheck)
  :defines (tide-mode-map flycheck-check-syntax-automatically)
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1))

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
  :defines web-mode-map
  :mode ("\\.html\\'" "\\.blade.php\\'")
  :bind (:map web-mode-map
	      ("C-c C-v" . browse-url-of-buffer))
  :custom
  (web-mode-enable-current-column-highlight t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (js-indent-level 2)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-auto-expanding t)
  (web-mode-enable-css-colorization t)
  (browse-url-browser-function 'browse-url-chrome)
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
  :defines js2-mode-map
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
  :custom
  (emmet-indentation 2)
  (emmet-move-cursor-between-quotes t)
  ;; Auto-start on any markup modes
  :hook ((sgml-mode-hook web-mode-hook php-mode-hook) . emmet-mode))

(use-package php-mode
  :pin melpa
  :ensure t
  :defer t)

;;;========================================
;;; Lua
;;;========================================

(use-package lua-mode
  :ensure t
  :defer t
  :bind (:map lua-mode-map
	      ("C-c C-r" . lua-send-region)
	      ("C-c C-e" . lua-send-current-line)))

;;;========================================
;;; C/C++
;;;========================================

(use-package cc-mode
  :ensure nil
  :config
  (defun nf-compile-current-c/c++-file ()
    "Compiles a C/C++ file on the fly."
    (interactive)
    (let* ((clang-choices '(("c" . "clang --std=c17") ("cpp" . "clang++ --std=c++20")))
	   (filename (file-name-nondirectory buffer-file-name))
	   (file-ext (file-name-extension buffer-file-name))
	   (compile-choice (cdr (assoc file-ext clang-choices))))
      (compile (concat compile-choice " -Wall -pedantic " filename " -o " (file-name-sans-extension filename) ".o"))))

  (defun nf-run-exec-file ()
    "Runs an executable file named after the buffer if it exists."
    (interactive)
    (if (file-executable-p (concat (file-name-sans-extension buffer-file-name) ".o"))
	(async-shell-command
	 (concat "./" (file-name-nondirectory (file-name-sans-extension buffer-file-name)) ".o"))))

  (defun nf-compile-and-run ()
    "Compiles a C/C++ file then runs it."
    (interactive)
    (nf-compile-current-c/c++-file)
    (nf-run-exec-file))
  :hook ((c++-mode-hook . (lambda ()
			    (setq comment-start "/**")
			    (setq comment-end "*/")
			    (setq flycheck-clang-language-standard "c++20"
				  flycheck-gcc-language-standard "c++20")))
	 (c-mode-hook . (lambda ()
			  (setq flycheck-clang-language-standard "c17"
				flycheck-gcc-language-standard "c17"))))
  :bind ((:map c++-mode-map
	       ("C-c C-c" . nf-compile-current-c/c++-file)
	       ("C-c e" . nf-run-exec-file)
	       ("C-c o" . nf-compile-and-run))
	 (:map c-mode-map
	       ("C-c C-c" . nf-compile-current-c/c++-file)
	       ("C-c e" . nf-run-exec-file)
	       ("C-c o" . nf-compile-and-run))
	 (:map c-mode-base-map
	       ("C-c C-r" . recompile))))

;;; build systems

(use-package meson-mode
  :ensure t
  :defer t)

(use-package cmake-mode
  :ensure t
  :defer t)

;;;========================================
;;; Rust
;;;========================================

(use-package rust-mode
  :ensure t
  :defer t)

(use-package rustic
  :ensure t
  :defer t
  :custom
  (rustic-cargo-bin (expand-file-name "~/.cargo/bin/cargo")))

(use-package cargo
  :ensure t
  :defer t
  :hook (rust-mode-hook . cargo-minor-mode))

;;;========================================
;;; Zig
;;;========================================

(use-package zig-mode
  :ensure t
  :defer t)

;;;========================================
;;; OCaml
;;;========================================

(use-package tuareg 			; OCaml-mode
  :ensure t
  :defer t)

(use-package merlin
  :ensure t
  :defer t
  :hook ((tuareg-mode-hook ocaml-mode-hook) . merlin-mode))

(use-package ocamlformat
  :ensure t
  :defer t
  :custom (ocamlformat-enable 'enable-outside-detected-project)
  :hook (before-save-hook . ocamlformat-before-save))

(use-package reason-mode
  :ensure t
  :defer t)

;; utop configuration
(use-package utop
  :ensure t
  :defer t
  :config
  :hook (tuareg-mode-hook . utop-minor-mode))

;;;========================================
;;; Code snippets and skeletons
;;;========================================

(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode
  :functions yas-reload-all
  :config
  (yas-reload-all nil)
  :hook ((prog-mode-hook org-mode-hook) . yas-minor-mode))

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
;;; Blogging
;;;========================================

(use-package ox-hugo
  :ensure t
  :defer 3
  :after ox)

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
  :custom
  (plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
  (plantuml-default-exec-mode 'jar))

(use-package gnuplot
  :ensure t
  :defer t)

;;;========================================
;;; Assembly language
;;;========================================

(use-package nasm-mode
  :ensure t
  :defer t
  :delight "Νasm"
  :custom
  (nasm-basic-offset 4)
  :hook (nasm-mode-hook . flymake-mode))

(use-package masm-mode
  :ensure t
  :defer t
  :delight "Masm")

(use-package flymake-nasm
  :ensure t
  :defer t
  :hook (nasm-mode-hook . flymake-nasm-setup))

;;;========================================
;;; QoL
;;;========================================

;;; Shortcuts

(use-package handy
  :load-path "lisp/"
  :bind ("C-c I" . handy-find-user-init-file))

;;; Language parsing

(use-package tree-sitter
  :ensure t
  :defer t
  :delight " tree"
  :hook ((c-mode-hook c++-mode-hook css-mode-hook html-mode-hook
		      js2-mode-hook julia-mode-hook python-mode-hook rust-mode-hook
		      typescript-mode-hook sh-mode-hook tuareg-mode-hook zig-mode-hook scala-mode-hook ruby-mode-hook) . (lambda ()
						(tree-sitter-mode)
						(tree-sitter-hl-mode))))
(use-package tree-sitter-langs
  :ensure t
  :defer t)

(use-package treesit
  :commands (treesit-install-language-grammar nf/treesit-install-all-languages)
  :init
  (setq treesit-language-source-alist
   '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
     (c . ("https://github.com/tree-sitter/tree-sitter-c"))
     (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
     (css . ("https://github.com/tree-sitter/tree-sitter-css"))
     (go . ("https://github.com/tree-sitter/tree-sitter-go"))
     (html . ("https://github.com/tree-sitter/tree-sitter-html"))
     (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
     (json . ("https://github.com/tree-sitter/tree-sitter-json"))
     (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
     (make . ("https://github.com/alemuller/tree-sitter-make"))
     (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" "ocaml/src" "ocaml"))
     (python . ("https://github.com/tree-sitter/tree-sitter-python"))
     (php . ("https://github.com/tree-sitter/tree-sitter-php"))
     (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "typescript/src" "typescript"))
     (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
     (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
     (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
     (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
     (zig . ("https://github.com/GrayJack/tree-sitter-zig"))))
  :config
  (defun nf/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	      (treesit-install-language-grammar lang)
	      (message "`%s' parser was installed." lang)
	      (sit-for 0.75)))))

(use-package docstr
  :ensure t
  :defer t
  :custom
  (docstr-c++-style 'javadoc)
  :hook ((c-mode-hook c++-mode-hook) . docstr-mode))

;;; Colors

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode-hook . rainbow-delimiters-mode))


;;; CSV

(use-package csv-mode
  :ensure t
  :defer t)

;;;========================================
;;; Docker
;;;========================================

(use-package docker
  :ensure t
  :defer t)

(use-package dockerfile-mode
  :ensure t
  :defer t)

;;; init.el ends here
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3459d238cbac96e5d9bdfbd985165f12d3ffcaac944b61ca71bc6378912194ce" default))
 '(package-selected-packages
   '(csv-mode rainbow-delimiters dockerfile-mode docker docstr tree-sitter-langs tree-sitter flymake-nasm masm-mode nasm-mode gnuplot plantuml-mode yaml-mode maxima ox-hugo gif-screencast yasnippet-snippets utop reason-mode ocamlformat merlin tuareg zig-mode cargo rustic rust-mode cmake-mode meson-mode lua-mode php-mode emmet-mode nodejs-repl impatient-mode web-mode json-mode js2-refactor tide rjsx-mode ob-php ess numpydoc blacken poetry hide-mode-line racket-mode geiser-mit geiser paredit sly xr elisp-lint package-lint buttercup iedit magit pandoc-mode markdown-mode pdf-tools olivetti org-tree-slide org-modern ox-reveal imenu-list org-roam shackle org-ref cdlatex engrave-faces auctex org-special-block-extras flycheck transpose-frame treemacs cape corfu which-key marginalia orderless circadian modus-themes vertico vterm moody exec-path-from-shell gcmh delight diminish)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
