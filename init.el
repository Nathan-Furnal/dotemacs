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
  (use-package-hook-name-suffix nil)
  (use-package-compute-statistics t))

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("elpa" . "https://elpa.gnu.org/packages/")))

(unless (bound-and-true-p package--initialized)
  (package-initialize))

(use-package diminish :ensure t :after use-package) ;; if you use :diminish
(use-package bind-key :ensure t :after use-package) ;; if you use any :bind variant

;;;========================================
;;; Useful defaults
;;;========================================

(use-package emacs
  :custom
  (browse-url-browser-function 'browse-url-firefox)
  (browse-url-new-window-flag  t)
  (browse-url-firefox-new-window-is-tab t)
  (package-install-upgrade-built-in t)
  :init
  (set-face-attribute 'default nil :family "Iosevka Term" :height 110 :weight 'regular)
  (set-face-attribute 'fixed-pitch nil :family "Iosevka Term" :height 110 :weight 'medium)
  (set-face-attribute 'variable-pitch nil :family "Iosevka Etoile" :height 100 :weight 'medium)
  (setq initial-major-mode 'fundamental-mode)   ; No need to have an Elisp buffer when starting up
  (setq-default cursor-type 'bar)               ; Line-style cursor similar to other text editors
  (setq initial-scratch-message
	"Welcome to Emacs!")	                ; Make *scratch* buffer have a welcome message
  (setq-default frame-title-format '("%b"))     ; Make window title the buffer name
  (setq-default fill-column 80)		        ; Set fill column to 80 rather than 70, in all cases.
  (setq inhibit-startup-screen t)               ; Disable startup screen
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8-unix)
  (setq confirm-kill-processes nil)		; Stop confirming the killing of processes
  (setq use-short-answers t)                    ; y-or-n-p makes answering questions faster
  (show-paren-mode t)                           ; Visually indicates pair of matching parentheses
  (delete-selection-mode t)                     ; Start writing straight after deletion
  (put 'narrow-to-region 'disabled nil)	        ; Allows narrowing bound to C-x n n (region) and C-x n w (widen)
  (setq read-process-output-max (* 1024 1024))  ; Increase the amount of data which Emacs reads from the process
  (global-hl-line-mode 1)			; Highlight the current line to make it more visible
  (setq create-lockfiles nil)                   ; lock files kill `npm start'
  (pixel-scroll-precision-mode 1)	        ; Precision scrolling

  (setq backup-directory-alist
	`(("." . ,(concat user-emacs-directory "backups"))))

  (dolist  (mapping '((python-mode . python-ts-mode)
		      (ruby-mode . ruby-ts-mode)
		      (c-mode . c-ts-mode)
		      (c++-mode . c++-ts-mode)
		      (c-or-c++-mode . c-or-c++-ts-mode)
		      (css-mode . css-ts-mode)
		      (js-mode . js-ts-mode)
		      (javascript-mode . js-ts-mode)
		      (typescript-mode . tsx-ts-mode)
		      (js-json-mode . json-ts-mode)
		      (sh-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  
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
	   ("C-l" . duplicate-dwim))
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
  :defer t
  :hook (after-init-hook . exec-path-from-shell-initialize))

(use-package eldoc
  :diminish eldoc-mode)

(use-package autorevert
  :defer 2
  :diminish auto-revert-mode)

(use-package recentf
  :defer 2)

;; Better shell

(use-package vterm
  :ensure t
  :defer t
  :bind ("C-$" . vterm))

(use-package vertico
  :ensure t
  :pin elpa
  :init
  (vertico-mode)
  :config
  (vertico-multiform-mode 1)
  (add-to-list 'vertico-multiform-categories
             '(jinx grid (vertico-grid-annotate . 20))))

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
	modus-themes-variable-pitch-ui nil
	modus-themes-common-palette-overrides '((bg-mode-line-active bg-main)
						(bg-mode-line-inactive bg-dim)
						(border-mode-line-inactive bg-inactive)
						(fringe subtle)
						(bg-paren-match bg-yellow-intense)
						(custom-set-faces
						 '(mode-line ((t :family "Iosevka Etoile" :height 100 :weight 'regular))))))
  (setq modus-themes-headings
        (quote ((1 . (overline variable-pitch 1.4))
                (2 . (overline variable-pitch 1.25))
                (3 . (overline 1.1))
                (t . (monochrome))))))

;; Running modus-themes depending on the time of the day.

(use-package solar
  :custom
  (calendar-latitude 50.85)
  (calendar-longitude 4.35))

(use-package circadian
  :ensure t
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
  :custom (marginalia-annotators '(marginalia-annotators-light))
  :init
  (marginalia-mode))

(use-package which-key
  :ensure t
  :defer 4
  :diminish which-key-mode
  :config
  (which-key-mode 1))

(use-package corfu
  :pin elpa
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

(use-package deadgrep
  :ensure t
  :defer t
  :bind ("M-s o" . deadgrep))

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

(use-package jinx
  :ensure t
  :defer t
  :hook ((text-mode-hook prog-mode-hook) . jinx-mode)
  :bind
  (("C-x C-;" . jinx-languages)
   ("M-$" . jinx-correct)))

;; Syntax checking for GNU Emacs

(use-package flycheck
  :diminish flycheck-mode
  :ensure t
  :defer t
  :custom
  (flycheck-check-syntax-automatically '(mode-enabled save)) ; Check on save instead of running constantly
  :hook ((prog-mode-hook text-mode-hook) . flycheck-mode))

(use-package flymake
  :ensure t
  :defer t)

;; Grammar checking

(use-package langtool
  :ensure t
  :defer t
  :custom
  (langtool-language-tool-jar "/usr/share/java/languagetool/languagetool-commandline.jar")
  (langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*")
  :commands
  (langtool-check
   langtool-correct-buffer
   langtool-show-message-at-point
   langtool-check-done))

;;;========================================
;;; Org-mode
;;;========================================

(use-package org
  :ensure nil
  :diminish "Οrg"
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
  (citar-citeproc-csl-styles-dir (expand-file-name "~/Zotero/styles"))
  (org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
  :config
  ;; Set :scale to 2 instead of 1 when org mode renders LaTeX
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))   ; Open PDF's with Emacs

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
	      ("C-x p" . nf-toggle-presentation))
  :hook (org-mode-hook . (lambda ()
			   (variable-pitch-mode t)
			   (setq-default fill-column 100))))

(use-package ox-latex
  :after org
  :config
  (use-package engrave-faces
    :ensure t
    :custom
    (org-latex-src-block-backend 'engraved)
    :config
    (add-to-list 'org-latex-engraved-options '("numbers" . "left"))))

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
  :defer t
  :hook (org-mode-hook . (lambda ()
			   (load-file (expand-file-name (concat user-emacs-directory "latex/template.el"))))))

(use-package cdlatex
  :ensure t
  :defer t
  :diminish " cdlatex"
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

(use-package citar
  :ensure t
  :defer t
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

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
	   "#+latex_class: el-notes\n#+title: ${title}\n#+author: Nathan Furnal\n#+filetags:\n#+created: %U\n#+last_modified: %U\n\n")
	   :unnarrowed t)
	  ("l" "literature" plain "%?"
	   :if-new (file+head "literature/${slug}.org"
	   "#+latex_class: el-notes\n#+title: ${title}\n#+author: Nathan Furnal\n#+filetags:\n#+created: %U\n#+las_modified: %U\n\n")
	   :unnarrowed t)))
  (org-roam-db-autosync-enable)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture)))


;;; Replacing deft with the faster alternative : xeft.
(use-package xeft
  :pin elpa
  :ensure t
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
  :pin elpa
  :ensure t
  :defer t
  :custom
  (read-process-output-max (* 1024 1024))
  (eldoc-echo-area-use-multiline-p)
  (eglot-autoshutdown t)
  :config
  ;; Python specific
  (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
  (setq-default eglot-workspace-configuration
		'((:pyright .
			    ((useLibraryCodeForTypes . t)))))
  :hook ((zig-mode-hook . eglot-ensure)
	 (python-base-mode-hook . (lambda ()
				    (poetry-tracking-mode)
				    (eglot-ensure)))
	 (c-ts-mode-hook . eglot-ensure)
	 (c++-ts-mode-hook . eglot-ensure)
	 (kotlin-ts-mode-hook . eglot-ensure)
	 (rustic-mode-hook . eglot-ensure)
	 (css-ts-mode-hook . eglot-ensure)
	 (html-mode-hook . eglot-ensure)
	 (js-base-mode-hook . eglot-ensure)
	 (tsx-ts-mode-hook . eglot-ensure)
	 (php-mode-hook . eglot-ensure)
	 (latex-mode-hook . eglot-ensure)
	 (julia-ts-mode-hook . (lambda ()
				 (eglot-jl-init)
				 (eglot-ensure))))
  :bind (("C-c l b" . eglot-format-buffer)
	 ("C-c l a" . eglot-code-actions)
	 ("C-c l e" . eglot-reconnect)
	 ("C-c l r" . eglot-rename)))

;;;========================================
;;; (E)Lisp development
;;;========================================

(use-package elisp-mode
  :config
  :diminish "EL")

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
  :diminish "CL")

(use-package sly
  :ensure t
  :defer t
  :custom
  (inferior-lisp-program "sbcl"))

(use-package puni
  :diminish puni-mode
  :ensure t
  :defer t
  :hook ((emacs-lisp-mode-hook lisp-mode-hook racket-mode-hook) . puni-mode))

;;;========================================
;;; Scheme & Racket
;;;========================================

(use-package scheme
  :defer t
  :ensure nil
  :mode ("\\.scm$\\'")
  :hook (scheme-mode-hook . puni-mode))

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
  :diminish "RKT"
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
  :hook (python-base-mode-hook . blacken-mode))

;; numpy docstring for python
(use-package numpydoc
  :ensure t
  :defer t
  :config
  (setq numpydoc-insert-examples-block nil
	numpydoc-template-long nil)
  :bind (:map python-base-mode-map
              ("C-c C-n" . numpydoc-generate)))

;;;========================================
;;; Julia
;;;========================================

(use-package julia-ts-mode
  :ensure t
  :defer t)

(use-package julia-vterm
  :ensure t
  :defer t
  :hook (julia-ts-mode-hook . julia-vterm-mode))

(use-package eglot-jl
  :ensure t
  :defer t)

;;;========================================
;;; PHP
;;;========================================

(use-package php-mode
  :pin elpa
  :ensure t
  :defer t)

;;;========================================
;;; Org-mode Babel
;;;========================================

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
;;; Lua
;;;========================================

(use-package lua-mode
  :ensure t
  :defer t
  :bind (:map lua-mode-map
	      ("C-c C-r" . lua-send-region)
	      ("C-c C-e" . lua-send-current-line)))

;;;========================================
;;; Kotlin
;;;========================================

(use-package kotlin-ts-mode
  :ensure t
  :defer t)

;;;========================================
;;; Rust
;;;========================================

(use-package rustic
  :custom
  (rustic-lsp-client 'eglot)
  :ensure t
  :defer t)

(use-package cargo
  :ensure t
  :defer t
  :hook ((rust-ts-mode-hook rustic-mode-hook) . cargo-minor-mode))

;;;========================================
;;; Zig
;;;========================================

(use-package zig-mode
  :ensure t
  :defer t)

;;;========================================
;;; Code snippets and skeletons
;;;========================================

;; Configure Tempel
(use-package tempel
  :ensure t
  :defer t
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-=" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))
  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  :hook
  (prog-mode-hook . tempel-setup-capf)
  (text-mode-hook . tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
)

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection
  :ensure t
  :after tempel)

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
  :diminish "Νasm"
  :custom
  (nasm-basic-offset 4)
  :hook (nasm-mode-hook . flymake-mode))

(use-package masm-mode
  :ensure t
  :defer t
  :diminish "Masm")

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

;;; Multiple cursors

(use-package multiple-cursors
  :ensure t
  :defer t
  :bind
  (("C-<" . mc/mark-all-like-this-dwim)
   ("C->" . mc/mark-all-dwim)))

;;; Language parsing

(use-package tree-sitter
  :ensure t
  :defer t
  :diminish " tree"
  :hook ((zig-mode-hook) . (lambda ()
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
     (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
     (go . ("https://github.com/tree-sitter/tree-sitter-go"))
     (html . ("https://github.com/tree-sitter/tree-sitter-html"))
     (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
     (json . ("https://github.com/tree-sitter/tree-sitter-json"))
     (julia . ("https://github.com/tree-sitter/tree-sitter-julia"))
     (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
     (make . ("https://github.com/alemuller/tree-sitter-make"))
     (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" "master" "ocaml/src"))
     (python . ("https://github.com/tree-sitter/tree-sitter-python"))
     (php . ("https://github.com/tree-sitter/tree-sitter-php"))
     (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
     (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
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

(use-package cmake-ts-mode
  :ensure t
  :defer t)

(use-package combobulate
  :load-path "site-lisp/combobulate"
  :defer t
  :hook ((python-ts-mode-hook . combobulate-mode)
         (js-ts-mode-hook . combobulate-mode)
         (css-ts-mode-hook . combobulate-mode)
         (yaml-ts-mode-hook . combobulate-mode)
         (typescript-ts-mode-hook . combobulate-mode)
         (tsx-ts-mode-hook . combobulate-mode)))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(zig-mode yaml-mode xr xeft which-key vertico verilog-mode use-package-ensure-system-package treemacs tree-sitter-langs transpose-frame tramp tempel-collection soap-client sly shackle rustic rainbow-delimiters racket-mode puni poetry plantuml-mode php-mode pdf-tools pandoc-mode ox-reveal ox-hugo org-tree-slide org-special-block-extras org-roam org-ref org-modern orderless olivetti numpydoc nasm-mode multiple-cursors modus-themes maxima masm-mode marginalia magit lua-mode langtool kotlin-ts-mode julia-vterm julia-ts-mode jinx imenu-list iedit hide-mode-line graphviz-dot-mode gnuplot gif-screencast geiser-mit gcmh flymake-nasm flycheck faceup exec-path-from-shell engrave-faces emacsql-sqlite-builtin elisp-lint eglot-jl dockerfile-mode docker diminish deadgrep csv-mode corfu citar circadian cdlatex cargo cape buttercup bqn-mode blacken bibtex-capf auctex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
