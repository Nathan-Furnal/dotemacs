;;; init.el --- Fun stuff all around -*- lexical-binding: t; -*-

;;; Commentary:
;; This file aims to provide a lightweight Emacs experience, it's heavily inspired from Prot's config as explained in the README.

;;; Code:

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Initialise the packages, avoiding a re-initialization.

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
  (setq use-package-compute-statistics t)
  ;; The following is VERY IMPORTANT.  Write hooks using their real name
  ;; instead of a shorter version: after-init ==> `after-init-hook'.
  ;;
  ;; This is to empower help commands with their contextual awareness,
  ;; such as `describe-symbol'.
  (setq use-package-hook-name-suffix nil))

(eval-when-compile
  (require 'use-package))

(use-package diminish :ensure t) ;; if you use :diminish
(use-package bind-key :ensure t) ;; if you use any :bind variant

;;;========================================
;;; Useful defaults
;;;========================================

(use-package emacs
  :init
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode 1)
  (set-face-attribute 'default nil :family "Roboto Mono" :height 100)
  (set-face-attribute 'fixed-pitch nil :family "Roboto Mono" :height 100)
  (set-face-attribute 'variable-pitch nil :family "Roboto Regular" :height 110)
  
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

  (defun my/electric-pair-local-text-mode ()
    "Advise and wrap electric pairs in text mode."
    (add-function :before-until electric-pair-inhibit-predicate
		  (lambda (c) (eq c ?<)))
    (electric-pair-local-mode))
  
  :hook ((prog-mode-hook . electric-pair-local-mode)
	 (text-mode-hook . my/electric-pair-local-text-mode)))

(use-package recentf
  ;; Loads after 2 second of idle time.
  :defer 2)

;; Feature that provides the ability to browse Emacs kill ring in autocomplete style popup menu.

(use-package popup-kill-ring
  :ensure t
  :defer 2
  :bind ("M-y" . popup-kill-ring))

;;;========================================
;;; Themes
;;;========================================

(use-package modus-operandi-theme
  :ensure t
  :init
  (setq modus-operandi-theme-org-blocks 'greyscale)
  (setq modus-operandi-theme-completions 'opinionated)
  (setq modus-operandi-theme-fringes 'subtle)
  (setq modus-operandi-theme-scale-headings t
	modus-operandi-theme-slanted-constructs t
	modus-operandi-theme-bold-constructs t
	modus-operandi-theme-faint-syntax nil
	modus-operandi-theme-intense-hl-line nil
	modus-operandi-theme-variable-pitch-headings t
	modus-operandi-theme-intense-paren-match t
	modus-operandi-theme-section-headings t)

  (setq modus-operandi-theme-scale-1 1.05
	modus-operandi-theme-scale-2 1.1
	modus-operandi-theme-scale-3 1.15
	modus-operandi-theme-scale-4 1.2
	modus-operandi-theme-scale-5 1.3)

  (setq modus-operandi-theme-headings
      '((1 . highlight)
        (2 . line)
        (t . rainbow-line-no-bold)))
  
  (run-at-time "08:00" (* 60 60 24)
               (lambda () (enable-theme 'modus-operandi)))
  :config
  
  (defadvice load-theme (before theme-dont-propagate activate)
    "Disable theme before loading new one."
    (mapc #'disable-theme custom-enabled-themes))
  (load-theme 'modus-operandi t))

(use-package modus-vivendi-theme
  :ensure t
  :init
  (setq modus-vivendi-theme-org-blocks 'greyscale)
  (setq modus-vivendi-theme-completions 'opinionated)
  (setq modus-vivendi-theme-fringes 'subtle)
  (setq modus-vivendi-theme-scale-headings t
	modus-vivendi-theme-slanted-constructs t
	modus-vivendi-theme-bold-constructs t
	modus-vivendi-theme-faint-syntax nil
	modus-vivendi-theme-intense-hl-line t
	modus-vivendi-theme-variable-pitch-headings t
	modus-vivendi-theme-intense-paren-match t
	modus-vivendi-theme-section-headings t)

   (setq modus-vivendi-theme-scale-1 1.05
	 modus-vivendi-theme-scale-2 1.1
	 modus-vivendi-theme-scale-3 1.15
	 modus-vivendi-theme-scale-4 1.2
	 modus-vivendi-theme-scale-5 1.3)

   (setq modus-vivendi-theme-headings
      '((1 . highlight)
        (2 . line)
        (t . rainbow-line-no-bold)))
  
  (run-at-time "20:00" (* 60 60 24)
               (lambda () (enable-theme 'modus-vivendi)))
  :config
  
  (defadvice load-theme (before theme-dont-propagate activate)
    "Disable theme before loading new one."
    (mapc #'disable-theme custom-enabled-themes))
  (load-theme 'modus-vivendi t))

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
  :config
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
  :after imenu
  :bind ("C-c i" . imenu-list))

(use-package flimenu
  :ensure t
  :defer t
  :after imenu
  :config
  (flimenu-global-mode 1))

(use-package ctrlf
  :ensure t
  :config (ctrlf-mode))

(use-package ibuffer
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-display-summary nil)
  (setq ibuffer-use-other-window nil)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-movement-cycle nil)
  (setq ibuffer-default-sorting-mode 'filename/process)
  (setq ibuffer-use-header-line t)
  (setq ibuffer-default-shrink-to-minimum-size nil)
  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 30 30 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename)))
  (setq ibuffer-saved-filter-groups nil)
  (setq ibuffer-old-time 48)

  :hook (ibuffer-mode-hook . hl-line-mode)
  :bind ("C-x C-b" . ibuffer))

(use-package which-key
  :ensure t
  :defer 1
  :diminish which-key-mode
  :config
  (which-key-mode 1))

(use-package company
  :ensure t
  :defer t
  :diminish
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
;;; Navigation & Completion
;;;========================================

;; icomplete-vertical will be included in emacs in the future
;; See https://github.com/oantolin/icomplete-vertical/issues/14

(use-package icomplete-vertical
  :disabled
  :ensure t
  :demand t
  :custom
  (completion-styles '(partial-completion substring))
  (completion-category-overrides '((file (styles basic substring))))
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (resize-mini-windows 'grow-only)
  :config
  (fido-mode)
  (icomplete-vertical-mode)
  :bind (:map icomplete-minibuffer-map
              ("<down>" . icomplete-forward-completions)
              ("C-n" . icomplete-forward-completions)
              ("<up>" . icomplete-backward-completions)
              ("C-p" . icomplete-backward-completions)
              ("C-v" . icomplete-vertical-toggle)
	      ("<tab>" . icomplete-fido-ret)))

;;;========================================
;;; Windows & movement
;;;========================================

(use-package windmove
  :config
  (setq windmove-create-window nil)     ; Emacs 27.1
  :bind (("C-c <up>" . windmove-up)
         ("C-c <right>" . windmove-right)
         ("C-c <down>" . windmove-down)
         ("C-c <left>" . windmove-left)))

(use-package transpose-frame
  :ensure t
  :defer 2
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

  (defvar my/ispell-dicts
    '(("English" . "en_US")
      ("Français" . "fr"))
    "Alist of languages dictionaries")

  (defun my/ispell-dictionaries-complete ()
    "Select an item from `my/ispell-dicts'."
    (interactive)
    (let* ((dicts (mapcar #'car my/ispell-dicts))
           (choice (completing-read "Select dictionary: " dicts nil t))
           (key (cdr (assoc `,choice my/ispell-dicts))))
      (ispell-change-dictionary key)
      (message "Switched to %s" key)))

  :bind ("C-x C-;" . my/ispell-dictionaries-complete))


;; Syntax checking for GNU Emacs

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))) ; Check on save instead of running constantly

;;;========================================
;;; Org-mode
;;;========================================

(use-package org
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

  
  ;; Setting macros that can be expanded
  ;; Ref in the manual https://orgmode.org/manual/Macro-Replacement.html
  ;; Good ref in a blog post : https://bnolet.me/posts/2019/06/macros-in-org-mode/
  (setq org-export-global-macros
	'(("glossentry" . "#+latex_header_extra: \\newglossaryentry{$1}{name=$2, description={$3}}")))
  :bind (:map org-mode-map
	      ("C-c i" . imenu-list)))


;; Custome LaTeX templates
;; Requires a full LaTeX install, usually called `texlive'.
;; The arch wiki https://wiki.archlinux.org/index.php/TeX_Live details how to use it
;; latex compilation found at https://github.com/jkitchin/org-ref/blob/master/org-ref.org
;; Better latexmk for glossaries with a ~/.latexmkrc file. Explained at `https://tex.stackexchange.com/a/44316/223017'.

(use-package auctex
  :ensure t
  :defer t)

(use-package ox-latex
  :after org
  :defer t
  :config
  (add-to-list 'org-latex-packages-alist
	       '("AUTO" "polyglossia" t ("xelatex" "lualatex")))
  (setq
   org-latex-listings 'minted
   org-latex-minted-options '(("linenos=true") ("bgcolor=gray!10!white")))

  (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))


  (add-to-list 'org-latex-classes
	       '("notes"
		 "\\documentclass[french]{article}
\\usepackage{minted}
\\usepackage[round]{natbib}
\\usepackage{babel}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{mathptmx}
\\usepackage{booktabs}
\\usepackage{longtable}
\\usepackage[skip=1.\\baselineskip]{caption}
\\usepackage{soul}
\\usepackage[usenames,dvipsnames,svgnames]{xcolor}
\\usepackage{parskip}
\\usepackage[many]{tcolorbox}
\\usepackage{hyperref}
\\usepackage[export]{adjustbox}
\\usepackage{subcaption}
\\hypersetup{
    colorlinks=true,
    linkcolor={PineGreen!30!black},
    citecolor={Bittersweet!50!Sepia},
    urlcolor={blue!80!black}}
\\usepackage{geometry}
\\geometry{a4paper,left=2.5cm,top=1.2cm,right=2.5cm,bottom=1.5cm,marginparsep=7pt, marginparwidth=.6in}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
	       '("notes_en"
		 "\\documentclass{article}
\\usepackage{minted}
\\usepackage[round]{natbib}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{mathptmx}
\\usepackage{booktabs}
\\usepackage{longtable}
\\usepackage[skip=1.\\baselineskip]{caption}
\\usepackage{soul}
\\usepackage[usenames,dvipsnames,svgnames]{xcolor}
\\usepackage{parskip}
\\usepackage[many]{tcolorbox}
\\usepackage{hyperref}
\\hypersetup{
    colorlinks,
    linkcolor={PineGreen!30!black},
    citecolor={DarkGray},
    urlcolor={blue!80!black}}
\\usepackage[export]{adjustbox}
\\usepackage{subcaption}
\\usepackage{geometry}
\\geometry{a4paper,left=2.5cm,top=1.2cm,right=2.5cm,bottom=1.5cm,marginparsep=7pt, marginparwidth=.6in}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))


(use-package cdlatex
  :ensure t
  :defer t
  :hook ((org-mode-hook . turn-on-org-cdlatex)    ; Enable cdlatex by default
	 (LaTex-mode-hook . turn-on-cdlatex)
	 (latex-mode-hook . turn-on-cdlatex)))

(use-package org-ref
  :ensure t
  :defer 2)
  
(use-package pdf-tools
  :ensure t
  :mode  ("\\.pdf\\'" . pdf-view-mode)
  :bind  (:map pdf-view-mode-map
	       ("C-s" . isearch-forward))
  :hook (TeX-after-compilation-finished-hook . TeX-revert-document-buffer)
  :config
  (setq-default pdf-view-display-size 'fit-page)

  ;; more fine-grained zooming
  
  (setq pdf-view-resize-factor 1.05)

  ;; create annotation on highlight
  
  (setq pdf-annot-activate-created-annotations t)
   
  (pdf-tools-install :no-query)
  (require 'pdf-occur))

(use-package shackle
  :ensure t
  :defer t
  :hook ((org-mode-hook . shackle-mode)
	 (prog-mode-hook . shackle-mode))
  :config
  (setq shackle-rules
	'((pdf-view-mode :align right))))                                   ; Ensure PDF view opens on the right

;; Emacs document annotator, using Org-mode.

(use-package org-noter
  :ensure t
  :defer t
  :config
  (setq org-noter-notes-search-path '("~/Documents" "~/Notes")))


;; Adding Deft an easy way to go through files and create notes on the fly
;; Source : https://jblevins.org/projects/deft/

(use-package deft
  :ensure t
  :defer t
  :config (setq deft-default-extension "org"
	        deft-directory "~/Notes"
		deft-use-filter-string-for-filename t
		deft-recursive t                                   ; Allows searching through sub-directories
		deft-use-filename-as-title t)                      ; use filename instead of first line of doc
  :bind ("C-c d" . deft))

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
  :defer nil
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

(use-package ede
  :ensure nil
  :defer t)

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
  :hook ((css-mode-hook . lsp-deferred)
	 (html-mode-hook . lsp-deferred)
	 (web-mode-hook . lsp-deferred)
	 (js2-mode-hook . lsp-deferred)
	 (c++-mode-hook . lsp-deferred)
	 (c-mode-hook . lsp-deferred)
	 (java-mode-hook . lsp-deferred)
	 (python-mode-hook . lsp-deferred)
	 (sql-mode-hook . lsp-deferred)
	 (lsp-mode-hook . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :bind (:map lsp-mode-map
	      ("M-<RET>" . lsp-execute-code-action))
  :config
  (setq lsp-prefer-capf t
	lsp-keep-workspace-alive nil
	lsp-auto-guess-root nil)
  (add-hook 'lsp-completion-mode-hook
	    (lambda ()
	      (when (eq (car company-backends) 'company-capf)
		(setq company-backends (cdr company-backends)))))
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
  :after lsp-mode
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
  :defer t
  :bind ("C-$" . eshell))


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
  :hook (lisp-mode-hook . paredit-mode))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (lisp-mode-hook . rainbow-delimiters-mode))

;;;========================================
;;; Clojure
;;;========================================

(use-package clojure-mode
  :ensure t
  :defer t
  :config
  (require 'flycheck-clj-kondo)
  :hook (clojure-mode-hook . paredit-mode))

(use-package cider
  :ensure t
  :defer t
  :hook (clojure-mode-hook . cider-mode))

;;;========================================
;;; Scheme
;;;========================================

(use-package scheme
  :defer t
  :ensure nil
  :mode ("\\.scm$\\'")
  :hook (scheme-mode-hook . paredit-mode))

(use-package geiser
  :ensure t
  :defer t
  :config
  (setq geiser-mit-binary "/usr/bin/mit-scheme"
	geiser-active-implementations '(mit)))

;;;========================================
;;; Julia
;;;========================================

;; Requires a Julia install

(use-package julia-mode
  :ensure t
  :defer t
  :mode ("\\.jl\\'" . julia-mode)
  :init
  (setq inferior-julia-program "/usr/bin/julia")
  (setenv "JULIA_NUM_THREADS" "16")
  :hook (julia-mode-hook . julia-repl-mode))

(use-package julia-repl
  :requires julia-mode
  :ensure t
  :defer t)

;; ========================================
;; Python
;; ========================================

(use-package python
  :ensure t
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil))

(use-package python-mode
  :ensure t
  :defer t
  :mode "\\.py\\'")

;; Hide the modeline for inferior python processes
(use-package inferior-python-mode
  :ensure nil
  :hook ((inferior-python-mode-hook . hide-mode-line-mode)
	 (inferior-ess-r-mode-hook . hide-mode-line-mode)))

(use-package hide-mode-line
  :ensure t
  :defer t)

(use-package pyvenv
  :ensure t
  :defer t
  :config
  ;; Setting work on to easily switch between environments
  (setenv "WORKON_HOME" (expand-file-name "~/miniconda3/envs/"))
  ;; Display virtual envs in the menu bar
  (setq pyvenv-menu t)
  ;; Restart the python process when switching environments
  (add-hook 'pyvenv-post-activate-hooks (lambda ()
					  (pyvenv-restart-python)))
  :hook (python-mode-hook . pyvenv-mode))

(use-package lsp-pyright
  :ensure t
  :defer t
  :config
  (setq lsp-clients-python-library-directories '("/usr/" "~/miniconda3/pkgs"))
  (setq lsp-pyright-disable-language-service nil
	lsp-pyright-disable-organize-imports nil
	lsp-pyright-auto-import-completions t
	lsp-pyright-use-library-code-for-types t
	lsp-pyright-venv-path "~/miniconda3/envs")
  (defun my/use-ipython ()
    "Enable IPython as shell interpreter."
    (interactive)
    (setq python-shell-interpreter "ipython"
	  python-shell-interpreter-args "-i --simple-prompt"))
  :hook ((python-mode-hook . (lambda () (require 'lsp-pyright) (lsp-deferred)))
	 (python-mode-hook . my/use-ipython)))

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
  :after (ob)
  :init
  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                       (:session . "py")))
  
  (setq org-babel-default-header-args:jupyter-julia '((:async . "yes")
                                                      (:session . "jl")
                                                      (:kernel . "julia-1.5")))
  
  (setq org-babel-default-header-args:jupyter-R '((:async . "yes")
						  (:session . "R")
						  (:kernel . "ir"))))

(use-package ob
  :after org
  :defer nil
  :config
  (setq org-babel-load-languages
	'((latex . t)
	  (python . t)
	  (R . t)
	  (shell . t)
	  (sql . t)
	  (emacs-lisp . t)
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
  :after lsp
  :mode ("\\.java\\'")
  :config
  (setq lsp-java-format-on-type-enabled nil)
  (defun my/java-mode-hook ()
    (setq c-basic-offset 2
          c-label-offset 0
          tab-width 2
          indent-tabs-mode nil
	  require-final-newline nil))
  :hook (java-mode-hook . (lsp my/java-mode-hook)))

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
   (before-save-hook . tide-format-before-save)))

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

;; Evaluate JS from Emacs
;; have `skewer-mode' enabled and `run-skewer'

(use-package skewer-mode
  :ensure t
  :defer t
  :hook ((js2-mode-hook . skewer-mode)))

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
;;; Code snippets and skeletons
;;;========================================

(use-package yasnippet
  :ensure t
  :defer 5
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

(use-package keycast
  :after doom-modeline
  :ensure t
  :defer t
  :init
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (add-hook 'pre-command-hook 'keycast-mode-line-update t)
      (remove-hook 'pre-command-hook 'keycast-mode-line-update)))
  :config
  (add-to-list 'global-mode-string '("" mode-line-keycast)))

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

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(geiser treemacs-projectile projectile pyvenv jupyter yaml-mode gcmh rainbow-delimiters paredit maxima marginalia flycheck-clj-kondo yapfify python lsp-pyright python-mode keycast gif-screencast yasnippet-snippets emmet-mode skewer-mode impatient-mode web-mode json-mode js2-refactor tide prettier-js rjsx-mode lsp-java ess hide-mode-line elpy julia-repl julia-mode cider clojure-mode sly elisp-lint package-lint buttercup dap-mode lsp-treemacs lsp-ui lsp-mode treemacs iedit multiple-cursors magit pandoc-mode markdown-mode deft org-noter shackle org-ref cdlatex auctex flycheck transpose-frame company which-key ctrlf flimenu imenu-list selectrum-prescient selectrum centaur-tabs doom-modeline modus-vivendi-theme modus-operandi-theme popup-kill-ring diminish use-package))
 '(safe-local-variable-values '((geiser-scheme-implementation quote mit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
