;;; post-init.el --- Init -*- no-byte-compile: t; lexical-binding: t; -*-

;; Author: Nathan Furnal
;; URL:  https://gitlab.com/nathanfurnal/dotemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: main
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; My Emacs config

;;; Code:

(use-package compile-angel
  :ensure t
  :demand t
  :custom
  ;; Set `compile-angel-verbose` to nil to suppress output from compile-angel.
  ;; Drawback: The minibuffer will not display compile-angel's actions.
  (compile-angel-verbose t)

  :config
  ;; The following directive prevents compile-angel from compiling your init
  ;; files. If you choose to remove this push to `compile-angel-excluded-files'
  ;; and compile your pre/post-init files, ensure you understand the
  ;; implications and thoroughly test your code. For example, if you're using
  ;; `use-package', you'll need to explicitly add `(require 'use-package)` at
  ;; the top of your init file.
  (push "/init.el" compile-angel-excluded-files)
  (push "/early-init.el" compile-angel-excluded-files)
  (push "/pre-init.el" compile-angel-excluded-files)
  (push "/post-init.el" compile-angel-excluded-files)
  (push "/pre-early-init.el" compile-angel-excluded-files)
  (push "/post-early-init.el" compile-angel-excluded-files)

  ;; A local mode that compiles .el files whenever the user saves them.
  ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)

  ;; A global mode that compiles .el files before they are loaded.
  (compile-angel-on-load-mode))

;;;========================================
;;; Defaults
;;;========================================
(use-package use-package
  :custom
  (use-package-compute-statistics t))
(use-package diminish :ensure t :after use-package) ;; if you use :diminish
(use-package bind-key :ensure t :after use-package) ;; if you use any :bind variant

;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(add-hook 'after-init-hook #'global-auto-revert-mode)

;; recentf is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(add-hook 'after-init-hook #'(lambda()
                               (let ((inhibit-message t))
                                 (recentf-mode 1))))

(with-eval-after-load "recentf"
  (add-hook 'kill-emacs-hook #'recentf-cleanup))

;; savehist is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(add-hook 'after-init-hook #'savehist-mode)

;; save-place-mode enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(add-hook 'after-init-hook #'save-place-mode)

;;;========================================
;;; Better defaults
;;;========================================

;;; Improve defaults with packages (not builtins)

;; Better shell
(use-package vterm
  :pin melpa
  :ensure t
  :defer t
  :custom-face (vterm-face ((t (:family "Iosevka Nerd Font"))))
  :bind ("C-$" . vterm)
  ;; Speed up vterm
  :config
  (setq vterm-timer-delay 0.01))

(use-package fish-mode
  :ensure t
  :defer t)

;; Vertical minibuffer
(use-package vertico
  :pin gnu  
  :ensure t  
  :hook (after-init . vertico-mode)
  :config
  (vertico-multiform-mode 1)
  (add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 20))))

;; Language parsing with tree-sitter and tree-sitter based modes
(use-package treesit-auto
  :pin melpa
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (add-to-list 'treesit-load-name-override-list
               '(janet "libtree-sitter-janet-simple" "tree_sitter_janet_simple"))
  (add-to-list 'treesit-language-source-alist
             '(janet-simple . ("https://github.com/sogaiu/tree-sitter-janet-simple")))  
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Rainbow parens
(use-package rainbow-delimiters
  :pin melpa
  :ensure t
  :defer t
  ;; I reduced the default value of 9 to simplify the font-lock keyword,
  ;; aiming to improve performance. This package helps differentiate
  ;; nested delimiter pairs, particularly in languages with heavy use of
  ;; parentheses.
  :custom
  (rainbow-delimiters-max-face-count 6)
  :hook (prog-mode . rainbow-delimiters-mode))

;; Better key handling when running inside a terminal via `emacs -nw`
(use-package kkp
  :ensure t
  :config
  ;; (setq kkp-alt-modifier 'alt) ;; use this if you want to map the Alt keyboard modifier to Alt in Emacs (and not to Meta)
  (global-kkp-mode +1))

;;;========================================
;;; Themes
;;;========================================

(use-package modus-themes
  :init
  (require-theme 'modus-themes) ; `require-theme' is ONLY for the built-in Modus themes
  :config
  (setq modus-themes-org-blocks 'tinted-background
	modus-themes-italic-constructs t
	modus-themes-bold-constructs t
	modus-themes-mixed-fonts t
	modus-themes-variable-pitch-ui nil
	modus-themes-common-palette-overrides
    '((bg-mode-line-active bg-lavender)
      (fg-mode-line-active fg-main)
      (border-mode-line-active bg-magenta-intense)
	  (fringe subtle)
	  (bg-paren-match bg-yellow-intense)
	  (custom-set-faces
	   '(mode-line ((t :family "Iosevka Etoile" :height 100 :weight 'regular))))))
  (setq modus-themes-headings
        (quote ((1 . (overline variable-pitch 1.5))
                (2 . (overline variable-pitch 1.3))
                (3 . (overline 1.1))
                (t . (monochrome)))))
  (if (display-graphic-p)
      (modus-themes-load-theme 'modus-operandi-tinted)
    (modus-themes-load-theme 'modus-vivendi-tinted))
  (setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

;;;========================================
;;; Completion, navigation & actions
;;;========================================

;; More permissive matching for file names and minibuffer actions
(use-package orderless
  :pin melpa
  :ensure t
  :custom
  (completion-styles '(partial-completion orderless flex))
  (completion-category-defaults nil)
  (read-file-name-completion-ignore-case t)
  (completion-category-overrides '((file (styles partial-completion))
				   (minibuffer (initials orderless)))))

;; Enable richer annotations
(use-package marginalia
  :pin melpa
  :ensure t
  :commands (marginalia-mode marginalia-cycle)  
  :custom
  (marginalia-annotators '(marginalia-annotators-light))
  :hook (after-init . marginalia-mode))

;; Better completion popup
(use-package corfu
  :pin gnu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary t)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-scroll-margin 5)        ;; Use scroll margin
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :hook (after-init . global-corfu-mode))

;; Better completion at point
(use-package cape
  :ensure t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;; Example configuration for Consult
(use-package consult
  :pin gnu
  :ensure t
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )


(use-package embark
  ;; Embark is an Emacs package that acts like a context menu, allowing
  ;; users to perform context-sensitive actions on selected items
  ;; directly from the completion interface.
  :ensure t
  :defer t
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :pin gnu
  :after consult
  :ensure t
  :defer t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;========================================
;;; Version control
;;;========================================

;; Git integration for Emacs
;; Requires git
(use-package magit
  :pin melpa
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status))

;; Structural diff tool for git
;; Requires difftastic
(use-package difftastic
  :pin melpa
  :ensure t
  :bind (:map magit-blame-read-only-mode-map
         ("D" . difftastic-magit-show)
         ("S" . difftastic-magit-show))
  :after magit-diff
  :config
  (transient-append-suffix 'magit-diff '(-1 -1)
       [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)]))

;;;========================================
;;; Spell & Syntax checking
;;;========================================

;; Requires aspell, aspell-en & aspell-fr to work
(use-package jinx
  :pin melpa
  :ensure t
  :defer t
  :hook (emacs-startup . global-jinx-mode)
  :bind
  (("C-x C-;" . jinx-languages)
   ("M-$" . jinx-correct)))

;; Syntax checking for GNU Emacs
(use-package flymake
  :pin gnu
  :ensure t
  :defer t)

;; Grammar checking
(use-package langtool
  :pin melpa
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
;;; Org-mode, notes and publishing
;;;========================================

(use-package org
  :pin gnu
  :ensure nil
  :defer t
  :diminish "Οrg"
  :if (display-graphic-p)
  :custom
  (org-imenu-depth 5)
  
  (org-confirm-babel-evaluate nil)         ; Don't prompt before running code in org
  (org-src-fontify-natively t)             ; Fontify code in code blocks
  (org-src-tab-acts-natively t)            ; Tabs act as 4 spaces in source blocks
  (org-src-preserve-indentation t)         ; Preserving indentation in source blocks
  (org-highlight-latex-and-related '(latex))    ; Coloring latex code in mode
  (org-latex-prefer-user-labels t)         ; Prefer user names and labels for references
  (org-log-done 'time)                        ; When TODO is one, record timestamp
  (org-return-follows-link t)                 ; Follows links on RET
  ;; See `C-h v` for detail on the keywords, @ means adding a note with time and ! only registers
  ;; the timestamp on state change
  (org-todo-keywords
   '((sequence "TODO(t)" "IN-PROGRESS(i@/!)" "PENDING-CLARIFICATION(p@/!)" "VERIFY(v!)" "|" "DONE(d!)" "WONT-DO(w@/!)")))
  (org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
  :config
  ;; Set :scale to 2 instead of 1 when org mode renders LaTeX
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))   ; Open PDF's with Emacs
  :hook ((after-init . org-mode)
         (org-mode . (lambda ()
			           (variable-pitch-mode t)
			           (setq-default fill-column 89)))))

(use-package org-agenda
  :ensure nil
  :defer t
  :after org
  :custom
  (org-agenda-files '("~/Documents/notes/agendas/"))
  (org-agenda-start-on-weekday 1))

(use-package oc
  :ensure nil
  :defer t
  :after org
  :custom
  (org-cite-csl-styles-dir "~/Zotero/styles"))

;; Simple notes for Emacs with an efficient file-naming scheme
(use-package denote
  :pin gnu
  :ensure t
  :defer t  
  :custom
  (denote-directory (expand-file-name "~/Documents/notes/")))

;; Manipulate denote files in consult
(use-package consult-denote
  :pin gnu
  :after (consult denote)
  :custom
  (consult-denote-find-command #'consult-fd)
  (consult-denote-grep-command #'consult-ripgrep)
  :ensure t
  :defer t)

;; Custome LaTeX templates Requires a full LaTeX install, usually
;; called `texlive'.  The arch wiki
;; https://wiki.archlinux.org/index.php/TeX_Live details how to use it
;; latex compilation found at
;; https://github.com/jkitchin/org-ref/blob/master/org-ref.org Better
;; latexmk for glossaries with a ~/.latexmkrc file. Explained at
;; `https://tex.stackexchange.com/a/44316/223017'.
(use-package auctex
  :pin gnu
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

;; My custom templates for LaTeX
(use-package template
  :ensure nil
  :defer t
  :hook (org-mode . (lambda ()
			   (load-file
			    (expand-file-name
			     (concat user-emacs-directory "latex/template.el"))))))

;; CDLaTeX is a minor mode supporting fast insertion of environment
;; templates and math stuff in LaTeX.
(use-package cdlatex
  :pin melpa
  :ensure t
  :defer t
  :diminish " cdlatex"
  :hook ((org-mode . turn-on-org-cdlatex)    ; Enable cdlatex by default
	 (LaTex-mode . turn-on-cdlatex)
	 (latex-mode . turn-on-cdlatex))
  :bind (:map org-cdlatex-mode-map
	      ("`" . nil)))

;; Citar provides a highly-configurable completing-read front-end to
;; browse and act on BibTeX, BibLaTeX, and CSL JSON bibliographic
;; data, and LaTeX, markdown, and org-cite editing support.
(use-package citar
  :pin melpa
  :ensure t
  :defer t
  :custom
  (citar-citeproc-csl-styles-dir (expand-file-name "~/Zotero/styles"))
  (citar-library-paths '((expand-file-name "~/Drive/KLB/")))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

;; citeproc-el is an Emacs Lisp library for rendering citations and
;; bibliographies in styles described in the Citation Style Language
;; (CSL), an XML-based, open format to describe the formatting of
;; bibliographic references (see http://citationstyles.org/ for
;; further information on CSL).
(use-package citeproc
  :ensure t
  :defer t)

;; Drawing with UTF-8
(use-package uniline
  :ensure t
  :defer t
  :config (set-default 'uniline-hint-style 1))

;;;========================================
;;; Reading
;;;========================================

(use-package pdf-tools
  :pin melpa
  :ensure t
  :defer t
  :magic ("%PDF" . pdf-view-mode)
  :hook (TeX-after-compilation-finished . TeX-revert-document-buffer)
  :defines pdf-annot-activate-created-annotations
  :custom
  (pdf-view-display-size 'fit-page)
  ;; more fine-grained zooming
  (pdf-view-resize-factor 1.05)
  ;; create annotation on highlight
  (pdf-annot-activate-created-annotations t)
  :bind (:map pdf-view-mode-map
	      ("C-s" . isearch-forward)
	      ("C-r" . isearch-backward)))

(use-package nov
  :pin melpa
  :ensure t
  :defer t
  :commands (nov-mode)
  :mode ("\\.epub\\'" . nov-mode))

;;;========================================
;;; Atom/RSS
;;;========================================

(use-package elfeed
  :ensure t
  :defer t
  :commands (elfeed)
  :custom
  (url-queue-timeout 30)
  (elfeed-feeds
   '(("https://mazzo.li/rss.xml" c low-level unix)
     ("https://simblob.blogspot.com/feeds/posts/default" gamedev math algorithms)
     ("https://box2d.org/posts/index.xml" gamedev math algorithms)
     "https://davidgomes.com/rss/"
     ("https://fabiensanglard.net/rss.xml" retrogaming)
     ("https://ferd.ca/feed.rss" distsys)
     "https://blog.singleton.io/index.xml"
     ("https://johnnysswlab.com/feed/" cpp performance)
     ("https://jvns.ca/atom.xml" webdev)
     ("https://matklad.github.io/feed.xml" low-level programming)
     ("https://jonathan-frere.com/index.xml" programming)
     ("https://notes.eatonphil.com/rss.xml" distsys programming)
     ("https://samwho.dev/blog" programming visualization)
     ("https://wingolog.org/feed/atom" compilers guile scheme)
     ("https://jakelazaroff.com/rss.xml" webdev)
     ("https://www.localfirstnews.com/rss/" local-first)
     ("https://www.internalpointers.com/rss" networking concurrency)
     ("https://hazelweakly.me/rss.xml" observability)
     ("https://norvig.com/rss-feed.xml" software)
     ("https://pythonspeed.com/atom.xml" python)
     ("https://austinhenley.com/blog/feed.rss" machine-learning)
     ("https://www.pathsensitive.com/feeds/posts/default?alt=rss" software-design)
     ("https://blog.ploeh.dk/rss.xml" software-design)
     ("https://transactional.blog/feed.xml" database)
     ("https://www.jmeiners.com/feed.xml" software)
     ("https://olano.dev/feed.xml" software)
     ("https://explaining.software/rss" software-design)
     ("https://www.mattkeeter.com/blog/" rust lowl-level graphics)
     ("https://third-bit.com/atom.xml" software-design teaching)
     ("https://borretti.me/feed.xml" software)
     ("https://cybercat-institute.github.io//feed.xml" software-engineering plt machine-learing)
     ("https://dev.arie.bovenberg.net/feed.xml" python)
     ("https://martinfowler.com/feed.atom" software)
     ("https://yosefk.com/blog/feed" software)
     ("https://typesanitizer.com/blog/rss.xml" software software-design complexity)
     ("https://concerningquality.com/feed.xml" software)
     ("https://fossandcrafts.org/rss-feed.rss" hacking craft foss)
     ("https://sunshowers.io/atom.xml" software testing)
     ("https://entropicthoughts.com/feed.xml" software statistics software-design))))

;;;========================================
;;; Agenda & Organization
;;;========================================

;;;========================================
;;; Markdown
;;;========================================

(use-package markdown-mode
  :pin melpa
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
;;; CSV
;;;========================================

(use-package csv-mode
  :ensure t
  :defer t)

;;;========================================
;;; Exporting text files
;;;========================================

(use-package pandoc-mode
  :pin melpa
  :ensure t
  :defer t)

;;;========================================
;;; IDE-like behavior & Language servers
;;;========================================

(use-package eglot
  :pin gnu
  :ensure t
  :defer t 
  :commands (eglot
             eglot-rename
             eglot-ensure
             eglot-rename
             eglot-format-buffer)
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  :config
  ;; Python specific
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 "basedpyright-langserver" "--stdio"))
  ;; Elixir specific
  (add-to-list 'eglot-server-programs
	           '((elixir-ts-mode elixir-mode heex-ts-mode) . ("elixir-ls" "--stdio")))
  ;; Golang specific
  (setq-default eglot-workspace-configuration
    '((:gopls .
        ((staticcheck . t)
         (matcher . "CaseSensitive")
         (ui.semanticTokens . t)))))
  ;; Haskell specific
  (add-to-list 'eglot-server-programs
               '((haskell-ts-mode)
                 "haskell-language-server-wrapper" "--lsp"))
  (setq-default eglot-workspace-configuration
                '((haskell
                   (plugin
                    (stan
                     (globalOn . :json-false))))))  ;; disable stan
  ;; Roc specific
  (add-to-list 'eglot-server-programs '(roc-ts-mode "roc_language_server"))
  ;; Prolog specific: https://github.com/jamesnvc/lsp_server
  (setopt eglot-server-programs (cons
                                 (cons 'prolog-mode
                                       (list "swipl"
                                             "-O"
                                             "-g" "use_module(library(lsp_server))."
                                             "-g" "lsp_server:main"
                                             "-t" "halt"
                                             "--" "port" :autoport))
                                 eglot-server-programs))
  :bind (("C-c l b" . eglot-format-buffer)
	 ("C-c l a" . eglot-code-actions)
	 ("C-c l e" . eglot-reconnect)
	 ("C-c l r" . eglot-rename)))

;; Debugger
(use-package dape
  :ensure t
  :defer t
  :custom
  (dape-buffer-window-arrangement 'gud)
  :config
  ;; Global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode)
  ;; To not display info and/or buffers on startup
  ;; (remove-hook 'dape-on-start-hooks 'dape-info)
  ;; (remove-hook 'dape-on-start-hooks 'dape-repl)

  ;; To display info and/or repl buffers on stopped
  ;; (add-hook 'dape-on-stopped-hooks 'dape-info)
  ;; (add-hook 'dape-on-stopped-hooks 'dape-repl)

  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-compile-hooks 'kill-buffer)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-on-start-hooks (lambda () (save-some-buffers t t)))
  )

;;;========================================
;;; (E)Lisp development
;;;========================================

(use-package elisp-mode
  :ensure nil
  :config
  :diminish "EL")

(use-package buttercup
  :pin melpa
  :ensure t
  :defer t)

(use-package package-lint
  :pin melpa
  :ensure t
  :defer t)

(use-package elisp-lint
  :pin melpa
  :ensure t
  :defer t)

;;;========================================
;;; (Common) Lisp
;;;========================================

(use-package lisp-mode
  :ensure nil
  :diminish "CL")

(use-package slime
  :pin melpa
  :ensure t
  :defer t
  :custom
  (inferior-lisp-program "sbcl"))

(use-package puni
  :pin melpa
  :diminish puni-mode
  :ensure t
  :defer t
  :hook ((emacs-lisp-mode lisp-mode racket-mode scheme-mode janet-ts-mode) . puni-mode))

(use-package parinfer-rust-mode
  :ensure t
  :defer t
  :custom
  (setq parinfer-rust-auto-download t)
  :hook ((emacs-lisp-mode
          lisp-mode
          racket-mode
          scheme-mode
          clojure-ts-mode
          clojurescript-mode
          janet-ts-mode)))

;;;========================================
;;; Scheme 
;;;========================================

(use-package geiser
  :ensure t
  :defer t)

;; Guile
(use-package geiser-guile
  :ensure t
  :defer t
  :custom
  (geiser-guile-binary "/usr/bin/guile"))

;; Chez
(use-package geiser-chez
  :ensure t
  :defer t
  :custom
  (geiser-chez-binary "/usr/bin/chez"))

;; Chicken
(use-package geiser-chicken
  :ensure t
  :defer t
  :custom
  (geiser-chicken-binary "/usr/bin/chicken-csi"))


;;;========================================
;;; Clojure
;;;========================================

(use-package flycheck-clj-kondo
  :ensure t
  :defer t
  :hook ((clojure-mode clojurescript-mode clojure-ts-mode) . flycheck-mode))

(use-package clojure-ts-mode
  :ensure t
  :defer t
  :custom
  (clojure-ts-indent-style 'fixed)
  :config
  (add-hook 'before-save-hook 'cider-format-buffer t t)
  (require 'flycheck-clj-kondo))

(use-package cider
  :ensure t
  :defer t
  :custom
  (cider-enable-nrepl-jvmti-agent t))

(use-package clj-refactor
  :ensure t
  :defer t
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m")
  :hook (clojure-mode clojure-ts-mode))

;;;========================================
;;; Janet
;;;========================================

(use-package janet-ts-mode
  :ensure t
  :defer t
  :vc (:url "https://github.com/sogaiu/janet-ts-mode"
            :rev :newest))

(use-package ajrepl
  :ensure t
  :defer t
  :vc (:url "https://github.com/sogaiu/ajrepl"
            :rev :newest)
  :hook (janet-ts-mode . ajrepl-interaction-mode))

;;;========================================
;;; Maxima
;;;========================================

(use-package gnuplot
  :ensure t
  :defer t)

(use-package maxima
  :ensure t
  :defer t
  :config
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0)
	                        maxima-display-maxima-buffer nil)
  :mode (("\\.mac\\'" . maxima-mode))
  :interpreter ("maxima" . maxima-mode))

;;;========================================
;;; Python
;;;========================================

;; Hide the modeline for inferior python processes
(use-package hide-mode-line
  :pin melpa
  :ensure t
  :defer t
  :hook ((inferior-python-mode . hide-mode-line-mode)	
	     (pdf-view-mode . hide-mode-line-mode)
	     (shell-mode . hide-mode-line-mode)))

;; numpy docstring for python
(use-package numpydoc
  :pin melpa
  :ensure t
  :defer t
  :config
  (setq numpydoc-insert-examples-block nil
	numpydoc-template-long nil)
  :bind (:map python-base-mode-map
              ("C-c C-n" . numpydoc-generate)))

;; Tracks down the correct Python tooling executables from your
;; virtualenvs
(use-package pet
  :pin melpa
  :ensure t
  :defer t
  :hook (python-base-mode . pet-mode))

;; Use ruff with flymake in Python buffer
(use-package flymake-ruff
  :pin melpa
  :ensure t
  :defer t
  :hook (python-base-mode . (lambda ()
                              (flymake-mode 1)
                              (flymake-ruff-load))))

;;;========================================
;;; Lua
;;;========================================

(use-package lua-mode
  :pin melpa
  :ensure t
  :defer t
  :bind (:map lua-mode-map
	      ("C-c C-r" . lua-send-region)
	      ("C-c C-e" . lua-send-current-line)))


;;;========================================
;;; Golang
;;;========================================

(use-package go-mode
  :ensure t
  :defer t)

;;;========================================
;;; Julia
;;;========================================

(use-package julia-ts-mode
  :ensure t
  :defer t)

(use-package julia-snail
  :ensure t
  :defer t
  :hook (julia-ts-mode . julia-snail-mode))

;;;========================================
;;; Zig
;;;========================================

(use-package zig-mode
  :pin melpa
  :ensure t
  :defer t)

;;;========================================
;;; Elixir
;;;========================================

;; Major tree-sitter mode
(use-package elixir-ts-mode
  :pin melpa
  :ensure t
  :defer t
  :config
  (defun nf/insert-pipe()
    (interactive)
    (insert "|>"))
  (defun nf/insert-pry()
    (interactive)
    (insert "require IEx; IEx.pry()"))
  :bind (:map elixir-ts-mode-map
              ("C-c &" . nf/insert-pipe)
              ("C-c b" . nf/insert-pry)))

;; Build tool
(use-package mix
  :pin melpa
  :ensure t
  :defer t
  :hook (elixir-ts-mode . mix-minor-mode))

;; Static code analysis
(use-package flycheck-credo
  :pin melpa
  :ensure t
  :defer t  
  :custom
  (flycheck-elixir-credo-strict t)
  :hook (elixir-ts-mode . flycheck-mode))

;; Commands for exUnit
(use-package exunit
  :pin melpa
  :ensure t
  :defer t
  :hook (elixir-ts-mode . exunit-mode))

(use-package inf-elixir
  :ensure t
  :defer t  
  :bind (:map elixir-ts-mode-map
         ("C-c i i" . inf-elixir)
         ("C-c i p" . inf-elixir-project)
         ("C-c i l" . inf-elixir-send-line)
         ("C-c i r" . inf-elixir-send-region)
         ("C-c i b" . inf-elixir-send-buffer)
         ("C-c i R" . inf-elixir-reload-module)))

;;;========================================
;;; Haskell
;;;========================================

(use-package haskell-ts-mode
  :mode ("\\.hs\\'")
  :ensure t
  :defer t)

(use-package hindent
  :ensure t
  :defer t
  :hook (haskell-ts-mode . hindent-mode))

;;;========================================
;;; Array languages
;;;========================================

(use-package bqn-mode
  :ensure t
  :defer t)

;;;========================================
;;; Concatenative languages
;;;========================================

;; Forth
(use-package forth-mode
  :ensure t
  :defer t)

;; Factor
(use-package fuel
  :ensure t
  :defer t)

;;;========================================
;;; Roc
;;;========================================

(use-package roc-ts-mode
  :ensure t
  :defer t)

;;;========================================
;;; Ada
;;;========================================

(use-package ada-ts-mode
  :ensure t
  :defer t)

(use-package gpr-ts-mode
  :ensure t
  :defer t)

;;;========================================
;;; Prolog
;;;========================================

;; Requires swi-prolog (and swipl exec)
(use-package sweeprolog
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

;;; post-init.el ends here
