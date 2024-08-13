;;; init.el --- Init -*- no-byte-compile: t; lexical-binding: t; -*-

;; Author: James Cherti & Nathan Furnal
;; URL: https://github.com/jamescherti/minimal-emacs.d & https://gitlab.com/nathanfurnal/dotemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 1.0.2
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Based on Minimal Emacs, without pre and post loading and with my full config
;; This is the main initialization file for Emacs. It configures package
;; archives, ensures essential packages like `use-package` are installed, and
;; sets up further package management and customization settings.

;;; Code:

;;; package.el

(require 'package)

(when (version< emacs-version "28")
  (add-to-list 'package-archives
               '("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(customize-set-variable 'package-archive-priorities
                        '(("gnu"    . 99)
                          ("nongnu" . 80)                        
                          ("melpa"  . 0)))

(when package-enable-at-startup
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents t)))

;;; use-package
;; Load use-package for package configuration

;; Ensure the 'use-package' package is installed and loaded
(unless (package-installed-p 'use-package)
  (package-refresh-contents t)
  (package-install 'use-package)
  (eval-when-compile
    (require 'use-package)))

(eval-when-compile
  (require 'use-package))

(use-package use-package
  :custom
  (use-package-hook-name-suffix nil)
  (use-package-compute-statistics t))

(use-package diminish :ensure t :after use-package) ;; if you use :diminish
(use-package bind-key :ensure t :after use-package) ;; if you use any :bind variant

;;;========================================
;;; Defaults
;;;========================================

;; Useful defaults and setup
;; You don't need to use `use-package' here and can simply use
;; raw variables but it helps me organize the init file

(use-package emacs
  :custom
  ;; Ensures builtins are up to date
  (package-install-upgrade-built-in t)
  (enable-recursive-minibuffers t)
  ;; Keep the cursor out of the read-only portions of the minibuffer  
  (minibuffer-prompt-properties
   '(read-only t intangible t cursor-intangible t face
               minibuffer-prompt))
  ;; Can be activated with `display-line-numbers-mode'
  (display-line-numbers-width 3)
  (display-line-numbers-widen t)
  ;; Avoid generating lockfiles to prevent creating world-readable
  ;; copies of files. 
  (create-lockfiles nil)
  ;; Do not auto-disable auto-save after deleting large chunks of
  ;; text. The purpose of auto-save is to provide a failsafe, and
  ;; disabling it contradicts this objective.
  (auto-save-include-big-deletions t)
  ;; Auto save options
  (kill-buffer-delete-auto-save-files t)
  ;; Resizing the Emacs frame can be costly when changing the font. Disable this
  ;; to improve startup times with fonts larger than the system default.
  (frame-resize-pixelwise t)
  ;; However, do not resize windows pixelwise, as this can cause crashes in some
  ;; cases when resizing too many windows at once or rapidly.
  (window-resize-pixelwise nil)
  (resize-mini-windows 'grow-only)
  ;; Smooth scrolling
  (fast-but-imprecise-scrolling t)
  (hscroll-margin 2)
  (hscroll-step 1)
  ;; Emacs spends excessive time recentering the screen when the cursor
  ;; moves more than N lines past the window edges (where N is the value of
  ;; `scroll-conservatively`). This can be particularly slow in larger files
  ;; during extensive scrolling. If `scroll-conservatively` is set above
  ;; 100, the window is never automatically recentered. The default value of
  ;; 0 triggers recentering too aggressively. Setting it to 10 reduces
  ;; excessive recentering and only recenters the window when scrolling
  ;; significantly off-screen.
  (scroll-conservatively 10)
  (scroll-margin 0)
  (scroll-preserve-screen-position t)
  ;; Minimize cursor lag slightly by preventing automatic adjustment of
  ;; `window-vscroll' for tall lines.
  (auto-window-vscroll nil)
  ;; Don't stretch the cursor to fit wide characters, it is disorienting,
  ;; especially for tabs.
  (x-stretch-cursor nil)
  ;; No beeping or blinking
  (visible-bell nil)
  (ring-bell-function #'ignore)
  ;; Indent and formatting
  (left-fringe-width  8)
  (right-fringe-width 8)
  ;; Do not show an arrow at the top/bottomin the fringe and empty lines
  (indicate-buffer-boundaries nil)
  (indicate-empty-lines nil)
  ;; Continue wrapped lines at whitespace rather than breaking in the
  ;; middle of a word.
  (word-wrap t)
  ;; Disable wrapping by default due to its performance cost.
  (truncate-lines t)
  ;; If enabled and `truncate-lines' is disabled, soft wrapping will not occur
  ;; when the window is narrower than `truncate-partial-width-windows' characters.
  (truncate-partial-width-windows nil)
  ;; Prefer spaces over tabs. Spaces offer a more consistent default compared to
  ;; 8-space tabs. This setting can be adjusted on a per-mode basis as needed.
  (tab-width 4)
  ;; We often split terminals and editor windows or place them side-by-side,
  ;; making use of the additional horizontal space.
  (fill-column 80)
  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  ;; (text-mode-ispell-word-completion nil)
  (auto-save-list-file-prefix
   (expand-file-name "autosave/" user-emacs-directory))
  ;; Remove duplicates from the kill ring to reduce clutter
  (kill-do-not-save-duplicates t)
  (tab-always-indent 'complete)
  ;; Disable the obsolete practice of end-of-line spacing from the
  ;; typewriter era.
  (sentence-end-double-space nil)
  
  :init
  (set-face-attribute 'default nil :family "Iosevka Term Curly" :height 140 :weight 'regular)
  (set-face-attribute 'fixed-pitch nil :family "Iosevka Term" :height 140 :weight 'medium)
  (set-face-attribute 'variable-pitch nil :family "Iosevka Aile" :height 135 :weight 'medium)
  (put 'narrow-to-region 'disabled nil) ; Allows narrowing bound to C-x n n (region) and C-x n w (widen)
  :config
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (add-hook 'after-init-hook #'window-divider-mode)
  (blink-cursor-mode -1)  
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

(use-package hl-line
  :ensure nil  
  :custom
  (global-hl-line-mode 1))

(use-package delsel
  :ensure nil  
  :custom
  (delete-selection-mode t))

(use-package window
  :ensure nil  
  :custom
  ;; switch-to-buffer runs pop-to-buffer-same-window instead
  (switch-to-buffer-obey-display-actions t)
  ;; Prefer vertical splits over horizontal ones
  (split-width-threshold 170)
  (split-height-threshold nil))

(use-package frame
  :ensure nil
  :custom
  ;; The native border "uses" a pixel of the fringe on the rightmost
  ;; splits, whereas `window-divider` does not.n
  (window-divider-default-bottom-width 1)
  (window-divider-default-places t)
  (window-divider-default-right-width 1))

(use-package paren
  :ensure nil  
  :custom
  (show-paren-mode t)
  (show-paren-delay 0.1)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

(use-package whitespace
  :ensure nil  
  :custom
  (whitespace-line-column nil))

(use-package comint
  :ensure nil  
  :custom
  (comint-prompt-read-only t)
  (comint-buffer-maximum-size 2048))

(use-package compile
  :ensure nil
  :custom
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-scroll-output 'first-error))

(use-package files
  :ensure nil  
  :custom
  ;; Disable the warning "X and Y are the same file". Ignoring this warning is
  ;; acceptable since it will redirect you to the existing buffer regardless.
  (find-file-suppress-same-file-warnings t)
  ;; Resolve symlinks when opening files, so that any operations are conducted
  ;; from the file's true directory (like `find-file').
  (find-file-visit-truename t)
  ;; Skip confirmation prompts when creating a new file or buffer
  (confirm-nonexistent-file-or-buffer nil)
  ;; Avoid generating backups to prevent creating world-readable copies
  (make-backup-files nil)
  (backup-directory-alist
   `(("." . ,(expand-file-name "backup" user-emacs-directory))))
  (backup-by-copying-when-linked t)
  (backup-by-copying t)  ; Backup by copying rather renaming
  (delete-old-versions t)  ; Delete excess backup versions silently
  (version-control t)  ; Use version numbers for backup files
  (kept-new-versions 5)
  (kept-old-versions 5)
  ;; Enable auto-save to safeguard against crashes or data loss. The
  ;; `recover-file' or `recover-session' functions can be used to restore
  ;; auto-saved data.
  (auto-save-default t)
  ;; Auto revert
  ;; Auto-revert in Emacs is a feature that automatically updates the
  ;; contents of a buffer to reflect changes made to the underlying file
  ;; on disk.
  (revert-without-query (list "."))  ; Do not prompt
  ;; According to the POSIX, a line is defined as "a sequence of zero or
  ;; more non-newline characters followed by a terminating newline".
  (require-final-newline t))

(use-package autorevert
  :ensure nil  
  :custom
  (auto-revert-stop-on-user-input nil)
  (auto-revert-verbose t)
  ;; Revert other buffers (e.g, Dired)
  (global-auto-revert-non-file-buffers t)
  :hook (after-init-hook . global-auto-revert-mode))

(use-package vc-hooks
  :ensure nil  
  :custom
  (vc-follow-symlinks t)
  ;; Do not backup version controlled files
  (vc-make-backup-files nil))
  
(use-package uniquify
  :ensure nil  
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package mouse
  :ensure nil  
  :custom
  (mouse-yank-at-point t)
  ;; Found in `mwheel.el'
  (mouse-wheel-scroll-amount '(2 ((shift) . hscroll)))
  (mouse-wheel-scroll-amount-horizontal 2))  

(use-package simple
  :ensure nil  
  :custom
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Don't blink the paren matching the one at point, it's too distracting.
  (blink-matching-paren nil)
  (indent-tabs-mode nil))

(use-package tramp
  :ensure nil  
  :after files
  :custom
  (tramp-backup-directory-alist backup-directory-alist)
  (tramp-auto-save-directory
   (expand-file-name "tramp-autosave/" user-emacs-directory)))

(use-package recentf
  :ensure nil  
  ;; `recentf' is an Emacs package that maintains a list of recently
  ;; accessed files, making it easier to reopen files you have worked on
  ;; recently.
  :custom
  (recentf-max-saved-items 300) ; default is 20
  (recentf-auto-cleanup 'mode)
  :hook (after-init-hook . recentf-mode))

(use-package saveplace
  :ensure nil  
  ;; `save-place-mode` enables Emacs to remember the last location within a file
  ;; upon reopening. This feature is particularly beneficial for resuming work at
  ;; the precise point where you previously left off.  
  :custom
  (save-place-file (expand-file-name "saveplace" user-emacs-directory))
  (save-place-limit 600)
  :hook (after-init-hook . save-place-mode))

(use-package savehist
  :ensure nil  
  :custom
  ;; `savehist` is an Emacs feature that preserves the minibuffer history between
  ;; sessions. It saves the history of inputs in the minibuffer, such as commands,
  ;; search strings, and other prompts, to a file. This allows users to retain
  ;; their minibuffer history across Emacs restarts.
  (history-length 300)
  (savehist-save-minibuffer-history t)
  :hook (after-init-hook . savehist-mode))

(use-package isearch
  :ensure nil
  ;; Delete char on <DEL> instead of going back searches
  :bind (("C-c s" . isearch-forward-thing-at-point)
	 :map isearch-mode-map
	 ("<DEL>" . isearch-del-char)))

;;;========================================
;;; Better defaults
;;;========================================

;;; Improve defaults with packages (not builtins)

;; Better GC strategy
(use-package gcmh
  :pin gnu
  :ensure t
  :diminish ""                          ; Hide in modeline as it is always there
  :hook (after-init-hook . gcmh-mode)
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-low-cons-threshold minimal-emacs-gc-cons-threshold))

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

;; Vertical minibuffer
(use-package vertico
  :pin gnu  
  :ensure t  
  :hook (after-init-hook . vertico-mode)
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
  (rainbow-delimiters-max-face-count 5)
  :hook (prog-mode-hook . rainbow-delimiters-mode))

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
(use-package circadian
  :pin melpa
  :ensure t
  :custom
  (calendar-latitude 50.85)
  (calendar-longitude 4.35)
  :config
  (setq circadian-themes '((:sunrise . modus-operandi-tinted)
                           (:sunset  . modus-vivendi-tinted)))
  (circadian-setup))

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
  :hook (after-init-hook . marginalia-mode))

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

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :hook (after-init-hook . global-corfu-mode))

;; Better completion at point
(use-package cape
  :pin melpa
  :ensure t
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  ;; (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-history)
  ;; (add-hook 'completion-at-point-functions #'cape-keyword)
  ;; (add-hook 'completion-at-point-functions #'cape-tex)
  ;; (add-hook 'completion-at-point-functions #'cape-sgml)
  ;; (add-hook 'completion-at-point-functions #'cape-rfc1345)
  (add-hook 'completion-at-point-functions #'cape-abbrev)
  (add-hook 'completion-at-point-functions #'cape-dict)
  ;; (add-hook 'completion-at-point-functions #'cape-elisp-symbol)
  ;; (add-hook 'completion-at-point-functions #'cape-line)
  )

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
  :pin gnu
  :ensure t
  :defer t
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
  (embark-collect-mode-hook . consult-preview-at-point-mode))

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
  :diminish "Οrg"
  :custom
  (org-imenu-depth 5)
  
  (org-confirm-babel-evaluate nil)         ; Don't prompt before running code in org
  (org-src-fontify-natively t)             ; Use syntax highlighting in source blocks while editing
  (org-src-tab-acts-natively t)            ; Tabs act as 4 spaces in source blocks
  (org-src-preserve-indentation t)         ; Preserving indentation in source blocks
  (org-highlight-latex-and-related '(latex))    ; Coloring latex code in mode
  (org-latex-prefer-user-labels t)         ; Prefer user names and labels for references
  (org-cite-csl-styles-dir "~/Zotero/styles") ; Use Zotero styles for CSL exports (bibliography management)
  (org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
  :config
  ;; Set :scale to 2 instead of 1 when org mode renders LaTeX
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))   ; Open PDF's with Emacs

  :hook (org-mode-hook . (lambda ()
			   (variable-pitch-mode t)
			   (setq-default fill-column 100))))

;; Modern aesthetic for org-mode
(use-package org-modern
  :pin melpa
  :ensure t
  :defer t
  :custom
  (org-modern-table nil)
  :config
  (set-face-attribute 'org-modern-symbol nil :family "Iosevka")  
  :hook ((org-mode-hook . org-modern-mode)
	 (org-agenda-finalize-hook . org-modern-agenda)))

;; Simple notes for Emacs with an efficient file-naming scheme
(use-package denote
  :pin gnu
  :ensure t
  :defer t  
  :custom
  (denote-directory (expand-file-name "~/Documents/notes/")))

;; LaTeX exports from org-mode
(use-package ox-latex
  :ensure nil
  :after org
  :config
  (use-package engrave-faces
    :ensure t
    :custom
    (org-latex-src-block-backend 'engraved)
    :config
    (add-to-list 'org-latex-engraved-options '("numbers" . "left"))))

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
  :hook (org-mode-hook . (lambda ()
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
  :hook ((org-mode-hook . turn-on-org-cdlatex)    ; Enable cdlatex by default
	 (LaTex-mode-hook . turn-on-cdlatex)
	 (latex-mode-hook . turn-on-cdlatex))
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
  (LaTeX-mode-hook . citar-capf-setup)
  (org-mode-hook . citar-capf-setup))

;; citeproc-el is an Emacs Lisp library for rendering citations and
;; bibliographies in styles described in the Citation Style Language
;; (CSL), an XML-based, open format to describe the formatting of
;; bibliographic references (see http://citationstyles.org/ for
;; further information on CSL).
(use-package citeproc
  :ensure t
  :defer t)

;;;========================================
;;; Reading
;;;========================================

(use-package pdf-tools
  :pin melpa
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
  :custom
  (eldoc-echo-area-use-multiline-p)
  (eglot-autoshutdown t)
  (eglot-report-progress nil)  ; Prevent minibuffer spam  
  :commands (eglot
             eglot-rename
             eglot-ensure
             eglot-rename
             eglot-format-buffer)
  :config
  ;; Optimizations
  (fset #'jsonrpc--log-event #'ignore)
  (setq jsonrpc-event-hook nil)  
  ;; Python specific
  (add-to-list 'eglot-server-programs
	       '(python-base-mode . ("pyright-langserver" "--stdio")))
  (setq-default eglot-workspace-configuration
		'((:pyright .
			    ((useLibraryCodeForTypes . t)))))
  ;; Elixir specific
  (add-to-list 'eglot-server-programs
	       '((elixir-ts-mode elixir-mode heex-ts-mode) . ("elixir-ls" "--stdio")))
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
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package puni
  :pin melpa
  :diminish puni-mode
  :ensure t
  :defer t
  :hook ((emacs-lisp-mode-hook lisp-mode-hook racket-mode-hook scheme-mode-hook) . puni-mode))

;;;========================================
;;; Python
;;;========================================

;; Hide the modeline for inferior python processes
(use-package hide-mode-line
  :pin melpa
  :ensure t
  :defer t
  :hook ((inferior-python-mode-hook . hide-mode-line-mode)
	 (inferior-ess-r-mode-hook . hide-mode-line-mode)
	 (pdf-view-mode-hook . hide-mode-line-mode)
	 (shell-mode-hook . hide-mode-line-mode)))

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
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

;; Use ruff with flymake in Python buffer
(use-package flymake-ruff
  :pin melpa
  :ensure t
  :defer t
  :hook (python-base-mode-hook . (lambda () (flymake-mode 1)
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

(use-package go-ts-mode
  :ensure nil
  :custom
  (go-ts-mode-indent-offset 4))

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
  :defer t)

;; Build tool
(use-package mix
  :pin melpa
  :ensure t
  :defer t
  :hook (elixir-ts-mode-hook . mix-minor-mode))

;; Static code analysis
(use-package flycheck-credo
  :pin melpa
  :ensure t
  :defer t  
  :custom
  (flycheck-elixir-credo-strict t)
  :hook (elixir-ts-mode-hook . flycheck-mode))

;; Commands for exUnit
(use-package exunit
  :pin melpa
  :ensure t
  :defer t
  :hook (elixir-ts-mode-hook . exunit-mode))

;;;========================================
;;; Docker
;;;========================================

(use-package docker
  :ensure t
  :defer t)

(use-package dockerfile-mode
  :ensure t
  :defer t)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(flymake-ruff bind-key cl-generic cl-lib erc external-completion faceup flymake idlwave let-alist map nadvice ntlm org project python seq so-long soap-client svg tramp use-package verilog-mode xref eglot zig-mode vterm vertico treesit-auto slime rainbow-delimiters puni pet pdf-tools pandoc-mode org-modern orderless numpydoc modus-themes mix markdown-mode marginalia lua-mode langtool jinx hide-mode-line gcmh flycheck-credo exunit engrave-faces embark-consult elixir-ts-mode elisp-lint dockerfile-mode docker diminish difftastic denote dape csv-mode corfu citar circadian cdlatex cape buttercup auctex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
