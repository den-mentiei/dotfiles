;; -*- lexical-binding: t; -*-

;; Profile startup time
(add-hook 'emacs-startup-hook
		  (lambda ()
			(message "Emacs loaded in %s." (emacs-init-time))))

;; `early-init.el` set this to a very big number previously.
(setq gc-cons-threshold (* 10 1024 1024))

;; That's me.
(setq user-full-name "Denys Mentiei")
(setq user-mail-address "endden@gmail.com")

;;; General UI/UX

; 0123456789abcdefghijklmnopqrstuvwxyz [] () :;,. !@#$^&*
; 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ {} <> "'`  ~-_/|\?
(when (member "Fira Code" (font-family-list))
  (set-frame-font (font-spec :family "Fira Code" :size 13)))

;; Hello, 🐈
; pacman -S noto-fonts-emoji
(cond
 ((member "Noto Color Emoji" (font-family-list))
  (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend))
 ((member "Segoe UI Emoji" (font-family-list))
  (set-fontset-font t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend)))

;; Filename is enough.
(setq frame-title-format "%f - emacs")
(setq icon-title-format frame-title-format)

;; Defaults fringes.
(fringe-mode nil)
;; Leave space to other more important things.
(setq-default fringes-outside-margins nil)
(setq-default indicate-buffer-boundaries nil)
(setq-default indicate-empty-lines nil)
(setq-default overflow-newline-into-fringe t)

;; GUIs are inconsistent on different systems and Emacs can handle the
;; prompting just fine.
(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
(when my/is-linux
  (setq x-gtk-use-system-tooltips nil))

;; Favor vertical splits over horizontal ones. Monitors are wide,
;; dude!
(setq split-width-threshold 0)
(setq split-height-threshold nil)

;; Simpler buffer names for same base ones.
(setq uniquify-buffer-name-style 'forward)

;; No bells!
(setq ring-bell-function #'ignore)
(setq visual-bell nil)

;; Shows matching parens immediately.
(setq-default show-paren-delay 0)
(show-paren-mode t)

;; Help buffer is automatically selected, so it is easier to read and
;; quickly close it.
(setq help-window-select t)

;;; Lines/Columns

;; Explicitly set to reduce the cost of on-the-fly computation.
(setq-default display-line-numbers-width 3)

;; Show absolute numbers for narrowed regions, not to get lost.
(setq-default display-line-numbers-widen t)

;; Highlights the current line.
(global-hl-line-mode t)
;; Shows column number in the modeline.
(column-number-mode t)

;;; Scrolling

;; 2 columns away and we automagically scroll further one by one.
(setq hscroll-margin 2)
(setq hscroll-step 1)
;; Emacs spends too much effort recentering the screen if you scroll
;; the cursor more than N lines past window edges (where N is the
;; settings of `scroll-conservatively'). This is especially slow in
;; larger files during large-scale scrolling commands. If kept over
;; 100, the window is never automatically recentered.
(setq scroll-conservatively 101)
;; 2 rows away and we automagically scroll further.
(setq scroll-margin 2)
;; Point keeps its screen position if the scroll command moved it
;; vertically out of the window, e.g. when scrolling by full screens.
(setq scroll-preserve-screen-position t)
;; Reduce cursor lag by a tiny bit by not auto-adjusting
;; `window-vscroll' for tall lines.
(setq auto-window-vscroll nil)
;; Mouse!
(setq mouse-wheel-scroll-amount '(2 ((shift) . hscroll)))
(setq mouse-wheel-scroll-amount-horizontal 2)

;;; Minibuffer

;; Allow for minibuffer nesting.
(setq enable-recursive-minibuffers t)

;; Show current unfinished key-sequence to see what happens.
(setq echo-keystrokes 0.02)

;; Ask "y"/"n" instead of "yes"/"no".
(setq use-short-answers t)

;;; Editor.

;; Disables the warning "X and Y are the same file". Emacs will
;; redirect to thge existing buffer, anyway.
(setq find-file-suppress-same-file-warnings t)

;; Automatically update unmodified buffers whose files have changed.
(global-auto-revert-mode 1)

;; No backups, yolo.
(setq make-backup-files nil)
;; Some sensible defaults if backups are ever get enabled.
(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions t)
(setq version-control t)
(setq create-lockfiles nil)
(setq auto-save-default nil)

;;; Formatting.

;; Tabs > spaces. It can be changed on a per-mode basis (or even via
;; .dir-locals.el) anyway.
(setq-default tab-width 4)
(setq-default indent-tabs-mode t)
(setq-default indent-line-function 'insert-tab)

(defadvice align-regexp (around align-regexp-with-spaces)
  "Use spaces for aligning as opposed to tabs for indentation."
  (let ((indent-tabs-mode nil))
	ad-do-it))
(ad-activate 'align-regexp)

;; Only indent the line when at the beginning or in existing
;; indentation.
(setq-default tab-always-indent nil)

;; All the horizontal splits take the screen real-estate!
(setq-default fill-column 70)

;; Archaic thing that is no longer a thing.
(setq sentence-end-double-space nil)

;;; Utilities.

(defun my/find-user-init-file ()
  "Edits the `user-init-file` in another window."
  (interactive)
  (find-file user-init-file))

(defun my/kill-buffers ()
  "Kills all open buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun my/show-installed-packages ()
  "Shows installed non-builtin packages."
  (interactive)
  (package-refresh-contents)
  (package-show-package-list
    (seq-filter
   	  (lambda (x)
		(and
		  (package-installed-p x)
		  (not (package-built-in-p x))))
	  (mapcar 'car package-archive-contents))))

(defun my/scratch ()
  "Opens a scratch buffer."
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create "*scratch*")))

(defun my/rename-current-buffer-file ()
  "Renames both current fike and buffer visiting it."
  (interactive)
  (let ((name (buffer-name))
		(filename (buffer-file-name)))
	(if (not (and filename (file-exists-p filename)))
		(message "Buffer '%s' is not visiting a file!" name)
	  (let ((new-name (read-file-name "New name: " filename)))
		(if (get-buffer new-name)
			(message "A buffer named '%s' already exists!" new-name)
		  (progn
			(rename-file filename new-name 1)
			(rename-buffer new-name)
			(set-visited-file-name new-name)
			(set-buffer-modified-p nil)))))))

(defun my/delete-current-buffer-file ()
  "Deletes the file visited by current buffer and kills buffer too."
  (interactive)
  (let ((filename (buffer-file-name))
		(buffer (current-buffer))
		(name (buffer-name)))
	(if (not (and filename (file-exists-p filename)))
		(ido-kill-buffer)
	  (when (yes-or-no-p "Are you sure you want to delete this file?")
		(delete-file filename)
		(kill-buffer buffer)
		(message "File '%s' has been deleted!" filename)))))

(defun my/google ()
  "Googles the selected region if any, displays a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
	"https://google.com/search?q="
	(url-hexify-string
	 (if mark-active
		 (buffer-substring (region-beginning) (region-end))
	   (read-string "Google: "))))))

(defun my/cleanup-buffer ()
  "Cleanups the current buffer."
  (interactive)
  (delete-trailing-whitespace))

;; Hooks.

(add-hook 'before-save-hook 'my/cleanup-buffer)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; Packages.

(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Initialize use-package.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
;; Uncomment this and then call `use-package-report'.
;; (setq use-package-compute-statistics t)
(setq use-package-always-ensure t)

;; If something goes wrong with JIT, reset everything via:
;; M-: (byte-recompile-directory package-user-dir nil 'force)

;;; Built-ins.

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  :init
  ;; Ask for any top folder to be on the safe side.
  (setq dired-recursive-deletes 'top))

;;; Etc.

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

;;; Mode-line.

;; Icons for major-modes, etc.
;; Do not forget to call `all-the-icons-install-fonts'.
(use-package all-the-icons)

;; I'm using Doom for now as it looks nice and is pretty lightweight.
(use-package doom-modeline
  :init
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-percent-position nil)
  :config
  (doom-modeline-def-modeline 'my/mode-line
	'(bar modals matches buffer-info buffer-position) '(buffer-encoding vcs lsp))
  (defun my/setup-custom-modeline ()
	(doom-modeline-set-modeline 'my/mode-line 'default))
  :hook
  ((doom-modeline-mode . my/setup-custom-modeline)
   (after-init . doom-modeline-mode)))

;; Solarized theme thing.
(setq my-solarized-faces
	  '("My solarized theme customization."
		(custom-theme-set-faces
		 theme-name

		 `(mode-line
		   ((,class (:inverse-video unspecified
									:overline nil
									:underline nil
									:foreground ,s-mode-line-fg
									:background ,s-mode-line-bg
									:box (:line-width 1
													  :color ,s-mode-line-bg
													  :style unspecified)))))

		 `(mode-line-buffer-id ((,class (:foreground ,s-mode-line-buffer-id-fg :weight bold))))

		 `(mode-line-inactive
		   ((,class (:inverse-video unspecified
									:overline nil
									:underline nil
									:foreground ,s-mode-line-inactive-fg
									:background ,s-mode-line-inactive-bg
									:box (:line-width 1
													  :color ,s-mode-line-inactive-bg
													  :style unspecified)))))

		 `(doom-modeline-bar ((,class (:background ,green-hc))))
		 `(doom-modeline-inactive-bar ((,class (:background ,s-base1)))))))

(use-package solarized-theme
  :init
  (setq solarized-use-less-bold t)
  (setq solarized-use-variable-pitch nil)
  (setq solarized-scale-org-headlines nil)
  (setq x-underline-at-descent-line t)
  (require 'solarized)
  (eval-when-compile (require 'solarized-palettes))
  (deftheme my-solarized-dark "The dark varian of the Solarized colour theme.")
  (solarized-with-color-variables 'dark 'my-solarized-dark solarized-dark-color-palette-alist my-solarized-faces))

; TODO(dmi): Switch to straight and get it from https://github.com/mickeynp/ligature.el
;; Ligatures! != => <-
(use-package ligature
  :load-path "packages"
  :config
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
									   ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
									   "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
									   "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
									   "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
									   "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
									   "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
									   "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
									   "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
									   "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  (global-ligature-mode t))


;; Vim once, Vim forever, dude.
(use-package evil
  :init
  ;; TODO(dmi): @incomplete Use `evil-collection`.
  ;; (setq evil-want-integration t)
  (setq evil-want-C-i-jump nil)
  (setq evil-auto-indent t)
  (setq evil-echo-state nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-symbol-word-search t)
  :config
  (evil-mode 1))

(use-package general
  :after evil)

(use-package which-key
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)))

;; TODO(dmi): @looks Setup highlighting of files vs directories akin
;; to counsel.
;; TODO(dmi): @feature Setup consult-ripgrep separate buffer display.
(use-package vertico
  :init
  ;; Limit the candidates list.
  (setq vertico-resize nil)
  (setq vertico-count 16)
  (setq vertico-cycle t)
  ;; Verico integrates nicely!
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq completion-ignore-case t)
  ;; Custom sorting for `find-file'.
  (defun my/sort-directories-first (files)
	;; Sort by history position, length and alphabetically.
	(setq files (vertico-sort-history-alpha files))
	;; And then move directories to be first.
	(nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
		   (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))
  ;; Configurations per command/completion category.
  (setq vertico-multiform-categories
		'((file (vertico-sort-function . my/sort-directories-first))))
  :config
  (vertico-mode)
  (vertico-multiform-mode))

(use-package vertico-posframe
  :after vertico
  :init
  (setq vertico-posframe-border-width 1)
  (setq vertico-posframe-parameters
		'((left-fringe   . 8)
		  (right-fringe   . 8)))
  (setq vertico-posframe-truncate-lines t)
  :config
  (vertico-posframe-mode 1))

(use-package consult
  :after vertico
  :defer t
  :init
  ;; Show real line numbers when narrowed.
  (setq consult-line-numbers-widen t)
  (setq consult-async-min-input 2)
  (setq consult-async-refresh-delay 0.15)
  (setq consult-async-input-throttle 0.2)
  (setq consult-async-input-debounce 0.1)
  (setq consult-narrow-key "<")
  :bind
  (([remap goto-line]                      . consult-goto-line)
   ([remap imenu]                          . consult-imenu)
   ([remap imenu-multi]                    . consult-imenu-multi)
   ([remap load-theme]                     . consult-theme)
   ([remap switch-to-buffer]               . consult-buffer)
   ([remap switch-to-buffer-other-window]  . consult-buffer-other-window)
   ([remap xref-show-xrefs-function]       . consult-xref)
   ([remap xref-show-definitions-function] . consult-xref)
   ("C-s"                                  . consult-line))
  :config
  (advice-add #'multi-occur :override #'consult-multi-occur))

(use-package magit
  :config
  ; Disables the automatic diff show-off of the changes about to commit.
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (add-to-list 'magit-no-confirm 'stage-all-changes)
  (setq vc-handled-backends '(git))
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (evil-set-initial-state 'magit-log-edit-mode 'insert)
  (add-hook 'git-commit-mode-hook 'evil-insert-state)
  :bind
  ("C-x g" . magit-status))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :commands org-agenda
  :init
  ;; A bit nicer than the default `...`.
  (setq org-ellipsis " ▾")
  (setq org-src-fontify-natively t)
  (setq org-M-RET-may-split-line nil)
  (setq org-hide-leading-stars t)
  (setq org-startup-indented t)
  (setq org-indent-indentation-per-level 1)
  (setq org-startup-folded 'content)
  (setq org-export-backends '(ascii html latex md))
  ;;; Display entities like \alpha, \tilde, etc. via corresponding UTF-8 symbols.
  (setq org-pretty-entities t)
  ;;; Display sub/super-scripts, as well.
  (setq org-pretty-entities-include-sub-superscripts t)
  ;;; Blocks entries from going to DONE, if there are not-DONE children.
  (setq org-enforce-todo-dependencies t)
  ;;; Same goes for nested checkbox lists.
  (setq org-enforce-todo-checkbox-dependencies t)
  ;;; Should help preventing errorous edits.
  (setq org-catch-invisible-edits 'smart)
  ;;; Show *foo* and /foo/ without org markers, just the formatting.
  (setq org-hide-emphasis-markers t)
  ;;; Time will be recorded after task completion.
  (setq org-log-done 'time))

;; Typography things.
(use-package typo
  :after org-mode
  :hook
  (org-mode . typo-mode))

(use-package expand-region
  :bind ("S-SPC" . 'er/expand-region))

;;; Code completion.

(use-package company
  :custom
  (company-dabbrev-downcase nil)
  (company-idle-delay 0.1)
  :config
  (global-company-mode))

(use-package eglot
  :commands eglot
  :bind (:map eglot-mode-map
			  ("M-RET" . eglot-code-actions)))

(use-package consult-eglot
  :after (eglot consult))

;;; File format modes.

;; Rust.
(use-package rust-mode
  :mode ("\\.rs\\'")
  :config
  (defun my/rust-settings ()
	"Bunch of default settings valid for rust-mode."
	(interactive)
	(setq tab-width 4)
	(setq c-basic-offset 4)
	(setq indent-tabs-mode t)
	(modify-syntax-entry ?_ "w"))
  :hook
  (rust-mode . my/rust-settings))

;; Haskell.
(use-package haskell-mode
  :mode ("\\.hs\\'" "\\.lhs\\'" "\\.hsc\\'" "\\.cpphs\\'" "\\.c2hs\\'")
  :config
  (setq haskell-compile-cabal-build-command "cabal build")
  (defun my/haskell-settings ()
	"Bunch of default settings valid for haskell-mode"
	(interactive)
	(setq tab-width 2)
	(setq c-basic-offset 2)
	(setq indent-tabs-mode nil))
  :hook
  (haskell . my/haskell-settings))

(use-package csharp-mode
  :mode ("\\.cs\\'"))

(use-package typescript-mode
  :mode ("\\.ts\\'"))

(use-package swift-mode
  :mode ("\\.swift\\'"))

(use-package kotlin-mode
  :mode ("\\.kt\\'"))

(use-package anaconda-mode
  :mode ("\\.py\\'" . python-mode)
  :config
  (defun my/python-settings ()
	(setq tab-width 4)
	(setq indent-tabs-mode t)
	(setq indent-line-function 'insert-tab))
  :hook
  ((python-mode . anaconda-mode)
   (python-mode . my/python-settings)))

(use-package company-anaconda
  :after (company anaconda-mode)
  :config
  (add-to-list 'company-backends 'company-anaconda))

(use-package solidity-mode
  :mode ("\\.sol\\'")
  :init
  (setq solidity-comment-style 'slash))
(use-package company-solidity
  :after (company solidity-mode)
  :config
  (add-to-list 'company-backends 'company-solidity))

;; Oh, god.
(use-package php-mode
  :disabled
  :mode ("\\.php\\'"))

(use-package web-mode
  :mode ("\\.html\\'" "\\.css\\'" "\\.tsx\\'")
  :init
  (setq web-mode-enable-auto-quoting nil))

(use-package markdown-mode
  :mode ("\\.md\\'"))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'"))

(use-package yaml-mode
  :mode ("\\.yml\\'"))

(use-package pine-script-mode
  :mode ("\\.pine"))

(use-package fish-mode
  :mode ("\\.fish"))

;;; Bindings.

(general-define-key
 "C-x C-r" 'my/rename-current-buffer-file
 "C-x C-k" 'my/delete-current-buffer-file)

(defconst my/leader "SPC")
(general-create-definer my/leader-def :prefix my/leader)

(general-define-key
  :states 'normal
  "C-h"     'windmove-left
  "C-j"     'windmove-down
  "C-k"     'windmove-up
  "C-l"     'windmove-right)

(general-define-key
 :states 'insert
 "C-v" 'evil-paste-before)

(my/leader-def
  :states 'normal
  "e i" 'my/find-user-init-file
  "i"   'consult-imenu
  "t"   'consult-eglot-symbols
  "r"   'consult-ripgrep
  "g"   'my/google)

(my/leader-def
  :states 'visual
  "c" 'comment-or-uncomment-region
  "g" 'my/google)
