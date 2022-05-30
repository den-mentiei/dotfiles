;; -*- lexical-binding: t; -*-

;; Profile startup time
(add-hook 'emacs-startup-hook
		  (lambda ()
			(message "Emacs loaded in %s." (emacs-init-time))))

;; That's me.
(setq user-full-name "Denys Mentiei")
(setq user-mail-address "endden@gmail.com")

;;; General UI/UX

; 0123456789abcdefghijklmnopqrstuvwxyz [] () :;,. !@#$^&*
; 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ {} <> "'`  ~-_/|\?
(cond
  ((find-font (font-spec :name "Fira Code"))
   (set-frame-font "Fira Code Light-11"))
  ((find-font (font-spec :name "DejaVu Sans Mono"))
   (set-frame-font "DejaVu Sans Mono-11")))

; Hello, 🐈
; pacman -S noto-fonts-emoji
(when (member "Noto Color Emoji" (font-family-list))
  (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend))

;; Filename is enough.
(setq frame-title-format "%f - emacs")
(setq icon-title-format frame-title-format)

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

;;; Lines/Columns

;; Explicitly set to reduce the cost of on-the-fly computation.
(setq-default display-line-numbers-width 3)

;; Show absolute numbers for narrowed regions, not to get lost.
(setq-default display-line-numbers-widen t)

;; Highlights the current line.
(global-hl-line-mode t)
;; Shows column number in the modeline.
(column-number-mode t)

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
    (remove-if-not
   	  (lambda (x)
		(and
		  (package-installed-p x)
		  (not (package-built-in-p x))))
	  (mapcar 'car package-archive-contents))))

(defun my/default-tab-settings ()
  "Bunch of default settings valid for prog modes."
  (interactive)
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (setq indent-tabs-mode t)
  (modify-syntax-entry ?_ "w"))

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
(setq use-package-always-ensure t)

;;; Built-ins.

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :config
  ;; Ask for any top folder to be on the safe side.
  (setq dired-recursive-deletes 'top))

;;; Etc.

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

;;; Mode-line.

;; Icons for major-modes, etc.
;; Do not forget to call `all-the-icons-install-fonts`.
(use-package all-the-icons)

;; I'm using Doom for now as it looks nice and is pretty lightweight.
(use-package doom-modeline
  :config
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-percent-position nil)

  (doom-modeline-def-modeline 'my/mode-line
	'(bar modals matches buffer-info buffer-position) '(buffer-encoding vcs lsp))

  (defun my/setup-custom-modeline ()
	(doom-modeline-set-modeline 'my/mode-line 'default))

  (add-hook 'doom-modeline-mode-hook 'my/setup-custom-modeline)

  :hook
  (after-init . doom-modeline-mode))

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

(use-package diminish
  :config
  (diminish 'eldoc-mode)
  (diminish 'undo-tree-mode))

;; Vim once, Vim forever, dude.
(use-package evil
  :init
  ;; TODO(dmi): @incomplete Use `evil-collection`.
  ;; (setq evil-want-integration t)
  (setq evil-want-C-i-jump nil)
  (setq evil-auto-indent t)
  (setq evil-echo-state nil)
  (setq evil-want-Y-yank-to-eol t)
  :config
  (evil-mode 1))

(use-package general
  :after evil)

(use-package which-key
  :diminish 'which-key-mode
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

(use-package magit
  :config
  ; Disables the automatic diff show-off of the changes about to commit.
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (add-to-list 'magit-no-confirm 'stage-all-changes)
  (setq vc-handled-backends '(git))
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (evil-set-initial-state 'magit-log-edit-mode 'insert)
  (add-hook 'git-commit-mode-hook 'evil-insert-state)
  :general
  ("C-x g" 'magit-status))

(use-package org
  :mode ("\\.org$" . org-mode)
  :commands (org-capture org-agenda)
  :diminish 'org-indent-mode
  :general
  ("C-c c" 'org-capture)
  :config
  ;; A bit nicer than the default `...`.
  (setq org-ellipsis " ▾")
  (setq org-default-notes-file (concat org-directory "/inbox.org"))
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
  :diminish 'typo-mode
  :hook
  (org-mode . typo-mode))

;;; Code completion.

(use-package company
  :diminish 'company-mode
  :custom
  (company-dabbrev-downcase nil)
  (company-idle-delay 0.1)
  :config
  (global-company-mode))

(use-package eglot)

;;; File format modes.

;; Rust.
(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :hook
  (rust-mode . my/default-tab-settings))

;; Haskell.
(use-package haskell-mode
  :mode ("\\.hs\\'" "\\.lhs\\'" "\\.hsc\\'" "\\.cpphs\\'" "\\.c2hs\\'")
  :config
  (setq haskell-compile-cabal-build-command "stack build")
  :config
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
  :mode ("\\.py$" . python-mode)
  :diminish anaconda-mode
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

(use-package elm-mode
  :mode ("\\.elm\\'" . elm-mode)
  :after company
  :config
  (add-to-list 'company-backends 'company-elm))

(use-package solidity-mode
  :mode ("\\.sol\\'")
  :config
  (setq solidity-comment-style 'slash))
(use-package company-solidity
  :after (company solidity-mode)
  :config
  (add-to-list 'company-backends 'company-solidity))

;; Oh, god.
(use-package php-mode
  :mode ("\\.php\\'"))

(use-package web-mode
  :mode ("\\.html\\'" "\\.css\\'" "\\.tsx\\'"))

(use-package markdown-mode
  :mode ("\\.md\\'"))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'"))

(use-package yaml-mode
  :mode ("\\.yml\\'". yaml-mode))

;;; Bindings.

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
  "i"   'imenu
  "e i" 'my/find-user-init-file
  "w"   'writeroom-mode)

(my/leader-def
  :states 'visual
  "c" 'comment-or-uncomment-region)

;; OLD =============================

(use-package hydra
  :config
  ;;; TODO: Customize border/bg/fringes via
  ;;; https://github.com/abo-abo/hydra/blob/master/hydra.el#L227
  (setq hydra-hint-display-type 'posframe)
  :init
  (use-package use-package-hydra))

(use-package counsel
  :after ivy
  :diminish counsel-mode
  :config
  (setq counsel-fzf-cmd "rg --files . | fzf -f \"%s\"")
  (setq counsel-fzf-dir-function 'vc-root-dir)
  (counsel-mode 1))

(use-package swiper
  :after ivy
  :config
  (ivy-set-occur 'swiper-isearch 'swiper-occur)
  :general
  ("C-s" 'swiper-isearch))

(use-package ivy
  :diminish ivy-mode
  :config
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 16)
  ;;; No regexes by default.
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-extra-directories nil)
  (ivy-mode 1))

(use-package ivy-posframe
  :after ivy
  :config
  (setq ivy-posframe-parameters
		'((top-fringe    . 8)
		  (bottom-fringe . 8)
		  (left-fringe   . 8)))
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
  (ivy-posframe-mode 1))

(use-package writeroom-mode
  :after hydra
  :bind (:map writeroom-mode-map ("<f2>" . hydra/writeroom/body))
  :hydra
  (hydra/writeroom
    (:color red :hint nil)
"
Welcome to writeroom!
---------------------

^Toggles^                  ^Width^

_m_ toggle mode-line       _j_ increase
_q_ disable                _k_ decrease
"
	("m" writeroom-toggle-mode-line :color pink)
	("j" writeroom-decrease-width :color pink)
	("k" writeroom-increase-width :color pink)
	("q" writeroom-mode :color blue)))
