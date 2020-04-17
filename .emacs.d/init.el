;; -*- no-byte-compile: t; lexical-binding: t; -*-

(require 'cl)

;; Done to redirect custom generated code, not using it anyway.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;;; Basic stuff

(setq gc-cons-threshold (* 10 1024 1024))

(setq user-full-name "Denys Mentiei")
(setq user-mail-address "endden@gmail.com")

(setq make-backup-files nil)
(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions t)
(setq version-control t)
(setq create-lockfiles nil)
(setq auto-save-default nil)

(define-coding-system-alias 'cp65001 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
   (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

; ask "y"/"n" instead of "yes"/"no"
(defalias 'yes-or-no-p 'y-or-n-p)

(setq tags-revert-without-query 1)

(setq initial-scratch-message "")
(setq inhibit-splash-screen t)

(setq frame-title-format "%f")

(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq ring-bell-function 'ignore)

(setq use-dialog-box nil)

(setq help-window-select t)

(setq-default show-paren-delay 0)
(show-paren-mode t)

(setq search-highlight t)

(setq-default tab-width 4)
(setq-default indent-tabs-mode t)
(setq-default indent-line-function 'insert-tab)

(setq mouse-wheel-scroll-amount '(1))
(setq mouse-wheel-follow-mouse 't)
(setq scroll-conservatively 101)
(setq scroll-margin 10)

(global-hl-line-mode t)
(column-number-mode t)

(global-superword-mode 1)

;; Automatically update unmodified buffers whose files have changed.
(global-auto-revert-mode 1)

;; Enforces horizontal only splits.
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; Do not warn about defadvice redefinitions.
(setq ad-redefinition-action 'accept)

(defadvice align-regexp (around align-regexp-with-spaces)
  "Use spaces for aligning as opposed to tabs for indentation."
  (let ((indent-tabs-mode nil))
	ad-do-it))
(ad-activate 'align-regexp)

;; Do not resize the frame when font size is changed.
(add-to-list 'frame-inhibit-implied-resize 'font)

; 0123456789abcdefghijklmnopqrstuvwxyz [] () :;,. !@#$^&*
; 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ {} <> "'`  ~-_/|\?
(cond
  ((find-font (font-spec :name "Fira Code"))
   (set-frame-font "Fira Code Light-11"))
  ((find-font (font-spec :name "DejaVu Sans Mono"))
   (set-frame-font "DejaVu Sans Mono-11")))

(require 'dired)
(setq dired-recursive-deletes 'top)

;; Trust all themes.
(setq custom-safe-themes t)

;;; Functions

(defun my/win-p ()
  "Check if a system is running windows."
  (eq system-type 'windows-nt))

(defun my/macos-p ()
  "Check if a system is running macos."
  (eq system-type 'darwin))

(defun my/linux-p ()
  "Check if a system is running linux."
  (eq system-type 'gnu/linux))

(defun my/find-user-init-file ()
  "Edits the `user-init-file` in another window."
  (interactive)
  (find-file user-init-file))

(defun my/kill-buffers ()
  "Kills all open buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun my/kill-current-buffer ()
  "Kills the current buffer."
  (interactive)
  (kill-buffer (buffer-name)))

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

(defun my/cc-settings ()
  "Bunch of default settings valid for C-mode"
  (interactive)
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (setq indent-tabs-mode t)
  (modify-syntax-entry ?_ "w"))

;; Trying to make win performance better :(
(when (and (my/win-p) (boundp 'w32-pipe-read-delay))
  (setq w32-pipe-read-delay 0))

;;; Packages

(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;;; Mode-line

(use-package all-the-icons)

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

(use-package general)

(use-package hydra
  :config
  ;;; TODO: Customize border/bg/fringes via
  ;;; https://github.com/abo-abo/hydra/blob/master/hydra.el#L227
  (setq hydra-hint-display-type 'posframe)
  :init
  (use-package use-package-hydra))

(use-package evil
  :config
  (setq evil-auto-indent t)
  (setq evil-echo-state nil)
  (setq evil-want-Y-yank-to-eol t)
  (evil-mode 1))

(use-package diminish
  :config
  (diminish 'eldoc-mode)
  (diminish 'undo-tree-mode))

(use-package which-key
  :diminish 'which-key-mode
  :config
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  (which-key-mode 1))

(use-package org
  :mode ("\\.org$" . org-mode)
  :diminish 'org-indent-mode
  :general
  ("C-c c" 'org-capture)
  :config
  (setq org-default-notes-file (concat org-directory "/inbox.org"))
  (setq org-src-fontify-natively t)
  (setq org-M-RET-may-split-line '((item . nil)))
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

(use-package web-mode
  :mode ("\\.html?\\'" "\\.css\\'"))

(use-package company
  :diminish 'company-mode
  :init
  (setq company-dabbrev-downcase nil)
  :config
  (global-company-mode))

(use-package company-lsp
  :after company
  :config
  (add-to-list 'company-backends 'company-lsp))

(use-package magit
  :config
  ; Disables the automatic diff show-off of the changes about to commit.
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (setq vc-handled-backends '(git))
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (evil-set-initial-state 'magit-log-edit-mode 'insert)
  (add-hook 'git-commit-mode-hook 'evil-insert-state)
  :general
  ("C-x g" 'magit-status))

;; Elm

(use-package elm-mode
  :mode ("\\.elm$" . elm-mode)
  :after company
  :config
  (add-to-list 'company-backends 'company-elm))

;; Python

(defun my/python-mode ()
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  (setq indent-line-function 'insert-tab))

(use-package anaconda-mode
  :mode ("\\.py$" . anaconda-mode)
  :diminish anaconda-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'my/python-mode))

(use-package company-anaconda
  :after (company anaconda-mode)
  :config
  (add-to-list 'company-backends 'company-anaconda))

;; Lua

(defun my/lua-mode ()
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  (setq indent-line-function 'insert-tab)
  (setq lua-indent-level 4))

(use-package lua-mode
  :mode ("\\.lua$" . lua-mode)
  :config
  (add-hook 'lua-mode-hook 'my/lua-mode))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(use-package markdown-mode)

(use-package rg
  :config
  (setq rg-hide-command t)
  (setq rg-group-result t)
  (rg-define-toggle "--context 3" (kbd "C-c c")))

(use-package typo
  :diminish 'typo-mode
  :init
  (add-hook 'text-mode-hook 'typo-mode))

(use-package yaml-mode
  :mode ("\\.yml$". yaml-mode ))

;; Rust

(defun my/rust-settings ()
  "Bunch of default settings valid for rust-mode"
  (interactive)
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (setq indent-tabs-mode t)
  (modify-syntax-entry ?_ "w"))

(use-package rust-mode
  :mode ("\\.rs$" . rust-mode)
  :config
  (add-hook 'rust-mode-hook 'my/rust-settings))

;; Haskell

(defun my/haskell-settings ()
  "Bunch of default settings valid for haskell-mode"
  (interactive)
  (setq tab-width 2)
  (setq c-basic-offset 2)
  (setq indent-tabs-mode nil))

(use-package haskell-mode
  :mode ("\\.hs\\'" "\\.lhs\\'" "\\.hsc\\'" "\\.cpphs\\'" "\\.c2hs\\'")
  :config
  (setq haskell-compile-cabal-build-command "stack build")
  (add-hook 'haskell-mode 'my/haskell-settings))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'"))

(use-package typescript-mode
  :mode ("\\.ts\\'"))

;;; Bindings

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
 ;;; TODO:  Live with electric-indent or "RET" 'newline-and-indent
  "C-v" 'evil-paste-before)

(my/leader-def
  :states 'normal
  "d"   'my/kill-current-buffer
  "o"   'ff-find-other-file
  "s"   'save-buffer
  "i"   'imenu
  "e i" 'my/find-user-init-file
  "r"   'rg
  "w"   'writeroom-mode)

(my/leader-def
  :states 'visual
  "c" 'comment-or-uncomment-region)
