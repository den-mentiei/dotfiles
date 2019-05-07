;; -*- no-byte-compile: t; -*-
;; -*- lexical-binding: t; -*-

;; Done to redirect custom generated code, not using it anyway.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;;; Basic stuff

(setq gc-cons-threshold (* 10 1024 1024))

(setq user-full-name "Denys Mentiei")
(setq user-mail-address "endden@gmail.com")

(setq make-backup-files nil)
(setq auto-save-default nil)

(define-coding-system-alias 'cp65001 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

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

(electric-indent-mode -1)

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

;; Save session by default.
(desktop-save-mode 1)

;; Do not warn about defadvice redefinitions.
(setq ad-redefinition-action 'accept)

(defadvice align-regexp (around align-regexp-with-spaces)
  "Use spaces for aligning as opposed to tabs for indentation."
  (let ((indent-tabs-mode nil))
	ad-do-it))
(ad-activate 'align-regexp)

; 0123456789abcdefghijklmnopqrstuvwxyz [] () :;,. !@#$^&*
; 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ {} <> "'`  ~-_/|\?
(cond
  ((find-font (font-spec :name "Fira Code"))
   (set-frame-font "Fira Code-10"))
  ((find-font (font-spec :name "DejaVu Sans Mono"))
   (set-frame-font "DejaVu Sans Mono-12")))

(require 'dired)
(setq dired-recursive-deletes 'top)

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

(defun my/cc-settings ()
  "Bunch of default settings valid for C-mode"
  (interactive)
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (setq indent-tabs-mode t)
  (modify-syntax-entry ?_ "w"))

;;; TODO: Spelling

;; (setq ispell-program-name "hunspell")
;; (setq ispell-dictionary "en_US")

;; (add-hook 'text-mode-hook 'flyspell-mode)
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)

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

;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

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

(force-mode-line-update t)

;; (setq mode-line-format
;;   '(
;; 	;; When Emacs is nearly out of memory for Lisp objects, a brief message saying so. Otherwise, this is empty.
;; 	"%e"
;; 	mode-line-front-space
;; 	;; Encoding/line-endings.
;; 	;; mode-line-mule-info
;; 	"%l:%c"
;; 	" "
;; 	mode-line-buffer-identification
;; 	" "
;; 	mode-name
;; 	" "
;; 	mode-line-end-spaces
;;    ))
;; (force-mode-line-update t)

(use-package solarized-theme
  :init
  (setq solarized-use-less-bold t)
  (setq solarized-use-variable-pitch nil)
  (setq solarized-scale-org-headlines nil)
  (load-theme 'solarized-dark t))

(use-package general)

(use-package hydra
  :init
  (use-package use-package-hydra))

(use-package evil
  :config
  (setq evil-echo-state nil)
  (setq evil-want-Y-yank-to-eol t)
  (evil-mode 1))

(defconst my/leader "SPC")
(general-create-definer my/leader-def :prefix my/leader)

(general-define-key
  :states 'normal
  "<left>"  'evil-prev-buffer
  "<right>" 'evil-next-buffer
  "C-h"     'windmove-left
  "C-j"     'windmove-down
  "C-k"     'windmove-up
  "C-l"     'windmove-right)

(general-define-key
  :states 'insert
  "C-v"   'evil-paste-after)

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

(use-package diminish
  :config
  (diminish 'eldoc-mode)
  (diminish 'undo-tree-mode))

;; TODO: Enable or remove it eventually.
;; (use-package undo-tree
;;   :diminish undo-tree-mode
;;   :config
;;   (global-undo-tree-mode 1))

(use-package which-key
  :diminish 'which-key-mode
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

(use-package org
  :diminish 'org-indent-mode
  :config
  (setq org-default-notes-file (concat org-directory "/inbox.org"))

  (setq org-src-fontify-natively t)

  (setq org-M-RET-may-split-line '((item . nil)))

  (setq org-hide-leading-stars t)

  (setq org-startup-indented t)
  (setq org-indent-indentation-per-level 1)

  (setq org-startup-folded 'showall)

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

  :general
  ("C-c c" 'org-capture))

(use-package counsel
  :diminish counsel-mode
  :init
  (counsel-mode 1))

(use-package swiper
  :general
  ("C-s" 'swiper))

(use-package ivy
  :diminish ivy-mode
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package ivy-posframe
  :after ivy
  :config
  (setq ivy-display-function #'ivy-posframe-display-at-window-center)
  (setq ivy-posframe-parameters
		'((top-fringe . 8)
		  (bottom-fringe . 8)
		  (left-fringe . 8)
		  (right-fringe . 8)))
  :init
  (ivy-posframe-enable))

(use-package writeroom-mode
  :after hydra
  :bind (:map writeroom-mode-map ("<f2>" . hydra/writeroom/body))
  :hydra
  (hydra/writeroom
    (:color blue :hint nil)
"
Welcome to writeroom!
---------------------

^Toggles^                  ^Width^

_m_ toggle fullscreen      _j_ increase
_q_ disable                _k_ decrease
"
	("f" writeroom-set-fullscreen)
	("m" writeroom-toggle-mode-line)
    ("j" writeroom-decrease-width)
    ("k" writeroom-increase-width)
	("q" writeroom-mode)))

(use-package avy)

(use-package web-mode)

(use-package lsp-mode
  :config
  (setq lsp-enable-snippet t)
  (setq lsp-enable-xref t)
  (setq lsp-prefer-flymake :none))

;; (use-package lsp-ui
;;   :after lsp-mode
;;   :init
;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package company
  :diminish 'company-mode
  :init (global-company-mode)
  :config
  (setq company-dabbrev-downcase nil))

(use-package company-lsp
  :after company
  :init
  (add-to-list 'company-backends 'company-lsp))

(use-package magit
  :config
  ; Disables the automatic diff show-off of the changes about to commit.
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (setq vc-handled-backends nil)
  (general-define-key "C-x g" 'magit-status))

;; Elm

(use-package elm-mode
  :after company
  :init
  (add-to-list 'company-backends 'company-elm))

;; Python

(defun my/python-mode ()
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  (setq indent-line-function 'insert-tab))

(use-package anaconda-mode
  :diminish anaconda-mode
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'my/python-mode))

(use-package company-anaconda
  :after (company anaconda-mode)
  :init
  (add-to-list 'company-backends 'company-anaconda))

;; C/C++

;; (use-package irony
;;   :diminish irony-mode
;;   :config
;;   (when (and (my/win-p) (boundp 'w32-pipe-buffer-size))
;; 	(setq irony-server-w32-pipe-buffer-size (* 64 1024)))
;;   :init
;;   (add-hook 'c-mode-hook 'irony-mode)
;;   (add-hook 'c++-mode-hook 'irony-mode)
;;   (add-hook 'objc-mode-hook 'irony-mode)
;;   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; (use-package company-irony-c-headers
;;   :after irony
;;   :init
;;   (add-to-list 'company-backends 'company-irony-c-headers))

;; (use-package company-irony
;;   :after (irony company)
;;   :init
;;   (add-to-list 'company-backends 'company-irony))

;; C#

;; (use-package omnisharp
;;   :after company
;;   :init
;;   (add-hook 'csharp-mode-hook 'omnisharp-mode)
;;   (add-to-list 'company-backends 'company-omnisharp))

;; Lua

(defun my/lua-mode ()
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  (setq indent-line-function 'insert-tab)
  (setq lua-indent-level 4))

(use-package lua-mode
  :init
  (add-hook 'lua-mode-hook 'my/lua-mode))

(use-package fzf)

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

(use-package yaml-mode)

;; Rust

(setq my/rust-analyzer-el "~/.emacs.d/ra-emacs-lsp.el")

(defun my/setup-rust-analyzer ()
  "Setups rust analyzer LSP."
  (use-package dash)
  (use-package ht)
  (load-file my/rust-analyzer-el))

(when (file-readable-p my/rust-analyzer-el)
  (my/setup-rust-analyzer))

(defun my/rust-settings ()
  "Bunch of default settings valid for rust-mode"
  (interactive)
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (setq indent-tabs-mode t)
  (modify-syntax-entry ?_ "w"))

(use-package rust-mode
  :init
  (add-hook 'rust-mode-hook #'lsp)
  (add-hook 'rust-mode-hook 'my/rust-settings))

(use-package yasnippet
  :diminish 'yas-minor-mode
  :init
  (yas-global-mode 1))
