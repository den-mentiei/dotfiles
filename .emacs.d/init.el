;; done to redirect custom generated code, not using it anyway
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;;; Basic stuff

(setq user-full-name "Denys Mentiei")
(setq user-mail-address "endden@gmail.com")

(setq make-backup-files nil)
(setq auto-save-default nil)

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; do not word wrap lines, want horizontal scrolling
(setq-default truncate-lines t)

; ask "y"/"n" instead of "yes"/"no"
(defalias 'yes-or-no-p 'y-or-n-p)

(setq tags-revert-without-query 1)

(setq initial-scratch-message "")
(setq inhibit-splash-screen t)

(setq frame-title-format "emacs: %b")

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

;; Enables window configuration undo/redo by C-c left/right.
(when (fboundp 'winner-mode)
  (winner-mode))

(require 'dired)
(setq dired-recursive-deletes 'top)

(defadvice align-regexp (around align-regexp-with-spaces)
  "Use spaces for aligning as opposed to tabs for indentation."
  (let ((indent-tabs-mode nil))
	ad-do-it))
(ad-activate 'align-regexp)

;;; Functions

(defun my/macos-p ()
  "Check if a system is running macos."
  (eq system-type 'darwin))

(defun my/linux-p ()
  "Check if a system is running Linux."
  (eq system-type 'gnu/linux))

(defun my/find-user-init-file ()
  "Edits the `user-init-file` in another window."
  (interactive)
  (find-file-other-window user-init-file))

(defun my/kill-buffers ()
  "Kills all open buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun my/kill-current-buffer ()
  "Kills the current buffer."
  (interactive)
  (kill-buffer (buffer-name)))

;;; Packages

(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

(use-package diminish
  :init
  (diminish 'abbrev-mode)
  (diminish 'auto-revert-mode))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1))

(use-package org
  :init
  (require 'org-indent)
  :diminish org-indent-mode
  :config
  (setq org-startup-indented 1)
  (setq org-M-RET-may-split-line '((item . nil))))

(use-package solarized-theme
  :init (load-theme 'solarized-dark t))

;; Highlights the current line number.
(use-package hlinum
  :config (hlinum-activate))

(use-package evil
  :init (evil-mode 1)
  :config
  (setq evil-echo-state nil)
  (setq-default evil-symbol-word-search t)
  (defalias 'forward-evil-word 'forward-evil-symbol)
  :bind (:map evil-normal-state-map
         ("SPC r" . counsel-rg)
		 ("<right>" . evil-next-buffer)
		 ("<left>"	 . evil-prev-buffer)
		 ("SPC e i" . my/find-user-init-file)
		 ("SPC b"	 . recompile)
		 ("SPC s"	 . swiper)
		 ("SPC p"	 . counsel-file-jump)
		 ("SPC n"	 . counsel-imenu)
		 ("SPC g"	 . magit-status)
		 ("SPC o"	 . ff-find-other-file)
		 :map evil-visual-state-map
		 ("SPC c"	 . comment-or-uncomment-region)))

(use-package ivy
  :diminish (ivy-mode . "")
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  :config
  (use-package counsel
	:diminish (counsel-mode . ""))
  (use-package swiper)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package nyan-mode
  :init	 (nyan-mode 1)
  :config
  (setq nyan-bar-length 16)
  (setq nyan-wavy-trail t))

(use-package avy)

(use-package web-mode)

(use-package company
  :diminish ""
  :init (global-company-mode))

(use-package magit)

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

(defun my/cc-mode ()
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (setq indent-tabs-mode t))

(use-package irony
  :diminish irony-mode
  :init
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))
  (add-hook 'irony-mode-hook 'my/cc-mode)

(use-package company-irony-c-headers
  :after irony
  :init
  (add-to-list 'company-backends 'company-irony-c-headers))

(use-package company-irony
  :after (irony company)
  :init
  (add-to-list 'company-backends 'company-irony))

;; ObjC/C++

(defun my/objc-mode ()
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  (setq indent-line-function 'insert-tab))

(add-hook 'objc-mode-hook 'my/objc-mode)

;; C#

(defun my/csharp-mode ()
  (setq tab-width 4)
  (setq indent-tabs-mode t))

(use-package omnisharp
  :after company
  :init
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-hook 'csharp-mode-hook 'my/csharp-mode)
  (add-to-list 'company-backends 'company-omnisharp))

;; Lua

(defun my/lua-mode ()
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  (setq indent-line-function 'insert-tab)
  (setq lua-indent-level 4))

(use-package lua-mode
  :init
  (add-hook 'lua-mode-hook 'my/lua-mode))

(use-package fzf
  :if (my/macos-p))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(use-package markdown-mode)
