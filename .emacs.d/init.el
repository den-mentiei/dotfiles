;; done to redirect custom generated code, not using it anyway
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;;; Basic stuff

(setq user-full-name "Denys Mentiei")
(setq user-mail-address "endden@gmail.com")

(setq make-backup-files nil)
(setq auto-save-default nil)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; do not word wrap lines, want horizontal scrolling
(setq-default truncate-lines t)

; ask "y"/"n" instead of "yes"/"no"
(defalias 'yes-or-no-p 'y-or-n-p)

(setq tags-revert-without-query 1)

(setq initial-scratch-message "")
(setq inhibit-splash-screen t)

(setq frame-title-format "emacs: %b")

(blink-cursor-mode -1)

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

(setq tab-width 4)

(setq mouse-wheel-scroll-amount '(1))
(setq mouse-wheel-follow-mouse 't)
(setq scroll-conservatively 101)
(setq scroll-margin 10)

(global-hl-line-mode t)
(global-linum-mode t)
(column-number-mode t)

(global-subword-mode 1)

;; Automatically update unmodified buffers whose files have changed.
(global-auto-revert-mode 1)

(require 'dired)
(setq dired-recursive-deletes 'top)

(defadvice align-regexp (around align-regexp-with-spaces)
  "Use spaces for aligning as opposed to tabs for indentation."
  (let ((indent-tabs-mode nil))
    ad-do-it))
(ad-activate 'align-regexp)

;;; Functions

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

(use-package solarized-theme
  :init (load-theme 'solarized-dark t))

;; Highlights the current line number.
(use-package hlinum
  :config (hlinum-activate))

(use-package evil
  :init (evil-mode 1)
  :config (setq evil-echo-state nil)
  :bind (:map evil-normal-state-map
	      ("<right>" . evil-next-buffer)
	      ("<left>"  . evil-prev-buffer)
	      ("SPC a"   . align-regexp)
	      ("SPC e i" . my/find-user-init-file)
	      ("SPC s"   . save-buffer)
	      ("SPC d"   . my/kill-current-buffer)))

(use-package counsel)
(use-package ivy
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package nyan-mode
  :init  (nyan-mode 1)
  :config
  (setq nyan-bar-length 16)
  (setq nyan-wavy-trail t))

(use-package avy)

(use-package web-mode)

(use-package company
  :init (global-company-mode))

;; Elm

(use-package elm-mode)
(add-to-list 'company-backends 'company-elm)

;; Python

(defun my-python-mode ()
  (setq tab-width 4)
  (setq indent-tabs-mode t))

(use-package anaconda-mode
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'my-python-mode))

(use-package company-anaconda
  :init
  (add-to-list 'company-backends 'company-anaconda))

;; C/C++

(defun my-cc-mode ()
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (setq indent-tabs-mode t))

(use-package irony
  :init
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))
  (add-hook 'irony-mode-hook 'my-cc-mode)

(use-package company-irony-c-headers
  :init
  (add-to-list 'company-backends 'company-irony-c-headers))

(use-package company-irony
  :init
  (add-to-list 'company-backends 'company-irony))

;; C#

(defun my-csharp-mode ()
  (setq tab-width 4)
  (setq indent-tabs-mode t))

(use-package omnisharp
  :init
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-hook 'csharp-mode-hook 'my-csharp-mode)
  (add-to-list 'company-backends 'company-omnisharp)
  :config (setq omnisharp-server-executable-path "~/.omnisharp-mono/omnisharp.sh"))

;; Lua

(defun my-lua-mode ()
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  (setq indent-line-function 'insert-tab)
  (setq lua-indent-level 4))

(use-package lua-mode
  :init
  (add-hook 'lua-mode-hook 'my-lua-mode))
