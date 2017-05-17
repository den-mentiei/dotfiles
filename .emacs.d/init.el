;; done to redirect custom generated code, not using it anyway
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;;; Basic stuff

(setq user-full-name "Denys Mentiei")
(setq user-mail-address "endden@gmail.com")

(setq make-backup-files nil)
(setq auto-save-default nil)

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

(setq visible-bell t)

(setq use-dialog-box nil)

(setq-default show-paren-delay 0)
(show-paren-mode t)

(electric-pair-mode 1)
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

(require 'dired)
(setq dired-recursive-deletes 'top)

;;; Functions

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

(use-package gruvbox-theme
  :init (load-theme 'gruvbox t))

(use-package smart-mode-line
  :init (sml/setup))
(use-package smart-mode-line-powerline-theme)

(use-package evil
  :init (evil-mode 1)
  :config (setq evil-echo-state nil)
  :bind (:map evil-normal-state-map
			  ("<right>" . evil-next-buffer)
			  ("<left>" . evil-prev-buffer)
			  ("SPC s" . save-buffer)
			  ("SPC d" . my/kill-current-buffer)))

(use-package company)

(use-package elm-mode)
(add-to-list 'company-backends 'company-elm)

;;; Local Variables:
;;; -eval: (add-hook 'after-save-hook '(lambda () (byte-compile-file (buffer-file-name))) nil t)
;;; End:
