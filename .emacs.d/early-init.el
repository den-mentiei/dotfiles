;; -*- lexical-binding: t; -*-

;; A lot of this stuff has been taken from the Doom Emacs.

;; Increase the GC threshold for faster startup.
;; Measured in bytes.
(setq gc-cons-threshold most-positive-fixnum)

(setq package-enable-at-startup nil)

;; Done to redirect custom generated code, not using it anyway.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; In noninteractive sessions, prioritize non-byte-compiled source
;; files to prevent the use of stale byte-code. Otherwise, it saves us
;; a little IO time to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Native compilation settings
(when (featurep 'native-compile)
  ;; Silence compiler warnings as they can be pretty annoying.
  (setq native-comp-async-report-warnings-errors 'silent)
  ;; Let's compile asynchronously.
  (setq native-comp-deferred-compilation t))

(unless (or (daemonp)
			noninteractive
			init-file-debug)
  ;; Premature redisplays can substantially affect startup times and
  ;; produce ugly flashes before theme is loaded.
  (setq-default inhibit-redisplay t)
  (setq-default inhibit-message t)
  (add-hook 'window-setup-hook
			(lambda ()
			  (setq-default inhibit-redisplay nil)
			  (setq-default inhibit-message nil)
			  (redisplay)))
  ;; Site files tend to use `load-file', which emits "Loading X..."
  ;; messages in the echo area, which in turn triggers a
  ;; redisplay. Redisplays can have a substantial effect on startup
  ;; times and in this case happens so early that Emacs may flash
  ;; white while starting up.
  (define-advice load-file (:override (file) silence)
    (load file nil 'nomessage))
  ;; Undo the `load-file' advice above, to limit the scope of any edge
  ;; cases it may introduce down the road.
  (define-advice startup--load-user-init-file (:before (&rest _) init-doom)
    (advice-remove #'load-file #'load-file@silence)))

;;; Bootstrap

;; Do not warn about defadvice redefinitions.
(setq ad-redefinition-action 'accept)

;; The only thing that is needed to make utf-8 the default coding
;; system.
(set-language-environment "UTF-8")
;; `set-language-environment` sets `default-input-method`, which is
;; not needed.
(setq default-input-method nil)

(defconst my/is-macos (eq system-type 'darwin))
(defconst my/is-linux (eq system-type 'gnu-linux))
(defconst my/is-win (memq system-type '(cygwin windows-nt msd-dos)))

;; Disable bidirectional text scanning for a modest performance
;; boost. I've set this to `nil' in the past, but the
;; `bidi-display-reordering's docs say that is an undefined state and
;; suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right)
(setq-default bidi-paragraph-direction 'left-to-right)

;; Disabling the BPA makes redisplay faster, but might produce
;; incorrect display reordering of bidirectional text with embedded
;; parentheses and other bracket characters whose 'paired-bracket'
;; Unicode property is non-nil.
(setq bidi-inhibit-bpa t)

;; Reduce rendering/line scan work for Emacs by not rendering cursors
;; or regions in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause
;; brief spells of inaccurate syntax highlighting right after
;; scrolling, which should quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Resizing the Emacs frame can be a terribly expensive part of
;; changing the font. By inhibiting this, we halve startup times,
;; particularly when we use fonts that are larger than the system
;; default (which would resize the frame).
(add-to-list 'frame-inhibit-implied-resize 'font)

;; Font compacting can be terribly expensive, especially for rendering
;; icon fonts on Windows. Whether disabling it has a notable affect on
;; Linux and Mac hasn't been determined, but do it there anyway, just
;; in case. This increases memory usage, however!
(setq inhibit-compacting-font-caches t)

;; No need to see the splash screen.
(setq inhibit-startup-screen t)
;; Hides yet another startup *message*.
(setq inhibit-startup-echo-area-message user-login-name)
;; No need to load any default.el.
(setq inhibit-default-init t)
;; Make the initial buffer load faster by setting it to fundamental
;; mode.
(setq initial-major-mode 'fundamental-mode)
;; Empty your mind!
(setq initial-scratch-message nil)

;; Removing unused UI things.
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Performance on Windows is considerably worse than elsewhere. We'll
;; need everything we can get.
(when (boundp 'w32-get-true-file-attributes)
  ;; Decreases file IO workload.
  (setq w32-get-true-file-attributes nil)
  ;; Faster IPC.
  (setq w32-pipe-read-delay 0)
  ;; Read more at a time (was 4K).
  (setq w32-pipe-buffer-size (* 64 1024))
  ;; The clipboard on Windows could be in another encoding (likely
  ;; utf-16), so let Emacs/the OS decide what to use there.
  (setq selection-coding-system 'utf-8))
