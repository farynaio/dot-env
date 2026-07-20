;; -*- lexical-binding: t; -*-
;; (setq debug-on-error t)

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq package-install-upgrade-built-in t)

;; (setq inhibit-default-init t)
(setq load-prefer-newer t)
(setq use-package-always-defer t
      use-package-always-ensure t)

(defvar my/tmp-dir (expand-file-name "tmp" user-emacs-directory))

(setq gc-cons-threshold (* 50 1000 1000)) ;; reduce startup GC pauses

(defalias 'yes-or-no-p 'y-or-n-p)
;; (setq confirm-kill-emacs #'yes-or-no-p)

(load-theme 'modus-vivendi t)

;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (eq system-type 'android)
    (progn
      (setq tool-bar-position 'bottom)
      (when (fboundp 'tool-bar-mode) (tool-bar-mode 1))
      (when (fboundp 'modifier-bar-mode) (modifier-bar-mode 1))
      ;; (setq touch-screen-word-select t)
      ;; (setq touch-screen-enable-hscroll nil)
      ;; (setq touch-screen-extend-selection t)
      ;; (setq touch-screen-preview-select t)
      (bind-key "<down-mouse-1>" nil))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1)))

;; Be less obnoxious
(blink-cursor-mode -1)
(tooltip-mode -1)

(setq
 byte-compile-warnings '(not obsolete)
 warning-suppress-log-types '((comp) (bytecomp))
 native-comp-async-report-warnings-errors 'silent)
(setq-default
 compilation-always-kill t
 compilation-scroll-output t
 compilation-skip-threshold 2
 warning-minimum-level :error
 warning-suppress-types '((comp) (bytecomp)))

;; Default frame configuration: full screen, good-looking title bar on macOS
(setq frame-resize-pixelwise t)
;; (setq window-resize-pixelwise t)

;; Display line numbers in programming mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-width 3)           ; Set a minimum width

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)

;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

(setq calc-internal-prec 20)


(setq gnutls-verify-error t)

(setq enable-local-eval t)

(setq-default
 compare-ignore-case t
 compare-ignore-whitespace t
 cursor-in-non-selected-windows t
 view-read-only t
 indent-tabs-mode nil
 require-final-newline nil
 mode-require-final-newline nil)

;; start in fullscreen
;; (when (display-graphic-p)
  ;; (set-frame-parameter nil 'fullscreen 'fullboth))

;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq default-frame-alist
      '((fullscreen . maximized)
        ;; You can turn off scroll bars by uncommenting these lines:
        ;; (vertical-scroll-bars . nil)
        ;; (horizontal-scroll-bars . nil)

        ;; Setting the face in here prevents flashes of
        ;; color as the theme gets activated
        (background-color . "#000000")
        (foreground-color . "#ffffff")))
;; (ns-appearance . dark)
;; (ns-transparent-titlebar . t)))

;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; (setq backward-delete-char-untabify-method 'hungry)

;; external physical keyboard mappings
(when (eq system-type 'android)
  (define-key key-translation-map (kbd "ESC [ 3 4 ~") (kbd "ESC"))
  (define-key key-translation-map (kbd "ESC [ 1 ; 3 b") (kbd "ALT"))
  (bind-keys
   ("M-[ 1 ; 2 a" . windmove-up)
   ("M-[ 1 ; 2 b" . windmove-down)
   ("M-[ 1 ; 5 a" . backward-paragraph)
   ("M-[ 1 ; 5 b" . forward-paragraph)
   ("M-[ 1 ; 3 a" . backward-paragraph)
   ("M-[ 1 ; 3 b" . forward-paragraph)
   ("M-[ 1 ; 5 c" . forward-word)
   ("M-[ 1 ; 5 d" . backward-word)
   ("M-[ 1 ; 3 c" . forward-word)
   ("M-[ 1 ; 3 d" . backward-word)))

(bind-keys
 ("C-<mouse-4>" . ignore)
 ("C-<mouse-5>" . ignore)
 ("C-<wheel-down>" . ignore)
 ("C-<wheel-up>" . ignore))

(bind-keys
 ("C-x =" . balance-windows))

;; Disable zooming with pinch / mouse wheel
;;
;; In addition to zooming with `C-x C-+` and `C-x C--`, there's a different,
;; buffer specific zoom that zooms insanel fast.
(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "C-<wheel-up>") 'mwheel-scroll)
(global-set-key (kbd "C-<wheel-down>") 'mwheel-scroll)

;; Disable horizontal scrolling
(setq
 mouse-wheel-tilt-scroll nil
 mouse-wheel-flip-direction nil)

(pixel-scroll-precision-mode)                         ; Smooth scrolling

;; Use common keystrokes by default
;; (cua-mode)

(setq
 initial-scratch-message nil
 inhibit-startup-message t
 inhibit-startup-screen nil
 inhibit-startup-echo-area-message (user-login-name)
 confirm-nonexistent-file-or-buffer nil
 x-underline-at-descent-line t
 initial-buffer-choice nil
 password-cache-expiry nil
 word-wrap t
 shift-select-mode nil
 sentence-end-double-space nil
 revert-without-query '(".*")
 show-paren-delay 0
 save-some-buffers-default-predicate t
 buffer-save-without-query t
 help-window-select t
 bookmark-save-flag 1)

(setq delete-by-moving-to-trash t)

(setq initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
(setq display-time-default-load-average nil) ; this information is useless for most

;; Automatically reread from disk if the underlying file changes
(setq auto-revert-avoid-polling t)
;; Some systems don't do file notifications well; see
;; https://todo.sr.ht/~ashton314/emacs-bedrock/11
(setq auto-revert-interval 5)
(setq auto-revert-check-vc-info t)
(setq auto-revert-verbose nil)
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

;; Save history of minibuffer
(setq history-length 1000
      savehist-additional-variables
      '(search-ring regexp-search-ring extended-command-history)
      savehist-autosave-interval 300)
(savehist-mode 1)

;; Emacs minibuffer configurations.
(use-package emacs
  :demand t
  :ensure nil
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode (display-graphic-p)) ;; Make right-click do something sensible
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

;; Move through windows with Ctrl-<arrow keys>
(windmove-default-keybindings 'shift) ; You can use other modifiers here

(setq font-lock-maximum-decoration t)

(unless (file-exists-p my/tmp-dir)
  (make-directory my/tmp-dir t))

;; Backup files (~file~)
(setq backup-directory-alist `((".*" . ,my/tmp-dir))
      backup-by-copying t    ; avoid symlink issues
      delete-old-versions t
      kept-new-versions 2
      kept-old-versions 2
      version-control t)     ; use numbered backups

;; Auto-save files (#file#)
(setq
 auto-save-file-name-transforms `((".*" ,(concat my/tmp-dir "/auto-save-") t))
 auto-save-list-file-prefix (concat  my/tmp-dir "/auto-saves-"))

;; Lockfiles
(setq create-lockfiles nil) ; optional: disable .#lockfiles

(run-with-idle-timer (* 60 5) t 'garbage-collect)

;; Prompt indicator for `completing-read-multiple'.
(when (< emacs-major-version 31)
  (advice-add #'completing-read-multiple :filter-args
              (lambda (args)
                (cons (format "[CRM%s] %s"
                              (string-replace "[ \t]*" "" crm-separator)
                              (car args))
                      (cdr args)))))

;; TAB cycle if there are only few candidates
(setq
 completion-cycle-threshold 1
 completions-detailed t                        ; Show annotations
 completion-auto-help 'always                  ; Open completion always; `lazy' another option
 completions-max-height 20                     ; This is arbitrary
 completions-format 'one-column
 completions-group t
 completion-auto-select 'second-tab)            ; Much more eager

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

(setq show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
(setq indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

(use-package diminish
  :demand t
  :config
  ;; (diminish 'eldoc-mode)
  (diminish 'visual-line-mode))

(use-package delight
  :demand t
  :config
  (delight '(
             ;; (eldoc-mode)
             (js-mode "JS" :major)
             (js2-mode "JS2" :major))))

(defun my/join-line ()
  (interactive)
  (join-line -1))

(use-package simple
  :demand t
  :ensure nil
  :bind (("C-c C-j" . my/join-line)
         :map visual-line-mode-map
         ;; kill entire line even when visual-line-mode enabled
         ("C-k" . kill-line))
  :custom
  (visual-line-fringe-indicators '(left-curly-arrow nil))
  (set-mark-command-repeat-pop t)
  (mark-ring-max 4)
  :config
  (global-visual-line-mode 1)
  (column-number-mode 1)

  ;; Apply `visual-line-mode' only on not `org-agenda-mode' buffers.
  ;; (advice-add 'visual-line-mode :around
              ;; (lambda (orig-fun &rest args)
                      ;; (unless (memq major-mode (list 'org-agenda-mode))
  ;; (apply orig-fun args))))
  )

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

;; Set default input method and coding system
(use-package mule
  :demand t
  :ensure nil
  :init
  ;; (setq default-input-method 'MacOSX)
  :config
  (setq locale-coding-system 'utf-8) ; pretty
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8))

(setq
 switch-to-buffer-in-dedicated-window 'pop
 switch-to-buffer-obey-display-actions t)

(xterm-mouse-mode 1)

(unless (file-exists-p my/tmp-dir)
  (make-directory my/tmp-dir t))

;; No electric indent
(setq electric-indent-mode nil)

;; Disable opening anything with web browser
(setq-default browse-url-browser-function #'ignore)

;; Include entire file path in title
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

;; Enable indentation+completion using the TAB key.
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)

;; Emacs 30 and newer: Disable Ispell completion function.
;; Try `cape-dict' as an alternative.
(setq text-mode-ispell-word-completion nil)

;; https://github.com/jwiegley/emacs-async/
(use-package async
  :demand t
  :config
  (require 'async-bytecomp)
  ;; (require 'dired-async)
  ;; (dired-async-mode 1)
  (async-bytecomp-package-mode 1)
  (setq async-bytecomp-allowed-packages '(all))
  (setq async-quiet-switch "-q")) ; maybe that for tramp only

(use-package isearch
  :demand t
  :ensure nil
  :custom
  (isearch-lazy-count t))

(delete-selection-mode 1)
(auto-compression-mode 1)

(global-hl-line-mode -1)

(use-package view
  :demand t
  :ensure nil
  :bind
  (:map view-mode-map
  ("SPC" . nil)
  ("RET" . nil)
  ("DEL" . nil)
  ("q" . nil)
  ("s" . nil)
  ("r" . nil)
  ("c" . nil)
  ("h" . help-for-help)))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-pcm-leading-wildcard t)
  (completion-category-overrides '((file (styles partial-completion)))))

(defalias 'qcalc #'quick-calc)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
