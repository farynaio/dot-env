(eval-when-compile
  (defvar oauth--token-data ())
  (defvar url-http-method ())
  (defvar url-http-data ())
  (defvar url-http-extra-headers ())
  (defvar url-callback-function ())
  (defvar url-callback-arguments ()))

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/modules/")
(add-to-list 'exec-path "/usr/local/bin")

(require 'package)
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))
(unless (assoc-default "gnu" package-archives)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(unless (assoc-default "tromey" package-archives)
  (add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(setq
  use-package-verbose t
  use-package-always-ensure t)

(unless (package-installed-p 'org-plus-contrib)
  (package-install 'org-plus-contrib))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(require 'gnutls)

(when (eq system-type 'darwin)
  (add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem"))

(use-package auto-compile
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)

(require 'bind-key)
(require 'autorevert)
(require 'elisp-mode)

(require 'inc-dec-at-point)
(eval-after-load 'inc-dec-at-point
  '(progn
     (bind-key "C-c +" #'increment-integer-at-point)
     (bind-key "C-c -" #'decrement-integer-at-point)))

(add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "elisp")))

(use-package diminish
  :config
  (progn
    ;; (diminish 'editorconfig-mode)
    (diminish 'auto-revert-mode)
    (diminish 'company-mode)
    (diminish 'eldoc-mode)
    ;; (diminish 'auto-highlight-symbol-mode)
    (diminish 'abbrev-mode " A")))

;; This is necessary to fix PATH problems in Mac OS environments for shell-command.
(use-package exec-path-from-shell
  :config
  (progn
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize))))

(use-package miniedit)
(use-package calfw)

(require 'sql)

(use-package dash)
(use-package monitor)
(use-package popup)
(use-package oauth2)

(setq my/text-modes (list 'org-mode-map 'emacs-lisp-mode-map))

;; My modules
(require 'my-evil)
(require 'my-navigation)
(require 'my-writing)
(require 'my-org)
(require 'my-devel)
(require 'my-email)
(require 'my-dired)
(require 'my-notifications)

;; (use-package transpose-frame)
;; (use-package wgrep
;; 	:config
;; 	(progn
;; 		(if (commandp 'wgrep)
;; 				(progn
;; 					(setq wgrep-enable-key "r")))))
(use-package hl-todo)
(use-package editorconfig
  :config (diminish 'editorconfig-mode))
(use-package centered-cursor-mode)
(use-package auto-highlight-symbol
  :config
  (progn
    (diminish 'auto-highlight-symbol-mode)
    (setq ahs-idle-interval 0)))
(use-package imenu-anywhere)
;; (use-package smex)  ; better search for ido mode
(use-package with-editor)  ; dependency for other package
(use-package neotree)
(use-package multiple-cursors)

(setq custom-safe-themes t)
;; (require 'color-theme-sanityinc-tomorrow)

;; (eval-after-load 'color-theme-sanityinc-tomorrow
  ;; '(progn
    ;; (color-theme-sanityinc-tomorrow-night)))

(use-package persistent-scratch
  :config
  (progn
    (persistent-scratch-setup-default)))

(use-package guide-key
  :defer t
  :diminish guide-key-mode
  :config
  (progn
    (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
    (guide-key-mode 1)))

(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LANG" "en_US.UTF-8")
(setenv "LANGUAGE" "en_US.UTF-8")
(setenv "LC_COLLATE" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")
(setenv "LC_MESSAGES" "en_US.UTF-8")
(setenv "LC_MONETARY" "en_US.UTF-8")
(setenv "LC_NUMERIC" "en_US.UTF-8")
(setenv "LC_TIME" "en_US.UTF-8")

(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(require 'epa-file)
(require 'epg-config)
(require 'multiple-cursors)
;; (require 'wgrep)
(require 'calfw)
(require 're-builder)
;; (setq reb-re-syntax 'string)

(epa-file-enable)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

(bind-key ", c d"
  (lambda ()
    "Copy the current buffer file name to the clipboard."
    (interactive)
    (let ((filename
            (if (equal major-mode 'dired-mode)
              default-directory
              (buffer-file-name))))
      (when filename
        (kill-new filename)
        (message "Copied buffer file name '%s' to the clipboard." filename))))
  evil-normal-state-map)

(bind-key "C-x C-SPC" 'rectangle-mark-mode)

(display-time-mode nil)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq
  backup-by-copying t
  kept-new-versions 5
  kept-old-versions 5
  delete-old-versions t
  version-control t
  auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/" t))
  backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq
  visible-bell 1
 ring-bell-function 'ignore)
(setq inhibit-startup-screen t)
(setq ns-right-alternate-modifier nil)
(setq tab-width 2)

(setq undo-limit 10000)

(setq
  gc-cons-threshold (* 511 1024 1024)
  gc-cons-percentage 0.5)
(run-with-idle-timer 5 t #'garbage-collect)

;; (setq garbage-collection-messages t)
;; profiler-start
;; profiler-report

(setq
  bookmark-save-flag t
  show-paren-delay 0)

(bind-key "M-%" #'query-replace-regexp)

(setq help-window-select t)
(setq column-number-mode t)
(setq-default word-wrap t)
(setq compare-ignore-case t)
(setq compare-ignore-whitespace t)

(setq create-lockfiles nil) ; this should be safe as long I'm the only user of FS

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'ar #'align-regexp)

(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))


(setq sentence-end-double-space nil)

(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

(remove-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(setq dabbrev-friend-buffer-function '(lambda (other-buffer)
                                        (< (buffer-size other-buffer) (* 1 1024 1024))))
(global-set-key (kbd "M-/") 'hippie-expand)

(require 'hippie-exp)
(setq hippie-expand-try-functions-list
      '( ;;yas-hippie-try-expand ; requires yasnippet plugin
        try-expand-all-abbrevs
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-all-buffers
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))
(add-hook 'text-mode-hook 'abbrev-mode)

(setq mac-command-modifier 'super)

;; Mappings / Shortcuts
(global-set-key (kbd "C-x C-b") #'ibuffer) ; list buffers for editing
;; (global-set-key (kbd "C-c n") #'neotree-toggle)
(global-set-key (kbd "C-x 4 t") #'flop-frame)
(global-set-key (kbd "s-w") #'kill-ring-save)
(global-set-key (kbd "C-j") #'join-line)

;; (when (executable-find "curl")
;;   (setq helm-google-suggest-use-curl-p t))

;; Multiline cursor
(global-set-key (kbd "C->") #'mc/mark-next-like-this)
(global-set-key (kbd "C-<") #'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") #'mc/mark-all-like-this)

(defun air-revert-buffer-noconfirm ()
  (interactive)
  (revert-buffer :ignore-auto :noconfirm)
  (message (concat "Buffer '" (file-name-nondirectory buffer-file-name) "' reloaded.")))

(bind-key "s-u" 'air-revert-buffer-noconfirm global-map)

(defun air-toggle-maximize-buffer ()
   "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(defun my/move-current-window-to-new-frame ()
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

(global-set-key (kbd "C-x |") 'air-toggle-maximize-buffer)

;; Modes
(global-auto-revert-mode 1)
(blink-cursor-mode 0)
(global-linum-mode 1)
(editorconfig-mode 1)

(show-paren-mode 1)
(delete-selection-mode 1)
(scroll-bar-mode -1)
(if window-system (tool-bar-mode -1))

(desktop-save-mode 1)
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(setq desktop-buffers-not-to-save
  (concat "\\("
    "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
    "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb\\|ido.*"
    "\\)$"))

;; blogging
;; http://www.i3s.unice.fr/~malapert/org/tips/emacs_orgmode.html
;; (require 'ox-publish)
;; (setq org-html-coding-system 'utf-8-unix)
;; (setq org-html-head-include-default-style nil)
;; (setq org-html-head-include-scripts nil)
;; (setq org-html-validation-link nil)

(setq ack-path (executable-find "ack"))

(require 'grep)

;; (eval-after-load 'grep
;; 	'(progn
;; 		 (add-to-list 'grep-find-ignored-directories "auto-save-list")
;; 		 (add-to-list 'grep-find-ignored-directories "autosaves")
;; 		 (add-to-list 'grep-find-ignored-directories "backups")
;; 		 (add-to-list 'grep-find-ignored-directories "elpa")
;; 		 (add-to-list 'grep-find-ignored-directories "lisp")
;; 		 (add-to-list 'grep-find-ignored-directories "tools"))

;; (if ack-path
  ;; (grep-apply-setting 'grep-command "ack --with-filename --nofilter --nogroup ")
  ;; (message "No 'ack' executable found."))

  ;; (setq grep-program grep-command) ; ack
    ;; (setq sr-grep-command grep-program)
    ;; (grep-apply-setting 'grep-command "ack --with-filename --nofilter --nogroup ")
    ;; "ack --with-filename --nofilter --nogroup ")
    ;; (grep-apply-setting 'grep-command "ack --with-filename --nofilter --nogroup ")
    ;; (grep-apply-setting 'grep-find-template "find <D> <X> -type f <F> -exec ack --with-filename --nofilter --nogroup '<R>' /dev/null {} +")

  ;;   (grep-apply-setting 'grep-find-template
  ;;     (concat "find . -type f -exec " ack-path " --with-filename --nofilter --nogroup '<R>' /dev/null {} +"))
  ;;   )
  ;; (message "No 'ack' executable found.")
  ;; )

(defun reload-config ()
	"Reload config."
	(interactive)
	(load-file (expand-file-name "init.el" user-emacs-directory))
  (message "Config reloaded."))

;; (set-cursor-color "#ffffff")
(setq
  custom-safe-themes t
  default-font "-*-Menlo-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")
(set-face-attribute 'default nil :font default-font)

(when (file-exists-p my/local-config-file-path)
  (message (concat "Loading " my/local-config-file-path "..."))
  (load my/local-config-file-path))

(setq safe-local-variable-values
  '((ispell-dictionary . "en")
     (ispell-dictionary . "pl")))

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook (lambda ()
                             (mapc (lambda (project-root)
                                     (remhash project-root projectile-project-type-cache)
                                     (remhash project-root projectile-projects-cache)
                                     (remhash project-root projectile-projects-cache-time)
                                     (when projectile-verbose
                                       (message "Invalidated Projectile cache for %s."
                                         (propertize project-root 'face 'font-lock-keyword-face))))
                               (projectile-hash-keys projectile-projects-cache))
                             (projectile-serialize-cache)))

;; (use-package company-emoji
;;   :config
;;   (progn
;;     (add-to-list 'company-backends 'company-emoji)))

(defun my/set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display emoji properly."
  (if (eq system-type 'darwin)
      ;; For NS/Cocoa
      (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
    ;; For Linux
    (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))

;; For when Emacs is started in GUI mode:
(my/set-emoji-font nil)

;; Hook for when a frame is created with emacsclient
;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
(add-hook 'after-make-frame-functions 'my/set-emoji-font)

(display-time-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
;; (setq debug-on-error nil)

;; nice themes
;; deeper-blue
;; tango dark
;; wombat

;; light theme
;; adwaita

(setq custom-theme-directory "~/.emacs.d/themes/")

(defvar my/current-theme nil)

(defun my/set-dark-theme ()
  (when (featurep 'adwaita-theme)
    (disable-theme 'adwaita)
    (unload-feature 'adwaita-theme))
  (load-theme 'wombat)
  (custom-theme-set-faces
    'wombat
    '(cursor ((t (:inherit nil :underline nil :background "White"))))
    '(region ((t (:inherit nil :underline nil :foreground "White" :background "RoyalBlue4"))))
    '(hl-line ((t (:inherit nil :underline nil :background "gray34"))))
    '(ledger-font-xact-highlight-face ((t nil)))
    '(org-level-2 ((t (:inherit outline-2 :foreground "cyan2"))))
    '(org-level-3 ((t (:inherit outline-3 :foreground "DarkGoldenrod2"))))
    '(org-level-4 ((t (:inherit outline-4 :foreground "plum1"))))
    '(org-priority ((t (:inherit font-lock-keyword-face :foreground "gold3")))))
  (setq my/current-theme 'wombat))

(defun my/set-light-theme ()
  (when (featurep 'wombat-theme)
    (disable-theme 'wombat)
    (unload-feature 'wombat-theme))
  (load-theme 'adwaita)
  (setq my/current-theme 'adwaita))

(defun my/toggle-theme ()
  (interactive)
  (if (eq  my/current-theme 'wombat)
    (my/set-light-theme)
    (my/set-dark-theme)))

(defalias 'toggle-theme #'my/toggle-theme)

(my/set-dark-theme)

(setq safe-local-variable-values
  (quote
    ((org-hide-emphasis-markers . t)
      (ispell-dictionary . "en")
      (ispell-dictionary . "pl"))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(custom-enabled-themes (quote (wombat)))
  '(ledger-reports
     (quote
       ((#("Net value balance" 0 1
            (idx 0))
          "%(binary) [[ledger-mode-flags]] -f %(ledger-file) bal Assets Liabilities")
         (#("Net value balance (cleared)" 0 1
             (idx 1))
           "%(binary) [[ledger-mode-flags]] -f %(ledger-file) --cleared bal Assets Liabilities")
         (#("bal" 0 1
             (idx 2))
           "%(binary) -f %(ledger-file) bal")
         (#("reg" 0 1
             (idx 3))
           "%(binary) -f %(ledger-file) reg")
         (#("payee" 0 1
             (idx 4))
           "%(binary) -f %(ledger-file) reg @%(payee)")
         (#("account" 0 1
             (idx 5))
           "%(binary) -f %(ledger-file) reg %(account)")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
