; Folder with manualy added packages
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

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(unless (package-installed-p 'org-plus-contrib)
  (package-install 'org-plus-contrib))

(require 'use-package)

(use-package auto-compile
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)

(use-package diminish
  :config
  (progn
    (diminish 'editorconfig-mode)
    (diminish 'auto-revert-mode)
    (diminish 'auto-highlight-symbol-mode)
    (diminish 'undo-tree-mode)
    (diminish 'abbrev-mode " A")))

(use-package miniedit)
(use-package calfw)
(use-package calfw-org)

(use-package dash)
(use-package monitor)

(setq my/text-modes (list org-mode-map emacs-lisp-mode-map))
(setq my/devel-keymaps (list emacs-lisp-mode-map lisp-mode-map lisp-interaction-mode-map))

;; My modules
(require 'my-evil)
(require 'my-navigation)
(require 'my-writing)
(require 'my-org)
(require 'my-email)
(require 'my-devel)
(require 'my-dired)

(use-package oauth2
  :init
  (progn
    (defvar oauth--token-data ())
    (defvar url-http-extra-headers ())
    (defvar url-callback-function ())
    (defvar url-callback-arguments ())))

;; (use-package transpose-frame)
(use-package wgrep)
(use-package hl-todo)
(use-package editorconfig)
;; (use-package dash)
(use-package centered-cursor-mode)
(use-package auto-highlight-symbol
  :config
  (progn
    (setq ahs-idle-interval 0)))
(use-package imenu-anywhere)
;; (use-package smex)  ; better search for ido mode
(use-package with-editor)  ; dependency for other package
(use-package neotree)
(use-package multiple-cursors)
(use-package color-theme-sanityinc-tomorrow
  :init
  (progn
    (setq custom-safe-themes t))
  :config
  (progn
    (color-theme-sanityinc-tomorrow-night)))

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

(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(require 'epa-file)
(require 'epg-config)
(require 'multiple-cursors)
(require 'wgrep)
(require 'calfw)
(require 're-builder)
;; (setq reb-re-syntax 'string)

(epa-file-enable)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

(setq default-directory "~/.emacs.d")

;; (setq display-buffer-alist
;;   '(
;;      ("^.+\\.org\\(\\.gpg\\)?$"
;;        (display-buffer-reuse-window) . ((reusable-frames . t)))
;;      ("^\\(\\..+\\)\\|\\(.+\\..+\\)$"
;;        (display-buffer-reuse-window display-buffer-same-window display-buffer-reuse-window display-buffer-pop-up-frame) . ((reusable-frames . t)))))

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

(defun cal ()
  "Full month calendar by calfw-org-calendar."
  (interactive)
  (cfw:open-org-calendar))
(bind-key "C-x C-SPC" 'rectangle-mark-mode)

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
(setq
  gc-cons-threshold 3500000
  bookmark-save-flag t
  show-paren-delay 0)

(bind-key "M-%" #'query-replace-regexp)

(setq help-window-select t)
(setq column-number-mode t)
(setq-default word-wrap t)
(setq compare-ignore-case t)
(setq compare-ignore-whitespace t)

(setq holiday-bahai-holidays nil)
(setq holiday-local-holidays nil) ; set it one day
(setq org-agenda-include-diary t)

(setq create-lockfiles nil) ; this should be safe as long I'm the only user of FS

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'ar #'align-regexp)

(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

(setq undo-tree-visualizer-diff t)

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
(global-set-key (kbd "C-c C-r") #'air-revert-buffer-noconfirm)
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

(defun air-toggle-maximize-buffer ()
   "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(global-set-key (kbd "C-x |") 'air-toggle-maximize-buffer)

(if (commandp 'wgrep)
  (progn
    (setq wgrep-enable-key "r")))

;; Modes
(global-auto-revert-mode 1)
(blink-cursor-mode 0)
(global-linum-mode 1)
(editorconfig-mode 1)

(show-paren-mode 1)
(delete-selection-mode 1)
(scroll-bar-mode -1)
(if window-system (tool-bar-mode -1))

;; programming
(setq devel-buffers '("js" "jsx" "vim" "json" "java" "php" "css" "scss" "html" "md" "xml" "rb" "el"))

(add-hook 'find-file-hook
  (lambda ()
    (let* ((found nil)
            (buf-name (file-name-extension buffer-file-name) ))
	    (dolist (i devel-buffers)
	      (when (string= buf-name i)
          (hl-line-mode)
          (hl-todo-mode)
          (auto-highlight-symbol-mode)
          (rainbow-mode)
          (setq found t)))
        (when (not found)
          ))))

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

(if ack-path
  (progn
  ;; (setq grep-command "ack --with-filename --nofilter --nogroup ")
  ;; (setq grep-program grep-command) ; ack
    ;; (setq sr-grep-command grep-program)
    ;; (grep-apply-setting 'grep-command "ack --with-filename --nofilter --nogroup ")
    ;; "ack --with-filename --nofilter --nogroup ")
    ;; (grep-apply-setting 'grep-command "ack --with-filename --nofilter --nogroup ")
    ;; (grep-apply-setting 'grep-find-template "find <D> <X> -type f <F> -exec ack --with-filename --nofilter --nogroup '<R>' /dev/null {} +")
    (grep-apply-setting 'grep-find-template
      (concat "find . -type f -exec " ack-path " --with-filename --nofilter --nogroup '<R>' /dev/null {} +"))
    )
  (message "No 'ack' executable found.")
  )

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(debug-on-error nil)
  '(package-selected-packages
     (quote
       (langtool org-alert oauth2 wgrep w3m use-package ujelly-theme twilight-theme transpose-frame sublime-themes smartscan rainbow-mode persistent-scratch org-plus-contrib org-evil neotree multiple-cursors monokai-theme miniedit material-theme magit ivy-hydra imenu-anywhere image-dired+ hl-todo guide-key goto-last-change git-gutter evil-surround evil-mu4e evil-matchit editorconfig dracula-theme dired+ diminish darktooth-theme counsel-projectile color-theme-sanityinc-tomorrow challenger-deep-theme centered-cursor-mode calfw-org calfw base16-theme badger-theme avy auto-highlight-symbol auto-compile artbollocks-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
