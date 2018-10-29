(setq my/file-name-handler-alist file-name-handler-alist)
(setq
  gc-cons-threshold (* 511 1024 1024)
  gc-cons-percentage 0.5
  ;; gc-cons-threshold 402653184
  ;; gc-cons-percentage 0.6
  file-name-handler-alist nil)

(eval-when-compile
  (defvar oauth--token-data ())
  (defvar url-http-method ())
  (defvar url-http-data ())
  (defvar url-http-extra-headers ())
  (defvar url-callback-function ())
  (defvar url-callback-arguments ()))

(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LANG" "en_US.UTF-8")
(setenv "LANGUAGE" "en_US.UTF-8")
(setenv "LC_COLLATE" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")
(setenv "LC_MESSAGES" "en_US.UTF-8")
(setenv "LC_MONETARY" "en_US.UTF-8")
(setenv "LC_NUMERIC" "en_US.UTF-8")
(setenv "LC_TIME" "en_US.UTF-8")
(setenv "SHELL" "/bin/bash")
(setenv "PATH" (concat (getenv "HOME") "/.rbenv/shims:" (getenv "HOME") "/.rbenv/bin:" (getenv "PATH")))
(setq exec-path (cons (concat (getenv "HOME") "/.rbenv/shims") (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))

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

(use-package bind-key)
(use-package dash)

;; This is necessary to fix PATH problems in Mac OS environments for shell-command.
(use-package exec-path-from-shell
  :config
  (progn
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize))))

(use-package oauth2)

(setq my/text-modes (list 'org-mode-map 'emacs-lisp-mode-map))

(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(run-with-idle-timer 5 t #'garbage-collect)

;; My modules
(require 'my-path)
(require 'my-edit)
(require 'my-evil)
(require 'my-writing)
(require 'my-org)
(require 'my-devel)
(require 'my-email)
(require 'my-dired)
(require 'my-notifications)
(require 'my-navigation)
(require 'my-www)
(require 'my-irc)
(require 'my-theme)
(require 'my-shell)
(require 'my-cleanup)

(when (file-exists-p my/local-config-file-path)
  (message (concat "Loading " my/local-config-file-path "..."))
  (load my/local-config-file-path))

;; (setq
  ;; gc-cons-threshold (* 511 1024 1024)
  ;; gc-cons-percentage 0.5)

;; (setq garbage-collection-messages t)
;; profiler-start
;; profiler-report

(savehist-mode 1)
(setq savehist-file "~/.emacs.d/savehist"
  history-length t
  history-delete-duplicates t
  savehist-save-minibuffer-history 1
  savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

(setq dabbrev-friend-buffer-function '(lambda (other-buffer)
                                        (< (buffer-size other-buffer) (* 1 1024 1024))))
(setq mac-command-modifier 'super)

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system        'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

(setq-default
  create-lockfiles nil ; this should be safe as long I'm the only user of FS
  enable-recursive-minibuffers nil ;; ?
  apropos-do-all t
  compilation-always-kill t
  compilation-ask-about-save nil
  compilation-scroll-output t
  confirm-nonexistent-file-or-buffer nil
  idle-update-delay 2
  minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
  history-length 500
  initial-scratch-message nil)

(setq inhibit-startup-screen t)

(setq-default
  make-backup-files nil
  backup-by-copying t
  kept-new-versions 5
  kept-old-versions 5
  delete-old-versions t
  version-control t
  auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/" t))
  backup-directory-alist '(("." . "~/.emacs.d/backups")))

(desktop-save-mode 1)
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(setq desktop-buffers-not-to-save
  (concat "\\("
    "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
    "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb\\|ido.*"
    "\\)$"))

(defun reload-config ()
	"Reload config."
	(interactive)
	(load-file (expand-file-name "init.el" user-emacs-directory))
  (message "Config reloaded."))

(display-time-mode -1)

(setq company-backends '(company-capf company-dabbrev-code company-dabbrev))

(setq safe-local-variable-values
  '(
     (org-hide-emphasis-markers . t)
     (ispell-dictionary . "en")
     (ispell-dictionary . "pl")
     (my/language-local . pl)
     (my/language-local . en)
     (org-use-property-inheritance . t)
     (org-confirm-babel-evaluate)
     (eval progn
       (add-hook
         (quote js-mode-hook)
        (lambda nil
           (flycheck-mode -1))
         t ))))

(setq
  ;; gc-cons-threshold 16777216
  ;; gc-cons-percentage 0.1
  file-name-handler-alist my/file-name-handler-alist)

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
         (#("Net value balance (uncleared)" 0 1
             (idx 2))
           "%(binary) [[ledger-mode-flags]] -f %(ledger-file) --uncleared bal Assets Liabilities")
         (#("bal" 0 1
             (idx 3))
           "%(binary) -f %(ledger-file) bal")
         (#("reg" 0 1
             (idx 4))
           "%(binary) -f %(ledger-file) reg")
         (#("payee" 0 1
             (idx 5))
           "%(binary) -f %(ledger-file) reg @%(payee)")
         (#("account" 0 1
             (idx 6))
           "%(binary) -f %(ledger-file) reg %(account)")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:inherit nil :background "White" :underline nil)))))
