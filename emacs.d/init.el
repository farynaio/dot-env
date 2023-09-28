(setq
  gc-cons-threshold (* 1024 1024 2)
  ;; gc-cons-threshold (* 1024 800)
  gc-cons-percentage 0.1
  read-process-output-max (* 1024 1024 512))
;; (setq gc-cons-threshold 402653184)
;; (setq gc-cons-percentage 0.6)

;; (toggle-debug-on-error)

(setenv "SHELL" (executable-find "bash"))
(setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/10:/usr/local/opt/libgccjit/lib/gcc/10:/usr/local/opt/gcc/lib/gcc/10/gcc/x86_64-apple-darwin19/10.2.0")

(setq-default auth-sources '("~/.authinfo.gpg" "~/.netrc.gpg" "~/.authinfo" "~/.netrc"))

(eval-when-compile
  (defvar oauth--token-data ())
  (defvar url-http-method ())
  (defvar url-http-data ())
  (defvar url-http-extra-headers ())
  (defvar url-callback-function ())
  (defvar url-callback-arguments ()))

;; (setenv "LC_ALL" "en_US.UTF-8")
;; (setenv "LANG" "en_US.UTF-8")
;; (setenv "LANGUAGE" "en_US.UTF-8")
;; (setenv "LC_COLLATE" "en_US.UTF-8")
;; (setenv "LC_CTYPE" "en_US.UTF-8")
;; (setenv "LC_MESSAGES" "en_US.UTF-8")
;; (setenv "LC_MONETARY" "en_US.UTF-8")
;; (setenv "LC_NUMERIC" "en_US.UTF-8")
;; (setenv "LC_TIME" "en_US.UTF-8")
;; (setenv "GPG_AGENT_INFO" nil)

;; (setq shell-file-name "/bin/sh")
;; (setenv "PATH" (concat "/usr/local/opt/rbenv/shims:/usr/local/opt/rbenv/bin:" (getenv "PATH")))
;; (setenv "PATH" (concat "~/.rbenv/shims:" "~/.rbenv/bin:" "/usr/local/bin:" (getenv "PATH")))

(when (eq system-type 'darwin)
  (setq browse-url-chrome-program "chrome")
  (when (file-accessible-directory-p "/Applications/Firefox.app")
    ;; (setenv "PATH" (concat "~/Applications/Firefox.app/Contents/MacOS:" (getenv "PATH")))
    ;; (add-to-list 'exec-path "/Applications/Firefox.app/Contents/MacOS")
    (setq browse-url-generic-program "/Applications/Firefox.app/Contents/MacOS/firefox")))

;; (add-to-list 'exec-path "/usr/local/opt/rbenv/shims")
;; (add-to-list 'exec-path "/usr/local/opt/rbenv/bin")
(add-to-list 'exec-path "~/.npm-packages/bin")
(add-to-list 'exec-path "~/.rbenv/shims")

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/modules")
(add-to-list 'load-path "~/.emacs.d/modules/devel")
(add-to-list 'load-path "~/.emacs.d/modules/snippets")

(setq package-enable-at-startup nil)

(defvar bootstrap-version)
(defvar native-comp-deferred-compilation-deny-list nil)
(let ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
       (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq
  use-package-compute-statistics t
  use-package-expand-minimally t)

(setq
  straight-use-package-by-default t
  straight-disable-compile nil
  straight-vc-git-default-protocol 'https
  straight-check-for-modifications '(check-on-save find-when-checking))

(setq enable-local-eval t)

;; https://www.reddit.com/r/emacs/comments/8sykl1/emacs_tls_defaults_are_downright_dangerous/
;;(setq
;; network-security-level 'medium
;; gnutls-verify-error t
;; tls-checktrust t
;; gnutls-trustfiles
;; '(
;;   "/etc/ssl/cert.pem"
;;   "/usr/local/etc/openssl/cert.pem"
;;   "/usr/local/etc/openssl@1.1/cert.pem"
;;   "~/.certs/znc.pem"
;;   ))
;;(setq-default auth-sources '("~/.authinfo.gpg" "~/.netrc.gpg" "~/.authinfo" "~/.netrc"))

;; TODO is it needed?
;; (setq exec-path-from-shell-check-startup-files nil)

(use-package auto-compile
  :config
  (auto-compile-on-load-mode))

;; TODO safe to remove?
;; (use-package use-package-ensure-system-package)

(use-package diminish
  :config
  ;; (diminish 'editorconfig-mode)
  (diminish 'emacs-lock-mode)
  (diminish 'auto-revert-mode)
  (diminish 'visual-line-mode)
  (diminish 'js-mode "JS")
  (diminish 'reveal-mode)
  (diminish 'lisp-interaction-mode "Lisp-interaction"))

(setq load-prefer-newer t)

(use-package emacsql
  :straight (:type git
             :host github
             :repo "magit/emacsql"
             :branch "main"))

(use-package emacsql-sqlite3
  :straight (:type git
             :host github
             :repo "cireu/emacsql-sqlite3"
             :branch "master"))

(use-package org)
(use-package org-contrib
  :after org)

;; This is necessary to fix PATH problems in Mac OS environments for shell-command.
;; (use-package exec-path-from-shell
;;   :if (memq window-system '(mac ns x))
;;   :config
;;   (exec-path-from-shell-initialize))

(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(run-with-idle-timer (* 60 5) t 'garbage-collect)

(when (eq system-type 'darwin)
  (add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")
  (setq
    mac-command-modifier 'super
    ns-right-alternate-modifier nil))

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

(set-default-coding-systems 'utf-8)

(require 'tls)
(require 'gnutls)

(defvar my/local-config-file-path (expand-file-name "~/.emacs.d/emacs-local-config/local-config.el"))
(defvar my/emacs-should-compile nil)

(when my/emacs-should-compile
  (setq
    comp-speed 2
    comp-deferred-compilation t
    package-native-compile t
    comp-async-report-warnings-errors nil)
  (add-to-list 'exec-path (concat invocation-directory "bin") t))

;; Load my custom modules
(if (not (file-exists-p my/local-config-file-path))
  (error (concat "'emacs-local-config/local-config.el' file not exists"))
  (message (concat "Loading " my/local-config-file-path "..."))
  (load my/local-config-file-path))
  (when (and (fboundp 'native-comp-available-p) (native-comp-available-p) my/emacs-should-compile)
    (native--compile-async `(,my/local-config-file-path) t nil))

(setq
  ring-bell-function 'ignore
  visible-bell nil
  echo-keystrokes 0
  create-lockfiles nil ; this should be safe as long I'm the only user of FS
  enable-recursive-minibuffers nil ;; ?
  apropos-do-all t
  compilation-always-kill t
  compilation-ask-about-save nil
  compilation-scroll-output t
  confirm-nonexistent-file-or-buffer nil
  idle-update-delay 2
  minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
  initial-scratch-message nil
  inhibit-startup-screen t
  x-underline-at-descent-line t
  process-connection-type nil
  fill-column 99
  default-directory "~/"
  initial-buffer-choice t
  confirm-kill-processes nil
  password-cache-expiry nil)

(setq
  make-backup-files nil
  backup-by-copying t
  kept-new-versions 5
  kept-old-versions 5
  delete-old-versions t
  version-control t
  auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/" t))
  backup-directory-alist '(("." . "~/.emacs.d/backups")))

(set-language-environment "UTF-8")

(defun my/reload-config ()
	"Reload config."
	(interactive)
	(load-file (expand-file-name "init.el" user-emacs-directory))
  (message "Config reloaded."))

(display-time-mode -1)

(defvar *protected-buffers* '("*scratch*" "*Messages*" "Notes.org")
  "Buffers that cannot be killed.")

(defun my/protected-buffers ()
  "Protects some buffers from being killed."
  (dolist (buffer *protected-buffers*)
    (when (get-buffer buffer)
      (with-current-buffer buffer
        (emacs-lock-mode 'kill)))))

(add-hook 'emacs-startup-hook 'my/protected-buffers 90)

(setq safe-local-variable-values
  '(
     (org-hide-emphasis-markers . t)
     (ispell-dictionary . "en")
     (ispell-dictionary . "pl")
     (my/language-local . "pl")
     (my/language-local . "en")
     (org-use-property-inheritance . t)
     (org-confirm-babel-evaluate)
     (eval progn
       (add-hook
         (quote js-mode-hook)
         (lambda nil
           (flycheck-mode -1))
         t ))
     (browse-url-browser-function . my/browse-url-tor)
     ))

;; TODO is this working?
;; Disable warning on risky variables set on file local level.
;; (defun risky-local-variable-p (sym &optional _ignored) nil)
;; (advice-add 'risky-local-variable-p :override #'ignore)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :custom
  (exec-path-from-shell-variables '("MANPATH"))
  :config
  (dolist
    (var
      '(
         "LANG"
         "LANGUAGE"
         "LC_COLLATE"
         "LC_CTYPE"
         "LC_MESSAGES"
         "LC_MONETARY"
         "LC_NUMERIC"
         "LC_TIME"
         "GPG_AGENT_INFO"
         "NPM_TOKEN"
         "NVM_BIN"
         "SSH_AUTH_SOCK"
         "SSH_AGENT_PID"
         ))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize)
  (add-to-list 'exec-path (getenv "NVM_BIN"))
  (setenv "PATH" (format "%s:%s" (getenv "NVM_BIN") (getenv "PATH"))))

(setq custom-file (expand-file-name "emacs-local-config/custom.el" user-emacs-directory))

; Load my custom-set-variables settings
(if (or (null custom-file) (not (file-exists-p custom-file)))
  (error (format "File '%s' not exists" custom-file))
  (message (format "Loading %s..." custom-file))
  (load custom-file)
  (when (and (fboundp 'native-comp-available-p) (native-comp-available-p) my/emacs-should-compile)
    (native--compile-async `(,custom-file) t nil)))

(when (and (fboundp 'native-comp-available-p) (native-comp-available-p) my/emacs-should-compile)
  (native--compile-async '("~/.emacs.d/lisp/" "~/.emacs.d/themes/" "~/.emacs.d/modules/" "~/.emacs.d/init.el") t nil))
;;; init.el ends here
