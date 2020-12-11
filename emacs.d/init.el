(setq my/file-name-handler-alist file-name-handler-alist)

(setq gc-cons-threshold (* 1024 1024 1024))
(setq gc-cons-percentage 0.5)
(setq file-name-handler-alist nil)
;; (setq gc-cons-threshold 402653184)
;; (setq gc-cons-percentage 0.6)

(setq confirm-kill-processes nil)

(setq exec-path-from-shell-check-startup-files nil)

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
(setenv "SHELL" "/bin/ksh")
;; (setenv "PATH" (concat "/usr/local/opt/rbenv/shims:/usr/local/opt/rbenv/bin:" (getenv "PATH")))
(setenv "PATH" (concat (getenv "HOME") "/.rbenv/shims:" (getenv "HOME") "/.rbenv/bin:" (getenv "PATH")))
(setq exec-path (cons (concat (getenv "HOME") "/.rbenv/shims") (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))

(when (string-equal system-type "darwin")
  (when (file-accessible-directory-p "/Applications/Firefox.app/Contents/MacOS")
    (setenv "PATH" (concat (getenv "HOME") "/Applications/Firefox.app/Contents/MacOS:" (getenv "PATH")))
    (setq exec-path (cons "/Applications/Firefox.app/Contents/MacOS" exec-path))
    (setq browse-url-generic-program "firefox")))

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/modules/")
(add-to-list 'load-path "~/.emacs.d/modules/devel")
;; (add-to-list 'exec-path "/usr/local/opt/rbenv/shims")
;; (add-to-list 'exec-path "/usr/local/opt/rbenv/bin")
(add-to-list 'exec-path "/usr/local/bin")

(setq package-check-signature nil)

(require 'package)
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")))
(unless (assoc-default "elpa" package-archives)
  (add-to-list 'package-archives '("elpa" . "https://orgmode.org/elpa/") t))
(unless (assoc-default "gnu" package-archives)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
;; (unless (assoc-default "tromey" package-archives)
;;   (add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(setq
  use-package-verbose nil
  use-package-always-ensure t)

(unless (package-installed-p 'org-plus-contrib)
  (package-install 'org-plus-contrib))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package bind-key)

(use-package diminish
  :config
  ;; (diminish 'editorconfig-mode)
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode)
  (diminish 'visual-line-mode)
  (diminish 'editorconfig-mode)
  (diminish 'js-mode "JS")
  (diminish 'abbrev-mode))

(require 'gnutls)

(setq enable-local-eval t)
(setq safe-local-eval-forms (list))
(add-to-list 'safe-local-eval-forms '(progn (jarfar/org-tasks-refile-targets-local)))

(when (eq system-type 'darwin)
  (add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem"))

(use-package auto-compile
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)

(use-package dash)

;; This is necessary to fix PATH problems in Mac OS environments for shell-command.
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(use-package oauth2)

(setq my/text-modes (list 'org-mode-map 'emacs-lisp-mode-map))

(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(run-with-idle-timer 5 t #'garbage-collect)

;; My modules
(require 'my-path)

(when (file-exists-p my/local-config-file-path)
  (message (concat "Loading " my/local-config-file-path "..."))
  (load my/local-config-file-path))

(defvar my-edit-activate nil)
(defvar my-writing-activate nil)
(defvar my-evil-activate nil)
(defvar my-org-activate nil)
(defvar my-navigation-activate nil)
(defvar my-devel-activate nil)
(defvar my-email-activate nil)
(defvar my-dired-activate nil)
(defvar my-notifications-activate nil)
(defvar my-www-activate nil)
(defvar my-irc-activate nil)
(defvar my-theme-activate nil)
(defvar my-shell-activate nil)
(defvar my-encrypt-activate nil)
(defvar my-cleanup-activate nil)
(defvar my-rss-activate nil)
(defvar my-org-caldav-activate nil)
(defvar my-taskjuggler nil)
(defvar my-git nil)
(defvar my-python nil)
(defvar my-php nil)

(setq
  my-edit-activate t
  my-git t
  my-evil-activate t
  my-navigation-activate t
  my-org-activate t
  my-writing-activate t
  my-devel-activate t
  my-email-activate nil
  my-dired-activate t
  my-notifications-activate t
  my-www-activate nil
  my-irc-activate t
  my-theme-activate t
  my-shell-activate t
  my-encrypt-activate t
  my-cleanup-activate t
  my-rss-activate t
  my-org-caldav-activate nil
  my-taskjuggler nil
  my-python t
  my-php t
)

(when my-evil-activate (require 'my-evil))
(when my-edit-activate (require 'my-edit))
(when my-git (require 'my-git))
(when my-encrypt-activate (require 'my-encrypt))
(when my-navigation-activate (require 'my-navigation))
(when my-theme-activate (require 'my-theme))
(when my-org-activate (require 'my-org))
(when my-org-caldav-activate (require 'my-org-caldav))
(when my-notifications-activate (require 'my-notifications))
(when my-writing-activate (require 'my-writing))
(when my-dired-activate (require 'my-dired))
(when my-cleanup-activate (require 'my-cleanup))
(when my-devel-activate (require 'my-devel))
(when my-python (require 'my-python))
(when my-php (require 'my-php))
(when my-shell-activate (require 'my-shell))
(when my-rss-activate (require 'my-rss))
(when my-email-activate (require 'my-email))
(when my-www-activate (require 'my-www))
(when my-irc-activate (require 'my-irc))
(when my-taskjuggler (require 'my-taskjuggler))

;; (setq
  ;; gc-cons-threshold (* 511 1024 1024)
  ;; gc-cons-percentage 0.5)

;; (setq garbage-collection-messages t)
;; profiler-start
;; profiler-report

(server-start)

(defun jarfar/open-buffers-on-startup ()
  (when (file-exists-p "~/.emacs.d/init.el")
    (find-file "~/.emacs.d/init.el"))
  (switch-to-buffer "*scratch*"))

(add-hook 'emacs-startup-hook 'jarfar/open-buffers-on-startup)

(savehist-mode 1)
(setq savehist-file "~/.emacs.d/savehist"
  history-length t
  history-delete-duplicates t
  savehist-save-minibuffer-history 1
  savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

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

(add-hook 'desktop-save-mode-hook 'jarfar/desktop-config)

(defun jarfar/desktop-config ()
  "Configure desktop-save mode."

  (when desktop-save-mode
    (pushnew 'dired-mode desktop-modes-not-to-save)
    (pushnew 'Info-mode desktop-modes-not-to-save)
    (pushnew 'info-lookup-mode desktop-modes-not-to-save)
    (setq desktop-buffers-not-to-save
          (concat "\\("
                  "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
                  "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb\\|ido.*"
                  "\\)$"))))

;; (desktop-save-mode 1)

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
     (my/language-local . "pl")
     (my/language-local . "en")
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
       (("Household expenses period" "ledger [[ledger-mode-flags]] -b 2018/09/14 -e 2018/10/13 -f /Users/devil/Dropbox/emacs/ledger/private.ledger reg \"^Assets:Reimbursements:Household\"")
         (#("Net value balance" 0 1
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
           "%(binary) -f %(ledger-file) reg %(account)"))))
  '(package-selected-packages
     (quote
       (lsp-treemacs lsp-ivy eglot ac-js2 toml-mode register-list org-roam undo-fu parrot highlight yaml-mode xref-js2 which-key web-mode web-beautify vue-mode vimrc-mode use-package undo-tree tide terraform-mode synosaurus sr-speedbar smartscan rjsx-mode realgud rainbow-mode rainbow-delimiters projectile-rails prettier-js persistent-scratch org-roam-server org-review org-plus-contrib org-mime org-journal org-drill ob-async oauth2 neotree minimap miniedit magit ledger-mode langtool js2-refactor japanese-holidays jade-mode ivy-rich ivy-hydra hl-todo guide-key goto-last-change google-translate git-gutter geben exec-path-from-shell evil-visualstar evil-surround evil-matchit evil-anzu eslintd-fix emojify emmet-mode elpy elfeed-web elfeed-org elfeed-goodies editorconfig dtrt-indent drag-stuff dockerfile-mode diminish deft dash-at-point counsel company-web company-statistics company-quickhelp company-php company-org-roam company-lsp centered-cursor-mode calfw-org calfw avy auto-highlight-symbol auto-compile auctex artbollocks-mode all-the-icons ag add-node-modules-path ac-php))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:inherit nil :background "gray82" :underline nil))))
 '(hl-line ((t (:background "gray32"))))
 '(org-priority ((t (:inherit font-lock-keyword-face :foreground "gold3"))))
 '(region ((t (:background "DodgerBlue4" :foreground "#f6f3e8")))))
