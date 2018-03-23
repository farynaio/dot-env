; Folder with manualy added packages
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'exec-path "/usr/local/bin")

(require 'package)
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")))
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

(use-package miniedit)
(use-package calfw)
(use-package calfw-org)

(use-package dash)
(use-package monitor)

(use-package evil
  :init
  (progn
    (setq
      evil-want-C-u-scroll t
      evil-want-C-i-jump nil))
  :config
  (progn
    (evil-mode 1)
    (bind-key "C-e"   #'move-end-of-line                    evil-normal-state-map)
    (bind-key "C-e"   #'move-end-of-line                    evil-visual-state-map)
    (bind-key "C-a"   #'my/smarter-move-beginning-of-line   evil-normal-state-map)
    (bind-key "C-a"   #'my/smarter-move-beginning-of-line   evil-visual-state-map)
    (bind-key "TAB"   #'indent-for-tab-command              evil-normal-state-map)
    (bind-key "TAB"   #'indent-for-tab-command              evil-visual-state-map)
    (bind-key "TAB"   #'tab-to-tab-stop                     evil-insert-state-map)
    (bind-key "/"     #'swiper                              evil-normal-state-map)
    (bind-key "C-x T" #'my/move-current-window-to-new-frame evil-normal-state-map)
    (bind-key ", s"   #'flyspell-mode                       evil-normal-state-map)
    (bind-key ", g s" #'magit-status                        evil-normal-state-map)
    (bind-key "C-w"   #'evil-delete-char                    evil-visual-state-map)
    (bind-key "h"     #'evil-first-non-blank                evil-normal-state-map)
    (bind-key "h"     #'evil-first-non-blank                evil-visual-state-map)
    (bind-key "l"     #'evil-end-of-line                    evil-normal-state-map)
    (bind-key "l"     #'evil-end-of-line                    evil-visual-state-map)
    (bind-key "}"     #'forward-paragraph                   evil-motion-state-map)
    (bind-key "{"     #'backward-paragraph                  evil-motion-state-map)
    (bind-key "C-k"   #'kill-line                           evil-insert-state-map)
    (bind-key "C-y"   #'yank                                evil-normal-state-map)
    (bind-key "C-d"   #'evil-scroll-down                    evil-motion-state-map)
    (bind-key "C-d"   #'evil-scroll-down                    evil-normal-state-map)
    (bind-key "C-u"   #'evil-scroll-up                      evil-motion-state-map)
    (bind-key "C-u"   #'evil-scroll-up                      evil-normal-state-map)

    (bind-key "C-c w t"
      (lambda ()
        (interactive)
        "Move current window to new frame."
        (let ((buffer (current-buffer)))
          (unless (one-window-p)
            (delete-window))
          (display-buffer-pop-up-frame buffer nil)))
      evil-normal-state-map)

    (evil-define-key 'normal flyspell-mode-map
      "[s" 'flyspell-goto-next-error
      "]s" 'flyspell-goto-next-error)

    (evil-define-key 'normal help-mode-map
      "TAB" 'forward-button)
    (evil-define-key 'motion help-mode-map
      "l" 'help-go-back
      "r" 'help-go-forward
      "s-TAB" 'backward-button)

    (evil-define-key 'normal ediff-mode-map
      "[c" 'ediff-next-difference
      "]c" 'ediff-previous-difference)))

(use-package evil-surround
  :config
  (progn
    (global-evil-surround-mode 1)))

(use-package evil-matchit
  :config
  (progn
    (global-evil-matchit-mode 1)))

(use-package org-evil)

(use-package magit
  :diminish magit-auto-revert-mode
  :config
  (progn
    (setq
      magit-completing-read-function 'ivy-completing-read
      magit-item-highlight-face 'bold
      ;; magit-repo-dirs-depth 1
      )

    (bind-key "}"   #'evil-forward-paragraph  magit-mode-map)
    (bind-key "]"   #'evil-forward-paragraph  magit-mode-map)
    (bind-key "{"   #'evil-backward-paragraph magit-mode-map)
    (bind-key "["   #'evil-backward-paragraph magit-mode-map)
    (bind-key "C-d" #'evil-scroll-down        magit-mode-map)
    (bind-key "C-u" #'evil-scroll-up          magit-mode-map)
    (bind-key "r"   #'magit-reverse           magit-hunk-section-map)
    (bind-key "v"   #'evil-visual-char        magit-hunk-section-map)))

(use-package transpose-frame)
(use-package wgrep)
(use-package hl-todo)
(use-package rainbow-mode)
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
(use-package color-theme-sanityinc-tomorrow)
(use-package persistent-scratch)
(use-package git-gutter)
;; (use-package oauth2)
;; (use-package artbollocks-mode)

(use-package goto-last-change
  :config
  (progn
    (bind-key "C-x C-\\" #'goto-last-change)))
(use-package avy)
(use-package ivy-hydra)
(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :config
  (progn
    (ivy-mode 1)
    (bind-key "<return>" #'ivy-alt-done ivy-minibuffer-map)
    (bind-key "C-M-h" #'ivy-previous-line-and-call ivy-minibuffer-map)
    (bind-key "C-:" #'ivy-dired ivy-minibuffer-map)
    (bind-key "C-c o" #'ivy-occur ivy-minibuffer-map)
    (bind-key "C-'" #'ivy-avy ivy-minibuffer-map)
    (bind-key "C-x b" #'display-buffer)

    (defun ivy-dired ()
      (interactive)
      (if ivy--directory
        (ivy-quit-and-run
          (dired ivy--directory)
          (when (re-search-forward
                  (regexp-quote
                    (substring ivy--current 0 -1)) nil t)
            (goto-char (match-beginning 0))))
        (user-error
          "Not completing files currently")))

    (defun ivy-view-backtrace ()
      (interactive)
      (switch-to-buffer "*ivy-backtrace*")
      (delete-region (point-min) (point-max))
      (fundamental-mode)
      (insert ivy-old-backtrace)
      (goto-char (point-min))
      (forward-line 1)
      (let (part parts)
        (while (< (point) (point-max))
          (condition-case nil
            (progn
              (setq part (read (current-buffer)))
              (push part parts)
              (delete-region (point-min) (point)))
            (error
              (progn
                (ignore-errors (up-list))
                (delete-region (point-min) (point)))))))
      (goto-char (point-min))
      (dolist (part parts)
        (lispy--insert part)
        (lispy-alt-multiline)
        (insert "\n")))

    (setq ivy-switch-buffer-faces-alist
      '((emacs-lisp-mode . swiper-match-face-1)
         (dired-mode . ivy-subdir)
         (org-mode . org-level-4)))

    (setq
      ;; ivy-use-virtual-buffers t
      ivy-height 10
      ivy-use-selectable-prompt t
      ivy-count-format "(%d/%d) "
      ivy-re-builders-alist '((t   . ivy--regex-ignore-order)))
    )
  )

(use-package swiper
  :config
  (progn
    (bind-key "C-s" #'swiper)))

(use-package counsel
  :config
  (progn
    (setq counsel-find-file-ignore-regexp "\\`\\.")
    (bind-key "C-r" #'counsel-expression-history read-expression-map)
    (bind-key "C-r" #'counsel-minibuffer-history read-expression-map)
    (bind-key "M-x" #'counsel-M-x)
    (bind-key "C-x C-f" #'counsel-find-file)
    (bind-key "<f1> f" #'counsel-describe-function)
    (bind-key "<f1> v" #'counsel-describe-variable)
    (bind-key "<f1> l" #'counsel-find-library)
    (bind-key "<f2> s" #'counsel-info-lookup-symbol)
    (bind-key "<f2> u" #'counsel-unicode-char)
    (bind-key "C-h f" #'counsel-describe-function)
    (bind-key "C-h v" #'counsel-describe-variable)
    (bind-key "C-x r b" #'counsel-bookmark)
    ;; (global-set-key (kbd "C-c g") 'counsel-git)
    ;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
    ;; (global-set-key (kbd "C-c k") 'counsel-ack)

    ;; (setq counsel-grep-base-command "grep -niE %s %s")
    ;; (setq counsel-grep-base-command
    ;;   "rg -i -M 120 --no-heading --line-number --color never %s %s")
    ;; (setq counsel-rg-base-command
    ;;   "rg -i -M 120 --no-heading --line-number --color never %s .")
    ;; (setq counsel-git-grep-cmd-default
    ;;   (concat "git --no-pager grep --full-name -n --no-color -i -e '%s' -- './*' "
    ;;     (mapconcat (lambda (x) (format "':!*.%s'" x))
    ;;       '("htm" "so" "a" "TTC" "NDS" "png" "md5") " ")))
    ;; (setq counsel-git-grep-projects-alist
    ;;   (list
    ;;     (cons "/home/oleh/Dropbox/source/site-lisp/"
    ;;       (concat "/home/oleh/Dropbox/source/site-lisp/etc/git-multi-grep '%s' "
    ;;         "/home/oleh/Dropbox/source/site-lisp 'git/*'"))
    ;;     (cons "/home/oleh/git/ivy-dependencies/"
    ;;       (concat "/home/oleh/Dropbox/source/site-lisp/etc/git-multi-grep '%s' "
    ;;         "/home/oleh/git/ivy-dependencies '*'"))))
    ;; (setq counsel-git-cmd "rg --files")


    (ivy-set-display-transformer 'counsel-describe-function nil)
    )
  )

;; (use-package projectile
;;   :init
;;   (setq
;;     projectile-completion-system 'ivy
;;     projectile-indexing-method 'alien
;;     projectile-enable-caching t
;;     projectile-verbose nil
;;     projectile-do-log nil
;;     )
;;   )

;; (use-package ido)
;; (use-package ido-completing-read+)
;; (use-package ido-vertical-mode)

;; TODO make it language specific switch
(use-package artbollocks-mode
  :defer t
  :config
  (progn
    (setq artbollocks-weasel-words-regex
          (concat "\\b" (regexp-opt
                         '("one of the"
                           "should"
                           "just"
                           "sort of"
                           "a lot"
                           "probably"
                           "maybe"
                           "perhaps"
                           "I think"
                           "really"
                           "pretty"
                           "nice"
                           "action"
                           "utilize"
                           "leverage") t) "\\b"))
    (setq artbollocks-jargon nil)
    (add-hook 'text-mode-hook 'artbollocks-mode)))

(eval-after-load 'ediff
  '(progn
     (setq ediff-window-setup-function 'ediff-setup-windows-plain
       ediff-forward-word-function 'forward-char
       ediff-use-toolbar-p nil)
     (add-hook 'ediff-before-setup-hook 'new-frame)
     (add-hook 'ediff-quit-hook 'delete-frame)))

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

(bind-key "C-c p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

(bind-key "C-c j" 'org-clock-goto) ;; jump to current task from anywhere

(use-package smartscan
  :defer t
  :config
  (global-smartscan-mode t))

(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'my/smarter-move-beginning-of-line)

(require 'epa-file)
(require 'framemove)
(require 'sunrise-commander)
(require 'multiple-cursors)
(require 'neotree)
(require 'grep)
(require 'wgrep)
(require 'org-agenda)
(require 'calfw)
(require 'calfw-org)
(require 'recentf)
(require 're-builder)
(setq reb-re-syntax 'string)

(eval-after-load 'recentf
  '(progn
    (recentf-mode 1)))

(epa-file-enable)
(setq epa-file-encrypt-to '("adamfaryna@gmail.com"))
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

(setq default-directory "~/.emacs.d")

(setq display-buffer-reuse-frames t)
(setq display-buffer-fallback-action
  '(( display-buffer-reuse-window
      display-buffer--maybe-same-window
      display-buffer--maybe-pop-up-frame-or-window
      display-buffer-in-previous-window
      display-buffer-use-some-window
      display-buffer-pop-up-frame)))

;; (setq display-buffer-alist
;;   '(("*Help*" . ((display-buffer-same-window)))))

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
  dired-use-ls-diredto nil
  dired-recursive-copies 'always
  dired-recursive-deletes 'always)
(setq
  gc-cons-threshold 3500000
  bookmark-save-flag t
  show-paren-delay 0)

(bind-key "C-c -" #'diredp-up-directory-reuse-dir-buffer dired-mode-map)
(bind-key "M-%" #'query-replace-regexp)

(setq recentf-max-saved-items 200
  recentf-max-menu-items 15)

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
(setq savehist-additional-variables
      '(kill-ring
        search-ring
         regexp-search-ring))

(diminish 'editorconfig-mode)
(diminish 'auto-revert-mode)
(diminish 'auto-highlight-symbol-mode)
(diminish 'git-gutter-mode)

(setq sentence-end-double-space nil)

(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

(setq diredp-hide-details-initially-flag nil)
(require 'dired+)
(diredp-toggle-find-file-reuse-dir 1)

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
(use-package diminish
  :config
  (progn
    (diminish 'abbrev-mode " A")))

(eval-after-load 'sunrise-commander
  '(progn
     (defun mc ()
       "Open sunrise commander in default directory."
       (interactive)
       (make-frame-command)
       (sunrise default-directory default-directory)
       )

     ;; delete redundant window in MC mode
     (add-hook 'sr-start-hook (lambda () (delete-window (car (last (window-list))))))))

(eval-after-load 'org
  '(progn
     (bind-key "M-}"         #'forward-paragraph           org-mode-map)
     (bind-key "M-{"         #'backward-paragraph          org-mode-map)
     (bind-key "C-c C-r"     #'air-revert-buffer-noconfirm org-mode-map)
     (bind-key "C-c l"       #'org-store-link              org-mode-map)
     (bind-key "C-."         #'imenu-anywhere              org-mode-map)
     (bind-key "C-c C-x C-s" #'org-archive-subtree-default org-mode-map)
     (bind-key "C-c C-c"     #'counsel-org-tag             org-mode-map)
     (bind-key "C-x :"
       (lambda ()
         (interactive)
         "Insert tags in a capture window without losing the point"
         (save-excursion
           (org-back-to-heading)
           (org-set-tags))))
     (unbind-key "C-c $"       org-mode-map) ; removed archive subtree shortcut
     (unbind-key "C-c C-x C-a" org-mode-map) ; remove archive subtree default shortcut
     (unbind-key "C-c C-x C-s" org-mode-map) ; remove archive subtree shortcut
     (unbind-key "C-c C-x A"   org-mode-map) ; remove archive to archive siblings shortcut

     (advice-add 'org-forward-paragraph :around (lambda (orig &rest args) (forward-paragraph)))
     (advice-add 'org-backward-paragraph :around (lambda (orig &rest args) (backward-paragraph)))
     (add-hook 'org-mode-hook (lambda () (hl-line-mode)))))

(eval-after-load 'org-agenda
  '(progn
     (bind-key "C-c C-c" #'counsel-org-tag-agenda org-agenda-mode-map)
     (bind-key "C-d" #'evil-scroll-down org-agenda-mode-map)
     (bind-key "C-u" #'evil-scroll-up org-agenda-mode-map)))

; Gnus
(setq
  gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
  gnus-treat-hide-citation t
  gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject ; is it needed?
  ;; gnus-use-adaptive-scoring t
  gnus-inhibit-slow-scoring "^nntp[+:]"
  gnus-agent nil
  gnus-asynchronous t
  gnus-message-archive-group nil
  gnus-use-cache t ; cache everything
  gnus-read-active-file 'some
  gnus-thread-sort-functions
  '(gnus-thread-sort-by-score
     gnus-thread-sort-by-date
     (not gnus-thread-sort-by-number))
  gnus-thread-hide-subtree t ; hide specific threads?
  gnus-thread-ignore-subject t
  gnus-thread-indent-level 2
  gnus-sum-thread-tree-indent " "
  gnus-auto-select-first nil
  gnus-auto-select-next nil
  gnus-user-date-format-alist '((t . "%d.%m.%Y %H:%M"))
  gnus-activate-level 3
  message-default-charset `utf-8
  ;; gnus-default-adaptive-score-alist
  ;; '((gnus-unread-mark)
  ;;    (gnus-ticked-mark (subject 10))
  ;;    (gnus-killed-mark (subject -5))
  ;;    (gnus-catchup-mark (subject -1)))
                                        ; gnus-select-method '(nnnil "")

  gnus-summary-line-format "%D%U%R%z%I%(%[%4L: %-23,23f%]%) %s\n"

  ;; gnus-parameters '(
  message-directory "~/.Mail/"
  message-send-mail-function 'smtpmail-send-it
  send-mail-function 'smtpmail-send-it
  smtpmail-smtp-server "smtp.gmail.com"
  nnir-imap-default-search-key "gmail"
  nnheader-file-name-translation-alist '((?[ . ?_) (?] . ?_)))

(setq
  smtpmail-smtp-service 587
  mml2015-encrypt-to-self t
  mm-verify-option t
  mm-decrypt-option t)

(if (executable-find "w3m")
  (use-package w3m
    :config (progn
              (setq
                w3m-use-cookies t
                mm-text-html-renderer 'w3m)))
  (message (concat "Executable 'w3m' not found!")))

(add-hook 'gnus-summary-mode-hook (lambda ()
                                    (local-set-key "y" 'gmail-archive)
                                    (local-set-key "$" 'gmail-report-spam)))
(add-hook 'message-sent-hook 'gnus-score-followup-thread)
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; (eval-after-load 'gnus-topic
;;   '(progn
;;      (setq gnus-message-archive-group '((format-time-string "sent.%Y")))
;;      (setq gnus-topic-topology '((("gmail" visible nil nil))
;;                                   ("Gnus" visible)
;;                                   (("misc" visible))))))

     ;; Please not the key of topic is specified in my sample setup
     ;; (setq gnus-topic-alist '(("gmail" ; the key of topic
     ;;                           "INBOX"
     ;;                           "[Gmail]/Sent Mail"
     ;;                           "Drafts")
     ;;                          ("misc" ; the key of topic
     ;;                           "nnfolder+archive:sent.2018-12"
     ;;                           "nnfolder+archive:sent.2018"
     ;;                           "nndraft:drafts")
     ;;                          ("Gnus")))))


(defun gmail-archive ()
  "Archive the current or marked mails.
This moves them into the All Mail folder."
  (interactive)
  (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/All Mail"))

(defun gmail-report-spam ()
  "Report the current or marked mails as spam.
This moves them into the Spam folder."
  (interactive)
  (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/Spam"))

(setq mac-command-modifier 'super)

;; Mappings / Shortcuts
(global-set-key (kbd "C-x C-b") 'ibuffer) ; list buffers for editing
(global-set-key (kbd "C-c p") #'git-gutter:previous-hunk)
(global-set-key (kbd "C-c n") #'git-gutter:next-hunk)
;; (global-set-key (kbd "C-c n") #'neotree-toggle)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "C-c C-r") 'air-revert-buffer-noconfirm)
(global-set-key (kbd "C-x 4 t") 'flop-frame)
(global-set-key (kbd "s-w") 'kill-ring-save)
(global-set-key (kbd "C-x g s") 'magit-status)

;; Org mode
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-x a") #'org-agenda)

(global-unset-key (kbd "C-x c"))
(global-unset-key (kbd "C-x <C-left>"))
(global-unset-key (kbd "C-x <C-right>"))
(global-unset-key (kbd "<kp-end>"))
(global-unset-key (kbd "<kp-home>"))
(global-unset-key (kbd "<end>"))
(global-unset-key (kbd "<home>"))
(global-unset-key (kbd "C-x C-'"))

(global-set-key (kbd "<home>") 'left-word)
(global-set-key (kbd "<end>") 'right-word)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-j") 'join-line)

(bind-key "C-c l" 'org-store-link)
(bind-key "C-c L" 'org-insert-link-global)
(bind-key "C-c O" 'org-open-at-point-global)

(setq calendar-week-start-day 1)
(setq calendar-date-style "european")

(add-hook 'calendar-load-hook
  (lambda ()
    (calendar-set-date-style 'european)))

;; VCS / git
(setq ediff-split-window-function (if (> (frame-width) 150)
				      'split-window-horizontally
				    'split-window-vertically))

;; Bring back window configuration after ediff quits
(defvar my-ediff-bwin-config nil "Window configuration before ediff.")
(defcustom my-ediff-bwin-reg ?b
  "*Register to be set up to hold `my-ediff-bwin-config'
    configuration.")

(defvar my-ediff-awin-config nil "Window configuration after ediff.")
(defcustom my-ediff-awin-reg ?e
  "*Register to be used to hold `my-ediff-awin-config' window
    configuration.")

(defun my-ediff-bsh ()
  "Function to be called before any buffers or window setup for
    ediff."
  (setq my-ediff-bwin-config (current-window-configuration))
  (when (characterp my-ediff-bwin-reg)
    (set-register my-ediff-bwin-reg
		  (list my-ediff-bwin-config (point-marker)))))

(defun my-ediff-ash ()
  "Function to be called after buffers and window setup for ediff."
  (setq my-ediff-awin-config (current-window-configuration))
  (when (characterp my-ediff-awin-reg)
    (set-register my-ediff-awin-reg
		  (list my-ediff-awin-config (point-marker)))))

(defun my-ediff-qh ()
  "Function to be called when ediff quits."
  (when my-ediff-bwin-config
    (set-window-configuration my-ediff-bwin-config)))

(add-hook 'ediff-load-hook
	  (lambda ()
	    (add-hook 'ediff-before-setup-hook
		      (lambda ()
			(setq ediff-saved-window-configuration (current-window-configuration))))
	    (let ((restore-window-configuration
		   (lambda ()
		     (set-window-configuration ediff-saved-window-configuration))))
	      (add-hook 'ediff-quit-hook restore-window-configuration 'append)
	      (add-hook 'ediff-suspend-hook restore-window-configuration 'append))))
(add-hook 'ediff-startup-hook
	  (lambda ()
	    (select-frame-by-name "Ediff")
	    (set-frame-size(selected-frame) 40 10)))
(add-hook 'ediff-before-setup-hook 'my-ediff-bsh)
(add-hook 'ediff-after-setup-windows-hook 'my-ediff-ash 'append)
(add-hook 'ediff-quit-hook 'my-ediff-qh)
;; (when (executable-find "curl")
;;   (setq helm-google-suggest-use-curl-p t))

;; (setq helm-split-window-inside-p t
;;   helm-move-to-line-cycle-in-source t
;;   helm-ff-search-library-in-sexp t
;;   helm-scroll-amount 8
;;   helm-ff-file-name-history-use-recentf t
;;   helm-echo-input-in-header-line t)

;; Multiline cursor
(global-set-key (kbd "C->") #'mc/mark-next-like-this)
(global-set-key (kbd "C-<") #'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") #'mc/mark-all-like-this)

(eval-after-load 'dired-mode
  (lambda ()
    (define-key (kbd "t") (dired-toggle-marks)) ; toggle marks
    ))

(add-hook 'dired-mode
  (lambda ()
    ;; (define-key (kbd "<left>") (diredp-up-directory-reuse-dir-buffer))
    ))

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
    (setq wgrep-enable-key "r")
    )
  )

;; General
(setq vc-follow-symlinks t)

;; Modes
(global-auto-revert-mode 1)
(blink-cursor-mode 0)
(global-linum-mode 1)
(editorconfig-mode 1)

(show-paren-mode 1)
(delete-selection-mode 1)
(scroll-bar-mode -1)
(if window-system (tool-bar-mode -1))

;; Helm
;; (setq helm-ff-auto-update-initial-value t)

;; programming
(setq devel-buffers '("js" "jsx" "vim" "json" "java" "php" "css" "scss" "html" "md" "xml" "rb" "el"))

(add-hook 'org-agenda-mode-hook #'hl-line-mode)

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

;; mode hooks
(setq flyspell-mode-hooks '(text-mode-hook org-mode-hook))

(if (executable-find "aspell")
  (dolist (i flyspell-mode-hooks)
    (add-hook i #'flyspell-prog-mode)))

;; navigation
(windmove-default-keybindings)
(setq windmove-wrap-around t)
(setq framemove-hook-into-windmove t)

(setq local-config-file (expand-file-name "local-config.el" user-emacs-directory))

(desktop-save-mode 1)
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(setq desktop-buffers-not-to-save
  (concat "\\("
    "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
    "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb\\|ido.*"
    "\\)$"))

;; org mode / journal
(setq
  org-clock-into-drawer t
  org-log-into-drawer t)
(setq org-clock-persist t) ; or 'history?
(setq org-clock-idle-time 2) ; TODO requires testing
(setq org-default-notes-file (expand-file-name "notes" user-emacs-directory))
(setq org-lowest-priority 68)
(setq org-highest-priority 65)
(setq org-default-priority 65)
(setq org-log-done 'time)
(setq org-enforce-todo-dependencies t)
(setq org-track-ordered-property-with-tag t)
(setq org-use-property-inheritance t)
(setq org-priority-start-cycle-with-default nil)
(setq org-columns-default-format "%25ITEM(Task) %TODO %3PRIORITY %7Effort %8CLOCKSUM %TAGS")
;; (setq org-completion-use-ido t)
(setq org-export-exclude-category (list "google" "private"))
(setq org-icalendar-use-scheduled '(todo-start event-if-todo))
(setq org-icalendar-use-deadline '(event-if-todo))
(setq org-icalendar-honor-noexport-tag t) ; this is not supported in my version
;; (setq org-adapt-indentation nil)
(setq org-list-description-max-indent 5)
(setq org-closed-keep-when-no-todo t)
(setq org-log-done-with-time nil)
;; (setq org-tags-column -100)
(setq org-reverse-note-order t)
(setq org-global-properties '(("Effort_ALL" . "0:05 0:15 0:30 1:00 2:00 4:00")))
(setq org-clock-report-include-clocking-task t)
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-pretty-entities t)
(setq org-clock-in-resume t)
(setq org-clock-persist-query-resume nil)
(setq org-clock-in-switch-to-state "IN-PROCESS")
(setq org-clock-out-when-done (list "TODO" "BLOCKED" "WAITING"))
(setq org-agenda-scheduled-leaders '("" ""))
;; (setq org-agenda-window-setup 'current-window)
(setq org-return-follows-link nil)
(setq org-agenda-directory (expand-file-name "agenda" user-emacs-directory))
(setq org-directory (expand-file-name "orgs" user-emacs-directory))
(defvar my/org-contacts-file (expand-file-name "contacts.org.gpg" org-directory))
(setq org-journal-dir (expand-file-name "journal" user-emacs-directory))
(setq org-default-notes-file (expand-file-name "notes.org" org-directory))
(setq org-caldav-save-directory (expand-file-name "tmp" user-emacs-directory))
(setq org-icalendar-combined-agenda-file (expand-file-name "org.ics" org-caldav-save-directory))
(setq org-caldav-inbox (expand-file-name "google.org.gpg" org-agenda-directory))
(setq org-caldav-url 'google)
(setq org-icalendar-timezone "Europe/London") ; or nil
(setq org-icalendar-alarm-time 10)
;; (setq org-caldav-skip-conditions '(nottodo))
(setq org-caldav-files (directory-files org-agenda-directory t "^[^.][^#]*\\.org"))
(setq org-caldav-delete-calendar-entries 'always)
(setq org-caldav-delete-org-entries 'never)
(setq plstore-cache-passphrase-for-symmetric-encryption t)
(setq org-agenda-file-regexp ".*org\(.gpg\)?$")
(setq org-agenda-files
  (delq nil
    (mapcar (lambda (x) (and x (file-exists-p x) x))
      (list
        (expand-file-name "tasks.org" org-agenda-directory)
        (expand-file-name "shared.org" org-agenda-directory)
        (expand-file-name "tasks.org.gpg" org-agenda-directory)
        ))))

(org-remove-file org-caldav-inbox)
(setq org-icalendar-with-timestamps 'active)
(setq org-icalendar-include-todo t)
(setq org-icalendar-include-sexps t)
(setq org-icalendar-store-UID t)
(setq org-habit-show-habits-only-for-today nil)
(setq org-refile-use-outline-path t)
(setq org-refile-targets '((nil . (:maxlevel . 1))
                            (org-agenda-files . (:maxlevel . 1))))
(setq org-blank-before-new-entry nil)
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
(setq org-odt-preferred-output-format "doc")

(if (executable-find "unoconv")
  (setq org-odt-convert-processes '(("unoconv" "unoconv -f %f -o %d %i")))
  (setq org-odt-convert-processes '(("unoconv" "unoconv -f %f -o %d.xls %i")))
  (message "No executable \"unoconv found\".")
  )

(advice-add 'org-capture :before (lambda () (interactive) (message (concat "ala: org-agenda-directory: " org-agenda-directory " org-directory: " org-directory))))

(setq org-capture-templates
  `(("t" "Todo" entry (file ,(expand-file-name "tasks.org.gpg" org-agenda-directory))
      "* TODO %?
  :PROPERTIES:
  :CREATED: [%<%Y-%m-%d>]
  :END:" :prepend nil) ; wish :prepend t
     ("p" "Blog post" entry (file ,(expand-file-name "blog.org.gpg" org-directory))
       "* %?
  :PROPERTIES:
  :CREATED: [%<%Y-%m-%d>]
  :END:" :prepend nil) ; wish :prepend t
     ("h" "Habit" entry (file ,(expand-file-name "tasks.org.gpg" org-agenda-directory))
       "* TODO %?
  SCHEDULED: <%<%Y-%m-%d %a .+2d/4d>>
  :PROPERTIES:
  :CREATED: [%<%Y-%m-%d>]
  :STYLE: habit
  :END:" :prepend nil) ; wish :prepend t
     ("j" "Journal" entry (file ,(expand-file-name "journal.org.gpg" org-directory))
       "* [%<%Y-%m-%d>]\n%?"  :prepend t :jump-to-captured t)
     ("n" "Add note to currently clocked entry" plain (clock)
       "- Note taken on %U \\\\ \n  %?")
     ("c" "Contact" entry (file my/org-contacts-file) ;,(expand-file-name "contacts.org.gpg" org-directory))
       "* %(org-contacts-template-name)
  :PROPERTIES:
  :TITLE:
  :ALIAS:
  :COMPANY:
  :ROLE:
  :EMAIL: %(org-contacts-template-email)
  :MOBILE:
  :WORK_PHONE:
  :ADDRESS:
  :URL:
  :BIRTHDAY:
  :ITOLD_THEM_EMAIL:
  :ITOLD_THEM_PHONE:
  :NOTES:
  :CREATED: [%<%Y-%m-%d>]
  :END:")))

(set-register ?g (cons 'file (expand-file-name "goals.org.gpg" org-directory)))
(set-register ?k (cons 'file (expand-file-name "knowledge.org" org-directory)))
(set-register ?j (cons 'file (expand-file-name "journal.org.gpg" org-directory)))
(set-register ?p (cons 'file (expand-file-name "projects.org.gpg" org-directory)))
(set-register ?t (cons 'file (expand-file-name "tasks.org.gpg" org-agenda-directory)))
(set-register ?s (cons 'file (expand-file-name "shared.org" org-agenda-directory)))
(set-register ?i (cons 'file (expand-file-name "init.el" user-emacs-directory)))
(set-register ?l (cons 'file local-config-file))

(setq org-todo-keywords
  '((sequence "TODO(t)" "IN-PROCESS(p)" "BLOCKED(b@/!)" "WAITING(w@/!)" "SOMEDAY(s@)" "|" "DONE(d!)" "CANCELED(c@)" "UNDOABLE(u@)")))

(setq org-agenda-custom-commands
  '(("co" "TODOs weekly sorted by state, priority, deadline, scheduled, alpha and effort"
      (
        (agenda "*"))
      ((org-agenda-overriding-header "TODOs weekly sorted by state, priority, deadline, scheduled, alpha and effort")
        (org-agenda-sorting-strategy '(todo-state-down priority-down deadline-down scheduled-down alpha-down effort-up))))
     ("cn" "TODOs not sheduled"
       (
         (tags "-SCHEDULED={.+}/!+TODO|+BLOCKED|+IN-PROCESS|+WAITING"))
       ((org-agenda-overriding-header "TODOs not scheduled")
         (org-agenda-sorting-strategy '(deadline-down priority-down alpha-down effort-up))))
     ("cb" "TODOs blocked"
       (
         (tags "BLOCKED"))
       ((org-agenda-overriding-header "TODOs blocked")
         (org-agenda-sorting-strategy '(priority-down deadline-down alpha-down effort-up))))
     ("cc" "TODOs canceled"
       (
         (todo "CANCELED"))
       ((org-agenda-overriding-header "TODOs canceled")
         (org-agenda-sorting-strategy '(priority-down alpha-down effort-up))))
     ("cj" "Journal"
       (
         (search ""))
       ((org-agenda-files (list org-journal-dir))
         (org-agenda-overriding-header "Journal")
         (org-agenda-sorting-strategy '(timestamp-down))))
     ("d" "Coprehensive agenda"
      ((tags "PRIORITY=\"A\"+TODO=\"TODO\"|TODO=\"IN-PROCESS\"|TODO=\"BLOCKED\"|TODO=\"WAITING\""
         ((org-agenda-skip-function
            '(or
               (org-agenda-skip-entry-if 'todo 'done)
               (org-agenda-skip-entry-if 'todo '("SOMEDAY"))
               (my/org-agenda-skip-if-scheduled-later))
            )
           (org-agenda-overriding-header "High-priority unfinished tasks:")
           (org-agenda-sorting-strategy '(time-up effort-down category-keep alpha-up))
           ))
        (agenda "")
        (alltodo ""
          ((org-agenda-skip-function
             '(or (my/org-skip-subtree-if-priority ?A)
                (my/org-skip-subtree-if-habit)
                (org-agenda-skip-if nil '(scheduled deadline))
                (org-agenda-skip-entry-if 'todo '("IN-PROCESS" "BLOCKED" "WAITING"))))
            (org-agenda-sorting-strategy '(priority-down effort-down category-keep alpha-up))
            )
          )
        )
       )
     )
  )


(defun my/org-calendar-export-limit ()
  "Limit the export to items that have a date, time and a range. Also exclude certain categories."
  (setq org-tst-regexp "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ... [0-9]\\{2\\}:[0-9]\\{2\\}[^\r\n>]*?\
\)>")
  (setq org-tstr-regexp (concat org-tst-regexp "--?-?" org-tst-regexp))
  (save-excursion
    ; get categories
    (setq mycategory (org-get-category))
    ; get start and end of tree
    (org-back-to-heading t)
    (setq mystart    (point))
    (org-end-of-subtree)
    (setq myend      (point))
    (goto-char mystart)
    ; search for timerange
    (setq myresult (re-search-forward org-tstr-regexp myend t))
    ; search for categories to exclude
    (setq mycatp (member mycategory org-export-exclude-category))
    ; return t if ok, nil when not ok
    (if (and myresult (not mycatp)) t nil)))

;; (defun air-org-cal-export ()
;;   (let ((org-icalendar-verify-function 'my/org-calendar-export-limit))
;;     (org-export-icalendar-combine-agenda-files)))

(defun my/org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.
PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(defun my/org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(defun my/org-agenda-skip-if-scheduled-later ()
  "If this function returns nil, the current match should not be skipped.
Otherwise, the function must return a position from where the search
should be continued."
  (ignore-errors
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
           (scheduled-seconds
             (time-to-seconds
               (org-time-string-to-time
                 (org-entry-get nil "SCHEDULED"))))
           (now (time-to-seconds (current-time))))
      (and scheduled-seconds
        (>= scheduled-seconds now)
        subtree-end))))

(org-clock-persistence-insinuate)

(setq org-tag-alist '(("@health" . ?h) ; my energy level, my looks
                       ("@fun" . ?u) ; relax, enjoy life
                       ("@career" . ?c) ; my professional reputation, my credability, my professional skills, professional relationships
                       ("@family&friends" . ?f) ; my social network, my professional network
                       ("@love" . ?l) ; my happiness, my ultimate goal, my real legacy
                       ("@wealth" . ?w) ; my legacy
                       ))

(add-to-list 'org-modules 'org-habit t)
(add-to-list 'org-modules 'org-collector t)

;; org mode conflicts resolution: windmove
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; programming
(add-to-list 'auto-mode-alist '("\\.jsx$" . js-mode))

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

(if (fboundp 'global-git-gutter-mode)
  (global-git-gutter-mode +1))

(if (fboundp 'persistent-scratch-setup-default)
  (persistent-scratch-setup-default))

(defun reload-config ()
	"Reload config."
	(interactive)
	(load-file (expand-file-name "init.el" user-emacs-directory))
  (message "Config reloaded."))

;; TODO it could be rather based on ring implementation (hard to add new langs)
(defun dict-toggle ()
  "Toggle spell dictionary."
  (interactive)
  (if
    (string= ispell-current-dictionary "en")
    (ispell-change-dictionary "pl")
    (ispell-change-dictionary "en")
    )
  (message (concat "Current spell language is '" ispell-current-dictionary "'.")))

(set-cursor-color "#ffffff")
(setq
  custom-safe-themes t
  default-font "-*-Menlo-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")
(set-face-attribute 'default nil :font default-font)
(color-theme-sanityinc-tomorrow-night)

(when (file-exists-p local-config-file)
  (message "local config exists")
  (load local-config-file))


;; temporary
;; (set-face-foreground 'dired-directory "yellow" )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
