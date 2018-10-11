(require 'autorevert)
(require 're-builder)
(require 'grep)
(require 'inc-dec-at-point)
(require 'epa-file)
(require 'epg-config)
(require 'hippie-exp)
(require 'calendar)
(require 'reftex)

(use-package hydra)
;; (use-package monitor)
(use-package popup)
(use-package miniedit)
(use-package calfw)
(use-package imenu-anywhere)
(use-package with-editor)  ; dependency for other package
(use-package neotree)
(use-package multiple-cursors)

(setq hippie-expand-try-functions-list '())
(add-to-list 'hippie-expand-try-functions-list 'try-expand-dabbrev t)
(add-to-list 'hippie-expand-try-functions-list 'try-expand-dabbrev-all-buffers t)
(add-to-list 'hippie-expand-try-functions-list 'try-expand-dabbrev-from-kill t)
(add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t)
(add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
(add-to-list 'hippie-expand-try-functions-list 'try-expand-list t)
(add-to-list 'hippie-expand-try-functions-list 'try-expand-line t)

(defadvice hippie-expand (around hippie-expand-case-fold)
  "Try to do case-sensitive matching (not effective with all functions)."
  (let ((case-fold-search nil))
    ad-do-it))

(use-package diminish
  :config
  (progn
    ;; (diminish 'editorconfig-mode)
    (diminish 'auto-revert-mode)
    (diminish 'company-mode)
    (diminish 'eldoc-mode)
    (diminish 'visual-line-mode)
    (diminish 'editorconfig-mode)
    (diminish 'js-mode "JS")
    (diminish 'abbrev-mode)))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (progn
    (setq yas-new-snippet-default
"# name: $2
# key: $1
# --
$0`(yas-escape-text yas-selected-text)`")
    (yas-global-mode 1)))

(defhydra hydra-snippet ()
  "Snippet"
  ("s" #'yas-insert-snippet "insert")
  ("n" #'yas-new-snippet "new")
  ("e" #'yas-visit-snippet-file "edit"))

(use-package company
  :diminish company-mode
  :config
  (progn
    (setq
      company-show-numbers t
      company-tooltip-align-annotations t
      company-minimum-prefix-length 0
      company-begin-commands '(c-scope-operator c-electric-colon c-electric-lt-gt c-electric-slash))))

;; (require 'wgrep)
;; (setq reb-re-syntax 'string)

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

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (progn
    (editorconfig-mode 1)))

(use-package centered-cursor-mode)

(use-package auto-highlight-symbol
  :diminish auto-highlight-symbol-mode
  :config
  (progn
    (setq
      ahs-case-fold-search nil
      ahs-idle-interval 0)))

(require 'speedbar)
(eval-after-load 'speedbar
  '(progn
     (setq speedbar-show-unknown-files t)))

(use-package sr-speedbar)

;; (use-package transpose-frame)
;; (use-package wgrep
;; 	:config
;; 	(progn
;; 		(if (commandp 'wgrep)
;; 				(progn
;; 					(setq wgrep-enable-key "r")))))

(epa-file-enable)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

(setq visual-line-fringe-indicators '(left-curly-arrow nil))

(setq
  fill-column 80
  hscroll-margin  1
  hscroll-step 1
  scroll-conservatively 1001 ;; should be 0?
  scroll-preserve-screen-position t ;; ?
  require-final-newline t
  word-wrap t
  compare-ignore-case t
  compare-ignore-whitespace t
  sentence-end-double-space nil
  tab-width 2
  ;; undo-limit 1000
  visible-bell nil
  ring-bell-function 'ignore
  show-paren-delay 0
  ns-right-alternate-modifier nil
  save-some-buffers-default-predicate t
  help-window-select t)

(setq ack-path (executable-find "ack"))

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'ar #'align-regexp)

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

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; (eval-after-load 'grep
;; 	'(progn
;; 		 (add-to-list 'grep-find-ignored-directories "auto-save-list")
;; 		 (add-to-list 'grep-find-ignored-directories "autosaves")
;; 		 (add-to-list 'grep-find-ignored-directories "backups")
;; 		 (add-to-list 'grep-find-ignored-directories "elpa")
;; 		 (add-to-list 'grep-find-ignored-directories "lisp")
;; 		 (add-to-list 'grep-find-ignored-directories "tools"))

(setq
  bookmark-save-flag t)

;; (use-package company-emoji
;;   :config
;;   (progn
;;     (add-to-list 'company-backends 'company-emoji)))

;; (defun my/set-emoji-font (frame)
;;   "Adjust the font settings of FRAME so Emacs can display emoji properly."
;;   (if (eq system-type 'darwin)
;;       ;; For NS/Cocoa
;;       (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
;;     ;; For Linux
;;     (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))

;; For when Emacs is started in GUI mode:
;; (my/set-emoji-font nil)

;; Hook for when a frame is created with emacsclient
;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
;; (add-hook 'after-make-frame-functions 'my/set-emoji-font)

(eval-after-load 'inc-dec-at-point
  '(progn
     (bind-key "C-c +" #'increment-integer-at-point)
     (bind-key "C-c -" #'decrement-integer-at-point)))

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

(eval-after-load 'hippie-exp
  '(progn
     (setq hippie-expand-try-functions-list
       '(
          ;;yas-hippie-try-expand ; requires yasnippet plugin
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
     ))

;; (defun my/buffer-messages-tail ()
  ;; (let ((messages (get-buffer "*Messages*")))
    ;; (unless (eq (current-buffer) messages)
      ;; (with-current-buffer messages
        ;; (goto-char (point-max))))))

;; http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard
;; (defun my/copy-file-name-to-clipboard ()
;;   "Copy the current buffer file name to the clipboard."
;;   (interactive)
;;   (let ((filename (if (equal major-mode 'dired-mode)
;;                       default-directory
;;                     (buffer-file-name))))
;;     (when filename
;;       (kill-new filename)
;;       (message "Copied buffer file name '%s' to the clipboard." filename))))

;; (add-hook 'post-command-hook 'my/buffer-messages-tail)

(defun my/copy-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename
          (if (equal major-mode 'dired-mode)
            default-directory
            (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun air-revert-buffer-noconfirm ()
  (interactive)
  (revert-buffer :ignore-auto :noconfirm)
  (message (concat "Buffer '" (file-name-nondirectory buffer-file-name) "' reloaded.")))

(defun my/move-current-window-to-new-frame ()
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

(defun my/advice-around-skip (orig-fun &rest args) "")

(bind-key "C-x C-SPC"       #'rectangle-mark-mode)
(bind-key "C-x C-b"         #'pop-to-buffer)
(bind-key "M-/"             #'hippie-expand)
(bind-key "C->"             #'mc/mark-next-like-this)
(bind-key "C-<"             #'mc/mark-previous-like-this)
(bind-key "C-c C-<"         #'mc/mark-all-like-this)
(bind-key "s-u"             #'air-revert-buffer-noconfirm)
(bind-key "M-%"             #'query-replace-regexp)
(bind-key "C-c j"           #'org-clock-goto) ;; jump to current task from anywhere
(bind-key "C-c C-o"         #'org-open-at-point-global)
(bind-key "C-c c"           #'org-capture)
(bind-key "C-c a"           #'org-agenda)
(bind-key "C-x a"           #'org-agenda)
(bind-key "C-c l"           #'org-store-link)
(bind-key "C-c L"           #'org-insert-link-global)
(bind-key "C-c O"           #'org-open-at-point-global)
(bind-key "C-c p"           #'git-gutter:previous-hunk)
(bind-key "C-c n"           #'git-gutter:next-hunk)

(column-number-mode 1)
(global-auto-revert-mode 1)
(show-paren-mode 1)
(global-visual-line-mode 1)
(global-hl-line-mode 1)
(delete-selection-mode 1)

(advice-add 'visual-line-mode :around
  (lambda (orig-fun &rest args)
    (unless (memq major-mode (list 'org-agenda-mode))
      (apply orig-fun args))))

(smartparens-mode -1)
(blink-cursor-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(when window-system (tool-bar-mode -1))

(provide 'my-edit)
