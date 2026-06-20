  (defconst my/local-config-dir (expand-file-name "emacs-local-config" user-emacs-directory))
  (defconst my/local-config-file (expand-file-name "local-config.el" my/local-config-dir))

  ;; overwrite these in local-config.el to avoid pushing them to git
  (defvar my/tmp-dir (expand-file-name "tmp" user-emacs-directory))
  (defvar my/org-roam-dir nil)
  (defvar my/evil-enable nil)
  (defvar my/html-enable nil) ; advanced
  (defvar my/js-enable nil) ; advanced
  (defvar my/python-enable nil)
  (defvar my/go-enable nil)
  (defvar my/php-enable nil) ; requires html
  (defvar my/kotlin-enabled nil)

  (when my/org-roam-dir
    (unless (file-directory-p my/org-roam-dir)
      (mkdir my/org-roam-dir)))

  (if (file-exists-p my/local-config-file)
      (progn
        (message "Loading %s..." my/local-config-file)
        (load my/local-config-file))
      (error "File '%s' not exists!" my/local-config-file))

  (setq gc-cons-threshold (* 50 1000 1000)) ;; reduce startup GC pauses

  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq confirm-kill-emacs #'yes-or-no-p)

  ;; Turn off mouse interface early in startup to avoid momentary display
  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

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

  (when (display-graphic-p)
    (set-fringe-mode 10))

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
  (defalias 'qcalc #'quick-calc)

  (setq gnutls-verify-error t)

  (setq enable-local-eval t)

  (setq-default
   compare-ignore-case t
   compare-ignore-whitespace t
   cursor-in-non-selected-windows t
   display-time-default-load-average nil
   view-read-only t
   require-final-newline nil
   indent-tabs-mode nil
   mode-require-final-newline nil)

  ;; start in fullscreen
  (when (display-graphic-p)
    (set-frame-parameter nil 'fullscreen 'fullboth))

  ;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  (setq default-frame-alist '((fullscreen . maximized)
      			    ;; You can turn off scroll bars by uncommenting these lines:
      			    ;; (vertical-scroll-bars . nil)
      			    ;; (horizontal-scroll-bars . nil)

      			    ;; Setting the face in here prevents flashes of
      			    ;; color as the theme gets activated
      			    (background-color . "#000000")
      			    (foreground-color . "#ffffff")
      			    (ns-appearance . dark)
      			    (ns-transparent-titlebar . t)))

  ;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; (setq backward-delete-char-untabify-method 'hungry)

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

  (xterm-mouse-mode 1)

  (setq
   initial-scratch-message nil
   inhibit-startup-screen nil
   inhibit-startup-echo-area-message (user-login-name)
   confirm-nonexistent-file-or-buffer nil
   x-underline-at-descent-line t
   initial-buffer-choice t
   password-cache-expiry nil
   word-wrap t
   shift-select-mode nil
   sentence-end-double-space nil
   revert-without-query '(".*")
   enable-recursive-minibuffers t
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
  (global-auto-revert-mode 1)

  ;; Save history of minibuffer
  (setq history-length 1000
        savehist-additional-variables
        '(search-ring regexp-search-ring extended-command-history)
        savehist-autosave-interval 300)
  (savehist-mode 1)

  ;; Move through windows with Ctrl-<arrow keys>
  (windmove-default-keybindings 'shift) ; You can use other modifiers here

  ;; Make right-click do something sensible
  (when (display-graphic-p)
    (context-menu-mode))

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

  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 1)
  (setq completions-detailed t)                        ; Show annotations
  (setq tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
  (setq completion-styles '(basic initials substring)) ; Different styles to match input to candidates
  (setq completion-auto-help 'always)                  ; Open completion always; `lazy' another option
  (setq completions-max-height 20)                     ; This is arbitrary
  (setq completions-format 'one-column)
  (setq completions-group t)
  (setq completion-auto-select 'second-tab)            ; Much more eager
                                          ;(setq completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

  (keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  (setq-default
   compare-ignore-case t
   compare-ignore-whitespace t
   cursor-in-non-selected-windows t
   display-time-default-load-average nil
   view-read-only t
   require-final-newline nil
   indent-tabs-mode nil
   mode-require-final-newline nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  (setq x-underline-at-descent-line nil)           ; Prettier underlines
  (setq switch-to-buffer-obey-display-actions t)   ; Make s witching buffers more consistent

  (setq show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
  (setq indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

  (use-package simple
    :demand t
    :straight nil
    :bind (("C-c j" . join-line))
    :custom
    (visual-line-fringe-indicators '(left-curly-arrow nil))
    (set-mark-command-repeat-pop t)
    (mark-ring-max 4)
    :config
    (global-visual-line-mode 1)
    (column-number-mode 1)

    ;; Apply `visual-line-mode' only on not `org-agenda-mode' buffers.
    (advice-add 'visual-line-mode :around
      	      (lambda (orig-fun &rest args)
      		(unless (memq major-mode (list 'org-agenda-mode))
      		  (apply orig-fun args)))))

  (when (display-graphic-p)
    (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode))

  ;; Set default input method and coding system
  (use-package mule
    :demand t
    :straight nil
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

  (setq
   initial-scratch-message nil
   inhibit-startup-screen nil
   inhibit-startup-echo-area-message (user-login-name)
   confirm-nonexistent-file-or-buffer nil
   x-underline-at-descent-line t
   initial-buffer-choice t
   password-cache-expiry nil
   word-wrap t
   shift-select-mode nil
   sentence-end-double-space nil
   revert-without-query '(".*")
   enable-recursive-minibuffers t
   show-paren-delay 0
   save-some-buffers-default-predicate t
   buffer-save-without-query t)

  (setq initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
  (setq display-time-default-load-average nil) ; this information is useless for most

  ;; Automatically reread from disk if the underlying file changes
  (setq auto-revert-avoid-polling t)
  ;; Some systems don't do file notifications well; see
  ;; https://todo.sr.ht/~ashton314/emacs-bedrock/11
  (setq auto-revert-interval 5)
  (setq auto-revert-check-vc-info t)
  (global-auto-revert-mode 1)

  ;; Save history of minibuffer
  (setq history-length 1000
        savehist-additional-variables
        '(search-ring regexp-search-ring extended-command-history)
        savehist-autosave-interval 300)
  (savehist-mode 1)

  ;; Move through windows with Ctrl-<arrow keys>
  (windmove-default-keybindings 'shift) ; You can use other modifiers here

  ;; Make right-click do something sensible
  (when (display-graphic-p)
    (context-menu-mode))

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

  ;; No electric indent
  (setq electric-indent-mode nil)

  ;; Include entire file path in title
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))

  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 1)
  (setq completions-detailed t)                        ; Show annotations
  (setq tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
  (setq completion-styles '(basic initials substring)) ; Different styles to match input to candidates
  (setq completion-auto-help 'always)                  ; Open completion always; `lazy' another option
  (setq completions-max-height 20)                     ; This is arbitrary
  (setq completions-format 'one-column)
  (setq completions-group t)
  (setq completion-auto-select 'second-tab)            ; Much more eager
                                          ;(setq completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

  (keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (setq text-mode-ispell-word-completion nil)

  ;; Start Emacs server for emacsclient
  (use-package server
    :defer 2
    :config
    (unless (server-running-p)
      (server-start)))

;; https://github.com/jwiegley/emacs-async/
(use-package async
  :demand t
  :config
  (require 'async-bytecomp)
  (require 'dired-async)
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1)
  (setq async-bytecomp-allowed-packages '(all))
  (setq async-quiet-switch "-q")) ; maybe that for tramp only

  (delete-selection-mode 1)
  (auto-compression-mode 1)

  (global-hl-line-mode -1)

  (defalias 'qcalc #'quick-calc)

  (setq help-window-select t)

  (use-package help
    :disabled t
    :demand t
    :straight nil
  (help-quick-toggle))

  (use-package which-key
    :defer 2
    :diminish which-key-mode
    :custom
    (which-key-idle-delay 0.3)
    (which-key-show-early-on-C-h t) ; Allow F1 to trigger which-key before it is done automatically
    :config
    (which-key-mode 1))

  (use-package helpful
    :after counsel
    :custom
    (counsel-describe-function-function #'helpful-callable)
    (counsel-describe-variable-function #'helpful-variable)
    :bind
    ([remap describe-function] . counsel-describe-function)
    ([remap describe-command] . helpful-command)
    ([remap describe-variable] . counsel-describe-variable)
    ([remap describe-key] . helpful-key))

  (setq-default
   vc-follow-symlinks t
   vc-handled-backends '(Git))

  ;; Make backups of files, even when they're in version control
  (setq vc-make-backup-files t)

  (use-package diff-hl
    :defer 2
    :after magit
    :config
    (global-diff-hl-mode 1)
    (diff-hl-margin-mode 1)
    (diff-hl-amend-mode 1)
    (diff-hl-show-hunk-mouse-mode 1)
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  (use-package magit
    :demand 0.5
    :bind (:map magit-mode-map
                ("C-w" . my/copy-diff-region)
                :map magit-process-mode-map
                ("k" . magit-process-kill)
                :map magit-file-section-map
                ("RET" . magit-diff-visit-file-other-window)
                :map magit-hunk-section-map
                ("RET" . magit-diff-visit-file-other-window))
    :custom
    ;; (magit-diff-refine-hunk t) ; TODO ??
    (magit-completing-read-function 'ivy-completing-read)
    (magit-refresh-status-buffer nil)
    (magit-item-highlight-face 'bold)
    (magit-diff-paint-whitespace nil)
    (magit-ediff-dwim-show-on-hunks t)
    (magit-diff-hide-trailing-cr-characters t)
    (magit-bury-buffer-function 'quit-window)
    (magit-commit-ask-to-stage nil)
    (magit-commit-squash-confirm nil)
    (magit-no-confirm '(stage-all-changes unstage-all-changes set-and-push edit-published rebase-published amend-published))
    (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
    (auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffer-p)
    (magit-blame-styles '((margin
                           (margin-format " %s%f" " %C %a" " %H")
                           (margin-width . 42)
                           (margin-face . magit-blame-margin)
                           (margin-body-face magit-blame-dimmed))
            		(headings
                           (heading-format . "%-20a %C %s"))))
    (magit-section-initial-visibility-alist '((untracked . show)
                                              (unstaged . show)
                                              (unpushed . show)
                                              (unpulled . show)
                                              (stashes . show)))
    :config
    (defun my/copy-diff-region ()
      "Copy diff region without + or - markers."
      (interactive)
      (deactivate-mark)
      (let ((text (buffer-substring-no-properties
                   (region-beginning) (region-end))))
        (kill-new (replace-regexp-in-string "^[\\+\\-]" "" text))))

    (defalias 'magit-blame-echo #'magit-blame-addition)

    (magit-add-section-hook 'magit-status-sections-hook #'magit-insert-unpushed-to-upstream #'magit-insert-unpushed-to-upstream-or-recent)
    (magit-add-section-hook 'magit-status-sections-hook #'magit-insert-recent-commits #'magit-insert-unpushed-to-upstream-or-recent)
    (remove-hook 'magit-status-sections-hook #'magit-insert-unpushed-to-upstream-or-recent))

  (unbind-key "C-x <right>")
  (unbind-key "C-x <left>")

  (setq display-buffer-alist
        '(("\\*[hH]elp.*"
  	 (display-buffer-reuse-window display-buffer-below-selected)
  	 (window-height . 0.4)
  	 (reusable-frames . nil))
  	("\\*grep\\*"
  	 (display-buffer-reuse-window display-buffer-at-bottom)
  	 (window-height . 0.3)
  	 (reusable-frames . nil))
  	("\\*eshell\\*"
  	 (display-buffer-reuse-window display-buffer-at-bottom)
  	 (window-height . 0.3)
  	 (reusable-frames . nil))
  	("\\*Backtrace\\*"
  	 (display-buffer-reuse-window display-buffer-at-bottom)
  	 (window-height . 0.3)
  	 (reusable-frames . nil))
  	("\\*Warnings\\*"
  	 (display-buffer-reuse-window display-buffer-at-bottom)
  	 (window-height . 0.2)
  	 (reusable-frames . nil))
  	("\\*Completions\\*"
  	 (display-buffer-reuse-window display-buffer-at-bottom)
  	 (window-height . 0.3)
  	 (reusable-frames . nil))
  	("\\*Flycheck error messages\\*"
  	 (display-buffer-reuse-window display-buffer-at-bottom)
  	 (window-height . 0.3)
  	 (reusable-frames . nil))
  	("\\*Async-native-compile-log\\*"
  	 (display-buffer-reuse-window display-buffer-at-bottom)
  	 (window-height . 0.3)
  	 (reusable-frames . nil))
  	("\\*straight-byte-compilation\\*"
  	 (display-buffer-reuse-window display-buffer-at-bottom)
  	 (window-height . 0.3)
  	 (reusable-frames . nil))
  	("\\*straight-process\\*"
  	 (display-buffer-reuse-window display-buffer-at-bottom)
  	 (window-height . 0.3)
  	 (reusable-frames . nil))
  	("\\*chatgpt.*"
  	 (display-buffer-in-previous-window)
  	 (reusable-frames . nil))
  	("\\*eww history\\*"
  	 (display-buffer-reuse-window display-buffer-at-bottom)
  	 (window-height . 0.2)
  	 (reusable-frames . nil))
  	("\\*eww bookmarks\\*"
  	 (display-buffer-reuse-window display-buffer-at-bottom)
  	 (window-height . 0.2)
  	 (reusable-frames . nil))
  	("\\*ruby\\*"
  	 (display-buffer-reuse-window display-buffer-at-bottom)
  	 (window-height . 0.1)
  	 (reusable-frames . nil))
  	("\\magit:"
  	 (display-buffer-in-previous-window)
  	 (reusable-frames . nil))
  	("\\*eldoc"
  	 (display-buffer-reuse-window display-buffer-at-bottom)
  	 (window-height . 0.2)
  	 (reusable-frames . nil))
  	("\\*projectile-files-errors\\*"
  	 (display-buffer-reuse-window display-buffer-at-bottom)
  	 (window-height . 0.2)
  	 (reusable-frames . nil))
  	("\\*Org Entity Help\\*"
  	 (display-buffer-reuse-window display-buffer-at-bottom)
  	 (window-height . 0.3)
  	 (reusable-frames . nil))
  	;; ("\\*tree-sitter"
  	;;    (display-buffer-reuse-window display-buffer-same-window)
  	;;    (window-parameters . ((quit-restore . delete))))
  	))

  (use-package diminish
    :demand t
    :config
    (diminish 'eldoc-mode))

  (straight-register-package 'all-the-icons)
  (when (display-graphic-p)
    (use-package all-the-icons
      :demand t
      :config
      ;; (setq inhibit-compacting-font-caches t) ; uncomment on rendering performance issues
      (unless (find-font (font-spec :name "all-the-icons"))
        (all-the-icons-install-fonts t))))

  (straight-register-package 'nerd-icons)
  (when (display-graphic-p)
    (use-package nerd-icons
      :demand t
      :config
      (when (member system-type '(gnu gnu/linux gnu/kfreebsd darwin))
        (unless (find-font (font-spec :name "Symbols Nerd Font Mono"))
          (nerd-icons-install-fonts t)
          (message "nerd-icons installed")))))

  (setq  bookmark-save-flag 1)

  ;; Show more than 4 levels when evaling expressions
  (setq eval-expression-print-level 100)

  ;; Add parts of each file's directory to the buffer name if not unique
  (use-package uniquify
    :straight nil
    :defer 2 ;; Loads after 2 seconds of idle time.
    :custom
    (uniquify-buffer-name-style 'forward))

  (use-package avy
    :bind ("C-:" . avy-goto-char))

  ;; Modify search results en masse
  (use-package wgrep
    :demand t
    :custom
    (wgrep-auto-save-buffer t))

  (use-package elec-pair
    :straight nil
    ;; :hook (prog-mode . electric-pair-mode)
    :config
    (push '(?\{ . ?\}) electric-pair-pairs)
    (push '(?\( . ?\)) electric-pair-pairs)
    (push '(?\{ . ?\}) electric-pair-text-pairs))

  (use-package recentf
    :demand t
    :straight nil
    :custom
    (recentf-max-menu-items 15)
    (recentf-max-saved-items 100)
    (recentf-exclude
     '("COMMIT_EDITMSG"
       "~$"
       "/scp:"
       "/ssh:"
       "/sudo:"
       "/tmp/"
       "?:cache"
       "eln-cache"
       ))
    :config
    (recentf-mode 1)
    (run-at-time nil (* 60 5) #'recentf-save-list))

  (use-package consult
    :defer 1
    :bind (("C-x C-y" . consult-yank-from-kill-ring)
           ;; Drop-in replacements
           ("C-x b" . consult-buffer)     ; orig. switch-to-buffer
           ("M-y"   . consult-yank-pop)   ; orig. yank-pop
           ("M-g g" . consult-goto-line)
           ("M-g M-g" . consult-goto-line)
           ("C-x i" . consult-outline)
           ("C-x C-i" . consult-imenu)
           ("C-x M-i" . consult-imenu-multi)
           ;; Searching
           ("C-c b" . consult-bookmark)
           ("C-x f" . consult-recent-file)
           ("C-x C-r" . consult-recent-file)
           ("M-s r" . consult-ripgrep)
           ("M-s l" . consult-line)       ; Alternative: rebind C-s to use
           ("M-s s" . consult-line)       ; consult-line instead of isearch, bind
           ("M-s L" . consult-line-multi) ; isearch to M-s s
           ("M-s o" . consult-outline)
           ;; Kill ring
           ("C-x C-y" . consult-yank-from-kill-ring)
           ;; Isearch integration
           :map isearch-mode-map
           ("M-e" . consult-isearch-history)   ; orig. isearch-edit-string
           ("M-s e" . consult-isearch-history) ; orig. isearch-edit-string
           ("M-s l" . consult-line)            ; needed by consult-line to detect isearch
           ("M-s L" . consult-line-multi)      ; needed by consult-line to detect isearch
           )
    :config
    ;; Narrowing lets you restrict results to certain groups of candidates
    (setq consult-narrow-key "<"))

  (use-package consult-flycheck
    :demand 2
    :after (consult flycheck)
    :bind ("M-g f" . consult-flycheck))

  ;; Embark: supercharged context-dependent menu; kinda like a
  ;; super-charged right-click.
  (use-package embark
    :demand t
    :after avy
    :bind (("C-c a" . embark-act))        ; bind this to an easy key to hit
    :init
    ;; Add the option to run embark when using avy
    (defun bedrock/avy-action-embark (pt)
      (unwind-protect
          (save-excursion
            (goto-char pt)
            (embark-act))
        (select-window
         (cdr (ring-ref avy-ring 0))))
      t)

    ;; After invoking avy-goto-char-timer, hit "." to run embark at the next
    ;; candidate you select
    (setf (alist-get ?. avy-dispatch-alist) 'bedrock/avy-action-embark))

  (use-package embark-consult
    :demand t
    :after (embark consult))

  (use-package dashboard
    :demand t
    :after counsel
    :custom
    (dashboard-icon-type 'all-the-icons)
    (dashboard-display-icons-p (display-graphic-p))
    (dashboard-set-heading-icons nil)
    (dashboard-set-file-icons (display-graphic-p))
    (dashboard-heading-icons
     '((recents . "history")
       (registers . "database")
       (bookmarks . "bookmark")
       (projects . "rocket")))
    (dashboard-center-content t)
    (dashboard-set-init-info t)
    (dashboard-item-names
     '(("Recent Files:" . "Recent files:")
       ("Agenda for today:" . "Today's agenda:")
       ("Agenda for the coming week:" . "Agenda for the week:")))
    (dashboard-projects-switch-function #'counsel-projectile-switch-project-by-name)
    (dashboard-projects-backend 'projectile)
    :config
    (dashboard-setup-startup-hook))

  (use-package ivy
    :demand t
    :custom
    (ivy-height 15)
    (ivy-use-selectable-prompt t)
    (ivy-count-format "(%d/%d) ")
    (ivy-use-virtual-buffers t)
    (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
    :config
    (ivy-mode 1)
    ;; (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-alt-done) ;; or ivy-next-line
    )

  (use-package ivy-xref
    :demand t
    :after ivy
    :init
    (when (>= emacs-major-version 27)
      (setq-default xref-show-definitions-function #'ivy-xref-show-defs))
    :custom
    (xref-show-xrefs-function #'ivy-xref-show-xrefs)
    (xref-show-definitions-function #'ivy-xref-show-defs))

  (use-package counsel
    :demand t
    :bind (("M-x" . counsel-M-x)
           ("C-x C-f" . counsel-find-file)
           ;; ("C-c b" . counsel-bookmark)
           ;; ("C-x C-r" . counsel-recentf)
           ("C-h v" . counsel-describe-variable)
           ("C-h f" . counsel-describe-function)
           ("C-h S" . counsel-info-lookup-symbol)
           ("C-h a" . counsel-apropos)
           ;; ("C-x b" . counsel-switch-buffer)
    	 ;;  ("C-s" . my/counsel-grep-at-point-plain)
           ;; ("C-r" . my/counsel-grep-at-point-plain)
           ;; :map read-expression-map
           ;; ("C-r" . counsel-minibuffer-history)
           ;; ("C-r" . counsel-expression-history)
           )
    :custom
    (counsel-rg-base-command "rg -S -M 150 --no-heading --line-number --color never %s")
    (counsel-find-file-ignore-regexp "\\`\\.")
    (counsel-grep-base-command "grep -E -n -i -e %s %s")
    (counsel-outline-path-separator " > ")
    (counsel-find-file-at-point t)
    (swiper-use-visual-line-p #'ignore)
    :config
    ;; Pre-fill initial entry as symbol at point
    (defun my/counsel-grep-at-point-plain ()
      (interactive)
      (let ((w (thing-at-point 'symbol t)))
        (counsel-grep (or w nil))))

    (ivy-set-display-transformer 'counsel-describe-function nil))

  ;; Vertico: better vertical completion for minibuffer commands
  (use-package vertico
    :disabled t
    :demand t
    :config
    ;; You'll want to make sure that e.g. fido-mode isn't enabled
    (add-to-list 'load-path (expand-file-name "straight/repos/vertico/extensions" user-emacs-directory))
    (vertico-mode)

    (use-package vertico-directory
      :straight nil
      :after vertico
      :bind (:map vertico-map
  		("M-DEL" . vertico-directory-delete-word))))

  (use-package orderless
    :demand t
    :custom
    (completion-styles '(orderless basic))
    (completion-category-defaults nil)
    (completion-pcm-leading-wildcard t)
    (completion-category-overrides '((file (styles partial-completion)))))

  ;; Marginalia: annotations for minibuffer
  (use-package marginalia
    :demand t
    :config
    (marginalia-mode))

  (use-package corfu
    :demand t
    :bind (:map corfu-map
  	      ("SPC" . corfu-insert-separator)
  	      ("C-n" . corfu-next)
  	      ("C-p" . corfu-previous))
    :custom
    (corfu-cycle t)
    (corfu-on-exact-match nil)
    :init
    (add-to-list 'load-path (expand-file-name "straight/repos/corfu/extensions" user-emacs-directory))
    (require 'corfu-history)
    (require 'corfu-popupinfo)
    (require 'corfu-echo)
    (require 'corfu-info)
    :config
    (global-corfu-mode 1)
    (corfu-history-mode 1)
    (corfu-popupinfo-mode 1) ; Popup completion info
    (corfu-echo-mode 1)
    (add-hook 'eshell-mode-hook
  	    (lambda () (setq-local corfu-quit-at-boundary t
  				   corfu-quit-no-match t
  				   corfu-auto nil)
  	      (corfu-mode))))

  (use-package cape
    :demand t
    :after corfu
    ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
    ;; Press C-c p ? to for help.
    :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
    ;; Alternatively bind Cape commands individually.
    ;; :bind (("C-c p d" . cape-dabbrev)
    ;;        ("C-c p h" . cape-history)
    ;;        ("C-c p f" . cape-file)
    ;;        ...)
    :config
    ;; Add to the global default value of `completion-at-point-functions' which is
    ;; used by `completion-at-point'.  The order of the functions matters, the
    ;; first function returning a result wins.  Note that the list of buffer-local
    ;; completion functions takes precedence over the global list.
    (add-hook 'completion-at-point-functions #'cape-dabbrev)
    (add-hook 'completion-at-point-functions #'cape-file)
    (add-hook 'completion-at-point-functions #'cape-elisp-block)
    ;; (add-hook 'completion-at-point-functions #'cape-history)
    )

  (straight-register-package 'nerd-icons-corfu)
  (when (display-graphic-p)
    (use-package nerd-icons-corfu
      :demand t
      :after nerd-icons
      :config
      (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)))

  (straight-register-package 'evil)
  (straight-register-package 'evil-collection)
  (when my/evil-enable
    (use-package evil
      :disabled t
      :demand t
      :custom
      (evil-respect-visual-line-mode t)
      (evil-undo-system 'undo-redo)
      (evil-want-C-u-scroll t)
      :config
      (evil-mode)

      ;; If you use Magit, start editing in insert state
      ;; (add-hook 'git-commit-setup-hook 'evil-insert-state)

      ;; Configuring initial major mode for some modes
      (evil-set-initial-state 'eat-mode 'emacs)
      (evil-set-initial-state 'vterm-mode 'emacs))

    (use-package evil-collection
      :disabled t
      :demand t
      :after evil
      :config
      (evil-collection-init)))

  (setq undo-limit 160000)
  (global-set-key (kbd "C-\\") #'undo)
  (global-set-key (kbd "C-/") #'undo-redo)


;; (use-package move-text
  ;; :bind (("M-<up>" . move-text-up)
         ;; ("M-<down>" . move-text-down)))

(use-package multiple-cursors
  :demand t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; Automatically prompt for sudo in write protected files
(use-package auto-sudoedit
  :defer 1
  :config (auto-sudoedit-mode 1))

(use-package drag-stuff
  :demand t
  :diminish drag-stuff-mode
  :config
  (add-to-list 'drag-stuff-except-modes #'org-mode)
  (add-to-list 'drag-stuff-except-modes #'org-journal-mode)
  (drag-stuff-define-keys)
  (drag-stuff-global-mode 1)
  (define-key drag-stuff-mode-map (drag-stuff--kbd 'up) #'drag-stuff-up)
  (define-key drag-stuff-mode-map (drag-stuff--kbd 'down) #'drag-stuff-down))

(use-package expand-region
  :commands (er/expand-region er/contract-region)
  :bind (("C-'" . er/expand-region)))

(use-package persistent-scratch
  :disabled t
  :demand t
  :config
  (persistent-scratch-setup-default))

(use-package editorconfig
  :demand t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package ediff
  :straight nil
  :commands  (ediff ediff-buffers)
  :custom
  (ediff-quit-widened nil)
  (ediff-diff-options "-w")
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-forward-word-function #'forward-char))

(defun my/revert-buffer-noconfirm ()
  "Revert current buffer without asking for confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm)
  (message "Buffer '%s' reloaded." (file-name-nondirectory buffer-file-name)))

(use-package tab-bar
  :demand t
  :straight nil
  ;; :bind (("<backtab>" . tab-bar-switch-to-next-tab))
  :custom
  (tab-bar-auto-width t)
  ;; (tab-bar-tab-name-function #'tab-bar-tab-name-truncated)
  ;; (tab-bar-tab-name-truncated-max 30)
  :config
  (add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
  (add-to-list 'tab-bar-format 'tab-bar-format-global 'append)

  (if (eq system-type 'darwin)
      (setq tab-bar-auto-width-max '(150 15))
    (setq tab-bar-auto-width-max '(270 15)))
  (tab-bar-mode 1))

(use-package tab-line
  :disabled t
  :demand t
  :straight nil
  :custom
  ;; (tab-line-format)
  (tab-line-separator "")
  (tab-line-close-button-show t)
  (tab-line-new-button-show nil)
  ;; (tab-line-exclude-modes)
  ;; (tab-line-exclude)
  :config
  (global-tab-line-mode 1))

(use-package beacon
  :demand t
  :custom
  (beacon-mode 1))

(require 'files)
(defun my/rgrep ()
  (interactive)
  (if (executable-find "ack")
      (let* ((regexp (grep-read-regexp))
             (dir (read-directory-name "Base directory: " nil default-directory t))
             (command (concat "ack '" regexp "' '" dir "'")))
        (unless (file-accessible-directory-p dir)
          (error "directory: '%s' is not accessible." dir))
  	    (compilation-start (concat command " < " null-device) 'grep-mode))
    (error "No executable 'ack' found!")))

(use-package windmove
  :straight nil
  :defer 10
  :bind (("C-x <left>" . windmove-left)
         ("C-x <right>" . windmove-right)
         ("C-x <up>" . windmove-up)
         ("C-x <down>" . windmove-down))
  :custom
  (windmove-wrap-around t))

  (setq
   scroll-step 1
   scroll-margin 3
   ;; scroll-conservatively 101
   hscroll-margin  1
   hscroll-step 1
   auto-window-vscroll nil)

  (when (display-graphic-p)
    (setq-default scroll-up-aggressively 0.01)
    (setq
     ;; mouse-wheel-scroll-amount '(1 ((shift) . 1))
     redisplay-dont-pause t
     ;; scroll-conservatively 10000
     scroll-conservatively most-positive-fixnum ; Always scroll by one line
     scroll-preserve-screen-position nil))

  (use-package ibuffer
    :straight nil
    :commands ibuffer
    :hook (ibuffer-mode . hl-line-mode))

  (use-package browse-kill-ring
    :commands browse-kill-ring)

  (defun my/kill-clear ()
    "Clear kill-ring."
    (interactive)
    (setq kill-ring nil)
    (garbage-collect)
    (message "kill-ring cleared"))

  (defun my/parens-jump ()
    "Interactively show the character before and after point.
  Displays each as a readable string or \"<BOF/EOF>\" when out of range."
    (interactive)
    (let ((before (char-before (point)))
          (after  (char-after  (point))))
      (cond
       ((= after ?\() (forward-sexp))
       ((= before ?\)) (backward-sexp))
       (t nil))))

  (bind-keys
   ("C-M-b" . my/parens-jump)
   ("C-M-f" . my/parens-jump))

  (setq-default mml-secure-openpgp-sign-with-sender t)

  (if (boundp 'my/epa-file-encrypt-to-default)
      (use-package epa
        :demand t
        :straight nil
        :custom
        (epg-pinentry-mode 'loopback)
        (epa-file-select-keys nil)
        ;; (epa-file-cache-passphrase-for-symmetric-encryption t)
        :config
        (require 'epa-file)
        (require 'epg-config)
        ;; (setenv "GPG_AGENT_INFO" nil)
        ;; (setq epa-pinentry-mode 'loopback)

        (add-hook 'find-file-hook
                  (lambda ()
                    (when (and (stringp buffer-file-name) (string-match "\\.gpg\\'" buffer-file-name))
                      (unless (alist-get 'epa-file-encrypt-to file-local-variables-alist)
                        (when (boundp 'my/epa-file-encrypt-to-default) my/epa-file-encrypt-to-default
                              (setq-local epa-file-encrypt-to my/epa-file-encrypt-to-default)))))))
    (warn "Variable '%s' is not set, GPG not available!" my/epa-file-encrypt-to-default)))

  (use-package dired
    :demand t
    :straight nil
    :bind (:map dired-mode-map
                ("C-s" . isearch-forward)
                ("<backspace>" . dired-up-directory)
                ("DEL" . dired-up-directory) ; optional
                ("W" . my/dired-copy-path-to-file-as-kill)
                ("RET" .  dired-find-file))
    :hook (dired-mode . dired-hide-details-mode)
    :custom
    (enable-command 'dired-find-alternate-file)
    (dired-kill-when-opening-new-dired-buffer t)
    (dired-dwim-target t)
    (dired-listing-switches "-al --group-directories-first")
    (dired-recursive-deletes 'always)
    (dired-recursive-copies 'always)
    (dired-deletion-confirmer 'y-or-n-p)
    (dired-auto-revert-buffer t)
    (dired-clean-confirm-killing-deleted-buffers nil)
    :config
    (defun my/sudo-dired ()
      "Run Dired in sudo mode."
      (interactive)
      (dired "/sudo::/"))

    (add-hook 'dired-mode-hook
              (lambda ()
                (when (and recentf-mode (buffer-file-name)) (recentf-add-file (buffer-file-name)))))
    (define-key dired-mode-map [mouse-1] nil)
    (define-key dired-mode-map [mouse-2] nil)

    (defun my/dired-copy-path-to-file-as-kill (&optional arg)
      "Copy current file path into the kill ring."
      (interactive "P")
      (let ((string
             (or (dired-get-subdir)
                 (mapconcat #'identity
                            (if arg
                                (cond ((zerop (prefix-numeric-value arg))
                                       (dired-get-marked-files))
                                      ((consp arg)
                                       (dired-get-marked-files t))
                                      (t
                                       (dired-get-marked-files
  				      'no-dir (prefix-numeric-value arg))))
                              (dired-get-marked-files 'no-dir))
                            " "))))
        (unless (string= string "")
          (let ((new-kill (concat (expand-file-name string default-directory))))
            (kill-new new-kill)
            (message "%s" new-kill))))))

(straight-register-package 'all-the-icons-dired)
(when (display-graphic-p)
  (use-package all-the-icons-dired
    :demand t
    :after dired
    :config
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)))

  (use-package major-mode-hydra
    :demand t
    :commands major-mode-hydra
    :bind ("C-c h" . hydra-base/body)
    :config
    (defun with-alltheicon (icon str &optional height v-adjust)
      "Displays an icon from all-the-icon."
      (if (display-graphic-p)
    	(s-concat (all-the-icons-alltheicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str)
        str))

    (defun with-faicon (icon str &optional height v-adjust)
      "Displays an icon from Font Awesome icon."
      (if (display-graphic-p)
    	(s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str)
        str))

    (defun with-fileicon (icon str &optional height v-adjust)
      "Displays an icon from the Atom File Icons package."
      (if (display-graphic-p)
    	(s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str)
        str))

    (defun with-octicon (icon str &optional height v-adjust)
      "Displays an icon from the GitHub Octicons."
      (if (display-graphic-p)
    	(s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str)
        str))

    (pretty-hydra-define hydra-project
  		       (:hint nil :color teal :quit-key "q" :title (with-faicon "archive" "Project" 1 -0.05))
  		       ("Find"
  			(("p" counsel-projectile-switch-project "project")
    			 ("o" projectile-find-other-file "other file")
    			 ("t" projectile-find-tag "tag")
    			 ("s" counsel-rg "search")
    			 ("g" yant/occur-current-project "grep"))
  			"Actions"
  			(("a" my/projectile-add-known-project "add")
    			 ("d" projectile-remove-known-project "remove")
  			 ("i" projectile-invalidate-cache "reset cache")
    			 ("r" (my/func-call '(projectile-invalidate-cache nil) 'projectile-replace-regexp '(save-some-buffers t)) "regexp replace"))))

    (pretty-hydra-define hydra-org
  		       (:hint nil :color teal :quit-key "q" :title (with-faicon "anchor" "Org" 1 -0.05))
  		       ("Action"
  			(("c" org-reset-checkbox-state-subtree "reset all org checkbox in subtree" :exit t)
  			 ("d" my/org-remove-duplicate-lines-in-list "remove list duplicates")
  			 ("t" org-toggle-timestamp-type "timestamp toggle")
  			 ("l" org-link-archive-at-point "link archive")
  			 ("h" org-archive-subtree "archive heading subtree"))
  			"Toggle"
  			(("a" org-appear-mode "org-appear toggle" :toggle t)
  			 ("l" org-table-header-line-mode "org-table-header-line-mode" :toggle t))
  			"Navigation"
  			(("g" counsel-org-goto "goto heading"))
  			"Roam"
  			(("s" org-roam-node-find "find node"))))

    (pretty-hydra-define hydra-navigation
  		       (:hint nil :color teal :quit-key "q" :title (with-faicon "compass" "Navigation" 1 -0.05))
  		       (""
  			(("a" my/rgrep "ack" :exit t))
  			"Registers"
  			(("rp" point-to-register "point to register" :exit t)
  			 ("rj" jump-to-register "jump to register" :exit t))
  			"Bookmarks"
  			(("b" counsel-bookmark "jump or set" :exit t)
  			 ("s" bookmark-save "save bookmarks" :exit t) ; this probably happens automatically
  			 ("l" list-bookmarks "list bookmarks" :exit t))))

    (pretty-hydra-define hydra-git
  		       (:hint nil :color teal :quit-key "q" :title (with-alltheicon "git" "Git" 1 -0.05))
  		       ("Action"
  			(("s" magit-status "status")
  			 ("b" magit-blame "blame")
    			 ("l" magit-log-buffer-file "commit log (current file)")
    			 ("L" magit-log-current "commit log (project)"))))

    (pretty-hydra-define hydra-write
  		       (:hint nil :color teal :quit-key "q" :title (with-faicon "pencil" "Write" 1 -0.05))
  		       ("Language"
  			(("d" ispell-change-dictionary "change dictionary")
    			 ("s" (lambda () (interactive) (flyspell-mode 'toggle)) "flyspell toggle")
    			 ("w" my/lang-toggle "language switch" :exit t))
  			"Fix grammar"
  			(("o" artbollocks-mode "artbollocks" :toggle t)
  			 ("<" flyspell-correct-previous "previous")
    			 (">" flyspell-correct-next "next"))))

    (pretty-hydra-define hydra-dev
  		       (:hint nil :color teal :quit-key "q" :title (with-faicon "cogs" "Dev" 1 -0.05))
  		       ("Action"
  			(("x" xref-find-references-and-replace "replace references" :exit t)
  			 ("u" lorem-ipsum-insert-sentences "lorem ipsum" :exit t)
  			 ("ac" counsel-colors-emacs "color picker" :exit t)
  			 ("o" my/eglot-organize-imports "organize imports" :exit t))
  			"Navigate"
  			(("d" my/xref-find-definitions-at-point "find definitions" :exit t)
  			 ("r" my/xref-find-references-at-point "find references" :exit t)
  			 ("t" projectile-find-tag "find tag" :exit t)
  			 ("g" counsel-projectile-git-grep "git grep" :exit t)
  			 ("i" counsel-imenu "imenu" :exit t)
  			 ("k" my/treemacs-project-toggle "treemacs" :toggle t :exit t))
  			"Toggle"
  			(("fc" flycheck-mode "flycheck" :toggle t)
  			 ("fm" flymake-mode "flymake" :toggle t))))

    (pretty-hydra-define hydra-base
  		       (:hint nil :color teal :quit-key "q" :title (with-faicon "coffee" "Base" 1 -0.05))
  		       (""
  			(("p" hydra-project/body "project")
  			 ("n" hydra-navigation/body "navigation")
  			 ("g" hydra-git/body "git")
  			 ("o" hydra-org/body "org")
  			 ("d" hydra-dev/body "dev")
  			 ("w" hydra-write/body "write")))))

  ;; This is for async evalaution of org-babel blocks.
  (straight-register-package '(ob-async :repo "farynaio/ob-async" :host github :branch "master"))
  (use-package ob-async
    :config
    (setq ob-async-no-async-languages-alist '("python" "ipython" "jupyter-python" "jupyter-julia")))

  ;; (use-package org-table-cell-move
  ;; :demand t
  ;; :after org
  ;; :straight nil)

  (use-package org
    :demand t
    :bind (:map org-mode-map
                ("C-c C-j" . join-line)
    	      ("M-<up>" . org-metaup)
    	      ("M-<down>" . org-metadown)
    	      :map org-mode-map
    	      ("M-}" . forward-paragraph)
    	      ("M-{" . backward-paragraph ))
    :hook ((org-mode . org-sticky-header-mode)
    	 (org-agenda-mode . hl-line-mode)
           (org-mode . org-indent-mode))
    :custom
    (org-adapt-indentation nil)
    (org-startup-folded 'fold)
    (org-startup-indented t)
    (org-startup-with-inline-images t)
    (org-return-follows-link t)
    (org-blank-before-new-entry '((heading . auto) (plain-list-item . nil)))
    (org-list-demote-modify-bullet
     '(("+" . "-")
       ("-" . "+")
       ("1." . "-")))
    (org-hide-leading-stars t)
    (org-hide-emphasis-markers t)

    (org-refile-use-outline-path t)
    (org-refile-allow-creating-parent-nodes 'confirm)

    (org-cycle-include-plain-lists t)

    (org-fast-tag-selection-single-key t)

    (org-src-preserve-indentation t)
    (org-src-fontify-natively t)
    (org-src-tab-acts-natively t)

    (org-pretty-entities t)
    (org-pretty-entities-include-sub-superscripts nil)

    (org-return-follows-link t)

    (org-element-use-cache nil)

    (org-confirm-babel-evaluate
     (lambda (lang body)
       (not (member lang '("emacs-lisp")))))

    (org-export-babel-evaluate t)
    (org-export-preserve-breaks t)
    (org-export-with-toc t)
    (org-export-with-smart-quotes t) ; could cause problems on babel export
    (org-export-with-email nil)

    (org-group-tag nil)

    (org-lowest-priority 68)
    (org-highest-priority 65)
    (org-default-priority 66)
    (org-todo-keywords
     '((sequence "TODO(t)" "WIP(p!)" "BLOCKED(b!)" "WAITING(w@/!)" "DELEGATED(e@/!)")
       (sequence "|" "DONE(d!)" "SKIP(c@)" "UNDOABLE(u@)")))
    (org-todo-keyword-faces
     '(("TODO"        . (:foreground "LimeGreen"   :weight bold))
       ("IN-PROCESS" . (:foreground "IndianRed1"  :weight bold))
       ("WIP"        . (:foreground "IndianRed1"  :weight bold))
       ("WORK"       . (:foreground "IndianRed1"  :weight bold))
       ("BLOCKED"    . (:foreground "tomato3"     :weight bold))
       ("WAITING"    . (:foreground "coral"       :weight bold))
       ("DELEGATED"  . (:foreground "coral"       :weight bold))
       ("NOTE"       . (:foreground "white"       :weight bold))
       ("DONE"       . (:foreground "dark grey"   :weight normal))
       ("SKIP"       . (:foreground "dark grey"   :weight normal))
       ("HUGE"       . (:foreground "dark grey"   :weight normal))
       ("UNDOABLE"   . (:foreground "dark grey"   :weight normal))))
    (org-priority-cookie () (format "[#%c]" org-default-priority))
    :config
    (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

    ;; (major-mode-hydra-define (org-mode)
    ;;   (:hint nil :color teal :quit-key "q" :title (with-fileicon "org" "Org" 1 -0.05))
    ;;   ("Action"
    ;;    (("t" org-toggle-timestamp-type "timestamp toggle")
    ;;     ("a" org-link-archive-at-point "link archive")
    ;;     ("i" org-toggle-inline-images "images toggle" :toggle t)
    ;;     ("r" my/org-paragraphs-reverse-order "reverse paragraph order")
    ;;     ("h" org-archive-subtree "archive heading subtree")
    ;;     ("d" my/org-remove-duplicate-lines-in-list "remove list duplicates")
    ;;     ("R" org-reset-checkbox-state-subtree "reset all org checkbox in subtree" :exit t)
    ;;     ("p" my/org-align-tags "align tags" :exit t))
    ;;    "Toggle"
    ;;    (("e" my/org-toggle-emphasis "org-ephasis toggle" :toggle t :exit t)
    ;;     ("l" org-table-header-line-mode "org-table-header-line-mode" :toggle t)
    ;;     ;; ("ob" afa/org-breadcrums-mode "breadcrumbs" :toggle t)
    ;;     ("oa" org-appear-mode "org-appear toggle" :toggle t :exit t))
    ;;    "Navigation"
    ;;    (("s" counsel-org-goto "goto heading")
    ;;    ;; ("fa" counsel-org-file "browse attachments")
    ;;    )))

    (defun my/org-remove-duplicate-lines-in-list ()
      "Remove duplicate lines inside plain-list at point."
      (interactive)
      (let ((list-element (org-element-lineage (org-element-at-point) '(plain-list) t)))
        (if (not list-element)
            (user-error "Not at plain-list")
          (let ((nlines
  	       (delete-duplicate-lines
  	        (org-element-property :post-affiliated list-element)
  	        (save-excursion (goto-char (org-element-property :end list-element)) (skip-chars-backward "\r\n\t ") (point)))))
            (if (= 0 nlines)
  	      (message "List contains no duplicate lines")
              (message "Removed %d duplicate lines from list" nlines))))))

    (defun my/org-align-tags ()
      (interactive)
      (let ((current-prefix-arg '(4))) (call-interactively 'org-set-tags-command)))

    (defun my/org-metaup ()
      (interactive)
      (call-interactively
       (if (org-at-heading-or-item-p)
           'org-metaup
         'drag-stuff-up)))

    (defun my/org-metadown ()
      (interactive)
      (call-interactively
       (if (org-at-heading-or-item-p)
           'org-metadown
         'drag-stuff-down)))

    (defun my/org-toggle-emphasis ()
      "Toggle hiding/showing of org emphasize markers."
      (interactive)
      (setq 'org-hide-emphasis-markers (not org-hide-emphasis-markers))
      (message "org ephasis toggled %s" (if org-hide-emphasis-markers "on" "off")))

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)))

    (push '("conf-unix" . conf-unix) org-src-lang-modes))

  (use-package org-appear
    :after org
    :commands org-appear-mode
    :custom
    (org-appear-delay 0.6)
    (org-hide-emphasis-markers t))

  (straight-register-package 'org-roam)
  (if my/org-roam-dir
      (use-package org-roam
        :after (org emacsql magit)
        :diminish org-roam-mode
        :commands (org-roam-file-p org-roam-buffer-toggle org-roam-node-insert org-roam-find-directory org-roam-ui-open org-roam-node-find my/org-roam-node-find-other-window org-roam-switch-to-buffer org-id-get-create my/hydra-common/body)
        :init
        (unless (file-directory-p my/org-roam-dir)
          (make-directory my/org-roam-dir t))
        :custom
        (org-roam-directory my/org-roam-dir)
        ;; (org-roam-graph-viewer "/usr/bin/open")
        (org-roam-db-gc-threshold most-positive-fixnum)
        (org-roam-tag-sources '(prop))
        (org-roam-update-db-idle-second 60)
        (org-roam-verbose nil)
        (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
        :config
        (org-roam-db-autosync-mode 1)

        ;; (define-derived-mode my/org-roam-mode org-mode "my-org-roam"
        ;;   "Major mode for org-roam ready org buffers.")

        ;; (major-mode-hydra-define+ my/org-roam-mode
        ;;   (:hint nil :color teal :quit-key "q" :title (with-fileicon "org" "org-roam" 1 -0.05))
        ;;   ("Edit"
        ;;    (("n" org-id-get-create "turn heading into node")
        ;;     ("t" org-roam-tag-add "add org-roam tag to node at point")
        ;;     ("T" org-roam-alias-add "add org-roam alias to node at point"))
        ;;    "Navigation"
        ;;    (("u" org-roam-ui-open "open UI view")
        ;;     ("z" org-roam-buffer-toggle "toggle references sidebar" :toggle t))))

        (add-to-list 'display-buffer-alist
    	           '("\\*org-roam\\*"
    		     (display-buffer-in-direction)
    		     (direction . right)
    		     (window-width . 0.35)
    		     (window-height . fit-window-to-buffer)))

        (add-to-list 'magit-section-initial-visibility-alist '([org-roam-node-section org-roam-backlinks org-roam] . hide))

        (defun my/org-roam-file-p (&optional file)
          (when (fboundp 'org-roam-file-p)
            (org-roam-file-p file)))

        ;; (add-to-list 'magic-mode-alist '(my/org-roam-file-p . my/org-roam-mode))
        (defalias 'roam #'org-roam))
    (warn "Variable 'my/org-roam-dir' is not specified, org-roam will not be loaded!"))

  (use-package org-sticky-header
    :after org)

  (use-package org-link-archive
    :after org
    :straight (:type git
                     :host github
                     :repo "farynaio/org-link-archive"
                     :branch "main")
    :bind (:map org-mode-map
    	      ("C-x C-z" . org-link-archive-at-point)))

  (use-package calendar
    :commands (my/calendar-year)
    :bind (:map calendar-mode-map
                ("<" . my/scroll-year-calendar-backward)
                (">" . my/scroll-year-calendar-forward))
    :custom
    (diary-number-of-entries 31)
    (holiday-local-holidays nil)
    (diary-show-holidays-flag nil)
    (calendar-christian-all-holidays-flag t)
    ;; (calendar-mark-holidays-flag t)
    (calendar-week-start-day 1)
    (calendar-date-style 'european)
    :config
    ;; https://stackoverflow.com/questions/9547912/emacs-calendar-show-more-than-3-months
    (defun my/calendar-year (&optional year)
      "Generate a one year calendar that can be scrolled by year in each direction.
  This is a modification of:  http://homepage3.nifty.com/oatu/emacs/calendar.html
  See also: https://stackoverflow.com/questions/9547912/emacs-calendar-show-more-than-3-months"
      (interactive)
      ;; (require 'calendar)
      (let* (
             (current-year (number-to-string (nth 5 (decode-time (current-time)))))
             (month 0)
             (year (if year year (string-to-number (format-time-string "%Y" (current-time))))))
        (switch-to-buffer (get-buffer-create calendar-buffer))
        (when (not (eq major-mode 'calendar-mode))
          (calendar-mode))
        (setq displayed-month month)
        (setq displayed-year year)
        (setq buffer-read-only nil)
        (erase-buffer)
        ;; horizontal rows
        (dotimes (j 4)
          ;; vertical columns
          (dotimes (i 3)
            (calendar-generate-month
             (setq month (+ month 1))
             year
             ;; indentation / spacing between months
             (+ 5 (* 25 i))))
          (goto-char (point-max))
          (insert (make-string (- 10 (count-lines (point-min) (point-max))) ?\n))
          (widen)
          (goto-char (point-max))
          (narrow-to-region (point-max) (point-max)))
        (widen)
        (goto-char (point-min))
        (setq buffer-read-only t)))

    (defun my/scroll-year-calendar-forward (&optional arg event)
      "Scroll the yearly calendar by year in a forward direction."
      (interactive (list (prefix-numeric-value current-prefix-arg)
                         last-nonmenu-event))
      (unless arg (setq arg 0))
      (save-selected-window
        (if (setq event (event-start event)) (select-window (posn-window event)))
        (unless (zerop arg)
          (let* (
                 (year (+ displayed-year arg)))
            (my/calendar-year year)))
        (goto-char (point-min))
        (run-hooks 'calendar-move-hook)))

    (defun my/scroll-year-calendar-backward (&optional arg event)
      "Scroll the yearly calendar by year in a backward direction."
      (interactive (list (prefix-numeric-value current-prefix-arg)
                         last-nonmenu-event))
      (my/scroll-year-calendar-forward (- (or arg 1)) event))

    (defalias 'calendar-year #'my/calendar-year)
    (defalias 'my/calendar-full #'my/calendar-year)
    (defalias 'yearly-calendar #'my/calendar-year))

  (use-package projectile
    :demand t
    :diminish projectile-mode
    :after ivy
    :custom
    (projectile-completion-system 'ivy)
    (projectile-indexing-method 'hybrid)
    (projectile-enable-caching t)
    (projectile-verbose nil)
    (projectile-do-log nil)
    (projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))
    (projectile-track-known-projects-automatically nil)
    (projectile-globally-ignored-files '("TAGS" ".DS_Store" ".keep"))
    (projectile-globally-ignored-file-suffixes '(".png" ".gif" ".pdf" ".class"))
    :config
    (setq projectile-globally-ignored-directories (delete-dups (append '("node-modules" "dist" "target" "*elpa" "straight") projectile-globally-ignored-directories)))
    (unbind-key "C-c p" projectile-mode-map)

    (if (executable-find "ctags")
        (setq projectile-tags-command "ctags -R -e .")
      (warn "No executable 'ctags' found!"))
    ;; (add-hook 'projectile-after-switch-project-hook (lambda () (my/projectile-invalidate-cache nil)))

    (defun my/projectile-invalidate-cache (arg)
      "Remove the current project's files from `projectile-projects-cache'.

  With a prefix argument ARG prompts for the name of the project whose cache
  to invalidate."
      (interactive "P")
      (let ((project-root
             (if arg
                 (completing-read "Remove cache for: " projectile-projects-cache)
               (projectile-project-root))))
        (setq projectile-project-root-cache (make-hash-table :test 'equal))
        (remhash project-root projectile-project-type-cache)
        (remhash project-root projectile-projects-cache)
        (remhash project-root projectile-projects-cache-time)
        (projectile-serialize-cache)
        (when projectile-verbose
          (message "Invalidated Projectile cache for %s."
                   (propertize project-root 'face 'font-lock-keyword-face)))))


    (defun my/projectile-add-known-project (project-root)
      (interactive (list (read-directory-name "Add to known projects: ")))
      (projectile-add-known-project project-root)
      (projectile-cleanup-known-projects))

    (defun my/projectile-show-relative-path ()
      (interactive)
      (when (projectile-project-root)
        (message (substring buffer-file-name (length (projectile-project-root))))))

    (defun my/projectile-add-known-project (project-root)
      (interactive (list (read-directory-name "Add to known projects: ")))
      (projectile-add-known-project project-root)
      (projectile-cleanup-known-projects))

    (defun my/projectile-invalidate-cache (arg)
      "Remove the current project's files from `projectile-projects-cache'.

         With a prefix argument ARG prompts for the name of the project whose cache
         to invalidate."
      (interactive "P")
      (let ((project-root
  	   (if arg
  	       (completing-read "Remove cache for: " projectile-projects-cache)
  	     (projectile-project-root))))
        (setq projectile-project-root-cache (make-hash-table :test 'equal))
        (remhash project-root projectile-project-type-cache)
        (remhash project-root projectile-projects-cache)
        (remhash project-root projectile-projects-cache-time)
        (projectile-serialize-cache)
        (when projectile-verbose
  	(message "Invalidated Projectile cache for %s."
  		 (propertize project-root 'face 'font-lock-keyword-face)))))

    (projectile-mode 1))

  (use-package counsel-projectile
    :after (projectile ivy counsel)
    :demand t
    :bind (("C-x C-r" . my/recentf)
  	 ("C-x C-b" .
  	  counsel-projectile-switch-to-buffer))
    :config
    (counsel-projectile-mode 1))

  (defun my/recentf ()
    (interactive)
    (if (projectile-project-root)
        (projectile-recentf)
      (counsel-recent)))

  (setq-default tab-width 2)
  (setq sh-basic-offset tab-width)
  (setq c-basic-offset tab-width)
  (setq cperl-indent-level tab-width)
  (setq sgml-basic-offset tab-width)

  (use-package rainbow-mode
    :demand t
    :diminish rainbow-mode
    :config
    (add-to-list 'rainbow-html-colors-major-mode-list 'rjsx-mode)
    (add-to-list 'rainbow-html-colors-major-mode-list 'css-mode))

  ;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
  ;; Install grammars one by one with: M-x treesit-install-language-grammar
  ;; or all at once: (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
  (use-package treesit
    ;; :disabled t
    :straight nil
    :custom
    (treesit-language-source-alist
     '(
       ;; (bash       "https://github.com/tree-sitter/tree-sitter-bash")
       ;; (c          "https://github.com/tree-sitter/tree-sitter-c/" "master" "src")
       ;; (clojure    "https://github.com/sogaiu/tree-sitter-clojure" "master" "src")
       ;; (cpp        "https://github.com/tree-sitter/tree-sitter-cpp/" "master" "src")
       ;; (cmake      "https://github.com/uyha/tree-sitter-cmake")
       ;; (css        "https://github.com/tree-sitter/tree-sitter-css")
       ;; (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "main" "src")
       ;; (elisp      "https://github.com/Wilfred/tree-sitter-elisp")
       ;; (elixir     "https://github.com/elixir-lang/tree-sitter-elixir" "main" "src")
       ;; (erlang     "https://github.com/WhatsApp/tree-sitter-erlang" "main" "src")
       ;; (go         "https://github.com/tree-sitter/tree-sitter-go")
       ;; (haskell    "https://github.com/tree-sitter/tree-sitter-haskell" "master" "src")
       ;; (html       "https://github.com/tree-sitter/tree-sitter-html")
       ;; (java       "https://github.com/tree-sitter/tree-sitter-java" "master" "src")
       ;; (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
       ;; (json       "https://github.com/tree-sitter/tree-sitter-json")
       ;; (julia      "https://github.com/tree-sitter/tree-sitter-julia" "master" "src")
       ;; (lua        "https://github.com/MunifTanjim/tree-sitter-lua" "main" "src")
       ;; (make       "https://github.com/alemuller/tree-sitter-make")
       ;; (markdown   "https://github.com/ikatyang/tree-sitter-markdown")
       ;; (meson      "https://github.com/Decodetalkers/tree-sitter-meson" "master" "src")
       ;; (python     "https://github.com/tree-sitter/tree-sitter-python")
       ;; (ruby       "https://github.com/tree-sitter/tree-sitter-ruby" "master" "src")
       ;; (rust       "https://github.com/tree-sitter/tree-sitter-rust" "master" "src")
       ;; (toml       "https://github.com/tree-sitter/tree-sitter-toml")
       (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
       (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
       ;; (yaml       "https://github.com/ikatyang/tree-sitter-yaml")
       ))
    ;; :config
    ;; (add-to-list 'treesit-major-mode-language-alist '(tsx-ts-mode . typescript))
    ;; (major-mode-remap-alist
    ;;  '((yaml-mode . yaml-ts-mode)
    ;;    (bash-mode . bash-ts-mode)
    ;;    (js2-mode . js-ts-mode)
    ;;    (json-mode . json-ts-mode)
    ;;    (css-mode . css-ts-mode)
    ;;    (python-mode . python-ts-mode)))
)

  (use-package eglot
    :defer 1
    :straight nil
    :commands (eglot eglot-ensure eglot-alternatives)
    :custom
    (eglot-extend-to-xref t)
    ;; (eglot-events-buffer-size 100000)
    (eglot-events-buffer-size 0)
    (read-process-output-max (* 1024 1024)) ;; 1mb
    ;; (gc-cons-threshold 100000000)
    (eglot-ignored-server-capabilities '(:documentHighlightProvider :workspace/didChangeWorkspaceFolders))
    (eglot-autoshutdown t)
    :config
    (add-to-list 'eglot-ignored-server-capabilites :hoverProvider))

  (use-package dtrt-indent
    :disabled t
    :commands (dtrt-indent-mode my/dtrt-indent-mode-toggle)
    :diminish "dtrt"
    :preface
    (defun my/dtrt-indent-mode-toggle ()
      "Toggle dtrt-indent mode."
      (interactive)
      (if (eq dtrt-indent-mode t)
          (dtrt-indent-mode -1)
        (dtrt-indent-mode 1))))

  ;; https://stackoverflow.com/a/750933
  (defun my/remove-dos-eol ()
    "Do not show ^M in files containing mixed UNIX and DOS line endings."
    (interactive)
    (setq buffer-display-table (make-display-table))
    (aset buffer-display-table ?\^M []))

  (defvar my/flip-symbol-alist
    '(("true" . "false")
      ("false" . "true"))
    "symbols to be quick flipped when editing")

  (defun my/flip-symbol-at-point ()
    "Flip symbol at point."
    (interactive)
    (-let* ((symbol (symbol-at-point))
  	  (symbol (when symbol (symbol-name symbol)))
  	  ((beg . end) (bounds-of-thing-at-point 'symbol)))
  	 (if (assoc symbol my/flip-symbol-alist)
  	     (progn
  	       (delete-region beg end)
  	       (insert (alist-get symbol my/flip-symbol-alist "" nil 'equal)))
  	   (message "Nothing to flip here"))))

  (defun my/xref-find-references-at-point ()
    (interactive)
    (if lsp-managed-mode
        (funcall-interactively 'lsp-ui-peek-find-references)
      (let* ((symbol (symbol-at-point))
             (symbols-names (mapcar 'symbol-name (apropos-internal ".*")))
             (symbol (if (symbol-function symbol) (symbol-name symbol) ""))
             (symbol (if (and (not (string-empty-p symbol)) (seq-some (lambda (i) (string-match-p (regexp-quote symbol) i)) symbols-names)) symbol ""))
             (function-name
              (funcall
               completing-read-function
               "Find references to: "
               symbols-names
               'commandp
               t
               symbol)))
        (funcall-interactively 'xref-find-references function-name))))

  (defun my/xref-find-definitions-at-point ()
    (interactive)
    (if lsp-managed-mode
        (funcall-interactively 'lsp-ui-peek-find-definitions)
      (let* ((symbol (symbol-at-point))
             (symbols-names (mapcar 'symbol-name (apropos-internal ".*")))
             (symbol (if (symbol-function symbol) (symbol-name symbol) ""))
             (symbol (if (and (not (string-empty-p symbol)) (seq-some (lambda (i) (string-match-p (regexp-quote symbol) i)) symbols-names)) symbol ""))
             (function-name
              (funcall
               completing-read-function
               "Find definitions of: "
               symbols-names
               'commandp
               t
               symbol)))
        (funcall-interactively 'xref-find-definitions function-name))))

  (use-package highlight-thing
    :hook ((prog-mode . highlight-thing-mode))
    :custom
    (highlight-thing-delay-seconds 0.1)
    (highlight-thing-case-sensitive-p t)
    (highlight-thing-ignore-list '("False" "True"))
    (highlight-thing-limit-to-region-in-large-buffers-p nil)
    (highlight-thing-narrow-region-lines 15)
    (highlight-thing-large-buffer-limit 5000)
    (highlight-thing-exclude-thing-under-point t))

  (use-package eldoc
    :straight nil
    :commands eldoc-mode
    :diminish eldoc-mode
    :custom
    (eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
    (eldoc-echo-area-use-multiline-p t)
    ;; :config
    ;; Show all of the available eldoc information when we want it. This way Flymake errors
    ;; don't just get clobbered by docstrings.
    ;; (add-hook 'eglot-managed-mode-hook
    ;;   (lambda ()
    ;;     "Make sure Eldoc will show us all of the feedback at point."
    ;;     (setq-local eldoc-documentation-strategy
    ;;       #'eldoc-documentation-compose)))
    )

  (use-package lorem-ipsum
    :commands (lorem-ipsum-insert-sentences lorem-ipsum-insert-paragraphs lorem-ipsum-insert-list))

  (use-package hl-todo
    :hook ((prog-mode . hl-todo-mode)))

  (use-package topsy
    :hook ((prog-mode . topsy-mode)
           (magit-section-mode . topsy-mode)))

  (use-package rainbow-delimiters
    :commands rainbow-delimiters-mode)

  (use-package prog-mode
    :straight nil
    :hook (prog-mode . rainbow-delimiters-mode)
    :bind (("C-c (" . my/wrap-with-parens)
           ("C-c \"" . my/wrap-with-quotes))
    :config
    (defun my/prog-mode-hook ()
      (modify-syntax-entry ?- "w")
      (modify-syntax-entry ?_ "w")
      (modify-syntax-entry ?$ "w"))
    (add-hook 'prog-mode-hook #'my/prog-mode-hook -50)

    (defun my/wrap-with-parens (start end)
      "Wrap region (or char at point) with parentheses."
      (interactive "r")
      (if (use-region-p)
          (progn
            (goto-char end)
            (insert ")")
            (goto-char start)
            (insert "("))
        ;; no region: wrap around char at point
        (let ((c (char-after)))
          (when c
            (save-excursion
              (insert ")")
              (backward-char)
              (insert "("))))))

    (defun my/wrap-with-quotes (start end)
      "Wrap region (or char at point) with quotes."
      (interactive "r")
      (if (use-region-p)
          (progn
            (goto-char end)
            (insert "\"")
            (goto-char start)
            (insert "\""))
        ;; no region: wrap around char at point
        (let ((c (char-after)))
          (when c
            (save-excursion
              (insert "\"")
              (backward-char)
              (insert "\"")))))))

  ;; (use-package smart-comment
  ;; :bind ("M-;" . smart-comment))

  (use-package flycheck
    :defer 2
    :commands (flycheck-mode flycheck-buffer)
    :custom
    (flymake-phpcs-show-rule t)
    (flycheck-display-errors-delay .3)
    (flycheck-phpcs-standard "WordPress")
    :config
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc emacs-lisp html-tidy))
    ;; (global-flycheck-mode)
    ;; (add-to-list 'display-buffer-alist
    ;;   `(,(rx bos "*Flycheck errors*" eos)
    ;;      (display-buffer-reuse-window
    ;;        display-buffer-in-side-window)
    ;;      (side            . bottom)
    ;;      (reusable-frames . visible)
    ;;      (window-height   . 0.33)))
    )

  (use-package treemacs
    :commands (treeemacs treemacs-current-visibility)
    :custom
    (treemacs-default-visit-action #'treemacs-visit-node-in-most-recently-used-window)
    (treemacs-project-follow-cleanup t)
    :config
    (defun my/treemacs-project-toggle ()
      "Toggle treemacs for current project."
      (interactive)
      (if (string-equal (treemacs-current-visibility) "visible")
          (treemacs)
        (treemacs-add-and-display-current-project-exclusively)))

    (treemacs-follow-mode 1)
    (treemacs-project-follow-mode nil)
    (treemacs-filewatch-mode 1)
    (treemacs-git-mode 'simple)
    (treemacs-resize-icons 22)
    (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

  (use-package treemacs-projectile
    :demand t
    :after (treemacs projectile))

  (use-package treemacs-magit
    :demand t
    :after (treemacs magit))

  ;; Folding code blocks based on indentation.
  (use-package yafolding
    :hook (prog-mode . (lambda () (yafolding-mode 1))))

  (use-package nxml-mode
    :straight nil
    :hook (
           (nxml-mode . eldoc-mode)
           (nxml-mode . rainbow-delimiters-mode)
           (nxml-mode . highlight-thing-mode)
           (nxml-mode . electric-pair-local-mode)
           (nxml-mode . topsy-mode)
           (nxml-mode . emmet-mode)))

  (use-package markdown-mode
    :mode (("\\.markdown\\'" . markdown-mode)
           ("\\.mdx?\\'" . markdown-mode)
           ("README\\.md\\'" . gfm-mode))
    :hook ((markdown-mode . abbrev-mode)
           (markdown-mode . my/markdown-mode-init)
           (gfm-mode . my/markdown-mode-init))
    :preface
    (defun my/markdown-mode-init ()
      ""
      (if (executable-find "vscode-html-language-server")
          (add-to-list 'eglot-server-programs '(markdown-mode . ("vscode-markdown-language-server")))
        (error "https://github.com/hrsh7th/vscode-langservers-extracted is not installed, HTML, JSON, markdown language servers not available!")))
    :config
    (advice-add 'markdown-backward-paragraph :override #'backward-paragraph)
    (advice-add 'markdown-backward-block :override #'backward-paragraph)
    (advice-add 'markdown-forward-paragraph :override #'forward-paragraph)
    (advice-add 'markdown-forward-bblock :override #'forward-paragraph))

  (use-package sh-script
    :straight nil
    :hook (after-save . executable-make-buffer-file-executable-if-script-p)
    :mode (("\\.bashrc\\'" . sh-mode)
           ("\\.bash_logout\\'" . sh-mode)
           ("\\.sh\\'" . sh-mode)
           ("\\.z?sh\\'" . sh-mode)
           ("\\.profile\\'" . sh-mode)
           ("\\config.fish\\'" . sh-mode)))

  (use-package cc-mode
    :straight nil
    :hook (java-mode . eglot-ensure))

  (use-package ruby-mode
    :straight nil
    :mode "\\.rb\\'"
    :hook (ruby-mode . eglot-ensure))

  (use-package vimrc-mode
    :mode "\\vimrc\\'")

  (use-package dockerfile-mode
    :mode "^Dockerfile\\'"
    :config
    (add-to-list 'auto-mode-alist '("^Dockerfile\\'" . dockerfile-mode)))

  (use-package dns-mode
    :straight nil
    :mode ("\\.zone?\\'" . zone-mode))

  (use-package json-mode
    :hook ((json-mode . yafolding-mode)
           (json-mode  . my/json-mode-init))
    :mode ("\\.json\\'")
    :preface
    (defun my/json-mode-init ()
      ""
      (if (executable-find "vscode-html-language-server")
          (add-to-list 'eglot-server-programs '(json-mode . ("vscode-json-language-server" "--stdio")))
        (error "https://github.com/hrsh7th/vscode-langservers-extracted is not installed, HTML, JSON, markdown language servers not available!"))))

  (use-package css-mode
    :straight nil
    :mode "\\.s?css\\'"
    :hook (css-mode . rainbow-mode)
    :custom
    (css-indent-offset tab-width))

(defun my/web-mode-toggle ()
    "Toggle switch between `web-mode' and native major mode."
    (interactive)
    (when (not (boundp 'my/native-local-major-mode))
      (defvar-local my/native-local-major-mode major-mode))
    (if (eq major-mode 'web-mode)
      (funcall my/native-local-major-mode)
      (web-mode)))

  (straight-register-package 'rainbow-mode)
  (straight-register-package 'emmet-mode)
  (straight-register-package 'web-mode)
  (straight-register-package 'web-beautify)
  (straight-register-package 'jade-mode)
  (when my/html-enable

    ;; (use-package edit-color-stamp
    ;; :commands edit-color-stamp)

    (use-package emmet-mode
      :commands emmet-mode
      :diminish emmet-mode
      :custom
      (emmet-self-closing-tag-style " /")
      (emmet-expand-jsx-className? t))

    (use-package web-mode
      :hook ((web-mode . rainbow-mode)
             (web-mode . emmet-mode)
             (web-mode . my/web-mode-init)
             (web-mode . eglot-ensure))
      :bind (:map web-mode-map
                  ("C-c C-n" . web-mode-tag-end)
                  ("C-c C-p" . web-mode-tag-beginning)
                  ("<backtab>" . indent-relative)
                  ("<f5>" . my/toggle-php-flavor-mode))
      :mode ("\\.html\\.twig\\'"
             "\\.hbs\\'"
             "\\.ejs\\'"
             "\\.html?\\'"
             "\\.njk\\'"
             "\\.svg\\'")
      :preface
      (defun my/web-mode-init ()
        ""
        (if (executable-find "vscode-html-language-server")
            (add-to-list 'eglot-server-programs '(web-mode . ("vscode-html-language-server" "--stdio")))
          (error "https://github.com/hrsh7th/vscode-langservers-extracted is not installed, HTML, JSON, markdown language servers not available!")))
      :custom
      (web-mode-engines-alist '(("php" . "\\.php\\'")))
      (web-mode-markup-indent-offset tab-width)
      (web-mode-css-indent-offset tab-width)
      (web-mode-code-indent-offset tab-width)
      (web-mode-attr-indent-offset tab-width)
      (web-mode-block-padding tab-width)
      (web-mode-enable-html-entities-fontification nil)
      (web-mode-enable-block-face nil)
      (web-mode-enable-comment-annotation nil)
      (web-mode-enable-comment-interpolation nil)
      (web-mode-enable-control-block-indentation nil)
      (web-mode-enable-css-colorization nil)
      (web-mode-enable-current-column-highlight nil)
      (web-mode-enable-current-element-highlight nil)
      (web-mode-enable-element-content-fontification nil)
      (web-mode-enable-heredoc-fontification nil)
      (web-mode-enable-inlays nil)
      (web-mode-enable-optional-tags nil)
      (web-mode-enable-part-face nil)
      (web-mode-enable-sexp-functions nil)
      (web-mode-enable-sql-detection nil)
      (web-mode-enable-string-interpolation nil)
      (web-mode-enable-whitespace-fontification nil)
      (web-mode-enable-auto-expanding nil)
      (web-mode-enable-auto-indentation nil)
      (web-mode-enable-auto-closing nil)
      (web-mode-enable-auto-opening nil)
      (web-mode-enable-auto-pairing nil)
      (web-mode-enable-auto-quoting nil)
      (web-mode-content-types-alist
       '(("jsx" . "\\.jsx\\'")
         ("jsx" . "\\.tsx\\'")))
      :config

      ;; (major-mode-hydra-define+ web-mode
      ;;   (:hint nil :color amaranth :quit-key "q" :title (with-faicon "code" "HTML" 1 -0.05))
      ;;   ("Action"
      ;;    (("f" my/html-buffer-format "format buffer" :exit t))
      ;;    (("c" edit-color-stamp "edit color" :exit t))))

      (defun my/html-buffer-format ()
        (interactive)
        (sgml-pretty-print (point-min) (point-max))
        (save-buffer))

      (add-hook 'before-save-hook
                (lambda ()
                  (when (and (fboundp 'web-beautify-html) (eq dtrt-indent-mode nil))
                    (web-beautify-html)))
                0 t))

    (use-package web-beautify
      :commands web-beautify-js web-beautify-css web-beautify-html)

    (use-package jade-mode
      :hook (jade-mode . highlight-thing-mode)
      :mode ("\\.jade\\'" "\\.pug\\'"))

    ;; debugger
    ;; (use-package realgud)
    )

  (straight-register-package 'mmm-mode)
  (straight-register-package 'apheleia)
  (straight-register-package 'js2-mode)
  (straight-register-package 'rjsx-mode)
  (straight-register-package 'typescript-mode)
  (straight-register-package 'prisma-ts-mode)
  (straight-register-package 'graphql-mode)
  (when my/js-enable
    (use-package mmm-mode
      :commands mmm-mode
      ;; :after (rjsx-mode typescript-mode)
      :custom
      (mmm-submode-decoration-level 0)
      :config
      (mmm-add-classes
       '((mmm-styled-mode
          :submode css-mode
          :front "\\(styled\\|css\\)[.()<>[:alnum:]]?+`"
          :back "`;")))
      (mmm-add-mode-ext-class 'typescript-mode nil 'mmm-styled-mode)
      (mmm-add-mode-ext-class 'rjsx-mode nil 'mmm-styled-mode)

      (mmm-add-classes
       '((mmm-graphql-mode
          :submode graphql-mode
          :front "gr?a?p?h?ql`"
          :back "`;")))
      (mmm-add-mode-ext-class 'typescript-mode nil 'mmm-graphql-mode)
      (mmm-add-mode-ext-class 'rjsx-mode nil 'mmm-graphql-mode)

      (add-hook 'mmm-mode-hook
                (lambda () (set-face-background 'mmm-default-submode-face nil)))

      ;; (mmm-add-classes
      ;;   '((mmm-jupyter-python-mode
      ;;       :submode python-mode
      ;;       :front "```python"
      ;;       :back "```")))
      ;; (mmm-add-mode-ext-class 'org-mode nil 'mmm-jupyter-python-mode)

      ;; (mmm-add-classes
      ;;   '((mmm-jsx-mode
      ;;       :front "\\(return\s\\|n\s\\|(\n\s*\\)<"
      ;;       :front-offset -1
      ;;       :back ">\n?\s*)"
      ;;       :back-offset 1
      ;;       :submode web-mode)))
      ;; (mmm-add-mode-ext-class 'typescript-mode nil 'mmm-jsx-mode)
      )

    (setq my/prettier-modes '(css-mode js-mode yaml-mode typescript-mode))

    ;; TODO also "prettier" key in your package.json file.
    (setq my/prettier-config-files
          '(".prettierrc"
            ".prettierrc.json"
            ".prettierrc.yml"
            ".prettierrc.yaml"
            ".prettierrc.json5"
            ".prettierrc.js"
            ".prettierrc.cjs"
            "prettier.config.js"
            "prettier.config.cjs"
            ".prettierrc.toml"))

    (define-minor-mode my/prettier-mode
      "My Prettier mode implementation."
      :lighter " Prettier"
      (if (map-some (lambda (key val) (file-exists-p (concat (projectile-project-root) key))) my/prettier-config-files)
          ;; (prettier-mode 1)
          ;; (prettier-mode -1)
          (apheleia-mode 1)
        (apheleia-mode -1)))

    ;; Prettier support
    (use-package apheleia
      :commands (rjsx-mode js2-mode js-mode apheleia-format-buffer)
      ;; :commands (my/prettier-mode apheleia-format-buffer)
      :diminish apheleia-mode
      :config
      (add-to-list 'apheleia-mode-alist '(rjsx-mode . prettier)))

    (defun my/prettier-format-buffer ()
      "Prettify buffer using Prettier if available"
      (interactive)
      (if (executable-find "prettier")
          (progn
            (call-interactively 'apheleia-format-buffer)
            (message "Buffer prettified"))
        (error "prettier not installed!")))

    (defun my/eglot-organize-imports-ts ()
      (interactive "*")
      (save-excursion
        (let ((min (goto-char (point-min)))
              ;; (max (goto-char (point-max)))
              )
          ;; (eglot-code-actions min max "source.addMissingImports.ts")
          ;; (eglot-code-actions min max "source.removeUnused.ts")
          (eglot-code-action-organize-imports-ts min))))

    ;; Use binaries in node_modules
    (use-package add-node-modules-path
      :demand t)

    (defun my/js-mode-init ()
      ""
      (if (executable-find "typescript-language-server")
          (progn
            ;; (eglot--code-action eglot-code-action-organize-imports "source.organizeImports.ts")
            ;; (eglot--code-action eglot-code-action-organize-imports-ts "source.organizeImports.ts")
            (add-to-list 'eglot-server-programs '(major-mode . ("typescript-language-server" "--stdio")))
            (eglot-ensure))
        (error "https://github.com/hrsh7th/vscode-langservers-extracted is not installed, TypeScript language server not available!")))

    (use-package js
      :straight nil
      :hook ((js-mode . add-node-modules-path)
             (js-mode . my/js-mode-init))
      :custom
      (js-indent-level tab-width)
      (flycheck-disabled-checkers '(javascript-jshint javascript-jscs))
      (js-chain-indent t)
      (js-indent-align-list-continuation nil)
      ;; :config
      ;; (flycheck-add-mode 'javascript-eslint 'js-mode)
      ;; (add-hook 'js-mode-hook
      ;;   (lambda () (unless (eq major-mode 'json-mode) (lsp))))
      )

    ;; (use-package js-ts-mode
    ;;   :straight nil
    ;;   :mode ("\\.m?js\\'" "\\.cjs\\'")
    ;;   :hook (js-ts-mode . lsp-deferred))

    (use-package js2-mode
      ;; :hook (js2-mode . lsp-deferred)
      :hook ((js2-mode . apheleia-mode)
             (js2-mode . rainbow-mode)
             (js2-mode . my/js-mode-init))
      :mode ("\\.m?js\\'" "\\.cjs\\'")
      :diminish "js2"
      :custom
      (js2-mode-show-parse-errors nil)
      (js2-mode-show-strict-warnings nil)
      :config
      (major-mode-hydra-define+ js2-mode
  			      (:hint nil :color amaranth :quit-key "q" :title (with-fileicon "jsx-2" "JS" 1 -0.05))
  			      ("Action"
  			       (
  				("w" my/web-mode-toggle "toggle web-mode" :exit t)
  				("p" my/prettier-format-buffer "prettier buffer" :exit t)
  				("o" my/eglot-organize-imports-ts "organize imports" :exit t)
  				("c" edit-color-stamp "edit color" :exit t))))
      ;; Use js2-mode for Node scripts
      (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode)))

    (use-package rjsx-mode
      :commands rjsx-mode
      :mode ("\\.m?jsx\\'" "\\.tsx?\\'")
      :hook ((rjsx-mode . add-node-modules-path)
             (rjsx-mode . emmet-mode)
             (rjsx-mode . rainbow-mode)
             (rjsx-mode . mmm-mode)
             (rjsx-mode . my/js-mode-init))
      :bind (:map rjsx-mode-map
                  ("<" . rjsx-electric-lt))
      :config
      (major-mode-hydra-define+ rjsx-mode
  			      (:hint nil :color amaranth :quit-key "q" :title (with-fileicon "jsx-2" "JSX" 1 -0.05))
  			      ("Action"
  			       (
  				("w" my/web-mode-toggle "toggle web-mode" :exit t)
  				("p" my/prettier-format-buffer "prettier buffer" :exit t)
  				("o" my/eglot-organize-imports-ts "organize imports" :exit t)
  				("c" edit-color-stamp "edit color" :exit t)))))

    ;; NOTE not maintaned
    ;; based on https://github.com/emacs-typescript/typescript.el/issues/4#issuecomment-873485004
    (use-package typescript-mode
      :disabled t
      ;; :mode ("\\.tsx?\\'" . typescript-mode)
      :commands typescript-mode
      :mode ("\\.cts\\'")
      :hook ((typescript-mode . add-node-modules-path)
             (typescript-mode . mmm-mode)
             (typescript-mode . emmet-mode)
             (typescript-mode . rainbow-mode)
             (typescript-mode . apheleia-mode)
             (typescript-mode . subword-mode)
             (typescript-mode . my/js-mode-init))
      :custom
      (typescript-indent-level 2)
      :config
      ;; (major-mode-hydra-define+ typescript-mode
      ;; (:hint nil :color amaranth :quit-key "q" :title (with-fileicon "typescript" "TSX" 1 -0.05))
      ;; ("Action"
      ;;  (
      ;;   ("w" my/web-mode-toggle "toggle web-mode" :exit t)
      ;;   ("p" my/prettier-format-buffer "prettier buffer" :exit t)
      ;;   ("o" my/eglot-organize-imports-ts "organize imports" :exit t)
      ;; ("c" edit-color-stamp "edit color" :exit t))))

      (setq auto-mode-alist (delete '("\\.tsx?\\'" . typescript-mode) auto-mode-alist)))

    (use-package typescript-ts-mode
      ;; :disabled t
      :straight nil
      :mode (("\\.ts\\'" . typescript-ts-mode)
             ;; ("\\.tsx\\'" . tsx-ts-mode)
             )
      :hook ((typescript-ts-mode . my/js-mode-init)
             ;; (tsx-ts-mode . mmm-mode)
             ;; (tsx-ts-mode . emmet-mode)
             ;; (tsx-ts-mode . my/js-mode-init)
             )
      ;; :config
      ;; (setq auto-mode-alist (remove '("\\.tsx\\'" . tsx-ts-mode) auto-mode-alist))
      ;; (setq auto-mode-alist (remove '("\\.tsx\\'" . tsx-ts-mode) auto-mode-alist))
      ;; (major-mode-hydra-define+ typescript-ts-mode
      ;;   (:hint nil :color amaranth :quit-key "q" :title (with-fileicon "typescript" "Typescript" 1 -0.05))
      ;;   ("Action"
      ;;    (
      ;;     ("w" my/web-mode-toggle "toggle web-mode" :exit t)
      ;;     ("p" my/prettier-format-buffer "prettier buffer" :exit t)
      ;;     ("o" my/eglot-organize-imports-ts "organize imports" :exit t)
      ;; ("c" edit-color-stamp "edit color" :exit t))))
      )

    (use-package prisma-ts-mode
      :disabled t
      :mode "\\.prisma\\'")

    (use-package graphql-mode
      :disabled t
      :commands graphql-mode
      :mode "\\.graphql\\'"
      :custom
      (graphql-indent-level tab-width))

    ;; (use-package json-reformat)
    )

  (straight-register-package 'elpy)
  (straight-register-package 'conda)
  (when my/python-enable
  ;;; TODO install Python formatter
    ;; https://github.com/pythonic-emacs/blacken

    ;; (setq gud-pdb-command-name "python3 -m pdb ")

    (use-package python
      :straight nil
      :hook (python-mode . my/python-init)
      :preface
      (defun my/python-init ()
        (setq-local tab-width 4)
        (setq-local python-indent-offset 4)

        (if (executable-find "ipython")
            (progn
              (python-shell-interpreter "ipython")
              (python-shell-interpreter-args "-i --simple-prompt"))
          (error "ipython not found!"))

        (if (executable-find "pyright-langserver")
            (progn
              (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
              (eglot-ensure))
          (error "pyright-langserver not found!")))
      :config
      (require 'comint)
      (add-to-list 'comint-output-filter-functions 'python-pdbtrack-comint-output-filter-function)
      (add-to-list 'comint-preoutput-filter-functions  'python-pdbtrack-comint-output-filter-function))

    ;; On Debian it requires python3-venv apt package, than run elpy-rpc-reinstall-virtualenv
    (use-package elpy
      :disabled t
      :hook (python-mode . elpy-enable)
      :bind (:map elpy-mode-map
                  ("C-c C-l" . elpy-occur-definitions)
                  ("C-c C-e" . elpy-multiedit-python-symbol-at-point)
                  ("C-c C-r f" . elpy-format-code)
                  ("C-c C-r r" . elpy-refactor))
      :custom
      (elpy-shell-echo-output nil)
      ;; (elpy-rpc-backend "jedi")
      (elpy-rpc-python-command "python")
      (elpy-rpc-timeout 2)
      :config
      (evil-define-key '(normal motion visual) elpy-mode-map
                       (kbd "M-.") #'xref-find-definitions
                       (kbd "M-,") #'xref-pop-marker-stack)

      (setq
       elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules)
       elpy-modules (delq 'elpy-module-flymake elpy-modules))

      ;; (setq
      ;; python-shell-interpreter "python"
      ;; python-shell-interpreter-args "-i")

      ;; (elpy-enable)
      ;; (add-hook 'elpy-mode-hook 'flycheck-mode)

      (advice-add 'keyboard-quit :before #'elpy-multiedit-stop)

      ;; https://www.thedigitalcatonline.com/blog/2020/07/18/emacs-configuration-for-python-javascript-terraform-and-blogging/
      ;; Prevent Elpy from overriding Windmove shortcuts
      ;; (eval-after-load "elpy"
      ;;   '(cl-dolist (key '("M-<up>" "M-<down>" "M-<left>" "M-<right>"))
      ;;      (define-key elpy-mode-map (kbd key) nil)))

      ;; Prevent Elpy from overriding standard cursor movements
      ;; (eval-after-load "elpy"
      ;;   '(cl-dolist (key '("C-<left>" "C-<right>"))
      ;;      (define-key elpy-mode-map (kbd key) nil)))

      (if (executable-find "black")
          (add-hook 'elpy-mode-hook
                    (lambda () (add-hook 'before-save-hook 'elpy-black-fix-code nil t))))
      (error "No executable 'black' found!"))

    (use-package conda
      :disabled t
      :custom
      (conda-anaconda-home "/home/user/miniforge3/")
      :config
      ;; if you want interactive shell support, include:
      (conda-env-initialize-interactive-shells)
      ;; if you want eshell support, include:
      (conda-env-initialize-eshell)
      ;; if you want auto-activation (see below for details), include:
      (conda-env-autoactivate-mode t)
      ;; if you want to automatically activate a conda environment on the opening of a file:
      (add-hook 'find-file-hook (lambda () (when (bound-and-true-p conda-project-env-path))))
      (conda-env-activate-for-buffer))

    ;; (use-package guess-style
    ;; :config
    ;; (progn
    ;; (add-hook 'python-mode-hook 'guess-style-guess-tabs-mode)))

    ;; (use-package code-cells)
    )

(when my/go-enable
  (use-package go-mode
    :disabled t
    :mode ("\\.thtml\\'" "\\.gohtml\\'" "\\.tm?pl\\'")))

  (straight-register-package 'php-mode)
  (when my/php-enable
    (defun my/toggle-php-flavor-mode ()
      "Toggle mode between PHP & Web-Mode Helper modes."
      (interactive)
      (cond ((string= major-mode "php-mode")
             (web-mode))
            ((string= major-mode "web-mode")
             (php-mode))))

    (use-package php-mode
      :hook ((php-mode . emmet-mode)
             (php-mode . my/php-mode-init))
      :bind (:map php-mode-map
                  ("<f5>" . my/toggle-php-flavor-mode))
      :mode ("\\.php\\'" "\\.inc\\'" "\\.tpl\\.php\\'" "\\.phtml\\'")
      :config
      (defun ywb-php-lineup-arglist-intro (langelem)
        (save-excursion
          (goto-char (cdr langelem))
          (vector (+ (current-column) c-basic-offset))))

      (defun ywb-php-lineup-arglist-close (langelem)
        (save-excursion
          (goto-char (cdr langelem))
          (vector (current-column))))

      (defun my/php-mode-init ()
        (setq php-template-compatibility nil)
        (make-local-variable 'web-mode-code-indent-offset)
        (make-local-variable 'web-mode-markup-indent-offset)
        (make-local-variable 'web-mode-css-indent-offset)
        (c-set-offset 'arglist-intro 'ywb-php-lineup-arglist-intro)
        (c-set-offset 'arglist-close 'ywb-php-lineup-arglist-close)
        ;; https://phpactor.readthedocs.io/en/master/usage/standalone.html
        (add-to-list 'eglot-server-programs '(php-mode . ("phpactor" "language-server"))) (eglot-ensure))

      (defun my-php-symbol-lookup ()
        (interactive)
        (let ((symbol (symbol-at-point)))
          (if (not symbol)
              (message "No symbol at point.")
            (browse-url (concat "http://php.net/manual-lookup.php?pattern=" (symbol-name symbol))))))))

  (straight-register-package 'kotlin-mode)
  (when my/kotlin-enabled
    (use-package kotlin-mode
      :hook (kotlin-mode . my/kotlin-mode-init)
      :preface
      (defun my/kotlin-mode-init ()
        ""
        (if (executable-find "kotlin-language-server")
            (progn
              (setq-local eglot-connect-timeout 999999) ;; because kotlin-mode
              (add-to-list 'eglot-server-programs `(kotlin-mode . ("kotlin-language-server" :initializationOptions (:storagePath "/tmp"))))
              (eglot-ensure))
          (error "kotlin-language-server not found!")))))

  (load-theme 'modus-vivendi t)

  (use-package doom-modeline
    :disabled t
    :demand t
    :custom
    (doom-modeline-icon t)
    :init
    (setq doom-modeline-minor-modes t
  	doom-modeline-vcs-max-length 20)
    :config
    (doom-modeline-mode 1))

  (use-package doom-themes
    :disabled t
    :demand t
    :init
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
  	doom-themes-enable-italic t) ; if nil, italics is universally disabled
    :config
    ;; (load-theme 'doom-1337 t)
    ;; (load-theme 'doom-one t)
    ;; (load-theme 'doom-material t)
    ;; (load-theme 'doom-oceanic-next t)
    ;; (load-theme 'doom-opera t)
    ;;   (load-theme 'doom-palenight t)
    ;; (load-theme 'doom-peacock t)

    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)

    ;; use the colorful treemacs theme
    ;; (use-package doom-themes-ext-treemacs
    ;;      :after treemacs
    ;; :commands treemacs-mode
    ;; :custom
    ;; (doom-themes-treemacs-theme "doom-colors")
    ;; :config
    ;; (doom-themes-treemacs-config))

    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config))

  (setq shell-dirtrackp nil)

  (defalias 'sh #'eshell)

  (use-package eshell
    :straight nil
    :commands eshell
    :bind (:map eshell-mode-map
  	      ("C-r" . counsel-shell-history)
  	      ;; ("C-n" . company-next-page)
  	      ;; ("C-p" . company-previous-page)
  	      ("<tab>" . my/eshell-list-dir)
  	      ("<tab>" . company-next-page)
  	      ;; ("<backtab>" . company-previous-page)
  	      )
    ;; :hook ((eshell-mode . company-mode))
    :init
    (require 'esh-mode)
    :custom
    (eshell-destroy-buffer-when-process-dies t)
    (eshell-prompt-function #'dw/eshell-prompt)
    (eshell-prompt-regexp "^λ ")
    (eshell-history-size 10000)
    (eshell-buffer-maximum-lines 10000)
    (eshell-hist-ignoredups t)
    (eshell-highlight-prompt t)
    (eshell-scroll-to-bottom-on-input t)
    (eshell-prefer-lisp-functions nil)
    :config
    (defun my/eshell-init ()
      ;; (setq-local company-backends '((company-files company-capf)))
      ;; (setq-local company-backends '(esh-autosuggest company-files ))
      (setenv "PAGER" "cat"))
    (add-hook 'eshell-mode-hook #'my/eshell-init)

    (defun read-file (file-path)
      (with-temp-buffer
        (insert-file-contents file-path)
        (buffer-string)))

    (defun my/eshell-list-dir ()
      (interactive)
      (let ((output (eshell-command "ls -a" t)))
        (with-temp-message output)))

    (defun dw/get-current-package-version ()
      (interactive)
      (let ((package-json-file (concat (eshell/pwd) "/package.json")))
        (when (file-exists-p package-json-file)
  	(let* ((package-json-contents (read-file package-json-file))
  	       (package-json (ignore-errors (json-parse-string package-json-contents))))
  	  (when package-json
  	    (ignore-errors (gethash "version" package-json)))))))

    (defun dw/map-line-to-status-char (line)
      (cond ((string-match "^?\\? " line) "?")))

    (defun dw/get-git-status-prompt ()
      (let ((status-lines (cdr (process-lines "git" "status" "--porcelain" "-b"))))
        (seq-uniq (seq-filter 'identity (mapcar 'dw/map-line-to-status-char status-lines)))))

    (defun dw/get-prompt-path ()
      (let* ((current-path (eshell/pwd))
  	   (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
  	   (has-path (not (string-match "^fatal" git-output))))
        (if (not has-path)
  	  (abbreviate-file-name current-path)
  	(string-remove-prefix (file-name-directory git-output) current-path))))

    ;; This prompt function mostly replicates my custom zsh prompt setup
    ;; that is powered by github.com/denysdovhan/spaceship-prompt.
    (defun dw/eshell-prompt ()
      (let ((current-branch (magit-get-current-branch))
  	  (package-version (dw/get-current-package-version)))
        (concat
         "\n"
         (propertize (system-name) 'face `(:foreground "#62aeed"))
         (propertize " ॐ " 'face `(:foreground "white"))
         (propertize (dw/get-prompt-path) 'face `(:foreground "#82cfd3"))
         (when current-branch
  	 (concat
  	  (propertize " • " 'face `(:foreground "white"))
  	  (propertize (concat " " current-branch) 'face `(:foreground "#c475f0"))))
         (propertize " " 'face `(:foreground "white")))))

    (use-package xterm-color
      :demand t)

    (push 'eshell-tramp eshell-modules-list)
    ;; (push 'xterm-color-filter eshell-preoutput-filter-functions)
    ;; (delq 'eshell-handle-ansi-color eshell-output-filter-functions)

    ;; (defun flush-func (&optional args) t)

    ;; Save command history when commands are entered
    (add-hook 'eshell-pre-command-hook #'eshell-save-some-history)

    (add-hook 'eshell-before-prompt-hook
  	    (lambda ()
  	      (setq-local xterm-color-preserve-properties t)))

    ;; Truncate buffer for performance
    ;; (add-to-list 'eshell-output-filter-functions #'eshell-truncate-buffer)

    ;; We want to use xterm-256color when running interactive commands
    ;; in eshell but not during other times when we might be launching
    ;; a shell command to gather its output.
    (add-hook 'eshell-pre-command-hook
  	    (lambda () (setenv "TERM" "xterm-256color")))
    (add-hook 'eshell-post-command-hook
  	    (lambda () (setenv "TERM" "dumb"))))

  (use-package eshell-syntax-highlighting
    :demand t
    :after eshell
    :config
    (eshell-syntax-highlighting-global-mode 1))

  ;; (use-package esh-autosuggest
  ;;   :commands esh-autosuggest-mode
  ;;   :custom
  ;;   (esh-autosuggest-delay 0.5)
  ;;   :config
  ;;   (set-face-foreground 'company-preview-common "#4b5668")
  ;;   (set-face-background 'company-preview nil))

  (use-package eshell-toggle
    :after eshell
    :commands eshell-toggle
    ;; :bind (:map evil-normal-state-map
    ;;         ("C-`" . eshell-toggle)
    ;;        :map eshell-mode-map
    ;;         ("C-`" . eshell-toggle))
    ;; :straight (:type: git
    ;;             :host github
    ;;             :repo "4DA/eshell-toggle"
    ;;             :branch "master")
    :custom
    (eshell-toggle-size-fraction 3)
    (eshell-toggle-use-projectile-root t)
    (eshell-toggle-run-command nil)
    (eshell-toggle-default-directory "~/"))


  ;; Kill shell buffer when shell exits
  (use-package shell
    :straight nil
    :hook (shell-mode . my-shell-mode-hook-func)
    :config
    (defun my-shell-mode-hook-func ()
      (set-process-sentinel (get-buffer-process (current-buffer))
  			  'my-shell-mode-kill-buffer-on-exit))
    (defun my-shell-mode-kill-buffer-on-exit (process state)
      (message state)
      (if (or
  	 (string-match "exited abnormally with code.*" state)
  	 (string-match "finished" state))
  	(kill-buffer (current-buffer))))

    ;; default shell buffer
    (setq explicit-shell-file-name (getenv "SHELL"))
    ;; use shell-file-name for subprocesses
    (setq shell-file-name explicit-shell-file-name))

  ;; Eat: Emulate A Terminal
  (use-package eat
    :demand t
    :custom
    (eat-term-name "xterm")
    :config
    (eat-eshell-mode)                     ; use Eat to handle term codes in program output
    (eat-eshell-visual-command-mode))     ; commands like less will be handled by Eat

  (use-package tramp
    :demand t
    :straight nil
    :custom
    (tramp-default-method "ssh")
    (tramp-inline-compress-start-size 40960)
    (tramp-chunksize 500)
    (tramp-auto-save-directory "~/.emacs.d/tramp-autosaves/")
    (tramp-persistency-file-name  "~/.emacs.d/tramp-persistency.el")
    (tramp-encoding-shell "/bin/sh"))

  (use-package counsel-tramp
    :after (counsel tramp)
    :commands counsel-tramp)

  (defvar my/en-abbrevs nil)
  (define-abbrev-table
    'my/en-abbrevs '(
                     ("aint" "ain't" nil 0)
                     ("ami" "am I" nil 0)
                     ("arent" "aren't" nil 0)
                     ("cant" "can't" nil 0)
                     ("chinese" "Chinese" nil 0)
                     ("couldnt" "couldn't" nil 0)
                     ("didnt" "didn't" nil 0)
                     ("didt" "didn't" nil 0)
                     ("doesnt" "doesn't" nil 0)
                     ("dont" "don't" nil 0)
                     ("elses" "else's" nil 0)
                     ("exceause" "excuse" nil 0)
                     ("exceauses" "excuses" nil 0)
                     ("everobodys" "everybody's" nil 0)
                     ("hasnt" "hasn't" nil 0)
                     ("hast" "hasn't" nil 0)
                     ("ive" "I've" nil 0)
                     ("ihave" "I've" nil 0)
                     ("im" "I'm" nil 0)
                     ("inteligence" "intelligence" nil 0)
                     ("isnt" "isn't" nil 0)
                     ("Iwould" "I'd" nil 0)
                     ("Iwoudl" "I'd" nil 0)
                     ("ill" "I'll" nil 0)
                     ("interuption" "interruption" nil 0)
                     ("interuptions" "interruptions" nil 0)
                     ("iwill" "I'll" nil 0)
                     ("havent" "haven't" nil 0)
                     ("hows" "how's" nil 0)
                     ("youre" "you're" nil 0)
                     ("youare" "you're" nil 0)
                     ("youd" "you'd" nil 0)
                     ("youve" "you've" nil 0)
                     ("youll" "you'll" nil 0)
                     ("youwll" "you'll" nil 0)
                     ("mustnt" "mustn't" nil 0)
                     ("ppl" "people" nil 0)
                     ("Ppl" "People" nil 0)
                     ("prefered" "preferred" nil 0)
                     ("shouldnt" "shouldn't" nil 0)
                     ("shouldt" "shouldn't" nil 0)
                     ("shouldve" "should've" nil 0)
                     ("shes" "she's" nil 0)
                     ("shewill" "she'll" nil 0)
                     ("sheill" "she'll" nil 0)
                     ("wasnt" "wasn't" nil 0)
                     ("weare" "we're" nil 0)
                     ("wewill" "we'll" nil 0)
                     ("weill" "we'll" nil 0)
                     ("whatdo" "what'd" nil 0)
                     ("whats" "what's" nil 0)
                     ("whos" "who's" nil 0)
                     ("wont" "won't" nil 0)
                     ("wouldnt" "wouldn't" nil 0)
                     ("wouldve" "would've" nil 0)
                     ("wouldhave" "would've" nil 0)
                     ("thats" "that's" nil 0)
                     ("theres" "there's" nil 0)
                     ("therell" "there'll" nil 0)
                     ("therell" "there'll" nil 0)
                     ("theyre" "they're" nil 0)
                     ("totaly" "totally" nil 0)
                     ("readonly" "read-only" nil 0)

                     ("indexeddb" "IndexedDB" nil 0)
                     ("fomo" "FOMO" nil 0)
                     ("shopify" "Shopify" nil 0)
                     ("sms" "SMS" nil 0)
                     ("ios" "iOS" nil 0)
                     ("IOS" "iOS" nil 0)
                     ("android" "Android" nil 0)
                     ("google" "Google" nil 0)
                     ("docker" "Docker" nil 0)
                     ("sqlite" "SQLite" nil 0)
                     ("postgresql" "PostgreSQL" nil 0)
                     ("graphql" "GraphQL" nil 0)
                     ("linkedin" "LinkedIn" nil 0)
                     ("skype" "Skype" nil 0)
                     ("dns" "DNS" nil 0)
                     ("cloudflare" "Cloudflare" nil 0)
                     ("svg" "SVG" nil 0)
                     ("tlds" "TLDs" nil 0)
                     ("tld" "TLD" nil 0)
                     ("hackernews" "HackerNews" nil 0)
                     ("thunderbird" "Thunderbird" nil 0)
                     ("firefox" "Firefox" nil 0)
                     ("twitter" "Twitter" nil 0)
                     ("nextjs" "Next.js" nil 0)
                     ("mongodb" "MongoDB" nil 0)
                     ("expressjs" "Express.js" nil 0)
                     ("nodejs" "NodeJS" nil 0)
                     ("css" "CSS" nil 0)
                     ("wordpress" "Wordpress" nil 0)
                     ("debian" "Debian" nil 0)
                     ("github" "GitHub" nil 0)
                     ("coo" "COO" nil 0)
                     ("cfo" "CFO" nil 0)
                     ("ceo" "CEO" nil 0)
                     ("dao" "DAO" nil 0)
                     ("nft" "NFT" nil 0)
                     ("FF" "FireFox" nil 0)
                     ("ff" "FireFox" nil 0)
                     ("YT" "YouTube" nil 0)
                     ("FB" "Facebook" nil 0)
                     ("fb" "Facebook" nil 0)
                     ("IG" "Instagram" nil 0)
                     ("ig" "Instagram" nil 0)
                     ("facebook" "Facebook" nil 0)
                     ("telegram" "Telegram" nil 0)
                     ("youtube" "YouTube" nil 0)
                     ("BT" "Bluetooth" nil 0)
                     ("macos" "MacOS" nil 0)
                     ("seo" "SEO" nil 0)
                     ("irc" "IRC" nil 0)
                     ("rss" "RSS" nil 0)
                     ("url" "URL" nil 0)
                     ("crm" "CRM" nil 0)
                     ("erp" "ERP" nil 0)
                     ("b2b" "B2B" nil 0)
                     ("b2c" "B2C" nil 0)
                     ("btc" "BTC" nil 0)
                     ("eth" "ETH" nil 0)
                     ("arb" "ARB" nil 0)
                     ("matic" "MATIC" nil 0)
                     ("cms" "CMS" nil 0)
                     ("sim" "SIM" nil 0)
                     ("voip" "VoIP" nil 0)
                     ("linux" "Linux" nil 0)
                     ("ascii" "ASCII" nil 0)
                     ("twitch" "Twitch" nil 0)
                     ("airbnb" "AirBnB" nil 0)
                     ("json" "JSON" nil 0)
                     ("ai" "AI" nil 0)
                     ("chatgpt" "ChatGPT" nil 0)
                     ("asap" "ASAP" nil 0)
                     ("emacs" "Emacs" nil 0)
                     ("termux" "Termux" nil 0)
                     ("whatsup" "WhatsApp" nil 0)
                     ("whatsapp" "WhatsApp" nil 0)
                     ("whatapp" "WhatsApp" nil 0)
                     ("email" "e-mail" nil 0)
                     ("emails" "e-mails" nil 0)
                     ("ui" "UI" nil 0)
                     ("gui" "GUI" nil 0)
                     ("todo" "TODO" nil 0)
                     ) nil :case-fixed nil)

  (defvar my/pl-abbrevs nil)
  (define-abbrev-table
    'my/pl-abbrevs '(
                     ("indexeddb" "IndexedDB" nil 0)
                     ("fomo" "FOMO" nil 0)
                     ("shopify" "Shopify" nil 0)
                     ("sms" "SMS" nil 0)
                     ("IOS" "iOS" nil 0)
                     ("ios" "iOS" nil 0)
                     ("android" "Android" nil 0)
                     ("google" "Google" nil 0)
                     ("docker" "Docker" nil 0)
                     ("sqlite" "SQLite" nil 0)
                     ("postgresql" "PostgreSQL" nil 0)
                     ("graphql" "GraphQL" nil 0)
                     ("linkedin" "LinkedIn" nil 0)
                     ("skype" "Skype" nil 0)
                     ("dns" "DNS" nil 0)
                     ("cloudflare" "Cloudflare" nil 0)
                     ("svg" "SVG" nil 0)
                     ("tlds" "TLDs" nil 0)
                     ("tld" "TLD" nil 0)
                     ("hackernews" "HackerNews" nil 0)
                     ("thunderbird" "Thunderbird" nil 0)
                     ("firefox" "Firefox" nil 0)
                     ("twitter" "Twitter" nil 0)
                     ("nextjs" "Next.js" nil 0)
                     ("mongodb" "MongoDB" nil 0)
                     ("expressjs" "Express.js" nil 0)
                     ("nodejs" "NodeJS" nil 0)
                     ("css" "CSS" nil 0)
                     ("wordpress" "Wordpress" nil 0)
                     ("debian" "Debian" nil 0)
                     ("github" "GitHub" nil 0)
                     ("coo" "COO" nil 0)
                     ("cfo" "CFO" nil 0)
                     ("ceo" "CEO" nil 0)
                     ("dao" "DAO" nil 0)
                     ("nft" "NFT" nil 0)
                     ("FF" "FireFox" nil 0)
                     ("ff" "FireFox" nil 0)
                     ("YT" "YouTube" nil 0)
                     ("FB" "Facebook" nil 0)
                     ("fb" "Facebook" nil 0)
                     ("IG" "Instagram" nil 0)
                     ("ig" "Instagram" nil 0)
                     ("facebook" "Facebook" nil 0)
                     ("telegram" "Telegram" nil 0)
                     ("youtube" "YouTube" nil 0)
                     ("BT" "Bluetooth" nil 0)
                     ("macos" "MacOS" nil 0)
                     ("seo" "SEO" nil 0)
                     ("irc" "IRC" nil 0)
                     ("rss" "RSS" nil 0)
                     ("url" "URL" nil 0)
                     ("crm" "CRM" nil 0)
                     ("erp" "ERP" nil 0)
                     ("b2b" "B2B" nil 0)
                     ("b2c" "B2C" nil 0)
                     ("btc" "BTC" nil 0)
                     ("eth" "ETH" nil 0)
                     ("arb" "ARB" nil 0)
                     ("matic" "MATIC" nil 0)
                     ("cms" "CMS" nil 0)
                     ("sim" "SIM" nil 0)
                     ("voip" "VoIP" nil 0)
                     ("linux" "Linux" nil 0)
                     ("ascii" "ASCII" nil 0)
                     ("twitch" "Twitch" nil 0)
                     ("airbnb" "AirBnB" nil 0)
                     ("json" "JSON" nil 0)
                     ("ai" "AI" nil 0)
                     ("chatgpt" "ChatGPT" nil 0)
                     ("asap" "ASAP" nil 0)
                     ("emacs" "Emacs" nil 0)
                     ("termux" "Termux" nil 0)
                     ("whatsup" "WhatsApp" nil 0)
                     ("whatsapp" "WhatsApp" nil 0)
                     ("whatapp" "WhatsApp" nil 0)
                     ("email" "e-mail" nil 0)
                     ("emails" "e-mails" nil 0)
                     ("ui" "UI" nil 0)
                     ("gui" "GUI" nil 0)
                     ("todo" "TODO" nil 0)
                     ) nil :case-fixed nil)

  (define-minor-mode my/en-mode
    "Language mode for `en`."
    :init-value nil
    :lighter " en"
    (setq-local
     ispell-local-dictionary "en"
     local-abbrev-table my/en-abbrevs
     langtool-default-language "en")
    (message "English language activated."))

  (define-minor-mode my/pl-mode
    "Language mode for `pl`."
    :init-value nil
    :lighter " pl"
    (setq-local
     ispell-local-dictionary "pl"
     local-abbrev-table my/pl-abbrevs
     langtool-default-language "pl")
    (message "Polish language activated."))

  (use-package abbrev
    :straight nil
    :diminish "Abb"
    :demand t
    :hook ((text-mode . abbrev-mode)
           (text-mode . my/en-mode))
    :custom
    (save-abbrevs nil)
    (abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory)))

  (straight-register-package 'ispell)
  (if (executable-find "aspell")
      (use-package ispell
        :defer 2
        :custom
        (ispell-local-dictionary "en_US")
        (ispell-local-dictionary-alist
         '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
           ("fr_FR" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "fr_FR") nil utf-8)))
        (ispell-dictionary "en_US")
        (ispell-dictionary-alist
         '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
           ("fr_FR" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "fr_FR") nil utf-8)))
        ;; (ispell-program-name (executable-find "hunspell"))
        (ispell-program-name (executable-find "aspell"))
        (ispell-really-aspell t)
        ;; (ispell-really-hunspell t)
        (ispell-silently-savep t)
        ;; (ispell-extra-args '("--sug-mode=ultra") ;; for hunspell only
        :preface
        (setq my/lang-modes '((en . my/en-mode) (pl . my/pl-mode)))
        (defun my/lang-modes-deactivate ()
          "Deactivate all lang modes."
          (interactive)
          (my/en-mode -1)
          (my/pl-mode -1))

        (defun my/lang-toggle ()
          "Toggle language modes."
          (interactive)
          (unless (derived-mode-p 'prog-mode)
            (let ((new-mode (symbol-function
                             (cond
                              ((bound-and-true-p my/pl-mode) #'my/en-mode)
                              ((bound-and-true-p my/en-mode) #'my/pl-mode)
                              ((bound-and-true-p my/language-local) (cdr (assoc my/language-local my/lang-modes)))
                              (t #'my/en-mode))
                             )))
              (my/lang-modes-deactivate)
              (funcall new-mode 1)))))
    (message "No executable 'aspell' found"))

  (use-package flyspell
    :commands flyspell-mode
    :diminish "fly"
    :custom
    (flyspell-issue-message-flag nil)
    (flyspell-issue-welcome-flag nil)
    :config
    (flyspell-prog-mode)
    (add-to-list 'ispell-skip-region-alist '(":PROPERTIES:" ":END:"))
    (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
    (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE")))

  (use-package artbollocks-mode
    :commands artbollocks-mode
    :custom
    (artbollocks-weasel-words-regex
     (concat "\\b" (regexp-opt
                    '("one of the" "should" "just" "sort of" "a lot" "probably" "maybe" "perhaps" "I think" "really" "pretty" "nice" "action" "utilize" "leverage"
                                          ; test
                      "clavicles" "collarbones" "tiny birds" "antlers" "thrumming" "pulsing" "wombs" "ribcage" "alabaster" "grandmother" "redacting fairytales" "retelling fairytales" "my sorrow" "the window speaking" "avocados" "the blank page" "marrow" "starlings" "giving birth" "giving birth to weird shit" "apples" "peeling back skin" "god" "the mountain trembling" "poetry is my remedy" "sharp fragments" "shards" "grandpa" "i can remember" "this is how it happened" "the pain" "greek myths" "poems about poems" "scars" "cold, stinging" "oranges" "the body" "struggles" "shadows" "the moon reflecting off the" "waves" "echoes in the night" "painted skies" "a hundred" "again and again" "peace, love" "whimsy" "brooklyn" "the summer solstice" "the lunar eclipse" "veins" "soul"
  		    ) t) "\\b")
     artbollocks-jargon nil))

  (use-package ledger-mode
    :bind (:map ledger-mode-map
                ("C-c C-c" . ledger-post-align-dwim)
                ("C-x C-s" . my/ledger-save)
                ("C-x m" . hydra-ledger/body))
    :mode ("\\.ledger\\'" "\\.ledger.gpg\\'")
    :custom
    (ledger-clear-whole-transactions t)
    (ledger-post-account-alignment-column 2)
    (ledger-reconcile-default-commodity "GBP")
    (ledger-reports
     '(("account statement" "%(binary) reg --real [[ledger-mode-flags]] -f %(ledger-file) ^%(account)")
       ("balance sheet" "%(binary) --real [[ledger-mode-flags]] -f %(ledger-file) bal ^assets ^liabilities ^equity")
       ("budget" "%(binary) --empty -S -T [[ledger-mode-flags]] -f %(ledger-file) bal ^assets:bank ^assets:receivables ^assets:cash ^assets:budget")
       ("budget goals" "%(binary) --empty -S -T [[ledger-mode-flags]] -f %(ledger-file) bal ^assets:bank ^assets:receivables ^assets:cash ^assets:'budget goals'")
       ("budget obligations" "%(binary) --empty -S -T [[ledger-mode-flags]] -f %(ledger-file) bal ^assets:bank ^assets:receivables ^assets:cash ^assets:'budget obligations'")
       ("budget debts" "%(binary) --empty -S -T [[ledger-mode-flags]] -f %(ledger-file) bal ^assets:bank ^assets:receivables ^assets:cash ^assets:'budget debts'")
       ("cleared" "%(binary) cleared [[ledger-mode-flags]] -f %(ledger-file)")
       ("equity" "%(binary) --real [[ledger-mode-flags]] -f %(ledger-file) equity")
       ("income statement" "%(binary) --invert --real -S -T [[ledger-mode-flags]] -f %(ledger-file) bal ^income ^expenses -p \"this month\"")))
    :config
    (defun my/ledger-save ()
      "Automatically clean the ledger buffer at each save."
      (interactive)
      ;; (ledger-mode-clean-buffer)
      (save-buffer))

    (unbind-key "<tab>" ledger-mode-map)

    (pretty-hydra-define hydra-ledger
  		       (:hint nil :color teal :quit-key "q" :title (with-faicon "sack-dollar" "Ledger" 1 -0.05))
  		       ("Action"
  			(
  			 ("b" ledger-display-balance-at-point "bal at point")
  			 ("s" (lambda () (interactive) (ledger-sort-buffer) (save-buffer))  "sort")
  			 ("t" ledger-display-ledger-stats "stats")
  			 ("r" ledger-report "report")))))

  (use-package flycheck-ledger
    :demand t
    :after ledger-mode)

  (when nil
    (if (and (fboundp 'native-comp-available-p) (native-comp-available-p))
        (native--compile-async `(,my/local-config-dir) t nil)
      (warn "Native compile not available!")))

  (setq gc-cons-threshold most-positive-fixnum)
