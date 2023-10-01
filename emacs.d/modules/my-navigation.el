;; -*-  lexical-binding; -*-
(when (display-graphic-p)
  (setq-default scroll-up-aggressively 0.01)
  (setq
    ;; mouse-wheel-scroll-amount '(1 ((shift) . 1))
    redisplay-dont-pause t
    scroll-margin 1
    scroll-conservatively 10000
    scroll-preserve-screen-position nil))

(setq
  scroll-step 1
  scroll-margin 0
  scroll-conservatively 101
  auto-window-vscroll nil)

;;; Scrolling.
;; Good speed and allow scrolling through large images (pixel-scroll).
;; Note: Scroll lags when point must be moved but increasing the number
;;       of lines that point moves in pixel-scroll.el ruins large image
;;       scrolling. So unfortunately I think we'll just have to live with
;;       this.
;; https://emacs.stackexchange.com/a/42158/18445
(setq pixel-dead-time 0) ; Never go back to the old scrolling behaviour.
(setq pixel-resolution-fine-flag t) ; Scroll by number of pixels instead of lines (t = frame-char-height pixels).
;; (setq mouse-wheel-scroll-amount '(1)) ; Distance in pixel-resolution to scroll each mouse wheel event.
(setq mouse-wheel-progressive-speed nil) ; Progressive speed is too fast for me.
(setq fast-but-imprecise-scrolling t) ; No (less) lag while scrolling lots.
(setq jit-lock-defer-time 0) ; Just don't even fontify if we're still catching up on user input.

(setq mouse-wheel-scroll-amount
  '(5
     ((shift)
       . 1)
     ((meta))
     ((control)
       . text-scale)))

(setq
  switch-to-buffer-in-dedicated-window 'pop
  switch-to-buffer-obey-display-actions t)

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
       (display-buffer-reuse-window display-buffer-in-previous-window)
       (reusable-frames . nil))
     ("\\*projectile-files-errors\\*"
       (display-buffer-reuse-window display-buffer-at-bottom)
       (window-height . 0.2)
       (reusable-frames . nil))
     ;; ("\\*tree-sitter"
     ;;    (display-buffer-reuse-window display-buffer-same-window)
     ;;    (window-parameters . ((quit-restore . delete))))
     ))
;; switch-to-buffer-obey-display-actions

(bind-keys
  ("C-<mouse-4>" . ignore)
  ("C-<mouse-5>" . ignore)
  ("C-<wheel-down>" . ignore)
  ("C-<wheel-up>" . ignore))

(use-package dashboard
  :if my/dashboard-enabled
  :custom
  (dashboard-icon-type 'all-the-icons)
  (dashboard-display-icons-p t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-heading-icons
    '((recents . "history")
       (bookmarks . "bookmark")
       (agenda . "calendar")
       (projects . "rocket")
       (registers . "database")))
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

(use-package woman
  :straight nil
  :commands woman
  :config
  (bind-keys
    :map woman-mode-map
    ("C-d" . evil-scroll-down)
    ("C-u" . evil-scroll-up)
    ("C-s" . counsel-grep)
    ("/" . counsel-grep)))

(use-package simple
  :straight nil
  :custom
  (set-mark-command-repeat-pop t))

(use-package recentf
  :straight nil
  :custom
  (recentf-max-saved-items 200)
  (recentf-max-menu-items 15)
  (recentf-exclude
    '("COMMIT_EDITMSG"
       "~$"
       "/scp:"
       "/ssh:"
       "/sudo:"
       "/tmp/"))
  :config
  (recentf-mode 1)
  (run-at-time nil (* 60 5) #'recentf-save-list))

(use-package tab-bar
  :straight nil
  :custom
  (tab-bar-tab-name-truncated-max 15)
  (tab-bar-tab-name-current #'tab-bar-tab-name-truncated)
  :config
  (tab-bar-mode 1))

(use-package beacon
  :custom
  (beacon-mode 1))

(use-package popper
  :disabled t
  :commands popper-toggle-latest
  :custom
  (popper-reference-buffers
    '("\\*Messages\\*"
       "\\*Error\\*"
       "Output*$"
       "\\*Warnings\\*"
       "\\*Async Shell Command\\*"
       "\\*straight-process\\*"
       "\\*Flycheck error messages\\*"
       "\\*Backtrace\\*"
       help-mode
       compilation-mode))
  :config
  (popper-mode 1)
  (popper-echo-mode 1))

(use-package help
  :straight nil
  :bind (:map help-mode-map
          ("C-w =" . balance-windows)
          ("/" . evil-search-forward)
          ("v" . set-mark-command)
          ("n" . evil-search-next)
          ("N" . evil-search-previous)
          ("w" . evil-forward-word-begin)
          ("e" . evil-forward-word-end)
          ("E" . evil-forward-WORD-end)
          ("b" . evil-backward-word-begin)
          ("B" . evil-backward-WORD-begin)
          ("y" . evil-yank)
          ("<S-tab>" . backward-button))
  :custom
  (help-window-select t))

(use-package helpful
  :after counsel
  :bind (("C-h f" . helpful-callable)
          ("C-h v" . helpful-variable)
          ("C-h k" . helpful-key)
          ("C-c C-d" . helpful-at-point)
          :map helpful-mode-map
          ("/" . isearch-forward)
          ("C-s" . counsel-grep)
          ("," . 'my/hydra-help/body)
          ("q" . (lambda ()
                   (interactive)
                   (kill-buffer)
                   (delete-window))))
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)

  (pretty-hydra-define my/hydra-help
    (:hint nil :color amaranth :quit-key "q" :title (with-faicon "info" "Help" 1 -0.05))
    ("Action"
      (("l" my/helpful-org-link-store "store org link to page" :exit t))))

  (defun my/helpful-org-link-store ()
    "Copy org style link to current help buffer."
    (interactive)
    (let* ((link  (plist-get (helpful--org-link-store) :link)))
      (kill-new link)
      (message (format "Link to help page '%s' copied" link)))))

(use-package tramp
  :straight nil
  :custom
  (tramp-default-method "ssh")
  (tramp-inline-compress-start-size 40960)
  (tramp-chunksize 500)
  (tramp-auto-save-directory "~/.emacs.d/tramp-autosaves/")
  (tramp-persistency-file-name  "~/.emacs.d/tramp-persistency.el")
  (tramp-encoding-shell "/bin/sh"))

(use-package docker-tramp
  :disabled t
  :after tramp)

(use-package counsel-tramp
  :after (counsel tramp)
  :commands counsel-tramp)

(use-package anzu
  :demand t
  :diminish anzu-mode
  :bind ("M-%" . #'anzu-query-replace-regexp)
  :config
  (global-anzu-mode 1))

(use-package goto-last-change
  :demand 0.3
  :commands (goto-last-change goto-last-change-reverse)
  :config
  (evil-define-key 'normal global-map
    (kbd "[x") #'goto-last-change
    (kbd "]x") #'goto-last-change-reverse))

(use-package iscroll)

(use-package smartscan
  :defer 0.3
  :config
  (global-smartscan-mode 1))

(require 'grep)
(require 'files)

(defun my/rgrep ()
  (interactive)
  (if (executable-find "ack")
    (let* ((regexp (grep-read-regexp))
            (dir (read-directory-name "Base directory: " nil default-directory t))
            (command (concat "ack '" regexp "' " dir)))
      (unless (file-accessible-directory-p dir)
        (error (concat "directory: '" dir "' is not accessible.")))
	    (compilation-start command 'grep-mode))
    (message "No executable 'ack' found!")))

(evil-define-key 'normal global-map
  (kbd ",f") 'my/rgrep)

(use-package treemacs
  :commands (treeemacs treemacs-current-visibility)
  :custom
  (treemacs-default-visit-action #'treemacs-visit-node-in-most-recently-used-window)
  (treemacs-project-follow-cleanup t)
  :config
  (treemacs-follow-mode 1)
  (treemacs-project-follow-mode nil)
  (treemacs-filewatch-mode 1)
  (treemacs-git-mode 'simple)
  (treemacs-resize-icons 22)
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-evil
  :after (treemacs evil)
  :config
  (evil-define-key 'normal treemacs-mode-map
    (kbd "RET") #'treemacs-visit-node-in-most-recently-used-window
    [S-mouse-1] #'treemacs-visit-node-in-most-recently-used-window
    [mouse-3] #'treemacs-visit-node-in-most-recently-used-window
    [C-down-mouse-1] #'treemacs-visit-node-in-most-recently-used-window))

(use-package treemacs-magit
  :after (treemacs magit))

;; (use-package treemacs-all-the-icons
;;   :after treemacs)

(use-package avy
  :bind (:map help-mode-map
          ("\\w" . avy-goto-word-or-subword-1)
          :map evil-normal-state-map
          ("\\w" . avy-goto-word-or-subword-1)
          :map evil-motion-state-map
          ("\\w" . avy-goto-word-or-subword-1)))

;; https://oremacs.com/swiper/
(use-package ivy
  :diminish ivy-mode
  :demand t
  :bind (:map ivy-minibuffer-map
          ("<return>" . ivy-alt-done)
          ("<backspace>" . ivy-backward-delete-char)
          ("C-M-h" . ivy-previous-line-and-call)
          ("C-c o" . ivy-occur)
          ("C-'" . ivy-avy))
  :custom
  (ivy-switch-buffer-faces-alist
    '((emacs-lisp-mode . swiper-match-face-1)
       (dired-mode . ivy-subdir)
       (org-mode . org-level-4)))
  (ivy-height 15)
  (ivy-use-selectable-prompt t)
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffer t)
  (ivy-re-builders-alist '((t . ivy--regex-plus)))
  ;; (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  :config
  (ivy-mode 1)
  (advice-add #'ivy-switch-buffer :before #'my/evil-switch-to-normal-state-if-insert)

  (ivy-set-actions
    'counsel-colors-emacs
    '(("o" my/counsel-colors-action-insert-hex "insert hexadecimal value")))

  (defun my/counsel-colors-action-insert-hex (color)
    "Insert the hexadecimal RGB value of COLOR."
    (let* ((color-long (apply #'color-rgb-to-hex (color-name-to-rgb color)))
            (r (substring color-long 0 3))
            (g (substring color-long 5 7))
            (b (substring color-long 9 11)))
      (insert r g b))))

(use-package prescient
  :after ivy
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :after (ivy prescient)
  :config
  (ivy-prescient-mode 1))

(use-package counsel
  :after ivy
  :demand t
  :bind (("M-x" . counsel-M-x)
          ("C-x C-f" . counsel-find-file)
          ("<f1> f" . counsel-describe-function)
          ("<f1> v" . counsel-describe-variable)
          ("<f1> l" . counsel-find-library)
          ("<f2> s" . counsel-info-lookup-symbol)
          ("<f2> u" . counsel-unicode-char)
          ("C-h f" . counsel-describe-function)
          ("C-h v" . counsel-describe-variable)
          ("C-h a" . counsel-apropos)
          ("C-x r b" . counsel-bookmark)
          ("C-x b" . counsel-switch-buffer)
          ("C-x C-b" . counsel-switch-buffer)
          ("C-x B" . counsel-switch-buffer-other-window)
          ("C-x C-r" . my/recentf)
          ("C-h l" . counsel-find-library)
          :map read-expression-map
          ("C-r" . counsel-minibuffer-history)
          ("C-r" . counsel-expression-history)
          :map evil-normal-state-map
          ("C-s" . counsel-grep))
  :custom
  (counsel-rg-base-command "rg -S -M 150 --no-heading --line-number --color never %s")
  (counsel-find-file-ignore-regexp "\\`\\.")
  (counsel-grep-base-command "grep -E -n -i -e %s %s")
  (counsel-outline-path-separator " > ")
  (swiper-use-visual-line-p #'ignore)
  :config
  (unbind-key "C-x C-h" global-map)

  (add-to-list 'counsel-outline-settings '(my/org-roam-mode :outline-title counsel-outline-title-org :action counsel-org-goto-action :history counsel-org-goto-history :caller counsel-org-goto))

  (defun my/recentf ()
    (interactive)
    (if (projectile-project-root)
      (projectile-recentf)
      (counsel-recent)))

  (defun my/counsel-ibuffer-other-window (&optional name)
    "Use ibuffer to switch to another buffer.
NAME specifies the name of the buffer (defaults to \"*Ibuffer*\")."
    (interactive)
    (setq counsel-ibuffer--buffer-name (or name "*Ibuffer*"))
    (ivy-read "Switch to buffer: " (counsel-ibuffer--get-buffers)
      :history 'counsel-ibuffer-history
      :action 'counsel-ibuffer-visit-buffer-other-window
      :caller 'counsel-ibuffer))

  (advice-add 'counsel-grep :around 'my/counsel-grep-fallback)
  (ivy-set-display-transformer 'counsel-describe-function nil))

(defun my/counsel-grep-fallback (orig-fun &rest args)
  "Fallback counsel-grep to evil-search-forward if exists if not search-forward."
  (when (my/buffer-tramp-p)
    (if (fboundp 'evil-search-forward)
      (apply 'evil-search-forward args)
      (apply 'search-forward args)))
    ;; (apply orig-fun args))

  ;; (if (or (string= major-mode "dired-mode") (string= major-mode "org-mode") (string= major-mode "help-mode") (string= "*scratch*" (buffer-name)) (string= "*Org Agenda*" (buffer-name)) (string-match ".gpg\\'" (buffer-name)))

  (if (fboundp #'swiper)
    (apply #'swiper args)
    (if (fboundp #'evil-search-forward)
      (apply #'evil-search-forward args)
      (apply #'search-forward args))))

;; (use-package undo-tree
;;   :defer t
;;   :diminish undo-tree-mode
;;   :config
;;   (setq undo-tree-visualizer-diff t)
;;   (when (fboundp 'evil-make-overriding-map)
;;     (evil-make-overriding-map undo-tree-visualizer-mode-map 'motion)
;;     (evil-make-overriding-map undo-tree-visualizer-selection-mode-map 'motion)
;;     (evil-make-overriding-map undo-tree-map 'motion)))

;; (use-package perspective
;;   :demand t
;;   :hook ((kill-emacs . persp-state-save))
;;   :bind (("C-x C-b" . persp-counsel-switch-buffer)
;;           ("C-x C-k" . persp-kill-buffer*))
;;   :preface
;;   (defun my/persp-counsel-switch-buffer-other (arg)
;;     (interactive "P")
;;     (apply #'ivy-read
;;       (append
;;         (list
;;           (format "Switch to buffer other window (%s): " (persp-current-name))
;;           (cl-remove-if #'null (mapcar #'buffer-name
;;                                  ;; buffer-list is ordered by access time
;;                                  ;; seq-intersection keeps the order
;;                                  (seq-intersection (buffer-list)
;;                                    (persp-current-buffers))))
;;           :preselect (buffer-name (persp-other-buffer (current-buffer)))
;;           :keymap ivy-switch-buffer-map
;;           :caller #'ivy-switch-buffer
;;           :action #'ivy--switch-buffer-other-window-action
;;           :matcher #'ivy--switch-buffer-matcher)
;;         )))
;;   :custom
;;   (persp-sort 'created)
;;   (persp-state-default-file "~/.emacs.d/.persp-confs/persp-auto-save")
;;   :config
;;   (persp-mode))

(use-package ivy-rich
  :after (org counsel)
  :hook (org-mode . (lambda () (ivy-rich-local-mode 1)))
  :custom
  (ivy-rich-parse-remote-buffer nil)
  (ivy-rich-path-style 'full)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

  (defun my/ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
      (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
        (if (symbolp icon)
          (all-the-icons-icon-for-mode 'fundamental-mode)
          icon))))

  (defun my/ivy-switch-buffer-org-roam-title (candidate)
    (if (ivy-rich-switch-buffer-user-buffer-p candidate)
        (let* ((file (buffer-file-name (get-buffer candidate)))
                (file (if (and (buffer-file-name) (fboundp 'org-roam-file-p) (org-roam-file-p file))
                        (ignore-errors (org-roam-with-file file nil (org-roam-node-title (org-roam-node-at-point)))) "")))
          (if file file ""))
      ""))

  (setq ivy-rich-display-transformers-list
    '(ivy-switch-buffer
       (:columns
         ((my/ivy-rich-switch-buffer-icon (:width 2))
           (ivy-rich-candidate (:width 30))
           (my/ivy-switch-buffer-org-roam-title (:width 40))
           (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
         :predicate (lambda (cand) (get-buffer cand)))
       counsel-find-file
       (:columns
         ((ivy-read-file-transformer)
           (ivy-rich-counsel-find-file-truename
             (:face font-lock-doc-face))))
       counsel-M-x
       (:columns
         ((counsel-M-x-transformer
            (:width 40))
           (ivy-rich-counsel-function-docstring
             (:face font-lock-doc-face))))
       counsel-describe-function
       (:columns
         ((counsel-describe-function-transformer
            (:width 40))
           (ivy-rich-counsel-function-docstring
             (:face font-lock-doc-face))))
       counsel-describe-variable
       (:columns
         ((counsel-describe-variable-transformer
            (:width 40))
           (ivy-rich-counsel-variable-docstring
             (:face font-lock-doc-face))))
       counsel-recentf
       (:columns
         ((ivy-rich-candidate
            (:width 0.8))
           (ivy-rich-file-last-modified-time
             (:face font-lock-comment-face))))
       package-install
       (:columns
         ((ivy-rich-candidate
            (:width 30))
           (ivy-rich-package-version
             (:width 16 :face font-lock-comment-face))
           (ivy-rich-package-archive-summary
             (:width 7 :face font-lock-builtin-face))
           (ivy-rich-package-install-summary
             (:face font-lock-doc-face))))))

  (define-minor-mode ivy-rich-local-mode
    "Toggle ivy-rich mode locally."
    :global nil
    (if ivy-rich-local-mode
      (unless ivy-rich--original-display-transformers-list
        (ivy-rich-set-display-transformer nil))
      (ivy-rich-unset-display-transformer))))

(use-package visual-fill-column
  :commands visual-fill-column-mode)
;;   :hook ((text-mode . visual-fill-column-mode)))

(use-package nov
  :straight (:type git
             :host github
             :repo "emacs-pe/nov.el"
             :branch "master")
  :mode ("\\.epub\\'" . nov-mode)
  :hook ((nov-mode . visual-line-mode)
          (nov-mode . visual-fill-column-mode)
          (nov-mode . (lambda ()
                        (setq-local visual-fill-column-center-text t)
                        (face-remap-add-relative 'variable-pitch :family "Liberation Serif" :height 1.5))))
  :custom
  (nov-text-width 75)
  (visual-fill-column-center-text t))

(use-package pdf-tools
  :disabled t
  :straight (:type git
             :host github
             :repo "vedang/pdf-tools"
             :branch "master")
  ;; :pin manual ;; manually updat
  :bind (:map pdf-view-mode-map
          ("/" . isearch-forward))
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-annot-activate-created-annotations t)
  (pdf-view-resize-factor 1.25)
  :config
  (pdf-loader-install))

;; Bring back window configuration after ediff quits
(defvar my/ediff-bwin-config nil "Window configuration before ediff.")
(defcustom my/ediff-bwin-reg ?b
  "*Register to be set up to hold `my/ediff-bwin-config'
    configuration.")

(defvar my/ediff-awin-config nil "Window configuration after ediff.")
(defcustom my/ediff-awin-reg ?e
  "*Register to be used to hold `my/ediff-awin-config' window
    configuration.")

(defun my-ediff-bsh ()
  "Function to be called before any buffers or window setup for
    ediff."
  (setq my/ediff-bwin-config (current-window-configuration))
  (when (characterp my/ediff-bwin-reg)
    (set-register my/ediff-bwin-reg
		  (list my/ediff-bwin-config (point-marker)))))

(defun my-ediff-ash ()
  "Function to be called after buffers and window setup for ediff."
  (setq my/ediff-awin-config (current-window-configuration))
  (when (characterp my/ediff-awin-reg)
    (set-register my/ediff-awin-reg
		  (list my/ediff-awin-config (point-marker)))))

(defun my-ediff-qh ()
  "Function to be called when ediff quits."
  (when my/ediff-bwin-config
    (set-window-configuration my/ediff-bwin-config)))

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

(evil-define-key 'normal global-map
  (kbd "C-a") #'my/smarter-move-beginning-of-line)

(use-package windmove
  :straight nil
  :defer 10
  :bind (("C-x <left>" . windmove-left)
          ("C-x <right>" . windmove-right)
          ("C-x <up>" . windmove-up)
          ("C-x <down>" . windmove-down))
  :custom
  (windmove-wrap-around t)
  (framemove-hook-into-windmove t)
  (advice-add 'windmove-right :before #'my/evil-switch-to-normal-state-if-insert)
  (advice-add 'windmove-left :before #'my/evil-switch-to-normal-state-if-insert)
  (advice-add 'windmove-up :before #'my/evil-switch-to-normal-state-if-insert)
  (advice-add 'windmove-down :before #'my/evil-switch-to-normal-state-if-insert))

;; Folding blocks of code by indention
(use-package origami
  :commands origami-mode
  :custom
  (evil-define-key 'normal origami-mode-map
    (kbd "<C-backspace>") #'origami-toggle-node))

(use-package openwith
  :disabled t
  :if (eq system-type 'darwin)
  :custom
  (large-file-warning-threshold nil)
  (openwith-associations '(("\\.\\(?:mp3\\|ogg\\)\\'" "/usr/bin/open" (file))
                            ;; ("\\.\\(?:jpe?g\\|png\\|gif\\)\\'" "/usr/bin/open" (file))
                            ("\\.\\(?:ods\\|odt\\)\\'" "/usr/bin/open" (file))
                            ("\\.\\(?:mp4\\|mkv\\|mpe?g\\|avi\\|wmv\\)\\'" "/usr/bin/open" (file))
                            ("\\.pdf\\'" "/usr/bin/open" (file))))
  :config
  (openwith-mode t)
  (rassq-delete-all #'doc-view-mode-maybe auto-mode-alist))

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))))

(use-package centaur-tabs
  :demand t
  :bind (:map evil-normal-state-map
          ("g t" . centaur-tabs-forward)
          ("g T" . centaur-tabs-backward))
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  :custom
  (centaur-tabs-style "alternate")
  (centaur-tabs-height 32)
  (centaur-tabs-plain-icons nil)
  (centaur-tabs-gray-out-icons nil)
  (centaur-tabs-set-bar 'under)
  (centaur-tabs-set-icons t)
  (x-underline-at-descent-line t)
  (centaur-tabs-close-button "x")
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "*")
  (centaur-tabs-label-fixed-length 0)
  (centaur-tabs-show-count nil)
  (centaur-tabs-left-edge-margin nil)
  (centaur-tabs-show-navigation-buttons nil)
  (uniquify-separator "/")
  (uniquify-buffer-name-style 'forward)
  (centaur-tabs-adjust-buffer-order t)
  (centaur-tabs-cycle-scope 'tabs)
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-change-fonts (face-attribute 'default :font) 110)
  (centaur-tabs-headline-match)
  (centaur-tabs-enable-buffer-reordering)
  (centaur-tabs-enable-buffer-alphabetical-reordering)
  (centaur-tabs-group-by-projectile-project)
  (add-to-list 'evil-emacs-state-modes 'lsp-ui-imenu-mode)

  (defun my/centaur-tabs-hide-tab (x)
    "Do no to show buffer X in tabs."
    (let ((name (format "%s" x)))
      (or
        ;; Current window is not dedicated window.
        (window-dedicated-p (selected-window))

        ;; Buffer name not match below blacklist.
        (string-prefix-p "*epc" name)
        (string-prefix-p "*helm" name)
        (string-prefix-p "*Helm" name)
        (string-prefix-p "*Compile-Log*" name)
        (string-prefix-p "*lsp" name)
        (string-prefix-p "*company" name)
        (string-prefix-p "*Flycheck" name)
        (string-prefix-p "*tramp" name)
        (string-prefix-p " *Mini" name)
        (string-prefix-p "*help" name)
        (string-prefix-p "*straight" name)
        (string-prefix-p " *temp" name)
        (string-prefix-p "*Help" name)
        (string-prefix-p "*Messages" name)
        (string-prefix-p "*Async" name)
        (string-prefix-p "magit" name)

        ;; Is not magit buffer.
        (and (string-prefix-p "magit" name)
          (not (file-name-extension name))))))

  (setq centaur-tabs-hide-tab-function #'my/centaur-tabs-hide-tab)

  (defun my/centaur-tabs-buffer-tab-label (tab)
    "Return a label for TAB.
That is, a string used to represent it on the tab bar."
    ;; Init tab style.
    ;; Render tab.
    (format " %s"
	    (let* ((bufname (if centaur-tabs--buffer-show-groups
			                  (centaur-tabs-tab-tabset tab)
			                  (buffer-name (car tab))))
              (bufname (if (and (fboundp 'org-roam-buffer-p) (org-roam-buffer-p))
                         (with-current-buffer bufname (org-roam-db--file-title))
                         bufname)))
	            (if (> centaur-tabs-label-fixed-length 0)
		            (centaur-tabs-truncate-string  centaur-tabs-label-fixed-length bufname)
	              bufname))))

  (setq centaur-tabs-tab-label-function #'my/centaur-tabs-buffer-tab-label))

;; (use-package demap)

(defvar my/save-buffers-kill-terminal-was-called nil)

;; (defun my/save-buffers-kill-terminal ()
;;   (interactive)
;;   (setq my/save-buffers-kill-terminal-was-called t)
;;   (save-buffers-kill-terminal t))

(defun my/kill-all-buffers-except-toolkit ()
  "Kill all buffers except current one and toolkit (*Messages*, *scratch*).
Close other windows."
  (interactive)
  (mapc 'kill-buffer (remove-if
                       (lambda (x)
                         (or
                           (some (lambda (window) ( string-equal (buffer-name x) (buffer-name (window-buffer window)))) (window-list))
                           ;; (string-equal (buffer-file-name) (buffer-file-name x))
                           (member (buffer-name x) '("*Messages*" "*scratch*"))))
                         (buffer-list)))
  (delete-other-frames)
  (message "Closed other buffers and frames."))

(defun my/clone-indirect-buffer-new-window ()
  "Clone indirect buffer in new window."
  (interactive)
  (split-window-right)
  (clone-indirect-buffer nil t)
  (windmove-left))

; https://www.emacswiki.org/emacs/ToggleWindowSplit
;; (defun my/toggle-window-split ()
;;   (interactive)
;;   (if (= (count-windows) 2)
;;       (let* ((this-win-buffer (window-buffer))
;; 	     (next-win-buffer (window-buffer (next-window)))
;; 	     (this-win-edges (window-edges (selected-window)))
;; 	     (next-win-edges (window-edges (next-window)))
;; 	     (this-win-2nd (not (and (<= (car this-win-edges)
;; 					 (car next-win-edges))
;; 				     (<= (cadr this-win-edges)
;; 					 (cadr next-win-edges)))))
;; 	     (splitter
;; 	      (if (= (car this-win-edges)
;; 		     (car (window-edges (next-window))))
;; 		  'split-window-horizontally
;; 		'split-window-vertically)))
;; 	(delete-other-windows)
;; 	(let ((first-win (selected-window)))
;; 	  (funcall splitter)
;; 	  (if this-win-2nd (other-window 1))
;; 	  (set-window-buffer (selected-window) this-win-buffer)
;; 	  (set-window-buffer (next-window) next-win-buffer)
;; 	  (select-window first-win)
;; 	  (if this-win-2nd (other-window 1))))))

(defvar aok/read-only-folders
  '("~/emacs" "~/.emacs.d/lisp" "~/.emacs.d/straight" "~/.dotenv/emacs.d/straight")
  "Files in these folders will be opened in read-only mode.")

(defun aok/file-set-read-only-if-listed ()
  "Set current file buffer as `read-only' if it's in `aok/read-only-folders'."
  (when (seq-some (lambda (i) (string-prefix-p (expand-file-name i) buffer-file-name)) aok/read-only-folders)
    (read-only-mode 1)))

(add-hook 'find-file-hook #'aok/file-set-read-only-if-listed)

;; (defun my/prev-frame ()
;;   (interactive)
;;   (other-frame -1))

;; (defun my/next-frame ()
;;   (interactive)
;;   (other-frame 1))

(bind-keys
  ;; ("C-x |" . my/toggle-window-split)
  ;; ("C-x C-c" . my/save-buffers-kill-terminal)
  ([remap move-beginning-of-line] . my/smarter-move-beginning-of-line)
  ;; ("<s-right>" . my/next-frame)
  ;; ("<s-left>" . my/prev-frame)
  ;; ("C-c p" . #'pop-to-mark-command)
  )

;; https://github.com/minad/consult#live-previews
;; (use-package consult
;;   ;; :bind (("C-s" . consult-line)
;;   ;;        ("C-M-l" . consult-imenu)
;;   ;;        ("C-M-j" . persp-switch-to-buffer*)
;;   ;;        :map minibuffer-local-map
;;   ;;        ("C-r" . consult-history))
;;   :custom
;;   (consult-project-root-function
;;     (lambda () (when (fboundp 'projectile-project-root)
;;                  (projectile-project-root))))
;;   (completion-in-region-function 'consult-completion-in-region)
;;   ;; (xref-show-xrefs-function 'ivy-xref-show-xrefs)
;;   ;; (xref-show-definitions-function 'ivy-xref-show-defs)
;;   :config
;;   (add-to-list 'ivy-completing-read-handlers-alist '(consult-git-grep completing-read-default))
;;   (add-to-list 'ivy-completing-read-handlers-alist '(consult-man completing-read-default))
;;   (add-to-list 'ivy-completing-read-handlers-alist '(consult-buffer completing-read-default))
;;   (consult-preview-at-point-mode 1)
;;   (setq consult-preview-key any)
;;   )

;; (defun my/counsel-mark-ring-global ()
;;   "Browse `global-mark-ring' interactively.
;; Obeys `widen-automatically', which see."
;;   (interactive)
;;   (let* ((counsel--mark-ring-calling-point (point))
;;          (marks (copy-sequence global-mark-ring))
;;          (marks (delete-dups marks))
;;          (marks
;;            ;; mark-marker is empty?
;;           (if (equal (mark-marker) (make-marker))
;;               marks
;;             (message "foo 1")
;;             (cons (copy-marker (mark-marker)) marks)))
;;          (candidates (counsel-mark--get-candidates marks)))
;;     (message "foo 2")
;;     (if candidates
;;         (counsel-mark--ivy-read "Mark: " candidates 'my/counsel-mark-ring-global)
;;       (message "Mark ring is empty"))))

;; (require 'cl)
;; (use-package imenu-anywhere)
;; (use-package jump-tree)
;; (use-package ace-jump-buffer)
;; (use-package frog-jump-buffer)

(pretty-hydra-define hydra-buffer
  (:hint nil :color teal :quit-key "q" :title (with-faicon "align-justify" "Buffer" 1 -0.05))
  ("Actions"
   (("i" ibuffer "ibuffer")
    ("k" my/kill-all-buffers-except-toolkit))))

(evil-define-key 'normal global-map
  (kbd ",b") #'hydra-buffer/body)

(defun my/move-current-window-to-new-frame ()
  "Move current window to new frame."
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

(provide 'my-navigation)
;;; my-navigation.el ends here