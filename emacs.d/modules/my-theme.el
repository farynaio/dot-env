;; nice themes
;; deeper-blue
;; tango dark
;; wombat

;; light theme
;; adwaita

(use-package all-the-icons
  :config
  (unless (find-font (font-spec :name "all-the-icons"))
            (all-the-icons-install-fonts t))
  ;; (when (not (file-exists-p (expand-file-name "~/Library/Fonts/all-the-icons.ttf")))
  ;;   (all-the-icons-install-fonts))

  (defun my/vc-mode-line ()
    (let* (
            (branch nil)
            ;; (branch (if (boundf 'magit-status) (magit-get-current-branch)))
            (branch (if branch branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-"))))
      (concat
        (propertize (format " %s" (all-the-icons-octicon "git-branch"))
          'face `(:height 1 :family ,(all-the-icons-octicon-family))
          'display '(raise 0))
        (propertize (format " %s" branch))
        (propertize " "))))

  (defadvice vc-mode-line (after strip-backend () activate)
    (when (stringp vc-mode)
      (let ((noback (replace-regexp-in-string
                      (format "^ %s" (vc-backend buffer-file-name))
                      " " (my/vc-mode-line))))
        (setq vc-mode noback)))))

(use-package ivy-rich
  :hook (org-mode . (lambda () (ivy-rich-local-mode 1)))
  :config
  (setq ivy-rich-parse-remote-buffer nil)
  (setq ivy-rich-path-style 'full)

  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

  (defun jarfar/ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
      (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
        (if (symbolp icon)
          (all-the-icons-icon-for-mode 'fundamental-mode)
          icon))))

  (defun jarfar/ivy-switch-buffer-org-roam-title (candidate)
    (if (ivy-rich-switch-buffer-user-buffer-p candidate)
      (let* ((file (buffer-file-name (get-buffer candidate)))
              (file (if (and (fboundp 'org-roam--org-roam-file-p) (org-roam--org-roam-file-p file)) (org-roam-db--get-title file) "")))
        (if file file ""))
      ""))

  (setq ivy-rich-display-transformers-list
    '(ivy-switch-buffer
       (:columns
         ((jarfar/ivy-rich-switch-buffer-icon (:width 2))
           (ivy-rich-candidate (:width 30))
           (jarfar/ivy-switch-buffer-org-roam-title (:width 40))
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
        (ivy-rich-set-display-transformer))
      (ivy-rich-unset-display-transformer))))

(setq custom-theme-directory "~/.emacs.d/themes/")

(defvar my/current-theme nil)

(defun my/set-dark-theme ()
  (when (featurep 'adwaita-theme)
    (disable-theme 'adwaita)
    (unload-feature 'adwaita-theme))
  (load-theme 'wombat)

  (custom-theme-set-faces
    'wombat
    '(cursor ((t (:inherit nil :underline nil :background "gray82"))))
    '(region ((t (:background "DodgerBlue4" :foreground "#f6f3e8"))))
    ;; '(region ((t (:inherit nil :underline nil :foreground "White" :background "RoyalBlue4"))))
    '(hl-line ((t (:background "gray32"))))
    '(ledger-font-xact-highlight-face ((t nil)))
    '(org-level-2 ((t (:inherit outline-2 :foreground "LightGoldenrod2"))))
    '(org-level-3 ((t (:inherit outline-3 :foreground "DarkGoldenrod2"))))
    '(org-level-4 ((t (:inherit outline-4 :foreground "peru"))))
    '(org-level-5 ((t (:inherit outline-5 :foreground "plum1"))))
    '(org-level-6 ((t (:inherit outline-6 :foreground "burlywood"))))
    '(org-level-7 ((t (:inherit outline-7 :foreground "SpringGreen"))))
    '(org-level-8 ((t (:inherit outline-8 :foreground "cyan"))))
    '(org-priority ((t (:inherit font-lock-keyword-face :foreground "gold3"))))
    '(org-done ((t (:inherit nil :strike-through nil :weight normal))))
    '(org-headline-done ((t (:inherit nil :strike-through t :weight: normal))))
    '(org-agenda-done ((t (:inherit nil :strike-through t :weight normal))))
    '(ledger-font-payee-uncleared-face ((t (:inherit nil :foreground "gold1"))))
    '(persp-selected-face ((t (:foreground "cyan4" :weight bold))))
    '(symbol-overlay-default-face ((t (:background "gray37"))))
    )

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

(defalias 'toggle-theme 'my/toggle-theme)

(my/set-dark-theme)

(setq custom-safe-themes t)
(setq default-font "-*-Menlo-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")

(set-face-attribute 'default nil :font default-font)

(provide 'my-theme)
