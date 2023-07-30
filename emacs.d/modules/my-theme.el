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

(use-package all-the-icons-dired
  :diminish all-the-icons-dired-mode
  :after all-the-icons
  :config
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))

(setq custom-theme-directory "~/.emacs.d/themes/")

(defvar my/current-theme nil)

(defun my/set-dark-theme ()
  (when (featurep 'adwaita-theme)
    (disable-theme 'adwaita)
    (unload-feature 'adwaita-theme))
  (load-theme 'wombat t)

  (custom-theme-set-faces
    'wombat
    '(cursor ((t (:inherit nil :underline nil :background "gray82"))))
    '(region ((t (:background "DodgerBlue4" :foreground "#f6f3e8"))))
    ;; '(region ((t (:inherit nil :underline nil :foreground "White" :background "RoyalBlue4"))))
    '(hl-line ((t (:background "gray32"))))
    '(ledger-font-xact-highlight-face ((t nil)))
    '(org-level-2 ((t (:inherit default :extend nil :foreground "goldenrod"))))
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
    '(region ((t (:inherit default :extend nil :background "steel blue"))))
    )

  (setq my/current-theme 'wombat))

(defun my/set-light-theme ()
  (when (featurep 'wombat-theme)
    (disable-theme 'wombat)
    (unload-feature 'wombat-theme))
  (load-theme 'adwaita t)
  (setq my/current-theme 'adwaita))

(defun my/toggle-theme ()
  (interactive)
  (if (eq  my/current-theme 'wombat)
    (my/set-light-theme)
    (my/set-dark-theme)))

(defalias 'toggle-theme #'my/toggle-theme)

(my/set-dark-theme)

(setq custom-safe-themes t)

(when (eq system-type 'darwin)
  (setq default-font "-*-Menlo-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")
  (set-face-attribute 'default nil :font default-font))

;; (require 'chromatext)
;; (setq chromatext-color-pairs
;;   (list (list
;;           (face-foreground 'default)
;;           (color-lighten-name (face-foreground 'default) 80))))

(provide 'my-theme)
