;; nice themes
;; deeper-blue
;; tango dark
;; wombat

;; light theme
;; adwaita

(setq custom-theme-directory "~/.emacs.d/themes/")

(defvar my/current-theme nil)

(defun my/set-dark-theme ()
  (when (featurep 'adwaita-theme)
    (disable-theme 'adwaita)
    (unload-feature 'adwaita-theme))
  (load-theme 'wombat)

  (custom-theme-set-faces
    'wombat
    '(cursor ((t (:inherit nil :underline nil :background "White"))))
    '(region ((t (:inherit nil :underline nil :foreground "White" :background "RoyalBlue4"))))
    '(hl-line ((t (:inherit nil :underline nil :background "gray34"))))
    '(ledger-font-xact-highlight-face ((t nil)))
    '(org-level-2 ((t (:inherit outline-2 :foreground "LightGoldenrod2"))))
    '(org-level-3 ((t (:inherit outline-3 :foreground "DarkGoldenrod2"))))
    '(org-level-4 ((t (:inherit outline-4 :foreground "plum1"))))
    '(org-priority ((t (:inherit font-lock-keyword-face :foreground "gold3"))))
    '(org-done ((t (:inherit nil :strike-through nil :weight normal))))
    '(org-headline-done ((t (:inherit nil :strike-through t :weight: normal))))
    '(org-agenda-done ((t (:inherit nil :strike-through t :weight normal))))
    '(ledger-font-payee-uncleared-face ((t (:inherit nil :foreground "gold1"))))
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

(defalias 'toggle-theme #'my/toggle-theme)

(my/set-dark-theme)

(setq
  custom-safe-themes t
  default-font "-*-Menlo-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")
(set-face-attribute 'default nil :font default-font)

(provide 'my-theme)
