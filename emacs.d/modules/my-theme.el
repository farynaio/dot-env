
;;; Code:

(use-package all-the-icons
  :config
  (unless (find-font (font-spec :name "all-the-icons"))
            (all-the-icons-install-fonts t))
  ;; (when (not (file-exists-p (expand-file-name "~/Library/Fonts/all-the-icons.ttf")))
  ;;   (all-the-icons-install-fonts))

  ;; (defun my/vc-mode-line ()
  ;;   (let* (
  ;;           (branch nil)
  ;;           (branch (if branch branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-"))))
  ;;     (concat
  ;;       (propertize (format " %s" (all-the-icons-octicon "git-branch"))
  ;;         'face `(:height 1 :family ,(all-the-icons-octicon-family))
  ;;         'display '(raise 0))
  ;;       (propertize (format " %s" branch))
  ;;       (propertize " "))))

  ;; (defadvice vc-mode-line (after strip-backend () activate)
  ;;   (when (stringp vc-mode)
  ;;     (let ((noback (replace-regexp-in-string
  ;;                     (format "^ %s" (vc-backend buffer-file-name))
  ;;                     " " (my/vc-mode-line))))
  ;;       (setq vc-mode noback))))
  )

(setq custom-theme-directory "~/.emacs.d/themes/")

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :font "-*-Menlo-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")
;;   (set-face-attribute 'default nil :font "Source Code Pro Medium")
;;   (set-fontset-font t 'latin "Noto Sans")
  )

(use-package nerd-icons)

;; https://github.com/doomemacs/themes
(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-themes-treemacs-theme "doom-vibrant")
  :config
  ;; (doom-themes-neotree-config)
  (doom-themes-org-config)
  (doom-themes-visual-bell-config)
  (load-theme 'doom-vibrant t))

;; https://github.com/hlissner/emacs-solaire-mode
;; (use-package solaire-mode
;;   :config
;;   (solaire-global-mode 1))

;; After install run nerd-icons-install-fonts
(use-package doom-modeline
  :demand t
  :init
  (setq-default
    doom-modeline-support-imenu t
    doom-modeline-window-width-limit 85
    doom-modeline-minor-modes t
    doom-modeline-vcs-max-length 12)
  (if (facep 'mode-line-active)
    (set-face-attribute 'mode-line-active nil :family "Noto Sans" :height 120) ; For 29+
    (set-face-attribute 'mode-line nil :family "Noto Sans" :height 120))
  (set-face-attribute 'mode-line-inactive nil :family "Noto Sans" :height 120)
  :hook (after-init . doom-modeline-mode)
  :custom
  (mode-line-right-align-edge 'right-fringe)
  ;; (unless (find-font (font-spec :name "nerd-icons"))
  ;; (nerd-icons-install-fonts t))
)

;; (require 'chromatext)
;; (setq chromatext-color-pairs
;;   (list (list
;;           (face-foreground 'default)
;;           (color-lighten-name (face-foreground 'default) 80))))

(provide 'my-theme)
;;; my-theme.el ends here