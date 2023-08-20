
;;; Code:

;; (eval-after-load 'eshell
  ;; '(progn
     ;; (evil-make-overriding-map eshell-mode-map 'motion)
     ;; (evil-make-overriding-map eshell-mode-map 'normal)))

;; (setq explicit-shell-file-name "/usr/local/bin/bash")

(defalias 'esh 'eshell)

(use-package eshell-toggle
  :commands eshell-toggle
  :straight (:type: git
              :host github
              :repo "4DA/eshell-toggle"
              :branch "master")
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil))

(use-package vterm
  :disabled t
  :commands vterm
  :custom
  (vterm-max-scrollback 10000)
  :config
  (advice-add 'evil-collection-vterm-insert :before #'vterm-reset-cursor-point))

(provide 'my-shell)
;;; my-shell.el ends here