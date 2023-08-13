;; (eval-after-load 'eshell
  ;; '(progn
     ;; (evil-make-overriding-map eshell-mode-map 'motion)
     ;; (evil-make-overriding-map eshell-mode-map 'normal)))

;; (setq explicit-shell-file-name "/usr/local/bin/bash")

(defalias 'esh 'eshell)

(use-package eshell-toggle
  :disabled t
  :commands eshell
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
