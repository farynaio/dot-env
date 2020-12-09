;; (eval-after-load 'eshell
  ;; '(progn
     ;; (evil-make-overriding-map eshell-mode-map 'motion)
     ;; (evil-make-overriding-map eshell-mode-map 'normal)))

(defalias 'esh 'eshell)

(setq explicit-shell-file-name "/bin/ksh")

(provide 'my-shell)
