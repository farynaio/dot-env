;; (eval-after-load 'eshell
  ;; '(progn
     ;; (evil-make-overriding-map eshell-mode-map 'motion)
     ;; (evil-make-overriding-map eshell-mode-map 'normal)))

(setq explicit-shell-file-name "/bin/ksh")

(defalias 'esh 'eshell)

(provide 'my-shell)
