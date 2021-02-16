;; (use-package robe
;; :config
;; (progn
;;   (add-hook 'robe-mode-hook (lambda ()
;; (robe-start)
;;                               (make-local-variable 'company-backends)
;;                               (add-to-list 'company-backends 'company-robe t)))
;;   (add-hook 'ruby-mode-hook 'robe-mode)
;;   ))
;; (use-package inf-ruby)

(use-package projectile-rails
  :requires projectile
  :hook (ruby-mode . (lambda () (when (projectile-mode) (projectile-rails-on)))))

(provide 'my-ruby)