
;;; Code:

;; (use-package robe
;; :config
;; (progn
;;   (add-hook 'robe-mode-hook (lambda ()
;; (robe-start)
;;                               (make-local-variable 'company-backends)
;;                               (add-to-list 'company-backends 'company-robe t)))
;;   (add-hook 'ruby-mode-hook 'robe-mode)
;;   ))

(use-package inf-ruby
  :commands inf-ruby-minor-mode
  :config
  (add-to-list 'evil-emacs-state-modes 'inf-ruby-mode)
  ;; (evil-set-initial-state 'inf-ruby-minor-mode 'emacs)

  ;; (add-hook 'compilation-filter-hook #'inf-ruby-auto-enter)

  ;; (add-hook 'compilation-filter-hook #'inf-ruby-auto-enter-and-focus)
  )

(use-package ruby-mode
  :straight nil
  :mode "\\.rb\\'"
  :hook ((ruby-mode . seeing-is-believing)
          (ruby-mode . inf-ruby-minor-mode)
          ))

;; (use-package rbenv
;;   :after ruby-mode
;;   :config
;;   (global-rbenv-mode)
;;   (rbenv-use-global))

(use-package projectile-rails
  :requires projectile
  :hook (ruby-mode . (lambda () (when (projectile-mode) (projectile-rails-on)))))

;; (use-package flymake-ruby)

;; https://melpa.org/#/robe
;; https://github.com/dmexe/emacs-rails-reloaded ; unmaintaned
;; https://www.emacswiki.org/emacs/ri.el ; completion lookup tool
;; https://melpa.org/#/ruby-tools
;; https://melpa.org/#/ruby-refactor
;; https://melpa.org/#/ruby-interpolation
;; https://melpa.org/#/ruby-extra-highlight
;; https://melpa.org/#/ruby-electric
;; https://melpa.org/#/ruby-end
;; https://melpa.org/#/rspec-mode
;; https://melpa.org/#/rbtagger :: code navigation
;; https://melpa.org/#/format-all

(provide 'my-ruby)
;;; my-ruby.el ends here