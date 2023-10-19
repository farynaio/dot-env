
;;; Code:

(use-package inf-ruby
  :commands inf-ruby-minor-mode
  :config
  (add-hook 'compilation-filter-hook #'inf-ruby-auto-enter)
  (add-to-list 'evil-emacs-state-modes 'inf-ruby-mode)
  ;; (add-hook 'compilation-filter-hook #'inf-ruby-auto-enter-and-focus)
  )

(use-package ruby-mode
  :straight nil
  :mode "\\.rb\\'"
  :hook ((ruby-mode . inf-ruby-minor-mode)
          ;; (ruby-mode . lsp-deferred)
          (ruby-mode . eglot-ensure))
  :config
  (major-mode-hydra-define+ ruby-mode
    (:hint nil :color amaranth :quit-key "q" :title (with-alltheicon "ruby" "Ruby" 1 -0.05))
    ("Assess"
      (("s" my/hydra-seeing-is-believing/body "toggle seeing-is-believing" :exit t)
        ("d" eldoc-box-help-at-point "show eldoc" :exit t))))

  (pretty-hydra-define my/hydra-seeing-is-believing
    (:hint nil :color amaranth :quit-key "q" :title (with-faicon "eye" "Seeing is believing" 1 -0.05))
    ("Assess"
      (("t" seeing-is-believing "toggle" :toggle t :exit t)
        ("r" seeing-is-believing-run "run")
        ("c" seeing-is-believing-clear "clear" :exit t)))))

(use-package seeing-is-believing
  :commands seeing-is-believing)

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

;; (use-package robe
;; :config
;; (progn
;;   (add-hook 'robe-mode-hook (lambda ()
;; (robe-start)
;;                               (make-local-variable 'company-backends)
;;                               (add-to-list 'company-backends 'company-robe t)))
;;   (add-hook 'ruby-mode-hook 'robe-mode)
;;   ))

(provide 'my-ruby)
;;; my-ruby.el ends here