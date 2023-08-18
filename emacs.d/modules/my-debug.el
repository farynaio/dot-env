
;;; Code:

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
    (format "%.2f seconds"
      (float-time
        (time-subtract after-init-time before-init-time)))
    gcs-done))

(add-hook 'emacs-startup-hook 'efs/display-startup-time)

(use-package profiler
  :straight nil
  :commands profiler-start
  :hook ((profiler-report-mode . (lambda () (hl-line-mode 1)))))

(use-package free-keys
  :commands free-keys)

(use-package edebug
  :straight nil
  :commands edebug-on-entry
  :hook ((edebug-mode . edebug-x-mode)
          (edebug-mode . edebug-step-mode)
          (edebug-mode . edebug-inline-result-mode))
  :config
  (evil-make-overriding-map edebug-mode-map '(normal motion))
  (add-hook 'edebug-mode-hook 'evil-normalize-keymaps))

(use-package edebug-x
  :commands edebug-x-mode)

(use-package edebug-inline-result
  :commands edebug-inline-result-mode)

;; (defun my/occur-non-ascii ()
;;   "Find any non-ascii characters in the current buffer."
;;   (interactive)
;;   (occur "[^[:ascii:]]"))

(provide 'my-debug)
;;; my-debug.el ends here