
;;; Code:

(use-package taskjuggler-mode
  :straight (:type built-in)
  :custom
  (org-taskjuggler-process-command (concat my/ruby-gems-path "tj3 --silent --no-color --output-dir %o %f"))
  :config
(define-derived-mode my/org-taskjuggler-mode org-mode "TJP"
  "Major mode for TaskJuggler projects."
  (lambda ())
  ;; (add-hook 'before-save-hook (lambda () (interactive) (org-update-statistics-cookies t)) nil t)
  ))

(provide 'my-taskjuggler)
;;; my-taskjuggler.el ends here