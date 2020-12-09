(require 'taskjuggler-mode)

(eval-after-load 'taskjuggler-mode
  '(progn
     (setq org-taskjuggler-process-command (concat my/ruby-gems-path "tj3 --silent --no-color --output-dir %o %f"))))

(define-derived-mode my/org-taskjuggler-mode org-mode "TJP"
  "Major mode for TaskJuggler projects."
  ;; (add-hook 'before-save-hook (lambda () (interactive) (org-update-statistics-cookies t)) nil t)
  )

(provide 'my-taskjuggler')