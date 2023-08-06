(add-hook 'profiler-report-mode-hook (lambda () (hl-line-mode 1)))

(defun my/occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

(provide 'my-debug)