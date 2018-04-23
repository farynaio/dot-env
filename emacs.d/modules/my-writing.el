(setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))
(setq save-abbrevs 'silently)

(define-abbrev-table 'global-abbrev-table '(
  ("im" "I'm" nil 0)))

(provide 'my-writing)
