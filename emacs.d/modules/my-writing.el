(setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))
(setq save-abbrevs 'silently)

(define-abbrev-table 'global-abbrev-table '(
                                             ("im" "I'm" nil 0)
                                             ("dont" "don't" nil 0)
                                             ("cant" "can't" nil 0)
                                             ("its" "it's" nil 0)
                                             ("Its" "It's" nil 0)
                                             ("couldnt" "couldn't" nil 0)
                                             ("mustnt" "mustn't" nil 0)
                                             ("shouldnt" "shouldn't" nil 0)
                                             ("whats" "what's" nil 0)
                                             ("Whats" "What's" nil 0)
                                             ))

(provide 'my-writing)
