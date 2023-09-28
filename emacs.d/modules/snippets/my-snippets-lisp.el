(eval-after-load 'tempo
  '(progn
     (defvar my/tempo-lisp-tags nil
       "Tags for lisp mode.")

     (tempo-define-template "lisp-todo"
       '(";; TODO " ~)
       "todo"
       "Insert TODO block"
       'my/tempo-lisp-tags)
))

(provide 'my-snippets-lisp)
;;; my-snippets-lisp.el ends here