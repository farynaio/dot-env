(eval-after-load 'tempo
  '(progn
     (defvar my/tempo-general-tags nil
       "Tags for all modes.")

     (tempo-define-template "general-file-vars"
       '("-*- " ~ " -*-")
       "filev"
       "Insert file variables block"
       'my/tempo-general-tags)
     ))

(provide 'my-snippets-general)
;;; my-snippets-general.el ends here