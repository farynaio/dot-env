(defun my/func-call (&rest func)
  (interactive)
  (dolist (i func)
    (if (listp i)
      (apply (car i) (cdr i))
      (funcall i))))

(provide 'my-utils)