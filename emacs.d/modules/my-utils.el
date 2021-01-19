(defun my/func-call (&rest func)
  (interactive)
  (dolist (i func)
    (if (listp i)
      (apply (car i) (cdr i))
      (funcall i))))

;; https://emacs.stackexchange.com/a/18515/18445
(defun my/online-p (&optional host)
    (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1"
                       (if host host "www.google.com"))))

(provide 'my-utils)