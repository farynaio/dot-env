(require 'eww)

(eval-after-load 'eww
  '(progn
     (bind-key "/"   #'evil-search-forward eww-mode-map)
     (bind-key "C-s" #'evil-search-forward eww-mode-map)

     (defun af/eww ()
       (interactive)
       (let ((eww-current-buffer (get-buffer "*eww*"))
              (url (read-from-minibuffer "Enter URL or keywords: ")))
         (if (eq eww-current-buffer nil)
           (eww url)
           (switch-to-buffer (generate-new-buffer "eww"))
           (eww-mode)
           (eww url)
           )))

     (defalias 'w #'af/eww)))

(provide 'my-www)
