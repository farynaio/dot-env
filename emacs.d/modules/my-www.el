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

;; (if (executable-find "w3m")
;;   (use-package w3m
;;     :config (progn
;;               (setq
;;                 w3m-default-display-inline-images t
;;                 w3m-use-cookies t
;;                 mm-text-html-renderer 'w3m
;;                 w3m-coding-system 'utf-8
;;                 w3m-file-coding-system 'utf-8
;;                 w3m-file-name-coding-system 'utf-8
;;                 w3m-input-coding-system 'utf-8
;;                 w3m-output-coding-system 'utf-8
;;                 w3m-terminal-coding-system 'utf-8)

;;               (if (string= system-type "darwin")
;;                 (setq process-connection-type nil))
;;               ))
;;   (message (concat "Executable 'w3m' not found!")))

(provide 'my-www)
