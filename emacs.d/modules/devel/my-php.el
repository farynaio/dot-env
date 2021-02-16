;; TODO what it does?
(use-package company-php
  :requires company-mode)

;; (defun my/toggle-php-flavor-mode ()
;;   (interactive)
;;   "Toggle mode between PHP & Web-Mode Helper modes"
;;   (cond ((string= mode-name "PHP")
;;           (web-mode))
;;     ((string= mode-name "Web")
;;       (php-mode))))

(defun my/toggle-php-flavor-mode ()
  "Toggle mode between PHP & Web-Mode Helper modes."
  (interactive)
  (cond ((string= major-mode "php-mode")
          (web-mode))
    ((string= major-mode "web-mode")
      (php-mode))))

(use-package php-mode
  :hook ((php-mode . emmet-mode)
          (php-mode . (lambda ()
                       (make-local-variable 'company-backends)
                       (add-to-list 'company-backends 'company-ac-php-backend t)
                       (local-set-key (kbd "<f1>") 'my-php-symbol-lookup)
                       (setq php-template-compatibility nil)
                       ))
          (php-mode . (lambda ()
                      (defun ywb-php-lineup-arglist-intro (langelem)
                        (save-excursion
                          (goto-char (cdr langelem))
                          (vector (+ (current-column) c-basic-offset))))
                      (defun ywb-php-lineup-arglist-close (langelem)
                        (save-excursion
                          (goto-char (cdr langelem))
                          (vector (current-column))))
                      (c-set-offset 'arglist-intro 'ywb-php-lineup-arglist-intro)
                        (c-set-offset 'arglist-close 'ywb-php-lineup-arglist-close))))
  :bind (:map php-mode-map
          ("<f5>" . my/toggle-php-flavor-mode))
  :mode ("\\.php\\'" "\\.inc\\'")
  :config
  (evil-define-key 'normal php-mode-map
    ",d" #'hydra-php-debug/body
    ",t" #'my/toggle-php-flavor-mode)

  (defun my/php-setup ()
    (web-mode)
    (make-local-variable 'web-mode-code-indent-offset)
    (make-local-variable 'web-mode-markup-indent-offset)
    (make-local-variable 'web-mode-css-indent-offset))

  (add-hook 'php-mode-hook 'my/php-setup)

  (defun my-php-symbol-lookup ()
    (interactive)
    (let ((symbol (symbol-at-point)))
      (if (not symbol)
        (message "No symbol at point.")
        (browse-url (concat "http://php.net/manual-lookup.php?pattern=" (symbol-name symbol)))))))

(provide 'my-php)