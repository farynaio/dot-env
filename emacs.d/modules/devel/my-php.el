(defhydra hydra-php-debug ()
  "PHP debug"
  ("a" geben-add-current-line-to-predefined-breakpoints "add brk" :exit t)
  ("s" geben "start" :exit t)
  ("q" geben-end "end" :exit t))

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
                       (modify-syntax-entry ?_ "w" (syntax-table))
                       (modify-syntax-entry ?$ "w" (syntax-table))
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
  (defun my-php-symbol-lookup ()
    (interactive)
    (let ((symbol (symbol-at-point)))
      (if (not symbol)
        (message "No symbol at point.")
        (browse-url (concat "http://php.net/manual-lookup.php?pattern=" (symbol-name symbol)))))))

(provide 'my-php)