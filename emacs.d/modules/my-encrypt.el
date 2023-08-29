;;; Code:

(use-package epa
  :straight nil
  :custom
  (epg-pinentry-mode 'loopback)
  (epa-file-select-keys nil)
  ;; (epa-file-cache-passphrase-for-symmetric-encryption t)
  :config
  (require 'epa-file)
  (require 'epg-config)
  ;; (setenv "GPG_AGENT_INFO" nil)
  ;; (setq epa-pinentry-mode 'loopback)

  (add-hook 'find-file-hook
    (lambda ()
      (when (and (stringp buffer-file-name) (string-match "\\.gpg\\'" buffer-file-name))
        (unless (alist-get 'epa-file-encrypt-to file-local-variables-alist)
          (when (boundp 'my/epa-file-encrypt-to-default) my/epa-file-encrypt-to-default
            (setq-local epa-file-encrypt-to my/epa-file-encrypt-to-default))))))

  ;; (epa-file-enable)

  (when (fboundp 'evil-make-overriding-map)
    (evil-make-overriding-map epa-key-mode-map 'motion)
    (evil-make-overriding-map epa-key-mode-map 'state)
    (evil-make-overriding-map epa-key-mode-map 'normal)))

(provide 'my-encrypt)
;;; my-encrypt.el ends here