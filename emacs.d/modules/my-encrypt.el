;;; Code:

(use-package epa
  :straight nil
  :custom
  (epa-file-cache-passphrase-for-symmetric-encryption t)
  :config
  (require 'epa-file)
  (require 'epg-config)
  ;; (setenv "GPG_AGENT_INFO" nil)
  ;; (setq epa-pinentry-mode 'loopback)

  ;; (epa-file-enable)

  (when (fboundp 'evil-make-overriding-map)
    (evil-make-overriding-map epa-key-mode-map 'motion)
    (evil-make-overriding-map epa-key-mode-map 'state)
    (evil-make-overriding-map epa-key-mode-map 'normal)))

(provide 'my-encrypt)
;;; my-encrypt.el ends here