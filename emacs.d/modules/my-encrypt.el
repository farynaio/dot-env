(eval-after-load 'epa
  '(progn
     ;; (require 'epa)
     (require 'epa-file)
     (require 'epg-config)
     ;; (setenv "GPG_AGENT_INFO" nil)
     (setq epa-file-encrypt-to "adamfaryna@gmail.com")
     ;; (setq epa-pinentry-mode 'loopback)
     (setq epa-file-cache-passphrase-for-symmetric-encryption t)

     (epa-file-enable)

     (when (fboundp 'evil-make-overriding-map)
       (evil-make-overriding-map epa-key-mode-map 'motion)
       (evil-make-overriding-map epa-key-mode-map 'state)
       (evil-make-overriding-map epa-key-mode-map 'normal))))

(provide 'my-encrypt)