(require 'epa)
(require 'epa-file)
(require 'epg-config)

(eval-after-load 'epa
  '(progn
     (setenv "GPG_AGENT_INFO" nil)

     (setq
       epa-pinentry-mode 'loopback
       epa-file-cache-passphrase-for-symmetric-encryption t
       )

     (epa-file-enable)
     (evil-make-overriding-map epa-key-mode-map 'motion)
     (evil-make-overriding-map epa-key-mode-map 'state)
     (evil-make-overriding-map epa-key-mode-map 'normal)
     ))

(provide 'my-encrypt)