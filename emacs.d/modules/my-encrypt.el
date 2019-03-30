(require 'epa)
(require 'epa-file)
(require 'epg-config)

(eval-after-load 'epa
  '(progn
     (setq epa-file-cache-passphrase-for-symmetric-encryption t)

     (epa-file-enable)
     (evil-make-overriding-map epa-key-mode-map 'motion)
     (evil-make-overriding-map epa-key-mode-map 'state)
     (evil-make-overriding-map epa-key-mode-map 'normal)
     ))

(provide 'my-encrypt)