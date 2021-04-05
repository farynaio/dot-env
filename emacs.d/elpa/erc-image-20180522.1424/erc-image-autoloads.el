;;; erc-image-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "erc-image" "../../../../.emacs.d/elpa/erc-image-20180522.1424/erc-image.el"
;;;;;;  "16759d7922ba199ddeef2c7f1e2763e8")
;;; Generated autoloads from ../../../../.emacs.d/elpa/erc-image-20180522.1424/erc-image.el

(eval-after-load 'erc '(define-erc-module image nil "Display inlined images in ERC buffer" ((add-hook 'erc-insert-modify-hook 'erc-image-show-url t) (add-hook 'erc-send-modify-hook 'erc-image-show-url t)) ((remove-hook 'erc-insert-modify-hook 'erc-image-show-url) (remove-hook 'erc-send-modify-hook 'erc-image-show-url)) t))

;;;### (autoloads "actual autoloads are elsewhere" "erc-image" "../../../../.emacs.d/elpa/erc-image-20180522.1424/erc-image.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/erc-image-20180522.1424/erc-image.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "erc-image" '("erc-image-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/erc-image-20180522.1424/erc-image-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/erc-image-20180522.1424/erc-image.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; erc-image-autoloads.el ends here
