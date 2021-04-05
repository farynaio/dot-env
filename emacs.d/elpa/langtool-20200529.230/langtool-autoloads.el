;;; langtool-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "langtool" "../../../../.emacs.d/elpa/langtool-20200529.230/langtool.el"
;;;;;;  "a7cc91eb07f819f656e0c6ade9811e08")
;;; Generated autoloads from ../../../../.emacs.d/elpa/langtool-20200529.230/langtool.el

(defalias 'langtool-check 'langtool-check-buffer)

(autoload 'langtool-check-buffer "langtool" "\
Check context current buffer and light up errors.
Optional \\[universal-argument] read LANG name.

You can change the `langtool-default-language' to apply all session.
Restrict to selection when region is activated.

\(fn &optional LANG)" t nil)

(autoload 'langtool-switch-default-language "langtool" "\
Switch `langtool-default-language' to LANG

\(fn LANG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "langtool" "../../../../.emacs.d/elpa/langtool-20200529.230/langtool.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/langtool-20200529.230/langtool.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "langtool" '("langtool-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/langtool-20200529.230/langtool-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/langtool-20200529.230/langtool.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; langtool-autoloads.el ends here
