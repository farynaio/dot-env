;;; elisp-refs-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "elisp-refs" "../../../../.emacs.d/elpa/elisp-refs-20200815.2357/elisp-refs.el"
;;;;;;  "cc52cab1038ca3e465536d6949708795")
;;; Generated autoloads from ../../../../.emacs.d/elpa/elisp-refs-20200815.2357/elisp-refs.el

(autoload 'elisp-refs-function "elisp-refs" "\
Display all the references to function SYMBOL, in all loaded
elisp files.

If called with a prefix, prompt for a directory to limit the search.

This searches for functions, not macros. For that, see
`elisp-refs-macro'.

\(fn SYMBOL &optional PATH-PREFIX)" t nil)

(autoload 'elisp-refs-macro "elisp-refs" "\
Display all the references to macro SYMBOL, in all loaded
elisp files.

If called with a prefix, prompt for a directory to limit the search.

This searches for macros, not functions. For that, see
`elisp-refs-function'.

\(fn SYMBOL &optional PATH-PREFIX)" t nil)

(autoload 'elisp-refs-special "elisp-refs" "\
Display all the references to special form SYMBOL, in all loaded
elisp files.

If called with a prefix, prompt for a directory to limit the search.

\(fn SYMBOL &optional PATH-PREFIX)" t nil)

(autoload 'elisp-refs-variable "elisp-refs" "\
Display all the references to variable SYMBOL, in all loaded
elisp files.

If called with a prefix, prompt for a directory to limit the search.

\(fn SYMBOL &optional PATH-PREFIX)" t nil)

(autoload 'elisp-refs-symbol "elisp-refs" "\
Display all the references to SYMBOL in all loaded elisp files.

If called with a prefix, prompt for a directory to limit the
search.

\(fn SYMBOL &optional PATH-PREFIX)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "elisp-refs" "../../../../.emacs.d/elpa/elisp-refs-20200815.2357/elisp-refs.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/elisp-refs-20200815.2357/elisp-refs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elisp-refs" '("elisp-refs-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/elisp-refs-20200815.2357/elisp-refs-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/elisp-refs-20200815.2357/elisp-refs.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; elisp-refs-autoloads.el ends here
