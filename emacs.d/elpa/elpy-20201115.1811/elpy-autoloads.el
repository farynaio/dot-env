;;; elpy-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "elpy" "../../../../.emacs.d/elpa/elpy-20201115.1811/elpy.el"
;;;;;;  "3f35959a49e794dacde846d234155c34")
;;; Generated autoloads from ../../../../.emacs.d/elpa/elpy-20201115.1811/elpy.el

(autoload 'elpy-enable "elpy" "\
Enable Elpy in all future Python buffers.

\(fn &optional IGNORED)" t nil)

(autoload 'elpy-mode "elpy" "\
Minor mode in Python buffers for the Emacs Lisp Python Environment.

If called interactively, enable Elpy mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp, also
enable the mode if ARG is omitted or nil, and toggle it if ARG is
`toggle'; disable the mode otherwise.

This mode fully supports virtualenvs. Once you switch a
virtualenv using \\[pyvenv-workon], you can use
\\[elpy-rpc-restart] to make the elpy Python process use your
virtualenv.

\\{elpy-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'elpy-config "elpy" "\
Configure Elpy.

This function will pop up a configuration buffer, which is mostly
a customize buffer, but has some more options." t nil)

(autoload 'elpy-version "elpy" "\
Display the version of Elpy." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "elpy" "../../../../.emacs.d/elpa/elpy-20201115.1811/elpy.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/elpy-20201115.1811/elpy.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elpy" '("elpy-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "elpy-django"
;;;;;;  "../../../../.emacs.d/elpa/elpy-20201115.1811/elpy-django.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/elpy-20201115.1811/elpy-django.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elpy-django" '("elpy-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "elpy-profile"
;;;;;;  "../../../../.emacs.d/elpa/elpy-20201115.1811/elpy-profile.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/elpy-20201115.1811/elpy-profile.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elpy-profile" '("elpy-profile-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "elpy-refactor"
;;;;;;  "../../../../.emacs.d/elpa/elpy-20201115.1811/elpy-refactor.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/elpy-20201115.1811/elpy-refactor.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elpy-refactor" '("elpy-refactor-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "elpy-rpc" "../../../../.emacs.d/elpa/elpy-20201115.1811/elpy-rpc.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/elpy-20201115.1811/elpy-rpc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elpy-rpc" '("elpy-" "with-elpy-rpc-virtualenv-activated")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "elpy-shell" "../../../../.emacs.d/elpa/elpy-20201115.1811/elpy-shell.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/elpy-20201115.1811/elpy-shell.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elpy-shell" '("elpy-")))

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/elpy-20201115.1811/elpy-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/elpy-20201115.1811/elpy-django.el"
;;;;;;  "../../../../.emacs.d/elpa/elpy-20201115.1811/elpy-pkg.el"
;;;;;;  "../../../../.emacs.d/elpa/elpy-20201115.1811/elpy-profile.el"
;;;;;;  "../../../../.emacs.d/elpa/elpy-20201115.1811/elpy-refactor.el"
;;;;;;  "../../../../.emacs.d/elpa/elpy-20201115.1811/elpy-rpc.el"
;;;;;;  "../../../../.emacs.d/elpa/elpy-20201115.1811/elpy-shell.el"
;;;;;;  "../../../../.emacs.d/elpa/elpy-20201115.1811/elpy.el") (0
;;;;;;  0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; elpy-autoloads.el ends here
