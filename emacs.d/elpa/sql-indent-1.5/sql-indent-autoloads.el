;;; sql-indent-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "sql-indent" "../../../../.emacs.d/elpa/sql-indent-1.5/sql-indent.el"
;;;;;;  "97abac29a0f95073828f2170afeae681")
;;; Generated autoloads from ../../../../.emacs.d/elpa/sql-indent-1.5/sql-indent.el

(autoload 'sqlind-minor-mode "sql-indent" "\
Toggle SQL syntactic indentation on or off.
With syntactic indentation, hitting TAB on a line in a SQL buffer
will indent the line according to the syntactic context of the
SQL statement being edited.

If called interactively, enable Sqlind minor mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

A set of alignment rules are also enabled with this minor mode.
Selecting a region of text and typing `M-x align RET` will align
the statements.  This can be used, for example, to align the 'as'
column aliases in select statements.

\(fn &optional ARG)" t nil)

(autoload 'sqlind-setup "sql-indent" "\
Enable SQL syntactic indentation unconditionally.
This function is deprecated, consider using the function
`sqlind-minor-mode' instead." nil nil)

;;;### (autoloads "actual autoloads are elsewhere" "sql-indent" "../../../../.emacs.d/elpa/sql-indent-1.5/sql-indent.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/sql-indent-1.5/sql-indent.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sql-indent" '("sqlind-")))

;;;***

;;;***

;;;### (autoloads nil "sql-indent-left" "../../../../.emacs.d/elpa/sql-indent-1.5/sql-indent-left.el"
;;;;;;  "d666ef9520e32a537ac7b41ecc59e510")
;;; Generated autoloads from ../../../../.emacs.d/elpa/sql-indent-1.5/sql-indent-left.el

(autoload 'sqlind-setup-style-left "sql-indent-left" "\
Define an sql-indentation style where keywords are left aligned." t nil)

(autoload 'sqlind-setup-style-right "sql-indent-left" "\
Define an sql-indentation style where keywords are right aligned." t nil)

(autoload 'sqlind-setup-style-default "sql-indent-left" "\
Define an sql-indentation style where keywords are right aligned." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "sql-indent-left"
;;;;;;  "../../../../.emacs.d/elpa/sql-indent-1.5/sql-indent-left.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/sql-indent-1.5/sql-indent-left.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sql-indent-left" '("indent-case-statement-items" "sqlind-indent")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/sql-indent-1.5/sql-indent-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/sql-indent-1.5/sql-indent-left.el"
;;;;;;  "../../../../.emacs.d/elpa/sql-indent-1.5/sql-indent-pkg.el"
;;;;;;  "../../../../.emacs.d/elpa/sql-indent-1.5/sql-indent.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sql-indent-autoloads.el ends here
