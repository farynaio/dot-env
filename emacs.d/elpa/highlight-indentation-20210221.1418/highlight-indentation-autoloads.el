;;; highlight-indentation-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "highlight-indentation" "../../../../.emacs.d/elpa/highlight-indentation-20210221.1418/highlight-indentation.el"
;;;;;;  "9bb83c614f7c9d2e934cb5a366a5d656")
;;; Generated autoloads from ../../../../.emacs.d/elpa/highlight-indentation-20210221.1418/highlight-indentation.el

(autoload 'highlight-indentation-mode "highlight-indentation" "\
Highlight indentation minor mode highlights indentation based on spaces

If called interactively, enable Highlight-indentation mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'highlight-indentation-set-offset "highlight-indentation" "\
Set indentation offset locally in buffer, will prevent
highlight-indentation from trying to guess indentation offset
from major mode

\(fn OFFSET)" t nil)

(autoload 'highlight-indentation-current-column-mode "highlight-indentation" "\
Highlight Indentation minor mode displays a vertical bar
corresponding to the indentation of the current line

If called interactively, enable
Highlight-indentation-current-column mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp, also
enable the mode if ARG is omitted or nil, and toggle it if ARG is
`toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "highlight-indentation"
;;;;;;  "../../../../.emacs.d/elpa/highlight-indentation-20210221.1418/highlight-indentation.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/highlight-indentation-20210221.1418/highlight-indentation.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "highlight-indentation" '("highlight-indentation-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/highlight-indentation-20210221.1418/highlight-indentation-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/highlight-indentation-20210221.1418/highlight-indentation.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; highlight-indentation-autoloads.el ends here
