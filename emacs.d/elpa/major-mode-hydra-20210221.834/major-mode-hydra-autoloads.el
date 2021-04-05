;;; major-mode-hydra-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "major-mode-hydra" "../../../../.emacs.d/elpa/major-mode-hydra-20210221.834/major-mode-hydra.el"
;;;;;;  "97588ea4292af628000088780e6c0417")
;;; Generated autoloads from ../../../../.emacs.d/elpa/major-mode-hydra-20210221.834/major-mode-hydra.el

(autoload 'major-mode-hydra-define "major-mode-hydra" "\
Generate a major mode hydra for given MODE with given BODY and HEADS-PLIST.
Overwrite existing hydra if there is one.

MODE can also be a list of modes in which case the same hydras
are created for all these modes.  Useful in multiple closely
related major modes.

Refer to `pretty-hydra-define' for documentation about BODY and HEADS-PLIST.

\(fn MODE BODY HEADS-PLIST)" nil t)

(function-put 'major-mode-hydra-define 'lisp-indent-function 'defun)

(autoload 'major-mode-hydra-define+ "major-mode-hydra" "\
Generate a major mode hydra for given MODE with given BODY and HEADS-PLIST.
Add new heads if there is already an existing one.

MODE can also be a list of modes in which case the same hydras
are created for all these modes.  Useful in multiple closely
related major modes.

Refer to `pretty-hydra-define' for documentation about BODY and HEADS-PLIST.

\(fn MODE BODY HEADS-PLIST)" nil t)

(function-put 'major-mode-hydra-define+ 'lisp-indent-function 'defun)

(autoload 'major-mode-hydra-bind "major-mode-hydra" "\
Add BINDINGS (heads) for a MODE under the COLUMN.

MODE is the major mode name (symbol).  There is no need to quote it.

COLUMN is a string to put the hydra heads under.

BINDINGS is a list of hydra heads to be added.  Each head has
exactly the same structure as that in `pretty-hydra-define' or
`defhydra', except `:exit' is set to t by default.

\(fn MODE COLUMN &rest BINDINGS)" nil t)

(function-put 'major-mode-hydra-bind 'lisp-indent-function '2)

(make-obsolete 'major-mode-hydra-bind 'major-mode-hydra-define+ '"July 2019")

(autoload 'major-mode-hydra "major-mode-hydra" "\
Show the hydra for the current major mode." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "major-mode-hydra"
;;;;;;  "../../../../.emacs.d/elpa/major-mode-hydra-20210221.834/major-mode-hydra.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/major-mode-hydra-20210221.834/major-mode-hydra.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "major-mode-hydra" '("major-mode-hydra-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/major-mode-hydra-20210221.834/major-mode-hydra-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/major-mode-hydra-20210221.834/major-mode-hydra.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; major-mode-hydra-autoloads.el ends here
