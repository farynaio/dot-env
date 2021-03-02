;;; artbollocks-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "artbollocks-mode" "artbollocks-mode.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from artbollocks-mode.el

(autoload 'artbollocks-mode "artbollocks-mode" "\
Highlight passive voice, weasel words and artbollocks jargon in text, and provide useful text metrics

If called interactively, toggle `Artbollocks mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "artbollocks-mode" '("artbollocks-" "interactive-optional-region"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; artbollocks-mode-autoloads.el ends here
