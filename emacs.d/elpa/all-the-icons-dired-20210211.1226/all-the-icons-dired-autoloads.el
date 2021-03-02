;;; all-the-icons-dired-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "all-the-icons-dired" "all-the-icons-dired.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from all-the-icons-dired.el

(autoload 'all-the-icons-dired-mode "all-the-icons-dired" "\
Display all-the-icons icon for each files in a dired buffer.

If called interactively, toggle `All-The-Icons-Dired mode'.  If
the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "all-the-icons-dired" '("all-the-icons-dired-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; all-the-icons-dired-autoloads.el ends here
