;;; smartscan-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "smartscan" "smartscan.el" (0 0 0 0))
;;; Generated autoloads from smartscan.el

(autoload 'smartscan-symbol-go-forward "smartscan" "\
Jumps forward to the next symbol at point" t nil)

(autoload 'smartscan-symbol-go-backward "smartscan" "\
Jumps backward to the previous symbol at point" t nil)

(autoload 'smartscan-symbol-replace "smartscan" "\
Replaces the symbol at point with another string in the entire buffer.

With C-u the scope is limited to the current defun, as defined by
`narrow-to-defun'.

This function uses `search-forward' and `replace-match' to do the
actual work.

\(fn ARG)" t nil)

(autoload 'smartscan-mode "smartscan" "\
Jumps between other symbols found at point.

If called interactively, toggle `Smartscan mode'.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

When Smart Scan mode is enabled, you can jump between the all the
symbols in your current buffer that point is on.

You can customize Smart Scan by editing
`smartscan-use-extended-syntax' and `smartscan-symbol-selector'.

Key bindings:
\\{smartscan-map}

\(fn &optional ARG)" t nil)

(put 'global-smartscan-mode 'globalized-minor-mode t)

(defvar global-smartscan-mode nil "\
Non-nil if Global Smartscan mode is enabled.
See the `global-smartscan-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-smartscan-mode'.")

(custom-autoload 'global-smartscan-mode "smartscan" nil)

(autoload 'global-smartscan-mode "smartscan" "\
Toggle Smartscan mode in all buffers.
With prefix ARG, enable Global Smartscan mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if ARG is
omitted or nil.

Smartscan mode is enabled in all buffers where
`smartscan-mode-turn-on' would do it.

See `smartscan-mode' for more information on Smartscan mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "smartscan" '("smartscan-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; smartscan-autoloads.el ends here
