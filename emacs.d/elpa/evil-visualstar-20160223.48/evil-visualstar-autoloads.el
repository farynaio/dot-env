;;; evil-visualstar-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-visualstar" "../../../../.emacs.d/elpa/evil-visualstar-20160223.48/evil-visualstar.el"
;;;;;;  "376d559483619aeaf075b78d3e7696c3")
;;; Generated autoloads from ../../../../.emacs.d/elpa/evil-visualstar-20160223.48/evil-visualstar.el

(autoload 'evil-visualstar-mode "evil-visualstar" "\
Minor mode for visual star selection.

If called interactively, enable Evil-Visualstar mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(put 'global-evil-visualstar-mode 'globalized-minor-mode t)

(defvar global-evil-visualstar-mode nil "\
Non-nil if Global Evil-Visualstar mode is enabled.
See the `global-evil-visualstar-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-evil-visualstar-mode'.")

(custom-autoload 'global-evil-visualstar-mode "evil-visualstar" nil)

(autoload 'global-evil-visualstar-mode "evil-visualstar" "\
Toggle Evil-Visualstar mode in all buffers.
With prefix ARG, enable Global Evil-Visualstar mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Evil-Visualstar mode is enabled in all buffers where
`turn-on-evil-visualstar-mode' would do it.
See `evil-visualstar-mode' for more information on Evil-Visualstar mode.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-evil-visualstar-mode "evil-visualstar" "\
Turns on visual star selection." t nil)

(autoload 'turn-off-evil-visualstar-mode "evil-visualstar" "\
Turns off visual star selection." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "evil-visualstar"
;;;;;;  "../../../../.emacs.d/elpa/evil-visualstar-20160223.48/evil-visualstar.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/evil-visualstar-20160223.48/evil-visualstar.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-visualstar" '("evil-visualstar/")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/evil-visualstar-20160223.48/evil-visualstar-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/evil-visualstar-20160223.48/evil-visualstar.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-visualstar-autoloads.el ends here
