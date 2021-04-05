;;; git-gutter-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "git-gutter" "../../../../.emacs.d/elpa/git-gutter-20210127.1100/git-gutter.el"
;;;;;;  "6fc6a07d433ec897db6c88016530ffb6")
;;; Generated autoloads from ../../../../.emacs.d/elpa/git-gutter-20210127.1100/git-gutter.el

(autoload 'git-gutter:linum-setup "git-gutter" "\
Setup for linum-mode." nil nil)

(autoload 'git-gutter-mode "git-gutter" "\
Git-Gutter mode

If called interactively, enable Git-Gutter mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp, also
enable the mode if ARG is omitted or nil, and toggle it if ARG is
`toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(put 'global-git-gutter-mode 'globalized-minor-mode t)

(defvar global-git-gutter-mode nil "\
Non-nil if Global Git-Gutter mode is enabled.
See the `global-git-gutter-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-git-gutter-mode'.")

(custom-autoload 'global-git-gutter-mode "git-gutter" nil)

(autoload 'global-git-gutter-mode "git-gutter" "\
Toggle Git-Gutter mode in all buffers.
With prefix ARG, enable Global Git-Gutter mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Git-Gutter mode is enabled in all buffers where
`git-gutter--turn-on' would do it.
See `git-gutter-mode' for more information on Git-Gutter mode.

\(fn &optional ARG)" t nil)

(autoload 'git-gutter "git-gutter" "\
Show diff information in gutter" t nil)

(autoload 'git-gutter:toggle "git-gutter" "\
Toggle to show diff information." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "git-gutter" "../../../../.emacs.d/elpa/git-gutter-20210127.1100/git-gutter.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/git-gutter-20210127.1100/git-gutter.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "git-gutter" '("git-gutter")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/git-gutter-20210127.1100/git-gutter-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/git-gutter-20210127.1100/git-gutter.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; git-gutter-autoloads.el ends here
