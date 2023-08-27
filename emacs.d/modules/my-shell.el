
;;; Code:

;; (eval-after-load 'eshell
  ;; '(progn
     ;; (evil-make-overriding-map eshell-mode-map 'motion)
     ;; (evil-make-overriding-map eshell-mode-map 'normal)))

;; (setq explicit-shell-file-name "/usr/local/bin/bash")

(defalias 'sh #'eshell)

(use-package eshell
  :straight nil
  :commands eshell
  :bind (:map eshell-mode-map
          ("C-r" . counsel-shell-history)
          ("C-n" . company-next-page)
          ("C-p" . company-previous-page)

          ;; ("<tab>" . my/eshell-list-dir)
          ;; ("<tab>" . company-next-page)
          ;; ("<backtab>" . company-previous-page)
          )
  :hook ((eshell-mode . company-mode))
  :custom
  (eshell-destroy-buffer-when-process-dies t)
  (eshell-prompt-function #'dw/eshell-prompt)
  (eshell-prompt-regexp "^λ ")
  (eshell-history-size 10000)
  (eshell-buffer-maximum-lines 10000)
  (eshell-hist-ignoredups t)
  (eshell-highlight-prompt t)
  (eshell-scroll-to-bottom-on-input t)
  (eshell-prefer-lisp-functions nil)
:config
  (defun my/eshell-init ()
    (setq-local company-backends '(company-files company-capf))
    ;; (setq-local company-backends '(esh-autosuggest company-files ))
    (setenv "PAGER" "cat"))
  (add-hook 'eshell-mode-hook #'my/eshell-init)

  (defun read-file (file-path)
    (with-temp-buffer
      (insert-file-contents file-path)
      (buffer-string)))

  (defun my/eshell-list-dir ()
    (interactive)
    (let ((output (eshell-command "ls -a" t)))
      (with-temp-message output)))

  (defun dw/get-current-package-version ()
    (interactive)
    (let ((package-json-file (concat (eshell/pwd) "/package.json")))
      (when (file-exists-p package-json-file)
        (let* ((package-json-contents (read-file package-json-file))
                (package-json (ignore-errors (json-parse-string package-json-contents))))
          (when package-json
            (ignore-errors (gethash "version" package-json)))))))

  (defun dw/map-line-to-status-char (line)
    (cond ((string-match "^?\\? " line) "?")))

  (defun dw/get-git-status-prompt ()
    (let ((status-lines (cdr (process-lines "git" "status" "--porcelain" "-b"))))
      (seq-uniq (seq-filter 'identity (mapcar 'dw/map-line-to-status-char status-lines)))))

  (defun dw/get-prompt-path ()
    (let* ((current-path (eshell/pwd))
            (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
            (has-path (not (string-match "^fatal" git-output))))
      (if (not has-path)
        (abbreviate-file-name current-path)
        (string-remove-prefix (file-name-directory git-output) current-path))))

  ;; This prompt function mostly replicates my custom zsh prompt setup
  ;; that is powered by github.com/denysdovhan/spaceship-prompt.
  (defun dw/eshell-prompt ()
    (let ((current-branch (magit-get-current-branch))
           (package-version (dw/get-current-package-version)))
      (concat
        "\n"
        (propertize (system-name) 'face `(:foreground "#62aeed"))
        (propertize " ॐ " 'face `(:foreground "white"))
        (propertize (dw/get-prompt-path) 'face `(:foreground "#82cfd3"))
        (when current-branch
          (concat
            (propertize " • " 'face `(:foreground "white"))
            (propertize (concat " " current-branch) 'face `(:foreground "#c475f0"))))
        (when package-version
          (concat
            (propertize " @ " 'face `(:foreground "white"))
            (propertize package-version 'face `(:foreground "#e8a206"))))
        (propertize " • " 'face `(:foreground "white"))
        (propertize (format-time-string "%I:%M:%S %p") 'face `(:foreground "#5a5b7f"))
        (if (= (user-uid) 0)
          (propertize "\n#" 'face `(:foreground "red2"))
          (propertize "\nλ" 'face `(:foreground "#aece4a")))
        (propertize " " 'face `(:foreground "white")))))

  (use-package xterm-color)

  (push 'eshell-tramp eshell-modules-list)
  ;; (push 'xterm-color-filter eshell-preoutput-filter-functions)
  ;; (delq 'eshell-handle-ansi-color eshell-output-filter-functions)

  ;; (defun flush-func (&optional args) t)

  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook #'eshell-save-some-history)

  (add-hook 'eshell-before-prompt-hook
    (lambda ()
      (setq-local xterm-color-preserve-properties t)))

  ;; Truncate buffer for performance
  ;; (add-to-list 'eshell-output-filter-functions #'eshell-truncate-buffer)

  ;; We want to use xterm-256color when running interactive commands
  ;; in eshell but not during other times when we might be launching
  ;; a shell command to gather its output.
  (add-hook 'eshell-pre-command-hook
    (lambda () (setenv "TERM" "xterm-256color")))
  (add-hook 'eshell-post-command-hook
    (lambda () (setenv "TERM" "dumb"))))

(use-package eshell-syntax-highlighting
  :after eshell
  :config
  (eshell-syntax-highlighting-global-mode 1))

;; (use-package esh-autosuggest
;;   :commands esh-autosuggest-mode
;;   :custom
;;   (esh-autosuggest-delay 0.5)
;;   :config
;;   (set-face-foreground 'company-preview-common "#4b5668")
;;   (set-face-background 'company-preview nil))

(use-package eshell-toggle
  :after eshell
  :commands eshell-toggle
  :bind (:map evil-normal-state-map
          ("C-`" . eshell-toggle)
         :map eshell-mode-map
          ("C-`" . eshell-toggle))
  ;; :straight (:type: git
  ;;             :host github
  ;;             :repo "4DA/eshell-toggle"
  ;;             :branch "master")
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  (eshell-toggle-default-directory "~/"))

(use-package vterm
  :disabled t
  :commands vterm
  :custom
  (vterm-max-scrollback 10000)
  :config
  (advice-add 'evil-collection-vterm-insert :before #'vterm-reset-cursor-point))

(provide 'my-shell)
;;; my-shell.el ends here