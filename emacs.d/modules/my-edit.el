(setq-default
  cursor-in-non-selected-windows t
  display-time-default-load-average nil
  scroll-conservatively most-positive-fixnum ; Always scroll by one line
  view-read-only t
  require-final-newline nil
  indent-tabs-mode nil
  mode-require-final-newline nil)

(setq auto-save-no-message t)

(defalias 'yes-or-no-p #'y-or-n-p)
(defalias 'ar #'align-regexp)

(setq-default tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(defvaralias 'sgml-basic-offset 'tab-width)

(setq
  backward-delete-char-untabify-method 'hungry)

(setq visual-line-fringe-indicators '(left-curly-arrow nil))

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :font "Source Code Pro Medium")
  (set-fontset-font t 'latin "Noto Sans"))

(setq
  hscroll-margin  1
  hscroll-step 1
  word-wrap t
  shift-select-mode nil
  compare-ignore-case t
  compare-ignore-whitespace t
  sentence-end-double-space nil
  revert-without-query '(".*")
  undo-limit 160000
  show-paren-delay 0
  shell-dirtrackp nil
  save-some-buffers-default-predicate t
  help-window-select t
  bookmark-save-flag nil)

(setq calc-internal-prec 20)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'same-window-buffer-names "*SQL*")

(defun my/copy-region-or-word ()
  "Copy the selected region or the word below the cursor."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (let ((word (thing-at-point 'word)))
      (when word
        (kill-new word)
        (message "Copied word '%s'." word)))))

(use-package auth-source
  :ensure nil
  :straight nil
  ;; :config
  ;; (auth-source-debug 'trivi) ;; for debug info in *Messages*
  )

(use-package elec-pair
  :ensure nil
  :straight nil
  :config
  (push '(?\" . ?\") electric-pair-pairs)
  (push '(?\{ . ?\}) electric-pair-pairs)
  (push '(?\` . ?\`) electric-pair-pairs)
  (push '(?\( . ?\)) electric-pair-pairs)
  ;; (push '(?\" . ?\") electric-pair-text-pairs)
  (push '(?\{ . ?\}) electric-pair-text-pairs)
  ;; (push '(?\' . ?\') electric-pair-text-pairs)
  (push '(?\` . ?\`) electric-pair-text-pairs)
  (electric-pair-mode 1))

(use-package hungry-delete
  :init
  (setq-default hungry-delete-join-reluctantly t)
  :config
  (global-hungry-delete-mode))

(use-package with-editor)  ; dependency for other package

(use-package free-keys)

;; (use-package emojify
;;   :hook (org-mode . emojify-mode))

(use-package company
  :hook (prog-mode . company-mode)
  :demand t
  :diminish company-mode
  :custom
  (company-idle-delay 0.5)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 1)
  ;; (company-backends '(company-capf))
  (company-backends '((company-capf company-files company-keywords company-dabbrev-code :separate :with company-tempo)))
  (company-files-exclusions '(".git/" ".DS_Store")))

(use-package company-statistics
  :disabled t
  :after company
  :hook ((company-mode . company-statistics-mode)))

;; TODO needs key mapping
;; (use-package company-try-hard
;;   :after company)

(use-package company-quickhelp
  :disabled t
  :after company
  :hook ((company-mode . company-quickhelp-mode))
  :custom
  (company-quickhelp-delay nil)) ;; Invoke popup via shortcut

;; Used to hang emacs?
(use-package which-key
  :defer 0.2
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 0.5)
  :config
  (which-key-mode 1))

(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C-+" . er/contract-region)))

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

(use-package editorconfig
  :defer 0.3
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package ediff
  :commands ediff
  :ensure nil
  :straight nil
  :config
  (ediff-diff-options "-w")
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-forward-word-function 'forward-char))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
          ("C-h v" . helpful-variable)
          ("C-h k" . helpful-key)
          ("C-c C-d" . helpful-at-point))
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

(use-package savehist
  :ensure nil
  :straight nil
  :custom
  (savehist-file "~/.emacs.d/savehist")
  (history-length 100)
  (history-delete-duplicates t)
  ;; (auto-save-visited-interval 60)
  (savehist-save-minibuffer-history 1)
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  :config
  (savehist-mode 1))

;; https://github.com/xenodium/chatgpt-shell
(use-package chatgpt-shell
  :commands chatgpt-shell
  :custom
  (chatgpt-shell-openai-key (lambda () (auth-source-pick-first-password :host "openai-api-key-anon")))
  ;; dall-e-shell-openai-key
(defun my/chatgpt-shell-start-new ()
  "If `chatgpt-shell` session exists kill it, and start new session."
  (interactive)
  (let ((chatgpt-buffer (seq-find (lambda (item) (string-prefix-p "*chatgpt" (buffer-name item))) (buffer-list))))
    (when chatgpt-buffer
      (kill-buffer chatgpt-buffer)))
  (chatgpt-shell))

  (defalias 'ai 'my/chatgpt-shell-start-new))

(use-package ob-chatgpt-shell
  :after chatgpt-shell
  :config
  (ob-chatgpt-shell-setup))

(use-package gptel
  :disabled t
  :commands gptel
  :custom
  (gptel-api-key (lambda () (auth-source-pick-first-password :host "openai-api-key-anon"))))

(use-package starhugger
  :defer 0.2
  :custom
  (starhugger-api-token (lambda () (auth-source-pick-first-password :host "huggingface.co-api-key-anon")))
  ;; :config
  ;; resolve conflict with blamer.el
  ;; (defvar-local my-starhugger-inlining-mode--blamer-mode-state nil)
  ;; (defvar-local blamer-mode nil)

  ;; (defun my-starhugger-inlining-mode-h ()
  ;;   (if starhugger-inlining-mode
  ;;     (progn
  ;;       (add-hook 'my-evil-force-normal-state-hook
  ;;         (lambda () (starhugger-dismiss-suggestion t))
  ;;         nil t)
  ;;       (setq my-starhugger-inlining-mode--blamer-mode-state blamer-mode)
  ;;       (when my-starhugger-inlining-mode--blamer-mode-state
  ;;         (blamer-mode 0)))
  ;;     (progn
  ;;       (when (and my-starhugger-inlining-mode--blamer-mode-state
  ;;               (not blamer-mode))
  ;;         (blamer-mode 1)))))

  ;; (add-hook 'starhugger-inlining-mode-hook #'my-starhugger-inlining-mode-h)
  )

; https://emacs.stackexchange.com/questions/10932/how-do-you-disable-the-buffer-end-beginning-warnings-in-the-minibuffer/20039#20039
(defun my/command-error-function (data context caller)
  "Ignore the buffer-read-only, beginning-of-buffer,
end-of-buffer signals; pass the rest to the default handler."
  (when (not (memq (car data) '(
                                 ;; buffer-read-only
                                 beginning-of-buffer
                                 end-of-buffer)))
    (command-error-default-function data context caller)))

(setq command-error-function 'my/command-error-function)

(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(defun my/increment ()
  (interactive)
  (if (number-at-point)
    (increment-integer-at-point)
    (my/flip-symbol)))

(defun my/decrement ()
  (interactive)
  (if (number-at-point)
    (decrement-integer-at-point)
    (my/flip-symbol)))

(defvar my/flip-symbol-alist
  '(("true" . "false")
    ("false" . "true"))
  "symbols to be quick flipped when editing")

(defun my/flip-symbol ()
  "I don't want to type here, just do it for me."
  (interactive)
  (-let* (((beg . end) (bounds-of-thing-at-point 'symbol))
          (sym (buffer-substring-no-properties beg end)))
    (when (member sym (cl-loop for cell in my/flip-symbol-alist
                               collect (car cell)))
      (delete-region beg end)
      (insert (alist-get sym my/flip-symbol-alist "" nil 'equal)))))

(defun my/copy-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename
          (if (equal major-mode 'dired-mode)
            default-directory
            (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun air-revert-buffer-noconfirm ()
  (interactive)
  (revert-buffer :ignore-auto :noconfirm)
  (message (concat "Buffer '" (file-name-nondirectory buffer-file-name) "' reloaded.")))

(defun my/move-current-window-to-new-frame ()
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

(defun my/buffer-tramp-p ()
  "Returns t if buffer is tramp buffer."
  (interactive)
  (let ((name (buffer-file-name)))
    (and name (string-prefix-p "/ssh:" name))))

(defun my/advice-around-skip (orig-fun &rest args)
  "Skip around adviced function.")

(defun my/sudo-dired ()
  "Run Dired in sudo mode."
  (interactive)
  (dired "/sudo::/"))

(defun my/reinstall-package (pkg)
  (interactive (list (intern (completing-read "Reinstall package: " (mapcar #'car package-alist)))))
  (unload-feature pkg)
  (package-reinstall pkg)
  (require pkg))

(defun my/unindent-region ()
  (interactive)
  (let ((beg (region-beginning))
         (end (region-end)))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (unless (bolp)
          (back-to-indentation))
        (when (<= (current-column) 0)
          (indent-line-to 0))
        (forward-line)))))

;; https://stackoverflow.com/a/750933
(defun my/remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; (add-hook 'text-mode-hook #'my/remove-dos-eol)
(add-hook 'prog-mode-hook #'my/remove-dos-eol)

(bind-keys
  ("<S-tab>" . 'my/unindent-region)
  ("C-x C-r" . recentf-open-files)
  ("<home>" . left-word)
  ("<end>" . right-word)
  ("C-x s" . (lambda () (interactive) (save-some-buffers t)))
  ("C-x 4 c" . my/clone-indirect-buffer-new-window)
  ("C-x C-SPC" . rectangle-mark-mode)
  ("M-w" . #'my/copy-region-or-word))

(when (eq system-type 'darwin)
  (bind-keys
  ("s-t" . make-frame-command)
  ("s-u" . air-revert-buffer-noconfirm)))

(when (eq system-type 'gnu/linux)
  (bind-keys
    ("M-t" . make-frame-command)
    ("M-u" . air-revert-buffer-noconfirm))

  (defun my/copy-including-secondary ()
    (interactive)
    (call-interactively 'kill-ring-save)
    (gui-set-selection 'SECONDARY (buffer-substring (point) (mark t))))

  (defun my/paste-including-secondary ()
    (interactive)
    (insert (gui-get-selection 'SECONDARY)))

  (bind-keys
    ("M-c" . my/copy-including-secondary)
    ("M-v" . my/paste-including-secondary)))

(defalias 'qcalc 'quick-calc)

(column-number-mode 1)
(global-visual-line-mode 1)
(delete-selection-mode 1)
(auto-compression-mode 1)
(global-auto-revert-mode 1)
(global-hl-line-mode -1)
;; (shell-dirtrack-mode -1)
(blink-cursor-mode -1)
(auto-save-mode -1)
;; (file-name-shadow-mode -1)

(advice-add 'visual-line-mode :around
  (lambda (orig-fun &rest args)
    (unless (memq major-mode (list 'org-agenda-mode))
      (apply orig-fun args))))

(when window-system
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(provide 'my-edit)
;;; my-edit.el ends here