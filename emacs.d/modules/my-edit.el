(setq-default
  cursor-in-non-selected-windows t
  display-time-default-load-average nil
  scroll-conservatively most-positive-fixnum ; Always scroll by one line
  view-read-only t
  require-final-newline nil
  indent-tabs-mode nil
  mode-require-final-newline nil)

(setq auto-save-no-message t)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'ar 'align-regexp)

(setq-default tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(defvaralias 'sgml-basic-offset 'tab-width)

(setq backward-delete-char-untabify-method 'hungry)

(setq visual-line-fringe-indicators '(left-curly-arrow nil))

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
  :straight nil
  ;; :config
  ;; (auth-source-debug 'trivi) ;; for debug info in *Messages*
  )

(use-package elec-pair
  :defer 0.3
  :straight nil
  :config
  ;; (push '(?\" . ?\") electric-pair-pairs)
  (push '(?\{ . ?\}) electric-pair-pairs)
  ;; (push '(?\` . ?\`) electric-pair-pairs)
  (push '(?\( . ?\)) electric-pair-pairs)
  ;; (push '(?\" . ?\") electric-pair-text-pairs)
  (push '(?\{ . ?\}) electric-pair-text-pairs))
  ;; (push '(?\' . ?\') electric-pair-text-pairs)
  ;; (push '(?\` . ?\`) electric-pair-text-pairs)

(use-package hungry-delete
  :defer 0.3
  :init
  (setq-default hungry-delete-join-reluctantly t)
  :config
  (global-hungry-delete-mode))

;; (use-package with-editor)  ; dependency for other package

;; (use-package emojify
;;   :hook (org-mode . emojify-mode))

(use-package corfu
  :disabled t
  :defer 0.2
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert) ; Do not preview current candidate
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
  :init
  (add-to-list 'load-path (expand-file-name "straight/repos/corfu/extensions" user-emacs-directory))
  (require 'corfu-history)
  (require 'corfu-popupinfo)
  (require 'corfu-echo)
  (require 'corfu-info)
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode) ; Popup completion info
  (corfu-echo-mode)
  :config
  (add-hook 'eshell-mode-hook
    (lambda () (setq-local corfu-quit-at-boundary t
                 corfu-quit-no-match t
                 corfu-auto nil)
      (corfu-mode))))

;; alternative https://github.com/minad/corfu
(use-package company
  :defer 0.2
  :diminish company-mode
  :init
  (require 'company-tempo)
  :custom
  (company-idle-delay 0.5)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 1)
  ;; (company-backends '(company-capf))
  (company-backends '((company-capf company-files company-keywords company-dabbrev-code :separate)))
  (company-files-exclusions '(".git/" ".DS_Store"))
  :config
  (evil-declare-change-repeat 'company-complete)
  (evil-define-key 'normal global-map
    (kbd "C-n") 'company-next-page
    (kbd "C-p") 'company-previous-page))

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
  (which-key-idle-delay 0.3)
  :config
  (which-key-mode 1))

(use-package expand-region
  :commands (er/expand-region er/contract-region))

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package ediff
  :commands (ediff ediff-buffers)
  :straight nil
  :config
  (ediff-diff-options "-w")
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-forward-word-function 'forward-char)
  ;; :config
  ;; (evil-define-key 'normal ediff-mode-map
  ;;   "[c" 'ediff-next-difference
  ;;   "]c" 'ediff-previous-difference)
  )

(use-package savehist
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

;; has conflick with Blamer resolution https://github.com/daanturo/starhugger.el
(use-package starhugger
  :commands (starhugger-trigger-suggestion starhugger-accept-suggestion starhugger-show-next-suggestion starhugger-show-prev-suggestion)
  :custom
  (starhugger-api-token (lambda () (auth-source-pick-first-password :host "huggingface.co-api-key-anon"))))

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

(if (fboundp 'imagemagick-register-types)
  (imagemagick-register-types)
  (message "No imagemagick support"))

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

(evil-define-key 'normal global-map
  (kbd ",c d") 'my/copy-file-name)

(defun my/revert-buffer-noconfirm ()
  "Revert current buffer without asking for confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm)
  (message (format "Buffer '%s' reloaded." (file-name-nondirectory buffer-file-name))))

(defun my/move-current-window-to-new-frame ()
  "Move current window to new frame."
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

(evil-define-key 'normal global-map
  (kbd "C-x T") 'my/move-current-window-to-new-frame
  (kbd "C-w T") 'my/move-current-window-to-new-frame)

(defun my/buffer-tramp-p ()
  "Returns t if buffer is tramp buffer."
  (interactive)
  (let ((name (buffer-file-name)))
    (and name (string-prefix-p "/ssh:" name))))

(defun my/advice-around-skip (orig-fun &rest args)
  "Skip around adviced function.")

(defun my/reinstall-package (pkg)
  (interactive (list (intern (completing-read "Reinstall package: " (mapcar #'car package-alist)))))
  (unload-feature pkg)
  (package-reinstall pkg)
  (require pkg))

;; (defun my/unindent-region ()
;;   "Unindent selected region."
;;   (interactive)
;;   (let ((beg (region-beginning))
;;          (end (region-end)))
;;     (save-excursion
;;       (goto-char beg)
;;       (while (< (point) end)
;;         (unless (bolp)
;;           (back-to-indentation))
;;         (when (<= (current-column) 0)
;;           (indent-line-to 0))
;;         (forward-line)))))

;; https://stackoverflow.com/a/750933
(defun my/remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; (add-hook 'text-mode-hook #'my/remove-dos-eol)
(add-hook 'prog-mode-hook #'my/remove-dos-eol)

(bind-keys
  ;; ("<S-tab>" . 'my/unindent-region)
  ;; ("C-x C-r" . recentf-open-files)
  ("<home>" . left-word)
  ("<end>" . right-word)
  ("C-x s" . (lambda () (interactive) (save-some-buffers t) (message "Saved all buffers")))
  ("C-x 4 c" . my/clone-indirect-buffer-new-window)
  ("C-x C-SPC" . rectangle-mark-mode)
  ("M-w" . my/copy-region-or-word))

(cond
  ((eq system-type 'darwin)
    (bind-keys
      ("s-t" . make-frame-command)
      ("s-u" . my/revert-buffer-noconfirm)))
  ((eq system-type 'gnu/linux)
    (bind-keys
      ("M-t" . make-frame-command)
      ("M-u" . my/revert-buffer-noconfirm))

  (defun my/copy-including-secondary ()
    (interactive)
    (call-interactively 'kill-ring-save)
    (gui-set-selection 'SECONDARY (buffer-substring (point) (mark t))))

  (defun my/paste-including-secondary ()
    (interactive)
    (insert (gui-get-selection 'SECONDARY)))

  (bind-keys
    ("M-c" . my/copy-including-secondary)
    ("M-v" . my/paste-including-secondary))))

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

;; https://github.com/minad/tempel
;; https://www.lysator.liu.se/~davidk/elisp/tempo-examples.html
;; https://www.lysator.liu.se/~davidk/elisp/
(use-package tempo
  :defer 0.4
  :straight nil
  :custom
  (tempo-interactive t)
  :config
  (defvar general-tags nil
    "Tags for all modes.")

  (defun my/tempo-insert ()
    (interactive)
    (let* ((evil-state-pre (when (boundp 'evil-state) evil-state))
            (tags (mapcar 'car (tempo-build-collection)))
            (symbol (symbol-at-point))
            (symbol (if symbol (symbol-name symbol) ""))
            (symbol (if (and (not (string-empty-p symbol)) (seq-some (lambda (i) (string-match-p (regexp-quote symbol) i)) tags)) symbol ""))
            (tag
              (funcall
                completing-read-function
                "Choose template: "
                tags
                nil
                t
                symbol))
            (inhibit-message t)
            (message-log-max nil))
      (if (eq symbol "")
        (insert tag)
        (unless (string= symbol tag)
          (insert (format " %s" tag))))
      (tempo-complete-tag)
      (when evil-state-pre
        (evil-change-state evil-state-pre))))

  (add-hook 'text-mode-hook
    (lambda ()
      (tempo-use-tag-list 'general-tags)))

  (add-hook 'prog-mode-hook
    (lambda ()
      (tempo-use-tag-list 'general-tags)))

  (add-hook 'emacs-lisp-mode-hook
    (lambda ()
      (tempo-use-tag-list 'general-tags)))

  (tempo-define-template
    "file-vars"
    '("-*- " ~ " -*-")
    "filev"
    "Insert file variables block"
    'general-tags)

  (tempo-define-template
    "todo-tag"
    '("TODO ")
    "todo"
    "Insert TODO block"
    'general-tags)
  )

(use-package undo-fu
  :commands (undo-fu-only-undo undo-fu-only-redo)
  :config
  (evil-define-key '(normal) 'global-map
    (kbd "u") 'undo-fu-only-undo
    (kbd "C-r") 'undo-fu-only-redo))

(provide 'my-edit)
;;; my-edit.el ends here