;;; Code:

(when window-system
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

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
  :init
  (setq-default hungry-delete-join-reluctantly t)
  :config
  (global-hungry-delete-mode 1))

;; (use-package with-editor)  ; dependency for other package

(use-package emojify
  :disabled t
  :custom
  (emojify-emoji-set "twemoji-v2-22")
  ;; (emojify-emoji-set "openmoji-v13-0")
  :commands global-emojify-mode
  :config
  (add-hook 'after-init-hook #'global-emojify-mode))

;; Allow to move selected lines up and down
(use-package drag-stuff
  :diminish drag-stuff-mode
  :config
  (add-to-list 'drag-stuff-except-modes #'org-mode)
  (add-to-list 'drag-stuff-except-modes #'org-journal-mode)
  ;; (add-to-list 'drag-stuff-except-modes 'my/org-taskjuggler-mode)
  (drag-stuff-global-mode 1)
  (define-key drag-stuff-mode-map (drag-stuff--kbd 'up) #'drag-stuff-up)
  (define-key drag-stuff-mode-map (drag-stuff--kbd 'down) #'drag-stuff-down))

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

;; This works well with corfu
;; (use-package cape)

;; alternative https://github.com/minad/corfu
(use-package company
  :after evil
  :diminish company-mode
  :demand t
  :bind (:map evil-normal-state-map
          ("C-n" . company-next-page)
          ("C-p" . company-previous-page))
  :custom
  (company-idle-delay 0.8)
  (company-show-numbers t)
  (company-show-quick-access t)
  (company-tooltip-align-annotations t)
  (company-selection-wrap-around t)
  (company-minimum-prefix-length 2)
  (company-files-exclusions '(".git/" ".DS_Store"))
  (company-backends '((:separate company-yasnippet company-capf company-files company-keywords company-dabbrev-code)))
  :config
  (evil-declare-change-repeat #'company-complete))

;; Required for proportional text
(use-package company-posframe
  :after company
  :diminish ""
  :config
  (company-posframe-mode t))

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

(use-package which-key
  :disabled t
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
  :straight nil
  :commands (ediff ediff-buffers)
  :custom
  (ediff-diff-options "-w")
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-forward-word-function #'forward-char)
  ;; :config
  ;; (evil-define-key 'normal ediff-mode-map
  ;;   "[c" 'ediff-next-difference
  ;;   "]c" 'ediff-previous-difference)
  )

(use-package savehist
  :disabled t
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
  :if (and (boundp 'my/chatgpt-enabled) my/chatgpt-enabled)
  :commands (chatgpt-shell my/chatgpt-shell-start-new ai)
  :init
  (defun my/chatgpt-shell-start-new ()
    "If `chatgpt-shell` session exists kill it, and start new session."
    (interactive)
    (let ((chatgpt-buffer (seq-find (lambda (item) (string-prefix-p "*chatgpt" (buffer-name item))) (buffer-list))))
      (when chatgpt-buffer
        (kill-buffer chatgpt-buffer)))
    (chatgpt-shell))
  (defalias 'ai #'my/chatgpt-shell-start-new)
  :custom
  (chatgpt-shell-openai-key (lambda () (auth-source-pick-first-password :host "openai-api-key-anon")))
  ;; dall-e-shell-openai-key
  :config
  (bind-keys
    :map chatgpt-shell-mode-map
    ("C-w v" . split-window-right))
  )

(use-package ob-chatgpt-shell
  :disabled t
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
(setq command-error-function #'my/command-error-function)

;; (if (fboundp 'imagemagick-register-types)
;;   (imagemagick-register-types)
;;   (message "No imagemagick support"))

(defvar my/flip-symbol-alist
  '(("true" . "false")
    ("false" . "true"))
  "symbols to be quick flipped when editing")

(defun my/flip-symbol-at-point ()
  "Flip symbol at point."
  (interactive)
  (-let* ((symbol (symbol-at-point))
           (symbol (when symbol (symbol-name symbol)))
           ((beg . end) (bounds-of-thing-at-point 'symbol)))
    (if (assoc symbol my/flip-symbol-alist)
      (progn
        (delete-region beg end)
        (insert (alist-get symbol my/flip-symbol-alist "" nil 'equal)))
      (message "Nothing to flip here"))))

(defun my/revert-buffer-noconfirm ()
  "Revert current buffer without asking for confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm)
  (message (format "Buffer '%s' reloaded." (file-name-nondirectory buffer-file-name))))

(unless (boundp 'straight-version)
  (defun my/reinstall-package (pkg)
    (interactive (list (intern (completing-read "Reinstall package: " (mapcar #'car package-alist)))))
    (unload-feature pkg)
    (package-reinstall pkg)
    (require 'pkg)))

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
  ("C-x s" . my/save-all-buffers)
  ("C-x 4 c" . my/clone-indirect-buffer-new-window)
  ("C-x C-SPC" . rectangle-mark-mode)
  ("M-w" . my/copy-region-or-word))

(defun my/save-all-buffers ()
  "Save all buffers."
  (interactive)
  (save-some-buffers t)
  (message "All buffers saved"))

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
    (call-interactively #'kill-ring-save)
    (gui-set-selection 'SECONDARY (buffer-substring (point) (mark t))))

  (defun my/paste-including-secondary ()
    (interactive)
    (insert (gui-get-selection 'SECONDARY)))

  (bind-keys
    ("M-c" . my/copy-including-secondary)
    ("M-v" . my/paste-including-secondary))))

(defalias 'qcalc #'quick-calc)

(column-number-mode 1)
(global-visual-line-mode 1)
(delete-selection-mode 1)
(auto-compression-mode 1)
(context-menu-mode 1)
(global-auto-revert-mode -1)
(global-hl-line-mode -1)
;; (shell-dirtrack-mode -1)
(blink-cursor-mode -1)
(auto-save-mode -1)
;; (file-name-shadow-mode -1)

(use-package desktop
  :straight nil
  :custom
  (desktop-load-locked-desktop t)
  :config
  (desktop-save-mode 1))

;; Apply `visual-line-mode' only on not `org-agenda-mode' buffers.
(advice-add 'visual-line-mode :around
  (lambda (orig-fun &rest args)
    (unless (memq major-mode (list 'org-agenda-mode))
      (apply orig-fun args))))

(provide 'my-edit)
;;; my-edit.el ends here