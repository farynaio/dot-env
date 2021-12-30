(setq-default
  cursor-in-non-selected-windows t
  display-time-default-load-average nil
  scroll-conservatively most-positive-fixnum ; Always scroll by one line
  view-read-only t
  require-final-newline nil
  indent-tabs-mode nil
  mode-require-final-newline nil)

(setq ack-path (executable-find "ack"))

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'ar 'align-regexp)

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
  undo-limit 16000
  show-paren-delay 0
  shell-dirtrackp nil
  save-some-buffers-default-predicate t
  help-window-select t
  bookmark-save-flag nil)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'same-window-buffer-names "*SQL*")

(use-package elec-pair
  :ensure nil
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

;; (use-package org-pomodoro)

(use-package hungry-delete
  :init
  (setq-default hungry-delete-join-reluctantly t)
  :config
  (global-hungry-delete-mode))

(use-package with-editor)  ; dependency for other package

(use-package emojify
  :hook (org-mode . emojify-mode))

(use-package company
  :hook ((prog-mode . company-mode)
          (mu4e-compose-mode . company-mode))
  :diminish company-mode
  :config
  (setq
    company-idle-delay 0.5
    company-show-numbers t
    company-tooltip-align-annotations t
    company-minimum-prefix-length 1
    company-backends '(company-files (company-yasnippet company-dabbrev-code) company-keywords company-capf company-gtags company-etags)))

;; (use-package which-key
;;   :defer 0.2
;;   :diminish which-key-mode
;;   :custom
;;   (which-key-idle-delay 0.5)
;;   :config
;;   (which-key-mode 1))

(use-package expand-region
  :bind (("C-=" . er/contract-region)
         ("C-+" . er/expand-region)))

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
  :custom
  (savehist-file "~/.emacs.d/savehist")
  (history-length 500)
  (history-delete-duplicates t)
  (auto-save-visited-interval 60)
  (savehist-save-minibuffer-history 1)
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  :config
  (savehist-mode 1))

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

(defun farynaio/increment ()
  (interactive)
  (if (number-at-point)
    (increment-integer-at-point)
    (farynaio/flip-symbol)))

(defun farynaio/decrement ()
  (interactive)
  (if (number-at-point)
    (decrement-integer-at-point)
    (farynaio/flip-symbol)))

(defvar farynaio/flip-symbol-alist
  '(("true" . "false")
    ("false" . "true"))
  "symbols to be quick flipped when editing")

(defun farynaio/flip-symbol ()
  "I don't want to type here, just do it for me."
  (interactive)
  (-let* (((beg . end) (bounds-of-thing-at-point 'symbol))
          (sym (buffer-substring-no-properties beg end)))
    (when (member sym (cl-loop for cell in farynaio/flip-symbol-alist
                               collect (car cell)))
      (delete-region beg end)
      (insert (alist-get sym farynaio/flip-symbol-alist "" nil 'equal)))))

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

(defun sudo-dired ()
  (interactive)
  (dired "/sudo::/"))

(defun my/reinstall-package (pkg)
  (interactive (list (intern (completing-read "Reinstall package: " (mapcar #'car package-alist)))))
  (unload-feature pkg)
  (package-reinstall pkg)
  (require pkg))

;; https://stackoverflow.com/a/11624677/346921
(defun jarfar/unindent-region ()
  (interactive)
  (let ((offset (* tab-width -1)))
    (if (use-region-p)
      (progn (indent-rigidly (region-beginning) (region-end) offset)
        (setq deactivate-mark nil))
      (indent-rigidly (line-beginning-position) (line-end-position) offset))))

(bind-keys
  ("<S-tab>" . 'jarfar/unindent-region)
  ("C-x C-r" . recentf-open-files)
  ("<home>" . left-word)
  ("<end>" . right-word)
  ("C-x s" . (lambda () (interactive) (save-some-buffers t)))
  ("C-x 4 c" . my/clone-indirect-buffer-new-window)
  ("C-x C-SPC" . rectangle-mark-mode))

(when (eq system-type 'darwin)
  (bind-keys
  ("s-t" . make-frame-command)
  ("s-u" . air-revert-buffer-noconfirm)))

(when (eq system-type 'gnu/linux)
  (bind-keys
    ("M-t" . make-frame-command)
    ("M-u" . air-revert-buffer-noconfirm)))

(when (eq system-type 'gnu/linux)
  (defun jarfar/copy-including-secondary ()
    (interactive)
    (call-interactively 'kill-ring-save)
    (gui-set-selection 'SECONDARY (buffer-substring (point) (mark t))))

  (defun jarfar/paste-including-secondary ()
    (interactive)
    (insert (gui-get-selection 'SECONDARY)))

  (bind-keys
    ("M-c" . jarfar/copy-including-secondary)
    ("M-v" . jarfar/paste-including-secondary)))

(defalias 'qcalc 'quick-calc)

(column-number-mode 1)
(global-visual-line-mode 1)
(delete-selection-mode 1)
(auto-compression-mode 1)
(global-auto-revert-mode 1)
(global-hl-line-mode -1)
;; (shell-dirtrack-mode -1)
;; (global-eldoc-mode -1)
(blink-cursor-mode -1)
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
