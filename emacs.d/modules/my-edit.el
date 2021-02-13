(require 'autorevert)
(require 're-builder)
;; (require 'calendar)
;; (require 'reftex)
;; (require 'face-remap)
(require 'ediff)
(require 'conf-mode)
;; (require 'tramp)
;; (require 'elec-pair)
;; (require 'subr-x)

(setq-default
  cursor-in-non-selected-windows t
  display-time-default-load-average nil
  scroll-conservatively most-positive-fixnum ; Always scroll by one line
  view-read-only t
  mode-require-final-newline nil)

(setq ack-path (executable-find "ack"))

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'ar 'align-regexp)

(setq-default tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(defvaralias 'sgml-basic-offset 'tab-width)

(setq
  indent-tabs-mode nil
  backward-delete-char-untabify-method 'hungry)

(setq visual-line-fringe-indicators '(left-curly-arrow nil))

(setq-default indent-tabs-mode nil)

(set-face-attribute 'default nil :font "Source Code Pro Medium")
(set-fontset-font t 'latin "Noto Sans")

(setq
  fill-column 80
  hscroll-margin  1
  hscroll-step 1
  ;; scroll-conservatively most-positive-fixnum ;; 1001 ;; should be 0?
  word-wrap t
  shift-select-mode nil
  compare-ignore-case t
  compare-ignore-whitespace t
  sentence-end-double-space nil
  require-final-newline nil
  revert-without-query '(".*")
  ;; undo-limit 1000
  show-paren-delay 0
  save-some-buffers-default-predicate t
  help-window-select t
  bookmark-save-flag nil)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'same-window-buffer-names "*SQL*")

(eval-after-load 'elec-pair
  '(progn
     (push '(?\" . ?\") electric-pair-pairs)
     (push '(?\{ . ?\}) electric-pair-pairs)
     (push '(?\` . ?\`) electric-pair-pairs)
     (push '(?\" . ?\") electric-pair-text-pairs)
     (push '(?\{ . ?\}) electric-pair-text-pairs)
     (push '(?\' . ?\') electric-pair-text-pairs)
     (push '(?\` . ?\`) electric-pair-text-pairs)))

(use-package hydra)
(use-package with-editor)  ; dependency for other package

(use-package emojify
  :hook (org-mode . emojify-mode))

(use-package company
  :hook ((prog-mode . company-mode)
          (mu4e-compose-mode . company-mode))
  :diminish company-mode
  :config
  (setq
    company-idle-delay 0.4
    company-show-numbers t
    company-tooltip-align-annotations t
    company-minimum-prefix-length 1
    company-backends '(company-files (company-yasnippet company-dabbrev-code) company-keywords company-capf company-gtags company-etags)))

;; (require 'hippie-exp)
;; (eval-after-load 'hippie-exp
;;   '(progn
;;      (setq hippie-expand-try-functions-list
;;        '(
;;           yas-hippie-try-expand ; requires yasnippet plugin
;;           try-expand-all-abbrevs
;;           try-complete-file-name-partially
;;           try-complete-file-name
;;           try-expand-dabbrev
;;           try-expand-dabbrev-from-kill
;;           try-expand-dabbrev-all-buffers
;;           try-expand-list
;;           try-expand-line
;;           try-complete-lisp-symbol-partially
;;           try-complete-lisp-symbol))
;;      (setq hippie-expand-verbose nil)
;;      (setq hippie-expand-try-functions-list '())
;;      (bind-key "M-/" 'hippie-expand)
;;      (add-to-list 'hippie-expand-try-functions-list 'try-expand-dabbrev t)
;;      (add-to-list 'hippie-expand-try-functions-list 'try-expand-dabbrev-all-buffers t)
;;      (add-to-list 'hippie-expand-try-functions-list 'try-expand-dabbrev-from-kill t)
;;      (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t)
;;      (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
;;      (add-to-list 'hippie-expand-try-functions-list 'try-expand-list t)
;;      (add-to-list 'hippie-expand-try-functions-list 'try-expand-line t)

;;      (defadvice hippie-expand (around hippie-expand-case-fold)
;;        "Try to do case-sensitive matching (not effective with all functions)."
;;        (let ((case-fold-search nil))
;;          ad-do-it))))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode 1))

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(setq
  ediff-window-setup-function 'ediff-setup-windows-plain
  ediff-forward-word-function 'forward-char)

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

;; https://emacs.stackexchange.com/questions/63336/deleting-a-file-with-name-that-already-exists-in-trash/63342#63342
(when (eq system-type 'darwin)
  (defun system-move-file-to-trash (filename)
    "Move file or directory named FILENAME to the trash."
    (ns-do-applescript
      (format
        "tell application \"Finder\" to delete POSIX file \"%s\""
        filename))))

(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(defhydra farynaio/hydra-buffer ()
  "Buffer"
  ("i" ibuffer "ibuffer" :exit t))

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

(bind-key "<S-tab>" 'jarfar/unindent-region)
(defvar epa-key-mode-map (make-sparse-keymap))
(defvar org-mode-map (make-sparse-keymap))
(defvar mu4e:view-mode-map (make-sparse-keymap))
(defvar mu4e-headers-mode-map (make-sparse-keymap))
(defvar mu4e-compose-mode-map (make-sparse-keymap))
;; (defvar flyspell-mode-map (make-sparse-keymap))
(defvar elpy-mode-map (make-sparse-keymap))
(defvar js2-mode-map (make-sparse-keymap))
(defvar eshell-mode-map (make-sparse-keymap))
(defvar php-mode-map (make-sparse-keymap))
(defvar web-mode-map (make-sparse-keymap))

(bind-keys
  ("C-x C-r" . recentf-open-files)
  ("<home>" . left-word)
  ("<end>" . right-word)
  ("C-x <left>" . windmove-left)
  ("C-x <right>" . windmove-right)
  ("C-x <up>" . windmove-up)
  ("C-x <down>" . windmove-down)
  ("C-x s" . (lambda () (interactive) (save-some-buffers t)))
  ("C-x 4 c" . my/clone-indirect-buffer-new-window)
  ("s-t" . make-frame-command)
  ("C-x C-SPC" . rectangle-mark-mode)
  ("s-u" . air-revert-buffer-noconfirm))

(defalias 'qcalc 'quick-calc)

(column-number-mode 1)
(global-auto-revert-mode 1)
(global-visual-line-mode 1)
(delete-selection-mode 1)
(auto-compression-mode 1)
(electric-pair-mode 1)

(global-hl-line-mode -1)
(savehist-mode -1)
(shell-dirtrack-mode -1)
(global-eldoc-mode -1)
(blink-cursor-mode -1)
(file-name-shadow-mode -1)

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
