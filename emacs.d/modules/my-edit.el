(require 'autorevert)
(require 're-builder)
;; (require 'calendar)
;; (require 'reftex)
;; (require 'face-remap)
(require 'conf-mode)
;; (require 'tramp)
;; (require 'elec-pair)
;; (require 'subr-x)

(setq-default mode-require-final-newline nil)

(setq tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(defvaralias 'sgml-basic-offset 'tab-width)

(setq indent-tabs-mode nil)
(setq backward-delete-char-untabify-method 'hungry)

(when (boundp 'evil-shift-width)
  (setq-default evil-shift-width tab-width))

(use-package hydra)
;; (use-package monitor)
;; (use-package popup)
;; (use-package miniedit)
;; (use-package calfw)
(use-package with-editor)  ; dependency for other package
;; (use-package neotree)
;; (use-package multiple-cursors)

;; (use-package aggressive-indent
;;   :config
;;   (global-aggressive-indent-mode 1))

(use-package emojify
  :hook (org-mode . emojify-mode))

(require 'hippie-exp)
(eval-after-load 'hippie-exp
  '(progn
     (setq hippie-expand-try-functions-list
       '(
          ;;yas-hippie-try-expand ; requires yasnippet plugin
          try-expand-all-abbrevs
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-dabbrev
          try-expand-dabbrev-from-kill
          try-expand-dabbrev-all-buffers
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol))
     (setq hippie-expand-verbose nil)
     (setq hippie-expand-try-functions-list '())
     (bind-key "M-/" 'hippie-expand)
     (add-to-list 'hippie-expand-try-functions-list 'try-expand-dabbrev t)
     (add-to-list 'hippie-expand-try-functions-list 'try-expand-dabbrev-all-buffers t)
     (add-to-list 'hippie-expand-try-functions-list 'try-expand-dabbrev-from-kill t)
     (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t)
     (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
     (add-to-list 'hippie-expand-try-functions-list 'try-expand-list t)
     (add-to-list 'hippie-expand-try-functions-list 'try-expand-line t)

     (defadvice hippie-expand (around hippie-expand-case-fold)
       "Try to do case-sensitive matching (not effective with all functions)."
       (let ((case-fold-search nil))
         ad-do-it))))

(use-package undo-fu
  :config
  (if (bound-and-true-p evil-mode)
    (progn
      (bind-key "u" 'undo-fu-only-undo evil-normal-state-map)
      (bind-key "C-r" 'undo-fu-only-redo evil-normal-state-map))
    (message "'undo-fu' not initiated. Evil mode is not active")))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode 1))

;; (require 'wgrep)
;; (setq reb-re-syntax 'string)

;; (use-package imenu-anywhere
;;   :config
;;   (progn
;;     (when (fboundp 'evil-mode)
;;       (bind-key "\\a" #'ivy-imenu-anywhere  evil-motion-state-map)
;;     )))

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

;; (require 'speedbar)
;; (eval-after-load 'speedbar
;;   '(progn
;;      (setq speedbar-show-unknown-files t)))
;; (use-package sr-speedbar)

;; (use-package transpose-frame)
;; (use-package wgrep
;; 	:config
;; 	(progn
;; 		(if (commandp 'wgrep)
;; 				(progn
;; 					(setq wgrep-enable-key "r")))))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)

(setq visual-line-fringe-indicators '(left-curly-arrow nil))

(setq-default indent-tabs-mode nil)

(setq
  fill-column 80
  hscroll-margin  1
  hscroll-step 1
  scroll-conservatively 1001 ;; should be 0?
  scroll-preserve-screen-position t ;; ?
  word-wrap t
  compare-ignore-case t
  compare-ignore-whitespace t
  sentence-end-double-space nil
  require-final-newline nil
  revert-without-query '(".*")
  tab-width 2
  ;; undo-limit 1000
  visible-bell nil
  ring-bell-function 'ignore
  show-paren-delay 0
  ns-right-alternate-modifier nil
  save-some-buffers-default-predicate t
  help-window-select t)

(setq create-lockfiles nil)

(setq echo-keystrokes 0)

(setq x-underline-at-descent-line t)
(setq confirm-kill-processe nil)
(setq process-connection-type nil)

(when (string-equal system-type "darwin")
  (setq browse-url-chrome-program "chrome"))

(eval-after-load 'elec-pair
  '(progn
     (push '(?\" . ?\") electric-pair-pairs)
     (push '(?\{ . ?\}) electric-pair-pairs)
     (push '(?\` . ?\`) electric-pair-pairs)
     (push '(?\" . ?\") electric-pair-text-pairs)
     (push '(?\{ . ?\}) electric-pair-text-pairs)
     (push '(?\' . ?\') electric-pair-text-pairs)
     (push '(?\` . ?\`) electric-pair-text-pairs)))

(setq auto-save-visited-interval 60)

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


;; (defun my/set-syntax-entry ()
;;   ""
;;   (when (and (not (eq major-mode "php-mode")) (not (eq major-mode "web-mode")))
    ;; (modify-syntax-entry ?- "w" (syntax-table)))
;;   (modify-syntax-entry ?_ "w" (syntax-table)))

;; (add-hook 'after-change-major-mode-hook #'my/set-syntax-entry)

(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(setq ack-path (executable-find "ack"))

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'ar 'align-regexp)

;; (if ack-path
  ;; (grep-apply-setting 'grep-command "ack --with-filename --nofilter --nogroup ")
  ;; (message "No 'ack' executable found."))

  ;; (setq grep-program grep-command) ; ack
    ;; (setq sr-grep-command grep-program)
    ;; (grep-apply-setting 'grep-command "ack --with-filename --nofilter --nogroup ")
    ;; "ack --with-filename --nofilter --nogroup ")
    ;; (grep-apply-setting 'grep-command "ack --with-filename --nofilter --nogroup ")
    ;; (grep-apply-setting 'grep-find-template "find <D> <X> -type f <F> -exec ack --with-filename --nofilter --nogroup '<R>' /dev/null {} +")

  ;;   (grep-apply-setting 'grep-find-template
  ;;     (concat "find . -type f -exec " ack-path " --with-filename --nofilter --nogroup '<R>' /dev/null {} +"))
  ;;   )
  ;; (message "No 'ack' executable found.")
  ;; )

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; (require 'grep)
;; (eval-after-load 'grep
;; 	'(progn
;; 		 (add-to-list 'grep-find-ignored-directories "auto-save-list")
;; 		 (add-to-list 'grep-find-ignored-directories "autosaves")
;; 		 (add-to-list 'grep-find-ignored-directories "backups")
;; 		 (add-to-list 'grep-find-ignored-directories "elpa")
;; 		 (add-to-list 'grep-find-ignored-directories "lisp")
;; 		 (add-to-list 'grep-find-ignored-directories "tools"))

;; (add-hook 'minibuffer-inactive-mode-hook
;;   (lambda ()
;;     (message (buffer-name (current-buffer)))
;;     ))

;; (advice-add 'text-scale-adjust )

(defun my/quail-setup-overlays-advice (orig-fun &rest args)
  (let ((res (apply orig-fun args))
         (buf-guidence (get-buffer " *Quail-guidance*"))
         (buf-completions (get-buffer "*Quail Completions*")))
	  ;; (overlay-put quail-overlay 'display 'underline)
	  ;; (overlay-put quail-conv-overlay 'display 'underline)

    (dolist
      (buf (list buf-guidence buf-completions))
      (when (bufferp buf)
        ;; (message (buffer-name buf))
        (with-current-buffer buf
          (set (make-local-variable 'face-remapping-alist)
            `((default :height 3))))))
    res))

;; (advice-add 'quail-setup-overlays :after #'my/quail-setup-overlays-advice)
;; (advice-add 'quail-show-guidance :around #'my/quail-setup-overlays-advice)

(defun my/list-frames (orig-fun &rest args)
  (let ((res (apply orig-fun args)))
    (message "here")
    (frame-list)
    (res))
  )

;; (advice-add 'quail-make-guidance-frame :around #'my/list-frames)

(defun my/minibuffer-setup ()
  (let* (
          ;; (buf (buffer-name (window-buffer (minibuffer-selected-window))))
          (buf (window-buffer (minibuffer-selected-window)))
          (amount (buffer-local-value 'text-scale-mode-amount buf))
          (amount-new
            (cond
              ((= amount 1) 1)
              ((= amount 0) 1)
              ((> amount 1) (* 0.75 amount))
              ((<= amount 0) 1))))

    (set (make-local-variable 'face-remapping-alist)
      `((default :height ,(float amount-new))))
    ))

;; (add-hook 'minibuffer-setup-hook 'my/minibuffer-setup)

    ;; (dolist
      ;; (buf (list "*Quail Completions*" "*code-conversion-work*" " *code-conversion-work*" " *Minibuf-0*" " *Minibuf-1*" " *Echo Area 0*" " *Echo Area 1*"))
      ;; (when (get-buffer buf)

    ;; (when (string= (buffer-name buf) "*Quail Completions*") ;; " *Minibuf-0*" " *Minibuf-1*" " *Echo Area 0*" " *Echo Area 1*")
      ;; (with-current-buffer buf
      ;; (setq-local face-remapping-alist '((default (:height 1))))))))


(setq bookmark-save-flag t)

;; (use-package company-emoji
;;   :config
;;   (progn
;;     (add-to-list 'company-backends 'company-emoji)))

;; (defun my/set-emoji-font (frame)
;;   "Adjust the font settings of FRAME so Emacs can display emoji properly."
;;   (if (eq system-type 'darwin)
;;       ;; For NS/Cocoa
;;       (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
;;     ;; For Linux
;;     (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))

;; For when Emacs is started in GUI mode:
;; (my/set-emoji-font nil)

;; Hook for when a frame is created with emacsclient
;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
;; (add-hook 'after-make-frame-functions 'my/set-emoji-font)

(defhydra hydra-buffer ()
  "Buffer"
  ("i" ibuffer "ibuffer" :exit t))

(defun jarfar/increment ()
  (interactive)
  (if (number-at-point)
    (increment-integer-at-point)
    (my/flip-symbol)))

(defun jarfar/decrement ()
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

;; (defun my/buffer-messages-tail ()
  ;; (let ((messages (get-buffer "*Messages*")))
    ;; (unless (eq (current-buffer) messages)
      ;; (with-current-buffer messages
        ;; (goto-char (point-max))))))

;; http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard
;; (defun my/copy-file-name-to-clipboard ()
;;   "Copy the current buffer file name to the clipboard."
;;   (interactive)
;;   (let ((filename (if (equal major-mode 'dired-mode)
;;                       default-directory
;;                     (buffer-file-name))))
;;     (when filename
;;       (kill-new filename)
;;       (message "Copied buffer file name '%s' to the clipboard." filename))))

;; (add-hook 'post-command-hook 'my/buffer-messages-tail)

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

;; (add-hook 'after-save-hook 'air-revert-buffer-noconfirm)

(defun my/move-current-window-to-new-frame ()
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

(defun my/buffer-tramp-p ()
  "Returns t if buffer is tramp buffer."
  ; file-remote-p
  (interactive)
  (let ((name (buffer-file-name)))
    (and name (string-prefix-p "/ssh:" name))))

(defun my/advice-around-skip (orig-fun &rest args) "")

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

(bind-key "C-x C-r"       'recentf-open-files)
(bind-key "<home>"        'left-word)
(bind-key "<end>"         'right-word)
(bind-key "C-x <left>"    'windmove-left)
(bind-key "C-x <right>"   'windmove-right)
(bind-key "C-x <up>"      'windmove-up)
(bind-key "C-x <down>"    'windmove-down)
(bind-key "C-x s"         (lambda () (interactive) (save-some-buffers t)))
(bind-key "C-x 4 c"       'my/clone-indirect-buffer-new-window)
(bind-key "s-t"           'make-frame-command)

(bind-key "C-x C-SPC"     'rectangle-mark-mode)
(bind-key "C->"           'mc/mark-next-like-this)
(bind-key "C-<"           'mc/mark-previous-like-this)
(bind-key "C-c C-<"       'mc/mark-all-like-this)
(bind-key "s-u"           'air-revert-buffer-noconfirm)
;; (bind-key "C-c O"      'org-open-at-point-global)

(defalias 'qcalc 'quick-calc)

(column-number-mode 1)
(global-auto-revert-mode 1)
(global-visual-line-mode 1)
;; (global-emojify-mode 1)
(delete-selection-mode 1)
(global-hl-line-mode -1)
(savehist-mode -1)
(shell-dirtrack-mode -1)
;; (auto-save-visited-mode)
(auto-compression-mode 1)
(electric-pair-mode 1)

(advice-add 'visual-line-mode :around
  (lambda (orig-fun &rest args)
    (unless (memq major-mode (list 'org-agenda-mode))
      (apply orig-fun args))))

(global-eldoc-mode -1)
(blink-cursor-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(file-name-shadow-mode -1)

(when window-system
  (tool-bar-mode -1))

(add-to-list 'same-window-buffer-names "*SQL*")

(provide 'my-edit)
