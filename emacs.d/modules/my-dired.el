(setq
  delete-by-moving-to-trash t
  trash-directory "~/.Trash")

(when (file-executable-p "/usr/local/bin/gls")
  (setq
    insert-directory-program "/usr/local/bin/gls"
    dired-listing-switches "-alh1v"))

(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq
    ls-lisp-dirs-first t
    ls-lisp-use-insert-directory-program nil))

(eval-after-load 'dired
  '(progn
     ;; (require 'dired+)
     ;; (use-package image-dired+)
     (use-package image-dired+
       :after image-dired)

     (require 'org-link-archive)

     (setq
       find-name-arg "-iname" ;; -iregex
       dired-dwim-target t
       dired-use-ls-diredto nil
       dired-recursive-copies 'always
       dired-recursive-deletes 'always
       dired-listing-switches "-alh"
       dired-deletion-confirmer 'y-or-n-p
       dired-auto-revert-buffer t
       dired-clean-confirm-killing-deleted-buffers nil
       dired-hide-details-mode 1
       dired-guess-shell-alist-user
       '(("\\.pdf\\'" "open")
          ("\\.html\\'" "open")
          ("\\.xlsx?m?\\'" "open")
          ("\\.ods\\'" "open")
          ("\\.\\(?:jpe?g\\|png\\|gif\\)\\'" "open")
          ("\\.\\(?:mp3\\|ogg\\)\\'" "open")
          ("\\.\\(?:mpe?g\\|mp4\\|avi\\|wmv\\)\\'" "open")))

     (bind-keys
       :map dired-mode-map
       ("C-s" . find-name-dired)
       ("<" . beginning-of-buffer)
       (">" . end-of-buffer)
       ("W" . my/dired-copy-dirname-as-kill)
       ("k" . (lambda () (interactive) (dired-do-kill-lines t)))
       ("r" . my/rgrep)
       ("C-w =" . balance-windows)
       ("C-w |" . maximize-window)
       ("C-w q" . quit-window)
       ;; ("RET" . dired-find-file)
       ("<backspace>" . (lambda () (interactive) (farynaio/dired-go-up-reuse ".."))))

     (when (bound-and-true-p evil-mode)
       (bind-keys
         :map dired-mode-map
         ("n" . evil-ex-search-next)
         ("N" . evil-ex-search-previous)))

     (add-hook 'dired-mode-hook
       (lambda ()
         (dired-hide-details-mode 1)
         (hl-line-mode 1)))
))

;; (eval-after-load 'dired+
;;   '(progn
;;      (setq
;;        diredp-hide-details-initially-flag nil
;;        diredp-auto-focus-frame-for-thumbnail-tooltip-flag t)
    ;; (diredp-toggle-find-file-reuse-dir 1)
    ;; (bind-key "<backspace>" 'diredp-up-directory-reuse-dir-buffer dired-mode-map)))

;; (mapc (lambda (buffer)
;;         (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
;;           (message "dired buf")))
;;   (buffer-list))

(defun farynaio/dired-shell-command ()
  "Run any shell command in Dired."
  (interactive )
  (let ((cmd (read-string "Run shell command: ")))
    (if cmd
      (dired-shell-command cmd)
      (user-error "Command is required!"))))

(defun farynaio/dired-go-up-reuse (&optional dir)
  (interactive)
  (let ((new-dir (if dir (expand-file-name dir) (dired-get-file-for-visit)))
          (buffer
            (seq-find
              (lambda (w) (and (not (eq w (selected-window))) (eq (current-buffer) (window-buffer w))))
              (window-list-1))))
    (if (or buffer (file-regular-p new-dir))
      (find-file new-dir)
      (find-alternate-file new-dir))))

(defun my/dired-jump-make-new-window ()
  "Open new vertical window and open dired there."
  (interactive)
  (split-window-right)
  (windmove-right)
  (dired-jump))

(defun my/dired-copy-dirname-as-kill ()
  "Copy the current directory into the kill ring."
  (interactive)
  (message (format "Path '%s' copied to clipboard." default-directory))
  (kill-new default-directory))

(provide 'my-dired)
