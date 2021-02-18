(setq-default
  delete-by-moving-to-trash t
  trash-directory "~/.Trash")

(use-package ls-lisp
  :ensure nil
  :if (eq system-type 'darwin)
  :custom
    (ls-lisp-dirs-first t)
    (ls-lisp-use-insert-directory-program nil))

(use-package dired
  :ensure nil
  :delight "Dired "
  :hook ((dired-mode . dired-hide-details-mode)
          (dired-mode . hl-line-mode))
  :bind (:map dired-mode-map
          ("C-s" . find-name-dired)
          ("<" . beginning-of-buffer)
          (">" . end-of-buffer)
          ("W" . my/dired-copy-dirname-as-kill)
          ("k" . (lambda () (interactive) (dired-do-kill-lines t)))
          ("r" . my/rgrep)
          ("C-w =" . balance-windows)
          ("C-w |" . maximize-window)
          ("C-w q" . quit-window)
          ("/" . evil-ex-search-forward)
          ("n" . evil-ex-search-next)
          ("N" . evil-ex-search-previous)
          ("<backspace>" . (lambda () (interactive) (farynaio/dired-go-up-reuse ".."))))
  :custom
  (find-name-arg "-iname")
  (dired-dwim-target t)
  (dired-use-ls-diredto nil)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-listing-switches "-alh")
  (dired-deletion-confirmer 'y-or-n-p)
  (dired-auto-revert-buffer t)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-hide-details-mode 1)
  (dired-guess-shell-alist-user
    '(("\\.pdf\\'" "open")
       ("\\.html\\'" "open")
       ("\\.xlsx?m?\\'" "open")
       ("\\.ods\\'" "open")
       ("\\.\\(?:jpe?g\\|png\\|gif\\)\\'" "open")
       ("\\.\\(?:mp3\\|ogg\\)\\'" "open")
       ("\\.\\(?:mpe?g\\|mp4\\|avi\\|wmv\\)\\'" "open")))
  :preface
  (defun farynaio/dired-shell-command ()
    "Run any shell command in Dired."
    (interactive )
    (let ((cmd (read-string "Run shell command: ")))
      (if cmd
        (dired-shell-command cmd)
        (user-error "Command is required!"))))
  :config
  (when (file-executable-p "/usr/local/bin/gls")
    (setq
      insert-directory-program "/usr/local/bin/gls"
      dired-listing-switches "-alh1v"))

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
    (kill-new default-directory)))

(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
          ("<tab>" . dired-subtree-cycle)
          ("i" . dired-subtree-toggle)))

(use-package dired-narrow
  :after dired
  :bind (("C-c C-n" . dired-narrow)
         ("C-c C-f" . dired-narrow-fuzzy)
         ("C-c C-r" . dired-narrow-regexp)))

  ;; https://emacs.stackexchange.com/questions/63336/deleting-a-file-with-name-that-already-exists-in-trash/63342#63342
  (when (eq system-type 'darwin)
    (defun system-move-file-to-trash (filename)
      "Move file or directory named FILENAME to the trash."
      (ns-do-applescript
        (format
          "tell application \"Finder\" to delete POSIX file \"%s\""
          filename))
      (revert-buffer)))

(provide 'my-dired)
