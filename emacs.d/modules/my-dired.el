(setq-default delete-by-moving-to-trash t)

(cond
  ((eq system-type 'darwin)
    (setq-default trash-directory "~/.Trash")
    ;; https://emacs.stackexchange.com/questions/63336/deleting-a-file-with-name-that-already-exists-in-trash/63342#63342
    (when (eq system-type 'darwin)
      (defun system-move-file-to-trash (filename)
        "Move file or directory named FILENAME to the trash."
        (save-excursion
          (ns-do-applescript
            (format
              "tell application \"Finder\" to delete POSIX file \"%s\""
              filename))))))
  ((eq system-type 'gnu/linux)
    (setq-default trash-directory "~/.local/share/Trash/files")))

(use-package ls-lisp
  :straight nil
  :if (eq system-type 'darwin)
  :custom
  (ls-lisp-dirs-first t)
  (ls-lisp-use-insert-directory-program nil))

(use-package dired
  :straight nil
  :demand t
  :delight "Dired "
  :hook ((dired-mode . dired-hide-details-mode)
          (dired-mode . hl-line-mode)
          (dired-mode . all-the-icons-dired-mode))
  :bind (:map dired-mode-map
          ("<tab>" . dired-subtree-cycle)
          ("<S-tab>" . dired-subtree-toggle)
          ("i" . dired-subtree-toggle)
          ("C-<return>" . dired-find-file-other-window)
          ("C-c C-n" . dired-narrow)
          ("C-c C-f" . dired-narrow-fuzzy)
          ("C-c C-r" . dired-narrow-regexp))
  :preface
  (defun my/dired-shell-command ()
    "Run any shell command in Dired."
    (interactive)
    (let ((cmd (read-string "Run shell command: ")))
      (if cmd
        (dired-run-shell-command cmd)
        (user-error "Command is required!"))))
  :custom
  (find-name-arg "-iname")
  (dired-dwim-target t)
  (dired-use-ls-diredto nil)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-listing-switches "-alh --group-directories-first")
  (dired-deletion-confirmer 'y-or-n-p)
  (dired-auto-revert-buffer t)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-hide-details-mode 1)
  ;; (dired-guess-shell-alist-user
  ;;   '(
       ;; ("\\.pdf\\'" "open")
       ;; ("\\.html\\'" "open")
       ;; ("\\.xlsx?m?\\'" "open")
       ;; ("\\.ods\\'" "open")
       ;; ("\\.\\(?:jpe?g\\|png\\|gif\\)\\'" "open")
       ;; ("\\.\\(?:mp3\\|ogg\\)\\'" "open")
       ;; ("\\.\\(?:mpe?g\\|mp4\\|avi\\|wmv\\)\\'" "open")))
  :config
  (evil-define-key 'normal dired-mode-map
    (kbd "C-s") #'find-name-dired
    (kbd "<") #'beginning-of-buffer
    (kbd ">") #'end-of-buffer
    (kbd "C-x w") #'my/dired-copy-dirname-as-kill
    (kbd "W") #'my/dired-copy-path-to-file-as-kill
    (kbd "k") (lambda () (interactive) (dired-do-kill-lines t))
    (kbd "r") #'my/rgrep
    (kbd "C-w =") #'balance-windows
    (kbd "C-w |") #'maximize-window
    (kbd "C-w q") #'evil-quit
    (kbd "C-w v") #'split-window-right
    (kbd "/") #'evil-ex-search-forward
    (kbd "n") #'evil-ex-search-next
    (kbd "N") #'evil-ex-search-previous
    (kbd "<backspace>") (lambda () (interactive) (my/dired-go-up-reuse "..")))

  (evil-define-key '(normal visual) dired-mode-map
    (kbd ",l") #'my/hydra-dired/body)

  (evil-define-key 'normal global-map
    (kbd ",m") #'my/dired-jump-make-new-window)

  (pretty-hydra-define my/hydra-dired
    (:hint nil :color teal :quit-key "q" :title (with-faicon "folder" "Dired" 1 -0.05))
    ("Basic"
      (("x" my/dired-shell-command "run command" :exit t)
        ("s" find-name-dired "find regexp" :exit t)
        ("c" dired-copy-filename-as-kill "copy filename" :exit t)
        ("u" my/sudo-dired "sudo dired" :exit t)
        ("m" dired-mark-files-regexp "mark files with regexp" :exit t))
      "Encryption"
      (("ee" epa-dired-do-encrypt "encrypt files" :exit t)
        ("ed" epa-dired-do-decrypt "decrypt files" :exit t)
        ("es" epa-dired-do-sign "sign files" :exit t)
        ("ev" epa-dired-do-verify "decrypt file" :exit t))
      "File"
      (("fg" dired-do-chgrp "chgrp" :exit t)
        ("fm" dired-do-chmod "chmod" :exit t)
        ("fo" dired-do-chown "chown" :exit t)
        ("fl" dired-do-symlink "symlink" :exit t)
        ("fc" dired-do-compress "compress" :exit t))
    "Navigate"
    (("k" my/treemacs-project-toggle "treemacs" :toggle t :exit t))))

  (when (eq system-type 'darwin)
    (if (executable-find "gls")
      (setq
        insert-directory-program "/usr/local/bin/gls"
        dired-listing-switches "-alh1v")
      (message "Executable 'gls' not found")))

  (put 'dired-find-alternate-file 'disabled nil)

  (defun my/sudo-dired ()
    "Run Dired in sudo mode."
    (interactive)
    (dired "/sudo::/"))

  (defun my/dired-go-up-reuse (&optional dir)
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
    "Copy current file directory into the kill ring."
    (interactive)
    (message (format "Path '%s' copied to clipboard." default-directory))
    (kill-new default-directory))

  (defun my/dired-copy-path-to-file-as-kill (&optional arg)
    "Copy current file path into the kill ring."
    (interactive "P")
    (let ((string
            (or (dired-get-subdir)
              (mapconcat #'identity
                (if arg
                  (cond ((zerop (prefix-numeric-value arg))
                          (dired-get-marked-files))
                    ((consp arg)
                      (dired-get-marked-files t))
                    (t
                      (dired-get-marked-files
				                'no-dir (prefix-numeric-value arg))))
                  (dired-get-marked-files 'no-dir))
                " "))))
      (unless (string= string "")
        (let ((new-kill (concat default-directory string)))
               (kill-new new-kill)
          (message "%s" new-kill))))))

(use-package all-the-icons-dired
  :after (dired all-the-icons)
  :diminish all-the-icons-dired-mode
  :commands all-the-icons-dired-mode)

(use-package dired-subtree
  :after dired
  :commands (dired-subtree-cycle dired-subtree-toggle dired-subtree-toggle))

(use-package dired-narrow
  :after dired
  :commands (dired-narrow dired-narrow-fuzzy dired-narrow-regexp))

(provide 'my-dired)
;;; my-dired.el ends here
