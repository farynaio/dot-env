(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Trash")

(when (file-executable-p "/usr/local/bin/gls")
  (setq insert-directory-program "/usr/local/bin/gls")
  (setq dired-listing-switches "-alh1v"))

(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-dirs-first t)
  (setq ls-lisp-use-insert-directory-program nil))

(eval-after-load 'dired
  '(progn
     (require 'dired+)
     (use-package image-dired)
     ;; (use-package image-dired+)
     ;; (require 'bookmark+)

     (setq dired-dwim-target t)
     (setq dired-use-ls-diredto nil)
     (setq dired-recursive-copies 'always)
     (setq dired-recursive-deletes 'always)
     (setq dired-listing-switches "-alh")
     (setq dired-deletion-confirmer 'y-or-n-p)
     (setq dired-clean-confirm-killing-deleted-buffer nil)
     (setq dired-hide-details-mode 1)
     ;; (setq find-name-arg "-iregex")
     (setq dired-guess-shell-alist-user
       '(("\\.pdf\\'" "open")
          ("\\.html\\'" "open")
          ("\\.xlsx?m?\\'" "open")
          ("\\.ods\\'" "open")
          ("\\.\\(?:jpe?g\\|png\\|gif\\)\\'" "open")
          ("\\.\\(?:mp3\\|ogg\\)\\'" "open")
          ("\\.\\(?:mpe?g\\|mp4\\|avi\\|wmv\\)\\'" "open")))
     ;; (bind-key (kbd "t") #'dired-toggle-marks dired-mode-map) ; toggle marks
     ;; (define-key (kbd "<left>") (diredp-up-directory-reuse-dir-buffer))

     (when (fboundp 'evil-make-overriding-map)
       (evil-make-overriding-map dired-mode-map 'motion)
       (evil-make-overriding-map dired-mode-map 'normal)
       (bind-key "n" 'evil-ex-search-next dired-mode-map)
       (bind-key "N" 'evil-ex-search-previous dired-mode-map)
       (bind-key "n" 'evil-search-next dired-mode-map)
       (bind-key "N" 'evil-search-previous dired-mode-map))

     (add-hook 'dired-mode-hook
       (lambda ()
         (dired-hide-details-mode 1)
         (hl-line-mode 1)))

     (bind-key "J" 'dired-goto-file dired-mode-map)
     (bind-key "C-s" 'find-name-dired dired-mode-map)
     (bind-key "C-c -" 'diredp-up-directory-reuse-dir-buffer dired-mode-map)
     (bind-key "<" 'beginning-of-buffer dired-mode-map)
     (bind-key ">" 'end-of-buffer dired-mode-map)
     (bind-key "W" 'my/dired-copy-dirname-as-kill dired-mode-map)))

(eval-after-load 'dired+
  '(progn
    (setq diredp-hide-details-initially-flag nil)
    (setq diredp-auto-focus-frame-for-thumbnail-tooltip-flag t)
    (diredp-toggle-find-file-reuse-dir 1)
    (bind-key "<backspace>" 'diredp-up-directory-reuse-dir-buffer dired-mode-map)))

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

;; http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html
;; (defun my/open-in-external-app (&optional @fname)
;;   "Open the current file or dired marked files in external app.
;; The app is chosen from your OS's preference.
;; When called in emacs lisp, if @fname is given, open that."
;;   (interactive)
;;   (let* (
;;          ($file-list
;;           (if @fname
;;               (progn (list @fname))
;;             (if (string-equal major-mode "dired-mode")
;;                 (dired-get-marked-files)
;;               (list (buffer-file-name)))))
;;          ($do-it-p (if (<= (length $file-list) 5)
;;                        t
;;                      (y-or-n-p "Open more than 5 files? "))))
;;     (when $do-it-p
;;       (cond
;;        ((string-equal system-type "windows-nt")
;;         (mapc
;;          (lambda ($fpath)
;;            (w32-shell-execute "open" $fpath)) $file-list))
;;        ((string-equal system-type "darwin")
;;         (mapc
;;          (lambda ($fpath)
;;            (shell-command
;;             (concat "open " (shell-quote-argument $fpath))))  $file-list))
;;        ((string-equal system-type "gnu/linux")
;;         (mapc
;;          (lambda ($fpath) (let ((process-connection-type nil))
;;                             (start-process "" nil "xdg-open" $fpath))) $file-list))))))

(provide 'my-dired)
