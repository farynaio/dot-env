; (require 'sunrise-commander)

(setq
  dired-dwim-target t
  dired-use-ls-diredto nil
  dired-recursive-copies 'always
  dired-recursive-deletes 'always
  dired-listing-switches "-alh"
  )

(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq
    ls-lisp-dirs-first t
    ls-lisp-use-insert-directory-program nil
    ))

(bind-key "C-c -" #'diredp-up-directory-reuse-dir-buffer dired-mode-map)
(bind-key "n" #'evil-search-next                         dired-mode-map)
(bind-key "N" #'evil-search-previous                     dired-mode-map)
(bind-key "<" #'beginning-of-buffer                      dired-mode-map)
(bind-key ">" #'end-of-buffer                            dired-mode-map)
(bind-key "n" #'evil-ex-search-next                      dired-mode-map)
(bind-key "N" #'evil-ex-search-previous                  dired-mode-map)
(bind-key "W" #'my/dired-copy-dirname-as-kill            dired-mode-map)

; (eval-after-load 'sunrise-commander
;   '(progn
;      (defun mc ()
;        "Open sunrise commander in default directory."
;        (interactive)
;        (make-frame-command)
;        (sunrise default-directory default-directory))

;      (bind-key "<backspace>" #'sr-dired-prev-subdir sr-mode-map)

;      ;; delete redundant window in MC mode
;      (add-hook 'sr-start-hook (lambda () (delete-window (car (last (window-list))))))))

(require 'dired+)

(eval-after-load 'dired+
  '(progn
    (setq diredp-hide-details-initially-flag nil)
    (diredp-toggle-find-file-reuse-dir 1)
    (setq diredp-auto-focus-frame-for-thumbnail-tooltip-flag t)
    (bind-key (kbd "<backspace>") #'diredp-up-directory-reuse-dir-buffer dired-mode-map)))

;; (require 'dired+)
;; (eval-after-load 'dired+
;;   '(progn
;;     (setq diredp-hide-details-initially-flag nil)
;;     (diredp-toggle-find-file-reuse-dir 1)
;;     (setq diredp-auto-focus-frame-for-thumbnail-tooltip-flag t)
;;     (bind-key (kbd "<backspace>") #'diredp-up-directory-reuse-dir-buffer dired-mode-map)))

;; (use-package image-dired+)
(use-package image-dired)
;; (require 'bookmark+)

(eval-after-load 'dired
  '(progn
     (setq dired-hide-details-mode 1)
     (setq find-name-arg "-iregex")
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
     (bind-key "J" #'dired-goto-file dired-mode-map)
     (bind-key "C-s" #'find-name-dired dired-mode-map)

     (evil-make-overriding-map dired-mode-map 'motion)
     (evil-make-overriding-map dired-mode-map 'normal)

     (defun my/dired-mode-setup ()
       (dired-hide-details-mode 1)
       (hl-line-mode 1))

     (add-hook 'dired-mode-hook #'my/dired-mode-setup)
     ))

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
