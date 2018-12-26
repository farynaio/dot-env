; (require 'sunrise-commander)

(setq
  dired-dwim-target t
  dired-use-ls-diredto nil
  dired-recursive-copies 'always
  dired-recursive-deletes 'always)

(bind-key "C-c -" #'diredp-up-directory-reuse-dir-buffer dired-mode-map)

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

     (evil-make-overriding-map dired-mode-map 'motion)
     (evil-make-overriding-map dired-mode-map 'normal)
     ))

(defun my/dired-jump-make-new-window ()
  "Open new vertical window and open dired there."
  (interactive)
  (split-window-right)
  (windmove-right)
  (dired-jump))

(add-hook 'dired-mode-hook
  (lambda ()
    (dired-hide-details-mode 1)
    (hl-line-mode 1)))

(provide 'my-dired)
