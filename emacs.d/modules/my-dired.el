(require 'sunrise-commander)

(setq
  dired-use-ls-diredto nil
  dired-recursive-copies 'always
  dired-recursive-deletes 'always)

(bind-key "C-c -" #'diredp-up-directory-reuse-dir-buffer dired-mode-map)

(eval-after-load 'sunrise-commander
  '(progn
     (defun mc ()
       "Open sunrise commander in default directory."
       (interactive)
       (make-frame-command)
       (sunrise default-directory default-directory))

     (bind-key "<backspace>" #'sr-dired-prev-subdir sr-mode-map)

     ;; delete redundant window in MC mode
     (add-hook 'sr-start-hook (lambda () (delete-window (car (last (window-list))))))))

(use-package dired+
  :init
    (setq diredp-hide-details-initially-flag nil)
  :config
  (progn
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

(eval-after-load 'dired-mode
  (lambda ()
    (define-key (kbd "t") (dired-toggle-marks)) ; toggle marks
    ;; (define-key (kbd "<left>") (diredp-up-directory-reuse-dir-buffer))
    ))

(provide 'my-dired)
