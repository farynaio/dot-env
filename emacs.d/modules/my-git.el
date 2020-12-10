(require 'smerge-mode)
(require 'vc-git)
;; (require 'git-rebase)

;; (use-package git-timemachine)

(setq vc-follow-symlinks t)

(eval-after-load 'smerge-mode
  '(progn
     (defun my/smerge-mode-setup ()
       (bind-key "[w" 'smerge-prev smerge-mode-map)
       (bind-key "]w" 'smerge-next smerge-mode-map))
     (add-hook 'smerge-mode-hook 'my/smerge-mode-setup)))

(use-package git-commit
  :config
  (setq git-commit-style-convention-checks nil))

(use-package git-gutter
  :diminish git-gutter-mode
  :bind (("C-c p" . 'git-gutter:previous-hunk)
          ("C-c n" . 'git-gutter:next-hunk))
  :config
  (global-git-gutter-mode +1))

(eval-after-load 'git-rebase
  '(progn
     (add-hook 'git-rebase-mode-hook (lambda () (read-only-mode -1)))))

(use-package transient) ;; magit dependency

(use-package magit
  :diminish magit-auto-revert-mode
  :after transient
  :hook ((magit-git-mode . (lambda () (read-only-mode nil)))
          (magit-status-mode . (lambda () (save-some-buffers t))))
  :bind (:map magit-mode-map
          ("|" . evil-window-set-width)
          ("}" . evil-forward-paragraph)
          ("]" . evil-forward-paragraph)
          ("{" . evil-backward-paragraph)
          ("[" . evil-backward-paragraph)
          ("C-d" . evil-scroll-down)
          ("C-u" . evil-scroll-up)
          ("C-s" . isearch-forward)
          ("=" . balance-windows)
          ("C-w" . my/copy-diff-region)
          :map magit-hunk-section-map
          ("r" . magit-reverse)
          ("v" . evil-visual-char)
          :map magit-revision-mode-map
          ("C-s" . isearch-forward)
          ("n" . evil-search-next)
          ("p" . evil-search-previous)
          ("=" . balance-windows)
          :map magit-status-mode-map
          ("\\w" . avy-goto-word-or-subword-1)
          ("\\c" . avy-goto-char))
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq magit-refresh-status-buffer nil)
  (setq magit-item-highlight-face 'bold)
  (setq magit-diff-paint-whitespace nil)
  (setq magit-ediff-dwim-show-on-hunks t)
  (setq magit-diff-hide-trailing-cr-characters t)
  (setq magit-bury-buffer-function 'magit-mode-quit-window)

  (setq auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffers-p)
  (setq vc-handled-backends (delq 'Git vc-handled-backends))
  (setq magit-blame-styles
    '(
       (margin
         (margin-format " %s%f" " %C %a" " %H")
         (margin-width . 42)
         (margin-face . magit-blame-margin)
         (margin-body-face magit-blame-dimmed))
       (headings
         (heading-format . "%-20a %C %s
"))))

  (add-to-list 'magit-blame-disable-modes 'evil-mode)

  (magit-add-section-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream 'magit-insert-unpushed-to-upstream-or-recent)
  (magit-add-section-hook 'magit-status-sections-hook 'magit-insert-recent-commits 'magit-insert-unpushed-to-upstream-or-recent)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent))

(defun my/copy-diff-region ()
  "Copy diff region without + or - markers."
  (interactive)
  (deactivate-mark)
  (let ((text (buffer-substring-no-properties
               (region-beginning) (region-end))))
    (kill-new (replace-regexp-in-string "^[\\+\\-]" "" text))))


(provide 'my-git)