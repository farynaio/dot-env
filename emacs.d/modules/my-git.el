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

(use-package projectile
  :config
  (bind-key "C-c p" 'projectile-command-map projectile-mode-map)
  (bind-key "C-c p F" 'projectile-find-file-other-window projectile-mode-map)

  (setq projectile-completion-system 'ivy)
  (setq projectile-indexing-method 'hybrid)
  (setq projectile-enable-caching t)
  (setq projectile-verbose nil)
  (setq projectile-do-log nil)
  (setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))

  ;; (setq projectile-generic-command "fd . -0")

  (if (executable-find "ctags")
    (setq projectile-tags-command "ctags -R -e .")
    (warn "'ctags' not installed!"))

  (setq projectile-track-known-projects-automatically nil)
  (setq projectile-globally-ignored-file-suffixes '(".png" ".gif" ".pdf" ".class"))
  (setq projectile-globally-ignored-files '("TAGS" ".DS_Store" ".keep"))
  (setq projectile-globally-ignored-directories
        (append '("node-modules" "dist" "target") projectile-globally-ignored-directories))

  (projectile-mode 1)

  (add-hook 'projectile-after-switch-project-hook (lambda () (my/projectile-invalidate-cache nil)))

  (add-hook 'after-init-hook
            (lambda ()
              (mapc (lambda (project-root)
                      (remhash project-root projectile-project-type-cache)
                      (remhash project-root projectile-projects-cache)
                      (remhash project-root projectile-projects-cache-time)
                      (when projectile-verbose
                        (message "Invalidated Projectile cache for %s."
                                 (propertize project-root 'face 'font-lock-keyword-face))))
                    projectile-known-projects)
              (projectile-serialize-cache))))

(defhydra hydra-git ()
  "git"
  ("g" magit-blame "blame" :exit t)
  ("e" magit-ediff-popup "ediff" :exit t)
  ("c" vc-resolve-conflicts "conflicts" :exit t) ;; this could be better -> magit?
  ;; ("b" magit-bisect-popup "bisect") ;; find a commit that introduces the bug
  ("s" magit-status "status" :exit t)
  ("o" magit-checkout "checkout" :exit t)
  ("b" magit-branch-popup "branch" :exit t)
  ("d" magit-diff-popup "diff" :exit t)
  ("h" magit-diff-buffer-file "diff file" :exit t)
  ("z" magit-stash-popup "stash" :exit t)
  ("l" magit-log-all "log" :exit t)
  ("f" magit-log-buffer-file "file log" :exit t))

(defhydra hydra-project ()
  "Project"
  ("a" jarfar/projectile-show-relative-path "show path" :exit t)
  ("p" hydra-project-projectile/body "projectile add/remove" :exit t)
  ("t" projectile-find-tag "Find tag" :exit t)
  ("o" projectile-find-other-file "Find other file" :exit t)
  ("f" projectile-find-file "Find file" :exit t)
  ("r" projectile-replace-regexp "Replace" :exit t)
  ("i" projectile-invalidate-cache "Invalidate projectile cache" :exit t)
  ("b" modi/kill-non-project-buffers "Kill unrelated buffers" :exit t)
  ("d" my/dtrt-indent-mode-toggle "Toggle dtrt-indent-mode" :exit t)
  ;; ("t" treemacs "Toggle treemacs" :exit t)
  )

(defhydra hydra-project-projectile ()
  "Projectile project"
  ("a" my/projectile-add-known-project "add" :exit t)
  ("r" projectile-remove-known-project "remove" :exit t))

(defun jarfar/projectile-show-relative-path ()
  (interactive)
  (when (projectile-project-root)
    (message (substring buffer-file-name (length (projectile-project-root))))))

(defun my/projectile-add-known-project (project-root)
  (interactive (list (read-directory-name "Add to known projects: ")))
  (projectile-add-known-project project-root)
  (projectile-cleanup-known-projects))

(defun my/projectile-invalidate-cache (arg)
  "Remove the current project's files from `projectile-projects-cache'.

With a prefix argument ARG prompts for the name of the project whose cache
to invalidate."
  (interactive "P")
  (let ((project-root
         (if arg
             (completing-read "Remove cache for: " projectile-projects-cache)
           (projectile-project-root))))
    (setq projectile-project-root-cache (make-hash-table :test 'equal))
    (remhash project-root projectile-project-type-cache)
    (remhash project-root projectile-projects-cache)
    (remhash project-root projectile-projects-cache-time)
    (projectile-serialize-cache)
    (when projectile-verbose
      (message "Invalidated Projectile cache for %s."
               (propertize project-root 'face 'font-lock-keyword-face)))))

;; (use-package counsel-projectile
;;   :config
;;   (progn
;;     (counsel-projectile-mode 1)))

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