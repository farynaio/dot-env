(require 'recentf)

(eval-after-load 'recentf
  '(progn
    (recentf-mode 1)))

(setq recentf-max-saved-items 200
  recentf-max-menu-items 15)

(require 'tramp)
(setq tramp-default-method "ssh")
(setq password-cache-expiry nil)

(use-package move-text
  :config
  (progn
    (global-set-key [(meta up)] #'move-text-up)
    (global-set-key [(meta down)] #'move-text-down)))

(use-package goto-last-change
  :config
  (progn
    (bind-key "C-x C-\\" #'goto-last-change)))

(setq my/org-base-path "~/Documents/emacs")
(setq my/tools-path "~/.emacs.d/tools/")
(setq org-agenda-directory (expand-file-name "agenda" my/org-base-path))
(setq org-directory (expand-file-name "orgs" my/org-base-path))
(setq my/tmp-base-path (expand-file-name "tmp" my/org-base-path))
(setq my/drills-base-path (expand-file-name "drills" my/org-base-path))

(setq my/org-english-drill-file-path (expand-file-name "english_drill.org" my/drills-base-path))

(setq my/org-active-file-path (expand-file-name "active.org" org-agenda-directory))
(setq my/org-repeatables-file-path (expand-file-name "repeat.org" org-agenda-directory))
(setq my/org-anniversaries-file-path (expand-file-name "anniversaries.org" org-agenda-directory))
(setq my/org-languages-file-path (expand-file-name "languages.org" org-agenda-directory))
(setq my/org-inbox-file-path (expand-file-name "inbox.org" org-agenda-directory))
(setq my/org-knowledge-review-file-path (expand-file-name "knowledge_reviews.org" org-agenda-directory))
(setq my/org-yearly-goals-file-path (expand-file-name "yearly_goals.org" org-agenda-directory))

(setq my/org-diet-log-file-path (expand-file-name "diet.org" org-directory))
(setq my/org-tasks-file-path (expand-file-name "someday.org" org-directory))
(setq my/org-projects-file-path (expand-file-name "projects.org.gpg" org-directory))
(setq my/org-notes-file-path (expand-file-name "notes.org" org-directory))
(setq my/org-contacts-file-path (expand-file-name "contacts.org.gpg" org-directory))
(setq my/org-blog-file-path (expand-file-name "blog.org.gpg" org-directory))
(setq my/org-journal-file-path (expand-file-name "journal.org.gpg" org-directory))
(setq my/org-journal-dating-file-path (expand-file-name "journal_dating.org.gpg" org-directory))
(setq my/org-media-file-path (expand-file-name "media.org.gpg" org-directory))
(setq my/org-tools-file-path (expand-file-name "tools.org" org-directory))
(setq my/org-review-file-path (expand-file-name "reviews.org.gpg" org-directory))

(setq my/local-config-file-path (expand-file-name "local-config.el" user-emacs-directory))
(setq my/org-goals-file-path (expand-file-name "goals.org" org-directory))
(setq my/org-knowledge-file-path (expand-file-name "knowledge.org" org-directory))
(setq my/org-quotes-file-path (expand-file-name "quotes.org.gpg" org-directory))
(setq my/org-projects-folder (expand-file-name "projects" my/org-base-path))
(setq my/org-project-become-confident-pua (expand-file-name "become_confident_pua.org" my/org-projects-folder))
(setq my/org-project-trip-nottingham (expand-file-name "trip_to_nottingham.org" my/org-projects-folder))
(setq my/org-project-trip-edinburgh (expand-file-name "trip_to_edinburgh.org" my/org-projects-folder))
(setq my/org-project-setup-digital-agency (expand-file-name "setup_digital_agency.org.gpg" my/org-projects-folder))
(setq my/org-project-setup-freelance (expand-file-name "setup_freelance.org.gpg" my/org-projects-folder))
(setq my/org-project-launch-amazon-business (expand-file-name "launch_amazon_business.org.gpg" my/org-projects-folder))

(setq my/org-ledger-base-path (expand-file-name "ledger" my/org-base-path))
(setq my/org-current-ledger-file-path (expand-file-name "2018.dat" my/org-ledger-base-path))

(use-package smartscan
  :defer t
  :config
  (global-smartscan-mode t))

(bind-key "C-c p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

(require 'framemove)
;; (require 'grep)

(use-package neotree)

(setq default-directory "~/.emacs.d")

;; (setq display-buffer-alist
;;   '(
;;      ("^.+\\.org\\(\\.gpg\\)?$"
;;        (display-buffer-reuse-window) . ((reusable-frames . t)))
;;      ("^\\(\\..+\\)\\|\\(.+\\..+\\)$"
;;        (display-buffer-reuse-window display-buffer-same-window display-buffer-reuse-window display-buffer-pop-up-frame) . ((reusable-frames . t)))))


(use-package avy)
(use-package ivy-hydra)
(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :config
  (progn
    (ivy-mode 1)
    (bind-key "<return>" #'ivy-alt-done ivy-minibuffer-map)
    (bind-key "C-M-h" #'ivy-previous-line-and-call ivy-minibuffer-map)
    (bind-key "C-:" #'ivy-dired ivy-minibuffer-map)
    (bind-key "C-c o" #'ivy-occur ivy-minibuffer-map)
    (bind-key "C-'" #'ivy-avy ivy-minibuffer-map)
    (bind-key "C-x b" #'pop-to-buffer)
    ;; (bind-key "C-x b" #'display-buffer)

    (defun ivy-dired ()
      (interactive)
      (if ivy--directory
        (ivy-quit-and-run
          (dired ivy--directory)
          (when (re-search-forward
                  (regexp-quote
                    (substring ivy--current 0 -1)) nil t)
            (goto-char (match-beginning 0))))
        (user-error
          "Not completing files currently")))

    (defun ivy-view-backtrace ()
      (interactive)
      (switch-to-buffer "*ivy-backtrace*")
      (delete-region (point-min) (point-max))
      (fundamental-mode)
      (insert ivy-old-backtrace)
      (goto-char (point-min))
      (forward-line 1)
      (let (part parts)
        (while (< (point) (point-max))
          (condition-case nil
            (progn
              (setq part (read (current-buffer)))
              (push part parts)
              (delete-region (point-min) (point)))
            (error
              (progn
                (ignore-errors (up-list))
                (delete-region (point-min) (point)))))))
      (goto-char (point-min))
      (dolist (part parts)
        (lispy--insert part)
        (lispy-alt-multiline)
        (insert "\n")))

    (setq ivy-switch-buffer-faces-alist
      '((emacs-lisp-mode . swiper-match-face-1)
         (dired-mode . ivy-subdir)
         (org-mode . org-level-4)))

    (setq
      ;; ivy-use-virtual-buffers t
      ivy-height 10
      ivy-use-selectable-prompt t
      ivy-count-format "(%d/%d) "
      ivy-re-builders-alist '((t   . ivy--regex-ignore-order)))
    )
  )

(use-package swiper
  :config
  (progn
    (bind-key "C-s" #'swiper)))

(use-package counsel
  :config
  (progn
    (setq counsel-find-file-ignore-regexp "\\`\\.")
    (bind-key "C-r" #'counsel-expression-history read-expression-map)
    (bind-key "C-r" #'counsel-minibuffer-history read-expression-map)
    (bind-key "M-x" #'counsel-M-x)
    (bind-key "C-x C-f" #'counsel-find-file)
    (bind-key "<f1> f" #'counsel-describe-function)
    (bind-key "<f1> v" #'counsel-describe-variable)
    (bind-key "<f1> l" #'counsel-find-library)
    (bind-key "<f2> s" #'counsel-info-lookup-symbol)
    (bind-key "<f2> u" #'counsel-unicode-char)
    (bind-key "C-h f" #'counsel-describe-function)
    (bind-key "C-h v" #'counsel-describe-variable)
    (bind-key "C-x r b" #'counsel-bookmark)
    ;; (global-set-key (kbd "C-c g") 'counsel-git)
    ;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
    ;; (global-set-key (kbd "C-c k") 'counsel-ack)

    ;; (setq counsel-grep-base-command "grep -niE %s %s")
    ;; (setq counsel-grep-base-command
    ;;   "rg -i -M 120 --no-heading --line-number --color never %s %s")
    ;; (setq counsel-rg-base-command
    ;;   "rg -i -M 120 --no-heading --line-number --color never %s .")
    ;; (setq counsel-git-grep-cmd-default
    ;;   (concat "git --no-pager grep --full-name -n --no-color -i -e '%s' -- './*' "
    ;;     (mapconcat (lambda (x) (format "':!*.%s'" x))
    ;;       '("htm" "so" "a" "TTC" "NDS" "png" "md5") " ")))
    ;; (setq counsel-git-grep-projects-alist
    ;;   (list
    ;;     (cons "/home/oleh/Dropbox/source/site-lisp/"
    ;;       (concat "/home/oleh/Dropbox/source/site-lisp/etc/git-multi-grep '%s' "
    ;;         "/home/oleh/Dropbox/source/site-lisp 'git/*'"))
    ;;     (cons "/home/oleh/git/ivy-dependencies/"
    ;;       (concat "/home/oleh/Dropbox/source/site-lisp/etc/git-multi-grep '%s' "
    ;;         "/home/oleh/git/ivy-dependencies '*'"))))
    ;; (setq counsel-git-cmd "rg --files")


    (ivy-set-display-transformer 'counsel-describe-function nil)
    )
  )

(use-package projectile
  :init
  (setq
    projectile-completion-system 'ivy
    projectile-indexing-method 'alien
    projectile-enable-caching t
    projectile-verbose nil
    projectile-do-log nil
    projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))
  :config
  (progn
    projectile-track-known-projects-automatically nil))

(use-package counsel-projectile
  :config
  (progn
    (counsel-projectile-mode 1)))

(use-package undo-tree
  :config
  (progn
    (diminish 'undo-tree-mode)
    (setq undo-tree-visualizer-diff t)
    (evil-make-overriding-map undo-tree-visualizer-mode-map 'motion)
    (evil-make-overriding-map undo-tree-visualizer-selection-mode-map 'motion)
    (evil-make-overriding-map undo-tree-map 'motion)))

(eval-after-load 'ediff
  '(progn
     (setq ediff-window-setup-function 'ediff-setup-windows-plain
       ediff-forward-word-function 'forward-char
       ediff-use-toolbar-p nil)
     (add-hook 'ediff-before-setup-hook 'new-frame)
     (add-hook 'ediff-quit-hook 'delete-frame)))


(set-register ?a (cons 'file my/org-active-file-path))
(set-register ?t (cons 'file my/org-tasks-file-path))
(set-register ?p (cons 'file my/org-projects-file-path))
(set-register ?n (cons 'file my/org-anniversaries-file-path))
(set-register ?g (cons 'file my/org-goals-file-path))
(set-register ?k (cons 'file my/org-knowledge-file-path))
(set-register ?i (cons 'file my/org-inbox-file-path))
(set-register ?j (cons 'file my/org-journal-file-path))
(set-register ?h (cons 'file my/org-repeatables-file-path)) ; repeat, habit
(set-register ?l (cons 'file my/local-config-file-path))
(set-register ?w (cons 'file my/org-languages-file-path))
(set-register ?q (cons 'file my/org-quotes-file-path))
(set-register ?t (cons 'file my/org-tools-file-path))
(set-register ?c (cons 'file (expand-file-name "init.el" user-emacs-directory)))

(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(windmove-default-keybindings)
(setq windmove-wrap-around t)
(setq framemove-hook-into-windmove t)

;; org mode conflicts resolution: windmove
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(global-set-key [remap move-beginning-of-line]
                'my/smarter-move-beginning-of-line)


(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(global-set-key (kbd "<home>") 'left-word)
(global-set-key (kbd "<end>") 'right-word)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)

(global-unset-key (kbd "C-x c"))
(global-unset-key (kbd "C-x <C-left>"))
(global-unset-key (kbd "C-x <C-right>"))
(global-unset-key (kbd "<kp-end>"))
(global-unset-key (kbd "<kp-home>"))
(global-unset-key (kbd "<end>"))
(global-unset-key (kbd "<home>"))
(global-unset-key (kbd "C-x C-'"))
(global-unset-key (kbd "<S-up>"))
(global-unset-key (kbd "<S-down>"))
(global-unset-key (kbd "C-x <down>"))

(defalias 'qcalc #'quick-calc)

(provide 'my-navigation)
