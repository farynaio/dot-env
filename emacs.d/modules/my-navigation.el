;; (require 'tramp)
(require 'recentf)
(require 'ediff)
(require 'help)

(bind-key "C-c p" 'pop-to-mark-command)

(setq set-mark-command-repeat-pop t)
(setq default-directory "~/")
(setq initial-buffer-choice t) ;; *scratch* as default buffer

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-forward-word-function 'forward-char)

(eval-after-load 'recentf
  '(progn
     (setq recentf-max-saved-items 200)
     (setq recentf-max-menu-items 15)
     (recentf-mode 1)))

(eval-after-load 'help
  '(progn
     (bind-key "/"          'evil-search-forward       help-mode-map)
     (bind-key "v"          'set-mark-command          help-mode-map)
     (bind-key "C-w ="      'balance-windows           help-mode-map)
     (bind-key "n"          'evil-search-next          help-mode-map)
     (bind-key "N"          'evil-search-previous      help-mode-map)
     (bind-key "w"          'evil-forward-word-begin   help-mode-map)
     (bind-key "e"          'evil-forward-word-end     help-mode-map)
     (bind-key "E"          'evil-forward-WORD-end     help-mode-map)
     (bind-key "b"          'evil-backward-word-begin  help-mode-map)
     (bind-key "B"          'evil-backward-WORD-begin  help-mode-map)
     (bind-key "y"          'evil-yank                 help-mode-map)
     (bind-key "gg"         'evil-goto-first-line      help-mode-map)
     (bind-key "G"          'evil-goto-line            help-mode-map)))

(eval-after-load 'tramp
  '(progn
     (setq tramp-default-method "ssh")
     (setq tramp-inline-compress-start-size 40960)
     (setq tramp-chunksize 500)
     (setq tramp-auto-save-directory "~/.emacs.d/tramp-autosaves/")
     (setq tramp-persistency-file-name  "~/.emacs.d/tramp-persistency.el")
     (setq tramp-encoding-shell "/bin/ksh")))

(setq password-cache-expiry nil)

(use-package anzu
  :diminish anzu-mode
  :bind ("M-%" . 'anzu-query-replace-regexp)
  :config
  (global-anzu-mode 1))

(use-package drag-stuff
  :diminish drag-stuff-mode
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  ;; (add-to-list 'drag-stuff-except-modes 'my/org-taskjuggler-mode)
  (drag-stuff-global-mode 1)
  (define-key drag-stuff-mode-map (drag-stuff--kbd 'up) 'drag-stuff-up)
  (define-key drag-stuff-mode-map (drag-stuff--kbd 'down) 'drag-stuff-down))

(use-package goto-last-change)

(use-package smartscan
  :defer t
  :config
  (global-smartscan-mode 1))

(require 'framemove)
(require 'compile)
(require 'grep)
(require 'files)

(defun my/rgrep ()
  (interactive)
  (if (executable-find "ack")
    (let* ((regexp (grep-read-regexp))
            (dir (read-directory-name "Base directory: " nil default-directory t))
            (command (concat "ack '" regexp "' " dir)))
      (unless (file-accessible-directory-p dir)
        (error (concat "directory: '" dir "' is not accessible.")))
	    (compilation-start command 'grep-mode))
    ))

;; (use-package neotree)

;; (use-package ace-jump-mode
;;   :config
;;   (progn
;;     (when (fboundp 'evil-mode)
;;       (bind-key "\\w" 'ace-jump-word-mode  evil-motion-state-map)
;;       (bind-key "\\c" 'ace-jump-mode       evil-motion-state-map)
;;       (bind-key "\\a" 'ivy-imenu-anywhere  evil-motion-state-map)
;;       )))

;; (setq display-buffer-alist
;;   '(
;;      ("^.+\\.org\\(\\.gpg\\)?$"
;;        (display-buffer-reuse-window) . ((reusable-frames . t)))
;;      ("^\\(\\..+\\)\\|\\(.+\\..+\\)$"
;;        (display-buffer-reuse-window display-buffer-same-window display-buffer-reuse-window display-buffer-pop-up-frame) . ((reusable-frames . t)))))

;; (use-package treemacs
;;   :defer t
;;   :commands treeemacs
;;   :config
;;   (treemacs-follow-mode 1)
;;   (treemacs-fringe-indicator-mode 1)
;;   (treemacs-git-mode 'simple)
;;   (treemacs-filewatch-mode 1))

;; (use-package treemacs-projectile
;;   :after projectile treemacs)

;; (use-package treemacs-evil
;;   :after evil treemacs)

;; (use-package treemacs-magit
;;   :after treemacs magit)

(use-package avy
  :bind (:map help-mode-map
          ("\\c" . 'avy-goto-char)
          ("\\w" . 'avy-goto-word-or-subword-1))
  :config
  (when (fboundp 'evil-mode)
    (bind-key "\\w" 'avy-goto-word-or-subword-1  evil-motion-state-map)
    (bind-key "\\c" 'avy-goto-char evil-motion-state-map)))

(use-package ivy-hydra
  :after ivy hydra)

(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (bind-key "<return>" 'ivy-alt-done ivy-minibuffer-map)
  (bind-key "C-M-h" 'ivy-previous-line-and-call ivy-minibuffer-map)
  (bind-key "C-:" 'ivy-dired ivy-minibuffer-map)
  (bind-key "C-c o" 'ivy-occur ivy-minibuffer-map)
  (bind-key "C-'" 'ivy-avy ivy-minibuffer-map)

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

  (setq ivy-height 15)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-use-virtual-buffer t)
  (setq ivy-re-builders-alist '((t   . ivy--regex-ignore-order))))

(use-package ivy-rich
  :hook (org-mode . (lambda () (ivy-rich-local-mode 1)))
  :config
  (setq ivy-rich-parse-remote-buffer nil)
  (setq ivy-rich-path-style 'full)

  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

  (defun jarfar/ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
      (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
        (if (symbolp icon)
          (all-the-icons-icon-for-mode 'fundamental-mode)
          icon))))

  (defun jarfar/ivy-switch-buffer-org-roam-title (candidate)
    (if (ivy-rich-switch-buffer-user-buffer-p candidate)
      (let* ((file (buffer-file-name (get-buffer candidate)))
              (file (if (and (fboundp 'org-roam--org-roam-file-p) (org-roam--org-roam-file-p file)) (org-roam-db--get-title file) "")))
        (if file file ""))
      ""))

  (setq ivy-rich-display-transformers-list
    '(ivy-switch-buffer
       (:columns
         ((jarfar/ivy-rich-switch-buffer-icon (:width 2))
           (ivy-rich-candidate (:width 30))
           (jarfar/ivy-switch-buffer-org-roam-title (:width 40))
           (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
         :predicate (lambda (cand) (get-buffer cand)))
       counsel-find-file
       (:columns
         ((ivy-read-file-transformer)
           (ivy-rich-counsel-find-file-truename
             (:face font-lock-doc-face))))
       counsel-M-x
       (:columns
         ((counsel-M-x-transformer
            (:width 40))
           (ivy-rich-counsel-function-docstring
             (:face font-lock-doc-face))))
       counsel-describe-function
       (:columns
         ((counsel-describe-function-transformer
            (:width 40))
           (ivy-rich-counsel-function-docstring
             (:face font-lock-doc-face))))
       counsel-describe-variable
       (:columns
         ((counsel-describe-variable-transformer
            (:width 40))
           (ivy-rich-counsel-variable-docstring
             (:face font-lock-doc-face))))
       counsel-recentf
       (:columns
         ((ivy-rich-candidate
            (:width 0.8))
           (ivy-rich-file-last-modified-time
             (:face font-lock-comment-face))))
       package-install
       (:columns
         ((ivy-rich-candidate
            (:width 30))
           (ivy-rich-package-version
             (:width 16 :face font-lock-comment-face))
           (ivy-rich-package-archive-summary
             (:width 7 :face font-lock-builtin-face))
           (ivy-rich-package-install-summary
             (:face font-lock-doc-face))))))

  (define-minor-mode ivy-rich-local-mode
    "Toggle ivy-rich mode locally."
    :global nil
    (if ivy-rich-local-mode
      (unless ivy-rich--original-display-transformers-list
        (ivy-rich-set-display-transformer))
      (ivy-rich-unset-display-transformer))))

(use-package counsel
  :config
  (setq counsel-find-file-ignore-regexp "\\`\\.")
  (setq counsel-grep-base-command "grep -E -n -i -e %s %s")

  (bind-key "C-r" 'counsel-expression-history read-expression-map)
  (bind-key "C-r" 'counsel-minibuffer-history read-expression-map)
  (bind-key "M-x" 'counsel-M-x)
  (bind-key "C-x C-f" 'counsel-find-file)
  (bind-key "<f1> f" 'counsel-describe-function)
  (bind-key "<f1> v" 'counsel-describe-variable)
  (bind-key "<f1> l" 'counsel-find-library)
  (bind-key "<f2> s" 'counsel-info-lookup-symbol)
  (bind-key "<f2> u" 'counsel-unicode-char)
  (bind-key "C-h f" 'counsel-describe-function)
  (bind-key "C-h v" 'counsel-describe-variable)
  (bind-key "C-h a" 'counsel-apropos)
  (bind-key "C-x r b" 'counsel-bookmark)
  ;; (bind-key "C-x b" 'counsel-ibuffer)
  (bind-key "C-x b" 'counsel-switch-buffer)
  (bind-key "C-x C-b" 'counsel-switch-buffer-other-window)
  ;; (bind-key "C-x C-b" 'my/counsel-ibuffer-other-window)
  (bind-key "C-x C-r" 'counsel-recentf)

  (when (fboundp 'evil-mode)
    (bind-key "\\a" 'counsel-imenu  evil-motion-state-map))

  (defun my/counsel-ibuffer-other-window (&optional name)
    "Use ibuffer to switch to another buffer.
NAME specifies the name of the buffer (defaults to \"*Ibuffer*\")."
    (interactive)
    (setq counsel-ibuffer--buffer-name (or name "*Ibuffer*"))
    (ivy-read "Switch to buffer: " (counsel-ibuffer--get-buffers)
      :history 'counsel-ibuffer-history
      :action 'counsel-ibuffer-visit-buffer-other-window
      :caller 'counsel-ibuffer))

  (advice-add 'counsel-grep :around 'my/counsel-grep-fallback)
  (ivy-set-display-transformer 'counsel-describe-function nil)

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
  )

;; (use-package swiper)

(defun my/counsel-grep-fallback (orig-fun &rest args)
  "Fallback counsel-grep to evil-search-forward if exists if not search-forward."
  (when (my/buffer-tramp-p)
    (if (fboundp 'evil-search-forward)
      (apply 'evil-search-forward args)
      (apply 'search-forward args)))
    ;; (apply orig-fun args))

  ;; (if (or (string= major-mode "dired-mode") (string= major-mode "org-mode") (string= major-mode "help-mode") (string= "*scratch*" (buffer-name)) (string= "*Org Agenda*" (buffer-name)) (string-match ".gpg\\'" (buffer-name)))
  (if (fboundp 'swiper)
    (apply 'swiper args)
    (if (fboundp 'evil-search-forward)
      (apply 'evil-search-forward args)
      (apply 'search-forward args))))

;; (use-package undo-tree
;;   :defer t
;;   :diminish undo-tree-mode
;;   :config
;;   (setq undo-tree-visualizer-diff t)
;;   (when (fboundp 'evil-make-overriding-map)
;;     (evil-make-overriding-map undo-tree-visualizer-mode-map 'motion)
;;     (evil-make-overriding-map undo-tree-visualizer-selection-mode-map 'motion)
;;     (evil-make-overriding-map undo-tree-map 'motion)))

(use-package ag
  :config
  (setq ag-reuse-buffers t))

;; (setq ediff-split-window-function
;;   (if (> (frame-width) 150)
;; 		'split-window-horizontally
;; 		'split-window-vertically))

;; (add-hook 'ediff-before-setup-hook 'new-frame)
;; (add-hook 'ediff-quit-hook 'delete-frame)

;; Bring back window configuration after ediff quits
(defvar my-ediff-bwin-config nil "Window configuration before ediff.")
(defcustom my-ediff-bwin-reg ?b
  "*Register to be set up to hold `my-ediff-bwin-config'
    configuration.")

(defvar my-ediff-awin-config nil "Window configuration after ediff.")
(defcustom my-ediff-awin-reg ?e
  "*Register to be used to hold `my-ediff-awin-config' window
    configuration.")

(defun my-ediff-bsh ()
  "Function to be called before any buffers or window setup for
    ediff."
  (setq my-ediff-bwin-config (current-window-configuration))
  (when (characterp my-ediff-bwin-reg)
    (set-register my-ediff-bwin-reg
		  (list my-ediff-bwin-config (point-marker)))))

(defun my-ediff-ash ()
  "Function to be called after buffers and window setup for ediff."
  (setq my-ediff-awin-config (current-window-configuration))
  (when (characterp my-ediff-awin-reg)
    (set-register my-ediff-awin-reg
		  (list my-ediff-awin-config (point-marker)))))

(defun my-ediff-qh ()
  "Function to be called when ediff quits."
  (when my-ediff-bwin-config
    (set-window-configuration my-ediff-bwin-config)))

;; (add-hook 'ediff-load-hook
;; 	  (lambda ()
;; 	    (add-hook 'ediff-before-setup-hook
;; 		      (lambda ()
;; 			      (setq ediff-saved-window-configuration (current-window-configuration))))
;; 	    (let ((restore-window-configuration
;; 		          (lambda ()
;; 		            (set-window-configuration ediff-saved-window-configuration))))
;; 	      (add-hook 'ediff-quit-hook restore-window-configuration 'append)
;; 	      (add-hook 'ediff-suspend-hook restore-window-configuration 'append))))

;; (add-hook 'ediff-startup-hook
;; 	  (lambda ()
;; 	    (select-frame-by-name "Ediff")
;; 	    (set-frame-size(selected-frame) 40 10)))

;; (add-hook 'ediff-before-setup-hook 'my-ediff-bsh)
;; (add-hook 'ediff-after-setup-windows-hook 'my-ediff-ash 'append)
;; (add-hook 'ediff-quit-hook 'my-ediff-qh)

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

;; https://gist.github.com/magnars/2350388
;; Set mark to make it easy to jump back.
(defadvice ido-imenu (before push-mark activate)
  (push-mark))

(defvar my/save-buffers-kill-terminal-was-called nil)

(defun my/save-buffers-kill-terminal ()
  (interactive)
  (setq my/save-buffers-kill-terminal-was-called t)
  (save-buffers-kill-terminal t))

(defun my/kill-all-buffers-except-toolkit ()
  "Kill all buffers except current one and toolkit (*Messages*, *scratch*). Close other windows."
  (interactive)
  (mapc 'kill-buffer (remove-if
                       (lambda (x)
                         (or
                           (some (lambda (window) (string-equal (buffer-name x) (buffer-name (window-buffer window)))) (window-list))
                           ;; (string-equal (buffer-file-name) (buffer-file-name x))
                           (string-equal "*Messages*" (buffer-name x))
                           (string-equal "*scratch*" (buffer-name x))))
                       (buffer-list)))
  (delete-other-frames)
  (message "Closed other buffers and frames."))

(defun my/clone-indirect-buffer-new-window ()
  "Clone indirect buffer in new window."
  (interactive)
  (split-window-right)
  (clone-indirect-buffer nil t)
  (windmove-left))

; https://www.emacswiki.org/emacs/ToggleWindowSplit
(defun my/toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(defun my/evil-jump-to-tag-other-buffer ()
  (interactive)
  (save-excursion
    (evil-window-vsplit)
    (windmove-right)
    (evil-jump-to-tag)))

(bind-key "C-x |" 'my/toggle-window-split)
(bind-key "C-x C-c" 'my/save-buffers-kill-terminal)
(bind-key [remap move-beginning-of-line] 'my/smarter-move-beginning-of-line)
(bind-key "<s-right>" 'ns-next-frame)
(bind-key "<s-left>" 'ns-prev-frame)

(provide 'my-navigation)
