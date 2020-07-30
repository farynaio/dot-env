(require 'tramp)
(require 'recentf)
(require 'ediff)
(require 'help)

(eval-after-load 'recentf
  '(progn
    (recentf-mode 1)))

(setq recentf-max-saved-items 200
  recentf-max-menu-items 15)

(eval-after-load 'help
  '(progn
     (bind-key "/"          #'evil-search-forward       help-mode-map)
     (bind-key "v"          #'set-mark-command          help-mode-map)
     (bind-key "C-w ="      #'balance-windows           help-mode-map)
     (bind-key "n"          #'evil-search-next          help-mode-map)
     (bind-key "N"          #'evil-search-previous      help-mode-map)
     (bind-key "w"          #'evil-forward-word-begin   help-mode-map)
     (bind-key "e"          #'evil-forward-word-end     help-mode-map)
     (bind-key "E"          #'evil-forward-WORD-end     help-mode-map)
     (bind-key "b"          #'evil-backward-word-begin  help-mode-map)
     (bind-key "B"          #'evil-backward-WORD-begin  help-mode-map)
     (bind-key "y"          #'evil-yank                 help-mode-map)
     (bind-key "gg"         #'evil-goto-first-line      help-mode-map)
     (bind-key "G"          #'evil-goto-line            help-mode-map)
     ))

(eval-after-load 'tramp
  '(progn
     (setq
       tramp-default-method "ssh"
       tramp-inline-compress-start-size 40960
       tramp-chunksize 500
       tramp-auto-save-directory "~/.emacs.d/tramp-autosaves/"
       tramp-persistency-file-name  "~/.emacs.d/tramp-persistency.el"
       tramp-encoding-shell "/bin/ksh"
       )
     ))

(setq password-cache-expiry nil)

(use-package anzu
  :diminish (anzu-mode . "")
  :config
  (progn
    (global-anzu-mode 1)
    (bind-key "M-%" #'anzu-query-replace-regexp)))

(use-package drag-stuff
  :diminish (drag-stuff-mode . "")
  :config
  (progn
    (add-to-list 'drag-stuff-except-modes 'org-mode)
    ;; (add-to-list 'drag-stuff-except-modes 'my/org-taskjuggler-mode)

    (drag-stuff-global-mode 1)
    (define-key drag-stuff-mode-map (drag-stuff--kbd 'up) 'drag-stuff-up)
    (define-key drag-stuff-mode-map (drag-stuff--kbd 'down) 'drag-stuff-down)))

(use-package goto-last-change)

(use-package smartscan
  :defer t
  :config
  (global-smartscan-mode 1))

(bind-key "C-c p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

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

(use-package neotree)

(setq default-directory "~/.emacs.d")

;; (use-package ace-jump-mode
;;   :config
;;   (progn
;;     (when (fboundp 'evil-mode)
;;       (bind-key "\\w" #'ace-jump-word-mode  evil-motion-state-map)
;;       (bind-key "\\c" #'ace-jump-mode       evil-motion-state-map)
;;       (bind-key "\\a" #'ivy-imenu-anywhere  evil-motion-state-map)
;;       )))

;; (setq display-buffer-alist
;;   '(
;;      ("^.+\\.org\\(\\.gpg\\)?$"
;;        (display-buffer-reuse-window) . ((reusable-frames . t)))
;;      ("^\\(\\..+\\)\\|\\(.+\\..+\\)$"
;;        (display-buffer-reuse-window display-buffer-same-window display-buffer-reuse-window display-buffer-pop-up-frame) . ((reusable-frames . t)))))

(use-package avy
  :config
  (bind-key "\\c" #'avy-goto-char help-mode-map)
  (bind-key "\\w" #'avy-goto-word-or-subword-1 help-mode-map)

  (when (fboundp 'evil-mode)
    (bind-key "\\w" #'avy-goto-word-or-subword-1  evil-motion-state-map)
    (bind-key "\\c" #'avy-goto-char               evil-motion-state-map)))

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
    (setq ivy-re-builders-alist '((t   . ivy--regex-ignore-order)))
    ))

(use-package counsel
  :config
  (progn
    (setq
      counsel-find-file-ignore-regexp "\\`\\."
      counsel-grep-base-command "grep -E -n -i -e %s %s")
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
    (bind-key "C-h a" #'counsel-apropos)
    (bind-key "C-x r b" #'counsel-bookmark)
    (bind-key "C-x b" #'counsel-ibuffer)
    (bind-key "C-x C-b" #'my/counsel-ibuffer-other-window)

    (when (fboundp 'evil-mode)
      (bind-key "\\a" #'counsel-imenu  evil-motion-state-map))

    (defun my/counsel-ibuffer-other-window (&optional name)
      "Use ibuffer to switch to another buffer.
NAME specifies the name of the buffer (defaults to \"*Ibuffer*\")."
      (interactive)
      (setq counsel-ibuffer--buffer-name (or name "*Ibuffer*"))
      (ivy-read "Switch to buffer: " (counsel-ibuffer--get-buffers)
        :history 'counsel-ibuffer-history
        :action #'counsel-ibuffer-visit-buffer-other-window
        :caller 'counsel-ibuffer))

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

(use-package swiper)

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

(advice-add #'counsel-grep :around #'my/counsel-grep-fallback)

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
    (bind-key "C-c p" #'projectile-command-map projectile-mode-map)
    ;; (setq projectile-generic-command "fd . -0")
    (setq projectile-tags-command "ctags -R -e .")
    (setq projectile-track-known-projects-automatically nil)
    (setq projectile-globally-ignored-file-suffixes '(".png" ".gif" ".pdf"  "*.class"))

    (projectile-mode 1)

    (add-to-list 'projectile-globally-ignored-directories '"node-modules")
    (add-to-list 'projectile-globally-ignored-directories '"dist")
    (add-to-list 'projectile-globally-ignored-directories '"target")
    (add-hook 'projectile-after-switch-project-hook (lambda () (my/projectile-invalidate-cache nil)))))

(defun my/projectile-add-known-project (project-root)
  ""
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

(use-package undo-tree
  :diminish (undo-tree-mode . "")
  :config
  (progn
    (setq undo-tree-visualizer-diff t)
    (evil-make-overriding-map undo-tree-visualizer-mode-map 'motion)
    (evil-make-overriding-map undo-tree-visualizer-selection-mode-map 'motion)
    (evil-make-overriding-map undo-tree-map 'motion)
    ))

(use-package ag
  :config
  (progn
    (setq ag-reuse-buffers 't)))

(setq
  ediff-window-setup-function 'ediff-setup-windows-plain
  ediff-forward-word-function 'forward-char)

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

(global-set-key (kbd "C-x |") #'my/toggle-window-split)
(global-set-key [remap move-beginning-of-line] 'my/smarter-move-beginning-of-line)

(bind-key "C-x C-c"       #'my/save-buffers-kill-terminal)

(provide 'my-navigation)
