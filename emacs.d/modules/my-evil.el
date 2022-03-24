(require 'inc-dec-at-point)

(use-package evil
  :init
  (setq
    evil-want-fine-undo t
    evil-want-C-u-scroll t
    evil-want-C-i-jump nil
    evil-search-module 'evil-search
    evil-undo-system 'undo-fu)
  :config
  (setq-default
    evil-mode-line-format nil
    evil-shift-width tab-width
    evil-cross-lines t)

  (evil-mode 1)

  (bind-keys
    ("C-f" . universal-argument)
    :map universal-argument-map
    ("C-f" . universal-argument-more)
    ("C-u" . nil)
    :map evil-insert-state-map
    ("TAB" . tab-to-tab-stop)
    ("C-k" . kill-line)
    ("C-n" . company-next-page)
    ("C-p" . company-previous-page)
    :map evil-normal-state-map
    ("C-e" . move-end-of-line)
    ("C-a" . my/smarter-move-beginning-of-line)
    ("TAB" . indent-for-tab-command)
    ("C-x T" . my/move-current-window-to-new-frame)
    ("C-]" . xref-find-definitions)
    ("C-}" . xref-find-definitions-other-window)
    (",." . dired-jump)
    (",m" . my/dired-jump-make-new-window)
    (",n" . minimap-mode)
    (",f" . my/rgrep)
    (",w" . my/hydra-browser/body)
    (",x b" . my/kill-all-buffers-except-toolkit)
    (",x t" . delete-frame)
    (",c d" . my/copy-file-name)
    (",a" . 'artbollocks-mode)
    (",cd" . my/copy-file-name-to-clipboard)
    (",/" . evil-ex-nohighlight)
    ("h" . evil-first-non-blank)
    ("l" . evil-end-of-line)
    ("v" . set-mark-command)
    ("C-y" . yank)
    ("C-d" . evil-scroll-down)
    ("C-u" . evil-scroll-up)
    ("C-p" . evil-jump-forward)
    ("C-w T" . my/move-current-window-to-new-frame)
    ("]c" . git-gutter:next-hunk)
    ("[c" . git-gutter:previous-hunk)
    ("[l" . langtool-goto-previous-error)
    ("]l" . langtool-goto-next-error)
    ("]e" . my/next-error)
    ("[e" . my/previous-error)
    ("<"  . beginning-of-buffer)
    (">"  . end-of-buffer)
    ("<home>" . evil-first-non-blank)
    ("<end>" . evil-end-of-line)
    (",s" . my/flip-symbol)
    ("C-d" . evil-scroll-down)
    ("C-u" . evil-scroll-up)
    ("C-s" . evil-search-forward) ;; counsel-grep
    ("C-c C-S-o" . browse-url-generic)
    :map evil-visual-state-map
    ("C-e" . move-end-of-line)
    ("C-a" . my/smarter-move-beginning-of-line)
    ("TAB" . indent-for-tab-command)
    (",f" . my/rgrep)
    ("C-w" . evil-delete-char)
    ("h" . evil-first-non-blank)
    ("l" . evil-end-of-line)
    ("<home>" . evil-first-non-blank)
    ("<end>" . evil-end-of-line)
    (",/" . keyboard-quit)
    ("{" . backward-paragraph)
    ("}" . forward-paragraph)
    :map evil-motion-state-map
    ("{" . backward-paragraph)
    ("C-d" . evil-scroll-down)
    ("}" . forward-paragraph)
    ("C-u" . evil-scroll-up)
    ("C-w T" . my/move-current-window-to-new-frame)
    ("<down>" . evil-next-visual-line)
    ("<up>" . evil-previous-visual-line)
    )

  (defun my/next-error ()
    (interactive)
    (cond ((get-buffer "*Flycheck errors*") (next-error))
      ((and (bound-and-true-p flycheck-mode) (flycheck-has-current-errors-p)) (flycheck-next-error))
      (t (next-error))))

  (defun my/previous-error ()
    (interactive)
    (cond ((get-buffer "*Flycheck errors*") (previous-error))
      ((and (bound-and-true-p flycheck-mode) (flycheck-has-current-errors-p)) (flycheck-previous-error))
      (t (previous-error))))

  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  (add-hook 'evil-insert-state-entry-hook (lambda ()
                                            (when current-input-method
                                              (deactivate-input-method))))
  ;; (bind-key "<M-right>" 'right-word                         evil-normal-state-map)
  ;; (bind-key "<M-left>" 'left-word                           evil-normal-state-map)

  ;; (bind-key ",\\"    'skk-mode                            evil-normal-state-map)

  (unbind-key "M-." evil-normal-state-map)
  (unbind-key "\\" evil-motion-state-map)
  (unbind-key "K" evil-motion-state-map)
  (unbind-key "RET" evil-motion-state-map)
  (bind-key "RET" (lambda () (interactive) (evil-ret 0) evil-motion-state-map))

  ;; (define-key global-map (kbd "C-u") 'kill-whole-line)

  (eval-after-load 'evil-maps
    '(progn
       (bind-key "C-f" nil evil-motion-state-map)
       (bind-key "C-u" 'evil-scroll-up evil-motion-state-map)))

  ;; (define-key evil-normal-state-map "c" nil)
  ;; (define-key evil-motion-state-map "cu" 'universal-argument)

  (advice-add 'eval-region :after (lambda (&rest r)
                                    (deactivate-mark)
                                    (when (fboundp 'evil-exit-visual-state)
                                      (evil-exit-visual-state))))

  ;; (add-to-list 'evil-overriding-maps 'ediff-mode-map)

  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'profiler-report-mode 'emacs)
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'finder-mode 'emacs)
  (evil-set-initial-state 'woman-mode 'emacs)
  (evil-set-initial-state 'geben-context-mode 'emacs)
  (evil-set-initial-state 'geben-backtrace-mode 'emacs)
  (evil-set-initial-state 'geben-backtrace-mode 'emacs)
  (evil-set-initial-state 'geben-breakpoint-list-mode 'emacs)
  (evil-set-initial-state 'epa-key-list-mode 'emacs)
  (evil-set-initial-state 'image-mode 'emacs)
  (evil-set-initial-state 'shell-mode 'emacs)
  (evil-set-initial-state 'treemacs-mode 'emacs)
  (evil-set-initial-state 'lsp-ui-imenu-mode 'emacs)
  (evil-set-initial-state 'w3m-form-input-select-mode 'emacs)
  (evil-set-initial-state 'xwidget-webkit-mode 'emacs)
  (evil-set-initial-state 'erc-mode 'emacs)
  (evil-set-initial-state 'dashboard-mode 'emacs)

  (add-to-list 'evil-emacs-state-modes 'shell-mode)
  (add-to-list 'evil-emacs-state-modes 'eshell-mode)
  (add-to-list 'evil-emacs-state-modes 'org-agenda-mode)
  (add-to-list 'evil-emacs-state-modes 'org-toc-mode)
  (add-to-list 'evil-emacs-state-modes 'eww-mode)
  (add-to-list 'evil-emacs-state-modes 'magit-mode)
  (add-to-list 'evil-emacs-state-modes 'magit-log-mode)
  (add-to-list 'evil-emacs-state-modes 'magit-process-mode)
  (add-to-list 'evil-emacs-state-modes 'git-rebase-mode)
  (add-to-list 'evil-emacs-state-modes 'mu4e-main-mode)
  (add-to-list 'evil-emacs-state-modes 'mu4e-org-mode)
  ;; (add-to-list 'evil-emacs-state-modes 'woman-mode)
  ;; (add-to-list 'evil-emacs-state-modes 'help-mode)
  (add-to-list 'evil-emacs-state-modes 'debugger-mode)
  ;; (add-to-list 'evil-emacs-state-modes 'ediff-mode)
  (add-to-list 'evil-emacs-state-modes 'messages-buffer-mode)
  (add-to-list 'evil-emacs-state-modes 'epa-key-mode)
  (add-to-list 'evil-emacs-state-modes 'inferior-python-mode)
  (add-to-list 'evil-emacs-state-modes 'xref--xref-buffer-mode)
  (add-to-list 'evil-emacs-state-modes 'erc-mode)
  (add-to-list 'evil-emacs-state-modes 'custom-mode)
  (add-to-list 'evil-emacs-state-modes 'elfeed-search-mode)
  (add-to-list 'evil-emacs-state-modes 'elfeed-show-mode)
  (add-to-list 'evil-emacs-state-modes 'jarfar/org-roam-side-mode)
  (add-to-list 'evil-emacs-state-modes 'deft-mode)
  (add-to-list 'evil-emacs-state-modes 'ledger-report-mode)
  (add-to-list 'evil-emacs-state-modes 'treemacs-mode)
  (add-to-list 'evil-emacs-state-modes 'flycheck-error-list-mode)
  (add-to-list 'evil-emacs-state-modes 'nov-mode)
  (add-to-list 'evil-emacs-state-modes 'helpful-mode)
  (add-to-list 'evil-emacs-state-modes 'process-menu-mode)

  (evil-declare-change-repeat 'company-complete)

  (with-eval-after-load 'edebug
    (evil-make-overriding-map edebug-mode-map '(normal motion))
    (add-hook 'edebug-mode-hook 'evil-normalize-keymaps))

  (dolist (element my/text-modes)
    (evil-define-key '(motion normal) element
      (kbd "<down>") 'evil-next-visual-line
      (kbd "<up>")   'evil-previous-visual-line))

  (evil-define-key 'normal prog-mode-map
    (kbd "C-c m") #'hydra-merge/body)

  (evil-define-key 'normal ledger-mode-map
    (kbd "C-c L") #'hydra-ledger/body)

  (evil-define-key 'normal org-mode-map
    (kbd "C-x ,") #'hydra-org/body
    (kbd "C-x C-,") #'hydra-org/body
    (kbd "<tab>") #'org-cycle
    (kbd "TAB") #'org-cycle)
    ;; (kbd "C-c s") #'hydra-spelling/body)

  ;; (evil-define-key '(visual normal) org-mode-map
  ;;   ",t" #'my/google-translate-at-point)

  (evil-define-key '(motion normal) org-mode-map
    (kbd "C-c C-s") 'org-schedule)

  ;; (evil-define-key '(motion normal) help-mode-map
  ;;   "l" 'help-go-back
  ;;   "r" 'help-go-forward
  ;;   "s-TAB" 'backward-button
  ;;   "TAB" 'forward-button)

  ;; (evil-define-key 'normal ediff-mode-map
  ;;   "[c" 'ediff-next-difference
  ;;   "]c" 'ediff-previous-difference)

  ;; (evil-define-key 'normal tide-mode-map
  ;;   ",t" 'hydra-tide/body
  ;;   "M-." 'tide-jump-to-implementation
  ;;   "M-," 'tide-jump-back)

  ;; https://www.emacswiki.org/emacs/Evil#toc12
  ;; Brings back the access to RET and SPC in some modes.
  ;; (defun my/move-key (keymap-from keymap-to key)
  ;;   "Moves key binding from one keymap to another, deleting from the old location. "
  ;;   (define-key keymap-to key (lookup-key keymap-from key))
  ;;   (define-key keymap-from key nil))
  ;; (my/move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
  ;; (my/move-key evil-motion-state-map evil-normal-state-map " ")

  (defalias 'forward-evil-word 'forward-evil-symbol))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package evil-visualstar
  :after evil
  :config
  (global-evil-visualstar-mode))

(use-package evil-anzu
  :after evil)

(use-package evil-multiedit
  :after evil
  :bind (:map evil-normal-state-map
          ("M-d" . evil-multiedit-match-and-next)
          ("M-D" . 'evil-multiedit-match-and-prev)
          ("C-M-D" . 'evil-multiedit-restore)
          :map evil-visual-state-map
          ("R" . evil-multiedit-match-all)
          ("M-d" . evil-multiedit-match-and-next)
          ("M-D" . 'evil-multiedit-match-and-prev)
          ("C-M-D" . 'evil-multiedit-restore)
          :map evil-multiedit-state-map
          ("RET" . 'evil-multiedit-toggle-or-restrict-region)
          ("C-n" . 'evil-multiedit-next)
          ("C-p" . 'evil-multiedit-prev)
          :map evil-multiedit-insert-state-map
          ("C-n" . 'evil-multiedit-next)
          ("C-p" . 'evil-multiedit-prev))
  :config
  (evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match))

(use-package undo-fu
  :after evil
  :config
  (bind-keys
    :map evil-normal-state-map
    ("u" . undo-fu-only-undo)
    ("C-r" . undo-fu-only-redo)))

(provide 'my-evil)
