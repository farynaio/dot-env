
;;; Code:

(use-package evil
  :demand t
  :bind
  (("C-f" . universal-argument)
    :map universal-argument-map
    ("C-f" . universal-argument-more)
    ("C-u" . nil)
    :map evil-insert-state-map
    ("TAB" . tab-to-tab-stop)
    ("C-k" . kill-line)
    :map evil-normal-state-map
    ("DEL" . ignore) ;; Unbind 'evil-backward-char'
    ("C-e" . move-end-of-line)
    ("TAB" . indent-for-tab-command)
    ("C-]" . xref-find-definitions)
    ("C-}" . xref-find-definitions-other-window)
    (",." . dired-jump)
    (",g" . hydra-git/body)
    (",x b" . my/kill-all-buffers-except-toolkit)
    (",x t" . delete-frame)
    (",/" . evil-ex-nohighlight)
    ("h" . evil-first-non-blank)
    ("l" . evil-end-of-line)
    ("v" . set-mark-command)
    ("C-y" . yank)
    ("C-d" . evil-scroll-down)
    ("C-u" . evil-scroll-up)
    ("C-p" . evil-jump-forward)
    ("]c" . diff-hl-next-hunk)
    ("[c" . diff-hl-previous-hunk)
    ("[l" . langtool-goto-previous-error)
    ("]l" . langtool-goto-next-error)
    ("]e" . my/next-error)
    ("[e" . my/previous-error)
    ("<"  . beginning-of-buffer)
    (">"  . end-of-buffer)
    ("<home>" . evil-first-non-blank)
    ("<end>" . evil-end-of-line)
    (",s" . my/flip-symbol)
    ("*" . evil-search-word-forward)
    ("#" . evil-search-word-backward)
    ("N" . evil-search-previous)
    ("n" . evil-search-next)
    ("/" . evil-search-forward)
    ("C-d" . evil-scroll-down)
    ("C-u" . evil-scroll-up)
    ("C-s" . counsel-grep)
    ("C-c C-S-o" . browse-url-generic)
    ;; (",w" . hydra-browser/body)
    ;; (",p" . hydra-project/body)
    ;; ("C-c o" . hydra-org/body)
    ("C-c v" . hydra-org-roam/body)
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
    ("<up>" . evil-previous-visual-line))
  :custom
  (evil-want-fine-undo t)
  (evil-want-C-u-scroll t)
  (evil-want-C-i-jump nil)
  (evil-search-module 'evil-search)
  (evil-undo-system 'undo-fu)
  (evil-mode-line-format nil)
  (evil-shift-width tab-width)
  (evil-cross-lines t)
  :config
  (evil-mode 1)

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

  (unbind-key "C-w" global-map)
  (unbind-key "C-w q" global-map)
  (unbind-key "C-w C-q" global-map)
  (unbind-key "C-x C-z" global-map)
  (unbind-key "C-z" global-map)
  (unbind-key "C-/" global-map)
  (unbind-key "C-_" global-map) ;; undo
  (unbind-key "C-x u" global-map)
  (unbind-key "C-x m" global-map)
  (unbind-key "C-\"" global-map) ;; 'toggle-input-method'

  (bind-key "C-w q" 'delete-window)
  (bind-key "C-w C-q" 'delete-window)

  (unbind-key "=" evil-normal-state-map)
  (unbind-key "+" evil-normal-state-map)
  (unbind-key "+" evil-motion-state-map)
  (unbind-key "M-." evil-normal-state-map)
  (unbind-key "\\" evil-motion-state-map)
  (unbind-key "K" evil-motion-state-map)

  (unbind-key "SPC" evil-motion-state-map)
  (unbind-key "C-z" evil-motion-state-map)
  (unbind-key "C-z" evil-normal-state-map)
  (unbind-key "C-z" evil-insert-state-map)

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
  ;; (evil-set-initial-state 'lsp-ui-imenu-mode 'emacs)
  (evil-set-initial-state 'w3m-form-input-select-mode 'emacs)
  (evil-set-initial-state 'xwidget-webkit-mode 'emacs)
  (evil-set-initial-state 'erc-mode 'emacs)
  (evil-set-initial-state 'dashboard-mode 'emacs)
  (evil-set-initial-state 'read-only-mode 'emacs)
  (evil-set-initial-state 'Info-mode 'emacs)
  (evil-set-initial-state 'grep-mode 'emacs)
  (evil-set-initial-state 'org-roam-mode 'emacs)

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
  (add-to-list 'evil-emacs-state-modes 'debugger-mode)
  (add-to-list 'evil-emacs-state-modes 'messages-buffer-mode)
  (add-to-list 'evil-emacs-state-modes 'epa-key-mode)
  (add-to-list 'evil-emacs-state-modes 'inferior-python-mode)
  (add-to-list 'evil-emacs-state-modes 'xref--xref-buffer-mode)
  (add-to-list 'evil-emacs-state-modes 'erc-mode)
  (add-to-list 'evil-emacs-state-modes 'custom-mode)
  (add-to-list 'evil-emacs-state-modes 'elfeed-search-mode)
  (add-to-list 'evil-emacs-state-modes 'elfeed-show-mode)
  (add-to-list 'evil-emacs-state-modes 'my/org-roam-side-mode)
  (add-to-list 'evil-emacs-state-modes 'deft-mode)
  (add-to-list 'evil-emacs-state-modes 'ledger-report-mode)
  (add-to-list 'evil-emacs-state-modes 'treemacs-mode)
  (add-to-list 'evil-emacs-state-modes 'flycheck-error-list-mode)
  (add-to-list 'evil-emacs-state-modes 'nov-mode)
  (add-to-list 'evil-emacs-state-modes 'helpful-mode)
  (add-to-list 'evil-emacs-state-modes 'process-menu-mode)
  (add-to-list 'evil-emacs-state-modes 'Info-mode)
  (add-to-list 'evil-emacs-state-modes 'fireplace-mode)
  (add-to-list 'evil-emacs-state-modes 'chatgpt-shell-mode)
  (add-to-list 'evil-emacs-state-modes 'jump-tree-visualizer-mode)
  (add-to-list 'evil-emacs-state-modes 'edebug-x-breakpoint-list-mode)
  (add-to-list 'evil-emacs-state-modes 'edebug-x-instrumented-function-list-mode)

  ;; (evil-define-key '(motion normal) help-mode-map
  ;;   "l" 'help-go-back
  ;;   "r" 'help-go-forward
  ;;   "s-TAB" 'backward-button
  ;;   "TAB" 'forward-button)

  ;; https://www.emacswiki.org/emacs/Evil#toc12
  ;; Brings back the access to RET and SPC in some modes.
  ;; (defun my/move-key (keymap-from keymap-to key)
  ;;   "Moves key binding from one keymap to another, deleting from the old location. "
  ;;   (define-key keymap-to key (lookup-key keymap-from key))
  ;;   (define-key keymap-from key nil))
  ;; (my/move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
  ;; (my/move-key evil-motion-state-map evil-normal-state-map " ")

  (defalias 'forward-evil-word 'forward-evil-symbol)

  (defun my/evil-switch-to-normal-state-if-insert (&optional arg)
    "Switched to evil normal state from insert"
    (when (and (boundp 'evil-state) (string-equal evil-state "insert"))
      (evil-normal-state))))

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
  :config
  (evil-multiedit-default-keybinds)
  (defun make-evil-multiedit-case-sensitive (fn &rest args)
    (let ((case-fold-search (not iedit-case-sensitive)))
      (apply fn args)))
  (advice-add 'evil-multiedit-match-and-next :around 'make-evil-multiedit-case-sensitive))

(use-package evil-numbers
  :after evil
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt))

(provide 'my-evil)
;;; my-evil.el ends here
