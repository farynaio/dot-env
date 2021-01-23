;; (require 'help)

(use-package evil
  :init
  (setq evil-want-fine-undo t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-search-module 'evil-search)
  (setq evil-undo-system 'undo-fu)
  :config
  (setq evil-mode-line-format nil)

  (evil-mode 1)
  (bind-key "C-d"    'evil-scroll-down)
  (bind-key "C-u"    'evil-scroll-up)
  (bind-key "C-e"    'move-end-of-line                    evil-normal-state-map)
  (bind-key "C-e"    'move-end-of-line                    evil-visual-state-map)
  (bind-key "C-a"    'my/smarter-move-beginning-of-line   evil-normal-state-map)
  (bind-key "C-a"    'my/smarter-move-beginning-of-line   evil-visual-state-map)
  (bind-key "TAB"    'indent-for-tab-command              evil-normal-state-map)
  (bind-key "TAB"    'indent-for-tab-command              evil-visual-state-map)
  (bind-key "TAB"    'tab-to-tab-stop                     evil-insert-state-map)
  (bind-key "C-x T"  'my/move-current-window-to-new-frame evil-normal-state-map)
  (bind-key "M-]"    'my/evil-jump-to-tag-other-buffer    evil-normal-state-map)
  (bind-key ", ."    'dired-jump                          evil-normal-state-map)
  (bind-key ", m"    'my/dired-jump-make-new-window       evil-normal-state-map)
  (bind-key ", n"    'minimap-mode                        evil-normal-state-map)
  (bind-key ", g"    'hydra-git/body                      evil-normal-state-map)
  (bind-key ", b"    'hydra-buffer/body                   evil-normal-state-map)
  ;; (bind-key ", l"    'treemacs-display-current-project-exclusively evil-normal-state-map)
  (bind-key ", w"    'hydra-writting/body                 evil-normal-state-map)
  (bind-key ", i"    'hydra-snippet/body                  evil-normal-state-map)
  (bind-key ", p"    'hydra-project/body               evil-normal-state-map)
  (bind-key ", j"    'hydra-japanese/body                 evil-visual-state-map)
  (bind-key ", f"    'my/rgrep                            evil-normal-state-map)
  (bind-key ", f"    'my/rgrep                            evil-visual-state-map)
  (bind-key ", x b"  'my/kill-all-buffers-except-toolkit  evil-normal-state-map)
  (bind-key ", x t"  'delete-frame                        evil-normal-state-map)
  (bind-key ", c d"  'my/copy-file-name                   evil-normal-state-map)
  ;; (bind-key ", d"   'my/lang-toggle                      evil-normal-state-map)
  (bind-key ", a"    'artbollocks-mode                    evil-normal-state-map)
  (bind-key ", cd"   'my/copy-file-name-to-clipboard      evil-normal-state-map)
  (bind-key ", /"    'evil-ex-nohighlight                 evil-normal-state-map)
  (bind-key "C-w"    'evil-delete-char                    evil-visual-state-map)
  (bind-key "h"      'evil-first-non-blank                evil-normal-state-map)
  (bind-key "h"      'evil-first-non-blank                evil-visual-state-map)
  (bind-key "l"      'evil-end-of-line                    evil-normal-state-map)
  (bind-key "l"      'evil-end-of-line                    evil-visual-state-map)
  (bind-key "v"      'set-mark-command                    evil-normal-state-map)
  (bind-key "}"      'forward-paragraph                   evil-motion-state-map)
  (bind-key "{"      'backward-paragraph                  evil-motion-state-map)
  (bind-key "C-k"    'kill-line                           evil-insert-state-map)
  (bind-key "C-y"    'yank                                evil-normal-state-map)
  (bind-key "C-d"    'evil-scroll-down                    evil-motion-state-map)
  (bind-key "C-d"    'evil-scroll-down                    evil-normal-state-map)
  (bind-key "C-u"    'evil-scroll-up                      evil-motion-state-map)
  (bind-key "C-u"    'evil-scroll-up                      evil-normal-state-map)
  (bind-key "C-w T"  'my/move-current-window-to-new-frame evil-normal-state-map)
  (bind-key "C-w T"  'my/move-current-window-to-new-frame evil-motion-state-map)
  (bind-key "<down>" 'evil-next-visual-line               evil-motion-state-map)
  (bind-key "<up>"   'evil-previous-visual-line           evil-motion-state-map)
  (bind-key "]c"     'git-gutter:next-hunk                evil-normal-state-map)
  (bind-key "[c"     'git-gutter:previous-hunk            evil-normal-state-map)
  (bind-key "[l"     'langtool-goto-previous-error        evil-normal-state-map)
  (bind-key "]l"     'langtool-goto-next-error            evil-normal-state-map)
  (bind-key "C-n"    'company-next-page                   evil-insert-state-map)
  (bind-key "C-p"    'company-previous-page               evil-insert-state-map)
  (bind-key "]e"     'my/next-error                       evil-normal-state-map)
  (bind-key "[e"     'my/previous-error                   evil-normal-state-map)
  (bind-key "<"      'beginning-of-buffer                 evil-normal-state-map)
  (bind-key ">"      'end-of-buffer                       evil-normal-state-map)
  (bind-key "<home>" 'evil-first-non-blank                evil-normal-state-map)
  (bind-key "<home>" 'evil-first-non-blank                evil-visual-state-map)
  (bind-key "<end>"  'evil-end-of-line                    evil-normal-state-map)
  (bind-key "<end>"  'evil-end-of-line                    evil-visual-state-map)
  (bind-key ", /"    'keyboard-quit                       evil-visual-state-map)
  (bind-key ", s"    'my/flip-symbol                      evil-normal-state-map)
  (bind-key "C-d"    'evil-scroll-down evil-normal-state-map)
  (bind-key "C-u"    'evil-scroll-up evil-normal-state-map)
  (bind-key "C-s"    'evil-search-forward evil-normal-state-map) ;; counsel-grep

  (defun my/next-error ()
    (interactive)
    (if (and (bound-and-true-p flycheck-mode) (flycheck-has-current-errors-p))
      (flycheck-next-error)
      (next-error)))

  (defun my/previous-error ()
    (interactive)
    (if (and (bound-and-true-p flycheck-mode) (flycheck-has-current-errors-p))
      (flycheck-previous-error)
      (previous-error)))

  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  (add-hook 'evil-insert-state-entry-hook (lambda ()
                                            (when current-input-method
                                              (deactivate-input-method))))
  ;; (bind-key "<M-right>" 'right-word                         evil-normal-state-map)
  ;; (bind-key "<M-left>" 'left-word                           evil-normal-state-map)

  ;; (bind-key ",\\"    'skk-mode                            evil-normal-state-map)

  (unbind-key "M-." evil-normal-state-map)
  (unbind-key "\\" evil-motion-state-map)

  (bind-key "C-f" 'universal-argument)
  (bind-key "C-u" nil universal-argument-map)
  (bind-key "C-f" 'universal-argument-more universal-argument-map)
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

  (add-to-list 'evil-emacs-state-modes 'shell-mode)
  (add-to-list 'evil-emacs-state-modes 'eshell-mode)
  (add-to-list 'evil-emacs-state-modes 'org-agenda-mode)
  (add-to-list 'evil-emacs-state-modes 'org-toc-mode)
  (add-to-list 'evil-emacs-state-modes 'eww-mode)
  (add-to-list 'evil-emacs-state-modes 'magit-mode)
  (add-to-list 'evil-emacs-state-modes 'magit-log-mode)
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

  ;; (setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
  ;; (setq evil-emacs-state-modes nil)

  (evil-declare-change-repeat 'company-complete)

  (with-eval-after-load 'edebug
    (evil-make-overriding-map edebug-mode-map '(normal motion))
    (add-hook 'edebug-mode-hook 'evil-normalize-keymaps))

  (evil-define-key '(motion normal) org-mode-map
    (kbd "C-c C-s") 'org-schedule)

  (dolist (element my/text-modes)
    (evil-define-key '(motion normal) element
      (kbd "<down>") 'evil-next-visual-line
      (kbd "<up>")   'evil-previous-visual-line))

  (bind-key "C-c w t"
    (lambda ()
      (interactive)
      "Move current window to new frame."
      (let ((buffer (current-buffer)))
        (unless (one-window-p)
          (delete-window))
        (display-buffer-pop-up-frame buffer nil)))
    evil-normal-state-map)

  (evil-define-key '(motion emacs normal) mu4e:view-mode-map
    "C-d" 'evil-scroll-down
    "C-u" 'evil-scroll-up)

  (evil-define-key '(motion emacs) mu4e-headers-mode-map
    "C-d" 'evil-scroll-down
    "C-u" 'evil-scroll-up)

  (evil-define-key '(visual) mu4e-compose-mode-map
    "H" 'org-mime-htmlize)

  (evil-define-key 'normal flyspell-mode-map
    ;; (kbd "[s") 'flyspell-goto-next-error
    ;; (kbd "]s") 'flyspell-goto-next-error
    (kbd "[l") 'langtool-goto-previous-error
    (kbd "]l") 'langtool-goto-next-error)

  (evil-define-key 'normal org-mode-map
    (kbd "<tab>") 'org-cycle
    (kbd "TAB")   'org-cycle)

  (evil-define-key '(visual normal) org-mode-map
    ",t"  'my/google-translate-at-point)

  ;; (evil-define-key '(motion normal) help-mode-map
  ;;   "l" 'help-go-back
  ;;   "r" 'help-go-forward
  ;;   "s-TAB" 'backward-button
  ;;   "TAB" 'forward-button)

  ;; (evil-define-key 'normal ediff-mode-map
  ;;   "[c" 'ediff-next-difference
  ;;   "]c" 'ediff-previous-difference)

  (evil-define-key '(normal motion visual) elpy-mode-map
    "M-." 'xref-find-definitions
    "M-," 'xref-pop-marker-stack)

  (evil-define-key '(normal motion visual) js2-mode-map
    "M-." 'xref-find-definitions
    "M-," 'xref-pop-marker-stack)

  (evil-define-key 'normal web-mode-map
    ",t" 'my/toggle-php-flavor-mode)

  (evil-define-key 'normal php-mode-map
    ",t" 'my/toggle-php-flavor-mode)

  (evil-define-key 'normal prog-mode-map
    ",e" 'my/flycheck-toggle)

  (evil-define-key 'normal tide-mode-map
    ",t" 'hydra-tide/body
    "M-." 'tide-jump-to-implementation
    "M-," 'tide-jump-back)

  (evil-define-key 'normal php-mode-map
    ",d" 'hydra-php-debug/body)

  (evil-define-key 'normal jarfar/org-roam-mode-map
    ",," 'deft)

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

(provide 'my-evil)
