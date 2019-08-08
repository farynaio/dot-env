(require 'help)

(use-package evil
  :init
  (progn
    (setq
      evil-want-fine-undo t
      evil-want-C-u-scroll t
      evil-want-C-i-jump nil
      evil-search-module 'evil-search
      ))
  :config
  (progn
    (evil-mode 1)
    (bind-key "C-d"    #'evil-scroll-down)
    (bind-key "C-u"    #'evil-scroll-up)
    (bind-key "C-e"    #'move-end-of-line                    evil-normal-state-map)
    (bind-key "C-e"    #'move-end-of-line                    evil-visual-state-map)
    (bind-key "C-a"    #'my/smarter-move-beginning-of-line   evil-normal-state-map)
    (bind-key "C-a"    #'my/smarter-move-beginning-of-line   evil-visual-state-map)
    (bind-key "TAB"    #'indent-for-tab-command              evil-normal-state-map)
    (bind-key "TAB"    #'indent-for-tab-command              evil-visual-state-map)
    (bind-key "TAB"    #'tab-to-tab-stop                     evil-insert-state-map)
    (bind-key "C-x T"  #'my/move-current-window-to-new-frame evil-normal-state-map)
    (bind-key "M-]"    #'my/evil-jump-to-tag-other-buffer    evil-normal-state-map)
    (bind-key ", ."    #'dired-jump                          evil-normal-state-map)
    (bind-key ", m"    #'my/dired-jump-make-new-window       evil-normal-state-map)
    (bind-key ", n"    #'minimap-mode                        evil-normal-state-map)
    (bind-key ", g"    #'hydra-git/body                      evil-normal-state-map)
    (bind-key ", b"    #'hydra-buffer/body                   evil-normal-state-map)
    (bind-key ", l"    #'sr-speedbar-toggle                  evil-normal-state-map)
    (bind-key ", w"    #'hydra-writting/body                 evil-normal-state-map)
    (bind-key ", i"    #'hydra-snippet/body                  evil-normal-state-map)
    (bind-key ", p"    #'hydra-projectile/body               evil-normal-state-map)
    (bind-key ", j"    #'hydra-japanese/body                 evil-visual-state-map)
    (bind-key ", f"    #'my/rgrep                            evil-normal-state-map)
    (bind-key ", f"    #'my/rgrep                            evil-visual-state-map)
    (bind-key ", x b"  #'my/kill-all-buffers-except-toolkit  evil-normal-state-map)
    (bind-key ", x t"  #'delete-frame                        evil-normal-state-map)
    (bind-key ", c d"  #'my/copy-file-name                   evil-normal-state-map)
    ;; (bind-key ", d"    #'my/lang-toggle                      evil-normal-state-map)
    (bind-key ", a"    #'artbollocks-mode                    evil-normal-state-map)
    (bind-key ", cd"   #'my/copy-file-name-to-clipboard      evil-normal-state-map)
    (bind-key ", /"    #'evil-ex-nohighlight                 evil-normal-state-map)
    (bind-key "C-w"    #'evil-delete-char                    evil-visual-state-map)
    (bind-key "h"      #'evil-first-non-blank                evil-normal-state-map)
    (bind-key "h"      #'evil-first-non-blank                evil-visual-state-map)
    (bind-key "l"      #'evil-end-of-line                    evil-normal-state-map)
    (bind-key "l"      #'evil-end-of-line                    evil-visual-state-map)
    (bind-key "v"      #'set-mark-command                    evil-normal-state-map)
    (bind-key "}"      #'forward-paragraph                   evil-motion-state-map)
    (bind-key "{"      #'backward-paragraph                  evil-motion-state-map)
    (bind-key "C-k"    #'kill-line                           evil-insert-state-map)
    (bind-key "C-y"    #'yank                                evil-normal-state-map)
    (bind-key "C-d"    #'evil-scroll-down                    evil-motion-state-map)
    (bind-key "C-d"    #'evil-scroll-down                    evil-normal-state-map)
    (bind-key "C-u"    #'evil-scroll-up                      evil-motion-state-map)
    (bind-key "C-u"    #'evil-scroll-up                      evil-normal-state-map)
    (bind-key "C-w T"  #'my/move-current-window-to-new-frame evil-normal-state-map)
    (bind-key "C-w T"  #'my/move-current-window-to-new-frame evil-motion-state-map)
    (bind-key "<down>" #'evil-next-visual-line               evil-motion-state-map)
    (bind-key "<up>"   #'evil-previous-visual-line           evil-motion-state-map)
    (bind-key "]c"     #'git-gutter:next-hunk                evil-normal-state-map)
    (bind-key "[c"     #'git-gutter:previous-hunk            evil-normal-state-map)
    (bind-key "[l"     #'langtool-goto-previous-error        evil-normal-state-map)
    (bind-key "]l"     #'langtool-goto-next-error            evil-normal-state-map)
    (bind-key "C-n"    #'company-next-page                   evil-insert-state-map)
    (bind-key "C-p"    #'company-previous-page               evil-insert-state-map)
    (bind-key "]e"     #'next-error                          evil-normal-state-map)
    (bind-key "[e"     #'previous-error                      evil-normal-state-map)
    (bind-key "<"      #'beginning-of-buffer                 evil-normal-state-map)
    (bind-key ">"      #'end-of-buffer                       evil-normal-state-map)

    ;; (bind-key ",\\"    #'skk-mode                            evil-normal-state-map)

    (unbind-key "M-." evil-normal-state-map)

    (add-hook 'with-editor-mode-hook 'evil-insert-state)
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

    ;; (setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
    ;; (setq evil-emacs-state-modes nil)

    (evil-declare-change-repeat 'company-complete)

    (evil-define-key '(motion normal) org-mode-map
      (kbd "C-c C-s") #'org-schedule)

    (dolist (element my/text-modes)
      (evil-define-key '(motion normal) element
        (kbd "<down>") #'evil-next-visual-line
        (kbd "<up>")   #'evil-previous-visual-line))

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
      "C-d" #'evil-scroll-down
      "C-u" #'evil-scroll-up)

    (evil-define-key '(motion emacs) mu4e-headers-mode-map
      "C-d" #'evil-scroll-down
      "C-u" #'evil-scroll-up)

    (evil-define-key '(visual) mu4e-compose-mode-map
      "H" #'org-mime-htmlize)

    (evil-define-key 'normal flyspell-mode-map
      ;; (kbd "[s") 'flyspell-goto-next-error
      ;; (kbd "]s") 'flyspell-goto-next-error
      (kbd "[l") #'langtool-goto-previous-error
      (kbd "]l") #'langtool-goto-next-error)

    (evil-define-key 'normal org-mode-map
      (kbd "<tab>") #'org-cycle
      (kbd "TAB")   #'org-cycle)

    (evil-define-key '(visual normal) org-mode-map
      ",t"  #'my/google-translate-at-point)

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
      ",t" #'my/toggle-php-flavor-mode)

    (evil-define-key 'normal php-mode-map
      ",t" #'my/toggle-php-flavor-mode)

    (evil-define-key 'normal prog-mode-map
      ",c" #'flycheck-mode
      ",e" #'flycheck-list-errors)

    (evil-define-key 'normal tide-mode-map
      ",t" #'hydra-tide/body
      "M-." #'tide-jump-to-implementation
      "M-," #'tide-jump-back)

    (evil-define-key 'normal php-mode-map
      ",d" 'hydra-php-debug/body)

    (defalias #'forward-evil-word #'forward-evil-symbol)

    (add-hook #'evil-insert-state-entry-hook
      (lambda ()
        (when current-input-method
          (deactivate-input-method))))
    ))

(use-package evil-surround
  :config
  (progn
    (global-evil-surround-mode 1)))

(use-package evil-matchit
  :config
  (progn
    (global-evil-matchit-mode 1)))

(use-package evil-visualstar
  :config
  (progn
    (global-evil-visualstar-mode)))

(use-package evil-anzu)

(evil-declare-change-repeat 'company-complete)

(provide 'my-evil)
