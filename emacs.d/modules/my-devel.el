;; (require 'cc-mode)
;; (require 'elisp-mode)
;; (require 'sql)
;; (require 'gud)
;; (require 'sh-script)
;; (require 'conf-mode)
;; (require 'ruby-mode)
;; (require 'dns-mode)
;; (require 'company-graphql)

(use-package rainbow-delimiters
  :after prog-mode
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package prog-mode
  :ensure nil
  :straight nil
  :config
  (evil-define-key 'normal prog-mode-map
    (kbd "<S-up>") #'farynaio/increment
    (kbd "<S-down>") #'farynaio/decrement
    (kbd "C-/") #'company-complete)

  (evil-define-key 'insert prog-mode-map
    (kbd "C-/") #'company-complete)

  (defun my/prog-mode-hook ()
    (modify-syntax-entry ?- "w" (syntax-table))
    (modify-syntax-entry ?_ "w" (syntax-table))
    (modify-syntax-entry ?$ "w" (syntax-table))

    (abbrev-mode -1)
    (flyspell-mode -1)
    (hungry-delete-mode 1)
    (hl-line-mode 1)
    (show-paren-mode 1))

  (add-hook 'prog-mode-hook #'my/prog-mode-hook -50))

(use-package mmm-mode
  :commands mmm-mode
  :config
  (setq mmm-submode-decoration-level 0)

  (mmm-add-classes
    '((mmm-styled-mode
        :submode css-mode
        :front "\\(styled\\|css\\)[.()<>[:alnum:]]?+`"
        :back "`;")))
  (mmm-add-mode-ext-class 'typescript-mode nil 'mmm-styled-mode)
  (mmm-add-mode-ext-class 'rjsx-mode nil 'mmm-styled-mode)

  (mmm-add-classes
    '((mmm-graphql-mode
        :submode graphql-mode
        :front "gr?a?p?h?ql`"
        :back "`;")))
  (mmm-add-mode-ext-class 'typescript-mode nil 'mmm-graphql-mode)
  (mmm-add-mode-ext-class 'rjsx-mode nil 'mmm-graphql-mode)

  ;; (mmm-add-classes
  ;;   '((mmm-jsx-mode
  ;;       :front "\\(return\s\\|n\s\\|(\n\s*\\)<"
  ;;       :front-offset -1
  ;;       :back ">\n?\s*)"
  ;;       :back-offset 1
  ;;       :submode web-mode)))
  ;; (mmm-add-mode-ext-class 'typescript-mode nil 'mmm-jsx-mode)
  )

(setq-default sh-basic-offset tab-width)

(define-derived-mode guest-mode fundamental-mode "guest"
  "major mode for guest editing."
  (editorconfig-mode -1))

(use-package dns-mode
  :ensure nil
  :straight nil
  :config
  (add-to-list 'auto-mode-alist '("\\.zone?\\'" . zone-mode)))

(use-package electric-operator
  :diminish electric-operator-mode
  :commands electric-operator-mode)

(use-package ledger-mode
  :hook ((ledger-mode . company-mode)
          (ledger-mode . ledger-flymake-enable))
  :bind (:map ledger-mode-map
          ("C-c C-c" . ledger-post-align-dwim)
          ("C-x C-s" . my/ledger-save))
  :mode "\\.ledger\\'"
  :preface
  (defun my/ledger-save ()
    "Automatically clean the ledger buffer at each save."
    (interactive)
    (ledger-mode-clean-buffer)
    (save-buffer))
  :custom
  (ledger-clear-whole-transactions t)
  (ledger-post-account-alignment-column 2)
  (ledger-reconcile-default-commodity "GBP")
  (ledger-reports
    '(("account statement" "%(binary) reg --real [[ledger-mode-flags]] -f %(ledger-file) ^%(account)")
       ("balance sheet" "%(binary) --real [[ledger-mode-flags]] -f %(ledger-file) bal ^assets ^liabilities ^equity")
       ("budget" "%(binary) --empty -S -T [[ledger-mode-flags]] -f %(ledger-file) bal ^assets:bank ^assets:receivables ^assets:cash ^assets:budget")
       ("budget goals" "%(binary) --empty -S -T [[ledger-mode-flags]] -f %(ledger-file) bal ^assets:bank ^assets:receivables ^assets:cash ^assets:'budget goals'")
       ("budget obligations" "%(binary) --empty -S -T [[ledger-mode-flags]] -f %(ledger-file) bal ^assets:bank ^assets:receivables ^assets:cash ^assets:'budget obligations'")
       ("budget debts" "%(binary) --empty -S -T [[ledger-mode-flags]] -f %(ledger-file) bal ^assets:bank ^assets:receivables ^assets:cash ^assets:'budget debts'")
       ("cleared" "%(binary) cleared [[ledger-mode-flags]] -f %(ledger-file)")
       ("equity" "%(binary) --real [[ledger-mode-flags]] -f %(ledger-file) equity")
       ("income statement" "%(binary) --invert --real -S -T [[ledger-mode-flags]] -f %(ledger-file) bal ^income ^expenses -p \"this month\"")))
  :config
  (unbind-key "<tab>" ledger-mode-map))

(use-package flycheck-ledger
  :after ledger-mode)

(use-package sh-script
  :mode "\\.z?sh\\'"
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package sql-indent
  :after (:any sql sql-interactive-mode)
  :diminish sql-indent-mode)

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package symbol-overlay
  :diminish symbol-overlay-mode
  :hook (prog-mode . symbol-overlay-mode)
  :custom
  (symbol-overlay-idle-time 0.1))

(setq tags-add-tables nil)

(unless (executable-find "ctags")
  (message "ctags: warning no ctags available!"))

;; http://mattbriggs.net/blog/2012/03/18/awesome-emacs-plugins-ctags/
(defun my/ctags-build ()
  (interactive)
  (let ((project-root (projectile-project-root)))
    (if project-root
      (progn
        (start-process "ctags" nil (format "ctags -e -f -R %s" project-root))
        (my/visit-project-ctags)
        (message "Tags build successfully."))
      (user-error "Cannot generate TAGS, not a projectile project."))))

(defalias 'ctags #'my/ctags-build)

(defun my/visit-project-ctags ()
  (interactive)
  (let ((project-root (projectile-project-root)))
    (if project-root
      (visit-tags-table (concat project-root "tags"))
      (user-error "Cannot view TAGS table, not a projectile project."))))

(defun my/ctags-update ()
  (interactive)
  (let* ((project-root (projectile-project-root))
          (current-file (file-name-nondirectory (buffer-file-name (current-buffer))))
          (current-file-path (buffer-file-name (current-buffer)))
          (tags-file (concat project-root "TAGS")))
    (when (and project-root (file-readable-p tags-file))
      (start-process "ctags update" nil (format "ctags -e %s" project-root))
      (message (format "Tags for file %s updated." current-file)))))

(use-package yaml-mode
  ;; :hook ((markdown-mode . jarfar/bind-value-togglers))
  :mode "\\.yaml\\'")

(use-package markdown-mode
  ;; :hook (markdown-mode . jarfar/bind-value-togglers)
  :mode (("\\.markdown\\'" . markdown-mode)
          ("\\.mdx?\\'" . markdown-mode)
          ("README\\.md\\'" . gfm-mode))
  ;; :config

  ;; (evil-define-key '(visual normal) markdown-mode-map
  ;;   "{" #'backward-paragraph
  ;;   "}" #'forward-paragraph
  ;;   "M-{" #'backward-paragraph
  ;;   "M-}" #'forward-paragraph
  ;;   )

  ;; (advice-add 'markdown-backward-paragraph :override #'backward-paragraph)
  ;; (advice-add 'markdown-backward-block :override #'backward-paragraph)
  ;; (advice-add 'markdown-forward-paragraph :override #'forward-paragraph)
  ;; (advice-add 'markdown-forward-bblock :override #'forward-paragraph)
  )

(use-package vimrc-mode
  :mode "\\vimrc\\'")

(use-package flycheck
  :custom
  (flymake-phpcs-show-rule t)
  (flycheck-display-errors-delay .3)
  (flycheck-phpcs-standard "WordPress")
  :config
  (add-to-list 'display-buffer-alist
    `(,(rx bos "*Flycheck errors*" eos)
       (display-buffer-reuse-window
         display-buffer-in-side-window)
       (side            . bottom)
       (reusable-frames . visible)
       (window-height   . 0.33))))

(define-minor-mode my/auto-indent-mode
  "Auto indent buffer on save."
  :init-value nil
  (add-hook 'before-save-hook
    (lambda ()
      (save-excursion
        (unless (eq dtrt-indent-mode t)
          (indent-region (point-min) (point-max))
          (untabify (point-min) (point-max)))))
    nil t))

(use-package lsp-mode
  :commands lsp lsp-deferred
  ;; :hook ((lsp-mode .
  ;;          (lambda ()
  ;;            (when (bound-and-true-p which-key-mode)
  ;;              (lsp-enable-which-key-integration))
  ;;            )))
  :custom
  (lsp-enable-snippet t)
  (lsp-enable-semantic-highlighting nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-file-watchers nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-signature-render-documentation nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-restart 'auto-restart)
  (lsp-lens-enable nil)
  (lsp-eslint-enable nil)
  (lsp-clients-svlangserver-disableLinting t)
  (lsp-rf-language-server-trace-serve "off")
  ;; (lsp-eslint-server-command '("node" "/Users/devil/.emacs.d/.extension/vscode/vscode-eslint/server/out/eslintServer.js" "--stdio"))
  :config
  (add-to-list 'lsp-language-id-configuration '(js-jsx-mode . "javascriptreact"))
  (add-to-list 'lsp-language-id-configuration '(graphql-mode . "graphql"))
  (add-to-list 'lsp-language-id-configuration '(".*\\.htm" . "html"))
  (add-to-list 'lsp-disabled-clients
    '(
       (typescript-mode . (eslint))
       (json-mode . (eslint json-ls))
       (js-mode . (eslint))
       (rjsx-mode . (eslint)))))

(use-package lsp-ui
  :after lsp-mode
	:commands lsp-ui-imenu
  :hook ((lsp-mode . lsp-ui-mode)
          (lsp-mode . lsp-ui-imenu-buffer-mode))
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-imenu-window-width 50)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-header t)
  :config
  (evil-define-key 'normal lsp-ui-mode-map
    (kbd ",l") #'lsp-ui-imenu))

(use-package lsp-treemacs
  :after lsp-mode treemacs
  :commands lsp-treemacs-errors-list lsp-treemacs-call-hierarch
  :config
  (lsp-treemacs-sync-mode 1))

;; https://emacs-lsp.github.io/lsp-mode/page/installation/#use-package
;; (use-package dap-mode
;;   :after lsp-mode
;;   :config
;;   (require 'dap-chrome)
;;   (dap-chrome-setup)
;;   ;; https://emacs-lsp.github.io/dap-mode/page/configuration/#javascript
;;   (setq dap-chrome-debug-program "/Users/devil/.vscode/extensions/msjsdiag.debugger-for-chrome-4.12.11/out/src/chromeDebug.js"))

(use-package lsp-ivy
  :after (lsp-mode ivy))

(use-package dtrt-indent
  :diminish "dtrt")

(defun my/dtrt-indent-mode-toggle ()
  "Toggle dtrt-indent mode."
  (interactive)
  (if (eq dtrt-indent-mode t)
    (dtrt-indent-mode -1)
    (dtrt-indent-mode 1)))

(use-package terraform-mode
  :hook (terraform-mode . terraform-format-on-save-mode)
  :mode "\\.tf\\'")

;; TODO what it does?
;; (use-package company-web
;; :requires company-mode)

;; (use-package company-quickhelp
;; :requires company-mode)

(use-package dockerfile-mode
  :mode "^Dockerfile")

(use-package graphql-mode
  :custom
  (graphql-indent-level tab-width)
  :commands graphql-mode)

(use-package yasnippet
  :hook (prog-mode . (lambda () (yas-reload-all) (yas-minor-mode)))
  :diminish yas-minor-mode
  :custom
  (yas-new-snippet-default
    "# name: $2
# key: $1
# --
$0`(yas-escape-text yas-selected-text)`")
  :config
  (yas-reload-all))

;; (use-package plantuml-mode
;;   :mode ("\\.plantuml\\'" "\\.puml\\'")
;;   :custom (plantuml-jar-path (expand-file-name (format "%s/plantuml.jar" xdg-lib))))

(defun my/breadcrumb-set-local ()
  (when buffer-file-name
  (if (projectile-project-p)
    (let* ((path (string-remove-prefix (projectile-project-root) buffer-file-name))
            (tokens (split-string path "/"))
            (path (string-join tokens " > ")))
      (setq-local header-line-format
        `(:eval
           (format "%s %s"
             (propertize (format "[%s]" (projectile-project-name)) 'face 'bold)
             ,path))))
    (setq-local header-line-format
      '(:eval
         (buffer-name (current-buffer)))))))

(add-hook 'conf-space-mode-hook #'my/breadcrumb-set-local t)
(add-hook 'conf-unix-mode-hook #'my/breadcrumb-set-local t)
(add-hook 'conf-toml-mode-hook #'my/breadcrumb-set-local t)
(add-hook 'conf-javaprop-mode-hook #'my/breadcrumb-set-local t)
(add-hook 'prog-mode-hook #'my/breadcrumb-set-local t)
(add-hook 'markdown-mode-hook #'my/breadcrumb-set-local t)

(use-package elisp-mode
  :ensure nil
  :straight nil
  :config
  (add-hook 'emacs-lisp-mode-hook
    (lambda ()
      (setq mode-name "Elisp")
      (flycheck-mode -1)
      (electric-operator-mode -1)
      (eldoc-mode 1)
      (unbind-key "C-M-i" emacs-lisp-mode-map))))

(use-package solidity-mode
  :config
  (add-hook 'solidity-mode-hook
    (lambda ()
      (setq-local c-basic-offset 4))))

(if (executable-find "solium")
  (progn
    (use-package solidity-flycheck
      :ensure nil
      :custom
      (solidity-flycheck-solium-checker-active t))

    (use-package company-solidity)
    )
  (progn
    (message "solidity-flycheck: Warning no 'solium' executable found, package not disabled!")
    (message "company-solidity: Warning no 'solium' executable found, package not disabled!")
    )
  )

;; blogging
;; http://www.i3s.unice.fr/~malapert/org/tips/emacs_orgmode.html
;; (require 'ox-publish)
;; (setq org-html-coding-system 'utf-8-unix)
;; (setq org-html-head-include-default-style nil)
;; (setq org-html-head-include-scripts nil)
;; (setq org-html-validation-link nil)

(require 'my-python)
(require 'my-web)
(require 'my-js)
(require 'my-php)
(require 'my-ruby)

(provide 'my-devel)
