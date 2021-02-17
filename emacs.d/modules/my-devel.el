;; (require 'cc-mode)
;; (require 'elisp-mode)
;; (require 'sql)
;; (require 'gud)
;; (require 'prog-mode)
;; (require 'sh-script)
;; (require 'conf-mode)
;; (require 'ruby-mode)
;; (require 'dns-mode)
;; (require 'company-graphql)

(evil-define-key 'normal prog-mode-map
  (kbd ",e") #'my/flycheck-toggle)

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

(eval-after-load 'dns-mode
  '(progn
     (add-to-list 'auto-mode-alist '("\\.zone?\\'" . zone-mode))))

;; (eval-after-load 'conf-mode
;;   '(progn
;;     (add-hook 'conf-mode-hook
;;       (lambda ()
;;         (setq-local indent-line-function 'insert-tab)))))

(use-package electric-operator
  :diminish electric-operator-mode
  :commands electric-operator-mode)

(use-package ledger-mode
  :hook (ledger-mode . company-mode)
  :bind (:map ledger-mode-map ("C-c C-c" . ledger-post-align-dwim))
  :mode "\\.ledger\\'"
  :config
  (setq
    ledger-clear-whole-transactions t
    ledger-post-account-alignment-column 2
    ledger-reconcile-default-commodity "GBP")
  (unbind-key "<tab>" ledger-mode-map))

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
  :config
  (setq symbol-overlay-idle-time 0.1))

(setq tags-add-tables nil)
(setq my/ctags-path "/usr/local/bin/ctags")

(unless (executable-find my/ctags-path)
  (user-error "Warning no ctags available!"))

;; http://mattbriggs.net/blog/2012/03/18/awesome-emacs-plugins-ctags/
(defun my/ctags-build ()
  (interactive)
  (let ((project-root (projectile-project-root)))
    (if project-root
      (progn
        (start-process "ctags" nil (format "%s -e -f -R %s" my/ctags-path project-root))
        (my/visit-project-ctags)
        (message "Tags build successfully."))
      (user-error "Cannot generate TAGS, not a projectile project."))))

(defalias 'ctags 'my/ctags-build)

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
      (start-process "ctags update" nil (format "%s -e %s" my/ctags-path project-root))
      (message (format "Tags for file %s updated." current-file)))))

;; (use-package counsel-etags) ; it's crazy slow

(use-package yaml-mode
  :hook ((markdown-mode . jarfar/bind-value-togglers))
  :mode "\\.yaml\\'")

(use-package markdown-mode
  :hook (markdown-mode . jarfar/bind-value-togglers)
  :mode (("\\.markdown\\'" . markdown-mode)
          ("\\.mdx?\\'" . markdown-mode)
          ("README\\.md\\'" . gfm-mode)))

(use-package vimrc-mode
  :mode "\\vimrc\\'")

(use-package flycheck
  :config
  (setq flymake-phpcs-show-rule t)
  (setq flycheck-phpcs-standard "WordPress")

  (add-to-list 'display-buffer-alist
    `(,(rx bos "*Flycheck errors*" eos)
       (display-buffer-reuse-window
         display-buffer-in-side-window)
       (side            . bottom)
       (reusable-frames . visible)
       (window-height   . 0.33)))

  (defun my/flycheck-toggle ()
    (interactive)
    (if (get-buffer "*Flycheck errors*")
      (kill-buffer "*Flycheck errors*")
      (flycheck-list-errors))))

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
  :config
  ;; (setq lsp-inhibit-message nil)
  ;; (setq lsp-auto-guess-root t)
  (setq
    lsp-enable-snippet t
    lsp-enable-semantic-highlighting nil
    lsp-enable-symbol-highlighting nil
    lsp-enable-file-watchers nil
    lsp-headerline-breadcrumb-enable nil
    lsp-modeline-code-actions-enable nil
    lsp-modeline-diagnostics-enable nil
    ;; lsp-signature-auto-activate nil
    lsp-signature-render-documentation nil
    lsp-eldoc-enable-hover nil
    lsp-restart 'auto-restart
    lsp-rf-language-server-trace-serve "off"
    lsp-eslint-server-command '("node" "/Users/devil/.emacs.d/.extension/vscode/vscode-eslint/server/out/eslintServer.js" "--stdio"))

  (setq
    lsp-lens-enable nil
    lsp-ui-doc-enable nil
    lsp-ui-doc-show-with-cursor nil
    lsp-ui-doc-show-with-mouse nil
    lsp-ui-sideline-enable nil
    lsp-ui-sideline-enable nil)

  (add-hook 'lsp-mode-hook
    (lambda ()
      (add-hook 'after-save-hook 'air-revert-buffer-noconfirm 0 t)

      (when (bound-and-true-p which-key-mode)
        (lsp-enable-which-key-integration))))

  (add-to-list 'lsp-language-id-configuration '(js-jsx-mode . "javascriptreact"))
  (add-to-list 'lsp-language-id-configuration '(graphql-mode . "graphql"))
  (add-to-list 'lsp-disabled-clients '((typescript-mode . (eslint)) (json-mode . (eslint json-ls)))))

;; https://emacs-lsp.github.io/lsp-mode/page/installation/#use-package
(use-package dap-mode
  :requires lsp-mode
  :config
  (require 'dap-chrome)
  (dap-chrome-setup)
  ;; https://emacs-lsp.github.io/dap-mode/page/configuration/#javascript
  (setq dap-chrome-debug-program "/Users/devil/.vscode/extensions/msjsdiag.debugger-for-chrome-4.12.11/out/src/chromeDebug.js"))

(use-package lsp-ui
  :requires lsp-mode
  :hook ((lsp-mode . lsp-ui-mode)
          (lsp-mode . lsp-ui-imenu-buffer-mode))
	:commands lsp-ui-imenu
  :config
  (setq
    lsp-ui-imenu-window-width 50
    lsp-ui-doc-position 'top
    lsp-ui-doc-header t)

  (evil-define-key 'normal lsp-ui-mode-map
    (kbd ",l") #'lsp-ui-imenu))

  ;; (bind-key ", l" 'lsp-ui-imenu lsp-ui-mode-map)

(use-package lsp-treemacs
  :after lsp-mode treemacs
  :commands lsp-treemacs-errors-list lsp-treemacs-call-hierarch
  :config
  (lsp-treemacs-sync-mode 1))

(use-package dtrt-indent
  :diminish "dtrt")

(defun my/dtrt-indent-mode-toggle ()
  "Toggle dtrt-indent mode."
  (interactive)
  (if (eq dtrt-indent-mode t)
    (dtrt-indent-mode -1)
    (dtrt-indent-mode 1)))

;; (use-package guess-style
;; :config
;; (progn
;; (add-hook 'python-mode-hook 'guess-style-guess-tabs-mode)))

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
  :commands graphql-mode)

(use-package yasnippet
  :hook (prog-mode . (lambda () (yas-reload-all) (yas-minor-mode)))
  :diminish yas-minor-mode
  :config
  (yas-reload-all)
  ;; (add-to-list 'yas-key-syntaxes w w")
  (setq yas-new-snippet-default
    "# name: $2
# key: $1
# --
$0`(yas-escape-text yas-selected-text)`"))

;; (use-package plantuml-mode
;;   :mode ("\\.plantuml\\'" "\\.puml\\'")
;;   :custom (plantuml-jar-path (expand-file-name (format "%s/plantuml.jar" xdg-lib))))

(defun my/prog-mode-hook ()
  (modify-syntax-entry ?- "w" (syntax-table))
  (modify-syntax-entry ?_ "w" (syntax-table))
  (modify-syntax-entry ?$ "w" (syntax-table))

  (evil-define-key 'normal prog-mode
    (kbd "<S-up>") #'farynaio/increment
    (kbd "<S-down>") #'farynaio/decrement)

  ;; (flycheck-mode 1)
  (abbrev-mode -1)
  (flyspell-mode -1)
  (hungry-delete-mode 1)
  (hl-line-mode 1)
  (show-paren-mode 1)
  (electric-operator-mode 1))

(add-hook 'prog-mode-hook #'my/prog-mode-hook -50)

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

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (setq mode-name "Elisp")
    (flycheck-mode -1)
    (electric-operator-mode -1)
    (eldoc-mode 1)
    (unbind-key "C-M-i" emacs-lisp-mode-map)))

;; blogging
;; http://www.i3s.unice.fr/~malapert/org/tips/emacs_orgmode.html
;; (require 'ox-publish)
;; (setq org-html-coding-system 'utf-8-unix)
;; (setq org-html-head-include-default-style nil)
;; (setq org-html-head-include-scripts nil)
;; (setq org-html-validation-link nil)

(provide 'my-devel)
