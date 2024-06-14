;;; Code:

(setq-default sh-basic-offset tab-width)
(setq-default tags-add-tables nil)

;; (add-hook 'conf-space-mode-hook #'my/breadcrumb-set-local t)
;; (add-hook 'conf-unix-mode-hook #'my/breadcrumb-set-local t)
;; (add-hook 'conf-javaprop-mode-hook #'my/breadcrumb-set-local t)
;; (add-hook 'conf-toml-mode-hook #'my/breadcrumb-set-local t)

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode)

(use-package eldoc
  :straight nil
  :commands eldoc-mode
  :diminish eldoc-mode
  :custom
  (eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  (eldoc-echo-area-use-multiline-p t)
  ;; (setq eldoc-echo-area-prefer-doc-buffer t) ;; disabled eldoc in echo area
  :config
  (add-to-list 'evil-emacs-state-modes 'eldoc-mode)

  ;; Show all of the available eldoc information when we want it. This way Flymake errors
  ;; don't just get clobbered by docstrings.
  ;; (add-hook 'eglot-managed-mode-hook
  ;;   (lambda ()
  ;;     "Make sure Eldoc will show us all of the feedback at point."
  ;;     (setq-local eldoc-documentation-strategy
  ;;       #'eldoc-documentation-compose)))
  )

(use-package eldoc-box
  :after eldoc
  :straight (:type git
              :host github
              :repo "casouri/eldoc-box"
              :branch "master")
  :custom
  (eldoc-box-cleanup-interval 0))

;; (defun my/eldoc-buffer-toggle ()
;;   "Toggle the display of the Eldoc buffer."
;;   (interactive)
;;   (let (buffer (eldoc-doc-buffer))
;;     (if buffer
;;       (delete-windows-on buffer)
;;       (eldoc-doc-buffer))))

(use-package highlight-thing
  :commands highlight-thing-mode
  :custom
  (highlight-thing-delay-seconds 0.1)
  (highlight-thing-case-sensitive-p t)
  (highlight-thing-ignore-list '("False" "True"))
  (highlight-thing-limit-to-region-in-large-buffers-p nil)
  (highlight-thing-narrow-region-lines 15)
  (highlight-thing-large-buffer-limit 5000)
  (highlight-thing-exclude-thing-under-point t))

(use-package lorem-ipsum
  :commands (lorem-ipsum-insert-sentences lorem-ipsum-insert-paragraphs lorem-ipsum-insert-list))

(use-package hl-todo
  :commands hl-todo-mode)

(defun my/eglot-organize-imports ()
  (interactive)
  (eglot-code-action-organize-imports (point-min)))

(major-mode-hydra-define (prog-mode ruby-mode web-mode markdown-mode yaml-mode json-mode conf-toml-mode js2-mode rjsx-mode typescript-mode typescript-ts-mode tsx-ts-mode)
    (:hint nil :color amaranth :quit-key "q" :title (with-faicon "code" "Programming" 1 -0.05))
    ("Action"
      (("." my/tempo-insert "insert snippet" :exit t)
        ("x" xref-find-references-and-replace "replace references" :exit t)
        ("u" lorem-ipsum-insert-sentences "lorem ipsum" :exit t)
        ("p" counsel-colors-emacs "color picker" :exit t)
        ("w" my/web-mode-toggle "toggle web-mode" :exit t)
        ("o" my/eglot-organize-imports "organize imports" :exit t)
        ("v" eldoc-box-help-at-point "eldoc" :exit t)
        ;; ("lr" eglot-rename "rename" :exit t)
        )
      "Navigate"
      (("d" my/xref-find-definitions "find definitions" :exit t)
        ("r" my/xref-find-references "find references" :exit t)
        ("t" projectile-find-tag "find tag" :exit t)
        ("g" counsel-projectile-git-grep "git grep" :exit t)
        ("i" counsel-imenu "imenu" :exit t)
        ("k" my/treemacs-project-toggle "treemacs" :toggle t :exit t)
        ("e" ediff "ediff files" :exit t)
        ("b" ediff-buffers "ediff buffers" :exit t)
        ;; ("i" lsp-ui-imenu "imenu" :exit t)
        ;; ("w" eglot-find-typeDefinition "find type definition" :exit t)
        ;; ("C-c e D" . eglot-find-declaration)
        ;; ("C-c e f" . eglot-format)
        ;; ("C-c e F" . eglot-format-buffer)
        )
      "AI"
      (("as" starhugger-trigger-suggestion "AI generate suggestion" :exit t)
        ("aa" starhugger-accept-suggestion "AI accept suggestion" :exit t)
        (">" starhugger-show-next-suggestion "AI next suggestion")
        ("<" starhugger-show-prev-suggestion "AI previous suggestion"))
      "Assess"
      (("cc" flycheck-mode "flycheck" :toggle t)
        ("cm" flymake-mode "flymake" :toggle t)
        ("si" treesit-inspect-mode "treesit-inspect-mode" :toggle t :exit t)
        ("se" treesit-explore-mode "treesit-explore-mode" :toggle t :exit t)
        ;; ("p" my/prettier-mode "prettier" :toggle t)
        ;; ("o" electric-operator-mode "electric operator" :toggle t)
        ;; ("i" my/dtrt-indent-mode-toggle "dtrt-indent" :toggle t)
        )))
(use-package prog-mode
  :straight nil
  :hook ((prog-mode . eldoc-mode)
          (prog-mode . rainbow-delimiters-mode)
          (prog-mode . hl-todo-mode)
          (prog-mode . highlight-thing-mode)
          (prog-mode . company-mode)
          (prog-mode . electric-pair-local-mode))
  :config
  (evil-define-key 'normal prog-mode-map
    (kbd "<S-up>") #'evil-numbers/inc-at-pt
    (kbd "<S-down>") #'evil-numbers/dec-at-pt
    (kbd "C-=") #'er/expand-region
    (kbd "C-+") #'er/contract-region)

  (cond ((and (fboundp 'company-mode) company-mode)
    (evil-define-key 'normal prog-mode-map
      (kbd "C-/") #'company-complete))
    ((and (fboundp 'corfu-mode) corfu-mode)
      (evil-define-key 'normal prog-mode-map
        (kbd "C-/") #'corfu-insert)))

  (evil-define-key 'insert prog-mode-map
    (kbd "C-/") #'company-complete
    (kbd "C-<return>") #'tempo-expand-if-complete)

  (evil-define-key 'normal prog-mode-map
    (kbd "C-c m") #'hydra-merge/body
    (kbd "/") #'counsel-grep)

  (defun my/web-mode-toggle ()
    "Toggle switch between `web-mode' and native major mode."
    (interactive)
    (when (not (boundp 'my/native-local-major-mode))
      (defvar-local my/native-local-major-mode major-mode))
    (if (eq major-mode 'web-mode)
      (funcall my/native-local-major-mode)
      (web-mode)))

  (defun my/treemacs-project-toggle ()
    "Toggle treemacs for current project."
    (interactive)
    (if (string-equal (treemacs-current-visibility) "visible")
      (treemacs)
      (treemacs-add-and-display-current-project-exclusively)))

  ;; (pretty-hydra-define hydra-prog
  ;;   (:hint nil :color amaranth :quit-key "q" :title (with-faicon "code" "Programming" 1 -0.05))
  ;;   ("Action"
  ;;     (("e" ediff "ediff files" :exit t)
  ;;       ("b" ediff-buffers "ediff buffers" :exit t)
  ;;       ("x" xref-find-references-and-replace "replace references" :exit t)
  ;;       ("." my/tempo-insert "insert snippet" :exit t)
  ;;       ("u" lorem-ipsum-insert-sentences "lorem ipsum" :exit t)
  ;;       ("c" counsel-colors-emacs "color picker" :exit t))
  ;;     "AI"
  ;;     (("ac" my/chatgpt-shell-start-new "ChatGpt shell" :exit t)
  ;;       ("as" starhugger-trigger-suggestion "generate suggestion" :exit t)
  ;;       ("aa" starhugger-accept-suggestion "accept suggestion" :exit t)
  ;;       (">" starhugger-show-next-suggestion "next suggestion")
  ;;       ("<" starhugger-show-prev-suggestion "previous suggestion"))
  ;;     "Find"
  ;;     (("d" my/xref-find-definitions "find definitions" :exit t)
  ;;       ("r" my/xref-find-references "find references" :exit t)
  ;;       ("t" projectile-find-tag "find tag" :exit t)
  ;;       ("g" counsel-projectile-git-grep "git grep" :exit t)
  ;;       ("l" counsel-imenu "imenu" :exit t)
  ;;       ("k" treemacs "treemacs" :toggle t :exit t))
  ;;     "Assess"
  ;;     (("fc" flycheck-mode "flycheck" :toggle t)
  ;;       ("fm" flymake-mode "flymake" :toggle t)
  ;;       ("p" my/prettier-mode "prettier" :toggle t)
  ;;       ;; ("o" electric-operator-mode "electric operator" :toggle t)
  ;;       ("i" my/dtrt-indent-mode-toggle "dtrt-indent" :toggle t))))

  (defun my/xref-find-references ()
    (interactive)
    (if lsp-managed-mode
      (funcall-interactively 'lsp-ui-peek-find-references)
      (let* ((symbol (symbol-at-point))
              (symbols-names (mapcar 'symbol-name (apropos-internal ".*")))
              (symbol (if (symbol-function symbol) (symbol-name symbol) ""))
              (symbol (if (and (not (string-empty-p symbol)) (seq-some (lambda (i) (string-match-p (regexp-quote symbol) i)) symbols-names)) symbol ""))
              (function-name
                (funcall
                  completing-read-function
                  "Find references to: "
                  symbols-names
                  'commandp
                  t
                  symbol)))
        (funcall-interactively 'xref-find-references function-name))))

  (defun my/xref-find-definitions ()
    (interactive)
    (if lsp-managed-mode
      (funcall-interactively 'lsp-ui-peek-find-definitions)
      (let* ((symbol (symbol-at-point))
              (symbols-names (mapcar 'symbol-name (apropos-internal ".*")))
              (symbol (if (symbol-function symbol) (symbol-name symbol) ""))
              (symbol (if (and (not (string-empty-p symbol)) (seq-some (lambda (i) (string-match-p (regexp-quote symbol) i)) symbols-names)) symbol ""))
              (function-name
                (funcall
                  completing-read-function
                  "Find definitions of: "
                  symbols-names
                  'commandp
                  t
                  symbol)))
        (funcall-interactively 'xref-find-definitions function-name))))

  (pretty-hydra-define hydra-merge
    (:hint nil :color pink :quit-key "q" :title (with-alltheicon "git" "Merge" 1 -0.05))
    ("Move"
      (("n" smerge-next "next")
        ("p" smerge-prev "previous"))
      "Keep"
      (("l" smerge-keep-lower "lower")
        ("u" smerge-keep-upper "upper"))
      "Diff"
      (("R" smerge-refine "redefine"))))

  (defun my/prog-mode-hook ()
    (modify-syntax-entry ?- "w")
    (modify-syntax-entry ?_ "w")
    (modify-syntax-entry ?$ "w")
    (abbrev-mode -1)
    (flyspell-mode -1)
    (hungry-delete-mode 1)
    (hl-line-mode 1)
    (show-paren-mode 1))

  (add-hook 'prog-mode-hook #'my/prog-mode-hook -50))

(use-package mmm-mode
  :commands mmm-mode
  :custom
  (mmm-submode-decoration-level 0)
  :config
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
  ;;   '((mmm-jupyter-python-mode
  ;;       :submode python-mode
  ;;       :front "```python"
  ;;       :back "```")))
  ;; (mmm-add-mode-ext-class 'org-mode nil 'mmm-jupyter-python-mode)

  ;; (mmm-add-classes
  ;;   '((mmm-jsx-mode
  ;;       :front "\\(return\s\\|n\s\\|(\n\s*\\)<"
  ;;       :front-offset -1
  ;;       :back ">\n?\s*)"
  ;;       :back-offset 1
  ;;       :submode web-mode)))
  ;; (mmm-add-mode-ext-class 'typescript-mode nil 'mmm-jsx-mode)
  )

(define-derived-mode guest-mode fundamental-mode "guest"
  "Major mode for editing as a guest in a file.
Use when `json-mode' or similar get stuck."
  (editorconfig-mode -1))

(use-package dns-mode
  :straight nil
  :mode ("\\.zone?\\'" . zone-mode))

(use-package electric-operator
  :commands electric-operator-mode
  :diminish electric-operator-mode)

(use-package sh-script
  :straight nil
  :hook (after-save . executable-make-buffer-file-executable-if-script-p)
  :mode (("\\.bashrc\\'" . sh-mode)
          ("\\.bash_logout\\'" . sh-mode)
          ("\\.sh\\'" . sh-mode)
          ("\\.z?sh\\'" . sh-mode)
          ("\\.profile\\'" . sh-mode)))

;; (use-package sql-indent
;;   :after (:any sql sql-interactive-mode)
;;   :diminish sql-indent-mode)

;; http://mattbriggs.net/blog/2012/03/18/awesome-emacs-plugins-ctags/
(defun my/ctags-build ()
  (interactive)
  (let ((project-root (projectile-project-root)))
    (if project-root
      (progn
        (if (not (executable-find "ctags"))
          (message "No executable 'ctags' found!")
        (start-process "ctags" nil (format "ctags -e -f -R %s" project-root))
        (my/visit-project-ctags)
        (message "Tags build successfully."))
      (user-error "Cannot generate TAGS, not a projectile project.")))))

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
    (if (not (executable-find "ctags"))
      (message "No executable 'ctags' found")
    (when (and project-root (file-readable-p tags-file))
      (start-process "ctags update" nil (format "ctags -e %s" project-root))
      (message (format "Tags for file %s updated." current-file))))))

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package conf-mode
  :straight nil
  :mode ("^\\.env\\(.local\\)?\\(.example\\)?\\'"))

(use-package markdown-mode
  :mode (("\\.markdown\\'" . markdown-mode)
          ("\\.mdx?\\'" . markdown-mode)
          ("README\\.md\\'" . gfm-mode))
  :hook ((markdown-mode . abbrev-mode)
          (markdown-mode .
            (lambda ()
              (setq-local fill-column 80)

              ))
          )
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
  :commands (flycheck-mode flycheck-buffer)
  :custom
  (flymake-phpcs-show-rule t)
  (flycheck-display-errors-delay .3)
  (flycheck-phpcs-standard "WordPress")
  :config
  (pretty-hydra-define hydra-flycheck
    (:hint nil :color teal :quit-key "q" :title (with-faicon "check" "Flycheck" 1 -0.05))
    ("Checker"
      (("f" flyspell-mode "flyspell" :toggle t)
        ("?" flycheck-describe-checker "describe")
        ("d" flycheck-disable-checker "disable")
        ("m" flycheck-mode "mode")
        ("s" flycheck-select-checker "select"))
      "Errors"
      (("<" flycheck-previous-error "previous" :color pink)
        (">" flycheck-next-error "next" :color pink)
        ("b" flycheck-buffer "check")
        ("l" flycheck-list-errors "list"))
      "Other"
      (("M" flycheck-manual "manual")
        ("v" flycheck-verify-setup "verify setup"))))

  (add-to-list 'evil-emacs-state-modes 'flymake-diagnostics-buffer-mode)

  (evil-define-key 'normal global-map
    (kbd "C-c f") 'hydra-flycheck/body)

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

(use-package eglot
  :commands (eglot eglot-ensure eglot-alternatives)
  :straight nil
  :custom
  (eglot-extend-to-xref t)
  ;; (eglot-events-buffer-size 100000)
  (eglot-events-buffer-size 0)
  (read-process-output-max (* 1024 1024)) ;; 1mb
  (gc-cons-threshold 100000000)
  (eglot-ignored-server-capabilities '(:documentHighlightProvider :workspace/didChangeWorkspaceFolders))
  (eglot-autoshutdown t)
  :config
  (add-to-list 'eglot-server-programs '(php-mode . ("intelephense" "--stdio")))
  (add-to-list 'eglot-server-programs '(svelte-mode . ("svelteserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(shopify-mode . ("theme-check-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(web-mode . ("vscode-html-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(rjsx-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))

  (eglot--code-action eglot-code-action-organize-imports-ts "source.organizeImports.ts")
  ;; (add-hook 'eglot-managed-mode-hook #'eldoc-box-help-at-point t))
  )

;; (use-package lsp-mode
;;   :commands (lsp lsp-deferred)
;;   ;; :hook (lsp-mode . lsp-enable-which-key-integration)
;;   :custom
;;   (lsp-enable-snippet nil)
;;   (lsp-enable-semantic-highlighting nil)
;;   (lsp-enable-symbol-highlighting nil)
;;   (lsp-enable-file-watchers t)
;;   (lsp-enable-folding nil)
;;   (lsp-diagnostics-provider :none)
;;   (lsp-enable-completion-at-point nil)
;;   (lsp-semantic-tokens-enable nil)
;;   (lsp-enable-links nil)
;;   ;; (lsp-client-packages '(lsp-clients)) ;; https://github.com/emacs-lsp/lsp-mode/pull/1498
;;   ;; (lsp-enable-indentation t)
;;   ;; (lsp-javascript-format-enable t)
;;   ;; (lsp-enable-on-type-formatting nil)
;;   ;; (lsp-javascript-suggest-enabled nil)
;;   ;; (lsp-javascript-validate-enabled nil)
;;   ;; (lsp-enable-relative-indentation nil)
;;   ;; (lsp-completion-enable-additional-text-edit nil)
;;   ;; (lsp-javascript-update-imports-on-file-move-enabled nil)
;;   (lsp-headerline-breadcrumb-enable t)
;;   (lsp-modeline-code-actions-enable nil)
;;   (lsp-modeline-diagnostics-enable nil)
;;   (lsp-signature-render-documentation t)
;;   ;; (lsp-keymap-prefix "C-c l")
;;   (lsp-eldoc-enable-hover t)
;;   (lsp-restart 'auto-restart)
;;   (lsp-lens-enable nil)
;;   (lsp-eslint-enable nil)
;;   ;; (lsp-log-io t)
;;   (lsp-clients-svlangserver-disableLinting t)
;;   (lsp-rf-language-server-trace-serve "off")
;;   ;; (lsp-eslint-server-command '("node" "~/.emacs.d/.extension/vscode/vscode-eslint/server/out/eslintServer.js" "--stdio"))
;;   :config
;;   (add-to-list 'lsp-language-id-configuration '(rjsx-mode . "javascriptreact"))
;;   (add-to-list 'lsp-language-id-configuration '(rjsx-mode . "javascript"))
;;   (add-to-list 'lsp-language-id-configuration '(typescript-ts-mode . "typescriptreact"))
;;   (add-to-list 'lsp-language-id-configuration '(web-mode . "html"))

;;   ;; don't scan 3rd party javascript libraries
;;   (push "[/\\\\][^/\\\\]*\\.\\(json\\|html\\|jade\\)$" lsp-file-watch-ignored-directories) ; json

;;   ;; don't ping LSP lanaguage server too frequently
;;   (defvar lsp-on-touch-time 0)
;;   (defadvice lsp-on-change (around lsp-on-change-hack activate)
;;     ;; don't run `lsp-on-change' too frequently
;;     (when (> (- (float-time (current-time))
;;                lsp-on-touch-time) 30) ;; 30 seconds
;;       (setq lsp-on-touch-time (float-time (current-time)))
;;       ad-do-it))

;;   ;; (add-to-list 'lsp-language-id-configuration '(graphql-mode . "graphql"))
;;   ;; (add-to-list 'lsp-language-id-configuration '(".*\\.htm" . "html"))
;;   ;; (add-to-list 'lsp-language-id-configuration '(".*\\.njk" . "html"))

;;   (setq lsp-disabled-clients
;;     '(
;;        ;; (typescript-mode . (eslint))
;;        ;; (json-mode . (eslint json-ls))
;;        ;; (js-mode . (eslint))
;;        (rjsx-mode . (eslint))
;;        lsp-emmet-ls
;;        emmet-ls)))

(use-package lsp-origami
  :disabled t
  :after (lsp-mode origami)
  :hook (lsp-after-open . lsp-origami-try-enable))

(use-package lsp-ui
  :disabled t
  :after lsp-mode
	:commands lsp-ui-mode
  ;; :hook ((lsp-mode . lsp-ui-mode)
  ;;         (lsp-mode . lsp-ui-imenu-buffer-mode))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-doc-max-height 30)
  (lsp-ui-peek-peek-height 20)
  (lsp-ui-doc-max-width 350)
  (lsp-ui-sideline-enable t)
  (lsp-ui-imenu-window-width 50)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-header t)
  (lsp-ui-imenu-auto-refresh t)
  (lsp-ui-imenu-refresh-delay 1)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t)
  ;; :config
  ;; (evil-define-key 'normal lsp-ui-mode-map
  ;;   (kbd ",l") #'lsp-ui-imenu))
  )

(use-package lsp-treemacs
  :disabled t
  :after (lsp-mode treemacs)
  :commands (lsp-treemacs-errors-list lsp-treemacs-call-hierarch)
  :config
  (lsp-treemacs-sync-mode 1))

(use-package lsp-ivy
  :disabled t
  :after (lsp-mode ivy)
  :commands lsp-ivy-workspace-symbol)

;; https://emacs-lsp.github.io/lsp-mode/page/installation/#use-package
;; (use-package dap-mode
;;   :after lsp-mode
;;   :config
;;   (require 'dap-chrome)
;;   (dap-chrome-setup)
;;   ;; https://emacs-lsp.github.io/dap-mode/page/configuration/#javascript
;;   (setq dap-chrome-debug-program "~/.vscode/extensions/msjsdiag.debugger-for-chrome-4.12.11/out/src/chromeDebug.js"))

(use-package dtrt-indent
  :commands (dtrt-indent-mode my/dtrt-indent-mode-toggle)
  :diminish "dtrt"
  :preface
  (defun my/dtrt-indent-mode-toggle ()
    "Toggle dtrt-indent mode."
    (interactive)
    (if (eq dtrt-indent-mode t)
      (dtrt-indent-mode -1)
      (dtrt-indent-mode 1))))

(use-package terraform-mode
  :mode "\\.tf\\'")

;; TODO what it does?
;; (use-package company-web
;; :requires company-mode)

;; (use-package company-quickhelp
;; :requires company-mode)

;; (use-package dockerfile-mode
;;   :disabled t
;;   :mode "^Dockerfile\\'")

(use-package dockerfile-ts-mode
  :straight nil
  :mode ("Dockerfile\\'" . dockerfile-ts-mode))

(use-package graphql-mode
  :commands graphql-mode
  :mode "\\.graphql\\'"
  :custom
  (graphql-indent-level tab-width))

(use-package go-mode
  :mode ("\\.thtml\\'" "\\.gohtml\\'" "\\.tm?pl\\'"))

;; (use-package plantuml-mode
;;   :mode ("\\.plantuml\\'" "\\.puml\\'")
;;   :custom (plantuml-jar-path (expand-file-name (format "%s/plantuml.jar" xdg-lib))))

(use-package elisp-mode
  :straight nil
  :commands emacs-lisp-mode
  :hook (emacs-lisp-mode . flycheck-mode)
  :diminish "Elisp"
  :mode "\\.elc?$"
  :config
  (unbind-key "C-M-i" emacs-lisp-mode-map)

  (evil-define-key 'normal emacs-lisp-mode-map
    (kbd ",d") #'hydra-debug-elisp/body
    (kbd "<down>") #'evil-next-visual-line
    (kbd "<up>") #'evil-previous-visual-line)

  (pretty-hydra-define hydra-debug-elisp
    (:hint nil :color amaranth :quit-key "q" :title (with-faicon "bug" "Debug" 1 -0.05))
    ("Toggle"
      (("e" afa/edebug-on-entry-on-point "debug function on entry" :exit t)
        ("c" edebug-cancel-on-entry "cancel debug function" :exit t)
        ("r" toggle-debug-on-error "toggle debug on error" :exit t)
        ("q" toggle-debug-on-quit "toggle debug on quit" :exit t)
        ("o" edebug-remove-instrumentation "remove instrumentation" :exit t))
        ;; ("c" edebug-x-kill-breakpoint "kill breakpoint" :exit t)
      "Breakpoint"
      (("s" edebug-set-breakpoint "set breakpoint" :exit t)
        ("u" edebug-unset-breakpoint "unset breakpoint" :exit t)
        ("t" edebug-toggle-disable-breakpoint "toggle disable breakpoint" :exit t))
      "Show"
      (("b" edebug-x-show-breakpoints "show breakpoints" :exit t)
        ("i" edebug-x-show-instrumented "show instrumented functions" :exit t)
        ("d" edebug-x-show-data "show breakpoints and instrumented functions buffer" :exit t)
        ("h" edebug-menu "menu" :exit t)
        ("v" edebug-visit-eval-list "visit eval list" :exit t))
      "Navigation"
      (("x" edebug-next "next" :exit t)
        ("n" edebug-step-in "step in" :exit t)
        ("o" edebug-step-out "step out" :exit t)
        ("g" edebug-goto-here "goto to point" :exit t))))

  (defun afa/edebug-on-entry-on-point (&optional flag)
    "Enhanced `edebug-on-entry'.
ets function symbol on point as initial suggestion."
    (interactive "P")
    (let ((function-name
            (intern
              (funcall
                completing-read-function
                "Edebug on entry to: "
                (mapcar 'symbol-name (apropos-internal ".*"))
                'commandp
                t
                (symbol-name (symbol-at-point))))))
      (edebug-on-entry function-name flag)))

  (defalias 'elisp-mode 'emacs-lisp-mode))

(use-package solidity-mode
  :straight (:type git
              :host github
              :repo "ethereum/emacs-solidity"
              :branch "master")
  :mode "\\.sol\\'"
  :hook ((solidity-mode . (lambda () (setq-local c-basic-offset 4))))
  :preface
  (setq-default solidity-solium-path (format "%s/solium" (getenv "NVM_BIN")))
  (unless (file-exists-p solidity-solium-path)
    (message "'solium' not installed!"))
  :if (file-exists-p solidity-solium-path)
  :custom
  (solidity-flycheck-solium-checker-active t))

(use-package company-solidity
  :after company
  :mode "\\.sol\\'")

(use-package jenkinsfile-mode
  :mode "^Jenkinsfile\\'")

(use-package ivy-xref
  :after ivy
  :init
  (when (>= emacs-major-version 27)
    (setq-default xref-show-definitions-function #'ivy-xref-show-defs))
  :custom
  (xref-show-xrefs-function #'ivy-xref-show-xrefs)
  (xref-show-definitions-function #'ivy-xref-show-defs))

;; TODO remove it when treesit for tsx mature
;; (use-package tree-sitter
;;   :disabled t
;;   :diminish ts)

;; TODO remove it when treesit for tsx mature
;; (use-package tree-sitter-langs
;;   :disabled t
;;   :after tree-sitter)

(use-package treesit
  :straight nil
  :custom
  (treesit-language-source-alist
    '((bash       "https://github.com/tree-sitter/tree-sitter-bash")
       (c          "https://github.com/tree-sitter/tree-sitter-c/" "master" "src")
       (clojure    "https://github.com/sogaiu/tree-sitter-clojure" "master" "src")
       (cpp        "https://github.com/tree-sitter/tree-sitter-cpp/" "master" "src")
       (cmake      "https://github.com/uyha/tree-sitter-cmake")
       (css        "https://github.com/tree-sitter/tree-sitter-css")
       (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "main" "src")
       (elisp      "https://github.com/Wilfred/tree-sitter-elisp")
       (elixir     "https://github.com/elixir-lang/tree-sitter-elixir" "main" "src")
       (erlang     "https://github.com/WhatsApp/tree-sitter-erlang" "main" "src")
       (go         "https://github.com/tree-sitter/tree-sitter-go")
       (haskell    "https://github.com/tree-sitter/tree-sitter-haskell" "master" "src")
       (html       "https://github.com/tree-sitter/tree-sitter-html")
       (java       "https://github.com/tree-sitter/tree-sitter-java" "master" "src")
       (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
       (json       "https://github.com/tree-sitter/tree-sitter-json")
       (julia      "https://github.com/tree-sitter/tree-sitter-julia" "master" "src")
       (lua        "https://github.com/MunifTanjim/tree-sitter-lua" "main" "src")
       (make       "https://github.com/alemuller/tree-sitter-make")
       (markdown   "https://github.com/ikatyang/tree-sitter-markdown")
       (meson      "https://github.com/Decodetalkers/tree-sitter-meson" "master" "src")
       (python     "https://github.com/tree-sitter/tree-sitter-python")
       (ruby       "https://github.com/tree-sitter/tree-sitter-ruby" "master" "src")
       (rust       "https://github.com/tree-sitter/tree-sitter-rust" "master" "src")
       (toml       "https://github.com/tree-sitter/tree-sitter-toml")
       (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
       (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
       (yaml       "https://github.com/ikatyang/tree-sitter-yaml")))
  ;; :config
  ;; (add-to-list 'treesit-major-mode-language-alist '(tsx-ts-mode . typescript))

  ;; (major-mode-remap-alist
  ;;   '((yaml-mode . yaml-ts-mode)
  ;;      (bash-mode . bash-ts-mode)
  ;;      (js2-mode . js-ts-mode)
  ;;      (json-mode . json-ts-mode)
  ;;      (css-mode . css-ts-mode)
  ;;      (python-mode . python-ts-mode)))
  )

;; (defun mp-remove-treesit-sexp-changes ()
;;   (when (eq forward-sexp-function #'treesit-forward-sexp)
;;     (setq forward-sexp-function nil))
;;   (when (eq transpose-sexps-function #'treesit-transpose-sexps)
;;     (setq transpose-sexps-function #'transpose-sexps-default-function))
;;   (when (eq forward-sentence-function #'treesit-forward-sentence)
;;     (setq forward-sentence-function #'forward-sentence-default-function)))
;; (add-hook 'prog-mode-hook #'mp-remove-treesit-sexp-changes)

(setq font-lock-maximum-decoration t)

(provide 'my-devel)
;;; my-devel.el ends here
