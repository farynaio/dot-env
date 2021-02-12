;; (require 'cc-mode)
(require 'css-mode)
(require 'js)
;; (require 'elisp-mode)
;; (require 'sql)
;; (require 'gud)
;; (require 'prog-mode)
;; (require 'sh-script)
;; (require 'conf-mode)
;; (require 'ruby-mode)
;; (require 'dns-mode)
;; (require 'company-graphql)

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

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package symbol-overlay
  :diminish symbol-overlay-mode
  :hook (prog-mode . symbol-overlay-mode)
  :config
  (setq symbol-overlay-idle-time 0.1))

(use-package js2-mode
  :config
  (setq
    js2-mode-show-parse-errors nil
    js2-mode-show-strict-warnings nil))

(use-package json-mode
  :mode ("\\.json\\'" "\\rc\\'"))

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

(use-package jade-mode
  :hook (jade-mode . symbol-overlay-mode)
  :mode "\\.jade\\'")

;; (use-package counsel-etags) ; it's crazy slow

(use-package emmet-mode
  :diminish emmet-mode
  :hook ((sgml-mode . emmet-mode)
          (js-mode . emmet-mode))
  :config
  (setq
    emmet-self-closing-tag-style " /"
    emmet-expand-jsx-className? t))

;; debugger
;; (use-package realgud)

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

  ;; (flycheck-add-next-checker 'javascript-tide 'append)
  (flycheck-add-mode 'typescript-tslint 'tide-mode)
  (flycheck-add-mode 'javascript-eslint 'js-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  (setq-default flycheck-disabled-checkers '(javascript-jshint javascript-jscs))

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

(use-package web-beautify
  :commands web-beautify-js web-beautify-css web-beautify-html)

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

(eval-after-load 'js
  '(progn
     (setq js-indent-level tab-width)))

(eval-after-load 'css-mode
  '(progn
     (setq css-indent-offset tab-width)
     (add-hook 'css-mode-hook
       (lambda ()
         (setq-local company-backends '((company-css company-keywords company-files)))))))

(use-package rainbow-mode
  :hook (css-mode . rainbow-mode)
  :diminish rainbow-mode)

(use-package lsp-mode
  :commands lsp lsp-deferred
  :init
  (add-hook 'js-mode-hook
    (lambda ()
      (when (not (eq major-mode 'json-mode))
        (lsp))))
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
  (add-to-list 'lsp-disabled-clients '((typescript-mode . (eslint))
                                        (json-mode . (eslint json-ls)))))

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
    lsp-ui-doc-header t))

  ;; (bind-key ", l" 'lsp-ui-imenu lsp-ui-mode-map)

(use-package lsp-treemacs
  :after lsp-mode treemacs
  :commands lsp-treemacs-errors-list lsp-treemacs-call-hierarch
  :config
  (lsp-treemacs-sync-mode 1))

(use-package typescript-mode
  :mode "\\.tsx?\\'"
  :hook ((typescript-mode . lsp)
          (typescript-mode . mmm-mode)))

(use-package rainbow-delimiters)

(use-package rjsx-mode
  :hook ((rjsx-mode . emmet-mode)
          (rjsx-mode . mmm-mode)
          (rjsx-mode . my/rjsx-mode-setup))
  :commands rjsx-mode
  :bind (:map rjsx-mode-map
          ("<" . rjsx-electric-lt))
  :mode "\\.jsx?\\'")
  ;; (add-to-list 'auto-mode-alist '("\\.[tj]sx?\\'" . rjsx-mode))

;; (defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
;;   "Workaround 'sgml-mode' and follow airbnb component style."
;;   (save-match-data
;;      (save-excursion
;;        (goto-char (line-beginning-position))
;;        (when (looking-at "^\\( +\\)\/?> *$")
;;          (let ((empty-spaces (match-string 1)))
;;            (while (search-forward empty-spaces      (line-end-position) t)
;;             (replace-match (make-string (- (length empty-spaces) sgml-basic-offset)))))))))

;; (use-package robe
;; :config
;; (progn
;;   (add-hook 'robe-mode-hook (lambda ()
;; (robe-start)
;;                               (make-local-variable 'company-backends)
;;                               (add-to-list 'company-backends 'company-robe t)))
;;   (add-hook 'ruby-mode-hook 'robe-mode)
;;   ))
;; (use-package inf-ruby)

(use-package projectile-rails
  :requires projectile
  :hook (ruby-mode . (lambda () (when (projectile-mode) (projectile-rails-on)))))

(use-package vue-mode
  :hook (vue-mode . lsp)
  :mode "//.vue//'")

(add-hook 'mmm-mode-hook
  (lambda () (set-face-background 'mmm-default-submode-face nil)))

(setq my/prettier-modes '(css-mode js-mode yaml-mode typescript-mode))

;; TODO also "prettier" key in your package.json file.
(setq my/prettier-config-files
  '(".prettierrc"
     ".prettierrc.json"
     ".prettierrc.yml"
     ".prettierrc.yaml"
     ".prettierrc.json5"
     ".prettierrc.js"
     ".prettierrc.cjs"
     "prettier.config.js"
     "prettier.config.cjs"
     ".prettierrc.toml"))

(define-minor-mode my/prettier-mode
  "My Prettier mode implementation."
  :lighter " Prettier"
  (if (map-some (lambda (key val) (file-exists-p (concat (projectile-project-root) key))) my/prettier-config-files)
      (apheleia-mode 1)
      (apheleia-mode -1)))

;; Prettier support
(require 'apheleia)
(eval-after-load 'apheleia
  '(progn
     (diminish 'apheleia-mode)))

(use-package dtrt-indent
  :diminish "dtrt")

(defun my/dtrt-indent-mode-toggle ()
  "Toggle dtrt-indent mode."
  (interactive)
  (if (eq dtrt-indent-mode t)
    (dtrt-indent-mode -1)
    (dtrt-indent-mode 1)))

(defhydra hydra-tide ()
  "Tide"
  ("i" tide-organize-imports "Organize imports" :exit t)
  ("r" tide-refactor "Refactor" :exit t)
  ("f" tide-fix "Fix" :exit t)
  ("r" tide-rename-file "Rename file" :exit t)
  ("e" tide-error-at-point "Error at point" :exit t)
  ("o" tide-references "References" :exit t)
  ("d" tide-documentation-at-point "Show docs" :exit t)
  ("x" tide-restart-server "Restart server" :exit t))

(defhydra hydra-js-search ()
  "JS search"
  ("p" my/rgrep "grep" :exit t)
  ("s" tern-find-definition "find JS definition" :exit t)
  ("t" tern-find-definition-by-name "find JS definition by name" :exit t))
;; (define-key tern-mode-keymap [(control ?c) (control ?r)] 'tern-rename-variable)

(defhydra hydra-js-refactoring ()
  "JS refactoring"
  ("n"  hydra-js-refactoring-node/body "node" :exit t)
  ("e"  hydra-js-refactoring-extract/body "extract" :exit t)
  ("m"  hydra-js-refactoring-rename/body "rename" :exit t)
  ("r"  hydra-js-refactoring-replace/body "replace" :exit t))

(defhydra hydra-js-refactoring-node ()
  "JS refactoring node"
  ("e" js2r-expand-node-at-point "expand 'node'" :exit t)
  ("c" js2r-contract-node-at-point "contract 'node'" :exit t))

(defhydra hydra-js-refactoring-extract ()
  "JS refactoring extract"
  ("v" js2r-extract-var "var" :exit t)
  ("l" js2r-extract-let "let" :exit t)
  ("c" js2r-extract-const "const" :exit t)
  ("f" js2r-extract-function "function" :exit t)
  ("m" js2r-extract-method "method" :exit t))

(defhydra hydra-js-refactoring-rename ()
  "JS refactoring rename"
  ("v" js2r-rename-var "var" :exit t))

(defhydra hydra-js-refactoring-replace ()
  "JS refactoring replace"
  ("t" js2r-var-to-this "'var' which 'this'" :exit t))


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

(use-package geben
  :hook (geben-mode . evil-emacs-state))

(use-package web-mode
  :hook ((web-mode . emmet-mode)
          (web-mode . lsp))
  :bind (:map web-mode-map
          ("C-c C-n" . web-mode-tag-end)
          ("C-c C-p" . web-mode-tag-beginning)
          ("<backtab>" . indent-relative)
          ("<f5>" . my/toggle-php-flavor-mode))
  :mode (("\\.php\\'" . web-mode)
          ("\\.phtml\\'" . web-mode)
          ("\\.tpl\\.php\\'" . web-mode)
          ;; ("\\.js\\'" . web-mode)
          ("\\.html\\.twig\\'" . web-mode)
          ("\\.hbs\\'" . web-mode)
          ("\\.ejs\\'" . web-mode)
          ("\\.html?\\'" . web-mode)
          ("\\.svg\\'" . web-mode)
          ;; ("\\.php\\'" . web-mode)
          )
  :config
  (setq
    web-mode-engines-alist '(("php" . "\\.php\\'"))
    web-mode-markup-indent-offset tab-width
    web-mode-css-indent-offset tab-width
    web-mode-code-indent-offset tab-width)

  (add-hook 'before-save-hook
    (lambda ()
      (when (and (fboundp 'web-beautify-html) (eq dtrt-indent-mode nil))
        (web-beautify-html)))
    0 t))

;; Use binaries in node_modules
(use-package add-node-modules-path
  :hook ((js-mode . add-node-modules-path)
          (rjsx-mode . add-node-modules-path)
          (typescript-mode . add-node-modules-path)))

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

(defhydra hydra-snippet ()
  "Snippet"
  ("s" yas-insert-snippet "insert" :exit t)
  ("n" yas-new-snippet "new" :exit t)
  ("e" yas-visit-snippet-file "edit" :exit t)
  ("r" yas-reload-all "reload" :exit t))

(defun my/prog-mode-hook ()
  (modify-syntax-entry ?- "w" (syntax-table))
  (modify-syntax-entry ?_ "w" (syntax-table))
  (modify-syntax-entry ?$ "w" (syntax-table))

  ;; (flycheck-mode 1)
  (abbrev-mode -1)
  (flyspell-mode -1)
  (hl-line-mode 1)
  (show-paren-mode 1))

(add-hook 'prog-mode-hook 'my/prog-mode-hook)

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

(add-hook 'conf-space-mode-hook 'my/breadcrumb-set-local t)
(add-hook 'conf-unix-mode-hook 'my/breadcrumb-set-local t)
(add-hook 'conf-toml-mode-hook 'my/breadcrumb-set-local t)
(add-hook 'conf-javaprop-mode-hook 'my/breadcrumb-set-local t)
(add-hook 'prog-mode-hook 'my/breadcrumb-set-local t)

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (setq mode-name "Elisp")
    (flycheck-mode -1)
    (eldoc-mode 1)
    (unbind-key "C-M-i" emacs-lisp-mode-map)))

;; (setq my/devel-keymaps (list emacs-lisp-mode-map web-mode-map sql-mode-map lisp-mode-map lisp-interaction-mode-map scss-mode-map java-mode-map php-mode-map python-mode-map ruby-mode-map))
;; (use-package dash-at-point
;;   :config
;;   (dolist (i my/devel-keymaps)
;;     (bind-key "C-c d" 'dash-at-point i)
;;     (bind-key "C-c e" 'dash-at-point-with-docset i)))

;; blogging
;; http://www.i3s.unice.fr/~malapert/org/tips/emacs_orgmode.html
;; (require 'ox-publish)
;; (setq org-html-coding-system 'utf-8-unix)
;; (setq org-html-head-include-default-style nil)
;; (setq org-html-head-include-scripts nil)
;; (setq org-html-validation-link nil)

;; https://stackoverflow.com/a/6255409/346921
(defun my/reformat-xml ()
  "Reformats xml to make it readable (respects current selection)."
  (interactive)
  (save-excursion
    (let ((beg (point-min))
           (end (point-max)))
      (if (and mark-active transient-mark-mode)
        (progn
          (setq beg (min (point) (mark)))
          (setq end (max (point) (mark))))
        (widen))
      (setq end (copy-marker end t))
      (goto-char beg)
      (while (re-search-forward ">\\s-*<" end t)
        (replace-match ">\n<" t t))
      (goto-char beg)
      (indent-region beg end nil))))

;; https://emacs.stackexchange.com/questions/5441/function-to-delete-all-comments-from-a-buffer-without-moving-them-to-kill-ring
(defun jarfar/comment-delete (arg)
  "Delete the first comment on this line, if any.  Don't touch
the kill ring.  With prefix ARG, delete comments on that many
lines starting with this one."
  ;; (interactive "P")
  (comment-normalize-vars)
  (dotimes (_i (prefix-numeric-value arg))
    (save-excursion
      (beginning-of-line)
      (let ((cs (comment-search-forward (line-end-position) t)))
        (when cs
          (goto-char cs)
          (skip-syntax-backward " ")
          (setq cs (point))
          (comment-forward)
          ;; (kill-region cs (if (bolp) (1- (point)) (point))) ; original
          (delete-region cs (if (bolp) (1- (point)) (point)))  ; replace kill-region with delete-region
          (indent-according-to-mode))))
    (if arg (forward-line 1))))

;; https://emacs.stackexchange.com/questions/5441/function-to-delete-all-comments-from-a-buffer-without-moving-them-to-kill-ring
(defun jarfar/comment-delete-dwim (beg end arg)
  "Delete comments without touching the kill ring.  With active
region, delete comments in region.  With prefix, delete comments
in whole buffer.  With neither, delete comments on current line."
  (interactive "r\nP")
  (let ((lines (cond (arg
                       (count-lines (point-min) (point-max)))
                 ((region-active-p)
                   (count-lines beg end)))))
    (save-excursion
      (when lines
        (goto-char (if arg (point-min) beg)))
      (jarfar/comment-delete (or lines 1)))))

(defun jarfar/comments-delete-buffer ()
  "Remove all comments from the buffer."
  (interactive)
  (jarfar/comment-delete-dwim (point-min) (point-max) 1)
  (jarfar/remove-empty-lines))

;; http://ergoemacs.org/emacs/elisp_compact_empty_lines.html
(defun jarfar/remove-empty-lines ()
  (interactive)
  (let ($begin $end)
    (if (region-active-p)
      (setq $begin (region-beginning) $end (region-end))
      (setq $begin (point-min) $end (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region $begin $end)
        (while (re-search-forward "[ \t]+\n" nil "move")
          (replace-match "\n"))
        (progn
          (goto-char (point-min))
          (while (re-search-forward "\n\n+" nil "move")
            (replace-match "\n")))))))

(defun jarfar/wp-gutenberg-to-md ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward "<!-- /?wp:\\(heading\\|image\\|paragraph\\|list\\|code\\|preformatted\\).*?-->\n?" nil t)
      (replace-match "" nil nil))

    (beginning-of-buffer)
    (while (re-search-forward "<!-- /?wp:\\(qubely\\|html\\).*?-->\n?" nil t)
      (replace-match "" nil nil))

    (beginning-of-buffer)
    (while (re-search-forward "</?p>" nil t)
      (replace-match "" nil nil))

    (beginning-of-buffer)
    (while (re-search-forward "</h[1-6]>" nil t)
      (replace-match "" nil nil))

    (beginning-of-buffer)
    (while (re-search-forward "<h2>" nil t)
      (replace-match "## " nil nil))

    (beginning-of-buffer)
    (while (re-search-forward "<h3>" nil t)
      (replace-match "### " nil nil))

    (beginning-of-buffer)
    (while (re-search-forward "</?code>" nil t)
      (replace-match "`" nil nil))))

(require 'inc-dec-at-point)

(add-hook 'prog-mode-hook 'jarfar/bind-value-togglers)

(defun jarfar/bind-value-togglers ()
  (when (boundp 'evil-normal-state-local-map)
    (bind-keys
      :map evil-normal-state-local-map
      ("<S-up>" . farynaio/increment)
      ("<S-down>" . farynaio/decrement))))

(provide 'my-devel)
