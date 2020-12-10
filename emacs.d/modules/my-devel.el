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

;; (use-package mmm-mode
;;   :config
;;   (progn
;;     (add-hook 'mmm-mode-hook
;;       (lambda ()
;;         (set-face-background 'mmm-default-submode-face nil)))))

(setq sh-basic-offset 2)
(setq c-basic-offset 'set-from-style)

(auto-highlight-symbol-mode 1)
(hl-todo-mode 1)

(define-derived-mode guest-mode fundamental-mode "guest"
  "major mode for guest editing."
  (editorconfig-mode -1))

;; (use-package minimap
;;   :config
;;   (setq minimap-window-location 'right))

(eval-after-load 'dns-mode
  '(progn
     (add-to-list 'auto-mode-alist '("\\.zone?\\'" . zone-mode))))

(eval-after-load 'conf-mode
  '(progn
     (add-hook 'conf-mode-hook
       (lambda ()
         (setq-local tab-width 2)
         (setq-local c-basic-offset 2)
         (setq-local indent-line-function #'insert-tab)
         (setq-local indent-tabs-mode t)
         (modify-syntax-entry ?_ "w" (syntax-table))
         (modify-syntax-entry ?- "w" (syntax-table))))))

(use-package ledger-mode
  :bind (:map ledger-mode-map ("C-c C-c" . ledger-post-align-dwim))
  :mode "\\.ledger\\'"
  :init
  (setq ledger-clear-whole-transactions 1)
  :config
  (setq ledger-post-account-alignment-column 2)
  (unbind-key "<tab>" ledger-mode-map))

;; nvm ; replaces shell nvm
;; prodigy ; manage external services
;; quickrun
;; expand-region.el
;; restclient.el

;; (use-package simple-httpd
;;   :config
;;   (setq httpd-root "~/Sites"))

;; (use-package skewer-mode)

;; TODO update and make JS2 minor mode to js-mode
;; (use-package js2-mode
;;   :config
;; (setq
;;   js2-skip-preprocessor-directives t
;;   js2-highlight-external-variables nil
;;   js2-mode-show-parse-errors nil
;;   js2-strict-missing-semi-warning nil)
;;     (setq js2-strict-inconsistent-return-warning nil)
;;     (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;;     (add-hook 'js2-mode-hook #'emmet-mode))

;; (use-package eglot
;;   :config
;;   (add-to-list 'eglot-server-programs '(js2-mode . ("typescript-language-server" "--stdio"))))

;; (require 'ac-js2)
;; (eval-after-load 'ac-js2
;;   '(progn
;;      (setq ac-js2-evaluate-calls nil)

;;      (add-hook 'js2-mode-hook 'jarfar/js2-init t)
;;      (add-hook 'js2-mode-hook 'skewer-mode t)
;;      ;; (add-hook 'js2-mode-hook 'jarfar/run-skewer-once t)
;;      (add-hook 'js2-mode-hook 'jarfar/js2-company-mode-init t)
;;      (add-hook 'js2-mode-hook 'ac-js2-mode t)

;;      (defun jarfar/js2-init ()
;;        (message "jarfar/js2-init run")
;;        (setq-local browse-url-browser-function 'browse-url-chrome))

;;      (defun jarfar/js2-company-mode-init ()
;;        (message "jarfar/js2-company-mode-init run")
;;        (make-local-variable 'company-backends)
;;        (add-to-list 'company-backends 'ac-js2-company))

;;      (defun jarfar/run-skewer-once ()
;;        (message "jarfar/run-skewer-once run")
;;        (run-skewer)
;;        (remove-hook 'js2-mode-hook 'jarfar/run-skewer-once))
;;      ))

;; (use-package json-mode) ; not sure if js-mode is aren't good enough
;; (use-package indium) ; inspector for node

(when (eq system-type 'gnu/linux)
  (defun my/crontab-e ()
    (interactive)
    (with-editor-async-shell-command "crontab -e"))
  (defalias 'crontab-e #'my/crontab-e))

(setq shift-select-mode nil)

(setq tags-add-tables nil)
(setq my/ctags-path "/usr/local/bin/ctags")

(unless (executable-find my/ctags-path)
  (message (concat "Warning no ctags available!")))

;; http://mattbriggs.net/blog/2012/03/18/awesome-emacs-plugins-ctags/
(defun my/ctags-build ()
  (interactive)
  (let ((project-root (projectile-project-root)))
    (if project-root
      (progn
        (start-process "ctags" nil (format "%s -e -f -R %s" my/ctags-path project-root))
        (my/visit-project-ctags)
        (message "Tags build successfully."))
      (message "Cannot generate TAGS, not a projectile project."))))

(defalias 'ctags #'my/ctags-build)

(defun my/visit-project-ctags ()
  (interactive)
  (let ((project-root (projectile-project-root)))
    (if project-root
      (visit-tags-table (concat project-root "tags"))
      (message "Cannot view TAGS table, not a projectile project."))))

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
  :hook (jade-mode . auto-highlight-symbol-mode))

;; (use-package counsel-etags) ; it's crazy slow
(use-package emmet-mode
  :diminish emmet-mode
  :hook ((sgml-mode . emmet-mode)
          (css-mode . emmet-mode))
  :config
  (setq emmet-self-closing-tag-style " /"))

(use-package realgud)
(use-package yaml-mode
  :mode "\\.yaml\\'")

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package vimrc-mode
  :mode "\\vimrc\\'")

(use-package flycheck
  :config
  (setq flymake-phpcs-show-rule t)
  (setq flycheck-phpcs-standard "WordPress")

  ;; (flycheck-add-next-checker 'javascript-tide 'append)
  (flycheck-add-mode 'typescript-tslint 'tide-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (setq-default flycheck-disabled-checkers '(javascript-jshint javascript-jscs)))

;; beautifier
(use-package web-beautify)

(define-minor-mode my/auto-indent-mode
  "Auto indent buffer on save."
  :init-value nil

  (add-hook 'before-save-hook
    (lambda ()
      (save-excursion
        (unless (eq dtrt-indent-mode t)
          (indent-region (point-min) (point-max))
          (untabify (point-min) (point-max))
          )
        )
      ) nil t))

(eval-after-load 'js
  '(progn
     (setq js-indent-level 2)
     (add-to-list 'auto-mode-alist '("\\.json\\'" . js-mode))

    (defun my/js-mode-hook()
      (modify-syntax-entry ?_ "w" (syntax-table))
      (modify-syntax-entry ?$ "w" (syntax-table)))

     (add-hook 'js-mode-hook 'my/js-mode-hook)
     (add-hook 'js-mode-hook 'my/auto-indent-mode)))

(eval-after-load 'css-mode
  '(progn
     (setq css-indent-offset 2)

     (defun my/css-mode-hook()
       ;; (flycheck-mode -1)
       (setq tab-width 2)
       (setq c-basic-offset 2)
       (setq indent-tabs-mode nil)
       (modify-syntax-entry ?_ "w" (syntax-table))
       (modify-syntax-entry ?$ "w" (syntax-table))
       (add-to-list 'company-backends 'company-css))
     (add-hook 'css-mode-hook 'my/css-mode-hook)))

;; (use-package xref-js2)

(use-package rainbow-mode
  :diminish rainbow-mode)

;; (eval-after-load 'js-mode
;;   '(progn
;;      (defun my/js-mode-hook ()
;;        (js2-refactor-mode 1)
;;        (rainbow-delimiters-mode 1)
;;        (modify-syntax-entry ?_ "w" (syntax-table))
;;        (modify-syntax-entry ?$ "w" (syntax-table))
;;        )
;;      (add-hook 'js-mode-hook #'my/js-mode-hook)))

;; (add-hook 'js2-mode-hook
;;   (lambda ()
;;     (add-to-list 'xref-backend-functions #'xref-js2-xref-backend)
;;     (evil-local-set-key 'normal (kbd ",r") #'hydra-js-refactoring/body)
;;     ))

(if (executable-find "eslint_d")
  (use-package eslintd-fix
    :hook (rjsx-mode . eslintd-fix-mode))
  (message "No executable 'eslint_d' found"))

(use-package lsp-mode
  :hook ((web-mode . lsp)
          (lsp-mode . lsp-enable-which-key-integration)) ;; which-key integration
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-enable-completion-at-point nil)
  (setq lsp-inhibit-message t)
  (setq lsp-hover-enabled nil)
  (setq lsp-signature-enabled nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-auto-guess-root t)
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-prefer-capf t)
  (setq lsp-eslint-server-command
    '("node"
       "~/.vscode/extensions/dbaeumer.vscode-eslint-2.1.5/server/out/eslintServer.js"
       "--stdio"))
  (add-to-list 'lsp-language-id-configuration '(js-jsx-mode . "javascriptreact")))

;; https://emacs-lsp.github.io/lsp-mode/page/installation/#use-package
;; (use-package dap-mode)

(use-package lsp-ui
  :after lsp-mode
	:commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-position 'top)
  (setq lsp-ui-doc-header t))

(use-package lsp-ivy
  :requires (lsp-mode ivy)
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :after lsp-mode
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode 1))

(use-package company-lsp
  :requires (company-mode lsp-mode))

(use-package typescript-mode
  :hook (typescript-mode . (lambda () (add-hook 'before-save-hook 'lsp-eslint-apply-all-fixes)))
  ;; :config
    ;; (modify-syntax-entry ?_ "w" typescript-mode-syntax-table)

    ;; (add-hook 'typescript-mode-hook
    ;;   (lambda ()
        ;; (lsp)
        ;; (make-local-variable 'company-backends)
        ;; (add-to-list 'company-backends 'company-lsp t)
        ;; )))
  )


(use-package rainbow-delimiters)

;; (use-package js2-refactor
;;   :diminish js2-refactor-mode
;;   :bind (:map js2-mode-map . ("C-k" js2r-kill)))

;; (use-package tern
;;   :config
;;   (progn
;;     (add-hook 'tern-mode-hook
;;       (lambda ()
;;         (add-to-list 'company-backends 'company-tern)))))

;; (use-package company-tern)


;; (use-package tide
;;   :diminish tide-mode
;;   :config
;;   (progn
;;     (add-hook 'tide-mode-hook
;;       (lambda ()
;;         (add-to-list 'company-backends 'company-tide)
;;         (add-hook 'before-save-hook 'tide-format-before-save nil t)
;;         ))

;;     (setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

;;     (bind-key "C-c C-l" #'tide-references tide-mode-map)

;;     (evil-make-overriding-map tide-references-mode-map 'motion)
;;     (evil-make-overriding-map tide-references-mode-map 'normal)

;;     (add-hook 'rjsx-mode-hook #'tide-setup)
;;     ))


(use-package prettier-js
  :config
  (setq prettier-js-args
    '(
       "--no-semi" "false"
       "--trailing-comma" "none"
       "--bracket-spacing" "true"
       "--jsx-bracket-same-line" "true"
       ))

  ;; (add-to-list 'auto-mode-alist '("\\.[tj]sx?\\'" . prettier-js-mode))
  )

(use-package rjsx-mode
  :hook ((rjsx-mode . emmet-mode)
          (rjsx-mode . my/rjsx-mode-setup))
  :bind (:map rjsx-mode-map
          ("<" . rjsx-electric-lt))
  :config
    ;; (add-to-list 'auto-mode-alist '("\\.[tj]sx?\\'" . rjsx-mode))
  (defun my/rjsx-mode-setup ()
    ""
    (prettier-js-mode 1)
    (setq-local emmet-expand-jsx-className? t)))

;; (defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
;;   "Workaround 'sgml-mode' and follow airbnb component style."
;;   (save-match-data
;;      (save-excursion
;;        (goto-char (line-beginning-position))
;;        (when (looking-at "^\\( +\\)\/?> *$")
;;          (let ((empty-spaces (match-string 1)))
;;            (while (search-forward empty-spaces      (line-end-position) t)
;;             (replace-match (make-string (- (length empty-spaces) sgml-basic-offset)))))))))

;; (defun my/tide-setup ()
;;   ""
;;   (unless (tide-current-server)
;;     (tide-restart-server))
;;   (tide-mode))

;; (add-hook 'js2-mode-hook #'my/tide-setup)
;; (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-mode))

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
  :mode "//.vue//'")

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
  ("i" #'tide-organize-imports "Organize imports" :exit t)
  ("r" #'tide-refactor "Refactor" :exit t)
  ("f" #'tide-fix "Fix" :exit t)
  ("r" #'tide-rename-file "Rename file" :exit t)
  ("e" #'tide-error-at-point "Error at point" :exit t)
  ("o" #'tide-references "References" :exit t)
  ("d" #'tide-documentation-at-point "Show docs" :exit t)
  ("x" #'tide-restart-server "Restart server" :exit t))

(defhydra hydra-projectile ()
  "Projectile"
  ("p" #'hydra-projectile-project/body "project" :exit t)
  ("t" #'projectile-find-tag "find tag" :exit t)
  ("o" #'projectile-find-other-file "find other file" :exit t)
  ("f" #'projectile-find-file "find file" :exit t)
  ("r" #'projectile-replace-regexp "replace" :exit t)
  ("i" #'projectile-invalidate-cache "invalidate cache" :exit t)
  ("b" #'modi/kill-non-project-buffers "kill unrelated buffers" :exit t)
  ("d" #'my/dtrt-indent-mode-toggle "dtrt-indent-mode toggle" :exit t))

(defhydra hydra-projectile-project ()
  "Projectile project"
  ("a" #'my/projectile-add-known-project "add" :exit t)
  ("r" #'projectile-remove-known-project "remove" :exit t))

(defhydra hydra-js-search ()
  "JS search"
  ("p" #'my/rgrep "grep" :exit t)
  ("s" #'tern-find-definition "find JS definition" :exit t)
  ("t" #'tern-find-definition-by-name "find JS definition by name" :exit t))
;; (define-key tern-mode-keymap [(control ?c) (control ?r)] 'tern-rename-variable)

(defhydra hydra-js-refactoring ()
  "JS refactoring"
  ("n"  hydra-js-refactoring-node/body "node" :exit t)
  ("e"  hydra-js-refactoring-extract/body "extract" :exit t)
  ("m"  hydra-js-refactoring-rename/body "rename" :exit t)
  ("r"  hydra-js-refactoring-replace/body "replace" :exit t))

(defhydra hydra-js-refactoring-node ()
  "JS refactoring node"
  ("e" #'js2r-expand-node-at-point "expand 'node'" :exit t)
  ("c" #'js2r-contract-node-at-point "contract 'node'" :exit t))

(defhydra hydra-js-refactoring-extract ()
  "JS refactoring extract"
  ("v" #'js2r-extract-var "var" :exit t)
  ("l" #'js2r-extract-let "let" :exit t)
  ("c" #'js2r-extract-const "const" :exit t)
  ("f" #'js2r-extract-function "function" :exit t)
  ("m" #'js2r-extract-method "method" :exit t))

(defhydra hydra-js-refactoring-rename ()
  "JS refactoring rename"
  ("v" #'js2r-rename-var "var" :exit t))

(defhydra hydra-js-refactoring-replace ()
  "JS refactoring replace"
  ("t" #'js2r-var-to-this "'var' which 'this'" :exit t))


;; (use-package guess-style
;; :config
;; (progn
;; (add-hook 'python-mode-hook 'guess-style-guess-tabs-mode)))

(use-package terraform-mode
  :hook (terraform-mode . terraform-format-on-save-mode)
  :mode "\\.tf\\'")

(use-package company-statistics
  :requires company)

;; TODO what it does?
(use-package company-web
  :requires company-mode)

(use-package company-quickhelp
  :requires company-mode)

(use-package dockerfile-mode
  :mode "^Dockerfile")

(use-package geben
  :hook (geben-mode . evil-emacs-state))

(use-package toml-mode
  :mode "\\.toml$")

(use-package web-mode
  :hook ((web-mode . (lambda ()
                      (modify-syntax-entry ?_ "w" (syntax-table))
                      (modify-syntax-entry ?- "w" (syntax-table))
                       (modify-syntax-entry ?$ "w" (syntax-table))))
          (web-mode . emmet-mode))
  :bind (:map web-mode-map
          ("C-c C-n" . web-mode-tag-end)
          ("C-c C-p" . web-mode-tag-beginning)
          ("<backtab>" . indent-relative)
          ("<f5>" . my/toggle-php-flavor-mode))
  :mode (("\\.php\\'" . web-mode-map)
          ("\\.phtml\\'" . web-mode-map)
          ("\\.tpl\\.php\\'" . web-mode-map)
          ("\\.js\\'" . web-mode-map)
          ("\\.html\\.twig\\'" . web-mode-map)
          ("\\.hbs\\'" . web-mode-map)
          ("\\.ejs\\'" . web-mode-map)
          ("\\.html?\\'" . web-mode-map)
          ;; ("\\.php\\'" . web-mode-map)
          )
  :config
  (setq web-mode-engines-alist '(("php" . "\\.php\\'")))
  (setq-default web-mode-markup-indent-offset tab-width)
  (setq-default web-mode-css-indent-offset tab-width)
  (setq-default web-mode-code-indent-offset tab-width))

;; Use binaries in node_modules
(use-package add-node-modules-path
  :config
  (add-hook 'web-mode 'add-node-modules-path))

;; (use-package graphql-mode)

(defun my/prog-mode-hook ()
  (make-local-variable 'company-backends)

  ;; (add-to-list 'company-backends 'company-graphql t)
  (add-to-list 'company-backends 'company-lsp t)
  (add-to-list 'company-backends 'company-gtags t)
  (add-to-list 'company-backends 'company-etags t)
  (add-to-list 'company-backends 'company-keywords)

  (setq-local company-backends (delete 'company-dabbrev company-backends))

  (make-local-variable 'flycheck-check-syntax-automatically)
  (make-local-variable 'jarfar/pairs-hash-table)

  (when (null (gethash ?\' jarfar/pairs-hash-table))
    (puthash ?\' ?\' jarfar/pairs-hash-table))

  (dolist (elt (hash-table-keys jarfar/pairs-hash-table))
    (define-key prog-mode-map (char-to-string elt) 'jarfar/electric-pair))

  (setq-local flycheck-check-syntax-automatically '(save mode-enabled))

  (setq-local tab-width 2)
  (setq-local c-basic-offset 2)

  (flycheck-mode 1)
  (hl-todo-mode 1)
  (auto-highlight-symbol-mode 1)
  (abbrev-mode -1)
  (flyspell-mode -1)
  (flyspell-prog-mode)
  (rainbow-mode 1))

(add-hook 'prog-mode-hook 'my/prog-mode-hook t)

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (setq mode-name "elisp")
    (setq-local c-basic-offset 2)
    (flycheck-mode -1)
    (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)
    (unbind-key "C-M-i" emacs-lisp-mode-map)))

;; (setq my/devel-keymaps (list emacs-lisp-mode-map web-mode-map sql-mode-map lisp-mode-map lisp-interaction-mode-map scss-mode-map java-mode-map php-mode-map python-mode-map ruby-mode-map))
;; (use-package dash-at-point
;;   :config
;;   (dolist (i my/devel-keymaps)
;;     (bind-key "C-c d" #'dash-at-point i)
;;     (bind-key "C-c e" #'dash-at-point-with-docset i)))

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

(provide 'my-devel)
