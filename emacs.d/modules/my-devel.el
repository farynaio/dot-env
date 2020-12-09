(require 'cc-mode)
(require 'css-mode)
(require 'js)
(require 'elisp-mode)
(require 'python)
(require 'sql)
(require 'comint)
(require 'gud)
;; (require 'git-rebase)
(require 'prog-mode)
(require 'vc-git)
(require 'sh-script)
(require 'conf-mode)
(require 'ruby-mode)
(require 'dns-mode)
(require 'smerge-mode)
;; (require 'company-graphql)

;; (use-package mmm-mode
;;   :config
;;   (progn
;;     (add-hook 'mmm-mode-hook
;;       (lambda ()
;;         (set-face-background 'mmm-default-submode-face nil)))))

(define-derived-mode guest-mode fundamental-mode "guest"
  "major mode for guest editing."
  (editorconfig-mode -1))

;; (use-package minimap
;;   :config
;;   (setq minimap-window-location 'right))

(eval-after-load 'smerge-mode
  '(progn
     (defun my/smerge-mode-setup ()
       (bind-key "[w" #'smerge-prev smerge-mode-map)
       (bind-key "]w" #'smerge-next smerge-mode-map))
     (add-hook 'smerge-mode-hook #'my/smerge-mode-setup)
     ))

(eval-after-load 'conf-mode
  '(progn
     (defun my/conf-mode-setup ()
       (setq-local tab-width 2)
       (setq-local c-basic-offset 2)
       (modify-syntax-entry ?_ "w" (syntax-table))
       (modify-syntax-entry ?- "w" (syntax-table))
       (hl-todo-mode 1)
       (auto-highlight-symbol-mode 1))

      (add-hook 'conf-mode-hook #'my/conf-mode-setup)
     ))

(eval-after-load 'dns-mode
  '(progn
     (add-to-list 'auto-mode-alist '("\\.zone?\\'" . zone-mode))))

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

;; Use binaries in node_modules
(use-package add-node-modules-path
  :config
  (add-hook 'js2-mode-hook 'add-node-modules-path))

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
  :hook (
          (sgml-mode . emmet-mode)
          (css-mode . emmet-mode)
          (rjsx-mode . emmet-mode))
  :config
  (setq emmet-self-closing-tag-style " /"))

(use-package realgud)
(use-package yaml-mode)
(use-package markdown-mode)
(use-package vimrc-mode
  :mode ("\\vimrc\\'" . vimrc-mode)
  :interpreter ("vim" . vimrc-mode))

(use-package flycheck
  :config
  (setq flymake-phpcs-show-rule t)
  (setq flycheck-phpcs-standard "WordPress")

  ;; (flycheck-add-next-checker 'javascript-tide 'append)
  (flycheck-add-mode 'typescript-tslint 'tide-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (setq-default flycheck-disabled-checkers '(javascript-jshint javascript-jscs)))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

;; (use-package git-timemachine)

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
  :hook ((web-mode . lsp-deferred)
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
	:commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-position 'top)
  (setq lsp-ui-doc-header t))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode 1))

(use-package company-lsp)

(use-package typescript-mode
  :hook (typescript-mode . (lambda () (add-hook 'before-save-hook 'lsp-eslint-apply-all-fixes)))
  :config
    ;; (modify-syntax-entry ?_ "w" typescript-mode-syntax-table)

    ;; (add-hook 'typescript-mode-hook
    ;;   (lambda ()
        ;; (lsp)
        ;; (make-local-variable 'company-backends)
        ;; (add-to-list 'company-backends 'company-lsp t)
        ;; )))
  )

(eval-after-load 'gud
  '(progn
     (setq gud-pdb-command-name "python -m pdb ")))

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
  :hook (rjsx-mode . my/rjsx-mode-setup)
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
  :hook (ruby-mode . (lambda () (when (projectile-mode) (projectile-rails-on)))))

(use-package vue-mode)

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

(defhydra hydra-git ()
  "git"
  ("g" #'magit-blame "blame" :exit t)
  ("e" #'magit-ediff-popup "ediff" :exit t)
  ("c" #'vc-resolve-conflicts "conflicts" :exit t) ;; this could be better -> magit?
  ;; ("b" #'magit-bisect-popup "bisect") ;; find a commit that introduces the bug
  ("s" #'magit-status "status" :exit t)
  ("o" #'magit-checkout "checkout" :exit t)
  ("b" #'magit-branch-popup "branch" :exit t)
  ("d" #'magit-diff-popup "diff" :exit t)
  ("h" #'magit-diff-buffer-file "diff file" :exit t)
  ("z" #'magit-stash-popup "stash" :exit t)
  ("l" #'magit-log-popup "log" :exit t)
  ("f" #'magit-log-buffer-file "file log" :exit t))

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

(defhydra hydra-php-debug ()
  "PHP debug"
  ("a" #'geben-add-current-line-to-predefined-breakpoints "add brk" :exit t)
  ("s" #'geben "start" :exit t)
  ("q" #'geben-end "end" :exit t))

;; (use-package guess-style
;; :config
;; (progn
;; (add-hook 'python-mode-hook 'guess-style-guess-tabs-mode)))

(eval-after-load 'python
  '(progn
     (evil-make-overriding-map inferior-python-mode-map 'motion)
     (evil-make-overriding-map inferior-python-mode-map 'normal)
     (bind-key "C-d"  #'evil-scroll-down inferior-python-mode-map)
     ))

(add-to-list 'comint-output-filter-functions 'python-pdbtrack-comint-output-filter-function)
(add-to-list 'comint-preoutput-filter-functions  'python-pdbtrack-comint-output-filter-function)

(use-package elpy
  :bind (:map elpy-mode-map
          ("C-c C-l" . elpy-occur-definitions)
          ("C-c C-e" . elpy-multiedit-python-symbol-at-point)
          ("C-c C-r f" . elpy-format-code)
          ("C-c C-r r" . elpy-refactor))
  :config
  (setq elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules))
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (setq elpy-rpc-backend "jedi")
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "-i --simple-prompt")
  ;; (setq
  ;; python-shell-interpreter "python"
  ;; python-shell-interpreter-args "-i")

  (elpy-enable)
  ;; (add-hook 'elpy-mode-hook 'flycheck-mode)

  (advice-add 'keyboard-quit :before #'elpy-multiedit-stop)

  ;; https://www.thedigitalcatonline.com/blog/2020/07/18/emacs-configuration-for-python-javascript-terraform-and-blogging/
  ;; Prevent Elpy from overriding Windmove shortcuts
  ;; (eval-after-load "elpy"
  ;;   '(cl-dolist (key '("M-<up>" "M-<down>" "M-<left>" "M-<right>"))
  ;;      (define-key elpy-mode-map (kbd key) nil)))

  ;; Prevent Elpy from overriding standard cursor movements
  ;; (eval-after-load "elpy"
  ;;   '(cl-dolist (key '("C-<left>" "C-<right>"))
  ;;      (define-key elpy-mode-map (kbd key) nil)))

  (when (executable-find "black")
    (add-hook 'elpy-mode-hook
      (lambda () (add-hook 'before-save-hook 'elpy-black-fix-code nil t)))))

(use-package terraform-mode
  :hook (terraform-mode . terraform-format-on-save-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode)))

(eval-after-load 'git-rebase
  '(progn
     (add-hook 'git-rebase-mode-hook (lambda () (read-only-mode -1)))))

(use-package git-commit
  :config
  (setq git-commit-style-convention-checks nil))

(use-package git-gutter
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode +1))

(use-package company-statistics)
(use-package company-web)
(use-package company-php)
(use-package company-quickhelp)

(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("^Dockerfile" . dockerfile-mode)))

(defun my/toggle-php-flavor-mode ()
  (interactive)
  "Toggle mode between PHP & Web-Mode Helper modes"
  (cond ((string= mode-name "PHP")
          (web-mode))
    ((string= mode-name "Web")
      (php-mode))))

(use-package php-mode
  :hook ((php-mode . emmet-mode)
          (php-mode . (lambda ()
                       (make-local-variable 'company-backends)
                       (add-to-list 'company-backends 'company-ac-php-backend t)
                       (local-set-key (kbd "<f1>") 'my-php-symbol-lookup)
                       (modify-syntax-entry ?_ "w" (syntax-table))
                       (modify-syntax-entry ?$ "w" (syntax-table))
                       (setq php-template-compatibility nil)
                       ))
          (php-mode . (lambda ()
                      (defun ywb-php-lineup-arglist-intro (langelem)
                        (save-excursion
                          (goto-char (cdr langelem))
                          (vector (+ (current-column) c-basic-offset))))
                      (defun ywb-php-lineup-arglist-close (langelem)
                        (save-excursion
                          (goto-char (cdr langelem))
                          (vector (current-column))))
                      (c-set-offset 'arglist-intro 'ywb-php-lineup-arglist-intro)
                        (c-set-offset 'arglist-close 'ywb-php-lineup-arglist-close))))
  :bind (:map php-mode-map
          ("<f5>" . my/toggle-php-flavor-mode))
  :config
  (add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
  (add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

  (defun my-php-symbol-lookup ()
    (interactive)
    (let ((symbol (symbol-at-point)))
      (if (not symbol)
        (message "No symbol at point.")
        (browse-url (concat "http://php.net/manual-lookup.php?pattern=" (symbol-name symbol)))))))

(use-package geben
  :hook (geben-mode . evil-emacs-state))

;; (use-package ac-php
;;   :config
;;   (setq ac-sources '(ac-source-php )))

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
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\.twig\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-engines-alist '(("php" . "\\.php\\'")))
  (setq-default web-mode-markup-indent-offset tab-width)
  (setq-default web-mode-css-indent-offset tab-width)
  (setq-default web-mode-code-indent-offset tab-width))

;; (use-package graphql-mode)

(defun my/toggle-php-flavor-mode ()
  "Toggle mode between PHP & Web-Mode Helper modes."
  (interactive)
  (cond ((string= major-mode "php-mode")
          (web-mode))
    ((string= major-mode "web-mode")
      (php-mode))))

(use-package transient)

(use-package magit
  :diminish magit-auto-revert-mode
  :after (transient)
  :hook ((magit-git-mode . (lambda () (read-only-mode nil)))
          (magit-status-mode . (lambda () (save-some-buffers t))))
  :bind (:map magit-mode-map
          ("|" . evil-window-set-width)
          ("}" . evil-forward-paragraph)
          ("]" . evil-forward-paragraph)
          ("{" . evil-backward-paragraph)
          ("[" . evil-backward-paragraph)
          ("C-d" . evil-scroll-down)
          ("C-u" . evil-scroll-up)
          ("C-s" . isearch-forward)
          ("=" . balance-windows)
          ("C-w" . my/copy-diff-region)
          :map magit-hunk-section-map
          ("r" . magit-reverse)
          ("v" . evil-visual-char)
          :map magit-revision-mode-map
          ("C-s" . isearch-forward)
          ("n" . evil-search-next)
          ("p" . evil-search-previous)
          ("=" . balance-windows)
          :map magit-status-mode-map
          ("\\w" . avy-goto-word-or-subword-1)
          ("\\c" . avy-goto-char))
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq magit-refresh-status-buffer nil)
  (setq magit-item-highlight-face 'bold)
  (setq magit-diff-paint-whitespace nil)
  (setq magit-ediff-dwim-show-on-hunks t)
  (setq magit-diff-hide-trailing-cr-characters t)
  (setq magit-bury-buffer-function 'magit-mode-quit-window)

  (setq auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffers-p)
  (setq vc-handled-backends (delq 'Git vc-handled-backends))
  (setq magit-blame-styles
    '(
       (margin
         (margin-format " %s%f" " %C %a" " %H")
         (margin-width . 42)
         (margin-face . magit-blame-margin)
         (margin-body-face magit-blame-dimmed))
       (headings
         (heading-format . "%-20a %C %s
"))))

  (add-to-list 'magit-blame-disable-modes 'evil-mode)

  (magit-add-section-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream 'magit-insert-unpushed-to-upstream-or-recent)
  (magit-add-section-hook 'magit-status-sections-hook 'magit-insert-recent-commits 'magit-insert-unpushed-to-upstream-or-recent)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent))

(setq vc-follow-symlinks t)

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
  (flyspell-prog-mode))

(add-hook 'prog-mode-hook 'my/prog-mode-hook t)

(add-hook 'python-mode-hook
  (lambda ()
    (setq-local tab-width 4)
    (setq python-indent-offset 4)))

(add-hook 'conf-space-mode-hook
  (lambda ()
    (setq-local tab-width 4)
    (setq-local c-basic-offset 2)
    (setq-local indent-line-function #'insert-tab)
    (setq-local indent-tabs-mode t)))

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (setq mode-name "elisp")
    (flycheck-mode -1)
    (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)
    (setq-local c-basic-offset 2)))

(setq c-basic-offset 'set-from-style)

;; (setq my/devel-keymaps (list emacs-lisp-mode-map web-mode-map sql-mode-map lisp-mode-map lisp-interaction-mode-map scss-mode-map java-mode-map php-mode-map python-mode-map ruby-mode-map))
;; (use-package dash-at-point
;;   :config
;;   (dolist (i my/devel-keymaps)
;;     (bind-key "C-c d" #'dash-at-point i)
;;     (bind-key "C-c e" #'dash-at-point-with-docset i)))

(use-package ledger-mode
  :bind (:map ledger-mode-map ("C-c C-c" . ledger-post-align-dwim))
  :init
  (setq ledger-clear-whole-transactions 1)
  :config
  (setq ledger-post-account-alignment-column 2)
  (add-to-list 'evil-emacs-state-modes 'ledger-report-mode)
  (add-to-list 'auto-mode-alist '("\\.ledger\\'" . ledger-mode))
  (unbind-key "<tab>" ledger-mode-map))

(use-package toml-mode)

(setq sh-basic-offset 2)

;; blogging
;; http://www.i3s.unice.fr/~malapert/org/tips/emacs_orgmode.html
;; (require 'ox-publish)
;; (setq org-html-coding-system 'utf-8-unix)
;; (setq org-html-head-include-default-style nil)
;; (setq org-html-head-include-scripts nil)
;; (setq org-html-validation-link nil)

(defun my/evil-jump-to-tag-other-buffer ()
  (interactive)
  (save-excursion
    (evil-window-vsplit)
    (windmove-right)
    (evil-jump-to-tag)))

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

(defun my/copy-diff-region ()
  "Copy diff region without + or - markers."
  (interactive)
  (deactivate-mark)
  (let ((text (buffer-substring-no-properties
               (region-beginning) (region-end))))
    (kill-new (replace-regexp-in-string "^[\\+\\-]" "" text))))

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
