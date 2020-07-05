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

(use-package minimap
  :config
  (progn
    (setq minimap-window-location 'right)))

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
;; skewer-mode
;; quickrun
;; expand-region.el
;; restclient.el

(use-package js2-mode
  :config
  (progn
    (setq js2-strict-inconsistent-return-warning nil)
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-hook 'js2-mode-hook #'emmet-mode)
    ))

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
  :config
  (progn
    (add-hook 'jade-mode-hook 'auto-highlight-symbol-mode)))

;; (use-package counsel-etags) ; it's crazy slow
(use-package emmet-mode
  :diminish emmet-mode
  :config
  (progn
    (setq emmet-self-closing-tag-style " /")
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook  'emmet-mode)
    (add-hook 'rjsx-mode-hook 'emmet-mode)))

(use-package realgud)
(use-package yaml-mode)
(use-package markdown-mode)
(use-package vimrc-mode
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\vimrc\\'" . vimrc-mode))))

(use-package flycheck
  :config
  (progn
    (setq
      flymake-phpcs-show-rule t
      flycheck-phpcs-standard "WordPress")

    ;; (flycheck-add-next-checker 'javascript-tide 'append)
    (flycheck-add-mode #'typescript-tslint #'tide-mode)
    (flycheck-add-mode #'javascript-eslint #'js2-mode)

    (setq-default flycheck-disabled-checkers '(javascript-jshint javascript-jscs))
    ))

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :config
    (progn
      (exec-path-from-shell-initialize)
      )))

;; (use-package git-timemachine)
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
     ;; (add-to-list 'auto-mode-alist '("\\rc\\'" . js-mode))
     (add-to-list 'auto-mode-alist '("\\.json\\'" . js-mode))

    (defun my/js-mode-hook()
      (modify-syntax-entry ?_ "w" (syntax-table))
      (modify-syntax-entry ?$ "w" (syntax-table)))

     (add-hook 'js-mode-hook #'my/js-mode-hook)
     (add-hook 'js-mode-hook #'my/auto-indent-mode)
     ))

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

(use-package xref-js2)

(eval-after-load 'js-mode
  '(progn
     (defun my/js-mode-hook ()
       (js2-refactor-mode 1)
       (rainbow-delimiters-mode 1)
       (modify-syntax-entry ?_ "w" (syntax-table))
       (modify-syntax-entry ?$ "w" (syntax-table))
       )
     (add-hook 'js-mode-hook #'my/js-mode-hook)))

(add-hook 'js2-mode-hook
  (lambda ()
    (add-to-list 'xref-backend-functions #'xref-js2-xref-backend)
    (evil-local-set-key 'normal (kbd ",r") #'hydra-js-refactoring/body)
    ))

(if (executable-find "eslint_d")
  (use-package eslintd-fix
    :config
    (progn
      (add-hook 'rjsx-mode-hook #'eslintd-fix-mode)
      ))
  (message "No executable 'eslint_d' found"))

(use-package lsp-mode
  :config
  (progn
    (require 'lsp-clients)
    (setq
      lsp-inhibit-message t
      lsp-hover-enabled nil
      lsp-signature-enabled nil
      lsp-enable-snippet nil
      lsp-auto-guess-root t)))

(use-package company-lsp)

(use-package typescript-mode
  :config
  (progn
    ;; (modify-syntax-entry ?_ "w" typescript-mode-syntax-table)

    (add-hook 'typescript-mode-hook
      (lambda ()
        ;; (lsp)
        ;; (make-variable-buffer-local 'company-backends)
        ;; (add-to-list 'company-backends 'company-lsp t)
        ))))

(eval-after-load 'gud
  '(progn
     (setq gud-pdb-command-name "python -m pdb ")))

(use-package rainbow-delimiters)

(use-package js2-refactor
  :diminish js2-refactor-mode
  :config
  (progn
    (bind-key "C-k" #'js2r-kill js2-mode-map)))

;; (use-package tern
;;   :config
;;   (progn
;;     (add-hook 'tern-mode-hook
;;       (lambda ()
;;         (add-to-list 'company-backends 'company-tern)))))

;; (use-package company-tern)


(use-package tide
  :diminish tide-mode
  :config
  (progn
    (add-hook 'tide-mode-hook
      (lambda ()
        (add-to-list 'company-backends 'company-tide)
        (add-hook 'before-save-hook 'tide-format-before-save nil t)
        ))

    (setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

    (bind-key "C-c C-l" #'tide-references tide-mode-map)

    (evil-make-overriding-map tide-references-mode-map 'motion)
    (evil-make-overriding-map tide-references-mode-map 'normal)

    (add-hook 'rjsx-mode-hook #'tide-setup)
    ))

(setq
  js2-skip-preprocessor-directives t
  js2-highlight-external-variables nil
  js2-mode-show-parse-errors nil
  js2-strict-missing-semi-warning nil)

(use-package prettier-js
  :config
  (progn
    (setq prettier-js-args
      '(
         "--no-semi" "false"
         "--trailing-comma" "none"
         "--bracket-spacing" "true"
         "--jsx-bracket-same-line" "true"
         ))

    ;; (add-to-list 'auto-mode-alist '("\\.[tj]sx?\\'" . prettier-js-mode))
    ))

(use-package rjsx-mode
  :config
  (progn
    (bind-key "<" #'rjsx-electric-lt rjsx-mode-map)
    ;; (add-to-list 'auto-mode-alist '("\\.[tj]sx?\\'" . rjsx-mode))

    (defun my/rjsx-mode-setup ()
      ""
      (prettier-js-mode 1)
      (setq-local emmet-expand-jsx-className? t)
      )

    (add-hook 'rjsx-mode-hook #'my/rjsx-mode-setup)
    ))

;; (defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
;;   "Workaround 'sgml-mode' and follow airbnb component style."
;;   (save-match-data
;;      (save-excursion
;;        (goto-char (line-beginning-position))
;;        (when (looking-at "^\\( +\\)\/?> *$")
;;          (let ((empty-spaces (match-string 1)))
;;            (while (search-forward empty-spaces      (line-end-position) t)
;;             (replace-match (make-string (- (length empty-spaces) sgml-basic-offset)))))))))

(defun my/tide-setup ()
  ""
  (unless (tide-current-server)
    (tide-restart-server))
  (tide-mode))

;; (add-hook 'js2-mode-hook #'my/tide-setup)
;; (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-mode))

;; (use-package robe
;; :config
;; (progn
;;   (add-hook 'robe-mode-hook (lambda ()
;; (robe-start)
;;                               (make-variable-buffer-local 'company-backends)
;;                               (add-to-list 'company-backends 'company-robe t)))
;;   (add-hook 'ruby-mode-hook 'robe-mode)
;;   ))
;; (use-package inf-ruby)

(use-package projectile-rails
  :config
  (progn
    (add-hook 'ruby-mode-hook
      (lambda ()
        (when (projectile-mode)
          (projectile-rails-on))))))

(use-package vue-mode)

(use-package dtrt-indent
  :diminish "dtrt"
  )

(defun my/dtrt-indent-mode-toggle ()
  "Toggle dtrt-indent mode."
  (interactive)
  (if (eq dtrt-indent-mode t)
    (dtrt-indent-mode -1)
    (dtrt-indent-mode 1)))

(defhydra hydra-tide ()
  "Tide"
  ("i" #'tide-organize-imports "Organize imports")
  ("r" #'tide-refactor "Refactor")
  ("f" #'tide-fix "Fix")
  ("r" #'tide-rename-file "Rename file")
  ("e" #'tide-error-at-point "Error at point")
  ("o" #'tide-references "References")
  ("d" #'tide-documentation-at-point "Show docs")
  ("x" #'tide-restart-server "Restart server")
  )

(defhydra hydra-projectile ()
  "Projectile"
  ("p" #'hydra-projectile-project/body "project" :exit t)
  ("t" #'projectile-find-tag "find tag")
  ("o" #'projectile-find-other-file "find other file")
  ("f" #'projectile-find-file "find file")
  ("r" #'projectile-replace-regexp "replace")
  ("i" #'projectile-invalidate-cache "invalidate cache")
  ("b" #'modi/kill-non-project-buffers "kill unrelated buffers")
  ("d" #'my/dtrt-indent-mode-toggle "dtrt-indent-mode toggle"))

(defhydra hydra-projectile-project ()
  "Projectile project"
  ("a" #'my/projectile-add-known-project "add" :exit t)
  ("r" #'projectile-remove-known-project "remove" :exit t))

(defhydra hydra-js-search ()
  "JS search"
  ("p" #'my/rgrep "grep")
  ("s" #'tern-find-definition "find JS definition")
  ("t" #'tern-find-definition-by-name "find JS definition by name"))
;; (define-key tern-mode-keymap [(control ?c) (control ?r)] 'tern-rename-variable)

(defhydra hydra-git ()
  "git"
  ("g" #'magit-blame "blame")
  ("e" #'magit-ediff-popup "ediff")
  ("c" #'vc-resolve-conflicts "conflicts") ;; this could be better -> magit?
  ;; ("b" #'magit-bisect-popup "bisect") ;; find a commit that introduces the bug
  ("s" #'magit-status "status")
  ("o" #'magit-checkout "checkout")
  ("b" #'magit-branch-popup "branch")
  ("d" #'magit-diff-popup "diff")
  ("h" #'magit-diff-buffer-file "diff file")
  ("z" #'magit-stash-popup "stash")
  ("l" #'magit-log-popup "log")
  ("f" #'magit-log-buffer-file-popup "file log"))

(defhydra hydra-js-refactoring ()
  "JS refactoring"
  ("n"  hydra-js-refactoring-node/body "node" :exit t)
  ("e"  hydra-js-refactoring-extract/body "extract" :exit t)
  ("m"  hydra-js-refactoring-rename/body "rename" :exit t)
  ("r"  hydra-js-refactoring-replace/body "replace" :exit t))

(defhydra hydra-js-refactoring-node ()
  "JS refactoring node"
  ("e" #'js2r-expand-node-at-point "expand 'node'")
  ("c" #'js2r-contract-node-at-point "contract 'node'"))

(defhydra hydra-js-refactoring-extract ()
  "JS refactoring extract"
  ("v" #'js2r-extract-var "var")
  ("l" #'js2r-extract-let "let")
  ("c" #'js2r-extract-const "const")
  ("f" #'js2r-extract-function "function")
  ("m" #'js2r-extract-method "method"))

(defhydra hydra-js-refactoring-rename ()
  "JS refactoring rename"
  ("v" #'js2r-rename-var "var"))

(defhydra hydra-js-refactoring-replace ()
  "JS refactoring replace"
  ("t" #'js2r-var-to-this "'var' which 'this'"))

(defhydra hydra-php-debug ()
  "PHP debug"
  ("a" #'geben-add-current-line-to-predefined-breakpoints "add brk")
  ("s" #'geben "start")
  ("q" #'geben-end "end"))

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
  :config
  (progn
    (setq elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules))
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (setq elpy-rpc-backend "jedi")
    (setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")
    ;; (setq
    ;; python-shell-interpreter "python"
    ;; python-shell-interpreter-args "-i")

    (bind-key "C-c C-l"   #'elpy-occur-definitions                  elpy-mode-map)
    (bind-key "C-c C-e"   #'elpy-multiedit-python-symbol-at-point   elpy-mode-map)
    (bind-key "C-c C-r f" #'elpy-format-code                        elpy-mode-map)
    (bind-key "C-c C-r r" #'elpy-refactor                           elpy-mode-map)

    (elpy-enable)
    ;; (add-hook 'elpy-mode-hook 'flycheck-mode)

    (advice-add 'keyboard-quit :before #'elpy-multiedit-stop)
    ))

(use-package rainbow-mode
  :diminish rainbow-mode)

(eval-after-load 'git-rebase
  '(progn
     (add-hook 'git-rebase-mode-hook (lambda () (read-only-mode -1)))))

(use-package git-commit
  :config
  (progn
    (setq git-commit-style-convention-checks nil)))

(use-package git-gutter
  :diminish git-gutter-mode
  :config
  (progn
    (global-git-gutter-mode +1)))

(use-package company-statistics)
(use-package company-web)
(use-package company-php)
(use-package company-quickhelp)

(use-package dockerfile-mode
  :config (add-to-list 'auto-mode-alist '("^Dockerfile" . dockerfile-mode)))

(defun my/toggle-php-flavor-mode ()
  (interactive)
  "Toggle mode between PHP & Web-Mode Helper modes"
  (cond ((string= mode-name "PHP")
          (web-mode))
    ((string= mode-name "Web")
      (php-mode))))

(use-package php-mode
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
    (add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

    (defun my-php-mode-hook ()
      (make-variable-buffer-local 'company-backends)
      (add-to-list 'company-backends 'company-ac-php-backend t)
      (local-set-key (kbd "<f1>") 'my-php-symbol-lookup)
      (modify-syntax-entry ?_ "w" (syntax-table))
      (modify-syntax-entry ?$ "w" (syntax-table))
      (setq php-template-compatibility nil))

    (defun my-php-symbol-lookup ()
      (interactive)
      (let ((symbol (symbol-at-point)))
        (if (not symbol)
          (message "No symbol at point.")

          (browse-url (concat "http://php.net/manual-lookup.php?pattern="
                        (symbol-name symbol))))))

    (bind-key "<f5>" 'my/toggle-php-flavor-mode php-mode-map)
    (add-hook 'php-mode-hook 'my-php-mode-hook)
    (add-hook 'php-mode-hook #'emmet-mode)))

(add-hook 'php-mode-hook (lambda ()
    (defun ywb-php-lineup-arglist-intro (langelem)
      (save-excursion
        (goto-char (cdr langelem))
        (vector (+ (current-column) c-basic-offset))))
    (defun ywb-php-lineup-arglist-close (langelem)
      (save-excursion
        (goto-char (cdr langelem))
        (vector (current-column))))
    (c-set-offset 'arglist-intro 'ywb-php-lineup-arglist-intro)
    (c-set-offset 'arglist-close 'ywb-php-lineup-arglist-close)))

(use-package geben
  :config
  (progn
    (add-hook 'geben-mode-hook 'evil-emacs-state)
    ))

(use-package ac-php
  :config
  (progn
    (setq ac-sources '(ac-source-php ))))

(use-package web-mode
  :config
  (progn
    (bind-key "C-c C-n" #'web-mode-tag-end web-mode-map)
    (bind-key "C-c C-p" #'web-mode-tag-beginning web-mode-map)

    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    ;; (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html\\.twig\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (setq web-mode-engines-alist '(("php" . "\\.php\\'")))
    (setq-default web-mode-markup-indent-offset tab-width)
    (setq-default web-mode-css-indent-offset tab-width)
    (setq-default web-mode-code-indent-offset tab-width)
    (bind-key "<backtab>" #'indent-relative web-mode-map)
    (bind-key "<f5>" 'my/toggle-php-flavor-mode web-mode-map)
    (add-hook #'web-mode-hook #'emmet-mode)

    (defun my/web-mode-setup ()
      (modify-syntax-entry ?_ "w" (syntax-table))
      (modify-syntax-entry ?- "w" (syntax-table))
      (modify-syntax-entry ?$ "w" (syntax-table)))

    (add-hook #'web-mode-hook #'my/web-mode-setup)

    ))

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
  :config
  (progn
    (setq
      magit-completing-read-function 'ivy-completing-read
      magit-refresh-status-buffer nil
      magit-item-highlight-face 'bold
      magit-diff-paint-whitespace nil
      magit-ediff-dwim-show-on-hunks t
      magit-diff-hide-trailing-cr-characters t
      magit-bury-buffer-function 'magit-mode-quit-window)

    (setq magit-display-file-buffer-function
      (lambda (buffer)
        (setq current-prefix-arg t)
        (magit-display-file-buffer-traditional buffer)))

    (setq auto-revert-buffer-list-filter
      'magit-auto-revert-repository-buffers-p)

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
    (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)

    (bind-key "|"   #'evil-window-set-width   magit-mode-map)
    (bind-key "}"   #'evil-forward-paragraph  magit-mode-map)
    (bind-key "]"   #'evil-forward-paragraph  magit-mode-map)
    (bind-key "{"   #'evil-backward-paragraph magit-mode-map)
    (bind-key "["   #'evil-backward-paragraph magit-mode-map)
    (bind-key "C-d" #'evil-scroll-down        magit-mode-map)
    (bind-key "C-u" #'evil-scroll-up          magit-mode-map)
    (bind-key "C-s" #'isearch-forward         magit-mode-map)
    (bind-key "="   #'balance-windows         magit-mode-map)
    (bind-key "C-w" #'my/copy-diff-region     magit-mode-map)
    (bind-key "r"   #'magit-reverse           magit-hunk-section-map)
    (bind-key "v"   #'evil-visual-char        magit-hunk-section-map)
    (bind-key "C-s" #'isearch-forward         magit-revision-mode-map)
    (bind-key "n"   #'evil-search-next        magit-revision-mode-map)
    (bind-key "p"   #'evil-search-previous    magit-revision-mode-map)
    (bind-key "="   #'balance-windows         magit-revision-mode-map)
    (bind-key "\\w" #'avy-goto-word-or-subword-1  magit-status-mode-map)
    (bind-key "\\c" #'avy-goto-char               magit-status-mode-map)

        ;; (when (fboundp 'evil-mode)
      ;; (bind-key "\\a" #'counsel-imenu  evil-motion-state-map))


    ;; (add-hook 'magit-ediff-quit-hook 'delete-frame)
    (add-hook 'magit-git-mode-hook (lambda () (read-only-mode nil)))
    (add-hook 'magit-status-mode-hook (lambda () (save-some-buffers t)))
    ))

(setq vc-follow-symlinks t)

(setq my/devel-keymaps (list emacs-lisp-mode-map web-mode-map sql-mode-map lisp-mode-map lisp-interaction-mode-map scss-mode-map java-mode-map php-mode-map python-mode-map ruby-mode-map))

(defun my/prog-mode-hook()
  (make-variable-buffer-local 'company-backends)

  ;; (add-to-list 'company-backends 'company-graphql t)
  (add-to-list 'company-backends 'company-gtags t)
  (add-to-list 'company-backends 'company-etags t)
  (add-to-list 'company-backends 'company-keywords)

  (setq-local company-backends (delete 'company-dabbrev company-backends))

  (make-variable-buffer-local 'flycheck-check-syntax-automatically)
  (setq-local flycheck-check-syntax-automatically '(save mode-enabled))

  (setq-local tab-width 2)
  (setq-local c-basic-offset 2)

  (flycheck-mode 1)
  (hl-todo-mode 1)
  (auto-highlight-symbol-mode 1)
  (abbrev-mode -1)
  (flyspell-mode -1)

  (when (executable-find "aspell")
    (flyspell-prog-mode))
  )

(add-hook 'prog-mode-hook 'my/prog-mode-hook t)

(add-hook 'python-mode-hook
  (lambda ()
    (setq-local tab-width 4)
    (setq python-indent-offset 4)
    ))

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

(use-package dash-at-point
  :config
  (progn
    (dolist (i my/devel-keymaps)
      (bind-key "C-c d" #'dash-at-point i)
      (bind-key "C-c e" #'dash-at-point-with-docset i))))

(use-package ledger-mode
  :init
  (setq ledger-clear-whole-transactions 1)

  :config
  (progn
    (setq ledger-post-account-alignment-column 2)

    (add-to-list 'evil-emacs-state-modes 'ledger-report-mode)
    (add-to-list 'auto-mode-alist '("\\.ledger\\'" . ledger-mode))

    (unbind-key "<tab>" ledger-mode-map)
    (bind-key "C-c C-c" #'ledger-post-align-dwim        ledger-mode-map)
    ))

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

(provide 'my-devel)
