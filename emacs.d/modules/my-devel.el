(require 'mmm-mode)
(require 'cc-mode)
(require 'css-mode)
(require 'js)
(require 'js2-mode)
(require 'elisp-mode)
(require 'python)
(require 'js)
(require 'sql)
(require 'comint)
(require 'gud)
(require 'epa)
(require 'git-rebase)
(require 'dash-at-point)

;; nvm ; replaces shell nvm
;; prodigy ; manage external services
;; skewer-mode
;; quickrun
;; expand-region.el
;; restclient.el

;; (use-package json-mode) ; not sure if js-mode is aren't good enough
;; (use-package indium) ; inspector for node

(setq tags-add-tables nil)
(setq my/ctags-path "/usr/local/bin/ctags")

(unless (executable-find my/ctags-path)
  (message concat "Warning no ctags available!"))

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

;; (use-package counsel-etags) ; it's crazy slow
(use-package emmet-mode
  :diminish emmet-mode
  :config
  (progn
    (setq
      emmet-self-closing-tag-style " /")
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
    (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
    (add-to-list 'flycheck-disabled-checkers 'javascript-jscs)
    ))
;; (use-package git-timemachine)
(use-package web-beautify)

(eval-after-load 'js
  '(progn
     (add-to-list 'auto-mode-alist '("\\rc\\'" . js-mode))))

(eval-after-load 'css-mode
  '(progn
     (add-hook 'css-mode-hook
       (lambda ()
         (add-to-list 'company-backends 'company-css)
         ))))

(eval-after-load 'gud
  '(progn
     (setq gud-pdb-command-name "python -m pdb ")))

(use-package rainbow-delimiters)
(use-package js2-refactor
  :diminish js2-refactor-mode)
(use-package tide
  :diminish tide-mode
  :config
  (progn
    (add-hook 'tide-mode-hook
      (lambda ()
        (add-to-list 'company-backends 'company-tide)))

    (setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

    (bind-key "C-c C-l"   #'tide-references                  tide-mode-map)

    (evil-make-overriding-map tide-references-mode-map 'motion)
    (evil-make-overriding-map tide-references-mode-map 'normal)
    ))

(setq
  js2-skip-preprocessor-directives t
  js2-highlight-external-variables nil
  js2-mode-show-parse-errors nil
  js2-strict-missing-semi-warning nil)

(use-package rjsx-mode
  :config
  (progn
    (bind-key "<" #'rjsx-electric-lt rjsx-mode-map)
    (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . rjsx-mode))

    (add-hook 'rjsx-mode-hook (lambda () (setq-local emmet-expand-jsx-className? t)))))

(add-hook 'js-mode-hook
  (lambda ()
    (flycheck-mode 1)
    (js2-refactor-mode 1)
    (rainbow-delimiters-mode 1)
    (evil-local-set-key 'normal (kbd ",r")  #'hydra-js-refactoring/body)))

(add-hook 'js2-mode-hook #'tide-setup)

(use-package vue-mode)

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

(eval-after-load 'epa
  '(progn
     (evil-make-overriding-map epa-key-mode-map 'motion)
     (evil-make-overriding-map epa-key-mode-map 'normal)
     ))

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
    (add-hook 'elpy-mode-hook 'flycheck-mode)

    (advice-add 'keyboard-quit :before #'elpy-multiedit-stop)
    ))

(use-package rainbow-mode
  :diminish rainbow-mode)

(eval-after-load 'git-rebase
  '(progn
      (add-hook 'git-rebase-mode-hook (lambda () (read-only-mode -1)))))

(require 'git-commit)
(eval-after-load 'git-commit
  '(progn
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

(electric-pair-mode 1)
(setq my/electric-pair-pairs '(
                            (?\" . ?\")
                            (?\{ . ?\})
                            (?\( . ?\))))
(setq electric-pair-text-pairs my/electric-pair-pairs)

(use-package php-mode
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
    (add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))))

(use-package web-mode
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html\\.twig\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.zone?\\'" . zone-mode))
    (setq web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-code-indent-offset 2)))

(use-package magit
  :diminish magit-auto-revert-mode
  :config
  (progn
    (setq
      magit-completing-read-function 'ivy-completing-read
      magit-item-highlight-face 'bold
      ;; magit-repo-dirs-depth 1
      )

    (bind-key "}"   #'evil-forward-paragraph  magit-mode-map)
    (bind-key "]"   #'evil-forward-paragraph  magit-mode-map)
    (bind-key "{"   #'evil-backward-paragraph magit-mode-map)
    (bind-key "["   #'evil-backward-paragraph magit-mode-map)
    (bind-key "C-d" #'evil-scroll-down        magit-mode-map)
    (bind-key "C-u" #'evil-scroll-up          magit-mode-map)
    (bind-key "r"   #'magit-reverse           magit-hunk-section-map)
    (bind-key "v"   #'evil-visual-char        magit-hunk-section-map)

    (add-hook 'magit-git-mode-hook (lambda () (interactive) (read-only-mode nil)))
    (add-hook 'magit-status-mode-hook (lambda () (interactive) (save-some-buffers t)))
  ))

(setq vc-follow-symlinks t)

;; VCS / git
(setq ediff-split-window-function (if (> (frame-width) 150)
				      'split-window-horizontally
				    'split-window-vertically))

;; Bring back window configuration after ediff quits
(defvar my-ediff-bwin-config nil "Window configuration before ediff.")
(defcustom my-ediff-bwin-reg ?b
  "*Register to be set up to hold `my-ediff-bwin-config'
    configuration.")

(defvar my-ediff-awin-config nil "Window configuration after ediff.")
(defcustom my-ediff-awin-reg ?e
  "*Register to be used to hold `my-ediff-awin-config' window
    configuration.")

(defun my-ediff-bsh ()
  "Function to be called before any buffers or window setup for
    ediff."
  (setq my-ediff-bwin-config (current-window-configuration))
  (when (characterp my-ediff-bwin-reg)
    (set-register my-ediff-bwin-reg
		  (list my-ediff-bwin-config (point-marker)))))

(defun my-ediff-ash ()
  "Function to be called after buffers and window setup for ediff."
  (setq my-ediff-awin-config (current-window-configuration))
  (when (characterp my-ediff-awin-reg)
    (set-register my-ediff-awin-reg
		  (list my-ediff-awin-config (point-marker)))))

(defun my-ediff-qh ()
  "Function to be called when ediff quits."
  (when my-ediff-bwin-config
    (set-window-configuration my-ediff-bwin-config)))

(add-hook 'ediff-load-hook
	  (lambda ()
	    (add-hook 'ediff-before-setup-hook
		      (lambda ()
			(setq ediff-saved-window-configuration (current-window-configuration))))
	    (let ((restore-window-configuration
		   (lambda ()
		     (set-window-configuration ediff-saved-window-configuration))))
	      (add-hook 'ediff-quit-hook restore-window-configuration 'append)
	      (add-hook 'ediff-suspend-hook restore-window-configuration 'append))))
(add-hook 'ediff-startup-hook
	  (lambda ()
	    (select-frame-by-name "Ediff")
	    (set-frame-size(selected-frame) 40 10)))
(add-hook 'ediff-before-setup-hook 'my-ediff-bsh)
(add-hook 'ediff-after-setup-windows-hook 'my-ediff-ash 'append)
(add-hook 'ediff-quit-hook 'my-ediff-qh)

;; (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js-mode))

(setq my/devel-keymaps (list emacs-lisp-mode-map web-mode-map sql-mode-map lisp-mode-map lisp-interaction-mode-map scss-mode-map java-mode-map php-mode-map python-mode-map))
(setq devel-buffers '("js" "jsx" "vim" "json" "java" "inc" "phtml" "php" "css" "scss" "html" "md" "xml" "rb" "el" "py" "el.gz"))

(add-hook 'find-file-hook
  (lambda ()
    (let* ((found nil)
            (buf-name (file-name-extension buffer-file-name) ))
	    (dolist (i devel-buffers)
	      (if (string= buf-name i)
          (progn
            (hl-todo-mode 1)
            (auto-highlight-symbol-mode 1)
            (rainbow-mode 1)
            )
          )))) t)

;; (setq c-basic-offset 2)

;; TODO modify-syntax-entry - _ for css group of modes

(add-hook 'prog-mode-hook (lambda ()
                            (make-variable-buffer-local 'company-backends)
                            (add-to-list 'company-backends 'company-gtags t)
                            (add-to-list 'company-backends 'company-etags t)
                            (add-to-list 'company-backends 'company-keywords)
                            (abbrev-mode -1)
                            (linum-mode 1)
                            (modify-syntax-entry ?_ "w" prog-mode-syntax-table)
                            (setq-local local-abbrev-table nil)
                            ))

(add-hook 'python-mode-hook (lambda ()
                              (setq-local tab-width 4)
                              (setq python-indent-offset 4)
                              ))
(add-hook 'conf-space-mode-hook (lambda () (setq-local tab-width 2)))
(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (setq mode-name "elisp")
    (setq-local c-basic-offset 2)))

(setq c-basic-offset 'set-from-style)

(bind-key "C-c C-r" #'air-revert-buffer-noconfirm python-mode-map)

(dolist (i my/devel-keymaps)
  (bind-key "C-c d" #'dash-at-point i)
  (bind-key "C-c e" #'dash-at-point-with-docset i))

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
    (bind-key "C-s"     #'counsel-grep                  ledger-mode-map)
    ))

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

(add-hook 'mmm-mode-hook
          (lambda ()
            (set-face-background 'mmm-default-submode-face nil)))

(provide 'my-devel)
