(require 'cc-mode)
(require 'css-mode)
(require 'python)
(require 'js)

(use-package rainbow-mode
  :diminish rainbow-mode)

(use-package yaml-mode)

(use-package git-gutter
  :diminish git-gutter-mode
  :config
  (progn
    (global-git-gutter-mode +1)))

;; (use-package git-timemachine)

(require 'dash-at-point)

;; quickrun
;; expand-region.el
;; restclient.el
;; php-auto-yasnippets
;; js2-mode

(use-package company
  :diminish company-mode

  :config
  (progn
    (setq company-show-numbers t)
    (setq company-minimum-prefix-length 0)
    (setq company-begin-commands '(c-scope-operator c-electric-colon c-electric-lt-gt c-electric-slash))))

(use-package company-statistics)
(use-package company-web)
(use-package company-php)
(use-package dockerfile-mode
  :config (add-to-list 'auto-mode-alist '("^Dockerfile" . dockerfile-mode)))

(electric-pair-mode 1)
(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\{ . ?\})
                            (?\( . ?\))))
(setq electric-pair-text-pairs electric-pair-pairs)

(use-package yasnippet
  :config
  (progn
    (yas-global-mode t)
    (diminish yas-minor-mode)
    (diminish yas/minor-mode)
    (diminish yas-global-mode)
    (diminish yas/global-mode)))

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
    (setq web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-code-indent-offset 2)))

(add-hook 'php-mode-hook #'company-mode)

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
    (bind-key "v"   #'evil-visual-char        magit-hunk-section-map)))

(global-set-key (kbd "C-c p") #'git-gutter:previous-hunk)
(global-set-key (kbd "C-c n") #'git-gutter:next-hunk)
(global-set-key (kbd "C-x g s") 'magit-status)

(bind-key "C-c l" 'org-store-link)
(bind-key "C-c L" 'org-insert-link-global)
(bind-key "C-c O" 'org-open-at-point-global)

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

(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js-mode))

(setq my/devel-keymaps (list emacs-lisp-mode-map web-mode-map sql-mode-map lisp-mode-map lisp-interaction-mode-map scss-mode-map java-mode-map php-mode-map python-mode-map))
(setq devel-buffers '("js" "jsx" "vim" "json" "java" "inc" "phtml" "php" "css" "scss" "html" "md" "xml" "rb" "el" "py"))

(add-hook 'find-file-hook
  (lambda ()
    (let* ((found nil)
            (buf-name (file-name-extension buffer-file-name) ))
	    (dolist (i devel-buffers)
	      (if (string= buf-name i)
          (progn
		        (hl-line-mode 1)
            (hl-todo-mode 1)
            (auto-highlight-symbol-mode 1)
            (rainbow-mode 1)
            (setq c-basic-offset 2))
          )))))

;; TODO modify-syntax-entry - _ for css group of modes

(add-hook 'prog-mode-hook (lambda () (modify-syntax-entry ?_ "w" prog-mode-syntax-table)))

(add-hook 'python-mode-hook (lambda () (setq tab-width 4)))

(setq python-indent-offset 4)
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
    (unbind-key "<tab>" ledger-mode-map)
    (bind-key "C-c C-c" 'ledger-post-align-dwim ledger-mode-map)

    (define-derived-mode my/ledger-mode ledger-mode "ledger"
      "Superior major ledger mode"
      (hl-line-mode 1)
      (add-hook 'after-save-hook #'ledger-post-align-dwim nil t))

    ;; (unbind-key "<TAB>" my/ledger-mode-map)
    (add-to-list 'auto-mode-alist '("\\.dat\\'" . my/ledger-mode))))

(provide 'my-devel)
