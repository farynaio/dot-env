(use-package rainbow-mode
  :diminish rainbow-mode)

(use-package git-gutter
  :diminish git-gutter-mode
  :config
  (progn
    (global-git-gutter-mode +1)))

(require 'dash-at-point)

;; quickrun
;; expand-region.el
;; restclient.el
;; php-auto-yasnippets


(use-package company)
(use-package company-php)

(use-package smartparens)

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
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))))

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


(add-to-list 'auto-mode-alist '("\\.jsx$" . js-mode))

(setq my/devel-keymaps (list emacs-lisp-mode-map web-mode-map sql-mode-map lisp-mode-map lisp-interaction-mode-map))
(setq devel-buffers '("js" "jsx" "vim" "json" "java" "inc" "phtml" "php" "css" "scss" "html" "md" "xml" "rb" "el"))

(add-hook 'find-file-hook
  (lambda ()
    (let* ((found nil)
            (buf-name (file-name-extension buffer-file-name) ))
	    (dolist (i devel-buffers)
	      (when (string= buf-name i)
          (hl-line-mode)
          (hl-todo-mode)
          (auto-highlight-symbol-mode)
          (rainbow-mode)
          (smartparens-mode)
          (setq found t)))
        (when (not found)
          ))))

(dolist (i my/devel-keymaps)
  (bind-key "C-c d" #'dash-at-point i)
  (bind-key "C-c e" #'dash-at-point-with-docset i))

(provide 'my-devel)
