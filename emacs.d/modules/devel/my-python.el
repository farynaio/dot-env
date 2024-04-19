;;; Code:

(require 'comint)

;;; TODO install Python formatter
;; https://github.com/pythonic-emacs/blacken

(setq gud-pdb-command-name "python3 -m pdb ")

(use-package python
  :straight nil
  :config
  (evil-make-overriding-map inferior-python-mode-map 'motion)
  (evil-make-overriding-map inferior-python-mode-map 'normal)
  (bind-key "C-d" 'evil-scroll-down inferior-python-mode-map)

  (defun my/python-setup ()
    (setq-local tab-width 4)
    (setq-local python-indent-offset 4))

  (add-hook 'python-mode-hook #'my/python-setup))

;; (add-to-list 'comint-output-filter-functions 'python-pdbtrack-comint-output-filter-function)
;; (add-to-list 'comint-preoutput-filter-functions  'python-pdbtrack-comint-output-filter-function)

;; On Debian it requires python3-venv apt package, than run elpy-rpc-reinstall-virtualenv
(use-package elpy
  :disabled t
  :hook (python-mode . elpy-enable)
  :bind (:map elpy-mode-map
          ("C-c C-l" . elpy-occur-definitions)
          ("C-c C-e" . elpy-multiedit-python-symbol-at-point)
          ("C-c C-r f" . elpy-format-code)
          ("C-c C-r r" . elpy-refactor))
  :custom
  (elpy-shell-echo-output nil)
  ;; (elpy-rpc-backend "jedi")
  (elpy-rpc-python-command "python3")
  (elpy-rpc-timeout 2)
  ;; (python-shell-interpreter "ipython")
  ;; (python-shell-interpreter-args "-i --simple-prompt")
  :config
  (evil-define-key '(normal motion visual) elpy-mode-map
    (kbd "M-.") #'xref-find-definitions
    (kbd "M-,") #'xref-pop-marker-stack)

  (setq
    elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules)
    elpy-modules (delq 'elpy-module-flymake elpy-modules))

  ;; (setq
  ;; python-shell-interpreter "python"
  ;; python-shell-interpreter-args "-i")

  ;; (elpy-enable)
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

;; (use-package guess-style
;; :config
;; (progn
;; (add-hook 'python-mode-hook 'guess-style-guess-tabs-mode)))

(provide 'my-python)
;;; my-python.el ends here