;;; Code:

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
    ;; (prettier-mode 1)
    ;; (prettier-mode -1)
    (apheleia-mode 1)
    (apheleia-mode -1)))

(use-package prettier
  :disabled t
  :commands prettier-prettify
  :hook ((rjsx-mode . (lambda ()
   (when (string-match "\.tsx?$" buffer-file-name)
     (setq-local prettier-parsers '(typescript)))))))

;; Prettier support
(use-package apheleia
  :commands (rjsx-mode js2-mode js-mode)
  ;; :commands (my/prettier-mode apheleia-format-buffer)
  :diminish apheleia-mode
  :config
  (add-to-list 'apheleia-mode-alist '(rjsx-mode . prettier)))

(defun my/prettier-format-buffer ()
  "Prettify buffer using Prettier if available"
  (interactive)
  (if (executable-find "prettier")
    (progn
      (call-interactively 'apheleia-format-buffer "prettier")
      (message "Buffer prettified"))
    (message "prettier not installed")))

  (defun my/eglot-organize-imports-ts ()
    (interactive)
    (eglot-code-action-organize-imports-ts (goto-char (point-min))))

(use-package js
  :straight nil
  :custom
  (js-indent-level tab-width)
  (flycheck-disabled-checkers '(javascript-jshint javascript-jscs))
  (js-chain-indent t)
  (js-indent-align-list-continuation nil)
  ;; :config
  ;; (flycheck-add-mode 'javascript-eslint 'js-mode)
  ;; (add-hook 'js-mode-hook
  ;;   (lambda () (unless (eq major-mode 'json-mode) (lsp))))
  )

;; (use-package js-ts-mode
;;   :straight nil
;;   :mode ("\\.m?js\\'" "\\.cjs\\'")
;;   :hook (js-ts-mode . lsp-deferred))

(use-package js2-mode
  ;; :hook (js2-mode . lsp-deferred)
  :hook (js2-mode . eglot-ensure)
  :mode ("\\.m?js\\'" "\\.cjs\\'")
  :diminish "js2"
  :custom
  (js2-mode-show-parse-errors nil)
  (js2-mode-show-strict-warnings nil)
  :config
  (major-mode-hydra-define+ js2-mode
    (:hint nil :color amaranth :quit-key "q" :title (with-fileicon "jsx-2" "JS" 1 -0.05))
    ("Action"
      (("f" my/prettier-format-buffer "prettier buffer" :exit t)
        ("o" my/eglot-organize-imports-ts "organize imports" :exit t))))
  ;; Use js2-mode for Node scripts
  ;; (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))
  )

;; (use-package js2-refactor
;;   :bind (:map js2-mode-map
;;               ("C-k" . js2r-kill)
;;               ("M-." . nil))
;;   :hook ((js2-mode . js2-refactor-mode)
;;          (js2-mode . (lambda ()
;;                        (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))
;;   :config (js2r-add-keybindings-with-prefix "C-c C-r"))

(use-package json-mode
  :hook ((json-mode . origami-mode))
  :mode ("\\.json\\'"))

  ;; (flycheck-add-mode 'javascript-eslint 'typescript-mode))

(use-package rjsx-mode
  :hook ((rjsx-mode . emmet-mode)
          (rjsx-mode . mmm-mode)
          ;; (rjsx-mode . lsp-deferred)
          (rjsx-mode . eglot-ensure)
          ;; (rjsx-mode .
          ;;   (lambda ()
          ;;     (setq-local company-backends '((:separate company-tempo company-capf company-files company-keywords company-dabbrev-code))))
          ;;     ;; (setq-local apheleia-formatter "prettier")))
          )
          ;; (rjsx-mode . my/prettier-mode))
  :commands rjsx-mode
  :bind (:map rjsx-mode-map
          ("<" . rjsx-electric-lt))
  :mode ("\\.m?jsx\\'" "\\.tsx?\\'")
  :config
  (major-mode-hydra-define+ rjsx-mode
    (:hint nil :color amaranth :quit-key "q" :title (with-fileicon "jsx-2" "JSX" 1 -0.05))
    ("Action"
      (("f" my/prettier-format-buffer "prettier buffer" :exit t)
        ("o" my/eglot-organize-imports-ts "organize imports" :exit t)))))

(use-package vue-mode
  :mode "\\.vue\\'")

(add-hook 'mmm-mode-hook
  (lambda () (set-face-background 'mmm-default-submode-face nil)))

;; Use binaries in node_modules
(use-package add-node-modules-path
  :hook ((js-mode . add-node-modules-path)
          (rjsx-mode . add-node-modules-path)
          (typescript-mode . add-node-modules-path)))

;; NOTE not maintaned
;; based on https://github.com/emacs-typescript/typescript.el/issues/4#issuecomment-873485004
(use-package typescript-mode
  ;; :disabled t
  ;; :mode ("\\.tsx?\\'" . typescript-mode)
  :hook ((typescript-mode . mmm-mode)
          ;; (typescript-mode . lsp-deferred)
          (typescript-mode . eglot-ensure)
          (typescript-mode . emmet-mode)
          (typescript-mode . apheleia-mode)
          (typescript-mode . subword-mode))
  :custom
  (typescript-indent-level 2)
  :config
  (major-mode-hydra-define+ typescript-mode
    (:hint nil :color amaranth :quit-key "q" :title (with-fileicon "typescript" "TSX" 1 -0.05))
    ("Action"
      (("f" my/prettier-format-buffer "prettier buffer" :exit t)
        ("o" my/eglot-organize-imports-ts "organize imports" :exit t))))
  (setq auto-mode-alist (delete '("\\.tsx?\\'" . typescript-mode) auto-mode-alist)))

(use-package typescript-ts-mode
  :disabled t
  :straight nil
  :mode ("\\.tsx?\\'" . typescript-ts-mode)
  :hook ((typescript-ts-mode . mmm-mode)
          ;; (typescript-ts-mode . lsp-deferred)
          (typescript-ts-mode . eglot-ensure)
          (typescript-ts-mode . emmet-mode)
          ;; (typescript-ts-mode . (lambda () (flymake-eslint-enable)))
          )
  :config
  (setq auto-mode-alist (remove '("\\.tsx\\'" . tsx-ts-mode) auto-mode-alist))
  (major-mode-hydra-define+ typescript-ts-mode
    (:hint nil :color amaranth :quit-key "q" :title (with-fileicon "typescript" "Typescript" 1 -0.05))
    ("Action"
      (("f" my/prettier-format-buffer "prettier buffer" :exit t)
        ("o" my/eglot-organize-imports-ts "organize imports" :exit t)))))

;; (defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
;;   "Workaround 'sgml-mode' and follow airbnb component style."
;;   (save-match-data
;;      (save-excursion
;;        (goto-char (line-beginning-position))
;;        (when (looking-at "^\\( +\\)\/?> *$")
;;          (let ((empty-spaces (match-string 1)))
;;            (while (search-forward empty-spaces      (line-end-position) t)
;;             (replace-match (make-string (- (length empty-spaces) sgml-basic-offset)))))))))

(use-package prisma-mode
  :mode "\\.prisma\\'"
  :straight (:type git
              :host github
              :repo "pimeys/emacs-prisma-mode"
              :branch "main"))

(use-package json-reformat)

(provide 'my-js)
;;; my-js.el ends here