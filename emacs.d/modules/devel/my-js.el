;;; Code:

(require 'js)

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
  :hook (js2-mode . lsp-deferred)
  :mode ("\\.m?js\\'" "\\.cjs\\'")
  :diminish "js2"
  :custom
  (js2-mode-show-parse-errors nil)
  (js2-mode-show-strict-warnings nil)
  :config
  (major-mode-hydra-define+ js2-mode
    (:hint nil :color amaranth :quit-key "q" :title (with-fileicon "jsx-2" "JS" 1 -0.05))
    ("Action"
      (("f" apheleia-format-buffer "prettier buffer" :exit t))))
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
          (rjsx-mode . lsp-deferred))
          ;; (rjsx-mode . my/prettier-mode))
  :commands rjsx-mode
  :bind (:map rjsx-mode-map
          ("<" . rjsx-electric-lt))
  :mode ("\\.m?jsx\\'")
  :config
  (major-mode-hydra-define+ rjsx-mode
    (:hint nil :color amaranth :quit-key "q" :title (with-fileicon "jsx-2" "JSX" 1 -0.05))
    ("Action"
      (("f" apheleia-format-buffer "prettier buffer" :exit t)))))

(use-package vue-mode
  :mode "\\.vue\\'")

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
(use-package apheleia
  :commands (my/prettier-mode)
  :diminish apheleia-mode)

;; Use binaries in node_modules
(use-package add-node-modules-path
  :hook ((js-mode . add-node-modules-path)
          (rjsx-mode . add-node-modules-path)
          (typescript-mode . add-node-modules-path)))


;; based on https://github.com/emacs-typescript/typescript.el/issues/4#issuecomment-873485004
(use-package typescript-mode
  :disabled t
  :mode ("\\.tsx?\\'" . typescript-mode)
  :hook ((typescript-mode . mmm-mode)
          (typescript-mode . lsp-deferred)
          (typescript-mode . emmet-mode)
          (typescript-mode . subword-mode))
  :custom
  (typescript-indent-level 2)
  :config
  (major-mode-hydra-define+ typescript-mode
    (:hint nil :color amaranth :quit-key "q" :title (with-fileicon "typescript" "TSX" 1 -0.05))
    ("Action"
      (("f" apheleia-format-buffer "prettier buffer" :exit t)))))

(use-package typescript-ts-mode
  :straight nil
  :mode ("\\.tsx?\\'" . typescript-ts-mode)
  :hook ((typescript-ts-mode . mmm-mode)
          (typescript-ts-mode . lsp-deferred)
          (typescript-ts-mode . emmet-mode))
  :config
  (setq auto-mode-alist (remove '("\\.tsx\\'" . tsx-ts-mode) auto-mode-alist))
  (major-mode-hydra-define+ typescript-ts-mode
    (:hint nil :color amaranth :quit-key "q" :title (with-fileicon "typescript" "Typescript" 1 -0.05))
    ("Action"
      (("f" apheleia-format-buffer "prettier buffer" :exit t)))))

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

(provide 'my-js)
;;; my-js.el ends here