(use-package rainbow-mode
  :hook (css-mode . rainbow-mode)
  :diminish rainbow-mode)

(use-package web-mode
  :hook (web-mode . rainbow-mode)
  :bind (:map web-mode-map
          ("C-c C-n" . web-mode-tag-end)
          ("C-c C-p" . web-mode-tag-beginning)
          ("<backtab>" . indent-relative)
          ("<f5>" . my/toggle-php-flavor-mode))
  :mode ("\\.php\\'"
          "\\.phtml\\'"
          "\\.tpl\\.php\\'"
          "\\.html\\.twig\\'"
          "\\.hbs\\'"
          "\\.ejs\\'"
          "\\.html?\\'"
          "\\.njk\\'"
          "\\.svg\\'")
  :custom
  (web-mode-engines-alist '(("php" . "\\.php\\'")))
  (web-mode-markup-indent-offset tab-width)
  (web-mode-css-indent-offset tab-width)
  (web-mode-code-indent-offset tab-width)
  (web-mode-attr-indent-offset tab-width)
  (web-mode-block-padding tab-width)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-html-entities-fontification nil)
  (web-mode-enable-block-face nil)
  (web-mode-enable-comment-annotation nil)
  (web-mode-enable-comment-interpolation nil)
  (web-mode-enable-control-block-indentation nil)
  (web-mode-enable-css-colorization nil)
  (web-mode-enable-current-column-highlight nil)
  (web-mode-enable-current-element-highlight nil)
  (web-mode-enable-element-content-fontification nil)
  (web-mode-enable-heredoc-fontification nil)
  (web-mode-enable-inlays nil)
  (web-mode-enable-optional-tags nil)
  (web-mode-enable-part-face nil)
  (web-mode-enable-sexp-functions nil)
  (web-mode-enable-sql-detection nil)
  (web-mode-enable-string-interpolation nil)
  (web-mode-enable-whitespace-fontification nil)
  (web-mode-enable-auto-expanding nil)
  (web-mode-enable-auto-indentation nil)
  (web-mode-enable-auto-closing nil)
  (web-mode-enable-auto-opening nil)
  (web-mode-enable-auto-pairing nil)
  (web-mode-enable-auto-quoting nil)
  :config
  (evil-define-key 'normal web-mode-map
    (kbd ",t") #'my/toggle-php-flavor-mode)

  (add-hook 'before-save-hook
    (lambda ()
      (when (and (fboundp 'web-beautify-html) (eq dtrt-indent-mode nil))
        (web-beautify-html)))
    0 t))

(use-package css-mode
  :straight nil
  :mode "\\.s?css\\'"
  :custom
  (css-indent-offset tab-width)
  :config
  (add-hook 'css-mode-hook
    (lambda ()
      (setq-local company-backends '((company-css company-capf company-keywords company-files))))))

(use-package web-beautify
  :commands web-beautify-js web-beautify-css web-beautify-html)

;; debugger
;; (use-package realgud)

(use-package geben
  :hook (geben-mode . evil-emacs-state))

(use-package jade-mode
  :hook (jade-mode . symbol-overlay-mode)
  :mode "\\.jade\\'")

(use-package emmet-mode
  :diminish emmet-mode
  :hook (sgml-mode js-mode web-mode)
  :config
  (setq
    emmet-self-closing-tag-style " /"
    emmet-expand-jsx-className? t))

(provide 'my-web)