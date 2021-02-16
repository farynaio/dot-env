(use-package web-mode
  :hook ((web-mode . lsp))
  :bind (:map web-mode-map
          ("C-c C-n" . web-mode-tag-end)
          ("C-c C-p" . web-mode-tag-beginning)
          ("<backtab>" . indent-relative)
          ("<f5>" . my/toggle-php-flavor-mode))
  :mode (("\\.php\\'" . web-mode)
          ("\\.phtml\\'" . web-mode)
          ("\\.tpl\\.php\\'" . web-mode)
          ;; ("\\.js\\'" . web-mode)
          ("\\.html\\.twig\\'" . web-mode)
          ("\\.hbs\\'" . web-mode)
          ("\\.ejs\\'" . web-mode)
          ("\\.html?\\'" . web-mode)
          ("\\.svg\\'" . web-mode)
          ;; ("\\.php\\'" . web-mode)
          )
  :config
  (setq
    web-mode-engines-alist '(("php" . "\\.php\\'"))
    web-mode-markup-indent-offset tab-width
    web-mode-css-indent-offset tab-width
    web-mode-code-indent-offset tab-width)

  (evil-define-key 'normal web-mode-map
    ",t" #'my/toggle-php-flavor-mode)

  (add-hook 'before-save-hook
    (lambda ()
      (when (and (fboundp 'web-beautify-html) (eq dtrt-indent-mode nil))
        (web-beautify-html)))
    0 t))

(require 'css-mode)
(eval-after-load 'css-mode
  '(progn
     (setq css-indent-offset tab-width)
     (add-hook 'css-mode-hook
       (lambda ()
         (setq-local company-backends '((company-css company-keywords company-files)))))))

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

(use-package rainbow-mode
  :hook (css-mode . rainbow-mode)
  :diminish rainbow-mode)

(use-package rainbow-delimiters)

(provide 'my-web)