;;; Code:

(use-package yasnippet
  ;; :disabled t
  :defer 0.3
  :diminish yas-minor-mode
  :custom
  (yas-new-snippet-default
    "# name: $2
# key: $1
# --
$0`(yas-escape-text yas-selected-text)`")
  :config
  ;; (add-hook 'prog-mode-hook (lambda () (yas-minor-mode)))
  (yas-reload-all)
  (yas-global-mode t)

  (pretty-hydra-define hydra-snippet
    (:hint nil :color teal :quit-key "q" :title (with-faicon "sticky-note" "Snippets" 1 -0.05))
    ("Snippet"
      (("s" yas-insert-snippet "insert")
        ("n" yas-new-snippet "new")
        ("e" yas-visit-snippet-file "edit")
        ("r" yas-reload-all "reload"))))

  (evil-define-key 'normal global-map
    (kbd ",i") 'hydra-snippet/body))

(use-package ivy-yasnippet
  ;; :disabled t
  :after (ivy yasnippet))

;; (require 'org-tempo)
;; (add-to-list 'org-structure-template-alist '("sh" . "src sh"))

;; https://github.com/minad/tempel
;; https://www.lysator.liu.se/~davidk/elisp/tempo-examples.html
;; https://www.lysator.liu.se/~davidk/elisp/
;; https://www.emacswiki.org/emacs/TempoMode
(use-package tempo
  :disabled t
  :straight nil
  :custom
  (tempo-interactive t)
  :config
  (require 'my-snippets-general)
  (require 'my-snippets-js)
  (require 'my-snippets-lisp)

  (evil-define-key 'insert prog-mode-map
    (kbd "M-z") (lambda ()
                  (interactive)
                  ;; (if (symbol-at-point)
                    (when (not (tempo-expand-if-complete))
                      (call-interactively #'company-tempo)
                      (tempo-expand-if-complete))))
  ;; )

  ;; (advice-add 'company-tempo :after (lambda (&rest) (tempo-expand-if-complete)))

  (defun my/tempo-insert ()
    (interactive)
    (let* ((evil-state-pre (when (boundp 'evil-state) evil-state))
            (tags (mapcar 'car (tempo-build-collection)))
            (symbol (symbol-at-point))
            (symbol (if symbol (symbol-name symbol) ""))
            (symbol (if (and (not (string-empty-p symbol)) (seq-some (lambda (i) (string-match-p (regexp-quote symbol) i)) tags)) symbol ""))
            (tag
              (funcall
                completing-read-function
                "Choose template: "
                tags
                nil
                t
                symbol))
            (inhibit-message t)
            (message-log-max nil))
      (if (eq symbol "")
        (insert tag)
        (unless (string= symbol tag)
          (insert (format " %s" tag))))
      (tempo-complete-tag)
      (when evil-state-pre
        (evil-change-state evil-state-pre))))

  (defvar my/tempo-initial-pos nil
    "Initial position in template after expansion")

  (defadvice tempo-insert (around tempo-insert-pos act)
    "Define initial position."
    (if (eq element '~)
      (setq my/tempo-initial-pos (point-marker))
      ad-do-it))

  (defadvice tempo-insert-template (around tempo-insert-template-pos act)
    "Set initial position when defined. ChristophConrad"
    (setq my/tempo-initial-pos nil)
    ad-do-it
    (if my/tempo-initial-pos
      (progn
        (put template 'no-self-insert t)
        (goto-char my/tempo-initial-pos))
      (put template 'no-self-insert nil)))

  (defadvice tempo-define-template (after no-self-insert-in-abbrevs activate)
    "Skip self-insert if template function is called by an abbrev."
    (put (intern (concat "tempo-template-" (ad-get-arg 0))) 'no-self-insert t))

  (add-hook 'text-mode-hook
    (lambda ()
      (tempo-use-tag-list 'my/tempo-general-tags)))

  (add-hook 'prog-mode-hook
    (lambda ()
      (tempo-use-tag-list 'my/tempo-general-tags)))

  (add-hook 'web-mode-hook
    (lambda ()
      (tempo-use-tag-list 'my/tempo-general-tags)
      (tempo-use-tag-list 'my/tempo-js-tags)))

  (add-hook 'rjsx-mode-hook
    (lambda ()
      (tempo-use-tag-list 'my/tempo-general-tags)
      (tempo-use-tag-list 'my/tempo-js-tags)))

  (add-hook 'js2-mode-hook
    (lambda ()
      (tempo-use-tag-list 'my/tempo-general-tags)
      (tempo-use-tag-list 'my/tempo-js-tags)))

  (add-hook 'typescript-mode-hook
    (lambda ()
      (tempo-use-tag-list 'my/tempo-general-tags)
      (tempo-use-tag-list 'my/tempo-js-tags)))

  (add-hook 'typescript-ts-base-mode-hook
    (lambda ()
      (tempo-use-tag-list 'my/tempo-general-tags)
      (tempo-use-tag-list 'my/tempo-js-tags)))

  (add-hook 'emacs-lisp-mode-hook
    (lambda ()
      (tempo-use-tag-list 'my/tempo-general-tags)
      (tempo-use-tag-list 'my/tempo-lisp-tags))))

(provide 'my-snippet)
;;; my-snippet.el ends here