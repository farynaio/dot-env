;;; Code:

;; (require 'org-tempo)
;; (add-to-list 'org-structure-template-alist '("sh" . "src sh"))

;; https://github.com/minad/tempel
;; https://www.lysator.liu.se/~davidk/elisp/tempo-examples.html
;; https://www.lysator.liu.se/~davidk/elisp/
;; https://www.emacswiki.org/emacs/TempoMode
(use-package tempo
  :straight nil
  :custom
  (tempo-interactive t)
  :config
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

  (defvar my/tempo-general-tags nil
    "Tags for all modes.")

  (defvar my/tempo-lisp-tags nil
    "Tags for lisp mode.")

  (defvar my/tempo-js-tags nil
    "Tags for JS mode.")

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

  (add-hook 'emacs-lisp-mode-hook
    (lambda ()
      (tempo-use-tag-list 'my/tempo-general-tags)
      (tempo-use-tag-list 'my/tempo-lisp-tags)))

  (add-hook 'rjsx-mode-hook
    (lambda ()
      (tempo-use-tag-list 'my/tempo-general-tags)
      (tempo-use-tag-list 'my/tempo-js-tags))
    )

  (add-hook 'typescript-mode-hook
    (lambda ()
      (tempo-use-tag-list 'my/tempo-general-tags)
      (tempo-use-tag-list 'my/tempo-js-tags)))

  (tempo-define-template "file-vars"
    '("-*- " ~ " -*-")
    "filev"
    "Insert file variables block"
    'my/tempo-general-tags)

  (tempo-define-template "todo-tag"
    '(";; TODO " ~)
    "todo"
    "Insert TODO block"
    'my/tempo-lisp-tags)

  (tempo-define-template "console-log-tag"
    '("console.log(" ~ ")")
    "console"
    "Insert console.log"
    'my/tempo-general-tags)

  (tempo-define-template "sless"
    '("const " (p "name: " name) " = () => {" n>
       "return (" n>
       ~ n>
       ")" > n>
       "}" > n n
       "export default " (s name)
       )
    "sless"
    "Insert stateless component"
    'my/tempo-js-tags)

  (tempo-define-template "=>"
    '("(" ~ ") => ")
    "=>"
    "Insert arrow function"
    'my/tempo-js-tags)

  (tempo-define-template "exportf"
    '("export default function " (p "name: " name) "() {" n>
       "return (" n>
       ~ n>
       ")" > n>
       "}" > n
       )
    "exportf"
    "Insert 'export default function'"
    'my/tempo-js-tags)

  (tempo-define-template "exportc"
    '("export const " ~ " = " >)
    "exportc"
    "Insert 'export const'"
    'my/tempo-js-tags)

  (tempo-define-template "todo-tag"
    '("// TODO " ~)
    "todo"
    "Insert TODO block"
    'my/tempo-js-tags)

  (tempo-define-template "cn-tag"
    '("className=\"" ~ "\"")
    "cn"
    "Insert className=\"\""
    'my/tempo-js-tags)

  )

(provide 'my-snippet)
;;; my-snippet.el ends here