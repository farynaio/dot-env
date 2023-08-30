
;;; Code:

(use-package major-mode-hydra
  :straight (:type git
             :host github
             :repo "jerrypnz/major-mode-hydra.el"
              :branch "master")
  :bind (("C-c I" . hydra-image/body)
          ;; ("C-c T" . hydra-tool/body)
          ("C-c t" . hydra-general/body)
          ;; ("C-c n" . hydra-navigate-to/body)
          ("C-c g" . hydra-git/body)
          ("C-c p" . hydra-projectile/body)
          ("C-c v" . my/hydra-common/body)
          ;; ("C-c q" . hydra-query/body) ;;
          ;; ("C-c b" . hydra-browser/body) ;;
          )
  :preface
  (defun with-alltheicon (icon str &optional height v-adjust)
    "Displays an icon from all-the-icon."
    (s-concat (all-the-icons-alltheicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-faicon (icon str &optional height v-adjust)
    "Displays an icon from Font Awesome icon."
    (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-fileicon (icon str &optional height v-adjust)
    "Displays an icon from the Atom File Icons package."
    (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-octicon (icon str &optional height v-adjust)
    "Displays an icon from the GitHub Octicons."
    (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))
  :config
  (pretty-hydra-define hydra-general
    (:hint nil :color amaranth :quit-key "q" :title (with-faicon "toggle-on" "Toggle" 1 -0.05))
    ("Toggle"
      (("ta" abbrev-mode "abbrev" :toggle t :exit t)
        ("td" global-hungry-delete-mode "hungry delete" :toggle t :exit t)
        ("tq" smart-quotes-mode "smart quotes" :toggle t :exit t)
        ("te" electric-pair-mode "electric pair" :toggle t :exit t)
        ("th" afa/header-line-path-mode "header line path" :toggle t :exit t))
      "ChatGPT"
      (("cc" chatgpt-shell "start session" :exit t)
        ("ci" chatgpt-shell-interrupt "interrupt" :exit t)
        ("cp" chatgpt-shell-proofread-region "proofread" :exit t)
        ("cs" chatgpt-shell-send-and-review-region "send and review" :exit t)
        ("ct" chatgpt-shell-save-session-transcript "save session transcript" :exit t))
      ))

  (pretty-hydra-define hydra-image
    (:hint nil :color pink :quit-key "q" :title (with-faicon "file-image-o" "Images" 1 -0.05))
    ("Action"
      (("r" image-rotate "rotate")
        ("s" image-save "save" :color teal))
      "Zoom"
      (("-" image-decrease-size "out")
        ("+" image-increase-size "in")
        ("=" image-transform-reset "reset"))))

  (pretty-hydra-define hydra-projectile
    (:hint nil :color teal :quit-key "q" :title (with-faicon "anchor" "Projectile" 1 -0.05))
    ("Buffers"
      (("b" counsel-projectile-switch-to-buffer "list")
        ("k" projectile-kill-buffers "kill all")
        ("S" projectile-save-project-buffers "save all"))
      "Find"
      (("d" counsel-projectile-find-dir "directory")
        ("D" projectile-dired "go to home dir dired")
        ("f" counsel-projectile-find-file "file")
        ("F" projectile-find-file-other-window "file other window")
        ("p" counsel-projectile-switch-project "project")
        ("o" projectile-find-other-file "other file")
        ("t" projectile-find-tag "tag"))
      "Project"
      (("a" my/projectile-add-known-project "add")
        ("m" projectile-remove-known-project "remove"))
      "Actions"
      (("i" projectile-invalidate-cache "reset cache")
        ("r" (my/func-call '(projectile-invalidate-cache nil) 'projectile-replace-regexp '(save-some-buffers t)) "regexp replace")
        ("s" counsel-rg "search"))))

  (pretty-hydra-define hydra-magit
    (:hint nil :color teal :quit-key "q" :title (with-alltheicon "git" "Magit" 1 -0.05))
    ("Action"
      (("b" magit-blame "blame")
        ("l" magit-log-buffer-file "commit log (current file)")
        ("L" magit-log-current "commit log (project)")
        ("s" magit-status "status"))))

  (pretty-hydra-define hydra-git
    (:hint nil :color teal :quit-key "q" :title (with-alltheicon "git" "Git" 1 -0.05))
    ("Action"
      (("s" magit-status "status")
        ("b" magit-branch "branch")
        ("z" magit-stash "stash")
        ("r" magit-rebase "rebase")
        ("B" magit-blame "blame")
        ("x" magit-reset "reset")
        ("c" magit-checkout "checkout")
        ("S" magit-bisect "bisect"))
      "Diff"
      (("dc" magit-diff "diff" :exit t)
        ("df" magit-diff-buffer-file "diff file" :exit t))
      "Log"
      (("ll" magit-log-all "log")
        ("lf" magit-log-buffer-file "file log"))
      "Update"
      (("f" magit-fetch "fetch")
        ("F" magit-pull "pull"))
      "Other"
      (("oi" magit-init "init")
        ("oc" magit-clone "clone"))))

  (pretty-hydra-define my/hydra-common
    ;; (:hint nil :color teal :quit-key "q" :title (with-octicon "light-bulb" "Common" 1 -0.05))
    (:hint nil :color teal :quit-key "q" :title (with-faicon "magic" "Common" 1 -0.05))
    ;; (:hint nil :color teal :quit-key "q" :title (with-faicon "wand-magic-sparkles" "Common" 1 -0.05))
    ("org-roam"
      (("j" my/org-journal-open-current-journal-file "journal")
        ("f" org-roam-node-find "find node")
        ("F" my/org-roam-node-find-other-window "find node other window")
        ("b" org-roam-switch-to-buffer "switch buffer")
        ("d" org-roam-find-directory "find dir")
        ("l" org-roam-node-insert "insert node"))
      "Commands"
      (("m" woman "man" :exit t)
        ("r" rss "RSS" :exit t)
        ("a" my/chatgpt-shell-start-new "ChatGpt shell" :exit t)
        ("k" my/kill-clear "clear kill-ring")
        ("." my/tempo-insert "insert snippet" :exit t))
      "Language"
      (("d" ispell-change-dictionary "change dictionary")
        ("s" (lambda () (interactive) (flyspell-mode 'toggle)) "flyspell toggle")
        ("w" my/lang-toggle "language switch" :exit t))
      "Spelling errors"
      (("o" artbollocks-mode "artbollocks" :toggle t)
        ("x" langtool-check-buffer "langtool check")
        ("q" langtool-check-done "langtool done")
        ("<" flyspell-correct-previous "previous")
        (">" flyspell-correct-next "next"))))

  (defun my/kill-clear ()
    "Clear kill-ring."
    (interactive)
    (setq kill-ring nil)
    (garbage-collect)
    (message "kill-ring cleared"))

  ;; (pretty-hydra-define hydra-query
  ;;   (:hint nil :color teal :quit-key "q" :title (with-faicon "search" "Engine-Mode" 1 -0.05))
  ;;   ("Query"
  ;;     (("d" engine/search-duckduckgo "duckduckgo")
  ;;     ("g" engine/search-github "github")
  ;;     ("i" engine/search-images "images")
  ;;     ("m" engine/search-maps "maps")
  ;;     ("w" engine/search-wikipedia "wikipedia"))))


  ;; (pretty-hydra-define hydra-tex
  ;;   (:hint nil :color teal :quit-key "q" :title (with-fileicon "tex" "LaTeX" 1 -0.05))
  ;;   ("Action"
  ;;     (("g" reftex-goto-label "goto")
  ;;       ("r" reftex-query-replace-document "replace")
  ;;       ("s" counsel-rg "search")
  ;;       ("t" reftex-toc "table of content"))))

  ;; (pretty-hydra-define hydra-tool
  ;;   (:hint nil :color teal :quit-key "q" :title (with-faicon "briefcase" "Tool" 1 -0.05))
  ;;   ("Format"
  ;;    (("x" my/reformat-xml "format XML"))
  ;;    "Remove"
  ;;    (("c" my/comments-delete-buffer "remove comments from buffer")
  ;;     ("l" my/remove-empty-lines "remove empty lines"))))

  ;; (pretty-hydra-define hydra-upload
  ;;   (:hint nil :color teal :quit-key "q" :title (with-faicon "cloud-upload" "Upload" 1 -0.05))
  ;;   ("Action"
  ;;    (("b" webpaste-paste-buffe "buffer")
  ;;     ("i" imgbb-upload "image")
  ;;     ("r" webpaste-paste-region "region"))))

  ;; (pretty-hydra-define hydra-php-debug
  ;;   (:hint nil :color teal :quit-key "q" :title (with-fileicon "php" "PHP" 1 -0.05))
  ;;   ("Debug"
  ;;     (("a" geben-add-current-line-to-predefined-breakpoints "add brk")
  ;;       ("s" geben "start")
  ;;       ("q" geben-end "end"))))

  ;; (defhydra hydra-tide ()
  ;;   "Tide"
  ;;   ("i" tide-organize-imports "Organize imports" :exit t)
  ;;   ("r" tide-refactor "Refactor" :exit t)
  ;;   ("f" tide-fix "Fix" :exit t)
  ;;   ("r" tide-rename-file "Rename file" :exit t)
  ;;   ("e" tide-error-at-point "Error at point" :exit t)
  ;;   ("o" tide-references "References" :exit t)
  ;;   ("d" tide-documentation-at-point "Show docs" :exit t)
  ;;   ("x" tide-restart-server "Restart server" :exit t))

  ;; (defhydra hydra-js-search ()
  ;;   "JS search"
  ;;   ("p" my/rgrep "grep" :exit t)
  ;;   ("s" tern-find-definition "find JS definition" :exit t)
  ;;   ("t" tern-find-definition-by-name "find JS definition by name" :exit t))
  ;; (define-key tern-mode-keymap [(control ?c) (control ?r)] 'tern-rename-variable)

  ;; (defhydra hydra-js-refactoring ()
  ;;   "JS refactoring"
  ;;   ("n"  hydra-js-refactoring-node/body "node" :exit t)
  ;;   ("e"  hydra-js-refactoring-extract/body "extract" :exit t)
  ;;   ("m"  hydra-js-refactoring-rename/body "rename" :exit t)
  ;;   ("r"  hydra-js-refactoring-replace/body "replace" :exit t))

  ;; (defhydra hydra-js-refactoring-node ()
  ;;   "JS refactoring node"
  ;;   ("e" js2r-expand-node-at-point "expand 'node'" :exit t)
  ;;   ("c" js2r-contract-node-at-point "contract 'node'" :exit t))

  ;; (defhydra hydra-js-refactoring-extract ()
  ;;   "JS refactoring extract"
  ;;   ("v" js2r-extract-var "var" :exit t)
  ;;   ("l" js2r-extract-let "let" :exit t)
  ;;   ("c" js2r-extract-const "const" :exit t)
  ;;   ("f" js2r-extract-function "function" :exit t)
  ;;   ("m" js2r-extract-method "method" :exit t))

  ;; (defhydra hydra-js-refactoring-rename ()
  ;;   "JS refactoring rename"
  ;;   ("v" js2r-rename-var "var" :exit t))

  ;; (defhydra hydra-js-refactoring-replace ()
  ;;   "JS refactoring replace"
  ;;   ("t" js2r-var-to-this "'var' which 'this'" :exit t))
  )

(provide 'my-hydra)
;;; my-hydra.el ends here
