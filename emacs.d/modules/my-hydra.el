(use-package hydra
  :bind (("C-c I" . hydra-image/body)
         ("C-c T" . hydra-tool/body)
         ("C-c t" . hydra-btoggle/body)
         ("C-c C" . hydra-clock/body)
         ("C-c e" . hydra-erc/body)
         ("C-c n" . hydra-navigate-to/body)
         ("C-c g" . hydra-git/body)
         ("C-c p" . hydra-projectile/body)
         ("C-c q" . hydra-query/body)
         ("C-c b" . hydra-browser/body)
         ("C-c s" . hydra-spelling/body)
         ("C-c v" . hydra-org-roam/body)
         :map evil-normal-state-map
         (",b" . hydra-buffer/body)
         (",w" . hydra-browser/body)
         (",i" . hydra-snippet/body)
         (",p" . hydra-project/body)
         ("C-c f" . hydra-flycheck/body)
         ("C-c o" . hydra-org/body)
         ("C-c v" . hydra-org-roam/body)
         (",g" . hydra-git/body)
         :map dired-mode-map
         ("C-c d" . hydra-dired/body)
         ;; ("C-c u" . hydra-upload/body)
         ))

(use-package major-mode-hydra
  :after hydra
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
    (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str)))

(pretty-hydra-define hydra-btoggle
  (:hint nil :color amaranth :quit-key "q" :title (with-faicon "toggle-on" "Toggle" 1 -0.05))
  ("Basic"
   (("a" abbrev-mode "abbrev" :toggle t)
    ("h" global-hungry-delete-mode "hungry delete" :toggle t)
    ("s" smart-quotes-mode "smart quotes toggle" :toggle t))
   "Coding"
   (("d" my/dtrt-indent-mode-toggle "Toggle dtrt-indent-mode" :toggle t)
    ("e" electric-operator-mode "electric operator" :toggle t)
    ("F" flyspell-mode "flyspell" :toggle t)
    ("f" flycheck-mode "flycheck" :toggle t)
    ("l" lsp-mode "lsp" :toggle t)
    ("p" my/prettier-mode "Prettier" :toggle t))
   ;; ("s" smartparens-mode "smartparens" :toggle t))
   "UI"
   (("i" ivy-rich-local-mode "ivy-rich local" :toggle t))))

(pretty-hydra-define hydra-clock
  (:hint nil :color teal :quit-key "q" :title (with-faicon "clock-o" "Clock" 1 -0.05))
  ("Action"
   (("c" org-clock-cancel "cancel")
    ("d" org-clock-display "display")
    ("e" org-clock-modify-effort-estimate "effort")
    ("i" org-clock-in "in")
    ("j" org-clock-goto "jump")
    ("o" org-clock-out "out")
    ("p" org-pomodoro "pomodoro")
    ("r" org-clock-report "report"))))

(pretty-hydra-define hydra-erc
  (:hint nil :color teal :quit-key "q" :title (with-faicon "comments-o" "ERC" 1 -0.05))
  ("Action"
   (
    ;; ("b" my/erc-browse-last-url "browse last url")
    ("c" my/erc-start-or-switch "connect")
    ("d" erc-quit-server "disconnect")
    ("j" erc-join-channel "join")
    ("n" erc-channel-names "names")
    ("o" my/erc-get-ops "ops")
    ("u" my/erc-count-users "users")
    ("r" my/erc-reset-track-mode "reset track mode"))))

(pretty-hydra-define hydra-flycheck
  (:hint nil :color teal :quit-key "q" :title (with-faicon "plane" "Flycheck" 1 -0.05))
  ("Checker"
   (("?" flycheck-describe-checker "describe")
    ("d" flycheck-disable-checker "disable")
    ("m" flycheck-mode "mode")
    ("s" flycheck-select-checker "select"))
   "Errors"
   (("<" flycheck-previous-error "previous" :color pink)
    (">" flycheck-next-error "next" :color pink)
    ("f" flycheck-buffer "check")
    ("l" flycheck-list-errors "list"))
   "Other"
   (("M" flycheck-manual "manual")
    ("v" flycheck-verify-setup "verify setup"))))

(pretty-hydra-define hydra-navigate-to
  (:hint nil :color teal :quit-key "q" :title (with-faicon "file-text-o" "Navigate To" 1 -0.05))
  ("Agenda"
   (("ac" (find-file my/org-contacts-file-path) "contacts"))
   ;; ("ao" (find-file "~/.personal/agenda/organizer.org") "organizer")
   ;; ("ap" (find-file "~/.personal/agenda/people.org") "people")
   ;; ("ar" (find-file "~/.personal/agenda/routine.org") "routine")
   "Config"
   (
    ("cs" (find-file "~/.authinfo.gpg") "authinfo.gpg")
    ("cE" (find-file "~/dotenv/editorconfig") "editorconfig")
    ("ce" (find-file "~/dotenv/emacs.d/") "emacs.d")
    ("cl" (find-file "~/dotenv/emacs.d/lisp/") "emacs.d/lisp")
    ("cm" (find-file "~/dotenv/emacs.d/modules/") "emacs.d/modules")
    ("ci" (find-file "~/dotenv/emacs.d/init.el") "init.el")
    ("cg" (find-file "~/dotenv/gitconfig") "gitconfig")
    ("cS" (find-file "~/.mbsyncrc") "mbsync")
    ("cs" (find-file "~/dotenv/shells/") "shells")
    )
   "Ledger"
   (("lp" (find-file my/ledger-private-path) "personal")
    ("lc" (find-file my/ledger-company-dir-path) "company dir")
    ("lr" (find-file my/ledger-company-report-path) "company report"))
   "Notes"
   (("na" (find-file my/affirmations-file-path) "affirmations")
    ("nb" (find-file my/path-coaching-brainstorm) "brainstorm")
    ("ne" (find-file my/path-health-exercises-path) "exercises")
    ("ng" (find-file my/path-coaching-goals-details) "goals details")
    ("nj" (find-file my/japanese-file-path) "japanese")
    ("nn" (find-file my/notes-file-path) "notes")
    ("nr" (find-file my/org-review-file-path) "reviews")
    ("nw" (find-file my/path-coaching-wheel-of-life) "wheel of life"))
   "Perspective"
   (("pl" (lambda () (interactive ) (persp-state-restore persp-state-default-file)) "perspective load")
    ("ps" (persp-state-save) "perspective save"))
   "Other"
   (("ob" (find-file my/org-media-file-path) "media"))
   ;; ("ol" (find-file "~/.personal/other/long-goals.org") "long-terms goals")
   ;; ("om" (find-file "~/.personal/other/movies.org"))
   ;; ("op" (find-file "~/.personal/other/purchases.org") "purchase")
   ;; ("os" (find-file "~/.personal/other/short-goals.org") "short-terms goals")
   ;; ("ou" (find-file "~/.personal/other/usb.org") "usb")
   ;; ("oL" (find-file "~/.personal/other/learning.org") "learning")
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

(pretty-hydra-define hydra-ledger
  (:hint nil :color teal :quit-key "q" :title (with-faicon "usd" "Ledger" 1 -0.05))
  ("Action"
   (("b" leadger-add-transaction "add")
    ("c" ledger-mode-clean-buffer "clear")
    ("i" ledger-copy-transaction-at-point "copy")
    ("s" ledger-delete-current-transaction "delete")
    ("r" ledger-report "report"))))

(pretty-hydra-define hydra-magit
  (:hint nil :color teal :quit-key "q" :title (with-alltheicon "git" "Magit" 1 -0.05))
  ("Action"
   (("b" magit-blame "blame")
    ("l" magit-log-buffer-file "commit log (current file)")
    ("L" magit-log-current "commit log (project)")
    ("s" magit-status "status")
    )))

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

(pretty-hydra-define hydra-merge
  (:hint nil :color pink :quit-key "q" :title (with-alltheicon "git" "Merge" 1 -0.05))
  ("Move"
   (("n" smerge-next "next")
    ("p" smerge-prev "previous"))
   "Keep"
   (("l" smerge-keep-lower "lower")
    ("u" smerge-keep-upper "upper"))
   "Diff"
   (("R" smerge-refine "redefine"))))

(pretty-hydra-define hydra-projectile
  (:hint nil :color teal :quit-key "q" :title (with-faicon "rocket" "Projectile" 1 -0.05))
  ("Buffers"
   (("b" counsel-projectile-switch-to-buffer "list")
    ("k" projectile-kill-buffers "kill all")
    ("S" projectile-save-project-buffers "save all"))
   "Find"
   (("d" counsel-projectile-find-dir "directory")
    ("D" projectile-dired "root")
    ("f" counsel-projectile-find-file "file")
    ("F" projectile-find-file-other-window "file other window")
    ("p" counsel-projectile-switch-project "project")
    ("o" projectile-find-other-file "other file")
    ("t" projectile-find-tag "tag")
    )
   "Project"
   (("a" my/projectile-add-known-project "add")
    ("r" projectile-remove-known-project "remove"))
   "Other"
   (("i" projectile-invalidate-cache "reset cache"))
   "Search"
   (("r" (my/func-call '(projectile-invalidate-cache nil) 'projectile-replace '(save-some-buffers t)) "replace")
    ("R" (my/func-call '(projectile-invalidate-cache nil) 'projectile-replace-regexp '(save-some-buffers t)) "regexp replace")
    ("s" counsel-rg "search"))))

(pretty-hydra-define hydra-query
  (:hint nil :color teal :quit-key "q" :title (with-faicon "search" "Engine-Mode" 1 -0.05))
  ("Query"
   (("a" engine/search-amazon "amazon")
    ("d" engine/search-duckduckgo "duckduckgo")
    ("g" engine/search-github "github")
    ("i" engine/search-google-images "google images")
    ("m" engine/search-google-maps "google maps")
    ("s" engine/search-stack-overflow "stack overflow")
    ("w" engine/search-wikipedia "wikipedia")
    ("y" engine/search-youtube "youtube"))))

(pretty-hydra-define hydra-spelling
  (:hint nil :color teal :quit-key "q" :title (with-faicon "magic" "Spelling" 1 -0.05))
  ("Checker"
   (("c" langtool-correct-buffer "correction")
    ("C" langtool-check-done "clear")
    ("d" ispell-change-dictionary "dictionary")
    ("s" (lambda () (interactive) (flyspell-mode 'toggle)) "flyspell toggle")
    ("l" my/lang-toggle "language switch")
    ("w" wiki-summary "wiki"))
   "Japanese"
   (("k" japanese-katakana-region "katakana charset")
    ("h" japanese-hiragana-region "hiragana charset"))
   ;; TODO add encoding switch
   "Errors"
   (("<" flyspell-correct-previous "previous" :color pink)
    (">" flyspell-correct-next "next" :color pink)
    ("x" langtool-check-buffer "langtool check")
    ("q" langtool-check-done "langtool done")
    ("a" artbollocks-mode "artbollocks"))))

;; (pretty-hydra-define hydra-tex
;;   (:hint nil :color teal :quit-key "q" :title (with-fileicon "tex" "LaTeX" 1 -0.05))
;;   ("Action"
;;     (("g" reftex-goto-label "goto")
;;       ("r" reftex-query-replace-document "replace")
;;       ("s" counsel-rg "search")
;;       ("t" reftex-toc "table of content"))))

(pretty-hydra-define hydra-tool
  (:hint nil :color teal :quit-key "q" :title (with-faicon "briefcase" "Tool" 1 -0.05))
  ("Format"
   (("x" my/reformat-xml "format XML"))
   "Remove"
   (("c" jarfar/comments-delete-buffer "remove comments from buffer")
    ("l" jarfar/remove-empty-lines "remove empty lines"))))

;; (pretty-hydra-define hydra-upload
;;   (:hint nil :color teal :quit-key "q" :title (with-faicon "cloud-upload" "Upload" 1 -0.05))
;;   ("Action"
;;    (("b" webpaste-paste-buffe "buffer")
;;     ("i" imgbb-upload "image")
;;     ("r" webpaste-paste-region "region"))))

(pretty-hydra-define hydra-snippet
  (:hint nil :color teal :quit-key "q" :title (with-faicon "file" "Snippets" 1 -0.05))
  ("Snippet"
   (("s" yas-insert-snippet "insert")
    ("n" yas-new-snippet "new")
    ("e" yas-visit-snippet-file "edit")
    ("r" yas-reload-all "reload"))))

(pretty-hydra-define hydra-browser
  (:hint nil :color teal :quit-key "q" :title (with-faicon "globe" "Browser" 1 -0.05))
  ("Go to"
   ;; ("S" my/w3m-search-frame "search in frame" :exit t)
   (("G" w3m-goto-url-new-session "go to")
    ("g" my/w3m-goto-frame "go to in frame"))
   "Open"
   (("w" my/w3m-open-frame "open browser in frame")
    ("W" my/w3m-open-other-window "open browser"))
   "Search"
   (("s" my/w3m-search-frame "search"))
   ;; ("s" my/w3m-search-new-session "search" :exit t)
   ))

(pretty-hydra-define hydra-dired
  (:hint nil :color teal :quit-key "q" :title (with-faicon "folder" "Dired" 1 -0.05))
  ("Actions"
   (("c" farynaio/dired-shell-command "run command")
    ("g" magit-status "magit status"))))

(pretty-hydra-define hydra-buffer
  (:hint nil :color teal :quit-key "q" :title (with-faicon "align-justify" "Buffer" 1 -0.05))
  ("Actions"
   (("i" ibuffer "ibuffer")
    ("k" my/kill-all-buffers-except-toolkit))))

(pretty-hydra-define hydra-org
  (:hint nil :color teal :quit-key "q" :title (with-fileicon "org" "Org" 1 -0.05))
  ("Action"
   (("t" org-toggle-timestamp-type "timestamp toggle" :toggle t)
    ("l" org-link-archive "link archive"))
   "Navigation"
   (("s" counsel-org-goto "goto heading")
    ("a" counsel-org-file "browse attachments"))))

(pretty-hydra-define hydra-org-roam
  (:hint nil :color teal :quit-key "q" :title (with-fileicon "org" "Org" 1 -0.05))
  ("Action"
   (("r" org-roam "org-roam")
    ("k" org-roam "Toggle sidebar")
    ("l" org-roam-insert "Insert")
    ("j" org-roam-dailies-find-date "Journal")
    ("J" farynaio/org-roam-dailies-find-date-other-window "Journal other window")
    ("f" org-roam-find-file "Find file")
    ("F" jarfar/org-roam-find-file-other-window "Find file")
    ("b" org-roam-switch-to-buffer "Switch buffer")
    ("d" org-roam-find-directory "Find dir"))))

(pretty-hydra-define hydra-elfeed-search
  (:hint nil :color teal :quit-key "q" :title (with-faicon "rss" "RSS" 1 -0.05))
  ("Filter"
   (("b" (elfeed-search-set-filter "+business") "Show Business")
    ("k" (elfeed-search-set-filter "+marketing") "Show Marketing")
    ("e" (elfeed-search-set-filter "+entr") "Show Entr")
    ("s" (elfeed-search-set-filter "+startup") "Show Startup")
    ("h" (elfeed-search-set-filter "+growth") "Show Growth Hacking")
    ("a" (elfeed-search-set-filter "+saas") "Show SaaS")
    ("o" (elfeed-search-set-filter "+seo") "Show SEO")
    ("g" (elfeed-search-set-filter "+blog") "Show Blogging")
    ("c" (elfeed-search-set-filter "+copy") "Show Copywriting")
    ("f" (elfeed-search-set-filter "+finances") "Show Finances")
    ("m" (elfeed-search-set-filter "+social") "Show Social Media")
    ("y" (elfeed-search-set-filter "+crypto") "Show Crypto")
    ("n" (elfeed-search-set-filter "+news") "Show News")
    ("l" (elfeed-search-set-filter "+ok") "Show Read Later")
    ("j" (elfeed-search-set-filter "+junk") "Show Junk"))))

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

(provide 'my-hydra)