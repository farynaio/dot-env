(use-package hydra
  :bind (("C-c I" . hydra-image/body)
         ("C-c L" . hydra-ledger/body)
         ("C-c M" . hydra-merge/body)
         ("C-c T" . hydra-tool/body)
         ("C-c b" . hydra-btoggle/body)
         ("C-c c" . hydra-clock/body)
         ("C-c e" . hydra-erc/body)
         ("C-c f" . hydra-flycheck/body)
         ("C-c g" . hydra-go-to-file/body)
         ("C-c m" . hydra-magit/body)
         ("C-c o" . hydra-org/body)
         ("C-c p" . hydra-projectile/body)
         ("C-c q" . hydra-query/body)
         ("C-c s" . hydra-spelling/body)
         ;; ("C-c t" . hydra-tex/body)
         ("C-c u" . hydra-upload/body)
         ("C-c w" . hydra-windows/body)))

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
    ("h" global-hungry-delete-mode "hungry delete" :toggle t))
   "Coding"
   (("e" electric-operator-mode "electric operator" :toggle t)
    ("F" flyspell-mode "flyspell" :toggle t)
    ("f" flycheck-mode "flycheck" :toggle t)
    ("l" lsp-mode "lsp" :toggle t))
    ;; ("s" smartparens-mode "smartparens" :toggle t))
   "UI"
   (("i" ivy-rich-mode "ivy-rich" :toggle t))))

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

(pretty-hydra-define hydra-go-to-file
  (:hint nil :color teal :quit-key "q" :title (with-faicon "file-text-o" "Go To" 1 -0.05))
  ("Agenda"
    (("ac" (find-file my/org-contacts-file-path) "contacts"))
    ;; ("ao" (find-file "~/.personal/agenda/organizer.org") "organizer")
    ;; ("ap" (find-file "~/.personal/agenda/people.org") "people")
    ;; ("ar" (find-file "~/.personal/agenda/routine.org") "routine")
    "Config"
    (("ce" (find-file "~/dotenv/emacs.d/") "emacs.d")
      ("ci" (find-file "~/dotenv/emacs.d/init.el") "init.el")
      ("cs" (find-file "~/dotenv/shells/") "shells")
      ("cs" (find-file "~/.authinfo.gpg") "~/.authinfo.gpg")
      ("cS" (find-file "~/.mbsyncrc") "mbsync")
      ("cE" (find-file "~/dotenv/editorconfig") "editorconfig")
      ("cl" (find-file "~/dotenv/emacs.d/lisp/") "emacs.d/lisp")
      ("cg" (find-file "~/dotenv/gitconfig") "gitconfig")
      ("cm" (find-file "~/dotenv/emacs.d/modules/") "emacs.d/modules"))
    "Private"
    ;; (("na" (find-file (format "~/.personal/notes/affirmations.pdf" xdg-config)) "Affirmations")
    (("nd" (find-file my/dashboard-file-path) "dashboard")
      ("ng" (find-file my/path-coaching-goals-details) "goals details")
      ("nw" (find-file my/path-coaching-wheel-of-life) "wheel of life")
      ("nb" (find-file my/path-coaching-brainstorm) "brainstorm")
      ("nr" (find-file my/org-review-file-path) "reviews"))
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
    ("c" magit-clone "clone")
    ("i" magit-init "init")
    ("l" magit-log-buffer-file "commit log (current file)")
    ("L" magit-log-current "commit log (project)")
    ("s" magit-status "status"))))

(pretty-hydra-define hydra-merge
  (:hint nil :color pink :quit-key "q" :title (with-alltheicon "git" "Merge" 1 -0.05))
  ("Move"
    (("n" smerge-next "next")
      ("p" smerge-prev "previous"))
    "Keep"
    (("RET" smerge-keep-current "current")
      ("a" smerge-keep-all "all")
      ("b" smerge-keep-base "base")
      ("l" smerge-keep-lower "lower")
      ("u" smerge-keep-upper "upper"))
    "Diff"
    (("<" smerge-diff-base-upper "upper/base")
      ("=" smerge-diff-upper-lower "upper/lower")
      (">" smerge-diff-base-lower "base/lower")
      ("R" smerge-refine "redefine")
      ("E" smerge-ediff "ediff"))
    "Other"
    (("C" smerge-combine-with-next "combine")
      ("r" smerge-resolve "resolve")
      ("k" smerge-kill-current "kill current"))))

(pretty-hydra-define hydra-org
  (:hint nil :color teal :quit-key "q" :title (with-fileicon "org" "Org" 1 -0.05))
  ("Action"
   (("A" my/org-archive-done-tasks "archive")
    ("a" org-agenda "agenda")
    ("c" org-capture "capture")
    ;; ("d" org-decrypt-entry "decrypt")
    ("i" org-insert-link-global "insert-link")
    ("j" my/org-jump "jump-task")
    ("k" org-cut-subtree "cut-subtree")
    ("o" org-open-at-point-global "open-link")
    ("r" org-refile "refile")
    ("s" org-store-link "store-link")
    ("t" org-show-todo-tree "todo-tree"))))

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
    ("p" counsel-projectile-switch-project "project"))
   "Other"
   (("i" projectile-invalidate-cache "reset cache"))
   "Search"
   (("r" projectile-replace "replace")
    ("R" projectile-replace-regexp "regexp replace")
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
    ("l" (message "Current language: %s (%s)" langtool-default-language ispell-current-dictionary) "language")
    ("s" my/switch-language "switch")
    ("w" wiki-summary "wiki"))
   "Errors"
   (("<" flyspell-correct-previous "previous" :color pink)
    (">" flyspell-correct-next "next" :color pink)
    ("f" langtool-check "find"))))

(pretty-hydra-define hydra-tex
  (:hint nil :color teal :quit-key "q" :title (with-fileicon "tex" "LaTeX" 1 -0.05))
  ("Action"
   (("g" reftex-goto-label "goto")
    ("r" reftex-query-replace-document "replace")
    ("s" counsel-rg "search")
    ("t" reftex-toc "table of content"))))

(pretty-hydra-define hydra-tool
  (:hint nil :color teal :quit-key "q" :title (with-faicon "briefcase" "Tool" 1 -0.05))
  ("Network"
    (
      ;; ("c" ipcalc "subnet calculator")
    ("i" ipinfo "ip info"))))

;; (pretty-hydra-define hydra-upload
;;   (:hint nil :color teal :quit-key "q" :title (with-faicon "cloud-upload" "Upload" 1 -0.05))
;;   ("Action"
;;    (("b" webpaste-paste-buffe "buffer")
;;     ("i" imgbb-upload "image")
;;     ("r" webpaste-paste-region "region"))))

(pretty-hydra-define hydra-windows
  (:hint nil :forein-keys warn :quit-key "q" :title (with-faicon "windows" "Windows" 1 -0.05))
  ("Window"
   (("b" balance-windows "balance")
    ("i" enlarge-window "heighten")
    ("j" shrink-window-horizontally "narrow")
    ("k" shrink-window "lower")
    ("l" enlarge-window-horizontally "widen")
    ("s" switch-window-then-swap-buffer "swap" :color teal))
   "Zoom"
   (("-" text-scale-decrease "out")
    ("+" text-scale-increase "in")
    ("=" (text-scale-increase 0) "reset"))))

(provide 'my-hydra)