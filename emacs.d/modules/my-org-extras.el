;; -*- lexical-binding: t; -*-

;;; Code:

(use-package calfw
  :after calendar)

(use-package calfw-org
  :after (calfw calendar)
  :commands cfw:open-org-calendar
  :custom
  (cfw:org-overwrite-default-keybinding t)
  :config
  (evil-make-overriding-map cfw:calendar-mode-map 'motion))

;; (defalias 'cal #'cfw:open-org-calendar)

(use-package org-contacts
  :if my/org-contacts-enabled
  :after org
  :custom
  (org-contacts-files `(,my/org-contacts-file-path))
  (org-contacts-birthday-format "%h (%Y)"))

(use-package japanese-holidays
  :straight (:type git
              :host github
              :repo "emacs-jp/japanese-holidays"
              :branch "master"))

(use-package bibtex
  :straight nil
  :custom
  (bibtex-dialect 'biblatex))

(use-package calendar
  :demand t
  :bind (:map calendar-mode-map
          ("<" . my/scroll-year-calendar-backward)
          (">" . my/scroll-year-calendar-forward))
  :custom
  (diary-number-of-entries 31)
  (holiday-local-holidays nil)
  (diary-show-holidays-flag nil)
  (calendar-christian-all-holidays-flag t)
  ;; (calendar-mark-holidays-flag t)
  (calendar-week-start-day 1)
  (calendar-date-style 'european)
  :config
  ;; https://stackoverflow.com/questions/9547912/emacs-calendar-show-more-than-3-months
  (defun my/calendar-year (&optional year)
    "Generate a one year calendar that can be scrolled by year in each direction.
This is a modification of:  http://homepage3.nifty.com/oatu/emacs/calendar.html
See also: https://stackoverflow.com/questions/9547912/emacs-calendar-show-more-than-3-months"
    (interactive)
    ;; (require 'calendar)
    (let* (
            (current-year (number-to-string (nth 5 (decode-time (current-time)))))
            (month 0)
            (year (if year year (string-to-number (format-time-string "%Y" (current-time))))))
      (switch-to-buffer (get-buffer-create calendar-buffer))
      (when (not (eq major-mode 'calendar-mode))
        (calendar-mode))
      (setq displayed-month month)
      (setq displayed-year year)
      (setq buffer-read-only nil)
      (erase-buffer)
      ;; horizontal rows
      (dotimes (j 4)
        ;; vertical columns
        (dotimes (i 3)
          (calendar-generate-month
            (setq month (+ month 1))
            year
            ;; indentation / spacing between months
            (+ 5 (* 25 i))))
        (goto-char (point-max))
        (insert (make-string (- 10 (count-lines (point-min) (point-max))) ?\n))
        (widen)
        (goto-char (point-max))
        (narrow-to-region (point-max) (point-max)))
      (widen)
      (goto-char (point-min))
      (setq buffer-read-only t)))

  (defun my/scroll-year-calendar-forward (&optional arg event)
    "Scroll the yearly calendar by year in a forward direction."
    (interactive (list (prefix-numeric-value current-prefix-arg)
                   last-nonmenu-event))
    (unless arg (setq arg 0))
    (save-selected-window
      (if (setq event (event-start event)) (select-window (posn-window event)))
      (unless (zerop arg)
        (let* (
                (year (+ displayed-year arg)))
          (my/calendar-year year)))
      (goto-char (point-min))
      (run-hooks 'calendar-move-hook)))

  (defun my/scroll-year-calendar-backward (&optional arg event)
    "Scroll the yearly calendar by year in a backward direction."
    (interactive (list (prefix-numeric-value current-prefix-arg)
                   last-nonmenu-event))
    (my/scroll-year-calendar-forward (- (or arg 1)) event))

  (defalias 'calendar-year #'my/calendar-year)
  (defalias 'my/calendar-full #'my/calendar-year)
  (defalias 'yearly-calendar #'my/calendar-year))

(use-package org-review
  :after org-agenda
  :bind (:map org-agenda-mode-map
          ("C-c C-r" . org-review-insert-last-review)))

(use-package org-drill
  :after org
  :commands org-drill
  :custom
  (org-drill-use-visible-cloze-face-p t)
  (org-drill-hide-item-headings-p t)
  (org-drill-maximum-items-per-session 30)
  (org-drill-maximum-duration 12)
  (org-drill-save-buffers-after-drill-sessions-p t)
  (org-drill-add-random-noise-to-intervals-p t)
  (org-drill-adjust-intervals-for-early-and-late-repetitions-p t)
  (org-drill-learn-fraction 0.3)
  :config
  (defalias 'drill (lambda (&optional scope drill-match) (interactive) (org-drill scope drill-match t)))
  (defalias 'resume-drill 'org-drill-resume))

(use-package org-roam
  :if my/org-roam-enabled
  :defer 10
  :after (org emacsql)
  :diminish org-roam-mode
  :commands (org-roam-file-p org-roam-buffer-toggle org-roam-node-insert org-roam-find-directory org-roam-ui-open org-roam-node-find my/org-roam-node-find-other-window org-roam-switch-to-buffer org-id-get-create my/hydra-common/body my/org-roam-mode)
  :custom
  (org-roam-directory my/org-roam-directory)
  (org-roam-graph-viewer "/usr/bin/open")
  (org-roam-db-gc-threshold most-positive-fixnum)
  (org-roam-tag-sources '(prop))
  (org-roam-update-db-idle-second 60)
  (org-roam-verbose nil)
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  :config
  (require 'org-roam-protocol)

  (org-roam-db-autosync-mode 1)

  (define-derived-mode my/org-roam-mode org-mode "my-org-roam"
    "Major mode for org-roam ready org buffers.")

  (major-mode-hydra-define+ my/org-roam-mode
    (:hint nil :color teal :quit-key "q" :title (with-fileicon "org" "org-roam" 1 -0.05))
    ("Edit"
      (("n" org-id-get-create "turn heading into node")
        ("t" org-roam-tag-add "add org-roam tag to node at point")
        ("T" org-roam-alias-add "add org-roam alias to node at point"))
      "Navigation"
      (("u" org-roam-ui-open "open UI view")
        ("z" org-roam-buffer-toggle "toggle references sidebar" :toggle t))))

  (add-to-list 'display-buffer-alist
    '("\\*org-roam\\*"
       (display-buffer-in-direction)
       (direction . right)
       (window-width . 0.35)
       (window-height . fit-window-to-buffer)))

  (make-directory my/org-roam-directory t)

  (add-to-list 'magit-section-initial-visibility-alist '([org-roam-node-section org-roam-backlinks org-roam] . hide))

  ;; (add-hook 'org-roam-dailies-find-file-hook #'abbrev-mode)

  ;; (defun my/org-roam-find-file-other-window (&rest args)
  ;;   (interactive)
  ;;   (let ((org-roam-find-file-function #'find-file-other-window))
  ;;     (apply #'org-roam-find-file args)))

  (defun my/org-roam-node-find-other-window (&rest args)
    (interactive)
    (let ((org-roam-find-file-function #'find-file-other-window))
      (apply 'org-roam-node-find args)))

;;--------------------------
  ;; Handling file properties for CREATED & LAST_MODIFIED
  ;;--------------------------
  (defun zp/org-find-time-file-property (property &optional anywhere)
    "Return the position of the time file PROPERTY if it exists.
When ANYWHERE is non-nil, search beyond the preamble."
    (save-excursion
      (goto-char (point-min))
      (let ((first-heading
              (save-excursion
                (re-search-forward org-outline-regexp-bol nil t))))
        (when (re-search-forward (format "^#\\+%s:" property)
                (if anywhere nil first-heading)
                t)
          (point)))))

  (defun zp/org-has-time-file-property-p (property &optional anywhere)
    "Return the position of time file PROPERTY if it is defined.
As a special case, return -1 if the time file PROPERTY exists but
is not defined."
    (when-let ((pos (zp/org-find-time-file-property property anywhere)))
      (save-excursion
        (goto-char pos)
        (if (and (looking-at-p " ")
              (progn (forward-char)
                (org-at-timestamp-p 'lax)))
          pos
          -1))))

  (defun zp/org-set-time-file-property (property &optional anywhere pos)
    "Set the time file PROPERTY in the preamble.
When ANYWHERE is non-nil, search beyond the preamble.
If the position of the file PROPERTY has already been computed,
it can be passed in POS."
    (when-let ((pos (or pos
                      (zp/org-find-time-file-property property))))
      (save-excursion
        (goto-char pos)
        (if (looking-at-p " ")
          (forward-char)
          (insert " "))
        (delete-region (point) (line-end-position))
        (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
          (insert now)))))

  (defun zp/org-set-last-modified ()
    "Update the LAST_MODIFIED file property in the preamble."
    (when (and (derived-mode-p 'org-mode) (string-prefix-p my/org-roam-directory buffer-file-name))
      (zp/org-set-time-file-property "LAST_MODIFIED")))

  (add-hook 'before-save-hook 'zp/org-set-last-modified 50)

  ;; faces
  ;; org-roam-link
  ;; org-roam-link-current

  ;; (defvar my/org-roam-side-mode-map (make-sparse-keymap)
  ;;   "Keymap for `my/org-roam-side-mode'.")

  ;; (define-minor-mode my/org-roam-side-mode
  ;;   "Minor mode for org-roam side org buffer."
  ;;   :init-value nil
  ;;   :keymap my/org-roam-side-mode-map)

  ;; (defvar my/org-roam-mode-map (make-sparse-keymap)
  ;;   "Keymap for `my/org-roam-side-mode'.")

  ;; (define-minor-mode my/org-roam-mode
  ;;   "Minor mode for org-roam org buffers."
  ;;   :init-value nil
  ;;   :keymap my/org-roam-mode-map)

  ;; (defun my/org-roam-mode-hook-org-ram ()
  ;;   (when (string-prefix-p my/org-roam-directory buffer-file-name)
  ;;     (my/org-roam-mode 1))
  ;;   (when (string-equal (buffer-name) "*org-roam*")
  ;;     (my/org-roam-side-mode 1)))

  ;; (add-hook 'org-mode-hook 'my/org-roam-mode-hook-org-ram)


  ;; (add-hook 'after-init-hook 'org-roam-mode)

  ;; (when (fboundp 'org-roam-dailies-today)
  ;;   (org-roam-dailies-today))

  (defalias 'roam #'org-roam))

(use-package org-roam-ui
  :after org-roam
  :commands org-roam-ui-open
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

(use-package websocket
  :after org-roam-ui)

;; (use-package org-roam-server
;;   :after org-roam
;;   :config
;;   (setq org-roam-server-host "127.0.0.1")
;;   (setq org-roam-server-port 3333)
;;   (setq org-roam-server-export-inline-images t)
;;   (setq org-roam-server-authenticate nil)
;;   (setq org-roam-server-network-poll t)
;;   (setq org-roam-server-network-arrows nil)
;;   (setq org-roam-server-network-label-truncate t)
;;   (setq org-roam-server-network-label-truncate-length 60)
;;   (setq org-roam-server-network-label-wrap-length 20))

(use-package org-journal
  :if my/org-journal-enabled
  :defer 10
  :after org
  :commands (org-journal-new-entry my/org-journal-open-current-journal-file)
  :custom
  (org-journal-dir my/org-journal-directory)
  (org-journal-file-type 'yearly)
  (org-journal-file-format "%Y.org")
  (org-journal-encrypt-journal t)
  (org-journal-created-property-timestamp-format "%Y-%m-%d")
  (org-journal-find-file 'find-file)
  (org-journal-date-format "%Y-%m-%d %A")
  (org-journal-time-prefix "*** ")
  :config
  (unbind-key "C-c C-j")

  (defun my/org-journal-open-current-journal-file ()
    "Do `org-journal-open-current-journal-file` and go to the most recent entry."
    (interactive)
    (org-journal-open-current-journal-file)
    (let* ((heading-title "Timeline")
            (poslist (org-map-entries 'point (format "ITEM=\"%s\"" heading-title) 'file)))
      (if (<= (length poslist) 0)
        (message (format "No heading with title '%s' found!" heading-title))
        (goto-char (nth 0 poslist))
        (org-cycle)))
      (org-journal-mode))

  (defun my/org-journal-after-header-create-hook ()
    (goto-char (point-min))
    (mark-whole-buffer)
    (org-sort-entries nil ?A)
    (org-back-to-heading)
    (let ((anchor (point)))
      (forward-line)
      (kill-visual-line)
      (insert my/org-journal-template)
      (goto-char anchor)
      (forward-line)
      (yank)))

  (add-hook 'org-journal-after-header-create-hook #'my/org-journal-after-header-create-hook))

(defun my/org-fold-other-headings ()
  "Fold all `org-mode` headings other than the current one."
  (interactive)
  (org-shifttab)
  (org-cycle))

;; (defun my/org-roam-dailies-find-date-other-window ()
;;   (interactive)
;;   (let ((current (current-buffer)))
;;     (org-roam-dailies-find-date)
;;     (let ((dailies-buffer (current-buffer)))
;;       (switch-to-buffer current)
;;       (switch-to-buffer-other-window dailies-buffer))))


;; (eval-after-load 'bibtex
;;   '(progn
;;       (setq bibtex-autokey-year-length 4)
;;       (setq bibtex-autokey-name-year-separator "-")
;;       (setq bibtex-autokey-year-title-separator "-")
;;       (setq bibtex-autokey-titleword-separator "-")
;;       (setq bibtex-autokey-titlewords 2)
;;       (setq bibtex-autokey-titlewords-stretch 1)
;;       (setq bibtex-autokey-titleword-length 5)
;;      ))

;; (use-package org-ref
;;   :config
;;   (require 'org-ref-pdf)
;;   (require 'org-ref-url-utils)
;;   (setq org-ref-completion-library 'org-ref-ivy-cite)

;;   (setq org-ref-bibliography-notes "~/Documents/bibliography/notes.org")
;;   (setq org-ref-default-bibliography '("~/Documents/bibliography/references.bib"))
;;   (setq org-ref-pdf-directory "~/Documents/bibliography/bibtex-pdfs/")

;;   (unless (file-exists-p org-ref-pdf-directory)
;;     (make-directory org-ref-pdf-directory t))
;;   )

;; (setq org-latex-pdf-process
;;       '("pdflatex -interaction nonstopmode -output-directory %o %f"
;; 	"bibtex %b"
;; 	"pdflatex -interaction nonstopmode -output-directory %o %f"
;; 	"pdflatex -interaction nonstopmode -output-directory %o %f"))

;; (setq org-latex-default-packages-alist
;;   (-remove-item
;;     '("" "hyperref" nil)
;;     org-latex-default-packages-alist))

;; This is an Emacs package that creates graphviz directed graphs from
;; the headings of an org file
(use-package org-mind-map
  :disabled t
  :after org
  :if (executable-find "graphviz")
  :commands (org-mind-map-write org-mind-map-write-current-branch org-mind-map-write-current-tree)
  :init
  (unless (executable-find "graphviz")
    (message "No executable 'graphviz' found"))
  :custom
  (org-mind-map-engine "dot")       ; Default. Directed Graph
  ;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
  ;; (setq org-mind-map-engine "twopi")  ; Radial Layout
  ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
  ;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
  ;; (setq org-mind-map-engine "twopi")  ; Radial layouts
  ;; (setq org-mind-map-engine "circo")  ; Circular Layout
  :config
  (require 'ox-org))

(add-to-list 'magic-mode-alist '(org-roam-file-p . my/org-roam-mode))

;; TODO adapt that to change
(defun my/rename-org-files-in-folder (folder)
  ;; (interactive "DRename org files in folder: ")
  (dolist (file (directory-files folder t ".*\\.org\\(.gpg\\)?$"))
    (with-current-buffer (find-file-noselect file)
      (let* ((title (ignore-error (org-roam-node-file-title (org-roam-node-at-point))))
              (title (when title (expand-file-name title))))
        (message "file: %s ; title: %s" file title)
        (when title
          (let* ((ext (file-name-extension file))
                  (new-ext (if (string-equal ext "org.gpg") ".org.gpg" ".org"))
                  (new-name (concat folder (downcase (string-replace " " "-" title)) new-ext)))
            (when (and (not (file-exists-p new-name)) (not (string-equal file new-name)))
            (rename-file file new-name))))))))
;; (my/rename-org-files-in-folder "~/Documents/roam/database/topic/")

;; TODO check this one
;; https://org-roam.discourse.group/t/org-similarity-v2-1-0-released-lexical-similarity-search-for-emacs/3134
;; https://github.com/brunoarine/org-similarity#usage
(use-package org-similarity
  :disabled t
  :straight (:type git
             :host github
             :repo "brunoarine/org-similarity"
              :branch "main")
  :custom
  (org-similarity-directory my/org-roam-directory)
  (org-similarity-file-extension-pattern "*.\\(org\\|gpg\\)")
  (org-similarity-algorithm "tfidf")
  (org-similarity-number-of-documents 10)
  (org-similarity-min-chars 0)
  (org-similarity-show-scores nil)
  (org-similarity-threshold 0.05)
  (org-similarity-use-id-links t)
  (org-similarity-recursive-search t)
  ;; (org-similarity-remove-first t)
  (org-similarity-heading "** Related notes")
  (org-similarity-prefix "- ")
  (org-similarity-ignore-frontmatter nil)
  )

(use-package ox-latex
  :straight nil
  :after org
  :custom
  (org-latex-subtitle-separate t)
  (org-latex-classes
    '(("article" "\\documentclass[11pt]{article}"
        ("\\section{%s}" . "\\section*{%s}")
        ("\\subsection{%s}" . "\\subsection*{%s}")
        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
        ("\\paragraph{%s}" . "\\paragraph*{%s}")
        ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
       ("report" "\\documentclass[11pt]{report}"
         ("\\part{%s}" . "\\part*{%s}")
         ("\\chapter{%s}" . "\\chapter*{%s}")
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
       ("book" "\\documentclass[11pt]{book}"
         ("\\part{%s}" . "\\part*{%s}")
         ("\\chapter{%s}" . "\\chapter*{%s}")
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

       ("myarticle"
         "\\documentclass[a4paper,11pt]{article}
\\usepackage[utf8]{inputenc}
\\usepackage{lmodern}
\\usepackage{cabin}
\\usepackage[T1]{fontenc}
\\usepackage{fancyhdr}
\\usepackage[a4paper, total={6in, 8in}]{geometry}
\\usepackage{lastpage}
\\usepackage{titling}
\\usepackage[dvipsnames]{xcolor}
\\usepackage[pdfpagelabels=false,hyperindex=false,hyperfootnotes=false]{hyperref}
"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

       ("mybeamer"
         "\\documentclass[presentation]{beamer}"
         org-beamer-sectioning)
       ))
  (org-latex-title-command
    (concat
      "\\begin{titlepage}\n"
      "\\centering\n"
      "{\\LARGE %t \\par }\n"
      "\\vspace 2cm\n"
      "{\\normalsize %a \\par}\n"
      "\\vspace 3cm\n"
      "{\\huge %D \\par}\n"
      "\\end{titlepage}\n")))

;; (use-package org-web-tools)

(provide 'my-org-extras)
;;; my-org.el ends here
