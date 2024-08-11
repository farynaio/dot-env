;; -*- lexical-binding: t; -*-

;;; Code:

;; custom style for org
;; https://github.com/minad/org-modern

(use-package org-contrib
  :after org)

(use-package org-table-cell-move
  :after org
  :straight (:type built-in))

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

(use-package org
  :demand t
  :init
  (when (and (boundp 'my/org-base-path) my/org-base-path)
    (make-directory my/org-base-path t))
  :hook ((org-mode . org-indent-mode)
          (org-mode . iscroll-mode)
          (org-mode . (lambda () (when (and (boundp 'company-mode) company-mode) (company-mode 1))))
          (org-mode . org-appear-mode)
          (org-mode . afa/org-breadcrums-mode)
          (org-mode . (lambda ()
                        (setq-local
                          paragraph-start "[:graph:]+$"
                          paragraph-separate "[:space:]*$")))
          ;; refresh agenda after adding new task via org-capture
          (org-capture-after-finalize . (lambda ()
                                          (save-excursion
                                            (when (get-buffer "*Org Agenda*")
                                              (with-current-buffer "*Org Agenda*" (org-agenda-redo))))))
          (org-agenda-mode . (lambda () (hl-line-mode 1)))
          (org-after-todo-state-change . my/org-state-canceled-timestamp-toggle))
  :bind (("C-c c" . org-capture)
          ("C-x a" . org-agenda)
          ("C-c l" . org-store-link)
          ("C-c L" . org-insert-link-global)
          ("C-c j" . org-clock-goto) ;; jump to current task from anywhere
          ("C-c C-o" . org-open-at-point-global)
          :map org-mode-map
          ("C-c C-l" . org-insert-link)
          ("C-x C-l" . my/org-link-copy)
          ("C-c l" . org-store-link)
          ("C-c C-x a" . org-archive-subtree-default)
          ("M-}" . forward-paragraph)
          ("M-{" . backward-paragraph)
          ("C-M-<up>" . org-table-move-single-cell-up)
          ("C-M-<down>" . org-table-move-single-cell-down)
          ("C-M-<left>" . org-table-move-single-cell-left)
          ("C-M-<right>" . org-table-move-single-cell-right)
          ("<S-tab>" . my/outline-hide-subtree)
          ("<M-up>" . my/org-metaup)
          ("<M-down>" . my/org-metadown)
          ("<s-mouse-1>" . org-open-at-point)
          ("<S-mouse-1>" . org-open-at-point)
          ([remap backward-paragraph] . nil)
          ([remap forward-paragraph] . nil)
          ("C-x :" . (lambda () (interactive) (save-excursion (org-back-to-heading) (org-set-tags))))
          :map org-agenda-mode-map
          ("C-c C-c" . org-agenda-set-tags)
          ("C-d" . evil-scroll-down)
          ("C-u" . evil-scroll-up)
          ("s-t" . make-frame-command)
          ("n" . evil-search-next)
          ("N" . evil-search-previous)
          ("*" . evil-search-word-forward)
          ("'" . org-agenda-filter-by-tag))
  :preface
  (defun my/org-priority-cookie-print () (format "[#%c]" org-default-priority))
  :custom
  (org-adapt-indentation nil)
  (org-return-follows-link t)

  (org-agenda-skip-scheduled-if-deadline-is-shown 'not-today)
  (org-agenda-start-on-weekday nil)
  (org-agenda-sort-noeffort-is-high nil)
  (org-agenda-skip-additional-timestamps-same-entry t)
  (org-agenda-skip-deadline-prewarning-if-scheduled t)
  (org-agenda-start-with-log-mode t)
  (org-agenda-inhibit-startup nil)
  (org-agenda-use-tag-inheritance nil)
  (org-agenda-fontify-priorities 'cookies)
  (org-agenda-log-mode-items '(clock))
  (org-agenda-scheduled-leaders '("" ""))
  (org-agenda-file-regexp ".*org\(.gpg\)?$")
  (org-agenda-dim-blocked-tasks t)
  (org-agenda-include-diary t)
  (org-agenda-search-headline-for-time nil)

  (org-ascii-links-to-notes nil)
  (org-ascii-headline-spacing '(1 . 1))

  (org-archive-reversed-order t)

  (org-clock-in-resume t)
  (org-clock-persist-query-resume nil)
  (org-clock-in-switch-to-state "IN-PROCESS")
  (org-clock-persist t) ; or 'history?
  (org-clock-idle-time 2) ; TODO requires testing
  (org-clock-into-drawer t)
  (org-clock-report-include-clocking-task t)
  (org-clock-out-remove-zero-time-clocks t)
  ;; (org-clock-out-when-done (list "TODO" "BLOCKED" "WAITING" "DONE" "DELEGATED" "UNDOABLE"))
  (org-clock-out-when-done t)

  (org-icalendar-use-scheduled '(todo-start event-if-todo))
  (org-icalendar-use-deadline '(event-if-todo))
  (org-icalendar-with-timestamps 'active)
  (org-icalendar-include-todo t)
  (org-icalendar-include-sexps t)
  (org-icalendar-store-UID t)
  (org-icalendar-timezone "Europe/London") ; or nil
  (org-icalendar-alarm-time 60)
  ;; (org-icalendar-honor-noexport-tag t) ; this is not supported in my version

  (org-log-into-drawer t)
  (org-log-done 'time)

  (org-habit-show-habits-only-for-today nil)
  (org-habit-graph-column 62)

  (org-startup-with-inline-images t)
  (org-startup-indented t)
  (org-startup-folded t)

  (org-babel-python-command "python3")

  (org-blank-before-new-entry '((heading . auto) (plain-list-item . nil)))
  (org-hide-emphasis-markers t)
  (org-src-preserve-indentation t)
  (org-table-header-line-p nil)
  (org-image-actual-width 1200)
  ;; (setq org-list-end-re "^$")
  (org-list-demote-modify-bullet
    '(("+" . "-")
       ("-" . "+")
       ("1." . "-")))
  (org-lowest-priority 68)
  (org-highest-priority 65)
  (org-default-priority 66)
  ;; (org-default-notes-file (expand-file-name "notes.org" org-directory))
  (org-hide-leading-stars t)
  (org-refile-allow-creating-parent-nodes 'confirm)

  (org-export-babel-evaluate t)
  (org-export-preserve-breaks t)
  (org-export-with-toc t)
  (org-export-with-smart-quotes t) ; could cause problems on babel export
  (org-export-with-email nil)

  ;; org-export for ODT config
  (org-odt-styles-file (expand-file-name "org/etc/styles/OrgOdtStyles.xml" user-emacs-directory))
  (org-odt-content-template-file (expand-file-name "org/etc/styles/OrgOdtContentTemplate.xml" user-emacs-directory))

  ;; org-export for LateX config
  ;; (org-latex-subtitle-separate t)

  (org-pretty-entities-include-sub-superscripts t)
  ;; (org-use-sub-superscripts '{})


  (org-group-tag nil)
  ;; (setq org-enforce-todo-dependencies t)
  ;; (setq org-track-ordered-property-with-tag t)
  (org-use-property-inheritance t)
  (org-use-speed-commands t)
  (org-edit-src-content-indentation 0)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-priority-start-cycle-with-default nil)
  (org-columns-default-format "%25ITEM(Task) %TODO %3PRIORITY %7Effort %8CLOCKSUM %TAGS")
  ;; (org-confirm-babel-evaluate nil)
  (org-confirm-babel-evaluate (lambda (lang body) (not (string= lang "ledger"))))
  (org-cycle-include-plain-lists t)
  (org-hide-block-startup t)
  (org-list-description-max-indent 5)
  (org-fontify-done-headline t)
  (org-closed-keep-when-no-todo t)
  (org-log-done-with-time nil)
  (org-deadline-warning-days 14)
  (org-reverse-note-order t)
  (org-global-properties '(("Effort_ALL" . "0:00 0:15 0:30 1:00 2:00 4:00")))
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts nil)
  ;; (org-agenda-window-setup 'current-window)
  (org-return-follows-link nil)
  (org-link-frame-setup
    '((vm . vm-visit-folder-other-frame)
       (vm-imap . vm-visit-imap-folder-other-frame)
       (gnus . org-gnus-no-new-news)
       (file . find-file)
       (wl . wl-other-frame)))
  (plstore-cache-passphrase-for-symmetric-encryption t)
  (org-element-use-cache nil)

  (org-refile-use-outline-path t)

  (org-fast-tag-selection-single-key t)
  (org-tags-exclude-from-inheritance '("project" "taskjuggler_project" "taskjuggler_resource") org-stuck-projects '("+project/-DONE" ("TODO") ()))

  (org-todo-keywords
    '((sequence "TODO(t)" "WIP(p!)" "BLOCKED(b!)" "WAITING(w@/!)" "DELEGATED(e@/!)")
       (sequence "|" "DONE(d!)" "SKIP(c@)" "UNDOABLE(u@)")))
  (org-todo-keyword-faces
    '(("TODO"        . (:foreground "LimeGreen"   :weight bold))
       ("IN-PROCESS" . (:foreground "IndianRed1"  :weight bold))
       ("WIP"        . (:foreground "IndianRed1"  :weight bold))
       ("WORK"       . (:foreground "IndianRed1"  :weight bold))
       ("BLOCKED"    . (:foreground "tomato3"     :weight bold))
       ("WAITING"    . (:foreground "coral"       :weight bold))
       ("DELEGATED"  . (:foreground "coral"       :weight bold))
       ("NOTE"       . (:foreground "white"       :weight bold))
       ("DONE"       . (:foreground "dark grey"   :weight normal))
       ("SKIP"       . (:foreground "dark grey"   :weight normal))
       ("HUGE"       . (:foreground "dark grey"   :weight normal))
       ("UNDOABLE"   . (:foreground "dark grey"   :weight normal))))

  (org-priority-cookie () (format "[#%c]" org-default-priority))
  :config
  (require 'org-agenda)

  (unbind-key "C-'" org-mode-map)
  (unbind-key "C-," org-mode-map)
  (unbind-key "C-c $" org-mode-map) ; removed archive subtree shortcut
  (unbind-key "C-c C-x C-a" org-mode-map) ; remove archive subtree default shortcut
  (unbind-key "C-c C-x C-s" org-mode-map) ; remove archive subtree shortcut
  (unbind-key "C-c C-x A" org-mode-map) ; remove archive to archive siblings shortcut
  (unbind-key "\\" org-agenda-mode-map)

  (evil-define-key '(motion normal) org-mode-map
    (kbd "<down>") 'evil-next-visual-line
    (kbd "<up>")   'evil-previous-visual-line
    (kbd "C-c C-s") 'org-schedule)

  ;; (evil-define-key 'insert org-mode-map
  ;;   (kbd "C-n") 'completion-at-point
  ;;   (kbd "C-p") 'completion-at-point)

  (evil-define-key 'normal org-mode-map
    ;; (kbd "C-n") 'completion-at-point
    ;; (kbd "C-p") 'completion-at-point
    ;; (kbd "<tab>") 'org-cycle
    (kbd "TAB") 'org-cycle)
  ;; (kbd "C-c s") 'hydra-spelling/body)
  ;; (kbd ",t") 'my/google-translate-at-point)

  ;; TODO maybe it's not needed
  (evil-define-key 'visual org-mode-map
    (kbd "C-c C-n") 'org-next-visible-heading
    (kbd "C-c C-p") 'org-previous-visible-heading)

  (add-to-list 'org-modules 'org-habit t)

  ;; (use-package org-auto-tangle
  ;; :hook (org-mode . org-auto-tangle-mode))

  (use-package org-tanglesync
    :disabled t
    :hook ((org-mode . org-tanglesync-mode)
            ;; enable watch-mode globally:
            ((prog-mode text-mode) . org-tanglesync-watch-mode))
    :bind
    (( "C-c M-i" . org-tanglesync-process-buffer-interactive)
      ( "C-c M-a" . org-tanglesync-process-buffer-automatic)))

  ;; blogging
  ;; http://www.i3s.unice.fr/~malapert/org/tips/emacs_orgmode.html
  ;; (require 'ox-publish)
  ;; (setq org-html-coding-system 'utf-8-unix)
  ;; (setq org-html-head-include-default-style nil)
  ;; (setq org-html-head-include-scripts nil)
  ;; (setq org-html-validation-link nil)

  (org-clock-persistence-insinuate)

  (diminish 'org-indent-mode)

  (add-to-list 'org-file-apps
    '("\\.html\\'" . (lambda (file-path link-without-schema) (eww-open-file file-path))))

  (when (and (boundp 'my/org-inbox-file-path) my/org-inbox-file-path)
    (setq org-default-notes-file my/org-inbox-file-path))

  (defun my/dnd-file-line-insert (uri action)
    "URI to the asset. ACTION is ignored."
    (insert (format "[[%s]]" uri))
    (when (and (eq major-mode 'org-mode) org-startup-with-inline-images)
      (org-toggle-inline-images)))

  (setq dnd-protocol-alist
    '(("^file:.*\\(jpe?g\\|png\\)\\'" . my/dnd-file-line-insert)
       ("^file:.*\\(pdf\\)\\'" . my/dnd-file-line-insert)
       ("^file:.*\\(ods\\|odt\\)\\'" . my/dnd-file-line-insert)
       ("^file:.*\\(mp3\\|mp4\\|avi\\)\\'" . my/dnd-file-line-insert)))

  ;; (use-package jupyter)

  (require 'ob-python)
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
       (ledger . t)
       (python . t)
       (gnuplot . t)
       (shell . t)
       (latex . t)
       ;; (jupyter . t)
       ))

  (use-package async
    :straight (:type git
                :host github
                :repo "jwiegley/emacs-async"))

  ;; This is for async evalaution of org-babel blocks.
  (use-package ob-async
    :straight (ob-async
                :type git
                :host github
                ;; :repo "farynaio/ob-async")
                :repo "astahlman/ob-async"
                :fork (:host github :repo "farynaio/ob-async"))
    :config
    (setq ob-async-no-async-languages-alist '("python" "ipython" "jupyter-python" "jupyter-julia")))

  (if (executable-find "unoconv")
    (setq org-odt-convert-processes '(("unoconv" "unoconv -f %f -o %d %i")))
    (message "No executable 'unoconv' found.")
    (setq org-odt-convert-processes '(("unoconv" "unoconv -f %f -o %d.xls %i"))))

  (major-mode-hydra-define (org-mode my/org-roam-mode)
    (:hint nil :color teal :quit-key "q" :title (with-fileicon "org" "Org" 1 -0.05))
    ("Action"
      (("t" org-toggle-timestamp-type "timestamp toggle")
        ("a" org-link-archive-at-point "link archive")
        ("i" org-toggle-inline-images "images toggle" :toggle t)
        ("r" my/org-paragraphs-reverse-order "reverse paragraph order")
        ("h" org-archive-subtree "archive heading subtree")
        ("d" my/org-remove-duplicate-lines-in-list "remove list duplicates")
        ("R" org-reset-checkbox-state-subtree "reset all org checkbox in subtree" :exit t)
        ("n" org-narrow-to-subtree "org narrow to subtree" :exit t)
        ("w" widen "widen narrowed area" :exit t)
        ("x" org-export-dispatch "org-export-dispatch" :exit t)
        ("p" my/org-align-tags "align tags" :exit t))
      "Toggle"
      (("e" my/org-toggle-emphasis "org-ephasis toggle" :toggle t :exit t)
        ("l" org-table-header-line-mode "org-table-header-line-mode" :toggle t)
        ("ob" afa/org-breadcrums-mode "breadcrumbs" :toggle t)
        ("oa" org-appear-mode "org-appear toggle" :toggle t :exit t))
      "Navigation"
      (("s" counsel-org-goto "goto heading")
        ("fa" counsel-org-file "browse attachments"))))

  (add-to-list 'org-file-apps '("\\.pdf\\'" . "open %s"))
  ;; Fixes problem with void function org-clocking-buffer
  (defun org-clocking-buffer ())

  (defun my/org-current-is-todo-p ()
    (string= "TODO" (org-get-todo-state)))

  ;; https://emacs.stackexchange.com/a/48385/18445
  (defun my/print-duplicate-headings ()
    "Print duplicate headings from the current org buffer."
    (interactive)
    (with-output-to-temp-buffer "*temp-out*"
      (let ((header-list '())) ; start with empty list
        (org-element-map (org-element-parse-buffer) 'headline
          (lambda (x)
            (let ((header (org-element-property :raw-value x)))
              (when (-contains? header-list header)
                (princ header)
                (terpri))
              (push header header-list)))))))

  (defun my/org-remove-duplicate-lines-in-list ()
    "Remove duplicate lines inside plain-list at point."
    (interactive)
    (let ((list-element (org-element-lineage (org-element-at-point) '(plain-list) t)))
      (if (not list-element)
        (user-error "Not at plain-list")
        (let ((nlines
	              (delete-duplicate-lines
	                (org-element-property :post-affiliated list-element)
	                (save-excursion (goto-char (org-element-property :end list-element)) (skip-chars-backward "\r\n\t ") (point)))))
          (if (= 0 nlines)
	          (message "List contains no duplicate lines")
            (message "Removed %d duplicate lines from list" nlines))))))

  ;; from https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
  (defun my/org-agenda-skip-all-siblings-but-first ()
    "Skip all but the first non-done entry."
    (let (should-skip-entry)
      (unless (my/org-current-is-todo-p)
        (setq should-skip-entry t))
      (save-excursion
        (while (and (not should-skip-entry) (org-goto-sibling t))
          (if (my/org-current-is-todo-p)
            (setq should-skip-entry t)
            (setq should-skip-entry nil))))
      (when should-skip-entry
        (or (outline-next-heading)
          (goto-char (point-max))))))

;; https://www.emacswiki.org/emacs/ReverseParagraphs
  (defun my/org-paragraphs-reverse-order ()
    (interactive)
    "Reverse the order of paragraphs in a region. From a program takes two point or marker arguments, BEG and END."
    (let ((beg (point-min)) (end (point-max)) (mid))
      (when (> beg end)
        (setq mid end end beg beg mid))
      (save-excursion
        ;; the last paragraph might be missing a trailing newline
        (goto-char end)
        (setq end (point-marker))
        ;; the real work.
        (goto-char beg)
        (let (paragraphs fix-newline)
          (while (< beg end)
            ;; skip to the beginning of the next paragraph instead of
            ;; remaining on the position separating the two paragraphs
            (when (= 0 (forward-paragraph 1))
              (goto-char (1+ (match-end 0))))
            (when (> (point) end)
              (goto-char end))
            (setq paragraphs (cons (buffer-substring beg (point))
                               paragraphs))
            (delete-region beg (point)))
          ;; if all but the last paragraph end with two newlines, add a
          ;; newline to the last paragraph
          (when (and (null (delete 2 (mapcar (lambda (s)
                                               (when (string-match "\n+$" s -2)
                                                 (length (match-string 0 s))))
                                       (cdr paragraphs))))
                  (when (string-match "\n+$" (car paragraphs) -2)
                    (= 1 (length (match-string 0 (car paragraphs))))))
            (setq fix-newline t)
            (setcar paragraphs (concat (car paragraphs) "\n")))
          ;; insert paragraphs
          (dolist (par paragraphs)
            (insert par))
          (when fix-newline
            (delete-char -1))))))

  ;; org mode conflicts resolution: windmove
  ;; (add-hook 'org-shiftup-final-hook #'windmove-up)
  ;; (add-hook 'org-shiftleft-final-hook 'windmove-left)
  ;; (add-hook 'org-shiftdown-final-hook 'windmove-down)
  ;; (add-hook 'org-shiftright-final-hook #'windmove-right)

  ;; https://emacs.stackexchange.com/questions/61101/keep-displaying-current-org-heading-info-in-some-way/61107#61107
  (defun afa/org-breadcrumbs ()
    "Get the chain of headings from the top level down to the current heading."
    (when (and (fboundp 'org-roam-file-p) (org-roam-file-p))
      (let* ((org-roam-node-title (ignore-errors (org-roam-node-file-title (org-roam-node-at-point))))
              (filename (if org-roam-node-title org-roam-node-title (buffer-name))))
    ;;           (path (ignore-errors (org-get-outline-path t)))
    ;;           (breadcrumbs
    ;;             (if path
    ;;               (org-format-outline-path
    ;;                 path
    ;;                 (1- (frame-width))
    ;;                 nil " > ")
    ;;               "")))
        ;; (format "[%s] %s" filename breadcrumbs))))
        (format "[ %s ]" filename))))

  ;; TODO need to fix it
  ;; maybe some inspiration from https://github.com/alphapapa/org-sticky-header
  (define-minor-mode afa/org-breadcrums-mode
    "Minor mode to display org breadcrumbs.
    Toggle `afa/org-breadcrums-mode'"
    :lighter "hlp"
    :global nil
    :init-value nil
    (when afa/org-breadcrums-mode
      (setq-local header-line-format (afa/org-breadcrumbs))))
      ;; (defvar afa/org-breadcrums-mode-timer
      ;;   (run-with-idle-timer
      ;;     5
      ;;     t
      ;;     (lambda ()
      ;;       (when (derived-mode-p 'org-mode)
      ;;         (setq-local header-line-format (afa/org-breadcrumbs))))))
      ;; (cancel-timer afa/org-breadcrums-mode-timer)
      ;; (setq afa/org-breadcrums-mode-timer nil)))

  ;; (afa/org-breadcrums-mode 1)

  (defun my/org-link-copy (&optional arg)
    "Extract URL from org-mode link and add it to kill ring."
    (interactive "P")
    (let* ((link (org-element-lineage (org-element-context) '(link) t))
            (type (org-element-property :type link))
            (url (org-element-property :path link))
            (url (concat type ":" url)))
      (kill-new url)
      (message (concat "Copied URL: " url))))

  (defun my/org-agenda-cmp-user-defined-birthday (a b)
    "Org Agenda user function to sort categories against other categories. The birthday category is considered to be behind other category by default."
    (let* ((pla (get-text-property 0 'org-category a))
            (plb (get-text-property 0 'org-category b))
            (pla (string-equal pla "Birthday"))
            (plb (string-equal plb "Birthday")))
      (if (or (and pla plb) (and (not pla) (not plb)))
        nil
        (if pla
          -1
          +1))))

  (defun my/org-agenda-cmp-user-defined-created-date (a b)
    "Org Agenda user function to sort tasks based on CREATED property."
    (let* (
            (marker-a (get-text-property 0 'org-marker a))
            (marker-b (get-text-property 0 'org-marker b))
            (time-a (if marker-a (org-entry-get marker-a "CREATED") nil))
            (time-b (if marker-b (org-entry-get marker-b "CREATED") nil)))

      (if (and time-a time-b)
        (if (org-time< time-a time-b)
          -1
          (if (org-time> time-a time-b) 1 nil))
        (if time-a -1 1))))

  ;; https://emacs.stackexchange.com/a/30194/18445
  (defun my/org-agenda-skip-deadline-if-not-today ()
    "If this function returns nil, the current match should not be skipped.
Otherwise, the function must return a position from where the search
should be continued."
    (ignore-errors
      (let ((subtree-end (save-excursion (org-end-of-subtree t)))
             (deadline-day
               (time-to-days
                 (org-time-string-to-time
                   (org-entry-get nil "DEADLINE"))))
             (now (time-to-days (current-time))))
        (and deadline-day
          (not (<= deadline-day now))
          subtree-end))))

  (defun my/org-calendar-export-limit ()
    "Limit the export to items that have a date, time and a range. Also exclude certain categories."
    (setq org-tst-regexp "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ... [0-9]\\{2\\}:[0-9]\\{2\\}[^\r\n>]*?\
\)>")
    (setq org-tstr-regexp (concat org-tst-regexp "--?-?" org-tst-regexp))
    (save-excursion
                                        ; get categories
      (setq mycategory (org-get-category))
                                        ; get start and end of tree
      (org-back-to-heading t)
      (setq mystart    (point))
      (org-end-of-subtree)
      (setq myend      (point))
      (goto-char mystart)
                                        ; search for timerange
      (setq myresult (re-search-forward org-tstr-regexp myend t))
                                        ; search for categories to exclude
      (setq org-export-exclude-category (list "private"))
      (setq mycatp (member mycategory org-export-exclude-category))
                                        ; return t if ok, nil when not ok
      (if (and myresult (not mycatp)) t nil)))

  (defun my/org-skip-subtree-if-priority (priority)
    "Skip an agenda subtree if it has a priority of PRIORITY.
PRIORITY may be one of the characters ?A, ?B, or ?C."
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
           (pri-value (* 1000 (- org-lowest-priority priority)))
           (pri-current (org-get-priority (thing-at-point 'line t))))
      (if (= pri-value pri-current)
        subtree-end
        nil)))

  (defun my/org-skip-subtree-if-habit ()
    "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
        nil)))

  (defun my/org-agenda-skip-if-scheduled-later ()
    "If this function returns nil, the current match should not be skipped.
Otherwise, the function must return a position from where the search
should be continued."
    (ignore-errors
      (let ((subtree-end (save-excursion (org-end-of-subtree t)))
             (scheduled-seconds
               (time-to-seconds
                 (org-time-string-to-time
                   (org-entry-get nil "SCHEDULED"))))
             (now (time-to-seconds (current-time))))
        (and scheduled-seconds
          (>= scheduled-seconds now)
          subtree-end))))

  (defun my/org-state-canceled-timestamp-toggle ()
    "Toggle active/inactive SCHEDULED or DEADLINE timestamp Remove SCHEDULED-cookie is switching state to WAITING."
    (save-excursion
      (let ((state (org-get-todo-state)))
        (cond
          ((equal state "SKIP")
            (when (and
                    (org-get-deadline-time (point))
                    (search-forward-regexp "DEADLINE: .*" nil t)
                    (org-at-timestamp-p 'agenda))
              (org-toggle-timestamp-type))
            (when (and
                    (org-get-scheduled-time (point))
                    (search-forward-regexp "SCHEDULED: .*" nil t)
                    (org-at-timestamp-p 'agenda))
              (org-toggle-timestamp-type)))
          ((equal state "TODO")
            (when (and
                    (org-get-deadline-time (point))
                    (search-forward-regexp "DEADLINE: .*" nil t)
                    (equal (char-to-string (char-before)) "]"))
              (org-toggle-timestamp-type))
            (when (and
                    (org-get-scheduled-time (point))
                    (search-forward-regexp "SCHEDULED: .*" nil t)
                    (equal (char-to-string (char-before)) "]"))
              (org-toggle-timestamp-type)))))
      (when (equal (buffer-name (current-buffer)) "*Org Agenda*")
        (with-current-buffer "*Org Agenda*" (org-agenda-redo)))))

  ;; (define-minor-mode my/org-agenda-appt-mode
  ;;   "Minor mode for org agenda updating appt"
  ;;   :init-value nil
  ;;   :lighter " appt"
  ;;   (add-hook 'after-save-hook 'my/org-agenda-to-appt-if-not-terminated nil t))
  ;; (diminish 'my/org-agenda-appt-mode)

  ;; (defvar my/save-buffers-kill-terminal-was-called nil)
  ;; (defun my/org-agenda-to-appt-if-not-terminated ()
  ;;   (unless my/save-buffers-kill-terminal-was-called
  ;;     (org-agenda-to-appt t)))

  (defun my/outline-hide-subtree ()
    (interactive)
    (if (org-at-heading-p)
      (outline-hide-subtree)
      (org-shifttab)))

  (defun my/org-metaup ()
    (interactive)
    (call-interactively
      (if (org-at-heading-or-item-p)
        'org-metaup
        'drag-stuff-up)))

  (defun my/org-metadown ()
    (interactive)
    (call-interactively
      (if (org-at-heading-or-item-p)
        'org-metadown
        'drag-stuff-down)))

  (defun my/org-toggle-emphasis ()
    "Toggle hiding/showing of org emphasize markers."
    (interactive)
    (if org-hide-emphasis-markers
      (set-variable 'org-hide-emphasis-markers nil)
      (set-variable 'org-hide-emphasis-markers t))
    (message "org ephasis toggled %s" (if org-hide-emphasis-markers "on" "off")))

  (defun my/org-align-tags ()
    (interactive)
    (let ((current-prefix-arg '(4))) (call-interactively 'org-set-tags-command)))

  (advice-add 'org-refile :after (lambda (&rest args) (org-save-all-org-buffers)))
  (advice-add 'org-archive-subtree-default :after (lambda () (org-save-all-org-buffers)))
  (advice-add 'org-agenda-archive-default :after (lambda () (org-save-all-org-buffers)))
  (advice-add 'org-clock-in  :after (lambda (&rest args) (org-save-all-org-buffers)))
  (advice-add 'org-clock-out :after (lambda (&rest args) (org-save-all-org-buffers))))

(define-derived-mode my/org-roam-mode org-mode "my-org-roam"
  "Major mode for org-roam ready org buffers.")

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

(use-package websocket)
(use-package simple-httpd)
(use-package zmq)

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


(use-package org-link-archive
  :after org
  :straight (:type git
              :host github
              :repo "adamWithF/org-link-archive"
              :branch "main"))

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

(use-package org-appear
  :after org
  :commands org-appear
  :custom
  (org-appear-delay 0.6)
  (org-hide-emphasis-markers t))

(defun my/org-roam-file-p (&optional file)
  (when (fboundp 'org-roam-file-p)
    (org-roam-file-p file)))

(add-to-list 'magic-mode-alist '(my/org-roam-file-p . my/org-roam-mode))

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

(provide 'my-org)
;;; my-org.el ends here
