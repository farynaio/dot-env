(defgroup my-writing nil "Writer config.")

(use-package langtool
  :init
  (progn
    (setq langtool-language-tool-jar (expand-file-name "languagetool-commandline.jar" my/tools-path)))
  :config
  (progn
    (setq langtool-default-language "en-GB")
    (setq langtool-mother-tongue "en")))

(use-package artbollocks-mode
  :config
  (progn
    (setq artbollocks-weasel-words-regex
      (concat "\\b" (regexp-opt
                      '("one of the"
                         "should"
                         "just"
                         "sort of"
                         "a lot"
                         "probably"
                         "maybe"
                         "perhaps"
                         "I think"
                         "really"
                         "pretty"
                         "nice"
                         "action"
                         "utilize"
                         "leverage") t) "\\b")
      artbollocks-jargon nil)
    (add-hook 'text-mode-hook 'artbollocks-mode)))

(setq safe-local-variable-values '((ispell-dictionary . "pl")))

;; mode hooks
(setq flyspell-mode-hooks '(text-mode-hook org-mode-hook))

(if (executable-find "aspell")
  (dolist (i flyspell-mode-hooks)
    (add-hook i #'flyspell-prog-mode)))

(add-to-list 'ispell-skip-region-alist '(":PROPERTIES:" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))

;; TODO it could be rather based on ring implementation (hard to add new langs)
(defun dict-toggle ()
  "Toggle spell dictionary."
  (interactive)
  (if
    (string= ispell-current-dictionary "en")
    (ispell-change-dictionary "pl")
    (ispell-change-dictionary "en")
    )
  (message (concat "Current spell language is '" ispell-current-dictionary "'.")))

;; org mode / journal
(setq
  org-clock-into-drawer t
  org-log-into-drawer t)
(setq org-clock-persist t) ; or 'history?
(setq org-clock-idle-time 2) ; TODO requires testing
(setq org-lowest-priority 68)
(setq org-highest-priority 65)
(setq org-default-priority 65)
(setq org-log-done 'time)


(setq org-default-notes-file my/org-notes-file-path)
(setq org-contacts-files `(,my/org-contacts-file-path))
;; (setq org-journal-dir (expand-file-name "journal" user-emacs-directory))
;; (setq org-default-notes-file (expand-file-name "notes.org" org-directory))
(setq org-caldav-save-directory my/tmp-base-path)
(setq org-icalendar-combined-agenda-file (expand-file-name "org.ics" org-caldav-save-directory))
(setq org-caldav-inbox (expand-file-name "google.org.gpg" org-agenda-directory))
(setq org-refile-targets `((,my/org-tasks-file-path :level . 1)
                            (,my/org-active-file-path :level . 1)
                            (,my/org-repeatables-file-path :level . 1)
                            (,my/org-projects-file-path :maxlevel . 3)))
(setq org-agenda-file
  (delq nil
    (mapcar (lambda (x) (and x (file-exists-p x) x))
      (list my/org-active-file-path
        my/org-anniversaries-file-path
        my/org-repeatables-file-path
         my/org-tasks-file-path))))

(setq org-enforce-todo-dependencies t)
(setq org-agenda-dim-blocked-tasks t) ; or "invisible"
(setq org-track-ordered-property-with-tag t)
(setq org-use-property-inheritance t)
(setq org-use-speed-commands t)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-priority-start-cycle-with-default nil)
(setq org-columns-default-format "%25ITEM(Task) %TODO %3PRIORITY %7Effort %8CLOCKSUM %TAGS")
;; (setq org-completion-use-ido t)
(setq org-export-exclude-category (list "google" "private"))
(setq org-export-babel-evaluate nil)
(setq org-ascii-links-to-notes nil)
(setq org-ascii-headline-spacing '(1 . 1))
(setq org-export-with-smart-quotes t) ; could cause problems on babel export
(setq org-icalendar-use-scheduled '(todo-start event-if-todo))
(setq org-icalendar-use-deadline '(event-if-todo))
(setq org-icalendar-honor-noexport-tag t) ; this is not supported in my version
(setq org-adapt-indentation nil)
(setq org-list-description-max-indent 5)
(setq org-agenda-inhibit-startup t)
(setq org-agenda-use-tag-inheritance nil)
(setq org-closed-keep-when-no-todo t)
(setq org-log-done-with-time nil)
(setq org-deadline-warning-days 5)
;; (setq org-tags-column -100)
(setq org-reverse-note-order t)
(setq org-global-properties '(("Effort_ALL" . "0:05 0:15 0:30 1:00 2:00 4:00")))
(setq org-clock-report-include-clocking-task t)
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-pretty-entities t)
(setq org-clock-in-resume t)
(setq org-clock-persist-query-resume nil)
(setq org-clock-in-switch-to-state "IN-PROCESS")
(setq org-clock-out-when-done (list "TODO" "BLOCKED" "WAITING"))
(setq org-agenda-scheduled-leaders '("" ""))
;; (setq org-agenda-window-setup 'current-window)
(setq org-return-follows-link nil)
(setq org-caldav-url 'google)
(setq org-icalendar-timezone "Europe/London") ; or nil
(setq org-icalendar-alarm-time 60)
;; (setq org-caldav-skip-conditions '(nottodo))
(setq org-caldav-files (directory-files org-agenda-directory t "^[^.][^#]*\\.org"))
(setq org-caldav-delete-calendar-entries 'always)
(setq org-caldav-delete-org-entries 'never)
(setq plstore-cache-passphrase-for-symmetric-encryption t)
(setq org-agenda-file-regexp ".*org\(.gpg\)?$")

(org-remove-file org-caldav-inbox)
(setq org-icalendar-with-timestamps 'active)
(setq org-icalendar-include-todo t)
(setq org-icalendar-include-sexps t)
(setq org-icalendar-store-UID t)
(setq org-habit-show-habits-only-for-today nil)
(setq org-refile-use-outline-path t)

(setq org-blank-before-new-entry nil)
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
(setq org-odt-preferred-output-format "doc")
(setq org-agenda-start-on-weekday nil)
(setq org-fast-tag-selection-single-key t) ; expert ?

(if (executable-find "unoconv")
  (setq org-odt-convert-processes '(("unoconv" "unoconv -f %f -o %d %i")))
  (setq org-odt-convert-processes '(("unoconv" "unoconv -f %f -o %d.xls %i")))
  (message "No executable \"unoconv found\".")
  )

(setq org-tags-exclude-from-inheritance '("project") ; prj
      org-stuck-projects '("+project/-DONE" ("TODO") ()))

(setq org-capture-templates
  `(("i" "Inbox" entry (file ,my/org-inbox-file-path)
      "* NOTE %?
:PROPERTIES:
:CREATED: [%<%Y-%m-%d>]
:END:" :prepend nil :empty-lines-after 1 :kill-buffer t) ; wish :prepend t
     ("t" "Todo" entry (file+headline ,my/org-tasks-file-path "Tasks")
      "* TODO %?
:PROPERTIES:
:CREATED: [%<%Y-%m-%d>]
:END:" :prepend nil :empty-lines-after 1 :kill-buffer t) ; wish :prepend t
     ("p" "Blog post" entry (file+headline ,my/org-blog-file-path "Posts")
       "* %?
:PROPERTIES:
:CREATED: [%<%Y-%m-%d>]
:END:" :prepend nil :empty-lines-after 1 :kill-buffer t) ; wish :prepend t
     ("r" "Repeatable" entry (file+headline ,my/org-repeatables-file-path "Repeatables")
       "* TODO %?
  SCHEDULED: <%<%Y-%m-%d %a .+2d/4d>>
:PROPERTIES:
:CREATED: [%<%Y-%m-%d>]
:STYLE: habit
:END:" :prepend nil :empty-lines-after 1 :kill-buffer t) ; wish :prepend t
     ("m" "Media" entry (file+headline ,my/org-media-file-path "Media")
       "* TODO %\\3 %\\1 %\\2 %? %^g
:PROPERTIES:
:CREATED: [%<%Y-%m-%d>]
:TITLE: %^{What Title: }
:AUTHOR: %^{What author: }
:TYPE: %^{What type: |AUDIO|BOOK|MOVIE|PODCAST}
:EFFORT: %^{What effort: }
:RECOMMENDED: %^{Who recommended: }
:RATING: %^{What rating: |5|4|3|2|1}
:END:" :prepend nil :kill-buffer t)
     ("j" "Journal" entry (file ,my/org-journal-file-path)
       "* [%<%Y-%m-%d>]\n%?" :prepend nil :jump-to-captured t :empty-lines-after 1 :kill-buffer t)
     ("d" "Dating Journal" entry (file ,my/org-journal-dating-file-path)
       "* [%<%Y-%m-%d>]\n%?" :prepend nil :jump-to-captured t :empty-lines-after 1 :kill-buffer t)
     ;; ("n" "Note" entry (file+headline ,my/org-notes-file-path "Notes")
       ;; "* NOTE taken on %U \\\\
    ;; %?" :prepend nil :kill-buffer t)
     ;; ("n" "Add note to currently clocked entry" plain (clock)
     ;;   "- Note taken on %U \\\\ \n  %?" :prepend nil :empty-lines-after 1)
     ("c" "Contact" entry (file ,my/org-contacts-file-path) ;,(expand-file-name "contacts.org.gpg" org-directory))
       "* %(org-contacts-template-name)
:PROPERTIES:
:TITLE:
:ALIAS:
:COMPANY:
:ROLE:
:EMAIL: %(org-contacts-template-email)
:MOBILE:
:WORK_PHONE:
:ADDRESS:
:URL:
:BIRTHDAY:
:ITOLD_THEM_EMAIL:
:ITOLD_THEM_PHONE:
:NOTES:
:CREATED: [%<%Y-%m-%d>]
:END:" :prepend nil :kill-buffer t)))


(provide 'my-writing)
