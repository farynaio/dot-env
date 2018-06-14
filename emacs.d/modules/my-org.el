(use-package calfw-org)
(require 'org-agenda)
(require 'org-contacts)
(require 'org-caldav)
(require 'taskjuggler-mode)
(require 'calendar)

(defvar my/ruby-gems-path "~/.rbenv/versions/2.3.3/bin/")

(setq org-taskjuggler-process-command (concat my/ruby-gems-path "tj3 --silent --no-color --output-dir %o %f"))

(define-derived-mode my/taskjuggler-mode org-mode "TJ"
  "Major mode for TaskJuggler projects."
  (visual-line-mode 1)
  (add-hook 'after-save-hook #'org-taskjuggler-export-and-process nil t)
  (add-hook 'find-file-hook #'org-taskjuggler-export-process-and-open nil t)
  (add-hook 'find-file-hook
    (lambda ()
      (setq-local org-taskjuggler-reports-directory (concat (file-name-sans-extension (file-name-sans-extension (file-relative-name buffer-file-name))) "_reports"))
      ) nil t))

(use-package synosaurus
  :config
  (progn
    (setq synosaurus-backend 'synosaurus-backend-wordnet)))
    ;; (add-hook 'text-mode-hook #'synosaurus-mode)))

(use-package artbollocks-mode
  :config
  (progn
    (setq artbollocks-weasel-words-regex
      (concat "\\b" (regexp-opt
                      '("one of the" "should" "just" "sort of" "a lot" "probably" "maybe" "perhaps" "I think" "really" "pretty" "nice" "action" "utilize" "leverage"
                                        ; test
                         "clavicles" "collarbones" "tiny birds" "antlers" "thrumming" "pulsing" "wombs" "ribcage" "alabaster" "grandmother" "redacting fairytales" "retelling fairytales" "my sorrow" "the window speaking" "avocados" "the blank page" "marrow" "starlings" "giving birth" "giving birth to weird shit" "apples" "peeling back skin" "god" "the mountain trembling" "poetry is my remedy" "sharp fragments" "shards" "grandpa" "i can remember" "this is how it happened" "the pain" "greek myths" "poems about poems" "scars" "cold, stinging" "oranges" "the body" "struggles" "shadows" "the moon reflecting off the" "waves" "echoes in the night" "painted skies" "a hundred" "again and again" "peace, love" "whimsy" "brooklyn" "the summer solstice" "the lunar eclipse" "veins" "soul"
                         ) t) "\\b")
      artbollocks-jargon nil)))
    ;; (add-hook 'text-mode-hook #'artbollocks-mode)))

;; org agenda full month calendar
(defun cal ()
  "Full month calendar by calfw-org-calendar."
  (interactive)
  (cfw:open-org-calendar))

;; https://stackoverflow.com/questions/9547912/emacs-calendar-show-more-than-3-months
(defun jarfar/year-calendar (&optional year)
  "Generate a one year calendar that can be scrolled by year in each direction.
This is a modification of:  http://homepage3.nifty.com/oatu/emacs/calendar.html
See also: https://stackoverflow.com/questions/9547912/emacs-calendar-show-more-than-3-months"
  (interactive)
  (require 'calendar)
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

(defun jarfar/scroll-year-calendar-forward (&optional arg event)
  "Scroll the yearly calendar by year in a forward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (unless arg (setq arg 0))
  (save-selected-window
    (if (setq event (event-start event)) (select-window (posn-window event)))
    (unless (zerop arg)
      (let* (
              (year (+ displayed-year arg)))
        (jarfar/year-calendar year)))
    (goto-char (point-min))
    (run-hooks 'calendar-move-hook)))

(defun jarfar/scroll-year-calendar-backward (&optional arg event)
  "Scroll the yearly calendar by year in a backward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (jarfar/scroll-year-calendar-forward (- (or arg 1)) event))

(define-key calendar-mode-map "<" 'jarfar/scroll-year-calendar-backward)
(define-key calendar-mode-map ">" 'jarfar/scroll-year-calendar-forward)

(defalias 'year-calendar #'jarfar/year-calendar)

;; (use-package org-alert
  ;; :config (org-alert-enable))

(bind-key "C-c j" 'org-clock-goto) ;; jump to current task from anywhere

(eval-after-load 'org
  '(progn
     (setq org-startup-with-inline-images t)
     (bind-key "M-}"         #'forward-paragraph           org-mode-map)
     (bind-key "M-{"         #'backward-paragraph          org-mode-map)
     (bind-key "C-c C-r"     #'air-revert-buffer-noconfirm org-mode-map)
     (bind-key "C-c l"       #'org-store-link              org-mode-map)
     (bind-key "C-."         #'imenu-anywhere              org-mode-map)
     (bind-key "C-c C-x a"   #'org-archive-subtree-default org-mode-map)

    (define-key org-mode-map [remap org-evil-motion-forward-heading] #'forward-paragraph)
    (define-key org-mode-map [remap org-evil-motion-backward-heading] #'backward-paragraph)
     (bind-key "C-x :"
       (lambda ()
         (interactive)
         "Insert tags in a capture window without losing the point"
         (save-excursion
           (org-back-to-heading)
           (org-set-tags))))
     (unbind-key "C-c $"       org-mode-map) ; removed archive subtree shortcut
     (unbind-key "C-c C-x C-a" org-mode-map) ; remove archive subtree default shortcut
     (unbind-key "C-c C-x C-s" org-mode-map) ; remove archive subtree shortcut
     (unbind-key "C-c C-x A"   org-mode-map) ; remove archive to archive siblings shortcut

     (advice-add #'org-refile :after
       (lambda (&rest args) (org-save-all-org-buffers)))

     (advice-add #'org-archive-subtree-default :after
       (lambda () (org-save-all-org-buffers)))

     (advice-add #'org-clock-in  :after (lambda (&rest args) (org-save-all-org-buffers)))
     (advice-add #'org-clock-out :after (lambda (&rest args) (org-save-all-org-buffers)))

     (add-hook 'org-mode-hook
       (lambda ()
         (evil-define-key '(motion normal) org-mode-map
            (kbd "C-c C-s") #'org-schedule)
         (hl-line-mode)
         (setq-local paragraph-start "[:graph:]+$")
         (setq-local paragraph-separate "[:space:]*$")
         (abbrev-mode t)))))

(eval-after-load 'org-caldav
  '(progn
    (setq org-caldav-url 'google)
    (setq org-caldav-files (directory-files org-agenda-directory t "^[^.][^#]*\\.org"))
    (setq org-caldav-delete-calendar-entries 'always)
    (setq org-caldav-delete-org-entries 'never)
    (setq org-caldav-save-directory my/tmp-base-path)
    (setq org-icalendar-combined-agenda-file (expand-file-name "org.ics" org-caldav-save-directory))
    (setq org-caldav-inbox (expand-file-name "google.org.gpg" org-agenda-directory))
    (org-remove-file org-caldav-inbox)))

(use-package org-evil)

(eval-after-load 'org-agenda
  '(progn
     (bind-key "C-c C-c" #'org-agenda-set-tags org-agenda-mode-map)
     (bind-key "C-d" #'evil-scroll-down org-agenda-mode-map)
     (bind-key "C-u" #'evil-scroll-up org-agenda-mode-map)))

(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-x a") #'org-agenda)
(bind-key "C-c C-o" #'org-open-at-point-global)

(use-package japanese-holidays)
(defvar english-holidays
  '(
    (holiday-fixed  3 30 "(bank) Good Friday")
    (holiday-fixed  5  7 "(bank) Early May bank holiday (England, Wales)")
    (holiday-fixed  8 27 "(bank) Spring bank holiday")
    (holiday-fixed  8 28 "(bank) Summer bank holiday")
    (holiday-fixed 12 25 "(bank) Christmas Day")
    (holiday-fixed 12 26 "(bank) Boxing Day")))
(defvar polish-holidays
  '(
     (holiday-fixed  1  6 "(bank) Trzech Króli")
     (holiday-fixed  1 21 "Dzień Babci")
     (holiday-fixed  1 22 "Dzień Diadka")
     (holiday-fixed  2 22 "Ofiarowanie Pańskie (Matki Boskiej Gromnicznej)")
     (holiday-fixed  2  8 "Tłusty Czwartek")
     (holiday-fixed  2 10 "Ostatnia Sobota Karnawału")
     (holiday-fixed  2 13 "Ostatki")
     (holiday-fixed  2 14 "Walentynki")
     (holiday-fixed  3  1 "Narodowy Dzień Pamięci Żołnierzy Wyklętych")
     (holiday-fixed  3  8 "Międzynarodowy Dzień Kobiet")
     (holiday-fixed  3 10 "Dzień Mężczyzn")
     (holiday-fixed  3 20 "Początek Astronomicznej Wiosny")
     (holiday-fixed  3 25 "Zmiana czasu z zimowego na letni")
     (holiday-fixed  3 25 "Niedziela Palmowa")
     (holiday-fixed  3 29 "Wielki Czwartek")
     (holiday-fixed  3 30 "Wielki Piątek")
     (holiday-fixed  3 31 "Wielka Sobota")
     (holiday-fixed  4  1 "(bank) Wielkanoc")
     (holiday-fixed  4  1 "Prima Aprilis")
     (holiday-fixed  4  2 "(bank) Poniedziałek Wielkanocny")
     (holiday-fixed  4  8 "Święto Bożego Miłosierdzia")
     (holiday-fixed  4 22 "Międzynarodowy Dzień Ziemi")
     (holiday-fixed  5  1 "(bank) Międzynarodowe Święto Pracy")
     (holiday-fixed  5  2 "Dzień Flagi Rzeczypospolitej Polskiej")
     (holiday-fixed  5  3 "(bank) Święto Konstytucji 3 Maja")
     (holiday-fixed  5 13 "Wniebowstąpienie")
     (holiday-fixed  5 20 "(bank) Zesłanie Ducha Świętego (Zielone Świątki)")
     (holiday-fixed  5 26 "Dzień Matki")
     (holiday-fixed  5 31 "(bank) Boże Ciało")
     (holiday-fixed  6  1 "Międzynarodowy Dzień Dziecka")
     (holiday-fixed  6 21 "Pierwszy Dzień Lata (najdłuższy dzień roku)")
     (holiday-fixed  6 23 "Dzień Ojca")
     (holiday-fixed  8  1 "Narodowy Dzień Pamięci Powstania Warszawskiego")
     (holiday-fixed  8 15 "(bank) Święto Wojska Polskiego")
     (holiday-fixed  8 15 "Wniebowzięcie Najświętrzej Maryi Panny")
     (holiday-fixed  8 31 "Dzień Solidarności i Wolności")
     (holiday-fixed  9 23 "Początek Astronomicznej Jesieni")
     (holiday-fixed  9 30 "Dzień Chłopaka")
     (holiday-fixed 10 14 "Dzień Nauczyciela (Dzień Edukacji Narodowej)")
     (holiday-fixed 10 28 "Zmiana czasu z letniego na zimowy")
     (holiday-fixed 11  1 "(bank) Wszystkich Świętych")
     (holiday-fixed 11  2 "Dzień Zaduszny")
     (holiday-fixed 11 11 "(bank) Narodowe Święto Niepodległości")
     (holiday-fixed 11 29 "Andrzejki")
     (holiday-fixed 12  4 "Barbórka (Dzień górnika, naftowca i gazownika)")
     (holiday-fixed 12  6 "Dzień św. Mikołaja")
     (holiday-fixed 12 21 "Początek Astronomicznej Zimy")
     (holiday-fixed 12 24 "Wigilia Bożego Narodzenia")
     (holiday-fixed 12 25 "(bank) Boże Narodzenie (1 dzień)")
     (holiday-fixed 12 26 "(bank) Boże Narodzenie (2 dzień)")))

(setq calendar-holidays
  (append
    japanese-holidays
    polish-holidays
    english-holidays
    holiday-local-holidays
    holiday-other-holidays
    holiday-general-holidays
    holiday-christian-holidays
    holiday-oriental-holidays
    holiday-solar-holidays
    ))

(setq calendar-christian-all-holidays-flag t)
(setq holiday-bahai-holidays nil)
(setq holiday-local-holidays nil) ; set it one day
(setq org-agenda-include-diary t)

(setq diary-number-of-entries 31)
(setq calendar-mark-holidays-flag t)
(setq calendar-week-start-day 1)
(setq calendar-date-style 'european)

(add-hook 'calendar-load-hook
  (lambda ()
    (calendar-set-date-style 'european)))

(setq
  org-clock-into-drawer t
  org-log-into-drawer t)
(setq org-clock-persist t) ; or 'history?
(setq org-clock-idle-time 2) ; TODO requires testing
(setq org-lowest-priority 68)
(setq org-highest-priority 65)
(setq org-default-priority 66)
(setq org-log-done 'time)

(setq org-default-notes-file my/org-notes-file-path)
(setq org-contacts-files `(,my/org-contacts-file-path))
;; (setq org-journal-dir (expand-file-name "journal" user-emacs-directory))
;; (setq org-default-notes-file (expand-file-name "notes.org" org-directory))

(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets `((nil :level . 1)
                            (,my/org-tasks-file-path :level . 1) ; pool of tasks
                            (,my/org-active-file-path :level . 1)
                            (,my/org-repeatables-file-path :level . 1)
                            (,my/org-project-trip-nottingham :level . 1)
                            (,my/org-project-trip-edinburgh :level . 1)
                            (,my/org-project-become-confident-pua :level . 1)
                            (,my/org-project-launch-amazon-business :level . 1)
                            (,my/org-project-setup-freelance :level . 1)
                            (,my/org-project-setup-digital-agency :level . 1) ; particular projects
                            (,my/org-projects-file-path :level . 1)))
(setq org-agenda-files
  (delq nil
    (mapcar (lambda (x) (and x (file-exists-p x) x))
      (list my/org-active-file-path
        my/org-anniversaries-file-path
        my/org-repeatables-file-path
        my/org-projects-file-path
        my/org-media-file-path
         my/org-tasks-file-path))))

(setq org-hide-emphasis-markers t)
(setq org-enforce-todo-dependencies nil)
(setq org-agenda-dim-blocked-tasks t) ; or "invisible"
;; (setq org-track-ordered-property-with-tag t)
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
(setq org-cycle-include-plain-lists t)
(setq org-hide-block-startup t)
(setq org-list-description-max-indent 5)
(setq org-agenda-inhibit-startup nil)
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
;; (setq org-clock-out-when-done (list "TODO" "BLOCKED" "WAITING" "DONE" "DELEGATED" "UNDOABLE"))
(setq org-clock-out-when-done t)
(setq org-agenda-scheduled-leaders '("" ""))
;; (setq org-agenda-window-setup 'current-window)
(setq org-return-follows-link nil)
(setq org-icalendar-timezone "Europe/London") ; or nil
(setq org-icalendar-alarm-time 60)
;; (setq org-caldav-skip-conditions '(nottodo))
(setq plstore-cache-passphrase-for-symmetric-encryption t)
(setq org-agenda-file-regexp ".*org\(.gpg\)?$")

(setq org-icalendar-with-timestamps 'active)
(setq org-icalendar-include-todo t)
(setq org-icalendar-include-sexps t)
(setq org-icalendar-store-UID t)
(setq org-habit-show-habits-only-for-today nil)
(setq org-habit-graph-column 62)
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

(setq org-tags-exclude-from-inheritance '("project" "taskjuggler_project" "taskjuggler_resource") ; prj
      org-stuck-projects '("+project/-DONE" ("TODO") ()))

(setq org-capture-templates
  `(("i" "Inbox" entry (file ,my/org-inbox-file-path)
"* NOTE %?
:PROPERTIES:
:CREATED: [%<%Y-%m-%d>]
:END:" :prepend t :empty-lines-after 1 :kill-buffer t)
     ("t" "Todo" entry (file+headline ,my/org-active-file-path "Tasks")
"* TODO %?
:PROPERTIES:
:CREATED: [%<%Y-%m-%d>]
:END:" :prepend t :empty-lines-after 1 :kill-buffer t)
     ("p" "Blog post" entry (file+headline ,my/org-blog-file-path "Posts")
"* %?
:PROPERTIES:
:CREATED: [%<%Y-%m-%d>]
:END:" :prepend t :empty-lines-after 1 :kill-buffer t)
     ("w" "New word" item (file+headline ,my/org-languages-file-path "New")
       "- %?" :prepend t :empty-lines-after 1 :kill-buffer t)
     ("q" "Quote" entry (file+headline ,my/org-quotes-file-path "Quotes")
      "* %?" :prepend nil :kill-buffer t)
     ("r" "Repeatable" entry (file+headline ,my/org-repeatables-file-path "Repeatables")
"* TODO %?
SCHEDULED: <%<%Y-%m-%d %a .+2d/4d>>
:PROPERTIES:
:CREATED: [%<%Y-%m-%d>]
:STYLE: habit
:END:" :prepend t :empty-lines-after 1 :kill-buffer t)
     ("u" "Review" entry (file ,my/org-review-file-path)
"* [%<%Y-%m-%d>]
:PROPERTIES:
:CREATED: [%<%Y-%m-%d>]
:END:

+ What's good:
  + %?

+ What's could be better:

+ What to work on:

" :prepend t :empty-lines-after 1 :jump-to-captured t)
     ("m" "Media" entry (file+headline ,my/org-media-file-path "Media")
"* TODO %\\3 \"%\\1\" %\\2 %? %^g
:PROPERTIES:
:CREATED: [%<%Y-%m-%d>]
:TITLE: %^{What Title: }
:AUTHOR: %^{What author: }
:TYPE: %^{What type: |AUDIO|BOOK|MOVIE|PODCAST}
:EFFORT: %^{What effort: }
:RECOMMENDED: %^{Who recommended: }
:RATING: %^{What rating: |5|4|3|2|1}
:END:" :prepend t :kill-buffer t)
     ("j" "Journal" entry (file ,my/org-journal-file-path)
       "* [%<%Y-%m-%d>]\n%?" :prepend t :jump-to-captured t :empty-lines-after 1 :kill-buffer t)
     ("d" "Dating Journal" entry (file ,my/org-journal-dating-file-path)
       "* [%<%Y-%m-%d>]\n%?" :prepend t :jump-to-captured t :empty-lines-after 1 :kill-buffer t)
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
:END:" :prepend t :kill-buffer t)))

(setq org-todo-keywords
  '((sequence "TODO(t)" "IN-PROCESS(p)" "BLOCKED(b@/!)" "WAITING(w@/!)" "DELEGATED(e@/!)")
     (sequence "|" "DONE(d!)" "CANCELED(c@)" "UNDOABLE(u@)" "NOTE(n)")))

(setq org-todo-keyword-faces
  '(("TODO" . (:foreground "LimeGreen" :weight bold))
     ("IN-PROCESS" . (:foreground "IndianRed1" :weight bold))
     ("BLOCKED"    . (:foreground "OrangeRed" :weight bold))
     ("WAITING"    . (:foreground "coral" :weight bold))
     ("DELEGATED"  . (:foreground "coral" :weight bold))
     ("DONE"       . (:foreground "dark grey" :weight bold))
     ("CANCELED"   . (:foreground "dark grey" :weight bold))
     ("UNDOABLE"   . (:foreground "dark grey" :weight bold))
     ("NOTE")      . (:foreground "white" :weight bold)))

; from https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
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

(defun my/org-current-is-todo-p ()
  (string= "TODO" (org-get-todo-state)))

;; (setq org-agenda-custom-commands
;;   '(("o" "At the office" tags-todo "@office"
;;       ((org-agenda-overriding-header "Office")
;;         (org-agenda-skip-function #'my/org-agenda-skip-all-siblings-but-first)))))

;; (setq org-agenda-custom-commands
;;       '(("o" "At the office" tags-todo "@office"
;;          ((org-agenda-overriding-header "Office")))))

(setq my/org-active-projects (list
                               my/org-project-setup-freelance
                               my/org-project-launch-amazon-business
                               my/org-project-setup-digital-agency
                               my/org-project-become-confident-pua
                               my/org-project-trip-edinburgh
                               my/org-project-trip-nottingham
                               ))

(setq org-agenda-custom-commands
  '(("co" "TODOs weekly sorted by state, priority, deadline, scheduled, alpha and effort"
      ((agenda "*"))
      ((org-agenda-overriding-header "agenda weekly sorted by state, priority, deadline, scheduled, alpha and effort")
        (org-agenda-sorting-strategy '(todo-state-down priority-down deadline-down scheduled-down alpha-down effort-up))))
     ("cn" "TODOs not sheduled"
       ((todo "*"))
       ((org-agenda-skip-function
           '(or (org-agenda-skip-if nil '(scheduled))))
         (org-agenda-category-filter-preset '("-Holidays"))
         (org-agenda-overriding-header "TODOs not scheduled")
         (org-agenda-sorting-strategy '(deadline-down priority-down alpha-down effort-up))))
     ("cb" "TODOs blocked"
       ((tags "BLOCKED"))
       ((org-agenda-overriding-header "TODOs blocked")
         (org-agenda-sorting-strategy '(priority-down deadline-down alpha-down effort-up))))
     ("cc" "TODOs canceled"
       ((todo "CANCELED"))
       ((org-agenda-overriding-header "TODOs canceled")
         (org-agenda-sorting-strategy '(priority-down alpha-down effort-up))))
     ("cj" "Journal entries"
       ((search ""))
       ((org-agenda-files (list org-journal-dir))
         (org-agenda-overriding-header "Journal")
         (org-agenda-sorting-strategy '(timestamp-down))))
     ("cm" "agenda 1 month ahead"
       ((agenda ""
         ((org-agenda-sorting-strategy '(time-up todo-state-down habit-down))
           (org-agenda-span 'month)
           (org-agenda-remove-tags t)
           (ps-number-of-columns 2)
           (ps-landscape-mode t)))))
     ("cp" "Active projects"
       ((tags "PROJECT"))
       ((org-agenda-overriding-header "Active Projects")
         (org-tags-match-list-sublevels nil)
         (org-agenda-remove-tags t)
         (org-agenda-files my/org-active-projects)))
         ;; (org-agenda-files (list my/org-active-file-path my/org-projects-file-path))))
         ;; (org-agenda-files (list my/org-active-file-path my/org-projects-file-path))))
     ("z" "DONE tasks not archived"
       ((tags "TODO=\"DONE\"|TODO=\"CANCELED\"|TODO=\"UNDOABLE\""))
       ((org-agenda-overriding-header "DONE tasks not archived")
         (org-agenda-files (list my/org-active-file-path my/org-tasks-file-path my/org-projects-file-path))))
     ("d" "Coprehensive agenda"
      ;; ((tags "PRIORITY=\"A\"+TODO=\"TODO\"|TODO=\"IN-PROCESS\"|TODO=\"BLOCKED\"|TODO=\"WAITING\""
      ((tags-todo "PRIORITY=\"A\"|TODO=\"IN-PROCESS\""
         ((org-agenda-skip-function
            '(or
               (org-agenda-skip-entry-if 'todo '("DONE" "UNDOABLE" "CANCELED"))
               (my/org-agenda-skip-if-scheduled-later)
               (and
                 (org-agenda-skip-entry-if 'nottodo '("IN-PROCESS"))
                 (my/org-skip-subtree-if-priority ?A)
                 (org-agenda-skip-entry-if 'notscheduled))))
           (org-agenda-remove-tags t)
           (org-agenda-files (append org-agenda-files my/org-active-projects))
           (org-agenda-overriding-header "High-priority unfinished tasks:")
           (org-agenda-sorting-strategy '(time-up priority-down effort-down category-keep alpha-up))))
        (tags "PROJECT"
          ((org-agenda-overriding-header "Active projects:")
            (org-tags-match-list-sublevels nil)
            (org-agenda-remove-tags t)
            (org-agenda-files my/org-active-projects)))
        (tags "-TODO=\"DONE\"|-TODO=\"CANCELED\"|-TODO=\"UNDOABLE\""
          ((org-agenda-overriding-header "Goals:")
            (org-tags-match-list-sublevels nil)
            (org-agenda-remove-tags t)
            (org-agenda-files (list my/org-yearly-goals-file-path))))
        (tags "-TODO=\"DONE\"|-TODO=\"CANCELED\"|-TODO=\"UNDOABLE\""
          ((org-agenda-overriding-header "Reviews:")
            (org-agenda-skip-function 'org-review-agenda-skip)
            (org-tags-match-list-sublevels nil)
            (org-agenda-remove-tags t)
            (org-agenda-cmp-user-defined 'org-review-compare)
            (org-agenda-files (list my/org-knowledge-review-file-path))
            ;; (org-agenda-sorting-strategy '(time-up category-keep))
            (org-agenda-sorting-strategy '(user-defined-down))))
        (agenda ""
          ((org-agenda-sorting-strategy '(time-up priority-down todo-state-down effort-down habit-down))
            (org-agenda-remove-tags t)
            (ps-number-of-columns 2)
            (ps-landscape-mode t)
            (org-agenda-files (append org-agenda-files my/org-active-projects))
            ))
        (alltodo ""
          ((org-agenda-skip-function
             '(or (my/org-skip-subtree-if-priority ?A)
                (my/org-skip-subtree-if-habit)
                (org-agenda-skip-if nil '(scheduled deadline))
                (org-agenda-skip-entry-if 'todo '("IN-PROCESS" "BLOCKED" "WAITING"))))
            (org-agenda-sorting-strategy '(priority-down effort-down category-keep alpha-up))
            )
          )
        )
       )
     )
  )

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
    (setq mycatp (member mycategory org-export-exclude-category))
    ; return t if ok, nil when not ok
    (if (and myresult (not mycatp)) t nil)))

;; (defun air-org-cal-export ()
;;   (let ((org-icalendar-verify-function 'my/org-calendar-export-limit))
;;     (org-export-icalendar-combine-agenda-files)))

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

(org-clock-persistence-insinuate)

(setq org-tag-alist '(
                       ("health" . ?h) ; my energy level, my looks
                       ("fun" . ?u) ; relax, enjoy life
                       ("career" . ?c) ; my professional reputation, my credability, my professional skills, professional relationships
                       ("family" . ?f) ; my social network, my professional network
                       ("love" . ?l) ; my happiness, my ultimate goal, my real legacy
                       ("wealth" . ?w) ; my legacy
                       ("@poland" . ?n)
                       (:startgroup . nil)
                       ("@home" . ?o)
                       ("@office" . ?i)
                       ("@delegate" . ?d)
                       (:endgroup . nil)
                       (:startgroup . nil)
                       ("@phone" . ?p)
                       ("@computer" . ?m)
                       (:endgroup . nil)
                       ))

(add-to-list 'org-modules 'org-habit t)
(add-to-list 'org-modules 'org-collector t)
(add-to-list 'org-modules 'org-depend t)

(add-hook 'org-agenda-mode-hook #'hl-line-mode)

(use-package org-review
  :config
  (progn
    (bind-key "C-c C-r" #'org-review-insert-last-review org-agenda-mode-map)
    ))

(use-package langtool
  :init
  (progn
    (setq langtool-language-tool-jar (expand-file-name "LanguageTool/languagetool-commandline.jar" my/tools-path)))
  :config
  (progn
    (setq langtool-default-language "en-GB")
    (setq langtool-mother-tongue "en")))


(setq safe-local-variable-values '(
                                    (ispell-dictionary . "pl")
                                    (ispell-dictionary . "en")))

;; mode hooks
(setq flyspell-mode-hooks '(text-mode-hook org-mode-hook))

(if (executable-find "aspell")
  (dolist (i flyspell-mode-hooks)
    (add-hook i #'flyspell-prog-mode)))

(add-to-list 'ispell-skip-region-alist '(":PROPERTIES:" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))

(setq ispell-extra-args '("--sug-mode=ultra"))

;; TODO it could be rather based on ring implementation (hard to add new langs)
(defun dict-toggle ()
  "Toggle spell dictionary."
  (interactive)
  (if
    (string= ispell-current-dictionary "en")
    (ispell-change-dictionary "pl")
    (ispell-change-dictionary "en"))
  (message (concat "Current spell language is '" ispell-current-dictionary "'.")))

(provide 'my-org)