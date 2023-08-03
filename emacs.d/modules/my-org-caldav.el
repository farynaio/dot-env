(use-package org-caldav
  :after org
  :config
  (progn
    (setq my/caldav-directory "~/.emacs.d/caldav")
    (setq org-caldav-url 'google)
    (setq org-caldav-delete-calendar-entries 'always)
    (setq org-caldav-delete-org-entries 'never)
    (setq org-caldav-files org-agenda-files)
    (setq org-caldav-sync-direction 'org->cal)
    ;; (setq org-caldav-skip-conditions '(nottodo))

    (setq org-caldav-save-directory my/caldav-directory)
    (make-directory org-caldav-save-directory t)

    (setq org-caldav-backup-file "/tmp/emacs/org-caldav-backup.org")
    (setq org-icalendar-combined-agenda-file (expand-file-name "org.ics" org-caldav-save-directory))
    (setq org-caldav-select-tags '("work"))
    (setq org-caldav-inbox (expand-file-name "google.org" my/org-agenda-directory))
    (setq org-caldav-show-sync-result nil)

    (defun my/org-caldav-delete-everything ()
      "Delete all entries from remote calendar."
      (interactive)
      (org-caldav-delete-everything t))

    (defalias 'caldav-sync #'org-caldav-sync)))

(provide 'my-org-cal-dav)