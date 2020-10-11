(setq my/emacs-directory "~/.emacs.d")
(setq my/org-base-path "~/Documents/emacs")
(setq my/org-roam-directory (expand-file-name "~/Documents/Dropbox/roam"))
(setq my/org-roam-journal-directory (expand-file-name "journal" my/org-roam-directory))
(setq my/tools-path (expand-file-name "tools" my/org-base-path))
(setq my/fast-path (expand-file-name "others" "~/Documents/Dropbox/emacs"))
(setq org-agenda-directory (expand-file-name "agenda" "~/Documents/Dropbox/emacs"))
(setq org-directory (expand-file-name "orgs" my/org-base-path))
(setq org-gpg-directory (expand-file-name "private" my/org-base-path))
(setq my/org-topics-directory (expand-file-name "topics" my/org-base-path))
(setq my/drills-base-path (expand-file-name "drills" my/org-base-path))

(setq my/org-archive-path (expand-file-name "archive" my/org-base-path))
(setq my/org-archive-tasks-path (expand-file-name "tasks_archive.org" my/org-archive-path))

(setq my/org-english-drill-file-path (expand-file-name "english_drill.org" my/drills-base-path))

(setq my/org-girls-file-path (expand-file-name "friends.org" org-directory))

;; Tasks
(setq my/org-backlog-file-path (expand-file-name "backlog.org" org-agenda-directory))
(setq my/org-events-file-path (expand-file-name "events.org" org-agenda-directory))
(setq my/org-inbox-file-path (expand-file-name "inbox.org" org-agenda-directory))
(setq my/org-tasks-file-path (expand-file-name "tasks.org" org-agenda-directory))
(setq my/org-taxes-file-path (expand-file-name "taxes.org" org-agenda-directory))
(setq my/org-tasks-maybe-someday-file-path (expand-file-name "maybe_someday.org" org-agenda-directory))

(setq my/org-languages-file-path (expand-file-name "languages.org" my/fast-path))
(setq my/org-yearly-goals-file-path (expand-file-name "yearly_goals.org" my/fast-path))
(setq my/org-goals-file-path (expand-file-name "goals.org" my/fast-path))

(setq my/org-media-reviews-file-path (expand-file-name "media_reviews.org" org-directory))
(setq my/org-diet-log-file-path (expand-file-name "health/diet.org" org-directory))
(setq my/org-projects-file-path (expand-file-name "projects.org" org-directory))
(setq my/org-blog-file-path (expand-file-name "blog_post_ideas.org" org-gpg-directory))
(setq my/org-media-file-path (expand-file-name "media.org" org-directory))
(setq my/org-tools-file-path (expand-file-name "tools.org" org-directory))

(setq my/org-ideas-file-path (expand-file-name "ideas.org.gpg" my/org-topics-directory))
(setq my/org-contacts-file-path (expand-file-name "contacts.org.gpg" org-gpg-directory))
(setq my/org-journal-file-path (expand-file-name "journal.org" org-directory))
(setq my/org-journal-dating-file-path (expand-file-name "journal_dating.org.gpg" org-gpg-directory))
(setq my/org-review-file-path (expand-file-name "reviews.org.gpg" org-gpg-directory))

(setq my/local-config-file-path (expand-file-name "local-config.el" user-emacs-directory))
(setq my/org-knowledge-file-path (expand-file-name "knowledge.org" org-directory))
(setq my/org-quotes-file-path (expand-file-name "quotes.org" org-directory))
(setq my/org-projects-folder (expand-file-name "projects" my/org-base-path))

;; Projects
(setq my/org-project-become-confident-pua (expand-file-name "become_confident_pua.org" my/org-projects-folder))
(setq my/org-project-trip-nottingham (expand-file-name "trip_to_nottingham.org" my/org-projects-folder))
(setq my/org-project-trip-edinburgh (expand-file-name "trip_to_edinburgh.org" my/org-projects-folder))
(setq my/org-project-guru (expand-file-name "guru.org.gpg" my/org-projects-folder))
(setq my/org-project-service-arbitrage (expand-file-name "service_arbitrage.org.gpg" my/org-projects-folder))
(setq my/org-project-best-offers-club (expand-file-name "best_offers_club.org.gpg" my/org-projects-folder))
(setq my/org-project-indie-dev (expand-file-name "indie-dev.org.gpg" my/org-projects-folder))
;; (setq my/org-project-setup-digital-agency (expand-file-name "setup_digital_agency.org.gpg" my/org-projects-folder))
;; (setq my/org-project-setup-career-it-blog (expand-file-name "setup_career_it_blog.org.gpg" my/org-projects-folder))
;; (setq my/org-project-setup-freelance (expand-file-name "setup_freelance.org.gpg" my/org-projects-folder))
;; (setq my/org-project-launch-amazon-business (expand-file-name "launch_amazon_business.org.gpg" my/org-projects-folder))
(setq my/org-project-switch-to-self-accounting (expand-file-name "switch_to_self_accounting.org.gpg" my/org-projects-folder))
;; (setq my/org-project-change-hosting-provider (expand-file-name "change_hosting_provider.org.gpg" my/org-projects-folder))
;; (setq my/org-project-launch-diy-app (expand-file-name "launch_diy_app.org.gpg" my/org-projects-folder))

(setq my/org-ledger-base-path (expand-file-name "ledger" my/org-base-path))
(setq my/ruby-gems-path "~/.rbenv/versions/2.3.3/bin/")

(provide 'my-path)
