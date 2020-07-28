(use-package elfeed
  :bind (:map elfeed-search-mode-map
              ("A" . my/elfeed-show-all)
              ("D" . my/elfeed-show-daily)
              ("q" . my/elfeed-save-db-and-bury)))

(use-package elfeed-goodies
  :config
  (elfeed-goodies/setup)
  (setq elfeed-goodies/entry-pane-position 'bottom))

(use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/Dropbox/emacs/orgs/rss_feeds.org")))

;;http://pragmaticemacs.com/emacs/read-your-rss-feeds-in-emacs-with-elfeed/
;;functions to support syncing .elfeed between machines
;;makes sure elfeed reads index from disk before launching
(defun my/elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed-update)
  (elfeed-search-update--force)
  (elfeed))

;;http://pragmaticemacs.com/emacs/read-your-rss-feeds-in-emacs-with-elfeed/
;;write to disk when quiting
(defun my/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

(defun my/elfeed-show-all ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-all"))

(defun my/elfeed-show-daily ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-daily"))

(defun my/elfeed-kill ()
  ""
  (elfeed-db-save)
  (elfeed-db-compact))

(add-hook 'kill-emacs-hook  #'my/elfeed-kill)

(defalias 'rss #'my/elfeed-load-db-and-open)

(provide 'my-rss)