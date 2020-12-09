;; -*- lexical-binding: t; -*-

(use-package elfeed
  :bind (:map elfeed-show-mode-map
          ("SPC" . 'elfeed-scroll-up-command)
          ("S-SPC" . 'elfeed-scroll-down-command)
          ("B" . 'elfeed-show-eww-open)
          :map elfeed-search-mode-map
          ("B" . 'elfeed-search-eww-open)
          ("f" . 'jarfar/hydra-elfeed-filter/body)
          ("A" . 'my/elfeed-show-all)
          ("D" . 'my/elfeed-show-daily)
          ("q" . 'my/elfeed-save-db-and-bury)
          ("d" . 'elfeed-youtube-download)
          ("o" . 'jarfar/elfeed-tag-toggle-ok)
          ("j" . 'jarfar/elfeed-tag-toggle-junk)
          ("m" . 'jarfar/elfeed-send-emails)
          ("M" . (lambda () (interactive) (jarfar/elfeed-send-emails t))))
  :config
  (setq elfeed-search-filter "+unread -skip -ok -junk -indie")
  (setq elfeed-search-title-max-width 115)
  (setq elfeed-search-remain-on-entry t))

(use-package elfeed-goodies
  :after elfeed
  :bind (:map elfeed-show-mode-map
          ("n" . 'elfeed-goodies/split-show-next)
          ("p" . 'elfeed-goodies/split-show-prev))
  :config
  ;; (elfeed-goodies/setup)
  (setq elfeed-goodies/entry-pane-position 'bottom)
  (setq elfeed-show-entry-switch 'elfeed-goodies/switch-pane)
  (setq elfeed-show-entry-delete 'elfeed-goodies/delete-pane))

(use-package elfeed-org
  :after (org elfeed)
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.elfeed/feeds.org")))

(use-package elfeed-web
  :after elfeed)

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

(defun jarfar/elfeed-update ()
  (elfeed-update)
  (elfeed-web-update))

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
  "Elfeed custom cleanup on exit."
  (elfeed-db-save)
  (elfeed-db-compact))

;; https://noonker.github.io/posts/2020-04-22-elfeed/
(defun elfeed-youtube-download (&optional use-generic-p)
  "Youtube-DL link"
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (jarfar/youtube-download it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(defun elfeed-scroll-up-command (&optional arg)
  "Scroll up or go to next feed item in Elfeed"
  (interactive "^P")
  (let ((scroll-error-top-bottom nil))
    (condition-case-unless-debug nil
        (scroll-up-command arg)
      (error (elfeed-show-next)))))

(defun elfeed-scroll-down-command (&optional arg)
  "Scroll up or go to next feed item in Elfeed"
  (interactive "^P")
  (let ((scroll-error-top-bottom nil))
    (condition-case-unless-debug nil
        (scroll-down-command arg)
      (error (elfeed-show-prev)))))

(defun jarfar/elfeed-send-emails (&optional all)
  (interactive)
  (let ((entries (if all
                   (progn
                     (elfeed-search-first-entry)
                     (set-mark-command nil)
                     (elfeed-search-last-entry)
                     (elfeed-search-selected))
                   (elfeed-search-selected)
                   )))
    (when (use-region-p) (deactivate-mark))

    (cl-loop for entry in entries
      when (elfeed-entry-title entry)
      ;; if (elfeed-tagged-p 'unread entry)
      if (elfeed-tagged-p 'mail entry)
      do (message "The email for entry was already send. No email send now.")
      else
      do (jarfar/elfeed-send-emails-processing entry)
      ;; else
      ;; do (message "The entry is not tagged as 'ok'. No email send now.")
      ;; else
      ;; do (message "The entry is already read. No email send now.")
     )

    (mapc #'elfeed-search-update-entry entries))
  (elfeed-search-update--force)
  )

(defun jarfar/elfeed-send-emails-processing (entry)
  (interactive)
  (let (
         (link (elfeed-entry-link entry))
         (title (elfeed-entry-title entry))
         (name (elfeed-feed-title (elfeed-entry-feed entry)))
         )
    (compose-mail elfeed-email-destination (format "RSS: [%s] %s" name title) nil nil)

    (when link
      (mail-text)
      (let* ((type (elfeed-entry-content-type entry))
              (content (elfeed-deref (elfeed-entry-content entry))))
        (if content
          (progn
            (insert "<#multipart type=alternative>")
            (insert "<#part type=text/plain>")
            (insert content)
            (insert "\n----\n")
            (insert (concat "Link to article: " link "\n"))

            (when (eq type 'html)
              (insert "<#part type=text/html>")
              (insert content)
              (insert "<br>----<br>")
              (insert (concat "Link to article: " link "<br>"))
              )
            (insert "<#/multipart>")
            )
          (insert (propertize "(empty)\n" 'face 'italic)))
        )

      (message-send-and-exit)
      (elfeed-tag entry 'mail)
      (elfeed-tag entry 'ok)
      (elfeed-untag entry 'unread)
      )))

(defun jarfar/elfeed-tag-toggle-ok ()
  "Toggle tag 'ok' on an entry or entries, and send email with the content."
  (interactive)
  (elfeed-search-untag-all 'junk)
  (elfeed-search-toggle-all 'ok)
  (unless (use-region-p) (next-line)))

(defun jarfar/elfeed-tag-toggle-junk ()
  (interactive)
  (elfeed-search-untag-all 'ok)
  (elfeed-search-untag-all-unread)
  (elfeed-search-toggle-all 'junk)
  (unless (use-region-p) (next-line)))

(defun elfeed-show-eww-open (&optional use-generic-p)
  "open with eww"
  (interactive "P")
  (let ((browse-url-browser-function #'eww-browse-url))
    (elfeed-show-visit use-generic-p)))

(defun elfeed-search-eww-open (&optional use-generic-p)
  "open with eww"
  (interactive "P")
  (let ((browse-url-browser-function #'eww-browse-url))
    (elfeed-search-browse-url use-generic-p)))

(defhydra jarfar/hydra-elfeed-filter ()
  "Elfeed"
  ("b" (elfeed-search-set-filter "+business") "Show Business" :exit t)
  ("k" (elfeed-search-set-filter "+marketing") "Show Marketing" :exit t)
  ("e" (elfeed-search-set-filter "+entr") "Show Entr" :exit t)
  ("s" (elfeed-search-set-filter "+startup") "Show Startup" :exit t)
  ("h" (elfeed-search-set-filter "+growth") "Show Growth Hacking" :exit t)
  ("a" (elfeed-search-set-filter "+saas") "Show SaaS" :exit t)
  ("o" (elfeed-search-set-filter "+seo") "Show SEO" :exit t)
  ("g" (elfeed-search-set-filter "+blog") "Show Blogging" :exit t)
  ("c" (elfeed-search-set-filter "+copy") "Show Copywriting" :exit t)
  ("f" (elfeed-search-set-filter "+finances") "Show Finances" :exit t)
  ("m" (elfeed-search-set-filter "+social") "Show Social Media" :exit t)
  ("y" (elfeed-search-set-filter "+crypto") "Show Crypto" :exit t)
  ("n" (elfeed-search-set-filter "+news") "Show News" :exit t)
  ("l" (elfeed-search-set-filter "+ok") "Show Read Later" :exit t)
  ("j" (elfeed-search-set-filter "+junk") "Show Junk" :exit t))

;; (bind-key "m" 'jarfar/elfeed-to-mail elfeed-search-mode-map)

(defalias 'rss #'my/elfeed-load-db-and-open)

(provide 'my-rss)