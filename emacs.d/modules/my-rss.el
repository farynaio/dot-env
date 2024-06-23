;; -*- lexical-binding: t; -*-

(use-package elfeed
  :commands elfeed-db-load
  :bind (:map elfeed-show-mode-map
          ("SPC" . elfeed-scroll-up-command)
          ("S-SPC" . elfeed-scroll-down-command)
          ("<mouse-1>" . shr-copy-url)
          ("<s-mouse-1>" . shr-copy-url)
          ("<S-mouse-1>" . shr-copy-url)
          ("C-x C-l" . shr-copy-url)
          ("l" . shr-copy-url)
          ("C-c C-l" . hydra-elfeed/body)
          :map shr-map
          ;; ("RET" . my/elfeed-shr-open-external)
          ;; ("v" . my/elfeed-shr-open-external)
          ;; ("O" . my/elfeed-shr-open-external)
          ("<mouse-1>" . shr-copy-url)
          ("<mouse-2>" . shr-copy-url)
          ("<S-mouse-1>" . shr-copy-url)
          ;; ("<mouse-1>" . my/elfeed-shr-open-click)
          ;; ("<mouse-2>" . my/elfeed-shr-open-click)
          ;; ("<S-mouse-1>" . my/elfeed-shr-open-click)
          :map elfeed-search-mode-map
          ("h" . hydra-elfeed-search/body)
          ("q" . my/elfeed-save-db-and-bury)
          ("d" . my/elfeed-youtube-download)
          ("o" . my/elfeed-tag-toggle-ok)
          ("f" . my/elfeed-tag-toggle-favorite)
          ("j" . my/elfeed-tag-toggle-junk)
          ("r" . my/elfeed-mark-read-move-next)
          ("u" . my/elfeed-mark-unread-move-next)
          ;; ("O" . my/elfeed-open-in-external-browser)
          ("g" . my/elfeed-update)
          ;; ("<S-mouse-1>" . my/elfeed-search-open-in-external-click)
          ;; ("<s-mouse-1>" . elfeed-search-browse-url)
          ("M" . (lambda () (interactive) (my/elfeed-send-emails t)))
          ("C-x m" . hydra-elfeed-search/body))
  :preface
  (defun my/elfeed-load-db-and-open ()
    "Wrapper to load the elfeed db from disk before opening"
    (interactive)
    (elfeed-db-load)
    (message "[%s] Updating feeds..." (format-time-string "%Y-%m-%d %H:%M:%S"))
    (elfeed-update)
    (elfeed-search-update--force)
    (elfeed-db-save)
    (elfeed))
  :custom
  (elfeed-db-directory (expand-file-name "rss" user-emacs-directory))
  (elfeed-search-filter "-ignore -ok -junk")
  (elfeed-search-title-max-width 115)
  (elfeed-search-remain-on-entry t)
  (elfeed-curl-timeout 200)
  (elfeed-web-limit 500)
  (url-queue-timeout 60)
  :config
  (pretty-hydra-define hydra-elfeed
    (:hint nil :color teal :quit-key "q" :title (with-faicon "comments-o" "RSS" 1 -0.05))
    ("Action"
      (
        ;; ("b" my/erc-browse-last-url "browse last url")
        ("c" my/erc-start-or-switch "connect")
        ("r" my/erc-reset-track-mode "reset track mode")))
    )

  (pretty-hydra-define hydra-elfeed-search
    (:hint nil :color teal :quit-key "q" :title (with-faicon "rss" "RSS search" 1 -0.05))
    ("Filter"
      (
        ;; ("b" (elfeed-search-set-filter "+business") "Show Business")
        ;; ("k" (elfeed-search-set-filter "+marketing") "Show Marketing")
        ("e" (elfeed-search-set-filter "+entreprenouriship") "Show entrepreneurship")
        ;; ("s" (elfeed-search-set-filter "+startup") "Show Startup")
        ;; ("h" (elfeed-search-set-filter "+growth") "Show Growth Hacking")
        ;; ("a" (elfeed-search-set-filter "+saas") "Show SaaS")
        ;; ("o" (elfeed-search-set-filter "+seo") "Show SEO")
        ;; ("g" (elfeed-search-set-filter "+blog") "Show Blogging")
        ;; ("c" (elfeed-search-set-filter "+copy") "Show Copywriting")
        ;; ("f" (elfeed-search-set-filter "+finances") "Show Finances")
        ;; ("m" (elfeed-search-set-filter "+social") "Show Social Media")
        ;; ("y" (elfeed-search-set-filter "+crypto") "Show Crypto")
        ("n" (elfeed-search-set-filter "+news") "Show news")
        ("l" (elfeed-search-set-filter "+ok") "Show read later")
        ("f" (elfeed-search-set-filter "+favorite") "Show favorite")
        ("j" (elfeed-search-set-filter "+junk") "Show junk"))
    "Action"
      (("m" my/elfeed-send-emails "Send emails")
        ("g" my/elfeed-update "Update feeds"))))

  (defun my/elfeed-search-open-in-external-click (event)
    "Open link with external browser"
    (interactive "e")
    (mouse-set-point event)
    (let ((urls (list))
           (entries (elfeed-search-selected)))
      (cl-loop for entry in entries
        do (elfeed-untag entry 'unread)
        when (elfeed-entry-link entry)
        do (push it urls))
      (mapc 'elfeed-search-update-entry entries)
      (dolist (url urls)
        (browse-url-generic url))))

  ;; based on shr-browse-url
  (defun my/elfeed-shr-open-click (event)
    (interactive "e")
    (mouse-set-point event)
    (let ((url (get-text-property (point) 'shr-url)))
      (cond
        ((not url)
          (message "No link under point"))
        ((string-match "^mailto:" url)
          (browse-url-mail url))
        (t
          (if (= (length (frame-list)) 1)
            (make-frame-command)
            (other-frame 1))
          (browse-url-generic url))))
    (shr--blink-link))

  ;;http://pragmaticemacs.com/emacs/read-your-rss-feeds-in-emacs-with-elfeed/
  ;;functions to support syncing .elfeed between machines
  ;;makes sure elfeed reads index from disk before launching
  (defun my/elfeed-open-in-external-browser (&optional use-generic-p)
    (interactive "P")
    (let ((urls (list))
           (entries (elfeed-search-selected)))
      (cl-loop for entry in entries
        do (elfeed-untag entry 'unread)
        when (elfeed-entry-link entry)
        do (push it urls))
      (mapc 'elfeed-search-update-entry entries)
      (unless (or elfeed-search-remain-on-entry (use-region-p))
        (forward-line))
      (dolist (url urls)
        (message url)
        (browse-url-generic url))))

  (defun my/elfeed-update ()
    (interactive)
    (message "[%s] Updating feeds..." (format-time-string "%Y-%m-%d %H:%M:%S"))
    (elfeed-update))

  ;; based on shr-browse-url
  (defun my/elfeed-shr-open-external (&optional mouse-event)
    (interactive (list last-nonmenu-event))
    (let ((url (get-text-property (point) 'shr-url)))
      (cond
        ((not url)
          (message "No link under point"))
        ((string-match "^mailto:" url)
          (browse-url-mail url))
        (t
	        (browse-url-generic url)
          (shr--blink-link)))))

  ;;http://pragmaticemacs.com/emacs/read-your-rss-feeds-in-emacs-with-elfeed/
  ;;write to disk when quiting
  (defun my/elfeed-save-db-and-bury ()
    "Wrapper to save the elfeed db to disk before burying buffer"
    (interactive)
    (elfeed-db-save)
    (quit-window))

  (defun my/elfeed-kill ()
    "Elfeed custom cleanup on exit."
    (elfeed-db-save)
    (elfeed-db-compact))

  ;; https://noonker.github.io/posts/2020-04-22-elfeed/
  (defun my/elfeed-youtube-download (&optional use-generic-p)
    "Youtube-DL link"
    (interactive "P")
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
        do (elfeed-untag entry 'unread)
        when (elfeed-entry-link entry)
        do (my/youtube-download it))
      (mapc 'elfeed-search-update-entry entries)
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

  (defun my/elfeed-send-emails (&optional all)
    (interactive)
    (let ((entries (if all
                     (progn
                       (elfeed-search-first-entry)
                       (set-mark-command nil)
                       (elfeed-search-last-entry)
                       (elfeed-search-selected))
                     (elfeed-search-selected))))
      (when (use-region-p) (deactivate-mark))

      (cl-loop for entry in entries
        when (elfeed-entry-title entry)
        ;; if (elfeed-tagged-p 'unread entry)
        if (elfeed-tagged-p 'mail entry)
        do (message "The email for entry was already send. No email send now.")
        else
        do (my/elfeed-send-emails-processing entry)
        ;; else
        ;; do (message "The entry is not tagged as 'ok'. No email send now.")
        ;; else
        ;; do (message "The entry is already read. No email send now.")
        )

      (mapc 'elfeed-search-update-entry entries))
    (elfeed-search-update--force))

  (defun my/elfeed-send-emails-processing (entry)
    (interactive)
    (let ((link (elfeed-entry-link entry))
           (title (elfeed-entry-title entry))
           (name (elfeed-feed-title (elfeed-entry-feed entry))))
      (compose-mail my/elfeed-email-destination (format "RSS: [%s] %s" name title) nil nil)

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
        (elfeed-untag entry 'unread))))

  (defun my/elfeed-tag-toggle-ok ()
    "Mark as 'ok' on an entry or entries, and send email with the content."
    (interactive)
    (elfeed-search-untag-all 'junk)
    (elfeed-search-toggle-all 'ok)
    (unless (use-region-p) (next-line)))

  (defun my/elfeed-tag-toggle-junk ()
    "Mark as 'junk'."
    (interactive)
    (elfeed-search-untag-all 'ok)
    (elfeed-search-untag-all-unread)
    (elfeed-search-toggle-all 'junk)
    (unless (use-region-p) (next-line)))

  (defun my/elfeed-tag-toggle-favorite ()
    "Mark as 'favorite'."
    (interactive)
    (elfeed-search-untag-all 'ok)
    (elfeed-search-untag-all 'junk)
    (elfeed-search-untag-all-unread)
    (elfeed-search-toggle-all 'favorite)
    (unless (use-region-p) (next-line)))

  (defun my/elfeed-mark-read-move-next ()
    (interactive)
    (elfeed-search-untag-all-unread)
    (unless (use-region-p) (next-line)))

  (defun my/elfeed-mark-unread-move-next ()
    (interactive)
    (elfeed-search-tag-all-unread)
    (unless (use-region-p) (next-line))))

(use-package elfeed-goodies
  :after elfeed
  :bind (:map elfeed-show-mode-map
          ("n" . 'elfeed-goodies/split-show-next)
          ("p" . 'elfeed-goodies/split-show-prev))
  :custom
  (elfeed-goodies/entry-pane-position 'bottom)
  (elfeed-show-entry-switch 'elfeed-goodies/switch-pane)
  (elfeed-show-entry-delete 'elfeed-goodies/delete-pane)
  :config
  (bind-key "q" #'elfeed-goodies/delete-pane elfeed-show-mode-map))

(use-package elfeed-org
  :after (org elfeed)
  :custom
  (rmh-elfeed-org-files
    (list
      ;; (expand-file-name "rss/feeds.org" user-emacs-directory)))
      (expand-file-name "feeds.org" "/home/user/Sync/elfeed")))
  :config
  (elfeed-org))

(defun my/rss-daily-fetch ()
  (my/elfeed-update))

(run-at-time 0 (* 3 60 60) 'my/rss-daily-fetch)

(defalias 'rss #'my/elfeed-load-db-and-open)
(provide 'my-rss)
;;; my-rss.el ends here
