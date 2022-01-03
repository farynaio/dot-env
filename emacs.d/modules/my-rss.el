;; -*- lexical-binding: t; -*-

(use-package elfeed
  :commands elfeed-db-load
  :bind (:map elfeed-show-mode-map
          ("SPC" . elfeed-scroll-up-command)
          ("S-SPC" . elfeed-scroll-down-command)
          ("<mouse-1>" . jarfar/elfeed-shr-open-click)
          ("<s-mouse-1>" . jarfar/elfeed-shr-open-click)
          ("<S-mouse-1>" . jarfar/elfeed-shr-open-click)
          :map shr-map
          ("RET" . jarfar/elfeed-shr-open-external)
          ("v" . jarfar/elfeed-shr-open-external)
          ("O" . jarfar/elfeed-shr-open-external)
          ("<mouse-1>" . jarfar/elfeed-shr-open-click)
          ("<mouse-2>" . jarfar/elfeed-shr-open-click)
          ("<S-mouse-1>" . jarfar/elfeed-shr-open-click)
          :map elfeed-search-mode-map
          ("h" . hydra-elfeed-search/body)
          ("q" . jarfar/elfeed-save-db-and-bury)
          ("d" . jarfar/elfeed-youtube-download)
          ("o" . jarfar/elfeed-tag-toggle-ok)
          ("f" . jarfar/elfeed-tag-toggle-favorite)
          ("j" . jarfar/elfeed-tag-toggle-junk)
          ("m" . jarfar/elfeed-send-emails)
          ("r" . jarfar/elfeed-mark-read-move-next)
          ("u" . jarfar/elfeed-mark-unread-move-next)
          ("O" . jarfar/elfeed-open-in-external-browser)
          ("g" . jarfar/elfeed-update)
          ("<S-mouse-1>" . jarfar/elfeed-search-open-in-external-click)
          ("<s-mouse-1>" . elfeed-search-browse-url)
          ("M" . (lambda () (interactive) (jarfar/elfeed-send-emails t))))
  :preface
  (defun jarfar/elfeed-load-db-and-open ()
    "Wrapper to load the elfeed db from disk before opening"
    (interactive)
    (elfeed-db-load)
    (elfeed-update)
    (elfeed-search-update--force)
    (elfeed))
  :custom
  (elfeed-search-filter "+news -ignore -ok -junk")
  (elfeed-search-title-max-width 115)
  (elfeed-search-remain-on-entry t)
  (elfeed-curl-timeout 200)
  (elfeed-web-limit 500)
  (url-queue-timeout 60)
  :config
  (defun jarfar/elfeed-search-open-in-external-click (event)
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
  (defun jarfar/elfeed-shr-open-click (event)
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
  (defun jarfar/elfeed-open-in-external-browser (&optional use-generic-p)
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

  (defun jarfar/elfeed-update ()
    (interactive)
    (elfeed-update)
    (elfeed-web-update))

  ;; based on shr-browse-url
  (defun jarfar/elfeed-shr-open-external (&optional mouse-event)
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
  (defun jarfar/elfeed-save-db-and-bury ()
    "Wrapper to save the elfeed db to disk before burying buffer"
    (interactive)
    (elfeed-db-save)
    (quit-window))

  (defun my/elfeed-kill ()
    "Elfeed custom cleanup on exit."
    (elfeed-db-save)
    (elfeed-db-compact))

  ;; https://noonker.github.io/posts/2020-04-22-elfeed/
  (defun jarfar/elfeed-youtube-download (&optional use-generic-p)
    "Youtube-DL link"
    (interactive "P")
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
        do (elfeed-untag entry 'unread)
        when (elfeed-entry-link entry)
        do (jarfar/youtube-download it))
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

  (defun jarfar/elfeed-send-emails (&optional all)
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
        do (jarfar/elfeed-send-emails-processing entry)
        ;; else
        ;; do (message "The entry is not tagged as 'ok'. No email send now.")
        ;; else
        ;; do (message "The entry is already read. No email send now.")
        )

      (mapc 'elfeed-search-update-entry entries))
    (elfeed-search-update--force))

  (defun jarfar/elfeed-send-emails-processing (entry)
    (interactive)
    (let ((link (elfeed-entry-link entry))
           (title (elfeed-entry-title entry))
           (name (elfeed-feed-title (elfeed-entry-feed entry))))
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
        (elfeed-untag entry 'unread))))

  (defun jarfar/elfeed-tag-toggle-ok ()
    "Mark as 'ok' on an entry or entries, and send email with the content."
    (interactive)
    (elfeed-search-untag-all 'junk)
    (elfeed-search-toggle-all 'ok)
    (unless (use-region-p) (next-line)))

  (defun jarfar/elfeed-tag-toggle-junk ()
    "Mark as 'junk'."
    (interactive)
    (elfeed-search-untag-all 'ok)
    (elfeed-search-untag-all-unread)
    (elfeed-search-toggle-all 'junk)
    (unless (use-region-p) (next-line)))

  (defun jarfar/elfeed-tag-toggle-favorite ()
    "Mark as 'favorite'."
    (interactive)
    (elfeed-search-untag-all 'ok)
    (elfeed-search-untag-all 'junk)
    (elfeed-search-untag-all-unread)
    (elfeed-search-toggle-all 'favorite)
    (unless (use-region-p) (next-line)))

  (defun jarfar/elfeed-mark-read-move-next ()
    (interactive)
    (elfeed-search-untag-all-unread)
    (unless (use-region-p) (next-line)))

  (defun jarfar/elfeed-mark-unread-move-next ()
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
  (elfeed-show-entry-delete 'elfeed-goodies/delete-pane))

(use-package elfeed-org
  :after (org elfeed)
  :custom
  (rmh-elfeed-org-files '("~/.elfeed/feeds.org"))
  :config
  (elfeed-org))

(defalias 'rss #'jarfar/elfeed-load-db-and-open)

(provide 'my-rss)