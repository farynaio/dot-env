;; -*- lexical-binding: t; -*-

(use-package elfeed
  :bind (:map elfeed-search-mode-map
          ("f" . jarfar/hydra-elfeed/body)
          ("A" . my/elfeed-show-all)
          ("D" . my/elfeed-show-daily)
          ("q" . my/elfeed-save-db-and-bury))
  :config
  (setq elfeed-search-filter "+unread -readlater -junk")
  )

(use-package elfeed-goodies
  :config
  ;; (elfeed-goodies/setup)
  (setq elfeed-goodies/entry-pane-position 'bottom)
  (setq elfeed-show-entry-switch #'elfeed-goodies/switch-pane)
  (setq elfeed-show-entry-delete #'elfeed-goodies/delete-pane)

  (define-key elfeed-show-mode-map "n" #'elfeed-goodies/split-show-next)
  (define-key elfeed-show-mode-map "p" #'elfeed-goodies/split-show-prev)
  )

(use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.elfeed/feeds.org")))

(use-package elfeed-web)

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

;; (add-hook 'kill-emacs-hook  #'my/elfeed-kill)
;; https://noonker.github.io/posts/2020-04-22-elfeed/
(defun yt-dl-it (url)
  "Downloads the URL in an async shell"
  (let ((default-directory "~/Videos"))
    (async-shell-command (format "youtube-dl %s" url))))

;; https://noonker.github.io/posts/2020-04-22-elfeed/
(defun elfeed-youtube-dl (&optional use-generic-p)
  "Youtube-DL link"
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (yt-dl-it it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(define-key elfeed-search-mode-map (kbd "d") 'elfeed-youtube-dl)


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

(define-key elfeed-show-mode-map (kbd "SPC") 'elfeed-scroll-up-command)
(define-key elfeed-show-mode-map (kbd "S-SPC") 'elfeed-scroll-down-command)

(defun elfeed-tag-selection-as (mytag)
    "Returns a function that tags an elfeed entry or selection as
MYTAG"
  (interactive)
    (lambda ()
      "Toggle a tag on an Elfeed search selection"
      (interactive)
      (elfeed-search-toggle-all mytag)))

(define-key elfeed-search-mode-map "l" (elfeed-tag-selection-as 'readlater))
(define-key elfeed-search-mode-map "d" (elfeed-tag-selection-as 'junk))

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

(define-key elfeed-show-mode-map (kbd "B") 'elfeed-show-eww-open)
(define-key elfeed-search-mode-map (kbd "B") 'elfeed-search-eww-open)

(setq browse-url-browser-function
      '(("https:\\/\\/www\\.youtu\\.*be." . browse-url-mpv)
        ("." . browse-url-default-browser)))

(defun browse-url-mpv (url &optional single)
  (start-process "mpv" nil "mpv" (shell-quote-argument url)))

(defhydra jarfar/hydra-elfeed ()
  "Elfeed"
  ("b" (elfeed-search-set-filter "+business") "Show Business" :exit t)
  ("k" (elfeed-search-set-filter "+marketing") "Show Marketing" :exit t)
  ("e" (elfeed-search-set-filter "+entr") "Show Entr" :exit t)
  ("s" (elfeed-search-set-filter "+startup") "Show Startup" :exit t)
  ("h" (elfeed-search-set-filter "+growth") "Show Growth Hacking" :exit t)
  ("a" (elfeed-search-set-filter "+saas") "Show SaaS" :exit t)
  ("o" (elfeed-search-set-filter "+seo") "Show SEO" :exit t)
  ("l" (elfeed-search-set-filter "+blog") "Show Blogging" :exit t)
  ("c" (elfeed-search-set-filter "+copy") "Show Copywriting" :exit t)
  ("f" (elfeed-search-set-filter "+finances") "Show Finances" :exit t)
  ("m" (elfeed-search-set-filter "+social") "Show Social Media" :exit t)
  ("y" (elfeed-search-set-filter "+crypto") "Show Crypto" :exit t)
  )

(defalias 'rss #'my/elfeed-load-db-and-open)

(provide 'my-rss)