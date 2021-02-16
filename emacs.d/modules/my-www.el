(when (string= system-type "darwin")
  (setq process-connection-type nil))

; https://noonker.github.io/posts/2020-04-22-elfeed/
(defun jarfar/youtube-download (url)
  "Downloads the URL in an async shell"
  (let ((default-directory "~/Documents/bibliography/videos"))
    (async-shell-command (format "youtube-dl %s" url))))

(defun jarfar/browse-url-mpv (url &optional single)
  (start-process "mpv" nil "mpv" (shell-quote-argument url)))

;; (require 'eww)
;; (eval-after-load 'eww
;;   '(progn
;;      (setq url-cookie-trusted-urls '())
;;      (setq url-cookie-untrusted-urls '(".*"))

;;      (bind-key "/"   'evil-search-forward eww-mode-map)
;;      (bind-key "C-s" 'evil-search-forward eww-mode-map)
;;      (bind-key "}" 'forward-paragraph eww-mode-map)
;;      (bind-key "{" 'backward-paragraph eww-mode-map)

;;      (defun af/eww ()
;;        (interactive)
;;        (let ((eww-current-buffer (get-buffer "*eww*"))
;;               (url (read-from-minibuffer "Enter URL or keywords: ")))
;;          (if (eq eww-current-buffer nil)
;;            (eww url)
;;            (switch-to-buffer (generate-new-buffer "eww"))
;;            (eww-mode)
;;            (eww url))))

;;      (defalias 'w 'af/eww)))

(use-package w3m
  :config
  (setq
    w3m-use-cookies nil
    w3m-use-favicon nil
    mm-text-html-renderer 'w3m
    w3m-search-default-engine "duckduckgo"
    w3m-delete-duplicated-empty-lines t
    w3m-coding-system 'utf-8
    w3m-default-coding-system 'utf-8
    w3m-file-coding-system 'utf-8
    w3m-file-name-coding-system 'utf-8
    w3m-input-coding-system 'utf-8
    w3m-output-coding-system 'utf-8
    w3m-terminal-coding-system 'utf-8
    w3m-display-mode 'tabbed
    w3m-default-display-inline-images nil
    w3m-confirm-leaving-secure-page nil
    w3m-new-session-in-background t
    w3m-session-load-last-sessions t
    w3m-new-session-url "about:"
    w3m-type 'w3m-m17n
    ;; w3m-user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:10.0) Gecko/20100101 Firefox/10.0"
    )

  (setq browse-url-browser-function
    '(("https:\\/\\/www\\.youtu\\.*be." . jarfar/browse-url-mpv)
       ("." . my/w3m-goto-frame)))

  ;; w3m-goto-url-new-session

  (bind-keys
    :map w3m-mode-map
    ("M-h" . my/w3m-history-full)
    ("<" . beginning-of-buffer)
    (">" . end-of-buffer)
    ("I" . my/w3m-view-image-generic-browser)
    ("O" . my/w3m-open-in-external)
    ("C-w" . w3m-delete-buffer)
    ;; ("C-w v" . evil-window-vsplit)
    ;; ("C-w C-v" . evil-window-vsplit)
    ;; ("C-w =" . balance-windows)
    ;; ("C-w |" . evil-window-set-width)
    ("s-}" . w3m-next-buffer)
    ("s-{" . w3m-previous-buffer)
    ("<right>" . w3m-view-next-page)
    ("C-c C-t" . farynaio/w3m-create-empty-session)
    ("C-t t" . farynaio/w3m-create-empty-session)
    ("<S-mouse-1>" . my/w3m-open-in-external-click)
    ("<s-mouse-1>" . w3m-mouse-view-this-url-new-session)
    ("C-c C-e" . my/w3m-goto-new-session-url))

  (add-hook 'w3m-mode-hook #'iscroll-mode)

  (defun farynaio/w3m-create-empty-session ()
    (interactive)
    (let ((w3m-new-session-in-background nil)) (w3m-create-empty-session)))

  (defun my/w3m-goto-new-session-url (&optional reload)
    "Open `w3m-new-session-url' in a new session."
    (interactive "P")
    (if (not (eq major-mode 'w3m-mode))
      (message "This command can be used in w3m mode only")
      (let ((w3m-new-session-in-background nil))
        (w3m-goto-url-new-session w3m-new-session-url reload))))

  (defun my/w3m-open-in-external-click (event)
    (interactive "e")
    (mouse-set-point event)
    (let ((url (w3m-url-valid (w3m-anchor))))
      (when url
        (browse-url-generic url))))

  (defun my/w3m-open-in-external (url &optional reload charset post-data referer handler
			 element background save-pos)
    (interactive
      (list (unless (w3m--buffer-busy-error)
	            (w3m-input-url "Open URL in current buffer" nil nil nil
			          'feeling-searchy 'no-initial))
	      current-prefix-arg coding-system-for-read))
    (let ((url (w3m-url-valid (w3m-anchor))))
      (when url
        (browse-url-generic url))))

  (defun my/w3m-view-image-generic-browser ()
    (interactive)
    (let ((url (w3m-url-valid (w3m-image))))
      (if url
        (browse-url-generic url)
        (w3m-message "No image at point"))))

  (defun my/w3m-history-full ()
    (interactive)
    (w3m-history 1))

  (defun my/w3m-open-other-window ()
    (interactive)
    (let ((w3m-alive (w3m-alive-p)))
      (switch-to-buffer-other-frame w3m-alive)
      (unless w3m-alive
        (w3m))))

  (defun my/w3m-open-frame ()
    (interactive)
    (let ((w3m-alive (w3m-alive-p)))
      (switch-to-buffer-other-frame w3m-alive)
      (unless w3m-alive
        (w3m))))

  (defun my/w3m-open-in-external (event)
    (interactive "e")
    (mouse-set-point event)
    (let ((url (w3m-url-valid (w3m-anchor))))
      (when url
        (browse-url-generic url))))

  (defun my/w3m-search-new-session (query &optional from to)
    (interactive
      (if (use-region-p)
        (list nil (region-beginning) (region-end))
        (list (read-string "Search phrase: "))))
    (let ((w3m-alive (w3m-alive-p))
           (query (if query query (buffer-substring-no-properties from to)))
           (w3m-new-session-in-background nil))
      (switch-to-buffer-other-window w3m-alive)
      (if w3m-alive
        (progn
          (w3m)
          (w3m-search-new-session w3m-search-default-engine query))
        (w3m-search w3m-search-default-engine query))))

  (defun my/w3m-search-frame (query &optional from to)
    (interactive
      (if (use-region-p)
        (list nil (region-beginning) (region-end))
        (list (read-string "Search phrase: "))))
    (let ((w3m-alive (w3m-alive-p))
           (query (if query query (buffer-substring-no-properties from to)))
           (w3m-new-session-in-background nil))
      (switch-to-buffer-other-frame w3m-alive)
      (if w3m-alive
        (progn
          (w3m)
          (w3m-search-new-session w3m-search-default-engine query))
        (w3m-search w3m-search-default-engine query))))

  (defun my/w3m-goto-frame (url &rest args)
    (interactive (list (read-string "URL: ")))
    (let ((w3m-alive (w3m-alive-p))
           (w3m-new-session-in-background nil))
      (switch-to-buffer-other-frame w3m-alive)
      (unless w3m-alive
        (w3m))
      (w3m-goto-url-new-session url)))

  ;; Fix asking for confirmation before visiting URL via generic browser
  (advice-add 'browse-url-interactive-arg :around
    (lambda (orig-fun &rest args)
      (let ((event (elt (this-command-keys) 0)))
        (and (listp event) (mouse-set-point event)))
      (list (or (and transient-mark-mode mark-active
                  ;; rfc2396 Appendix E.
                  (replace-regexp-in-string
                    "[\t\r\f\n ]+" ""
                    (buffer-substring-no-properties
                      (region-beginning) (region-end))))
              (browse-url-url-at-point))
        (not (eq (null browse-url-new-window-flag)
               (null current-prefix-arg))))))

  (advice-add 'w3m-goto-url-new-session :around
    (lambda (orig-fun &rest args)
      (if (string-prefix-p "*w3m*" (buffer-name))
        (apply orig-fun args)
        (if (w3m-alive-p)
          (progn
            (switch-to-buffer-other-window (w3m-alive-p t))
            (apply orig-fun args))
          (switch-to-buffer-other-window "*scratch*")
          (w3m (car args))))))

  (advice-add 'w3m-quit :around
    (lambda (orig-fun &rest args)
      (funcall orig-fun t))))

(when (not (executable-find "w3m"))
  (message (concat "Executable 'w3m' not found!")))

(use-package engine-mode
  :defer 3
  :config
  (defengine amazon
    "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s"
    :keybinding "a")

  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "g")

  (defengine google-images
    "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s"
    :keybinding "i")

  (defengine google-maps
    "http://maps.google.com/maps?q=%s"
    :keybinding "m"
    :docstring "Mappin' it up.")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")

  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :keybinding "y")

  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w"
    :docstring "Searchin' the wikis.")
  (engine-mode t))

(provide 'my-www)
