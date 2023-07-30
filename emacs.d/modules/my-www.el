;; Disable open link in browser on click and www link open
(setq browse-url-browser-function (lambda (URL &optional NEW-WINDOW)))

(defun my/browse-url-tor (url &optional new-window)
  "Ask the Tor WWW browser to load URL.
Defaults to the URL around or before point.  Passes the strings
in the variable `browse-url-firefox-arguments' to Tor.

Interactively, if the variable `browse-url-new-window-flag' is non-nil,
loads the document in a new Tor window.  A non-nil prefix argument
reverses the effect of `browse-url-new-window-flag'.

If `browse-url-firefox-new-window-is-tab' is non-nil, then
whenever a document would otherwise be loaded in a new window, it
is loaded in a new tab in an existing window instead.

Non-interactively, this uses the optional second argument NEW-WINDOW
instead of `browse-url-new-window-flag'."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "Tor Browser " url) nil
           "start-tor-browser"
           (append
            browse-url-firefox-arguments
            (if (browse-url-maybe-new-window new-window)
		(if browse-url-firefox-new-window-is-tab
		    '("-new-tab")
		  '("-new-window")))
            (list url)))))


(when (string= system-type "darwin")
  (setq process-connection-type nil))

; https://noonker.github.io/posts/2020-04-22-elfeed/
(defun my/youtube-download (url)
  "Downloads the URL in an async shell"
  (let ((default-directory "~/Documents/bibliography/videos"))
    (async-shell-command (format "youtube-dl %s" url))))

(defun my/browse-url-mpv (url &optional single)
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

;;      (defalias 'w #'af/eww)))
(use-package w3m
  :if (executable-find "w3m")
  :hook ((w3m-mode . iscroll-mode))
  :demand t
  :bind (:map w3m-mode-map
          ("M-h" . my/w3m-history-full)
          ("<" . beginning-of-buffer)
          (">" . end-of-buffer)
          ("I" . my/w3m-view-image-generic-browser)
          ("O" . my/w3m-open-in-external)
          ("C-w v" . evil-window-vsplit)
          ("C-w C-v" . evil-window-vsplit)
          ("C-w =" . balance-windows)
          ("C-w |" . evil-window-set-width)
          ("s-}" . w3m-next-buffer)
          ("s-{" . w3m-previous-buffer)
          ("<right>" . w3m-view-next-page)
          ("S" . my/w3m-search-new-session)
          ("C-c C-t" . my/w3m-create-empty-session)
          ("C-t t" . my/w3m-create-empty-session)
          ("<S-mouse-1>" . my/w3m-open-in-external-click)
          ("<s-mouse-1>" . w3m-mouse-view-this-url-new-session)
          ("<s-return>" . (lambda () (interactive) (w3m-view-this-url nil t)))
          ("C-c C-e" . my/w3m-goto-new-session-url))
  :preface
  (defun my/w3m-goto-frame (url &rest args)
    (interactive (list (read-string "URL: ")))
    (let ((w3m-alive (w3m-alive-p))
           (w3m-new-session-in-background nil))
      (switch-to-buffer-other-frame w3m-alive)
      (unless w3m-alive
        (w3m))
      (w3m-goto-url-new-session url)))

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
  :custom
  (w3m-use-cookies nil)
  (w3m-use-favicon nil)
  (mm-text-html-renderer 'w3m)
  (w3m-search-default-engine "duckduckgo")
  (w3m-delete-duplicated-empty-lines t)
  (w3m-coding-system 'utf-8)
  (w3m-default-coding-system 'utf-8)
  (w3m-file-coding-system 'utf-8)
  (w3m-file-name-coding-system 'utf-8)
  (w3m-input-coding-system 'utf-8)
  (w3m-output-coding-system 'utf-8)
  (w3m-terminal-coding-system 'utf-8)
  (w3m-display-mode 'tabbed)
  (w3m-default-display-inline-images nil)
  (w3m-confirm-leaving-secure-page nil)
  (w3m-new-session-in-background t)
  (w3m-session-load-crashed-sessions t)
  (w3m-session-load-last-sessions t)
  (w3m-new-session-url "about:")
  (w3m-type 'w3m-m17n)
  (w3m-user-agent "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36")
  (browse-url-handlers
    '(("https:\\/\\/www\\.youtu\\.*be." . my/browse-url-mpv)
       ("." . my/w3m-goto-frame)))
  ;; (browse-url-browser-function
  ;;   '(("https:\\/\\/www\\.youtu\\.*be." . my/browse-url-mpv)
  ;;      ("." . my/w3m-goto-frame)))
  :config
  (defun my/w3m-create-empty-session ()
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
      (if w3m-alive
        (progn
          (w3m)
          (w3m-search-new-session w3m-search-default-engine query))
        (w3m-search w3m-search-default-engine query))))

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
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "g")

  (defengine images
    "https://duckduckgo.com/?t=h_&iax=images&ia=images&q=%s"
    :keybinding "i")

  (defengine maps
    "https://www.qwant.com/maps/place/osm:way:25739565@%s"
    :keybinding "m"
    :docstring "Mappin' it up.")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")

  (defengine wikipedia
    "https://wikiless.org/w/index.php?title=Special%3ASearch&fulltext=Search&ns0=1&search=%s"
    :keybinding "w"
    :docstring "Searchin' the wikis.")

  (engine-mode t))

(provide 'my-www)
