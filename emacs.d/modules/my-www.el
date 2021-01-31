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
  :commands w3m w3m-goto-url-new-session w3m-goto-url
  :bind (:map w3m-mode-map
          ("<" . beginning-of-buffer)
          (">" . end-of-buffer))
  :config
  (setq
    w3m-default-display-inline-images t
    w3m-use-cookies t
    mm-text-html-renderer 'w3m
    w3m-coding-system 'utf-8
    w3m-file-coding-system 'utf-8
    w3m-file-name-coding-system 'utf-8
    w3m-input-coding-system 'utf-8
    w3m-output-coding-system 'utf-8
    w3m-search-default-engine "duckduckgo"
    w3m-terminal-coding-system 'utf-8
    w3m-display-mode 'tabbed
    w3m-default-display-inline-image nil
    w3m-confirm-leaving-secure-page nil
    )

  (setq browse-url-browser-function
    '(("https:\\/\\/www\\.youtu\\.*be." . jarfar/browse-url-mpv)
       ("." . w3m-browse-url)))

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

(defhydra my/hydra-browser ()
  "WWW browser shorcuts"
  ("s" (my/func-call 'w3m-goto-url-new-session) "search" :exit t)
  ("g" w3m-goto-url-new-session "go to" :exit t))

(provide 'my-www)
