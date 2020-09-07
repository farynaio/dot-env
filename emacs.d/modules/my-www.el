(require 'eww)

(setq browse-url-browser-function
      '(("https:\\/\\/www\\.youtu\\.*be." . jarfar/browse-url-mpv)
        ("." . browse-url-default-browser)))

; https://noonker.github.io/posts/2020-04-22-elfeed/
(defun jarfar/youtube-download (url)
  "Downloads the URL in an async shell"
  (let ((default-directory "~/Documents/bibliography/videos"))
    (async-shell-command (format "youtube-dl %s" url))))

(defun jarfar/browse-url-mpv (url &optional single)
  (start-process "mpv" nil "mpv" (shell-quote-argument url)))

(eval-after-load 'eww
  '(progn
     (setq url-cookie-trusted-urls '())
     (setq url-cookie-untrusted-urls '(".*"))

     (bind-key "/"   #'evil-search-forward eww-mode-map)
     (bind-key "C-s" #'evil-search-forward eww-mode-map)
     (bind-key "}" #'forward-paragraph eww-mode-map)
     (bind-key "{" #'backward-paragraph eww-mode-map)

     (defun af/eww ()
       (interactive)
       (let ((eww-current-buffer (get-buffer "*eww*"))
              (url (read-from-minibuffer "Enter URL or keywords: ")))
         (if (eq eww-current-buffer nil)
           (eww url)
           (switch-to-buffer (generate-new-buffer "eww"))
           (eww-mode)
           (eww url)
           )))

     (defalias 'w #'af/eww)))

;; (if (executable-find "w3m")
;;   (use-package w3m
;;     :config
;;     (progn
;;       (setq
;;         w3m-default-display-inline-images t
;;         w3m-use-cookies t
;;         mm-text-html-renderer 'w3m
;;         w3m-coding-system 'utf-8
;;         w3m-file-coding-system 'utf-8
;;         w3m-file-name-coding-system 'utf-8
;;         w3m-input-coding-system 'utf-8
;;         w3m-output-coding-system 'utf-8
;;         w3m-search-default-engine "duckduckgo"
;;         w3m-terminal-coding-system 'utf-8)

;;       (bind-key "<" #'beginning-of-buffer w3m-mode-map)
;;       (bind-key ">" #'end-of-buffer w3m-mode-map)

;;       (if (string= system-type "darwin")
;;         (setq process-connection-type nil))
;;       )
;;     (defalias 'w #'w3m)
;;     )
;;   (message (concat "Executable 'w3m' not found!")))

(provide 'my-www)
