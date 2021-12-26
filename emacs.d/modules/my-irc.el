(use-package erc
  :ensure nil
  :demand t
  :bind (:map erc-mode-map
          ("C-w =" . balance-windows)
          ("C-w |" . evil-window-set-width)
          ("C-w q" . evil-quit)
          ("C-w v" . evil-window-vsplit)
          ("C-c C-j" . my/erc-join-channel))
  ;; :hook ((erc-send-pre . my/erc-preprocess))
  :custom-face
  (erc-action-face ((t (:foreground "#8fbcbb"))))
  (erc-error-face ((t (:foreground "#bf616a"))))
  (erc-input-face ((t (:foreground "#ebcb8b"))))
  (erc-notice-face ((t (:foreground "#ebcb8b"))))
  (erc-timestamp-face ((t (:foreground "#a3be8c"))))
  :custom
  (erc-prompt-for-nickserv-password nil)
  (erc-autoaway-message "I'm away (after %i seconds of idle-time)")
  (erc-nick-uniquifier "_")
  (erc-kill-server-buffer-on-quit t)
  (erc-kill-queries-on-quit t)
  (erc-track-showcount t)
  (erc-auto-query 'window-noselect)
  (erc-autojoin-domain-only nil)
  (erc-rename-buffers t)
  (erc-enable-logging t)
  (erc-log-mode t)
  (erc-log-write-after-insert t)
  (erc-log-write-after-send t)
  (erc-save-buffer-on-part nil)
  (erc-save-queries-on-quit nil)
  (erc-log-insert-log-on-open nil)
  (erc-log-channels-directory  my/erc-logs)
  (erc-query-display 'window)
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                              "324" "329" "332" "333" "353" "477"))
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-autojoin-timing 'ident)
  (erc-user-mode "iR")
  (erc-header-line-format "%n on %t (%m) %o")
  (erc-join-buffer 'bury)
  (erc-lurker-threshold-time 43200)
  (erc-server-reconnect-timeout 3)
  (erc-server-reconnect-attempts 5)
  (erc-reuse-buffers t)
  (erc-dcc-get-default-directory "~/Download")
  ;; (erc-prompt-for-password nil)
  ;; (erc-kill-buffer-on-part t)
  ;; (erc-autojoin-domain-only t)
  :config
  (require 'erc-services)
  ;; (require 'erc-join)

  (setq erc-debug-irc-protocol t)
  (setq erc-modules (nconc erc-modules '(autoaway dcc irccontrols keep-place move-to-prompt spelling noncommands readonly services stamp track)))

  (erc-autojoin-mode -1)
  (erc-update-modules)
  ;; (erc-services-mode 1)

  ;; Save logs of all ERC buffers on Emacs quit
  (defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
    (save-some-buffers t (lambda () (when (eq major-mode 'erc-mode) t))))

  (add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)

  (defun my/erc-detach-channel ()
    (when (erc-server-process-alive)
      (let ((tgt (erc-default-target)))
        (erc-server-send (format "DETACH %s" tgt) nil tgt))))

  (defun my/erc-kill-channel-advice (orig-fun &rest args)
    (my/erc-detach-channel))

  (advice-add 'erc-kill-channel :around #'my/erc-kill-channel-advice)

  ;; https://web.archive.org/web/20160608060228/http://edward.oconnor.cx/2007/09/freenode-cloaking-and-erc
  ;; (add-hook 'erc-server-NOTICE-functions #'my/erc-post-cloak-autojoin)
  ;; (defun my/erc-post-cloak-autojoin (proc parsed)
  ;;   "Autojoin iff NickServ tells us to."
  ;;   (with-current-buffer (process-buffer proc)
  ;;     (when (and (string-suffix-p "freenode.net"
  ;;                  (erc-response.sender parsed))
  ;;             (string-match ".*NickServ set your hostname to.*"
  ;;               (erc-response.contents parsed)))
  ;;       (erc-autojoin-channels erc-session-server (erc-current-nick))
  ;; nil)))

  (defun my/erc-join-channel (channel &optional key)
    "Join CHANNEL. If `point' is at the beginning of a channel name, use that as default."
    (interactive
      (list
        (let ((chnl (if (looking-at "\\([&#+!][^ \n]+\\)") (match-string 1) ""))
               (table (when (erc-server-buffer-live-p)
                        (set-buffer (process-buffer erc-server-process))
                        erc-channel-list)))
          (completing-read "Join channel"
            my/erc-channel-list nil nil nil nil chnl))
        (when (or current-prefix-arg erc-prompt-for-channel-key)
          (read-from-minibuffer "Channel key (RET for none): " nil))))
    (erc-cmd-JOIN channel (when (>= (length key) 1) key)))

  (add-hook 'erc-join-hook (lambda () (switch-to-buffer (current-buffer))))

  ;; https://www.reddit.com/r/emacs/comments/8ml6na/tip_how_to_make_erc_fun_to_use/
  (defun my/erc-freenode ()
    "Connects to Freenode, or switch to last active buffer."
    (interactive)
    (let ((tls-checktrust nil)
           (gnutls-verify-error nil))
      (erc-tls :server my/znc-server :port my/znc-server-port :nick my/znc-server-nick-freenode :password my/znc-network-password-freenode)))

  (defun my/erc-oftc ()
    "Connects to OFTC, or switch to last active buffer."
    (interactive)
    (let ((tls-checktrust nil)
           (gnutls-verify-error nil))
      (erc-tls :server my/znc-server :port my/znc-server-port :nick my/znc-server-nick-oftc :password my/znc-network-password-oftc)))

        ;; (erc-tls :server "irc.oftc.net" :port "6697" :nick "apzeys" :password "9mHi2ekHAr3w8r2mKL3z")
        ;; (let ((user-login-name "azur12"))
        ;;   (erc-tls :server "chat.freenode.net" :port 6697 :nick my/erc-nick :password my/erc-freenode-password)
        ;;   )

  ;; https://www.reddit.com/r/emacs/comments/8ml6na/tip_how_to_make_erc_fun_to_use/
  ;; (defun my/erc-count-users ()
  ;;   "Displays the number of users connected on the current channel."
  ;;   (interactive)
  ;;   (if (get-buffer "freenode")
  ;;     (let ((channel (erc-default-target)))
  ;;       (if (and channel (erc-channel-p channel))
  ;;         (message "%d users are online on %s"
  ;;           (hash-table-count erc-channel-users)
  ;;           channel)
  ;;         (user-error "The current buffer is not a channel")))
  ;;     (user-error "You must first start ERC")))

  ;; (defun my/erc-get-ops ()
  ;;   "Displays the names of ops users on the current channel."
  ;;   (interactive)
  ;;   (if (get-buffer "freenode")
  ;;       (let ((channel (erc-default-target)))
  ;;         (if (and channel (erc-channel-p channel))
  ;;             (let (ops)
  ;;               (maphash (lambda (nick cdata)
  ;;                          (if (and (cdr cdata)
  ;;                                   (erc-channel-user-op (cdr cdata)))
  ;;                              (setq ops (cons nick ops))))
  ;;                        erc-channel-users)
  ;;               (if ops
  ;;                   (message "The online ops users are: %s"  (mapconcat 'identity ops " "))
  ;;                 (message "There are no ops users online on %s" channel)))
  ;;           (user-error "The current buffer is not a channel")))
  ;;     (user-error "You must first be connected on IRC")))

  (defun my/erc-preprocess (string)
    "Avoids channel flooding."
    (setq str (string-trim (replace-regexp-in-string "\n+" " " str))))

  ;; TODO what it does?
  ;; (defun my/erc-reset-track-mode ()
  ;;   "Resets ERC track mode."
  ;;   (interactive)
  ;;   (setq erc-modified-channels-alist nil)
  ;;   (erc-modified-channels-update)
  ;;   (erc-modified-channels-display)
  ;;   (force-mode-line-update))

  (when (eq system-type 'darwin)
    (require 'erc-desktop-notifications)
    (defun erc-notifications-notify (nick msg)
      "Notify that NICK send some MSG via AppleScript."
      (ns-do-applescript
        (concat "display notification \"" (replace-regexp-in-string "(\"|\\\\)" "\\\1" msg)
          "\" with title \"" (replace-regexp-in-string "(\"|\\\\)" "\\\1" nick) "\" sound name \"all-eyes-on-me-465\""))))

  ;; (defun oz/escape-applescript (str)
  ;;   "Quote \\ and \"."
  ;;   (let ((len (length str)) (i 0) (q "") char)
  ;;     (while (< i len)
  ;;       (setq char (substring str i (1+ i))
  ;;         i (1+ i))
  ;;       (when (or (string= char "\\") (string= char "\""))
  ;;         (setq q (concat q "\\")))
  ;;       (setq q (concat q char)))
  ;;     q))
  )

;; (if (file-exists-p (concat user-emacs-directory "lisp/erc-sasl.el"))
;;   (use-package erc-sasl
;;     :load-path "lisp/"
;;     :after erc
;;     :config
;;     (add-to-list 'erc-sasl-server-regexp-list "chat\\.freenode\\.net")
;;     (defun erc-login ()
;;       "Perform user authentication at the IRC server."
;;       (erc-log (format "login: nick: %s, user: %s %s %s :%s"
;;                  (erc-current-nick)
;;                  (user-login-name)
;;                  (or erc-system-name (system-name))
;;                  erc-session-server
;;                  erc-session-user-full-name))
;;       (if erc-session-password
;;         (erc-server-send (format "PASS %s" erc-session-password))
;;         (message "Logging in without password"))
;;       (when (and (featurep 'erc-sasl) (erc-sasl-use-sasl-p))
;;         (erc-server-send "CAP REQ :sasl"))
;;       (erc-server-send (format "NICK %s" (erc-current-nick)))
;;       (erc-server-send
;;         (format "USER %s %s %s :%s"
;;           ;; hacked - S.B.
;;           (if erc-anonymous-login erc-email-userid (user-login-name))
;;           "0" "*"
;;           erc-session-user-full-name))
;;       (erc-update-mode-line))))

(if my/znc-server-regex
  (use-package erc-sasl
    :after erc
    :ensure nil
    :config
    (add-to-list 'erc-sasl-server-regexp-list my/znc-server-regex)
    (defun erc-login ()
      "Perform user authentication at the IRC server."
      (erc-log (format "login: nick: %s, user: %s %s %s :%s"
                 (erc-current-nick)
                 (user-login-name)
                 (or erc-system-name (system-name))
                 erc-session-server
                 erc-session-user-full-name))
      (if erc-session-password
        (erc-server-send (format "PASS %s" erc-session-password))
        (message "Logging in without password"))
      (when (and (featurep 'erc-sasl) (erc-sasl-use-sasl-p))
        (erc-server-send "CAP REQ :sasl"))
      (erc-server-send (format "NICK %s" (erc-current-nick)))
      (erc-server-send
        (format "USER %s %s %s :%s"
          ;; hacked - S.B.
          (if erc-anonymous-login erc-email-userid (user-login-name))
          "0" "*"
          erc-session-user-full-name))
      (erc-update-mode-line))
    )

  (user-error "erc-sasl: variable 'my/znc-server-regex' not set, package disabled!")
  )

(use-package erc-image
  :after erc)

(use-package erc-hl-nicks
  :after erc)

(defalias 'irc #'my/erc-freenode)
(defalias 'irc-freenode #'my/erc-freenode)
(defalias 'irc-oftc #'my/erc-oftc)

(provide 'my-irc)
