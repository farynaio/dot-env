;; (require 'erc-services)

(use-package erc
  :ensure nil
  :preface
  ;; https://www.reddit.com/r/emacs/comments/8ml6na/tip_how_to_make_erc_fun_to_use/
  (defun my/erc-start-or-switch ()
    "Connects to ERC, or switch to last active buffer."
    (interactive)
    (if (get-buffer "freenode")
      (erc-track-switch-buffer 1)
      (erc)))
  :hook ((ercn-notify . my/erc-notify)
          (erc-send-pre . my/erc-preprocess))
  :custom-face
  (erc-action-face ((t (:foreground "#8fbcbb"))))
  (erc-error-face ((t (:foreground "#bf616a"))))
  (erc-input-face ((t (:foreground "#ebcb8b"))))
  (erc-notice-face ((t (:foreground "#ebcb8b"))))
  (erc-timestamp-face ((t (:foreground "#a3be8c"))))
  :custom
  (erc-nick "farynaio")
  (erc-away-nickname "farynaio")
  (erc-email-userid "adam@faryna.io")
  (erc-prompt-for-nickserv-password nil)
  (erc-format-nick-function 'erc-format-@nick)
  (erc-autoaway-message "I'm away (after %i seconds of idle-time)")
  (erc-nick-uniquifier "_")
  (erc-prompt-for-password nil)
  (erc-kill-server-buffer-on-quit t)
  (erc-kill-queries-on-quit t)
  (erc-track-showcount t)
  (erc-rename-buffers t)
  (erc-server-reconnect-timeout 3)
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                              "324" "329" "332" "333" "353" "477"))
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-autojoin-timing 'ident)

  (erc-header-line-format "%n on %t (%m) %o")
  (erc-join-buffer 'bury)
  ;; (erc-kill-buffer-on-part t)
  (erc-lurker-threshold-time 43200)
  (erc-server-reconnect-attempts 5)
  :config
  (require 'erc-join)

  (add-to-list 'erc-modules 'autojoin)
  (add-to-list 'erc-modules 'button)
  (add-to-list 'erc-modules 'completion)
  (add-to-list 'erc-modules 'fill)
  (add-to-list 'erc-modules 'irccontrols)
  (add-to-list 'erc-modules 'keep-place)
  (add-to-list 'erc-modules 'list)
  (add-to-list 'erc-modules 'match)
  (add-to-list 'erc-modules 'move-to-prompt)
  (add-to-list 'erc-modules 'netsplit)
  (add-to-list 'erc-modules 'networks)
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling)
  (add-to-list 'erc-modules 'noncommands)
  (add-to-list 'erc-modules 'readonly)
  (add-to-list 'erc-modules 'services)
  ;; (add-to-list 'erc-modules 'scrolltobottom)
  (add-to-list 'erc-modules 'ring)
  (add-to-list 'erc-modules 'stamp)
  (add-to-list 'erc-modules 'track)
  (add-to-list 'erc-autojoin-channels-alist '("freenode.net" "#bitcoin"))

  ;; https://www.reddit.com/r/emacs/comments/8ml6na/tip_how_to_make_erc_fun_to_use/
  (defun my/erc-notify (nickname message)
    "Displays a notification message for ERC."
    (let* ((channel (buffer-name))
            (nick (erc-hl-nicks-trim-irc-nick nickname))
            (title (if (string-match-p (concat "^" nickname) channel)
                     nick
                     (concat nick " (" channel ")")))
            (msg (s-trim (s-collapse-whitespace message))))
      (my-appt-send-notification (concat nick ": " msg) :title title)))

  ;; https://www.reddit.com/r/emacs/comments/8ml6na/tip_how_to_make_erc_fun_to_use/
  (defun my/erc-count-users ()
    "Displays the number of users connected on the current channel."
    (interactive)
    (if (get-buffer "irc.freenode.net:6667")
      (let ((channel (erc-default-target)))
        (if (and channel (erc-channel-p channel))
          (message "%d users are online on %s"
            (hash-table-count erc-channel-users)
            channel)
          (user-error "The current buffer is not a channel")))
      (user-error "You must first start ERC")))

  (defun my/erc-get-ops ()
    "Displays the names of ops users on the current channel."
    (interactive)
    (if (get-buffer "irc.freenode.net:6667")
        (let ((channel (erc-default-target)))
          (if (and channel (erc-channel-p channel))
              (let (ops)
                (maphash (lambda (nick cdata)
                           (if (and (cdr cdata)
                                    (erc-channel-user-op (cdr cdata)))
                               (setq ops (cons nick ops))))
                         erc-channel-users)
                (if ops
                    (message "The online ops users are: %s"  (mapconcat 'identity ops " "))
                  (message "There are no ops users online on %s" channel)))
            (user-error "The current buffer is not a channel")))
      (user-error "You must first be connected on IRC")))

  (defun my/erc-notify (nickname message)
    "Displays a notification message for ERC."
    (let* ((channel (buffer-name))
           (nick (erc-hl-nicks-trim-irc-nick nickname))
           (title (if (string-match-p (concat "^" nickname) channel)
                      nick
                    (concat nick " (" channel ")")))
           (msg (s-trim (s-collapse-whitespace message))))
      (my-appt-send-notification (concat nick ": " msg) :title title)))

  (defun my/erc-preprocess (string)
    "Avoids channel flooding."
    (setq str (string-trim (replace-regexp-in-string "\n+" " " str))))

  ;; TODO what it does?
  (defun my/erc-reset-track-mode ()
    "Resets ERC track mode."
    (interactive)
    (setq erc-modified-channels-alist nil)
    (erc-modified-channels-update)
    (erc-modified-channels-display)
    (force-mode-line-update))

  )

(defalias 'irc #'my/erc-start-or-switch)

(use-package erc-image
  :after erc)

(use-package erc-hl-nicks
  :after erc)

(provide 'my-irc)
