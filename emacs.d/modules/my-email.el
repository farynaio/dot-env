(require 'smtpmail)
(require 'cl)

(if (file-directory-p my/mu4e-local-path)
  (progn
    (add-to-list 'load-path my/mu4e-local-path)
    (require 'mu4e)
    (require 'org-mu4e))
  (message "'mu' not found"))

(setq send-mail-function 'smtpmail-send-it)

(setq nnheader-file-name-translation-alist '((?[ . ?_) (?] . ?_)))

(setq
  mm-coding-system-priorities '(utf-8 utf-8-mac)
  mm-decrypt-option t
  mm-verify-option t)

(setq
  mml2015-encrypt-to-self t
  mml2015-use 'epg
  mml2015-encrypt-to-self t
  mml2015-sign-with-sender t)

(setq
  message-send-mail-function 'smtpmail-send-it
  message-directory "~/.Mail/"
  message-default-charset 'utf-8
  message-kill-buffer-on-exit t
  message-forward-before-signature nil
  message-draft-coding-system 'utf-8
  ;; message-inhibit-body-encoding t
  ;; message-send-coding-system 'binary
  ;; message-send-coding-system 'utf-8
  ;; message-send-coding-system
  message-cite-function 'message-cite-original
  ;; message-cite-function 'message-cite-original-without-signature
  message-cite-style 'message-cite-style-gmail)

(push '(utf-8 . base64) mm-body-charset-encoding-alist)

(setq
  smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
  smtpmail-queue-mail nil
  ;; smtpmail-smtp-service 587
  ;; smtpmail-stream-type 'starttls
  ;; smtpmail-stream-type 'ssl
  smtpmail-queue-dir "~/Maildir/queue/cur")

(setq
  shr-inhibit-images t
  shr-use-colors nil
  shr-use-fonts nil
  shr-color-visible-distance-min 80
  shr-color-visible-luminance-min 5)

; Gnus
(setq
  gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
  gnus-treat-hide-citation t
  gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject ; is it needed?
  ;; gnus-use-adaptive-scoring t
  gnus-inhibit-slow-scoring "^nntp[+:]"
  ;; gnus-agent nil
  gnus-use-correct-string-widths nil
  gnus-check-new-newsgroups nil
  gnus-asynchronous t
  gnus-message-archive-group nil
  gnus-use-cache t ; cache everything
  gnus-read-active-file 'some
  gnus-thread-sort-functions
  '(gnus-thread-sort-by-score
     gnus-thread-sort-by-date
     (not gnus-thread-sort-by-number))
  gnus-thread-hide-subtree t ; hide specific threads?
  gnus-thread-ignore-subject t
  gnus-thread-indent-level 2
  gnus-sum-thread-tree-indent " "
  gnus-auto-select-first nil
  gnus-auto-select-next nil
  gnus-user-date-format-alist '((t . "%d.%m.%Y %H:%M"))
  gnus-summary-line-format "%D%U%R%z%I%(%[%4L: %-23,23f%]%) %s\n"
  gnus-activate-level 3
  gnus-select-method
  '(nnimap "Mail"
	   (nnimap-address "localhost")
	   (nnimap-stream network)
	   (nnimap-authenticator login)))

(setq
  starttls-gnutls-program "gnutls-cli"
  starttls-extra-arguments nil
  starttls-use-gnutls t)

(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(add-hook 'gnus-summary-mode-hook (lambda ()
                                    (local-set-key "y" 'gmail-archive)
                                    (local-set-key "$" 'gmail-report-spam)))
(add-hook 'message-sent-hook 'gnus-score-followup-thread)
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; (eval-after-load 'gnus-topic
;;   '(progn
;;      (setq gnus-message-archive-group '((format-time-string "sent.%Y")))
;;      (setq gnus-topic-topology '((("gmail" visible nil nil))
;;                                   ("Gnus" visible)
;;                                   (("misc" visible))))))

     ;; Please not the key of topic is specified in my sample setup
     ;; (setq gnus-topic-alist '(("gmail" ; the key of topic
     ;;                           "INBOX"
     ;;                           "[Gmail]/Sent Mail"
     ;;                           "Drafts")
     ;;                          ("misc" ; the key of topic
     ;;                           "nnfolder+archive:sent.2018-12"
     ;;                           "nnfolder+archive:sent.2018"
     ;;                           "nndraft:drafts")
     ;;                          ("Gnus")))))


(eval-after-load 'mu4e
  '(progn
     (evil-make-overriding-map mu4e-headers-mode-map 'motion)
     (evil-make-overriding-map mu4e-headers-mode-map 'normal)
     (evil-make-overriding-map mu4e-view-mode-map 'motion)
     (evil-make-overriding-map mu4e-view-mode-map 'normal)

     ;; main
     (bind-key "x"         #'mu4e-kill-update-mail                    mu4e-main-mode-map)
    ;; (,evil-mu4e-state mu4e-main-mode-map "s"               mu4e-headers-search)
    ;; (,evil-mu4e-state mu4e-main-mode-map "b"               mu4e-headers-search-bookmark)
    ;; (,evil-mu4e-state mu4e-main-mode-map "B"               mu4e-headers-search-bookmark-edit)

     (unbind-key "g" mu4e-view-mode-map)

     ;; view
     (bind-key "<tab>"      #'shr-next-link                            mu4e-view-mode-map)
     (bind-key "<backtab>"  #'shr-previous-link                        mu4e-view-mode-map)
     (bind-key "k"          #'shr-maybe-probe-and-copy-url             mu4e-view-mode-map)
     (bind-key "/"          #'evil-search-forward                      mu4e-view-mode-map)
     (bind-key "G"          #'end-of-buffer                            mu4e-view-mode-map)
     (bind-key "gg"         #'beginning-of-buffer                      mu4e-view-mode-map)
     (bind-key "}"          #'forward-paragraph                        mu4e-view-mode-map)
     (bind-key "{"          #'backward-paragraph                       mu4e-view-mode-map)
     (bind-key "v"          #'set-mark-command                         mu4e-view-mode-map)
     (bind-key "y"          #'evil-yank                                mu4e-view-mode-map)
     (bind-key "w"          #'evil-forward-word-begin                  mu4e-view-mode-map)
     (bind-key "b"          #'evil-backward-word-begin                 mu4e-view-mode-map)

     ;; headers
     (bind-key "RET"        #'mu4e-headers-view-message                mu4e-headers-mode-map)
     (bind-key "q"          #'mu4e~headers-quit-buffer                 mu4e-headers-mode-map)
     (bind-key "x"          #'mu4e-mark-execute-all                    mu4e-headers-mode-map)
     (bind-key "a"          #'mu4e-headers-action                      mu4e-headers-mode-map)
     (bind-key "C"          #'mu4e-compose-new                         mu4e-headers-mode-map)
     (bind-key "F"          #'mu4e-compose-forward                     mu4e-headers-mode-map)
     (bind-key "R"          #'mu4e-compose-reply                       mu4e-headers-mode-map)
     (bind-key "o"          #'mu4e-headers-change-sorting              mu4e-headers-mode-map)
     (bind-key "&"          #'mu4e-headers-mark-custom                 mu4e-headers-mode-map)
     (bind-key "A"          #'mu4e-headers-mark-for-action             mu4e-headers-mode-map)
     (bind-key "m"          #'mu4e-headers-mark-for-move               mu4e-headers-mode-map)
     (bind-key "r"          #'mu4e-headers-mark-for-refile             mu4e-headers-mode-map)
     (bind-key "D"          #'mu4e-headers-mark-for-delete             mu4e-headers-mode-map)
     (bind-key "d"          #'mu4e-headers-mark-for-trash              mu4e-headers-mode-map)
     (bind-key "="          #'mu4e-headers-mark-for-untrash            mu4e-headers-mode-map)
     (bind-key "u"          #'mu4e-headers-mark-for-unmark             mu4e-headers-mode-map)
     (bind-key "U"          #'mu4e-mark-unmark-all                     mu4e-headers-mode-map)
     (bind-key "["          #'mu4e-headers-prev-unread                 mu4e-headers-mode-map)
     (bind-key "]"          #'mu4e-headers-next-unread                 mu4e-headers-mode-map)
     (bind-key "G"          #'end-of-buffer                            mu4e-headers-mode-map)
     (bind-key "gg"         #'beginning-of-buffer                      mu4e-headers-mode-map)
     (bind-key "l"          #'mu4e-show-log                            mu4e-headers-mode-map)
     (bind-key "I"          #'mu4e-headers-toggle-include-related      mu4e-headers-mode-map)
     (bind-key "T"          #'mu4e-headers-toggle-threading            mu4e-headers-mode-map)
     (bind-key "D"          #'mu4e-headers-toggle-skip-duplicates      mu4e-headers-mode-map)
     (bind-key "s"          #'mu4e-headers-search                      mu4e-headers-mode-map)
     (bind-key "e"          #'mu4e-headers-search-edit                 mu4e-headers-mode-map)
     (bind-key "U"          #'mu4e-update-mail-and-index               mu4e-headers-mode-map)
     (bind-key "n"          #'evil-search-next                         mu4e-headers-mode-map)
     (bind-key "N"          #'evil-search-previous                     mu4e-headers-mode-map)
     (bind-key "/"          #'evil-search-forward                      mu4e-headers-mode-map)
     (bind-key "*"          #'evil-search-word-forward                 mu4e-headers-mode-map)
     (bind-key "#"          #'evil-search-word-backward                mu4e-headers-mode-map)
     (bind-key "x"          #'my/mu4e-mark-execute-all-no-confirm      mu4e-headers-mode-map)

     (defun my/mu4e-mark-execute-all-no-confirm ()
       "Execute all marks without confirmation."
       (interactive)
       (mu4e-mark-execute-all 'no-confirm))

     (setq mail-user-agent 'mu4e-user-agent)

     (setq
       mu4e-user-mail-address-list '("adamfaryna@gmail.com" "adamfaryna@appdy.co.uk")
       mu4e-compose-context-policy 'ask
       mu4e-view-prefer-html t
       mu4e-confirm-quit nil
       mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum
       ;; mu4e-view-html-plaintext-ratio-heuristic 5
       ;; mu4e-update-interval 900
       mu4e-compose-in-new-frame nil
       mu4e-html2text-command 'mu4e-shr2text
       ;; mu4e-html2text-command "w3m -dump -dump_extra -T text/html"
       mu4e-headers-results-limit 100
       mu4e-cache-maildir-list t
       mu4e-compose-dont-reply-to-self t
       mu4e-headers-include-related t
       mu4e-view-show-addresses t
       mu4e-view-show-images t
       mu4e-headers-date-format "%Y/%m/%d %H:%M %Z"
       mu4e-view-scroll-to-next nil
       mu4e-headers-fields '((:human-date . 22)
                              (:flags . 6)
                              (:from . 22)
                              (:subject))
       ;; mu4e-use-fancy-chars t
       mu4e-change-filenames-when-moving t
       mu4e-get-mail-command "true"
       mu4e-attachment-dir  "~/Downloads/mail_attachments"
       mu4e-index-cleanup nil
       mu4e-display-update-status-in-modeline t
       mu4e-index-lazy-check t
       mu4e-headers-skip-duplicates t)

     (add-to-list 'mu4e-view-actions
       '("ViewInBrowser" . mu4e-action-view-in-browser) t)

     (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

     (defun my/mu4e-compose-mode-hook ()
         (make-variable-buffer-local 'company-backends)
         (add-to-list 'company-backends 'company-bbdb)

         (set-fill-column 72)
         (save-excursion
           (goto-char (point-min))
           (insert (concat "X-Mailer: mu4e " mu4e-mu-version "; emacs " emacs-version "\n"))
           ))

     (add-hook 'mu4e-compose-mode-hook #'my/mu4e-compose-mode-hook)

     (defun my/mu4e-set-account ()
       "Set the account for composing a message."
       user-mail-address)
     (add-hook 'mu4e-compose-pre-hook #'my/mu4e-set-account)

     (advice-add 'mu4e-message :around #'my/advice-around-skip)

     (defalias 'mu #'mu4e)
     ))

(use-package org-mime)

(defun gmail-archive ()
  "Archive the current or marked mails.
This moves them into the All Mail folder."
  (interactive)
  (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/All Mail"))

(defun gmail-report-spam ()
  "Report the current or marked mails as spam.
This moves them into the Spam folder."
  (interactive)
  (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/Spam"))

(provide 'my-email)
