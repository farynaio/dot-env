(defvar my/mu4e-local-path "/usr/local/Cellar/mu/HEAD-58492f8/share/emacs/site-lisp/mu/mu4e")

(if (file-directory-p my/mu4e-local-path)
  (progn
    (add-to-list 'load-path my/mu4e-local-path)
    (require 'mu4e)
    (require 'org-mu4e))
  (message "'mu' not found"))

(use-package evil-mu4e
  :config
  (progn
    (evil-define-key 'normal mu4e-compose-mode-map (kbd "TAB") #'message-tab)
    (evil-define-key 'normal mu4e-view-mode-map (kbd "TAB") #'shr-next-link)
    (evil-define-key 'normal mu4e-view-mode-map (kbd "BACKTAB") #'shr-previous-link)))

; Gnus
(setq
  gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
  gnus-treat-hide-citation t
  gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject ; is it needed?
  ;; gnus-use-adaptive-scoring t
  gnus-inhibit-slow-scoring "^nntp[+:]"
  gnus-agent nil
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
  starttls-gnutls-program "gnutls-cli"
  starttls-extra-arguments nil
  starttls-use-gnutls t
  gnus-auto-select-next nil
  gnus-user-date-format-alist '((t . "%d.%m.%Y %H:%M"))
  gnus-activate-level 3
  message-default-charset `utf-8
  ;; gnus-default-adaptive-score-alist
  ;; '((gnus-unread-mark)
  ;;    (gnus-ticked-mark (subject 10))
  ;;    (gnus-killed-mark (subject -5))
  ;;    (gnus-catchup-mark (subject -1)))
                                        ; gnus-select-method '(nnnil "")

  gnus-summary-line-format "%D%U%R%z%I%(%[%4L: %-23,23f%]%) %s\n"

  ;; gnus-parameters '(
  message-directory "~/.Mail/"
  message-send-mail-function 'smtpmail-send-it
  send-mail-function 'smtpmail-send-it
  smtpmail-smtp-server "smtp.gmail.com"
  nnir-imap-default-search-key "gmail"
  nnheader-file-name-translation-alist '((?[ . ?_) (?] . ?_)))

(setq gnus-select-method
      '(nnimap "Mail"
	       (nnimap-address "localhost")
	       (nnimap-stream network)
	       (nnimap-authenticator login)))

(setq
  smtpmail-smtp-service 587
  mm-coding-system-priorities '(utf-8-mac utf-8)
  mml2015-encrypt-to-self t
  mm-verify-option t
  mm-decrypt-option t
  mml2015-use 'epg
  mml2015-encrypt-to-self t
  mml2015-sign-with-sender t)

(if (executable-find "w3m")
  (use-package w3m
    :config (progn
              (setq
                w3m-default-display-inline-images t
                w3m-use-cookies t
                mm-text-html-renderer 'w3m
                w3m-coding-system 'utf-8
                w3m-file-coding-system 'utf-8
                w3m-file-name-coding-system 'utf-8
                w3m-input-coding-system 'utf-8
                w3m-output-coding-system 'utf-8
                w3m-terminal-coding-system 'utf-8)

              (if (string= system-type "darwin")
                (setq process-connection-type nil))
              ))
  (message (concat "Executable 'w3m' not found!")))

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

(setq mail-user-agent 'mu4e-user-agent)

(eval-after-load 'mu4e
  '(progn
     (setq
       message-send-mail-function 'smtpmail-send-it
       message-kill-buffer-on-exit t
       mu4e-view-prefer-html t
       mu4e-update-interval 900
       shr-color-visible-luminance-min 80
       mu4e-headers-include-related t
       mu4e-view-show-images t
       ;; mu4e-get-mail-command "offlineimap -o"
       ;; mu4e-get-mail-command "offlineimap"
       mu4e-get-mail-command "true"
       ;; mu4e-attachment-dir  "/Volumes/RAM_Disk/"
       mu4e-attachment-dir  "~/Downloads/mail_attachments"
       mu4e-index-cleanup nil
       mu4e-display-update-status-in-modeline t
       mu4e-index-lazy-check t
       mu4e-headers-skip-duplicates t
       mu4e-update-interval 300)

     (when (fboundp 'imagemagick-register-types)
       (imagemagick-register-types))

     (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

     (add-hook 'mu4e-compose-mode-hook
       (lambda ()
         (set-fill-column 72)
         ;; (local-set-key (kbd "C-c <return> C-s") 'my/sign-this-message)
         ;; (local-set-key (kbd "C-c <return> C-e") 'my/encrypt-this-message)
         (save-excursion
           (goto-char (point-min))
           (insert (concat "X-Mailer: mu4e " mu4e-mu-version "; emacs " emacs-version "\n")))))

     (defun my/mu4e-set-account ()
       "Set the account for composing a message.
         (https://www.djcbsoftware.nl/code/mu/mu4e/Multiple-accounts.html)"
       (let* (
               (account
                 ;; (if mu4e-compose-parent-message
                 ;;   (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                 ;;     (string-match "/\\(.*?\\)/" maildir)
                 ;;     (match-string 1 maildir))
                 (completing-read (format "Compose with account: (%s) "
                                    (mapconcat #'(lambda (var) (car var))
                                      my/mu4e-account-alist "/"))
                   (mapcar #'(lambda (var) (car var)) my/mu4e-account-alist)
                   nil t nil nil (caar my/mu4e-account-alist))) ;)
               (account-vars (cdr (assoc account my/mu4e-account-alist))))
         (if account-vars
           (mapc #'(lambda (var)
                     (set (car var) (cadr var)))
             account-vars)
           (error "No email account found"))))
     (add-hook 'mu4e-compose-pre-hook #'my/mu4e-set-account)

     (evil-make-overriding-map mu4e-headers-mode-map 'motion)))

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

(defalias 'mu #'mu4e)
;; (defalias 'inbox #'mu4e)

(provide 'my-email)
