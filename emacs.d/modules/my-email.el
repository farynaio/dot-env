(defvar my/mu4e-local-path "/usr/local/Cellar/mu/HEAD-bf80b5b/share/emacs/site-lisp/mu/mu4e")

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

(setq
  smtpmail-smtp-service 587
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
