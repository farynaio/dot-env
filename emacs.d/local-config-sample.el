;; gpg email
(setq epa-file-encrypt-to '(""))

(eval-after-load 'org-caldav
  '(progn
    ; gmail calendar id
     (setq org-caldav-calendar-id "@group.calendar.google.com")
     ; google app id name
    (setq org-caldav-oauth2-client-id ".apps.googleusercontent.com")
    (setq org-caldav-oauth2-client-secret "")))

;; Mu
(setq user-mail-address "")

(eval-after-load 'mu4e
  '(progn
     (setq smtpmail-default-smtp-server "smtp.gmail.com")
     (setq mu4e-contexts `(
          ,(make-mu4e-context
             :name "G gmail-address"
             :match-func (lambda (msg)
                           (when msg (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
             :vars '(
                       (mu4e-trash-folder   . "/[gmail-address].Trash")
                       ;; (mu4e-refile-folder  . "/[gmail-address].Archive")
                       (mu4e-sent-folder    . "/[gmail-address].Sent")
                       (mu4e-drafts-folder  . "/[gmail-address].Drafts")

                       (mu4e-maildir-shortcuts .
                         (
                           ("/[gmail-address].Inbox"         . ?i)
                           ("/[gmail-address].Business UK"   . ?b)
                           ("/[gmail-address].Potwierdzenia" . ?c)
                           ("/[gmail-address].Important"     . ?m)
                           ("/[gmail-address].Starred"       . ?r)
                           ("/[gmail-address].Drafts"        . ?d)
                           ("/[gmail-address].Sent"          . ?s)
                           ("/[gmail-address].Trash"         . ?t)))
                       (user-mail-address . "gmail-address")
                       (smtpmail-smtp-user . "gmail-address")
                       (smtpmail-local-domain . "gmail.com")
                       (mu4e-sent-messages-behavior . 'delete)
                       (smtpmail-default-smtp-server . "smtp.gmail.com")
                       (smtpmail-smtp-server . "smtp.gmail.com")
                      (message-signature-file . "~/.signature_gmail")))

          ,(make-mu4e-context
              :name "A appdy-address"
              :match-func (lambda (msg)
                            (when msg (string-suffix-p "appdy.co.uk" (mu4e-message-field msg :maildir))))
              :vars '(
                       (my/mu4e-refile-rules . (("appdy-address" . "/Archive/Amazon")))
                       (mu4e-refile-folder .
                         (lambda (msg)
                           (let* ((to (cdar (mu4e-message-field msg :to)))
                                  (folder (or (cdar (member* to my/mu4e-refile-rules
                                                      :test #'(lambda (x y)
                                                                (string-match (car y) x))))
                                            "/Archive")))
                             folder)))

                       (mu4e-trash-folder  . "/[appdy-address].Trash")
                       (mu4e-refile-folder . "/[appdy-address].Archive")
                       (mu4e-sent-folder   . "/[appdy-address].Sent")
                       (mu4e-drafts-folder . "/[appdy-address].Drafts")

                       (mu4e-maildir-shortcuts .
                         (
                           ("/[appdy-address].Inbox"    . ?i)
                           ("/[appdy-address].Drafts"   . ?d)
                           ("/[appdy-address].Sent"     . ?s)
                           ("/[appdy-address].Trash"    . ?t)
                           ("/[appdy-address].Archive"  . ?a)
                           ("/[appdy-address].Junk"     . ?j)))

                       (user-mail-address             . "appdy-address")
                       (smtpmail-smtp-user            . "appdy-address")
                       (smtpmail-local-domain         . "smtp.appdy.co.uk")
                       (smtpmail-default-smtp-server  . "smtp.appdy.co.uk")
                       (smtpmail-smtp-server          . "smtp.appdy.co.uk")
                       (message-signature-file        . "~/.signature_appdy")
                       ))

           ,(make-mu4e-context
              :name "F freedomsamurai-address"
              :match-func (lambda (msg)
                            (when msg (string-suffix-p "freedomsamurai-address" (mu4e-message-field msg :maildir))))
              :vars '(
                       (mu4e-trash-folder  . "/[freedomsamurai-address].Trash")
                       (mu4e-refile-folder . "/[freedomsamurai-address].Archive")
                       (mu4e-sent-folder   . "/[freedomsamurai-address].Sent")
                       (mu4e-drafts-folder . "/[freedomsamurai-address].Drafts")

                       (mu4e-maildir-shortcuts .
                         (
                           ("/[freedomsamurai-address].Inbox"    . ?i)
                           ("/[freedomsamurai-address].Drafts"   . ?d)
                           ("/[freedomsamurai-address].Sent"     . ?s)
                           ("/[freedomsamurai-address].Trash"    . ?t)
                           ("/[freedomsamurai-address].Archive"  . ?a)
                           ("/[freedomsamurai-address].Junk"     . ?j)))

                       (user-mail-address             .        "freedomsamurai-address")
                       (smtpmail-smtp-user            .        "freedomsamurai-address")
                       ;; (smtpmail-local-domain .        "appdy.co.uk")
                       (smtpmail-local-domain         .        "smtp.appdy.co.uk")
                       (smtpmail-default-smtp-server  .        "smtp.appdy.co.uk")
                       (smtpmail-smtp-server          .        "smtp.appdy.co.uk")
                       ))

           ;; ,(make-mu4e-context
              ;; :name "N root@jarfar.nazwa.pl"
              ;; :match-func (lambda (msg)
              ;;               (when msg (string-suffix-p "/jarfar.nazwa.pl" (mu4e-message-field msg :maildir))))
              ;; :vars '(
              ;;          (mu4e-sent-folder "/jarfar.nazwa.pl/Sent")
              ;;          (mu4e-drafts-folder "/jarfar.nazwa.pl/Drafts")
              ;;          (user-mail-address "root@jarfar.nazwa.pl")
              ;;          (smtpmail-smtp-user "root@jarfar.nazwa.pl")
              ;;          (smtpmail-local-domain "jarfar.nazwa.pl")
              ;;          (smtpmail-default-smtp-server "jarfar.nazwa.pl")
              ;;          (smtpmail-smtp-server "jarfar.nazwa.pl")
              ;;          (smtpmail-smtp-service 587)
              ;;          (smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg"))
              ;;          (smtpmail-starttls-credentials '(("jarfar.nazwa.pl" 25 nil nil)))
              ;;          (smtpmail-stream-type starttls)
              ;;          ))
                             ))



))

(setq
  mml-secure-openpgp-sign-with-sender t
  ; gpg key id
  mml-secure-openpgp-signers '("")
  ; gpg key id
  epg-user-id "")

(setq gnus-ignored-from-addresses "devil")
