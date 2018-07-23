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
             :name "G @gmail.com"
             :match-func (lambda (msg)
                           (when msg (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
             :vars '(
                      (mu4e-trash-folder . "/[Gmail].Bin")
                      (mu4e-refile-folder . "/[Gmail].Archive")
                      (mu4e-maildir-shortcuts .
                        (("/[Gmail].Inbox"          . ?i)
                          ("/Business UK"           . ?a)
                          ("/[Gmail].Important"     . ?m)
                          ("/[Gmail].Starred"       . ?r)
                          ("/[Gmail].Drafts"        . ?d)
                          ("/[Gmail].Sent Mail"     . ?s)
                          ("/[Gmail].Bin"           . ?b)))))
          ,(make-mu4e-context
             :name "N Appdy.net"
             :match-func (lambda (msg)
                           (when msg (string-suffix-p "/Appdy.net" (mu4e-message-field msg :maildir))))
             :vars '(
                      (mu4e-trash-folder . "/Appdy.net/Trash")
                      ;; (mu4e-refile-folder . "/[Gmail].Archive")
                      (mu4e-maildir-shortcuts .
                        (("/Appdy.net/Inbox"    . ?i)
                         ("/Appdy.net/Drafts"   . ?d)
                         ("/Appdy.net/Sent"     . ?s)
                         ("/Appdy.net/Trash"    . ?b)
                         ("/Appdy.net/Junk"     . ?j)))))
          ,(make-mu4e-context
             :name "A @appdy.co.uk"
             :match-func (lambda (msg)
                           (when msg (string-suffix-p "/Appdy.co.uk" (mu4e-message-field msg :maildir))))
             :vars '(
                      (mu4e-trash-folder . "/Appdy.co.uk/Trash")
                      (my/mu4e-refile-rules . (("@appdy.co.uk" . "/Appdy.co.uk/Archive.Amazon")))
                      (mu4e-refile-folder .
                        (lambda (msg)
                          (let* ((to (cdar (mu4e-message-field msg :to)))
                                  (folder (or (cdar (member* to my/mu4e-refile-rules
                                                      :test #'(lambda (x y)
                                                                (string-match (car y) x))))
                                            "/Appdy.co.uk/Archive")))
                            folder)))
                      (mu4e-maildir-shortcuts .
                        (("/Appdy.co.uk/Inbox"    . ?i)
                         ("/Appdy.co.uk/Drafts"   . ?d)
                         ("/Appdy.co.uk/Sent"     . ?s)
                         ("/Appdy.co.uk/Trash"    . ?b)
                         ("/Appdy.co.uk/Archive"  . ?a)
                         ("/Appdy.co.uk/Junk"     . ?j)))))
                         ))

     (defvar my/mu4e-account-alist
       '(("@gmail.com"
           (mu4e-sent-folder "/[Gmail].Sent Mail")
           (mu4e-drafts-folder "/[Gmail].Drafts")
           (user-mail-address "@gmail.com")
           (smtpmail-smtp-user "@gmail.com")
           (smtpmail-local-domain "gmail.com")
           (smtpmail-default-smtp-server "smtp.gmail.com")
           (smtpmail-smtp-server "smtp.gmail.com")
           (smtpmail-smtp-service 587)
           (message-signature-file "~/.signature_gmail")
           )
          ("@appdy.co.uk"
            (mu4e-sent-folder "/Appdy.co.uk/Sent")
            (mu4e-drafts-folder "/Appdy.co.uk/Drafts")
            (user-mail-address "@appdy.co.uk")
            (smtpmail-smtp-user "@appdy.co.uk")
            (smtpmail-local-domain "appdy.co.uk")
            (smtpmail-default-smtp-server "smtp.zenbox.pl")
            (smtpmail-smtp-server "smtp.zenbox.pl")
            (smtpmail-smtp-service 587)
            (message-signature-file "~/.signature_appdy")
            )
          ))
))

(setq
  mml-secure-openpgp-sign-with-sender t
  ; gpg key id
  mml-secure-openpgp-signers '("")
  ; gpg key id
  epg-user-id "")

(setq gnus-ignored-from-addresses "devil")
