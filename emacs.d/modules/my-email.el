
;;; Code:

(defun my/smtpmail-send-it  (&rest args)
  "Send email without checking certificates.
ARGS is not processed."
  (let ((gnutls-verify-error nil)
         (tls-checktrust nil))
    (apply 'smtpmail-send-it args)))

(use-package message
  :straight nil
  :commands (message-send message-send-and-exit mu4e)
  :custom
  (send-mail-function 'my/smtpmail-send-it)
  (message-directory "~/Mail/")
  (message-default-charset 'utf-8)
  (message-kill-buffer-on-exit t)
  (message-forward-before-signature nil)
  (message-draft-coding-system 'utf-8)
  ;; message-inhibit-body-encoding t
  ;; message-send-coding-system 'binary
  ;; message-send-coding-system 'utf-8
  ;; message-send-coding-system
  (message-cite-function #'message-cite-original)
  ;; message-cite-function 'message-cite-original-without-signature
  (message-cite-style 'message-cite-style-gmail)
  ;; :config
  ;; (add-hook 'message-sent-hook 'mml-secure-message-sign-pgpmime)
  )

;; (setq nnheader-file-name-translation-alist '((?[ . ?_) (?] . ?_)))

(setq-default
  mm-coding-system-priorities '(utf-8 utf-8-mac)
  mm-decrypt-option t
  mm-verify-option t)

(setq-default
  mml2015-encrypt-to-self t
  mml2015-use 'epg
  mml2015-encrypt-to-self t
  mml2015-sign-with-sender t)

(add-to-list 'mm-body-charset-encoding-alist '(utf-8 . base64))

(use-package smtpmail
  :commands (message-send message-send-and-exit mu4e)
  :straight nil
  :custom
  (smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg"))
  (smtpmail-queue-dir "~/Mail/queue/cur")
  (smtpmail-queue-mail nil)
  (smtpmail-smtp-service 587)
  (smtpmail-stream-type 'ssl))

(use-package shr
  ;; :commands elfeed mu4e
  :commands (message-send message-send-and-exit mu4e)
  :straight nil
  :custom
  (shr-inhibit-images t)
  (shr-use-colors nil)
  (shr-use-fonts nil)
  (shr-color-visible-distance-min 80)
  (shr-color-visible-luminance-min 5))

(use-package gnus
  :straight nil
  :commands gnus
  ;; :commands message-send message-send-and-exit mu4e
  :hook ((message-sent . gnus-score-followup-thread)
          (gnus-group-mode . gnus-topic-mode))
  :custom
  (gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
  (gnus-treat-hide-citation t)
  (gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject) ; is it needed?)
  ;; gnus-use-adaptive-scoring t
  (gnus-inhibit-slow-scoring "^nntp[+:]")
  ;; gnus-agent nil
  (gnus-use-correct-string-widths nil)
  (gnus-check-new-newsgroups nil)
  (gnus-asynchronous t)
  (gnus-message-archive-group nil)
  (gnus-use-cache t)
  (gnus-read-active-file 'some)
  (gnus-thread-sort-functions
    '(gnus-thread-sort-by-score
       gnus-thread-sort-by-date
       (not gnus-thread-sort-by-number)))
  (gnus-thread-hide-subtree t)
  (gnus-thread-ignore-subject t)
  (gnus-thread-indent-level 2)
  (gnus-sum-thread-tree-indent " ")
  (gnus-auto-select-first nil)
  (gnus-auto-select-next nil)
  (gnus-user-date-format-alist '((t . "%d.%m.%Y %H:%M")))
  (gnus-summary-line-format "%D%U%R%z%I%(%[%4L: %-23,23f%]%) %s\n")
  (gnus-activate-level 3)
  (gnus-select-method
    '(nnimap "Mail"
	     (nnimap-address "localhost")
	     (nnimap-stream network)
	     (nnimap-authenticator login))))

(setq
  starttls-gnutls-program "gnutls-cli"
  starttls-extra-arguments nil
  starttls-use-gnutls t)

;; (when (fboundp 'imagemagick-register-types)
;;   (imagemagick-register-types)
;;   )

(use-package mu4e
  :straight nil
  :preface
  (add-to-list 'load-path
    (if (eq system-type 'darwin)
      "/usr/local/opt/mu/share/emacs/site-lisp/mu/mu4e"
      "/usr/share/emacs/site-lisp/mu4e"))
  :init
  (unless (executable-find "mu")
    (message "No executable 'mu' found"))
  (unless (locate-library "mu4e")
    (message "No 'mu4e' library found"))
  :if (and (executable-find "mu") (locate-library "mu4e"))
  :commands mu4e
  :hook (mu4e-compose-mode . (lambda () (when (and (fboundp 'company-mode) company-mode) (company-mode 1))))
  :bind (:map mu4e-main-mode-map
          ("U" . (lambda () (interactive) (mu4e-update-mail-and-index t)))
          ("x" . mu4e-kill-update-mail)
          :map mu4e-view-mode-map
          ("<tab>" . shr-next-link)
          ("<backtab>" . shr-previous-link)
          ("k" . shr-maybe-probe-and-copy-url)
          ("/" . evil-search-forward)
          ("G" . end-of-buffer)
          ("gg" . beginning-of-buffer)
          ("}" . forward-paragraph)
          ("{" . backward-paragraph)
          ("v" . set-mark-command)
          ("y" . evil-yank)
          ("w" . evil-forward-word-begin)
          ("b" . evil-backward-word-begin)
          :map mu4e-headers-mode-map
          ("RET" . mu4e-headers-view-message)
          ("q" . mu4e~headers-quit-buffer)
          ("x" . mu4e-mark-execute-all)
          ("a" . mu4e-headers-action)
          ("C" . mu4e-compose-new)
          ("F" . mu4e-compose-forward)
          ("R" . mu4e-compose-reply)
          ("o" . mu4e-headers-change-sorting)
          ("&" . mu4e-headers-mark-custom)
          ("A" . mu4e-headers-mark-for-action)
          ("m" . mu4e-headers-mark-for-move)
          ("r" . mu4e-headers-mark-for-refile)
          ("D" . mu4e-headers-mark-for-delete)
          ("d" . mu4e-headers-mark-for-trash)
          ("=" . mu4e-headers-mark-for-untrash)
          ("u" . mu4e-headers-mark-for-unmark)
          ("U" . mu4e-mark-unmark-all)
          ("[" . mu4e-headers-prev-unread)
          ("]" . mu4e-headers-next-unread)
          ("l" . mu4e-show-log)
          ("I" . mu4e-headers-toggle-include-related)
          ("T" . mu4e-headers-toggle-threading)
          ("D" . mu4e-headers-toggle-skip-duplicates)
          ("s" . mu4e-headers-search)
          ("e" . mu4e-headers-search-edit)
          ("U" . (lambda () (interactive) (mu4e-update-mail-and-index t)))
          ("n" . evil-search-next)
          ("N" . evil-search-previous)
          ("/" . evil-search-forward)
          ("*" . evil-search-word-forward)
          ("#" . evil-search-word-backward)
          ("x" . my/mu4e-mark-execute-all-no-confirm))
  :preface
  (defun my/mu4e-mark-execute-all-no-confirm ()
    "Execute all marks without confirmation."
    (interactive)
    (mu4e-mark-execute-all 'no-confirm))
  :custom
  (mail-user-agent 'mu4e-user-agent)
  (mu4e-context-policy 'pick-first)
  (mu4e-compose-context-policy 'ask)
  (mu4e-view-prefer-html t)
  (mu4e-confirm-quit nil)
  (mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum)
  (mu4e-compose-in-new-frame nil)
  (mu4e-html2text-command 'mu4e-shr2text)
  (mu4e-headers-results-limit 100)
  (mu4e-cache-maildir-list t)
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-compose-signature-auto-include nil)
  (mu4e-headers-include-related t)
  (mu4e-view-show-addresses t)
  (mu4e-view-show-images t)
  (mu4e-headers-date-format "%Y/%m/%d %H:%M %Z")
  (mu4e-view-scroll-to-next nil)
  (mu4e-html2text-command "iconv -c -t utf-8 | pandoc -f html -t plain")
  (mu4e-org-contacts-file (expand-file-name "~/Documents/emacs/private/contacts.org.gpg"))
  (mu4e-headers-fields '((:human-date . 22)
                          (:flags . 6)
                          (:from . 22)
                          (:subject)))
  (mu4e-change-filenames-when-moving t)
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-attachment-dir  "~/Downloads")
  (mu4e-index-cleanup nil)
  (mu4e-display-update-status-in-modeline t)
  (mu4e-index-lazy-check t)
  (mu4e-headers-skip-duplicates t)
  :config
  (unbind-key "g" mu4e-view-mode-map)

  (evil-define-key '(motion emacs normal) mu4e:view-mode-map
    (kbd "C-d") #'evil-scroll-down
    (kbd "C-u") #'evil-scroll-up)

  (evil-define-key '(motion emacs) mu4e-headers-mode-map
    (kbd "C-d") #'evil-scroll-down
    (kbd "C-u") #'evil-scroll-up)

  (evil-define-key 'visual mu4e-compose-mode-map
    (kbd "H") #'org-mime-htmlize)

  (evil-make-overriding-map mu4e-headers-mode-map 'motion)
  (evil-make-overriding-map mu4e-headers-mode-map 'normal)
  (evil-make-overriding-map mu4e-view-mode-map 'motion)
  (evil-make-overriding-map mu4e-view-mode-map 'normal)

  ;; (,evil-mu4e-state mu4e-main-mode-map "s"               mu4e-headers-search)
  ;; (,evil-mu4e-state mu4e-main-mode-map "b"               mu4e-headers-search-bookmark)
  ;; (,evil-mu4e-state mu4e-main-mode-map "B"               mu4e-headers-search-bookmark-edit)

  ;; (add-to-list 'mu4e-headers-actions '("org-contact-add" . mu4e-action-add-org-contact) t)
  ;; (add-to-list 'mu4e-view-actions '("org-contact-add" . mu4e-action-add-org-contact) t)

  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)
  (add-to-list 'mu4e-view-actions '("XWidget View" . mu4e-action-view-with-xwidget) t)

  ;; (add-to-list 'mu4e-view-actions
  ;;   '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  (defun my/mu4e-compose-mode-hook ()
    (setq-local
      fill-column 72)
      ;; company-backends '((company-bbdb company-files company-dabbrev)))
    (save-excursion
      (goto-char (point-min))
      (insert (format "X-Mailer: mu4e %s; emacs %s\n" mu4e-mu-version emacs-version))))

  (add-hook 'mu4e-compose-mode-hook 'my/mu4e-compose-mode-hook)
  (add-hook 'mu4e-main-mode-hook (lambda () (mu4e-update-mail-and-index t)))

  ;; (defun my/mu4e-set-account ()
  ;;   "Set the account for composing a message."
  ;;   user-mail-address)

  ;; (add-hook 'mu4e-compose-pre-hook 'my/mu4e-set-account)

  (advice-add 'mu4e-message :override (lambda (&rest r))))

(use-package org-mu4e
  :after mu4e
  :straight nil)

(use-package mu4e-maildirs-extension
  :after mu4e
  :config
  (mu4e-maildirs-extension))

(use-package org-mime
  :after mu4e)

(defalias 'mu #'mu4e)

(provide 'my-email)
;;; my-email.el ends here
