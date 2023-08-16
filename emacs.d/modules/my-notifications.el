; source http://article.gmane.org/gmane.emacs.orgmode/66151
;; TODO this needs alternative notifier app for Linux

;;; Code:

(use-package appt
  :straight nil
  :init
  (when (eq system-type 'darwin)
    (setq my-notifier-path "/usr/local/bin/terminal-notifier"))
  :preface
  (defun my-appt-send-notification (title msg)
    (shell-command (concat my-notifier-path " -message \"" msg "\" -title \"" title "\"")))
  :custom
  (appt-disp-window-function #'my-appt-display)
  (appt-time-msg-list nil)
  (appt-display-interval 0)
  (appt-message-warning-time 0)
  (appt-display-mode-line nil)
  (appt-display-format 'window)

  :config
  (appt-activate 1)

  (defun my-appt-display (min-to-app new-time msg)
    (my-appt-send-notification
      (format "'Appointment in %s minutes'" min-to-app)
      (format "'%s'" msg))))

(provide 'my-notifications)
;;; my-notifications.el ends here