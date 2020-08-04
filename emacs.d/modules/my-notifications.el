; source http://article.gmane.org/gmane.emacs.orgmode/66151
(require 'appt)

;; set up the call to terminal-notifier
(defvar my-notifier-path "/usr/local/bin/terminal-notifier")

(defun my-appt-send-notification (title msg)
  (shell-command (concat my-notifier-path " -message \"" msg "\" -title \"" title "\"")))

;; designate the window function for my-appt-send-notification
(defun my-appt-display (min-to-app new-time msg)
  (my-appt-send-notification
    (format "'Appointment in %s minutes'" min-to-app)    ;; passed to -title in terminal-notifier call
    (format "'%s'" msg)))                                ;; passed to -message in terminal-notifier call
(setq appt-disp-window-function (function my-appt-display))
(setq appt-time-msg-list nil)    ;; clear existing appt list
(setq appt-display-interval '0) ;; warn every 10 minutes from t - appt-message-warning-time
(setq appt-message-warning-time '0)  ;; send first warning 10 minutes before appointment
(setq appt-display-mode-line nil)     ;; don't show in the modeline
(setq appt-display-format 'window)   ;; pass warnings to the designated window function
(appt-activate 1)                ;; activate appointment notification
(display-time)                   ;; activate time display
(display-time-mode -1)

(provide 'my-notifications)
