(require 'erc)
(require 'erc-services)

(eval-after-load 'erc
  '(progn
     (setq
       erc-nick "jarfaros"
       erc-away-nickname "jarfaros"
       erc-email-userid "adamfaryna@gmail.com"
       erc-prompt-for-nickserv-password nil
       erc-format-nick-function 'erc-format-@nick
       erc-nick-uniquifier "_"
       erc-prompt-for-password nil)

     (setq
       erc-kill-server-buffer-on-quit t
       erc-kill-queries-on-quit t
       erc-rename-buffers t)

     (add-to-list 'erc-modules 'autojoin t)
     (add-to-list 'erc-modules 'button t)
     (add-to-list 'erc-modules 'completion t)
     (add-to-list 'erc-modules 'fill t)
     (add-to-list 'erc-modules 'irccontrols t)
     (add-to-list 'erc-modules 'keep-place t)
     (add-to-list 'erc-modules 'list t)
     (add-to-list 'erc-modules 'match t)
     (add-to-list 'erc-modules 'menu t)
     (add-to-list 'erc-modules 'move-to-prompt t)
     (add-to-list 'erc-modules 'netsplit t)
     (add-to-list 'erc-modules 'networks t)
     (add-to-list 'erc-modules 'noncommands t)
     (add-to-list 'erc-modules 'readonly t)
     (add-to-list 'erc-modules 'ring t)
     (add-to-list 'erc-modules 'stamp t)
     (add-to-list 'erc-modules 'track t)

     (evil-set-initial-state 'erc-mode 'emacs)

     (defalias 'irc #'erc)
     ))

(provide 'my-irc)
