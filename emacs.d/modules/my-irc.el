(require 'erc)

(eval-after-load 'erc
  '(progn
     (setq
       erc-nick "jarfar"
       erc-away-nickname "jarfar"
       erc-email-userid "jarfar")

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
     ))
