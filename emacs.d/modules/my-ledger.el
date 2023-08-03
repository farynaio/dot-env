(use-package ledger-mode
  :hook ((ledger-mode . company-mode)
          ;; (ledger-mode . ledger-flymake-enable)
          )
  :bind (:map ledger-mode-map
          ("C-c C-c" . ledger-post-align-dwim)
          ("C-x C-s" . my/ledger-save)
          ("C-c C-l" . hydra-ledger/body))
  :mode "\\.ledger\\'"
  :preface
  (defun my/ledger-save ()
    "Automatically clean the ledger buffer at each save."
    (interactive)
    ;; (ledger-mode-clean-buffer)
    (save-buffer))
  :custom
  (ledger-clear-whole-transactions t)
  (ledger-post-account-alignment-column 2)
  (ledger-reconcile-default-commodity "GBP")
  (ledger-reports
    '(("account statement" "%(binary) reg --real [[ledger-mode-flags]] -f %(ledger-file) ^%(account)")
       ("balance sheet" "%(binary) --real [[ledger-mode-flags]] -f %(ledger-file) bal ^assets ^liabilities ^equity")
       ("budget" "%(binary) --empty -S -T [[ledger-mode-flags]] -f %(ledger-file) bal ^assets:bank ^assets:receivables ^assets:cash ^assets:budget")
       ("budget goals" "%(binary) --empty -S -T [[ledger-mode-flags]] -f %(ledger-file) bal ^assets:bank ^assets:receivables ^assets:cash ^assets:'budget goals'")
       ("budget obligations" "%(binary) --empty -S -T [[ledger-mode-flags]] -f %(ledger-file) bal ^assets:bank ^assets:receivables ^assets:cash ^assets:'budget obligations'")
       ("budget debts" "%(binary) --empty -S -T [[ledger-mode-flags]] -f %(ledger-file) bal ^assets:bank ^assets:receivables ^assets:cash ^assets:'budget debts'")
       ("cleared" "%(binary) cleared [[ledger-mode-flags]] -f %(ledger-file)")
       ("equity" "%(binary) --real [[ledger-mode-flags]] -f %(ledger-file) equity")
       ("income statement" "%(binary) --invert --real -S -T [[ledger-mode-flags]] -f %(ledger-file) bal ^income ^expenses -p \"this month\"")))
  :config
  (pretty-hydra-define hydra-ledger
    (:hint nil :color teal :quit-key "q" :title (with-faicon "comments-o" "Ledger" 1 -0.05))
    ("Action"
      (
        ;; ("b" my/erc-browse-last-url "browse last url")
        ("b" ledger-display-balance-at-point "bal at point")
        ("s" (lambda () (interactive) (ledger-sort-buffer) (save-buffer))  "sort")
        ("t" ledger-display-ledger-stats "stats")
        ("r" ledger-report "report"))))

  (unbind-key "<tab>" ledger-mode-map))

(use-package flycheck-ledger
  :after ledger-mode)

(provide 'my-ledger)