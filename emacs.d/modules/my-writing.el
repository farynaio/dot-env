(setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))
(setq save-abbrevs 'silently)

(define-abbrev-table 'global-abbrev-table '(
                                             ("im" "I'm" nil 0)
                                             ("dont" "don't" nil 0)
                                             ("cant" "can't" nil 0)
                                             ("its" "it's" nil 0)
                                             ("Its" "It's" nil 0)
                                             ("couldnt" "couldn't" nil 0)
                                             ("mustnt" "mustn't" nil 0)
                                             ("shouldnt" "shouldn't" nil 0)
                                             ("doesnt" "doesn't" nil 0)
                                             ("didnt" "didn't" nil 0)
                                             ("thats" "that's" nil 0)
                                             ("chinese" "Chinese" nil 0)
                                             ("whats" "what's" nil 0)
                                             ("Whats" "What's" nil 0)
                                             ))

(use-package langtool
  :init
  (progn
    (setq langtool-language-tool-jar (expand-file-name "LanguageTool/languagetool-commandline.jar" my/tools-path)))
  :config
  (progn
    (setq langtool-default-language "en-GB")
    (setq langtool-mother-tongue "en")))

(use-package google-translate
  :config
  (progn
    (setq
      google-translate-default-source-language "en"
      google-translate-default-target-language "pl")
    (bind-key ", t" #'google-translate-at-point evil-normal-state-map)))

(setq ispell-extra-args '("--sug-mode=ultra"))

(setq safe-local-variable-values '(
                                    (ispell-dictionary . "pl")
                                    (ispell-dictionary . "en")))

;; mode hooks
(setq flyspell-mode-hooks '(text-mode-hook org-mode-hook))

(if (executable-find "aspell")
  (dolist (i flyspell-mode-hooks)
    (add-hook i #'flyspell-prog-mode)))

;; TODO it could be rather based on ring implementation (hard to add new langs)
(defun dict-toggle ()
  "Toggle spell dictionary."
  (interactive)
  (if
    (string= ispell-current-dictionary "en")
    (ispell-change-dictionary "pl")
    (ispell-change-dictionary "en"))
  (message (concat "Current spell language is '" ispell-current-dictionary "'.")))

(bind-key ", d" #'dict-toggle evil-normal-state-map)

(provide 'my-writing)
