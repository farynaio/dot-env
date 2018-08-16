(setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))
(setq save-abbrevs 'silently)

(define-abbrev-table 'global-abbrev-table '(
                                             ("im" "I'm" nil 0)
                                             ("dont" "don't" nil 0)
                                             ("cant" "can't" nil 0)
                                             ;; ("its" "it's" nil 0)
                                             ;; ("Its" "It's" nil 0)
                                             ("it is" "it's" nil 0)
                                             ("It is" "It's" nil 0)
                                             ("isnt" "isn't" nil 0)
                                             ("couldnt" "couldn't" nil 0)
                                             (" have" "'ve" nil 0)
                                             ("You are" "You're" nil 0)
                                             ("mustnt" "mustn't" nil 0)
                                             ("shouldnt" "shouldn't" nil 0)
                                             ("wouldnt" "wouldn't" nil 0)
                                             ("wasnt" "wasn't" nil 0)
                                             ("was not" "wasn't" nil 0)
                                             ("havent" "haven't" nil 0)
                                             ("totaly" "totally" nil 0)
                                             ("whos" "who's" nil 0)
                                             ("everobodys" "everybody's" nil 0)
                                             ("doesnt" "doesn't" nil 0)
                                             ("didnt" "didn't" nil 0)
                                             ("thats" "that's" nil 0)
                                             ("chinese" "Chinese" nil 0)
                                             ("whats" "what's" nil 0)
                                             ("Whats" "What's" nil 0)
                                             ("What is" "What's" nil 0)
                                             ("what is" "what's" nil 0)
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
    (bind-key ", t" #'google-translate-at-point evil-visual-state-map)
    (bind-key ", t" #'google-translate-at-point evil-normal-state-map)))

(setq ispell-extra-args '("--sug-mode=ultra"))

(add-to-list 'safe-local-variable-values '(ispell-dictionary . "pl"))
(add-to-list 'safe-local-variable-values '(ispell-dictionary . "en"))

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

(use-package artbollocks-mode
  :config
  (progn
    (setq artbollocks-weasel-words-regex
      (concat "\\b" (regexp-opt
                      '("one of the" "should" "just" "sort of" "a lot" "probably" "maybe" "perhaps" "I think" "really" "pretty" "nice" "action" "utilize" "leverage"
                                        ; test
                         "clavicles" "collarbones" "tiny birds" "antlers" "thrumming" "pulsing" "wombs" "ribcage" "alabaster" "grandmother" "redacting fairytales" "retelling fairytales" "my sorrow" "the window speaking" "avocados" "the blank page" "marrow" "starlings" "giving birth" "giving birth to weird shit" "apples" "peeling back skin" "god" "the mountain trembling" "poetry is my remedy" "sharp fragments" "shards" "grandpa" "i can remember" "this is how it happened" "the pain" "greek myths" "poems about poems" "scars" "cold, stinging" "oranges" "the body" "struggles" "shadows" "the moon reflecting off the" "waves" "echoes in the night" "painted skies" "a hundred" "again and again" "peace, love" "whimsy" "brooklyn" "the summer solstice" "the lunar eclipse" "veins" "soul"
                         ) t) "\\b")
      artbollocks-jargon nil)))
    ;; (add-hook 'text-mode-hook #'artbollocks-mode)))

(provide 'my-writing)
