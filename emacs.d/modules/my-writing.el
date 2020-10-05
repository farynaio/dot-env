(require 'text-mode)
(require 'flyspell)
(require 'smart-quotes)

(eval-after-load 'flyspell
  '(progn
     (diminish 'flyspell-mode "fly")

     (add-to-list 'ispell-skip-region-alist '(":PROPERTIES:" . ":END:"))
     (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
     (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))
     ))

(setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))
(setq save-abbrevs 'silently)

(defvar my/en-abbrevs nil)
(define-abbrev-table
  'my/en-abbrevs '(
                    ("im" "I'm" nil 0)
                    ("ami" "am I" nil 0)
                    ("dont" "don't" nil 0)
                    ("cant" "can't" nil 0)
                    ("aint" "ain't" nil 0)
                    ("isnt" "isn't" nil 0)
                    ("wont" "won't" nil 0)
                    ("couldnt" "couldn't" nil 0)
                    ("Youre" "You're" nil 0)
                    ("youre" "you're" nil 0)
                    ("Youd" "You'd" nil 0)
                    ("youd" "you'd" nil 0)
                    ;; ("Were" "We're" nil 0)
                    ;; ("were" "we're" nil 0)
                    ("arent" "aren't" nil 0)
                    ("mustnt" "mustn't" nil 0)
                    ("shouldnt" "shouldn't" nil 0)
                    ("wouldnt" "wouldn't" nil 0)
                    ("wasnt" "wasn't" nil 0)
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
                    ;; ("Well" "We'll" nil 0)
                    ("theyre" "they're" nil 0)
                    ;; ("Lets" "Let's" nil 0)
                    ("hows" "how's" nil 0)
                    ("Ill" "I'll" nil 0)
                    ("youve" "you've" nil 0)
                    ))

(defvar my/pl-abbrevs nil)
(define-abbrev-table 'my/pl-abbrevs '())

(define-minor-mode my/en-mode
  "Language mode for 'en'."
  :init-value nil
  :lighter " en"
  (setq-local ispell-local-dictionary "en")
  (setq-local local-abbrev-table my/en-abbrevs))

(define-minor-mode my/pl-mode
  "Language mode for 'pl'."
  :init-value nil
  :lighter " pl"
  (setq-local ispell-local-dictionary "pl")
  (setq-local local-abbrev-table my/pl-abbrevs))

(setq my/lang-modes
  '((en . my/en-mode) (pl . my/pl-mode)))

(defun my/lang-modes-deactivate ()
  "Deactivate all lang modes."
  (interactive)
  (my/en-mode -1)
  (my/pl-mode -1))

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
      google-translate-default-target-language "pl")))

(defun my/google-translate-at-point (&optional override-p)
  (interactive "P")

  (save-excursion
    (google-translate-at-point override-p))

  (deactivate-mark)
  (when (fboundp 'evil-exit-visual-state)
    (evil-exit-visual-state)))

(setq ispell-extra-args '("--sug-mode=ultra"))

(add-to-list 'safe-local-variable-values '(ispell-dictionary . "pl"))
(add-to-list 'safe-local-variable-values '(ispell-dictionary . "en"))

(defun my/lang-toggle ()
  "Toggle language modes."
  (interactive)
  (unless (derived-mode-p 'prog-mode)
    (let (
           (new-mode (symbol-function
                       (cond
                         ((bound-and-true-p my/pl-mode) 'my/en-mode)
                         ((bound-and-true-p my/en-mode) 'my/pl-mode)
                         ((bound-and-true-p my/language-local) (cdr (assoc my/language-local my/lang-modes)))
                         (t 'my/en-mode))
                       ))
           )
      (my/lang-modes-deactivate)
      (funcall new-mode 1))))

(add-hook 'find-file-hook #'my/lang-toggle)

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

(defhydra hydra-japanese ()
  "Japanese"
  ("k" #'japanese-katakana-region "katakana" :exit t)
  ("h" #'japanese-hiragana-region "hiragana" :exit t))

(defhydra hydra-writting ()
  "Spellcheck"
  ("s" #'flyspell-mode "flyspell toggle" :exit t)
  ("q" #'smart-quotes-mode "smart quotes toggle" :exit t)
  ("t" #'my/lang-toggle "language toggle" :exit t)
  ("c" #'langtool-check-buffer "langtool check" :exit t)
  ("d" #'langtool-check-done "langtool done" :exit t))

(add-hook 'text-mode-hook 'abbrev-mode)

(provide 'my-writing)
