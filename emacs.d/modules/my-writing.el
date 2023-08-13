;; (require 'text-mode)
(require 'smart-quotes)
(require 'cl)

;; (if (executable-find "hunspell")
;;     (progn
;;       (setenv "DICPATH" (concat (getenv "HOME") "/Documents/Dropbox/devel/spelling"))
;;       (setq ispell-program-name (executable-find "hunspell")))
;;   (message "'hunspell' not installed!"))


(setq skk-large-jisyo "~/.emacs.d/dict/SKK-JISYO.L") ;; is this needed?

(define-minor-mode my/en-mode
  "Language mode for 'en'."
  :init-value nil
  :lighter " en"
  (setq-local
    ispell-local-dictionary "en"
    local-abbrev-table my/en-abbrevs
    langtool-default-language "en"))

(define-minor-mode my/pl-mode
  "Language mode for 'pl'."
  :init-value nil
  :lighter " pl"
  (setq-local
    ispell-local-dictionary "pl"
    local-abbrev-table my/pl-abbrevs
    langtool-default-language "pl"))

(use-package abbrev
  :straight nil
  :ensure nil
  :diminish "Abb"
  :hook ((text-mode . abbrev-mode))
  :custom
  (save-abbrevs 'silently)
  (abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory)))

(add-hook 'org-mode-hook 'my/en-mode)
(add-hook 'org-roam-dailies-find-file-hook 'my/en-mode)

(use-package ispell
  :defer 2
  :custom
  (ispell-local-dictionary "en_US")
  (ispell-local-dictionary-alist
    '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
       ("fr_FR" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "fr_FR") nil utf-8)))
  (ispell-dictionary "en_US")
  (ispell-dictionary-alist
    '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
       ("fr_FR" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "fr_FR") nil utf-8)))
  ;; (ispell-program-name (executable-find "hunspell"))
  (ispell-program-name (executable-find "aspell"))
  (ispell-really-hunspell t)
  (ispell-silently-savep t)
  ;; (ispell-extra-args '("--sug-mode=ultra") ;; for hunspell only
  :preface
  (setq my/lang-modes '((en . my/en-mode) (pl . my/pl-mode)))
  (defun my/lang-modes-deactivate ()
    "Deactivate all lang modes."
    (interactive)
    (my/en-mode -1)
    (my/pl-mode -1))

  (defun my/lang-toggle ()
    "Toggle language modes."
    (interactive)
    (unless (derived-mode-p 'prog-mode)
      (let ((new-mode (symbol-function
                        (cond
                          ((bound-and-true-p my/pl-mode) 'my/en-mode)
                          ((bound-and-true-p my/en-mode) 'my/pl-mode)
                          ((bound-and-true-p my/language-local) (cdr (assoc my/language-local my/lang-modes)))
                          (t 'my/en-mode))
                        )))
        (my/lang-modes-deactivate)
        (funcall new-mode 1)))))

(use-package flyspell
  :commands flyspell-mode
  :diminish "Fly"
  :custom
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil)
  :config
  (flyspell-prog-mode)
  (evil-define-key 'normal flyspell-mode-map
    ;; (kbd "[s") 'flyspell-goto-next-error
    ;; (kbd "]s") 'flyspell-goto-next-error
    (kbd "[l") #'langtool-goto-previous-error
    (kbd "]l") #'langtool-goto-next-error)

  (add-to-list 'ispell-skip-region-alist '(":PROPERTIES:" ":END:"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE")))

(defvar my/en-abbrevs nil)
(define-abbrev-table
  'my/en-abbrevs '(
                    ("aint" "ain't" nil 0)
                    ("ami" "am I" nil 0)
                    ("arent" "aren't" nil 0)
                    ("cant" "can't" nil 0)
                    ("chinese" "Chinese" nil 0)
                    ("couldnt" "couldn't" nil 0)
                    ("didnt" "didn't" nil 0)
                    ("didt" "didn't" nil 0)
                    ("doesnt" "doesn't" nil 0)
                    ("dont" "don't" nil 0)
                    ("elses" "else's" nil 0)
                    ("exceause" "excuse" nil 0)
                    ("exceauses" "excuses" nil 0)
                    ("everobodys" "everybody's" nil 0)
                    ("hasnt" "hasn't" nil 0)
                    ("hast" "hasn't" nil 0)
                    ("ive" "I've" nil 0)
                    ("ihave" "I've" nil 0)
                    ("im" "I'm" nil 0)
                    ("inteligence" "intelligence" nil 0)
                    ("isnt" "isn't" nil 0)
                    ("Iwould" "I'd" nil 0)
                    ("Iwoudl" "I'd" nil 0)
                    ("ill" "I'll" nil 0)
                    ("interuption" "interruption" nil 0)
                    ("interuptions" "interruptions" nil 0)
                    ("iwill" "I'll" nil 0)
                    ("havent" "haven't" nil 0)
                    ("hows" "how's" nil 0)
                    ("youre" "you're" nil 0)
                    ("youare" "you're" nil 0)
                    ("youd" "you'd" nil 0)
                    ("youve" "you've" nil 0)
                    ("youll" "you'll" nil 0)
                    ("youwll" "you'll" nil 0)
                    ("FF" "FireFox" nil 0)
                    ("ff" "FireFox" nil 0)
                    ("YT" "YouTube" nil 0)
                    ("FB" "Facebook" nil 0)
                    ("fb" "Facebook" nil 0)
                    ("IG" "Instagram" nil 0)
                    ("ig" "Instagram" nil 0)
                    ("facebook" "Facebook" nil 0)
                    ("telegram" "Telegram" nil 0)
                    ("youtube" "YouTube" nil 0)
                    ("BT" "Bluetooth" nil 0)
                    ("macos" "MacOS" nil 0)
                    ("mustnt" "mustn't" nil 0)
                    ("seo" "SEO" nil 0)
                    ("irc" "IRC" nil 0)
                    ("rss" "RSS" nil 0)
                    ("url" "URL" nil 0)
                    ("crm" "CRM" nil 0)
                    ("erp" "ERP" nil 0)
                    ("b2b" "B2B" nil 0)
                    ("b2c" "B2C" nil 0)
                    ("btc" "BTC" nil 0)
                    ("cms" "CMS" nil 0)
                    ("prefered" "preferred" nil 0)
                    ("sim" "SIM" nil 0)
                    ("voip" "VoIP" nil 0)
                    ("ppl" "people" nil 0)
                    ("Ppl" "People" nil 0)
                    ("linux" "Linux" nil 0)
                    ("ascii" "ASCII" nil 0)
                    ("twitch" "Twitch" nil 0)
                    ("airbnb" "AirBnB" nil 0)
                    ("shouldnt" "shouldn't" nil 0)
                    ("shouldt" "shouldn't" nil 0)
                    ("shouldve" "should've" nil 0)
                    ("json" "JSON" nil 0)
                    ("shes" "she's" nil 0)
                    ("shewill" "she'll" nil 0)
                    ("sheill" "she'll" nil 0)
                    ("wasnt" "wasn't" nil 0)
                    ("weare" "we're" nil 0)
                    ("wewill" "we'll" nil 0)
                    ("weill" "we'll" nil 0)
                    ("whatdo" "what'd" nil 0)
                    ("whats" "what's" nil 0)
                    ("whos" "who's" nil 0)
                    ("wont" "won't" nil 0)
                    ("asap" "ASAP" nil 0)
                    ("emacs" "Emacs" nil 0)
                    ("wouldnt" "wouldn't" nil 0)
                    ("wouldve" "would've" nil 0)
                    ("wouldhave" "would've" nil 0)
                    ("whatsup" "WhatsApp" nil 0)
                    ("whatsapp" "WhatsApp" nil 0)
                    ("whatapp" "WhatsApp" nil 0)
                    ("thats" "that's" nil 0)
                    ("theres" "there's" nil 0)
                    ("therell" "there'll" nil 0)
                    ("therell" "there'll" nil 0)
                    ("theyre" "they're" nil 0)
                    ("totaly" "totally" nil 0)
                    ("email" "e-mail" nil 0)
                    ("emails" "e-mails" nil 0)
                    ) nil :case-fixed nil)

(defvar my/pl-abbrevs nil)
(define-abbrev-table 'my/pl-abbrevs '())

;; TODO is this working?
(use-package wiki-summary
  :defer 1
  :config
  (defun my/format-summary-in-buffer (summary)
    "Given a summary, sticks it in the *wiki-summary* buffer and displays
     the buffer."
    (let ((buf (generate-new-buffer "*wiki-summary*")))
      (with-current-buffer buf
        (princ summary buf)
        (fill-paragraph)
        (goto-char (point-min))
        (view-mode))
      (pop-to-buffer buf)))

  (advice-add 'wiki-summary/format-summary-in-buffer :override #'my/format-summary-in-buffer))

(use-package langtool
  :commands (langtool-check-buffer langtool-check-done)
  :if (executable-find (expand-file-name "LanguageTool/languagetool-commandline.jar" my/tools-path))
  :custom
  (langtool-language-tool-jar (expand-file-name "LanguageTool/languagetool-commandline.jar" my/tools-path))
  (langtool-default-language "en")
  (langtool-mother-tongue "en")
  (langtool-disabled-rules
    '("COMMA_PARENTHESIS_WHITESPACE"
       "COPYRIGHT"
       "DASH_RULE"
       "EN_QUOTES"
       "EN_UNPAIRED_BRACKETS"
       "UPPERCASE_SENTENCE_START"
       "WHITESPACE_RULE")))

;; (use-package google-translate
;;   :custom
;;   (google-translate-default-source-language "en")
;;   (google-translate-default-target-language "pl")
;;   (google-translate-backend-method 'curl)
;;   :config
;;   (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))

;;   (defun my/google-translate-at-point (&optional override-p)
;;     (interactive "P")
;;     (save-excursion
;;       (google-translate-at-point override-p))
;;     (deactivate-mark)
;;     (when (fboundp 'evil-exit-visual-state)
;;       (evil-exit-visual-state))))

(use-package artbollocks-mode
  :commands artbollocks-mode
  :custom
  (artbollocks-weasel-words-regex
    (concat "\\b" (regexp-opt
                    '("one of the" "should" "just" "sort of" "a lot" "probably" "maybe" "perhaps" "I think" "really" "pretty" "nice" "action" "utilize" "leverage"
                                        ; test
                       "clavicles" "collarbones" "tiny birds" "antlers" "thrumming" "pulsing" "wombs" "ribcage" "alabaster" "grandmother" "redacting fairytales" "retelling fairytales" "my sorrow" "the window speaking" "avocados" "the blank page" "marrow" "starlings" "giving birth" "giving birth to weird shit" "apples" "peeling back skin" "god" "the mountain trembling" "poetry is my remedy" "sharp fragments" "shards" "grandpa" "i can remember" "this is how it happened" "the pain" "greek myths" "poems about poems" "scars" "cold, stinging" "oranges" "the body" "struggles" "shadows" "the moon reflecting off the" "waves" "echoes in the night" "painted skies" "a hundred" "again and again" "peace, love" "whimsy" "brooklyn" "the summer solstice" "the lunar eclipse" "veins" "soul"
                       ) t) "\\b")
    artbollocks-jargon nil))

(use-package auctex
  :disabled t
  :hook (LaTeX-mode . turn-on-reftex)
  :defer 0.3)

(provide 'my-writing)
