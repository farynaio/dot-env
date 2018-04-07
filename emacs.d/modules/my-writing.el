(use-package langtool
  :init
  (progn
    (setq langtool-language-tool-jar (expand-file-name "languagetool-commandline.jar" my/tools-path)))
  :config
  (progn
    (setq langtool-default-language "en-GB")
    (setq langtool-mother-tongue "en")))

(use-package artbollocks-mode
  :config
  (progn
    (setq artbollocks-weasel-words-regex
      (concat "\\b" (regexp-opt
                      '("one of the"
                         "should"
                         "just"
                         "sort of"
                         "a lot"
                         "probably"
                         "maybe"
                         "perhaps"
                         "I think"
                         "really"
                         "pretty"
                         "nice"
                         "action"
                         "utilize"
                         "leverage"
                                        ; test
                         "clavicles"
                         "collarbones"
                         "tiny birds"
                         "antlers"
                         "thrumming"
                         "pulsing"
                         "wombs"
                         "ribcage"
                         "alabaster"
                         "grandmother"
                         "redacting fairytales"
                         "retelling fairytales"
                         "my sorrow"
                         "the window speaking"
                         "avocados"
                         "the blank page"
                         "marrow"
                         "starlings"
                         "giving birth"
                         "giving birth to weird shit"
                         "apples"
                         "peeling back skin"
                         "god"
                         "the mountain trembling"
                         "poetry is my remedy"
                         "sharp fragments"
                         "shards"
                         "grandpa"
                         "i can remember"
                         "this is how it happened"
                         "the pain"
                         "greek myths"
                         "poems about poems"
                         "scars"
                         "cold, stinging"
                         "oranges"
                         "the body"
                         "struggles"
                         "shadows"
                         "the moon reflecting off the"
                         "waves"
                         "echoes in the night"
                         "painted skies"
                         "a hundred"
                         "again and again"
                         "peace, love"
                         "whimsy"
                         "brooklyn"
                         "the summer solstice"
                         "the lunar eclipse"
                         "veins"
                         "soul"
                         ) t) "\\b")
      artbollocks-jargon nil)
    (add-hook 'text-mode-hook 'artbollocks-mode)))

(setq safe-local-variable-values '((ispell-dictionary . "pl")))

;; mode hooks
(setq flyspell-mode-hooks '(text-mode-hook org-mode-hook))

(if (executable-find "aspell")
  (dolist (i flyspell-mode-hooks)
    (add-hook i #'flyspell-prog-mode)))

(add-to-list 'ispell-skip-region-alist '(":PROPERTIES:" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))

;; TODO it could be rather based on ring implementation (hard to add new langs)
(defun dict-toggle ()
  "Toggle spell dictionary."
  (interactive)
  (if
    (string= ispell-current-dictionary "en")
    (ispell-change-dictionary "pl")
    (ispell-change-dictionary "en")
    )
  (message (concat "Current spell language is '" ispell-current-dictionary "'.")))

(provide 'my-writing)
