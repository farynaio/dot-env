;;; wiki-summary-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "wiki-summary" "../../../../.emacs.d/elpa/wiki-summary-20181010.1824/wiki-summary.el"
;;;;;;  "68234feee9f736e534b1dce48a8551ba")
;;; Generated autoloads from ../../../../.emacs.d/elpa/wiki-summary-20181010.1824/wiki-summary.el

(autoload 'wiki-summary/make-api-query "wiki-summary" "\
Given a wiki page title, generate the url for the API call
   to get the page info

\(fn S)" nil nil)

(autoload 'wiki-summary/extract-summary "wiki-summary" "\
Given the JSON reponse from the webpage, grab the summary as a string

\(fn RESP)" nil nil)

(autoload 'wiki-summary/format-summary-in-buffer "wiki-summary" "\
Given a summary, stick it in the *wiki-summary* buffer and display the buffer

\(fn SUMMARY)" nil nil)

(autoload 'wiki-summary/format-summary-into-buffer "wiki-summary" "\
Given a summary, stick it in the *wiki-summary* buffer and display the buffer

\(fn SUMMARY BUFFER)" nil nil)

(autoload 'wiki-summary "wiki-summary" "\
Return the wikipedia page's summary for a term

\(fn S)" t nil)

(autoload 'wiki-summary-insert "wiki-summary" "\
Return the wikipedia page's summary for a term

\(fn S)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "wiki-summary"
;;;;;;  "../../../../.emacs.d/elpa/wiki-summary-20181010.1824/wiki-summary.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/wiki-summary-20181010.1824/wiki-summary.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "wiki-summary" '("wiki-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/wiki-summary-20181010.1824/wiki-summary-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/wiki-summary-20181010.1824/wiki-summary.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; wiki-summary-autoloads.el ends here
