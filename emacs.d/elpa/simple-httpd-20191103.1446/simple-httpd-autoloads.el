;;; simple-httpd-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "simple-httpd" "../../../../.emacs.d/elpa/simple-httpd-20191103.1446/simple-httpd.el"
;;;;;;  "24c4c64dc773d4ce492d11bf6da9e15a")
;;; Generated autoloads from ../../../../.emacs.d/elpa/simple-httpd-20191103.1446/simple-httpd.el

(autoload 'httpd-start "simple-httpd" "\
Start the web server process. If the server is already
running, this will restart the server. There is only one server
instance per Emacs instance." t nil)

(autoload 'httpd-stop "simple-httpd" "\
Stop the web server if it is currently running, otherwise do nothing." t nil)

(autoload 'httpd-running-p "simple-httpd" "\
Return non-nil if the simple-httpd server is running." nil nil)

(autoload 'httpd-serve-directory "simple-httpd" "\
Start the web server with given `directory' as `httpd-root'.

\(fn DIRECTORY)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "simple-httpd"
;;;;;;  "../../../../.emacs.d/elpa/simple-httpd-20191103.1446/simple-httpd.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/simple-httpd-20191103.1446/simple-httpd.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "simple-httpd" '("defservlet" "httpd" "with-httpd-buffer")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/simple-httpd-20191103.1446/simple-httpd-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/simple-httpd-20191103.1446/simple-httpd.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; simple-httpd-autoloads.el ends here
