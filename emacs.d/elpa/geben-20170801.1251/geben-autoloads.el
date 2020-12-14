;;; geben-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dbgp" "dbgp.el" (0 0 0 0))
;;; Generated autoloads from dbgp.el

(autoload 'dbgp-start "dbgp" "\
Start a new DBGp listener listening to PORT.

\(fn HOST PORT)" t nil)

(autoload 'dbgp-exec "dbgp" "\
Start a new DBGp listener listening to PORT.
Set the process up with SESSION-PARAMS.

\(fn HOST PORT &rest SESSION-PARAMS)" nil nil)

(autoload 'dbgp-proxy-register "dbgp" "\
Register a new DBGp listener to an external DBGp proxy.
The proxy should be found at PROXY-IP-OR-ADDR / PROXY-PORT.
This creates a new DBGp listener and register it to the proxy
associating with the IDEKEY.
MULTI-SESSION-P indicates if multiple sessions are running or not.
SESSION-PORT is either the integer port number, or t.

\(fn PROXY-IP-OR-ADDR PROXY-PORT IDEKEY MULTI-SESSION-P &optional SESSION-PORT)" t nil)

(autoload 'dbgp-proxy-register-exec "dbgp" "\
Register a new DBGp listener to an external DBGp proxy.
The proxy should be found at IP-OR-ADDR / PORT.
This create a new DBGp listener and register it to the proxy
associating with the IDEKEY.
MULTI-SESSION-P indicates if multiple sessions are running or not.
SESSION-PORT is either the integer port number, or t.
SESSION-PARAMS are added to the listener process.

\(fn IP-OR-ADDR PORT IDEKEY MULTI-SESSION-P SESSION-PORT &rest SESSION-PARAMS)" nil nil)

(autoload 'dbgp-proxy-unregister "dbgp" "\
Unregister the DBGp listener associated with IDEKEY from a DBGp proxy.
After unregistration, it kills the listener instance.
PROXY-IP-OR-ADDR is the ip or host address of the proxy instance.
PROXY-PORT is the port number.

\(fn IDEKEY &optional PROXY-IP-OR-ADDR PROXY-PORT)" t nil)

(autoload 'dbgp-proxy-unregister-exec "dbgp" "\
Unregister PROXY from a DBGp proxy.
After unregistration, it kills the listener instance.

\(fn PROXY)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dbgp" '("dbgp-")))

;;;***

;;;### (autoloads nil "geben" "geben.el" (0 0 0 0))
;;; Generated autoloads from geben.el

(autoload 'geben-mode "geben" "\
Minor mode for debugging source code with GEBEN.
The geben-mode buffer commands:
\\{geben-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'geben-add-current-line-to-predefined-breakpoints "geben" "\
Add the current line to the predefined breakpoints.

\(fn)" t nil)

(autoload 'geben "geben" "\
Start GEBEN, a DBGp protocol frontend - a script debugger.
Variations are described below.

By default, starts GEBEN listening to port `geben-dbgp-default-port'.
Prefixed with one \\[universal-argument], asks listening port number interactively and
starts GEBEN on the port.
Prefixed with two \\[universal-argument]'s, starts a GEBEN proxy listener.
Prefixed with three \\[universal-argument]'s, kills a GEBEN listener.
Prefixed with four \\[universal-argument]'s, kills a GEBEN proxy listener.

GEBEN communicates with script servers, located anywhere local or
remote, in DBGp protocol (e.g. PHP with Xdebug extension)
to help you debugging your script with some valuable features:
 - continuation commands like `step in', `step out', ...
 - a kind of breakpoints like `line no', `function call' and
   `function return'.
 - evaluation
 - stack dump
 - etc.

The script servers should be DBGp protocol enabled.
Ask to your script server administrator about this setting up
issue.

Once you've done these setup operation correctly, run GEBEN first
and your script on your script server second. After some
negotiation GEBEN will display your script's entry source code.
The debugging session is started.

In the debugging session the source code buffers are under the
minor mode  `geben-mode'. Key mapping and other information is
described its help page.

\(fn &optional ARGS)" t nil)

(defvar geben-full-frame-mode nil "\
Non-nil if Geben-Full-Frame mode is enabled.
See the `geben-full-frame-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `geben-full-frame-mode'.")

(custom-autoload 'geben-full-frame-mode "geben" nil)

(autoload 'geben-full-frame-mode "geben" "\


\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "geben" '("geben-")))

;;;***

;;;### (autoloads nil nil ("geben-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; geben-autoloads.el ends here
