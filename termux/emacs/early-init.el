(setq
  package-enable-at-startup nil
  package-install-upgrade-built-in t)

;; init.el — minimal bootstrap
(setq gc-cons-threshold (* 50 1000 1000)) ;; reduce startup GC pauses

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Ensure use-package is available via straight
(straight-use-package 'use-package)
(setq
  straight-use-package-by-default t
  straight-vc-git-default-protocol 'https
  straight-check-for-modifications '(check-on-save find-when-checking))
(setq use-package-always-defer t)

;; Load Org and org-babel support
(straight-use-package 'org)
(require 'org)
(require 'ob-tangle)

;; Tangle & load init.org if it is newer than the tangled file
(let ((org-file (expand-file-name "init.org" user-emacs-directory)))
  (when (file-exists-p org-file)
    ;; org-babel-load-file tangles and loads the tangled elisp
    (org-babel-load-file org-file)))

(add-hook 'org-mode-hook
          (lambda ()
            (when (string-equal (buffer-file-name) (expand-file-name "init.org" user-emacs-directory))
              (add-hook 'after-save-hook #'org-babel-tangle nil t))))

 
;; Automatically tangle our Emacs.org config file when we save it
;; (defun efs/org-babel-tangle-config ()
;;   (when (string-equal (buffer-file-name)
;;                       (expand-file-name "init.org" user-emacs-directory))
;;     ;; Dynamic scoping to the rescue
;;     (let ((org-confirm-babel-evaluate nil))
;;       (org-babel-tangle))))

;; (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))
 
