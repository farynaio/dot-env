;; -*- lexical-binding: t; -*-
;; (setq debug-on-error t)

(setq inhibit-x-resources t)

(setq use-short-answers t)

(setq
  package-enable-at-startup nil
  package-install-upgrade-built-in t)

;; (setq inhibit-default-init t)
(setq load-prefer-newer t)

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

(straight-use-package 'org)
(straight-use-package '(xref :type built-in))
(straight-use-package 'use-package)

;; Ensure use-package is available via straight-use-package
(setq
  straight-use-package-by-default t
  straight-vc-git-default-protocol 'https
  straight-check-for-modifications '(check-on-save find-when-checking))
(setq use-package-always-defer t)

(add-to-list 'straight-built-in-pseudo-packages 'project)

;; Load Org and org-babel support
(require 'org)
(require 'ob-tangle)

(setq safe-local-variable-values
  '((eval add-hook 'after-save-hook
      (lambda nil
        (org-babel-tangle))
      nil t)))