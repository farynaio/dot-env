(defun my/func-call (&rest func)
  (interactive)
  (dolist (i func)
    (if (listp i)
      (apply (car i) (cdr i))
      (funcall i))))

;; https://emacs.stackexchange.com/a/18515/18445
(defun my/online-p (&optional host)
    (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1"
                       (if host host "gnu.org"))))

;; https://stackoverflow.com/a/6255409/346921
(defun my/reformat-xml ()
  "Reformats xml to make it readable (respects current selection)."
  (interactive)
  (save-excursion
    (let ((beg (point-min))
           (end (point-max)))
      (if (and mark-active transient-mark-mode)
        (progn
          (setq beg (min (point) (mark)))
          (setq end (max (point) (mark))))
        (widen))
      (setq end (copy-marker end t))
      (goto-char beg)
      (while (re-search-forward ">\\s-*<" end t)
        (replace-match ">\n<" t t))
      (goto-char beg)
      (indent-region beg end nil))))

;; https://emacs.stackexchange.com/questions/5441/function-to-delete-all-comments-from-a-buffer-without-moving-them-to-kill-ring
(defun my/comment-delete (arg)
  "Delete the first comment on this line, if any.  Don't touch
the kill ring.  With prefix ARG, delete comments on that many
lines starting with this one."
  ;; (interactive "P")
  (comment-normalize-vars)
  (dotimes (_i (prefix-numeric-value arg))
    (save-excursion
      (beginning-of-line)
      (let ((cs (comment-search-forward (line-end-position) t)))
        (when cs
          (goto-char cs)
          (skip-syntax-backward " ")
          (setq cs (point))
          (comment-forward)
          ;; (kill-region cs (if (bolp) (1- (point)) (point))) ; original
          (delete-region cs (if (bolp) (1- (point)) (point)))  ; replace kill-region with delete-region
          (indent-according-to-mode))))
    (if arg (forward-line 1))))

;; https://emacs.stackexchange.com/questions/5441/function-to-delete-all-comments-from-a-buffer-without-moving-them-to-kill-ring
(defun my/comment-delete-dwim (beg end arg)
  "Delete comments without touching the kill ring.  With active
region, delete comments in region.  With prefix, delete comments
in whole buffer.  With neither, delete comments on current line."
  (interactive "r\nP")
  (let ((lines (cond (arg
                       (count-lines (point-min) (point-max)))
                 ((region-active-p)
                   (count-lines beg end)))))
    (save-excursion
      (when lines
        (goto-char (if arg (point-min) beg)))
      (my/comment-delete (or lines 1)))))

(defun my/comments-delete-buffer ()
  "Remove all comments from the buffer."
  (interactive)
  (my/comment-delete-dwim (point-min) (point-max) 1)
  (my/remove-empty-lines))

;; http://ergoemacs.org/emacs/elisp_compact_empty_lines.html
(defun my/remove-empty-lines ()
  (interactive)
  (let ($begin $end)
    (if (region-active-p)
      (setq $begin (region-beginning) $end (region-end))
      (setq $begin (point-min) $end (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region $begin $end)
        (while (re-search-forward "[ \t]+\n" nil "move")
          (replace-match "\n"))
        (progn
          (goto-char (point-min))
          (while (re-search-forward "\n\n+" nil "move")
            (replace-match "\n")))))))

(defun my/wp-gutenberg-to-md ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward "<!-- /?wp:\\(heading\\|image\\|paragraph\\|list\\|code\\|preformatted\\).*?-->\n?" nil t)
      (replace-match "" nil nil))

    (beginning-of-buffer)
    (while (re-search-forward "<!-- /?wp:\\(qubely\\|html\\).*?-->\n?" nil t)
      (replace-match "" nil nil))

    (beginning-of-buffer)
    (while (re-search-forward "</?p>" nil t)
      (replace-match "" nil nil))

    (beginning-of-buffer)
    (while (re-search-forward "</h[1-6]>" nil t)
      (replace-match "" nil nil))

    (beginning-of-buffer)
    (while (re-search-forward "<h2>" nil t)
      (replace-match "## " nil nil))

    (beginning-of-buffer)
    (while (re-search-forward "<h3>" nil t)
      (replace-match "### " nil nil))

    (beginning-of-buffer)
    (while (re-search-forward "</?code>" nil t)
      (replace-match "`" nil nil))))

;; (defun my/evil-ex-nohighlight-frame ()
;;   ""
;;   (interactive)
;;   (dolist (x (window-list (selected-frame)))
;;     (with-selected-window x
;;       (evil-ex-nohighlight)
;;       )))

(provide 'my-utils)