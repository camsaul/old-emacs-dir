;; -*- lexical-binding: t -*-
;; -*- comment-column: 50; -*-

;;; Code:

(defun cam/lisp-mode-setup ()
  (mapc 'require '(highlight-parentheses
                   hl-sexp))
  (cam/enable-minor-modes
    (eldoc-mode . nil)
    (highlight-parentheses-mode . nil)            ; highlight parentheses that surround the current sexpr
    hl-sexp-mode                                  ; hl-sexp-mode highlights the current sexp
    (paredit-mode . " π"))
  (pretty-lambdas)
  (set-face-background 'hl-sexp-face "#332222")
  (add-hook 'before-save-hook 'cam/untabify-current-buffer t t))

(defun cam/backward-paredit-kill ()
  "calls paredit-kill with prefix arg 0 which effectively makes it kill backwards."
  (interactive)
  (paredit-kill 0))

(eval-after-load 'paredit
  '(progn
     (mapc (-lambda ((cmd val))
             (put cmd 'delete-selection val))
           '((paredit-forward-delete supersede)
             (paredit-backward-delete superscede)
             (paredit-open-round t)
             (paredit-open-square t)
             (paredit-doublequote t)
             (paredit-newline t)))))

(defun cam/define-lisp-keys (mode-map)
  (cam/define-keys mode-map
    "C-c C-d" 'elisp-slime-nav-describe-elisp-thing-at-point
    "RET" 'reindent-then-newline-and-indent
    "<f11>" 'paredit-mode
    "C-S-k" 'cam/backward-paredit-kill))

;; pretty-lambdas turns the word Lambda (lowercase) into a lambda. Credit: emacs-starter-kit on github
(defun pretty-lambdas ()
  ;; (font-lock-add-keywords
  ;;  nil `(("(?\\(lambda\\>\\)"
  ;;         (0 (progn (compose-region (match-beginning 1) (match-end 1)
  ;;                                   ,(make-char 'greek-iso8859-7 107))
  ;;                   nil)))))
  )

(provide 'lisp-init)
