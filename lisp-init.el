;; -*- comment-column: 50; -*-

(defun cam-lisp-mode-setup ()
  (mapc 'require '(highlight-parentheses
                   hl-sexp))
  (cam-enable-minor-modes
    (company-mode . " ¢")
    (highlight-parentheses-mode . nil)            ; highlight parentheses that surround the current sexpr
    hl-sexp-mode                                  ; hl-sexp-mode highlights the current sexp
    (paredit-mode . " π"))
  (turn-on-eldoc-mode)
  (diminish 'eldoc-mode)
  (pretty-lambdas)
  (set-face-background 'hl-sexp-face "#DDFFDD")
  (add-hook 'before-save-hook '(lambda ()
                                 (set-buffer-file-coding-system 'utf-8-auto-unix)
                                 (untabify-current-buffer))
            nil t))

(defun backward-paredit-kill ()
  "calls paredit-kill with prefix arg 0 which effectively makes it kill backwards."
  (interactive)
  (paredit-kill 0))

(defun cam-define-lisp-keys (mode-map)
  (define-keys mode-map
    '(("RET" reindent-then-newline-and-indent)
      ("<f11>" paredit-mode)
      ("C-S-k" backward-paredit-kill))))

;; pretty-lambdas turns the word Lambda (lowercase) into a lambda. Credit: emacs-starter-kit on github
(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(provide 'lisp-init)
