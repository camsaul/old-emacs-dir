(require 'highlight-parentheses)
(require 'auto-complete)
(require 'hl-sexp)

(defun cam-lisp-mode-setup ()
  (global-mode-setup)
  (highlight-parentheses-mode 1) ; highlight parentheses that surround the current sexpr
  (diminish 'highlight-parentheses-mode)
  (paredit-mode 1)
  (diminish 'paredit-mode " π")
  (auto-complete-mode 1)
  (diminish 'auto-complete-mode)
  (turn-on-eldoc-mode)
  (diminish 'eldoc-mode)
  (pretty-lambdas)
  (set-buffer-file-coding-system 'utf-8-auto-unix)
  (hl-sexp-mode 1) ; hl-sexp-mode highlights the current sexp
  (set-face-background 'hl-sexp-face "#EFFFFF"))

(defun backward-paredit-kill ()
  "calls paredit-kill with prefix arg 0 which effectively makes it kill backwards."
  (interactive)
  (paredit-kill 0))

(defun cam-define-lisp-keys (mode-map)
  (define-key mode-map (kbd "RET") 'reindent-then-newline-and-indent)
  (define-key mode-map (kbd "<f11>") 'paredit-mode)
  (define-key mode-map (kbd "C-S-k") 'backward-paredit-kill) 
  (define-key mode-map (kbd "TAB") 'lisp-complete-symbol)) ; tab to complete symbol)

;; pretty-lambdas turns the word Lambda (lowercase) into a lambda. Credit: emacs-starter-kit on github
(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(provide 'lisp-init)
