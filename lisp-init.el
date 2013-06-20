(provide 'lisp-init)
(require 'highlight-parentheses)
(require 'auto-complete)

(defun cam-lisp-mode-setup ()
  (highlight-parentheses-mode 1) ; highlight parentheses that surround the current sexpr
  (diminish 'highlight-parentheses-mode)
  (paredit-mode 1)
  (diminish 'paredit-mode " π")
  (auto-complete-mode 1)
  (diminish 'auto-complete-mode)
  (turn-on-eldoc-mode)
  (diminish 'eldoc-mode)
  (pretty-lambdas)
  (set-buffer-file-coding-system 'utf-8-auto-unix))

(defun cam-define-lisp-keys (mode-map)
  (define-key mode-map (kbd "RET") 'reindent-then-newline-and-indent)
  (define-key mode-map (kbd "<f11>") 'paredit-mode)
  (define-key mode-map (kbd "TAB") 'lisp-complete-symbol)) ; tab to complete symbol)

;; pretty-lambdas (lol) turns the word Lambda (lowercase) into a lambda. Credit: emacs-starter-kit on github
(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

;; pretty-fn turns fn's to fancy f symbols. From 
