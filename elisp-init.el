(provide 'elisp-init)
(require 'ielm)

(defun cam-elisp-mode-setup ()
  (cam-lisp-mode-setup)
  ; nothing right now
  )

(add-hook 'emacs-lisp-mode-hook 'cam-elisp-mode-setup)
(add-hook 'ielm-mode-hook 'cam-elisp-mode-setup)

(defun cam-define-elisp-keys (mode-map)
  (cam-define-lisp-keys mode-map)
  (define-keys mode-map
    '(("C-x C-e" pp-eval-last-sexp) ; pretty-print eval'd expressions
      )))

(cam-define-elisp-keys emacs-lisp-mode-map)
(cam-define-elisp-keys ielm-map)
(define-keys ielm-map
  '(("RET" ielm-return)))


