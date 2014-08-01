(require 'ielm)
(require 'lisp-init)
(require 'elisp-slime-nav)

(defun byte-recompile-this-file ()
  "Recompile the current Emacs Lisp file."
  (interactive)
  (byte-recompile-file (buffer-file-name) 0))


(defun cam-elisp-mode-setup ()
  (cam-lisp-mode-setup)
  (elisp-slime-nav-mode t)
  (setq completion-at-point-functions '(auto-complete)) ; make autocomplete a completion-at-point function
  (add-hook 'after-save-hook 'byte-recompile-this-file nil t))

(add-hook 'emacs-lisp-mode-hook 'cam-elisp-mode-setup)
(add-hook 'ielm-mode-hook 'cam-elisp-mode-setup)

(defun cam-define-elisp-keys (mode-map)
  (cam-define-lisp-keys mode-map)
  (define-keys mode-map
    '(("C-x C-e" pp-eval-last-sexp)  ; pretty-print eval'd expressions
      ("<s-mouse-1>" elisp-slime-nav-find-elisp-thing-at-point)
      )))

(cam-define-elisp-keys emacs-lisp-mode-map)
(cam-define-elisp-keys ielm-map)
(define-keys ielm-map
  '(("RET" ielm-return)))

(add-to-list 'ac-modes 'elisp-mode)

(provide 'elisp-init)
