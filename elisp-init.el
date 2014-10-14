;; -*- comment-column: 50; -*-

(cam-setup-autoloads
  ("lisp-init" cam-define-lisp-keys cam-lisp-mode-setup)
  ("lisp-mode" emacs-lisp-mode lisp-mode))

;;;; .EL FILES / GENERAL

(defun byte-recompile-this-file ()
  "Recompile the current Emacs Lisp file."
  (interactive)
  (byte-recompile-file (buffer-file-name)
                       t                          ; force recompile
                       0)                         ; 0 = compile even if .elc does not exist
  (eval-buffer))

(defun cam-elisp-mode-setup ()
  (cam-lisp-mode-setup)
  (cam-enable-minor-modes
    (auto-complete-mode . nil)
    elisp-slime-nav-mode)
  (setq-local completion-at-point-functions '(auto-complete)) ; make autocomplete a completion-at-point function
  (add-hook 'before-save-hook 'untabify-current-buffer nil t)
  (add-hook 'after-save-hook 'byte-recompile-this-file nil t)

  ;; use byte-compile-dynamic when compiling files in .emacs.d
  (when (string= default-directory                ; default-directory is buffer-local dir of the current buffer
           (expand-file-name "~/.emacs.d/"))
    (setq-local byte-compile-dynamic t)))

(add-hook 'emacs-lisp-mode-hook 'cam-elisp-mode-setup)
(add-hook 'ielm-mode-hook 'cam-elisp-mode-setup)


;;;; KEY MAPS

(defun cam-define-elisp-keys (mode-map)
  (cam-define-lisp-keys mode-map)
  (define-keys mode-map
    '(("C-x C-e" pp-eval-last-sexp)  ; pretty-print eval'd expressions
      ("<s-mouse-1>" elisp-slime-nav-find-elisp-thing-at-point))))

(eval-after-load "emacs-lisp-mode"
  '(progn
     (cam-define-elisp-keys emacs-lisp-mode-map)
     (eval-after-load "auto-complete"
       (add-to-list 'ac-modes 'emacs-lisp-mode))))


;;;; IELM SPECIFIC

(eval-after-load "ielm"
  '(progn
     (cam-define-elisp-keys ielm-map)
     (define-keys ielm-map
       '(("RET" ielm-return)))))

(provide 'elisp-init)
