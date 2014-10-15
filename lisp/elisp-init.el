;;; elisp-init -- Setup for editing Emacs Lisp
;;; Commentary:

;; -*- comment-column: 50; -*-

(require 'cam-functions)

;;; Code:

(cam-setup-autoloads
  ("flycheck" flycheck-mode)
  ("lisp-init" cam-define-lisp-keys cam-lisp-mode-setup)
  ("lisp-mode" emacs-lisp-mode lisp-mode))


;;;; GENERAL

(defun cam/is-init-file-p (filename)
  "Return t if FILENAME is ~/.emacs.d/init.el or in ~/.emacs.d/ directory; nil in any other case."
  (condition-case nil
      (or (file-equal-p filename "~/.emacs.d/init.el")
          (and (string= (file-name-extension filename)
                        "el")
               (file-in-directory-p filename "~/.emacs.d/lisp")))
    (error nil)))

(defun byte-recompile-this-file ()
  "Recompile the current Emacs Lisp file if it is an init file."
  (interactive)
  (when (and (buffer-file-name)
             (cam/is-init-file-p (buffer-file-name)))
    (byte-recompile-file (buffer-file-name)
                         t   ; force recompile
                         0)  ; 0 = compile even if .elc does not exist
    (eval-buffer)))

(defun cam-elisp-mode-setup ()
  "Code to be ran on \"emacs-lisp-mode-hook\" and \"ielm-mode-hook\"."
  (require 'lisp-init)
  (require 'morlock)

  (cam/declare-vars cam-define-elisp-keys
                    flycheck-emacs-lisp-load-path)
  (cam-lisp-mode-setup)

  (cam-enable-minor-modes
    elisp-slime-nav-mode
    flycheck-mode)
  (turn-on-morlock-mode-if-desired)

  (setq flycheck-emacs-lisp-load-path load-path)

  (cam-define-elisp-keys emacs-lisp-mode-map)

  (add-hook 'before-save-hook 'untabify-current-buffer nil t)
  (add-hook 'after-save-hook 'byte-recompile-this-file nil t)

  ;; use byte-compile-dynamic when compiling files in .emacs.d
  (when (string= default-directory                ; default-directory is buffer-local dir of the current buffer
           (expand-file-name "~/.emacs.d/"))
    (setq-local byte-compile-dynamic t)))

(add-hook 'emacs-lisp-mode-hook 'cam-elisp-mode-setup)
(add-hook 'ielm-mode-hook 'cam-elisp-mode-setup)


;;;; SETTINGS

(put 'add-hook 'lisp-indent-function 1)           ; nicer indentation for add-hook


;;;; FUNCTIONS

(defun cam/wrapping-flycheck-next-error ()
  "Call flycheck-next-error, wrap around if we've reached end of buffer."
  (interactive)
  (condition-case nil
      (flycheck-next-error)
    (error (goto-char (point-min)))))


;;;; KEY MAPS

(defun cam-define-elisp-keys (mode-map)
  (cam-define-lisp-keys mode-map)
  (define-keys mode-map
    '(("<f5>" flycheck-display-errors)
      ("<f6>" cam/wrapping-flycheck-next-error)
      ("<f7>" flycheck-mode)
      ("C-x C-e" pp-eval-last-sexp)  ; pretty-print eval'd expressions
      ("<s-mouse-1>" elisp-slime-nav-find-elisp-thing-at-point))))

(add-hook 'emacs-lisp-mode-hook (lambda () ))


;;;; IELM SPECIFIC

(add-hook 'ielm-mode-hook
  (lambda ()
    (cam/declare-vars ielm-map)
    (cam-define-elisp-keys ielm-map)
    (define-keys ielm-map
      '(("RET" ielm-return)))))

(provide 'elisp-init)
;;; elisp-init ends here
