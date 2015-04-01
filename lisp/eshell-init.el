;; -*- lexical-binding: t; comment-column: 50; -*-

;;; Commentary:
;;; 1.  put at the end of .bash_profile:
;;;     emacsclient -nw --create-frame --eval "(ignore-errors (eshell :create-new-session))" || emacs -nw --eval '(progn (server-start) (eshell))'
;;;
;;; 2.  (OS X) (Terminal -> Settings -> Shell) Add 'emacsclient' to list of processes not to prompt before closing
;;;
;;; <3

;;; Code:

(require 'eshell)
(require 'em-ls)
(require 'cam-functions)
(require 'cam-macros)

(defun cam/eshell-clear ()
  "Clear previous text in eshell."
  (interactive)
  (comint-kill-region (point-min) (point-max))
  (eshell-send-input))

(defun cam/eshell-setup ()
  (cam/define-keys eshell-mode-map
    "C-c M-o" #'cam/eshell-clear)

  (linum-mode -1)

  ;; Aliases
  ;; I would imagine there's some better way to do this
  (eshell-command "alias clear cam/eshell-clear"))

(add-hook 'eshell-mode-hook #'cam/eshell-setup)

(setq eshell-ls-initial-args '("-a")              ; list of args to pass to ls (default = nil)
      eshell-prefer-lisp-functions t              ; prefer built-in eshell commands to external ones
      )

(provide 'eshell-init)
;;; eshell-init.el ends here
