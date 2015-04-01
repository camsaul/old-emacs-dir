;; -*- lexical-binding: t; comment-column: 50; -*-

;;; Commentary:
;;; Just put at the end of .bash_profile:
;;; emacsclient -nw --eval '(eshell)' || emacs -nw --eval '(progn (server-start) (eshell))'
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

  ;; Aliases
  ;; I would imagine there's some better way to do this
  (eshell-command "alias clear cam/eshell-clear"))

(add-hook 'eshell-mode-hook #'cam/eshell-setup)

(setq eshell-ls-initial-args '("-a")              ; list of args to pass to ls (default = nil)
      eshell-prefer-lisp-functions t              ; prefer built-in eshell commands to external ones
      )

(provide 'eshell-init)
;;; eshell-init.el ends here
