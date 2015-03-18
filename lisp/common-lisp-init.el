;; -*- lexical-binding: t -*-
;;; common-lisp-init -- setup for developing Common Lisp
;;; Commentary:
;;; Code:

(eval-after-load "slime"
  '(setq inferior-lisp-program "/usr/local/bin/sbcl"))

(defun cam/common-lisp-mode-setup ()
  "Setup for 'common-lisp-mode'."
  (require 'lisp-mode)
  (require 'lisp-init))

(add-hook 'lisp-mode-hook #'cam/common-lisp-mode-setup)

(provide 'common-lisp-init)
;;; common-lisp-init.el ends here
