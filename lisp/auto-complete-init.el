;; -*- lexical-binding: t -*-
;; -*- comment-column: 50; -*-

;;; auto-complete-init -- Setup for auto-complete-mode
;;; Commentary:
;;; Code:

(require 'auto-complete)

(setq ac-delay 0.05                               ; delay before trying to auto-complete
      ac-auto-show-menu 0.1                       ; delay before showing completions list
      ac-quick-help-delay 0.2)                    ; delay before poping up docstr

(ac-config-default)

(add-to-list 'ac-modes 'emacs-lisp-mode)

(provide 'auto-complete-init)
;;; auto-complete-init.el ends here
