;; -*- lexical-binding: t -*-
;; -*- comment-column: 50; -*-

;;; dired-init -- Setup for dired
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'cam-functions)
  (require 'cam-macros))
(require 'dired)
(require 'dired+)
(require 'dired-x)                              ; things like C-x C-j for dired-jump

(add-hook 'dired-mode-hook
  (lambda ()
    (dired-hide-details-mode 1)))

(cam/suppress-messages
 (toggle-diredp-find-file-reuse-dir t))         ; reuse dired buffers

(setq dired-recursive-copies 'always
      dired-recursive-deletes 'always)

(defadvice dired-smart-shell-command (after refresh-dired-after-shell-command activate)
  "Revert dired buffer after executing a shell command in one."
  (revert-buffer))

(provide 'dired-init)
;;; dired-init.el ends here
