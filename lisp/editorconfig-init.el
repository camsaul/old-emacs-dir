;; -*- lexical-binding: t -*-
;; -*- comment-column: 50; -*-

;;; editorconfig-init -- Setup for editorconfig
;;; Commentary:
;;; Code:

(require 'editorconfig)

(unless (string-match "^EditorConfig" (shell-command-to-string "editorconfig --version"))
  (warn "EditorConfig is not installed. This is needed by editorconfig package.")
  (when (string= system-type "darwin")
    (warn "Attempting to install via 'brew install editorconfig'...")
    (call-process-shell-command "brew install editorconfig" nil nil)))

(provide 'editorconfig-init)
;;; editorconfig-init.el ends here
