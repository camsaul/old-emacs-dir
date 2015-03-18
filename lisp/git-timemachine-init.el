;; -*- lexical-binding: t -*-
;; -*- comment-column: 50; -*-

;;; git-timemachine-init -- Setup for git-timemachine
;;; Commentary:
;;; Code:

(require 'git-timemachine)

(defadvice git-timemachine (around git-timemachine-split-fullscreen activate)
  "Run git-timemachine for the current buffer in a special temporary fullscreen session"
  (window-configuration-to-register :git-timemachine-fullscreen-window-config)
  (delete-other-windows)
  (split-window-right)
  (call-interactively #'other-window)
  ad-do-it
  (defadvice git-timemachine-quit (after git-timemachine-fullscreen-quit activate)
    (jump-to-register :git-timemachine-fullscreen-window-config)
    (advice-remove #'git-timemachine-quit #'git-timemachine-fullscreen-quit)))

(provide 'git-timemachine-init)
;;; git-timemachine-int.eit ends here
