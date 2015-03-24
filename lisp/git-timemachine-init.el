;; -*- lexical-binding: t; comment-column: 50; -*-

;;; git-timemachine-init -- Setup for git-timemachine
;;; Commentary:
;;; Code:

(require 'git-timemachine)

(defadvice git-timemachine (around git-timemachine-split-fullscreen activate)
  "Run git-timemachine for the current buffer in a special temporary fullscreen session"
  (window-configuration-to-register :git-timemachine-fullscreen-window-config)
  (let ((parent-buffer (current-buffer)))
    (delete-other-windows)
    (select-window (split-window-right) :norecord) ; select the newly created window to the right
    ad-do-it
    (while (eq (current-buffer) parent-buffer) ; double-check we're not showing parent buffer
      (other-window 1)))
  (add-hook 'kill-buffer-hook (lambda ()
                                (jump-to-register :git-timemachine-fullscreen-window-config :delete))
            :append :local))

(defadvice git-timemachine-quit (after git-timemachine-fullscreen-quit activate)
  (jump-to-register :git-timemachine-fullscreen-window-config))

(provide 'git-timemachine-init)
;;; git-timemachine-int.eit ends here
