;; -*- lexical-binding: t -*-
;; -*- comment-column: 50; -*-

;;; magit-init -- Setup for magit
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'cam-functions)
  (require 'cam-macros))
(require 'magit)

(cam/enable-minor-modes
  (magit-auto-revert-mode . nil))            ; auto-revert buffers that change on disk as result of magit command

(defadvice magit-status (after magit-status-show-help activate)
  (magit-key-mode-popup-dispatch)            ; show help when showing magit-status
  (call-interactively #'other-window)        ; switch back to magit status window
  (add-hook 'kill-buffer-hook                ; Kill all of the other magit buffers like help + *magit-process*
    #'cam/kill-magit-buffers
    nil :local))

(cam/run-fullscreen "magit" magit-status)

(cam/define-keys magit-status-mode-map
  "V" #'cam/visit-pull-request-url
  "s-u" #'magit-refresh)

(defun cam/kill-magit-buffers ()
  "Kill all magit buffers."
  (->> (buffer-list)
       (mapcar #'buffer-name)
       (-filter (-partial #'s-starts-with-p "*magit"))
       (-filter (lambda (b)
                  (not (string= b
                                (buffer-name (current-buffer))))))
       (mapcar #'kill-buffer)))

;;; ## `V` when using magit will open corresponding PR on GitHub <3
;;; Inspired by http://endlessparentheses.com/easily-create-github-prs-from-magit.html
(defun cam/visit-pull-request-url ()
  "Visit the current git branch's PR on GitHub."
  (interactive)
  (let ((current-branch (magit-get-current-branch))
        (repo-url (->> (magit-get "remote" (magit-get-current-remote) "url")
                       (string-remove-suffix ".git" ))))
    (browse-url (concat repo-url "/pull/" current-branch))))


(provide 'magit-init)
;;; magit-init.el ends here
