;; -*- lexical-binding: t -*-

;;; sandbox --- features that are "under consideration"
;;; Commentary:
;;; Code:


;; ## DONT-PROMPT-ABOUT-KILLING
(defvar cam/dont-prompt-about-killing-processes
  '("*ansi-term*"
    "nrepl"
    "nrepl-server"
    "theangrypolicecaptain.com")
  "List of buffer names to *not* prompt about killing when quitting Emacs.")

(defadvice process-query-on-exit-flag (around dont-prompt-about-killing (process) activate)
  "Tell Emacs *not* to prompt about killing PROCESS if its name is in cam/dont-prompt-about-killing-processes."
  (if (member (process-name process) cam/dont-prompt-about-killing-processes)
      nil
    ad-do-it))


;; ;; AUTO-UPDATE PACKAGES ON LAUNCH ? YOU CRAY !
(require 'async)
(async-start
 (lambda ()
   (message "STARTING PACKAGE AUTO-UPDATE...")
   (nconc load-path '("~/.emacs.d/lisp/"))
   (require 'package-init)
   (cam/auto-update-packages))
 (lambda (result)
   (message "cam/auto-update-packages finished. -- %s" result)))


;; ## SOMEHOW, SOMEWAY I BROKE THE ESHELL. THIS IS A HACKY FIX AROUND IT
(add-hook 'eshell-mode-hook
  (lambda ()
    (setq-local inhibit-read-only t)))


;;; ## ACE-ZAP-UP-TO-CHAR
(sandbox/install-and-require 'ace-jump-zap)
(cam/define-keys nil
  "M-z" #'ace-jump-zap-up-to-char)


;;; ## 10X FOWARD-LINE / BACKWARD-LINE
(cam/define-keys nil
  "<A-down>" (lambda ()
               (interactive)
               (forward-line 10))
  "<A-up>" (lambda ()
             (interactive)
             (forward-line -10)))


;;; ## `V` when using magit will open corresponding PR on GitHub <3
;;; Inspired by http://endlessparentheses.com/easily-create-github-prs-from-magit.html
(defun cam/visit-pull-request-url ()
  "Visit the current git branch's PR on GitHub."
  (interactive)
  (let ((current-branch (magit-get-current-branch))
        (repo-url (->> (magit-get "remote" (magit-get-current-remote) "url")
                       (string-remove-suffix ".git" ))))
    (browse-url (concat repo-url "/pull/" current-branch))))

(eval-after-load "magit"
  '(cam/define-keys magit-mode-map
     "V" #'cam/visit-pull-request-url))


;; ## BALANCE WINDOWS AFTER DELETING THEM
(defadvice delete-window (after balance-windows-after-delete activate)
  "Re-balance windows after deleting one."
  (balance-windows))

(provide 'sandbox)
;;; sandbox.el ends here
