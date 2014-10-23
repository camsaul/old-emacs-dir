;;; sandbox --- features that are "under consideration"
;;; Commentary:
;;; Code:

(defun sandbox/install-and-require (package &rest more-packages)
  "Install PACKAGE if needed, require it for sandbox testing."
  (let ((package-name (symbol-name package)))
    (condition-case err
        (progn
          (unless (package-installed-p package)
            (message "--SANDBOX-- Installing package: %s..." package-name)
            (cam/refresh-package-contents-once)
            (package-install package))
          (package-activate package)
          (require package)
          (message "--SANDBOX-- Loaded package %s." package-name))
      (error (warn "--SANDBOX-- Failed to install %s: %s" package-name (error-message-string err)))))
  (when more-packages
    (apply 'sandbox/install-and-require more-packages)))

(defmacro dont-prompt-about-killing (package process)
  "Don't prompt before killing PROCESS with matching string name from PACKAGE with string name."
  `(eval-after-load ,package
     (quote (-map-when (-lambda (p) (->> p
                                      process-name
                                      (string-match-p ,process)))
                       (-rpartial 'set-process-query-on-exit-flag nil)
                       (process-list)))))

;; Do I want this?
(setq enable-recursive-minibuffers t)

;; smooth scrolling
(sandbox/install-and-require 'smooth-scrolling)


;; number of bytes of consing before garbage collection
;; default is ~800k, we can use a little bit more than that these days
(setq gc-cons-threshold (* 1024 1024 1024 4)) ; 4 GB

;;; Show AngryPoliceCaptain.com quotes when saving
(sandbox/install-and-require 'angry-police-captain)
(add-hook 'after-save-hook
  (lambda ()
    (unless (active-minibuffer-window)
      (angry-police-captain))))
(dont-prompt-about-killing "angry-police-captain" "angry-police-captain")

;; sentences don't need 2 spaces to end
(setq sentence-end-double-space nil)

(defalias #'pretty-lambdas #'::noop)
(global-prettify-symbols-mode 1)
(kill-local-variable 'prettify-symbols-alist)
(setq-default prettify-symbols-alist
  '(("nil" . ?¿)
    ("defun" . ?ƒ)))
(prettify-symbols-mode 1)

;; color tweaks
;; #ef2929 - TODO use this for something cool

(set-face-attribute 'font-lock-doc-face nil
                    :foreground "black"
                    :bold t)
(set-face-attribute 'font-lock-comment-face nil
                    :foreground "#ff0099"
                    :bold t
                    :italic t)


;; (setq ediff-split-window-function 'split-window-horizontally)

;; Save point position between sessions
;; (what was the command ?)

;; term tweaks
(dont-prompt-about-killing "term" "*ansi-term*")
(setq shell-file-name "/usr/local/bin/bash") ; why don't these work
(eval-after-load "term"
  (setq term-ansi-default-program "/usr/local/bin/bash"))


;; ido tweaks
(setq ido-enable-flex-matching t)
(sandbox/install-and-require
 'ido-vertical-mode
 'ido-at-point)
(ido-vertical-mode)
(nconc ido-ignore-directories '("node_modules"
                                "bower_components"
                                ".git"))
(ido-at-point-mode)                     ; what does this do ?


;; company tweaks
(eval-after-load "company"
  '(setq company-idle-delay 0.01        ; default is 0.5
         company-minimum-prefix-length 1 ; default is 3
         ))

;; projectile ?
(sandbox/install-and-require 'projectile)
(projectile-mode 1)
(cam/define-keys nil
  "M-`" 'projectile-recentf)

(provide 'sandbox)
;;; sandbox.el ends here
