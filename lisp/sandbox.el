;;; sandbox --- features that are "under consideration"
;;; Commentary:
;;; Code:

;; [[<sandbox/install-and-require]]

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

;;; Show AngryPoliceCaptain.com quotes when saving
(sandbox/install-and-require 'angry-police-captain)
(add-hook 'after-save-hook
  (lambda ()
    (unless (or (active-minibuffer-window)
                (current-message))
      (angry-police-captain))))
(dont-prompt-about-killing "angry-police-captain" "angry-police-captain")

;; sentences don't need 2 spaces to end
(setq sentence-end-double-space nil)

;; (defalias #'pretty-lambdas #'::noop)
;; (global-prettify-symbols-mode 1)
;; (kill-local-variable 'prettify-symbols-alist)
;; (setq-default prettify-symbols-alist
;;   '(("nil" . ?¿)
;;     ("defun" . ?ƒ)))
;; (prettify-symbols-mode 1)

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
;; (setq shell-file-name "/usr/local/bin/bash")
                                        ; why don't these work
;; (eval-after-load "term"
;;   (setq term-ansi-default-program "/usr/local/bin/bash"))

;; CAR-AZY SYNTAX HIGHLIGHTING
(defmacro ::add-keywords-patterns (face &rest patterns)
  `(progn
     ,@(mapcar (lambda (pattern)
               (list 'font-lock-add-keywords ''emacs-lisp-mode
                     `(quote ((,pattern 1 ,face)))))
               patterns)))

(defmacro ::add-keywords (face &rest kws)
  `(::add-keywords-patterns
    ,face ,@(mapcar (lambda (kw)
                      (concat "\\<\\(" kw "\\)\\>"))
                    kws)))

(set-face-attribute 'font-lock-builtin-face nil
                    :foreground "cc6633"
                    :bold nil)

(set-face-attribute 'font-lock-function-name-face nil
                    :foreground "#008FD7"
                    :bold t)

(::add-keywords 'font-lock-builtin-face
                "add-hook"
                "defmacro"
                "defun"
                "eval-after-load"
                "message"
                "nconc"
                "require"
                "set-face-attribute"
                "setq"
                )

(::add-keywords 'font-lock-warning-face
                "sandbox/[a-z-:/]+")
(::add-keywords-patterns 'font-lock-constant-face
                         "(?\\(::[a-z-/:]+\\)\\>")

(::add-keywords 'font-lock-constant-face
                "::[a-z-:/]+"
                "cam/[a-z-:/]+"
                "cam-[a-z-:/]+")

(set-face-attribute 'font-lock-preprocessor-face nil
                    :bold nil
                    :italic t)

(::add-keywords 'font-lock-doc-face
                "t"
                "nil")

(::add-keywords-patterns 'font-lock-preprocessor-face
                         "setq \\<\\([a-z-:/]+\\)\\>"
                         "'\\<\\([a-z-:/]+\\)\\>")

;; ido tweaks
;; (setq ido-enable-flex-matching t)
(sandbox/install-and-require
 'ido-vertical-mode
 ;; 'ido-at-point
 )
(ido-vertical-mode)
(nconc ido-ignore-directories '("node_modules"
                                "bower_components"
                                ".git"))
;; (ido-at-point-mode)                     ; what does this do ?

;; projectile ?
(sandbox/install-and-require 'projectile)
(projectile-mode 1)
(cam/define-keys nil
  "M-`" 'projectile-recentf)

;;;; EXPERIMENTAL KEYBINDINGS (!)
(::define-keys nil
  "`" 'helm-buffers-list                ; Seeing how I switch buffers more often than typing a `. Can still do C-q `
  )

;; AUTO-UPDATE PACKAGES ON LAUNCH ? YOU CRAY !
(sandbox/install-and-require 'async)
(async-start
 (lambda ()
   (message "STARTING")
   (nconc load-path '("~/.emacs.d/lisp/"))
   (require 'package-init)
   (::auto-update-packages)
   )
 (lambda (result)
   (message "::auto-update-packages finished. -- %s" result)))

;; Major modes for editing .gitignore, .git/config files... do I really need these ?
(sandbox/install-and-require 'gitignore-mode
                             'gitconfig-mode)

;; write backup files to own directory(setq backup - directory - alist `(("." . ,(expand-file-name
;; (concat user-emacs-directory "backups")))))
;; (setq vc-make-backup-files t) ;; make backups even if files are under VC

;; WIKI NAV
(sandbox/install-and-require 'wiki-nav)
(global-wiki-nav-mode 1)
(diminish 'button-lock-mode)
(diminish 'wiki-nav-mode)
;; see [[sandbox/install-and-require]]

(setq pp-escape-newlines t)

(defadvice make-frame-command (after make-frame-set-font activate)
  (interactive)
  ;; (toggle-frame-maximized)
  (set-frame-font "Source Code Pro-10")
  (toggle-frame-maximized))

;; (pp (font-family-list))
(set-frame-font "Source Code Pro-10")
;; (set-frame-font "Lucida Sans Typewriter-10")

(provide 'sandbox)
;;; sandbox.el ends here
