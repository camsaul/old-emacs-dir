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
;; smooth scrolling
;; (sandbox/install-and-require 'smooth-scrolling)

;;; Show AngryPoliceCaptain.com quotes when saving
(sandbox/install-and-require 'angry-police-captain)
(add-hook 'after-save-hook
  (lambda ()
    (unless (or (active-minibuffer-window)
                (minibufferp(current-buffer))
                ;; (current-message)
                )
      (angry-police-captain))))
(dont-prompt-about-killing "angry-police-captain" "angry-police-captain")

;; sentences don't need 2 spaces to end
(setq sentence-end-double-space nil)

;; color tweaks
;; #ef2929 - TODO use this for something cool

(set-face-attribute 'font-lock-doc-face nil
                    :foreground "black"
                    :bold t)
(set-face-attribute 'font-lock-comment-face nil
                    :foreground "#5fafd7"
                    :bold t
                    :italic t)


;; (setq ediff-split-window-function 'split-window-horizontally)

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
;; (sandbox/install-and-require
;;  ;; 'ido-at-point
;;  )
(nconc ido-ignore-directories '("node_modules"
                                "bower_components"
                                ".git"))
;; (ido-at-point-mode)                     ; what does this do ?

;; projectile ?
(sandbox/install-and-require 'projectile)
(projectile-mode 1)
(::define-keys nil "M-`" #'projectile-recentf)

;; AUTO-UPDATE PACKAGES ON LAUNCH ? YOU CRAY !
(sandbox/install-and-require 'async)
(async-start
 (lambda ()
   (message "STARTING PACKAGE AUTO-UPDATE...")
   (nconc load-path '("~/.emacs.d/lisp/"))
   (require 'package-init)
   (::auto-update-packages))
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
  (set-background-color "#f4f4f4")
  (set-frame-font "Source Code Pro-11")
  ;; (toggle-frame-maximized)
  )

;; (pp (font-family-list))
(set-frame-font "Source Code Pro-11")
;; (set-frame-font "Lucida Sans Typewriter-10")


(when (string= system-type "darwin")
  (menu-bar-mode +1)                    ; actually go ahead and enable menu bar on OS X since

  ;; proced doesn't work on OS X, load up vkill instead
  (sandbox/install-and-require 'vkill)
  (cam-setup-autoloads ("vkill" vkill))
  (fset #'proced #'vkill)               ; swoop proced -> vkill
  (cam/run-fullscreen "vkill" vkill))

;; highlight symbol under point after a short delay
(sandbox/install-and-require 'idle-highlight-mode)
(idle-highlight-mode +1)

;; save the position of point when killing a buffer
(sandbox/install-and-require 'saveplace)
(setq-default save-place t)                     ; enable save-place

(sandbox/install-and-require 'anaconda-mode)
(sandbox/install-and-require 'company-anaconda)

(set-background-color "#F4F4F4")

;; clean up obsolete buffers automatically
(require 'midnight)

;; Automatically make shell scripts executable on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))  ; ZShell scripts should be opened by shell-script-mode

;;; Dockerfile Mode !
(sandbox/install-and-require 'dockerfile-mode)

;; Maybe we don't want the cursor to blink all the time obnoxiously
(blink-cursor-mode -1)

(defadvice git-timemachine (around git-timemachine-split-fullscreen activate)
  "Run git-timemachine for the current buffer in a special temporary fullscreen session"
  (window-configuration-to-register :git-timemachine-fullscreen-window-config)
  (delete-other-windows)
  (split-window-right)
  (call-interactively #'other-window)
  ad-do-it
  (defadvice git-timemachine-quit (after git-timemachine-fullscreen-quit activate)
    (message "DONE <3")
    (jump-to-register :git-timemachine-fullscreen-window-config)
    (advice-remove #'git-timemachine-quit #'git-timemachine-fullscreen-quit)))

;;; show help when showing magit-status
(defadvice magit-status (after magit-status-show-help activate)
  (magit-key-mode-popup-dispatch)       ; show help
  (call-interactively #'other-window))  ; switch back to magit status window

;;;

;; outlined ELisp
(defun ::outline-enable-or-toggle-children ()
  (interactive)
  (if (not outline-minor-mode) (outline-minor-mode)
    (ignore-errors
      (outline-toggle-children))))
(::define-keys nil "H-SPC" #'::outline-enable-or-toggle-children)

;; (sandbox/install-and-require 'outlined-elisp-mode)
;; (add-hook 'emacs-lisp-mode-hook 'outlined-elisp-find-file-hook)

;; recentf can handle dired buffers, and switching to buffer bumps it to top of recentf list
(sandbox/install-and-require 'recentf-ext)

;; REGISTER LIST <3
(sandbox/install-and-require 'register-list)
(::define-keys nil "C-x r r" #'register-list)  ; overrides copy-rectangle-to-register, which I don't think I will ever user

;; (defun cam/cheatsheet ()
;;   (interactive)
;;   (let ((buf (get-buffer-create "*cheatsheet*")))
;;     (switch-to-buffer buf)
;;     (setq buffer-read-only t)
;;     ;; (window-configuration-to-register :cheatsheet-window-conf)
;;     ;; (delete-other-windows)
;;     ;; (add-hook 'kill-buffer-hook
;;     ;;   (lambda ()
;;     ;;     (jump-to-register :cheatsheet-window-conf))
;;     ;;   t t))
;;   )
;; (cam/fullscreen "sandbox" cam/cheatsheet)

;; ---------------- TO INVESTIGATE ----------------
;;   map-regexp         20130522.... available  melpa      map over matches of a regular expression

(sandbox/install-and-require 'dired-rainbow)

;; NICE <3
(sandbox/install-and-require 'clojure-cheatsheet)
(require 'clojure-cheatsheet)
;; (clojure-cheatsheet)

;; maximize-frame
(sandbox/install-and-require 'maxframe)


(provide 'sandbox)
;;; sandbox.el ends here
