;;; sandbox --- features that are "under consideration"
;;; Commentary:
;;; Code:

;; [[<sandbox/install-and-require]]

(defmacro dont-prompt-about-killing (package process)
  "Don't prompt before killing PROCESS with matching string name from PACKAGE with string name."
  `(eval-after-load ,package
     (quote (-map-when (-lambda (p) (->> p
                                      process-name
                                      (string-match-p ,process)))
                       (-rpartial 'set-process-query-on-exit-flag nil)
                       (process-list)))))

;; (sandbox/install-and-require 'smooth-scrolling)

;;; Show AngryPoliceCaptain.com quotes when saving
(add-hook 'after-save-hook
  (lambda ()
    (unless (or (active-minibuffer-window)
                (minibufferp(current-buffer))                )
      (angry-police-captain))))
(dont-prompt-about-killing "angry-police-captain" "angry-police-captain")

;; color tweaks
;; #ef2929 - TODO use this for something cool

(set-face-attribute 'font-lock-doc-face nil
                    :foreground "black"
                    :bold t)
(set-face-attribute 'font-lock-comment-face nil
                    :foreground "#5fafd7"
                    :bold t
                    :italic t)

;; term tweaks
(dont-prompt-about-killing "term" "*ansi-term*")

;; CAR-AZY SYNTAX HIGHLIGHTING
(defmacro cam/add-keywords-patterns (face &rest patterns)
  `(progn
     ,@(mapcar (lambda (pattern)
               (list 'font-lock-add-keywords ''emacs-lisp-mode
                     `(quote ((,pattern 1 ,face)))))
               patterns)))

(defmacro cam/add-keywords (face &rest kws)
  `(cam/add-keywords-patterns
    ,face ,@(mapcar (lambda (kw)
                      (concat "\\<\\(" kw "\\)\\>"))
                    kws)))

(set-face-attribute 'font-lock-builtin-face nil
                    :foreground "cc6633"
                    :bold nil)

(set-face-attribute 'font-lock-function-name-face nil
                    :foreground "#008FD7"
                    :bold t)

;; (cam/add-keywords 'font-lock-builtin-face
;;                 "add-hook"
;;                 "defmacro"
;;                 "defun"
;;                 "eval-after-load"
;;                 "message"
;;                 "nconc"
;;                 "require"
;;                 "set-face-attribute"
;;                 "setq"
;;                 )

(cam/add-keywords 'font-lock-warning-face
                "sandbox/[a-z-:/]+")
;; (cam/add-keywords-patterns 'font-lock-constant-face
;;                          "(?\\(cam/[a-z-/:]+\\)\\>")

;; (cam/add-keywords 'font-lock-constant-face
;;                 "cam/[a-z-:/]+")

(set-face-attribute 'font-lock-preprocessor-face nil
                    :bold nil
                    :italic t)

;; (cam/add-keywords 'font-lock-doc-face
;;                 "t"
;;                 "nil")

;; (cam/add-keywords-patterns 'font-lock-preprocessor-face
;;                          "setq \\<\\([a-z-:/]+\\)\\>"
;;                          "'\\<\\([a-z-:/]+\\)\\>")

(nconc ido-ignore-directories '("node_modules"
                                "bower_components"
                                ".git"))

;; AUTO-UPDATE PACKAGES ON LAUNCH ? YOU CRAY !
(sandbox/install-and-require 'async)
(async-start
 (lambda ()
   (message "STARTING PACKAGE AUTO-UPDATE...")
   (nconc load-path '("~/.emacs.d/lisp/"))
   (require 'package-init)
   (cam/auto-update-packages))
 (lambda (result)
   (message "cam/auto-update-packages finished. -- %s" result)))

;; write backup files to own directory(setq backup - directory - alist `(("." . ,(expand-file-name
;; (concat user-emacs-directory "backups")))))
;; (setq vc-make-backup-files t) ;; make backups even if files are under VC

;; WIKI NAV
(sandbox/install-and-require 'wiki-nav)
(global-wiki-nav-mode 1)
(diminish 'button-lock-mode)
(diminish 'wiki-nav-mode)
;; see [[sandbox/install-and-require]]

(defadvice make-frame-command (after make-frame-set-font activate)
  (interactive)
  (set-background-color "#f4f4f4")
  (set-frame-font "Source Code Pro-11"))

(set-frame-font "Source Code Pro-11")


(when (string= system-type "darwin")
  (menu-bar-mode +1)                    ; actually go ahead and enable menu bar on OS X since

  ;; proced doesn't work on OS X, load up vkill instead
  (sandbox/install-and-require 'vkill)
  (cam/setup-autoloads ("vkill" vkill))
  (fset #'proced #'vkill)               ; swoop proced -> vkill
  (cam/run-fullscreen "vkill" vkill))

(set-background-color "#F4F4F4")

;; clean up obsolete buffers automatically
(require 'midnight)

;; (add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))  ; ZShell scripts should be opened by shell-script-mode

;; (defun cam/outline-enable-or-toggle-children ()
;;   (interactive)
;;   (if (not outline-minor-mode) (outline-minor-mode)
;;     (ignore-errors
;;       (outline-toggle-children))))
;; (cam/define-keys nil "H-SPC" #'cam/outline-enable-or-toggle-children)

;; recentf can handle dired buffers, and switching to buffer bumps it to top of recentf list
(sandbox/install-and-require 'recentf-ext)

(sandbox/install-and-require 'pretty-symbols)
(setq pretty-symbol-categories '(lambda relational logical nil cam))
(nconc pretty-symbol-patterns
       '(;; general
         (?ℛ cam "\\<require\\>" (emacs-lisp-mode))                              ; REQUIRE
         (?ƒ cam "\\<defun\\>" (emacs-lisp-mode))                              ; DEFUN
         (?ƒ cam "\\<def\\>" (django-mode python-mode))                    ; DEF
         (?∫ cam "\\<self\\>" (emacs-lisp-mode django-mode python-mode))     ; SELF
         ;; python-specific
         (?∧ logical "\\<and\\>" (python-mode django-mode))                  ; AND
         (?∨ logical "\\<or\\>" (python-mode django-mode))                  ; OR
         (?¬ logical "\\<not\\>" (python-mode django-mode))                  ; NOT
         (?∅ logical "\\<None\\>" (python-mode django-mode))               ; NONE
         (?✓ logical "\\<True\\>" (python-mode django-mode))               ; TRUE
         (?𐄂 logical "\\<False\\>" (python-mode django-mode))              ; FALSE
         (?∀ logical "\\<for\\>" (python-mode django-mode))              ; FOR
         (?∈ logical "\\<in\\>" (python-mode django-mode))                 ; IN
         (?∉ logical "\\<not in\\>" (python-mode django-mode))               ; NOT IN
         (?⊦ logical "\\<assert\\>" (python-mode django-mode))             ; ASSERT
         (?≡ logical "==" (python-mode django-mode))                       ; ==
         (?∃ logical "\\<if\\>" (python-mode django-mode))                 ; IF
         (?∄ logical "\\<if not\\>" (python-mode django-mode))               ; IF NOT
         (?∋ logical "\\<hasattr\\>" (python-mode django-mode))            ; HASATTR
         ;; (?∌ logical "\\<not hasattr\\>" (python-mode django-mode))          ; NOT HASATTR
         (?↪ cam "\\<return\\>" (python-mode django-mode))                 ; RETURN
         (?⚠ cam "\\<raise\\>" (python-mode django-mode))                  ; RAISE
         (?⎾ cam "\\<try\\>" (python-mode django-mode))                    ; TRY
         (?⎿ cam "\\<except\\>" (python-mode django-mode))                 ; EXCEPT
         (?⌥ cam "\\<else\\>" (python-mode django-mode))                   ; ELSE
         (?Ⓐ cam "\\<args\\>" (python-mode django-mode))                   ; ARGS
         (?Ⓚ cam "\\<kwargs\\>" (python-mode django-mode))                 ; KWARGS
         (?℮ cam "\\<Exception\\>" (python-mode django-mode))              ; EXCEPTION
         ))

;; pretty-symbol-patterns
(mapc (-rpartial #'add-hook 'pretty-symbols-mode)
      '(emacs-lisp-mode-hook))

;; more symbols to use (?)
;; ∘ ∙ ∫ ∮ ⊛

(provide 'sandbox)

;; add all cl-lib font-lock-highlighting to emacs-lisp-mode
(sandbox/install-and-require 'cl-lib-highlight)
(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (ignore-errors
      (cl-lib-highlight-initialize))))

;;; packages to checkout !
;;; macrostep - interactive macro stepper for Emacs Lisp
;;; web-mode
;;;

;;;; PRETTY RAD FUNCTIONS / KEYBINDINGS, INSPIRED BY GRAPHENE
(defun cam/add-semicolon-to-eol ()
  "Add a semicolon to the end of current line without affecting point."
  (interactive)
  (save-excursion
    (end-of-line)
    (insert ";")))

(defun cam/insert-newline-below ()
  "Insert a newline below and move to it." ; OH THIS IS NICE
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defmacro cam/funcall-for-key-binding (keys)
  "Call the fn bound to KEYS."
  `(funcall (key-binding ,(kbd keys))))

(defun cam/comment-current-line ()
  "Comment/uncomment the current line."
  (interactive)
  (save-excursion
    (end-of-line)
    (set-mark (point))
    (beginning-of-line)
    (cam/funcall-for-key-binding "M-;")))

(cam/define-keys nil
  "C-;" #'cam/add-semicolon-to-eol
  "M-RET" #'cam/insert-newline-below
  "C-M-;" #'cam/comment-current-line)

;; THINGS TO CHECK OUT !
;; clang-format-before-save - run clang format every time you save a C++ file

;;; sandbox.el ends here

(sandbox/install-and-require 'http-post-simple)

(defvar slack-channel
  "data"
  "The channel to post Slack messages to.")

(defun slack-url ()
  "The URL for posting Slack messages."
  (concat "https://expa.slack.com/services/hooks/slackbot?token=FQ2iLGcb6m0dcByOTuSfnoZQ&channel=" slack-channel))

(defun set-slack-user (username)
  "Send future Slack messages to USERNAME."
  (interactive "sSend Slack messages to user: ")
  (setq slack-channel (concat "%40" username)))

(defun set-slack-channel (channel)
  "Send future Slack messages to CHANNEL."
  (interactive "sSend Slack messages to channel: ")
  (setq slack-channel (concat "%23" channel)))

(defun post-to-slack (message)
  "Post MESSAGE to Slack."
  (interactive "sMessage: ")
  (http-post-simple-internal
   (slack-url)
   message
   'utf-8
   nil)
  message)

(defun angry-police-captain-to-slack ()
  "Post angry police caption quote to slack."
  (interactive)
  (angry-police-captain)
  (sleep-for 1) ; wait for HTTP request to finish
  (->> (current-message)
    (s-replace "The Angry Police Captain" "David Baden")
    post-to-slack
    message))

(cam/define-keys nil
  "H-M-p" #'post-to-slack
  "H-M-P" #'angry-police-captain-to-slack
  "H-M-u" #'set-slack-user
  "H-M-c" #'set-slack-channel)

(provide 'sandbox)
;;; sandbox.el ends here
