;;; sandbox --- features that are "under consideration"
;;; Commentary:
;;; Code:

;; [[<sandbox/install-and-require]]

;; DOES THIS EVEN WORK ??
(defmacro dont-prompt-about-killing (package process)
  "Don't prompt before killing PROCESS with matching string name from PACKAGE with string name."
  `(eval-after-load ,package
     (quote (-map-when (-lambda (p) (->> p
                                 process-name
                                 (string-match-p ,process)))
                       (-rpartial 'set-process-query-on-exit-flag nil)
                       (process-list)))))

;; (sandbox/install-and-require 'smooth-scrolling)

(dont-prompt-about-killing "angry-police-captain" "angry-police-captain")


;; term tweaks
(dont-prompt-about-killing "term" "*ansi-term*")

;; CAR-AZY SYNTAX HIGHLIGHTING
;; (defmacro cam/add-keywords-patterns (face &rest patterns)
;;   `(progn
;;      ,@(mapcar (lambda (pattern)
;;                  (list 'font-lock-add-keywords ''emacs-lisp-mode
;;                        `(quote ((,pattern 1 ,face)))))
;;                patterns)))

;; (defmacro cam/add-keywords (face &rest kws)
;;   `(cam/add-keywords-patterns
;;     ,face ,@(mapcar (lambda (kw)
;;                       (concat "\\<\\(" kw "\\)\\>"))
;;                     kws)))

;; (cam/add-keywords 'font-lock-warning-face
;;                   "sandbox/[a-z-:/]+")


(font-lock-add-keywords 'emacs-lisp-mode
                        '(("\\<\\(sandbox/install-and-require\\)\\>" 1 'font-lock-warning-face)))

(nconc ido-ignore-directories '("node_modules"
                                "bower_components"
                                ".git"))

;; AUTO-UPDATE PACKAGES ON LAUNCH ? YOU CRAY !
(require 'async)
(async-start
 (lambda ()
   (message "STARTING PACKAGE AUTO-UPDATE...")
   (nconc load-path '("~/.emacs.d/lisp/"))
   (require 'package-init)
   (cam/auto-update-packages))
 (lambda (result)
   (message "cam/auto-update-packages finished. -- %s" result)))

;; write backup files to own directory
(setq backup-directory-alist
  `(("." . ,(expand-file-name
             (concat user-emacs-directory "backups")))))
(setq vc-make-backup-files t) ;; make backups even if files are under VC

;; ;; WIKI NAV
;; (sandbox/install-and-require 'wiki-nav)
;; (global-wiki-nav-mode 1)
;; (diminish 'button-lock-mode)
;; (diminish 'wiki-nav-mode)
;; ;; see [[sandbox/install-and-require]]


(when (string= system-type "darwin")
  (menu-bar-mode +1)                    ; actually go ahead and enable menu bar on OS X since

  ;; proced doesn't work on OS X, load up vkill instead
  (sandbox/install-and-require 'vkill)
  (cam/setup-autoloads ("vkill" #'vkill))
  (fset #'proced #'vkill)               ; swoop proced -> vkill
  (cam/run-fullscreen "vkill" vkill))


;; clean up obsolete buffers automatically
(require 'midnight)
;; (midnight-delay-set)

(setq apropos-do-all t
      mouse-yank-at-point t
      select-enable-primary t           ; cutting and pasting uses the primary selection ?
      )

;; (add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))  ; ZShell scripts should be opened by shell-script-mode

;; ;; recentf can handle dired buffers, and switching to buffer bumps it to top of recentf list
;; (sandbox/install-and-require 'recentf-ext)

(sandbox/install-and-require 'pretty-symbols)
(setq pretty-symbol-categories '(lambda relational logical nil cam))
(nconc pretty-symbol-patterns
       '(;; general
         (?ƒ cam "\\<defun\\>" (emacs-lisp-mode))                              ; DEFUN
         (?ƒ cam "\\<def\\>" (django-mode python-mode))                    ; DEF
         (?∫ cam "\\<self\\>" (emacs-lisp-mode django-mode python-mode))     ; SELF
         ;; python-specific
         (?∧ logical "\\<and\\>" (python-mode django-mode))                  ; AND
         (?∨ logical "\\<or\\>" (python-mode django-mode))                  ; OR
         (?∅ logical "\\<None\\>" (python-mode django-mode))               ; NONE
         (?✓ logical "\\<True\\>" (python-mode django-mode))               ; TRUE
         (?𐄂 logical "\\<False\\>" (python-mode django-mode))              ; FALSE
         (?≡ logical "==" (python-mode django-mode))                       ; ==
         (?↪ cam "\\<return\\>" (python-mode django-mode))                 ; RETURN
         ))

;; pretty-symbol-patterns
(mapc (-rpartial #'add-hook 'pretty-symbols-mode)
      '(emacs-lisp-mode-hook))

;;; THINGS TO CHECK OUT !
;;; clang-format-before-save - run clang format every time you save a C++ file
;;; web-mode

;;; HARASS COWORKERS BY AUTO-POSTING TO SLACK

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
    (s-replace "The Angry Police Captain" "Hobbs") ; actually, let's do angry Hobbs quotes
    post-to-slack
    message))

(cam/define-keys nil
  "H-M-p" #'post-to-slack
  "H-M-P" #'angry-police-captain-to-slack
  "H-M-u" #'set-slack-user
  "H-M-c" #'set-slack-channel)

(sandbox/install-and-require 'backup-each-save)
(add-hook 'after-save-hook
  #'backup-each-save)

;; (require 'json)
;; (-let (((response-json . response-info)
;;         (http-post-simple-internal
;;          "https://slack.com/api/auth.test" ; chat.postMessage
;;          (json-encode '(:token "xoxp-2170866713-2715807697-3066816744-33b4bb"
;;                                :username "hobbs"
;;                                :channel "data"
;;                                :text "HELLO"
;;                                :icon_url "https://s3-us-west-2.amazonaws.com/slack-files2/avatars/2014-11-14/3015677761_9539e16a0d353e4f17c4_48.jpg"))
;;          'utf-8
;;          `(("Content-Type"
;;             .
;;             ,(http-post-content-type
;;               "application/json"
;;               'utf-8))))))
;;   (->> response-json
;;     json-read-from-string)
;;   ;; response-info
;;   )

;; (defun json-encode-dict (&rest kwargs)
;;   "Encode KWARGS :key value into a JSON dict."
;;   (->> kwargs
;;     (-partition 2)
;;     (mapcar (-lambda ((kar kdr)) ; convert '(k v) -> '(k . v)
;;               (cons kar kdr)))
;;     json-encode))

;; (json-encode-dict
;;  :a "COOL"
;;  :b "VERY COOL"
;;  :c "SICK")

;; SOMEHOW, SOMEWAY I BROKE THE ESHELL. THIS IS A HACKY FIX AROUND IT
(add-hook 'eshell-mode-hook
  (lambda ()
    (setq-local inhibit-read-only t)))

;; WOAH
(sandbox/install-and-require 'guide-key)
(require 'guide-key)
(guide-key-mode 1)
(setq guide-key/idle-delay 0.1                    ; delay before showing the guide-key popup
      guide-key/recursive-key-sequence-flag t     ; e.g. specifying C-x below means to also show guides for things like C-x r
      guide-key/guide-key-sequence '(             ; prefixes to should guides for
                                     "<f12>"
                                     "<f1>"
                                     "<help>"
                                     "A-'"
                                     "A-*"
                                     "A-,"
                                     "A-/"
                                     "A-1"
                                     "A-3"
                                     "A-\""
                                     "A-^"
                                     "A-_"
                                     "A-`"
                                     "A-r"
                                     "A-~"
                                     "C-c"
                                     "C-h"
                                     "C-x"
                                     "M-g"
                                     "M-o"
                                     ))

(provide 'sandbox)
;;; sandbox.el ends here
