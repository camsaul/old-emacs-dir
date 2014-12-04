;;; sandbox --- features that are "under consideration"
;;; Commentary:
;;; Code:


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

(when (string= system-type "darwin")
  ;; proced doesn't work on OS X, load up vkill instead
  (sandbox/install-and-require 'vkill)
  (cam/setup-autoloads ("vkill" #'vkill))
  (fset #'proced #'vkill)               ; swoop proced -> vkill
  (cam/run-fullscreen "vkill" vkill))


;; clean up obsolete buffers automatically
(require 'midnight)
;; (midnight-delay-set)


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

                                                  ;
(sandbox/install-and-require 'backup-each-save)
(add-hook 'after-save-hook
  #'backup-each-save)

;; SOMEHOW, SOMEWAY I BROKE THE ESHELL. THIS IS A HACKY FIX AROUND IT
(add-hook 'eshell-mode-hook
  (lambda ()
    (setq-local inhibit-read-only t)))

(cam/setup-autoloads
  ("misc" #'zap-up-to-char))

;;; MORE NONSENSE
(cam/define-keys nil
  "C-x C-b" #'helm-buffers-list         ; this is (seemingly) better than buffer-menu or ibuffer
  ;; "M-z" #'zap-up-to-char             ; instead of zap-to-char
  )

(setq save-interprogram-paste-before-kill t       ; save clipboard strings (from other programs besides Emacs) into kill ring before replacing them in Emacs
      apropos-do-all t
      mouse-yank-at-point t                       ; mouse yank commands yank at point instead of at click.
      )

(provide 'sandbox)
;;; sandbox.el ends here
