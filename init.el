;; -*- lexical-binding: t -*-
;; -*- comment-column: 50 -*-

;;; init -- General Setup
;;; Commentary:
;;; Code:

;;;; DISABLE MENU/SCROLLBAR/TOOLBAR ASAP SO THEY DON'T FLASH

(mapc (lambda (mode)
        (when (boundp mode)
          (funcall mode -1)))
      '(scroll-bar-mode
        tool-bar-mode))
(unless (string= system-type "darwin")
  (menu-bar-mode -1))
(toggle-frame-maximized)

(setq-default default-frame-alist nil)


;;;; LOAD PACKAGES

(setq load-prefer-newer t)                        ; Load from .el files if they are newer than matching .elc files

(nconc load-path '("~/.emacs.d/lisp/"))


;;;; PACKAGES TO ALWAYS REQUIRE ON LAUNCH

(mapc #'require '(cl                              ; Common Lisp functions like cl-flet and stuffffff
                  package-init                    ; needs to be loaded before we can load ELPA packages like dash or powerline
                  dash                            ; load this next so cam-functions can build on it
                  dash-functional                 ; TODO - should we just load "s" and "f" here too ?
                  noflet                          ; noflet lets you temporarily override function definitions
                  cam-macros                      ; my own helper macros
                  cam-functions                   ; my own helper functions
                  powerline                       ; powerline is the port of VIM powerline (pretty Emacs modeline)
                  saveplace))                     ; open files in the same location as you last closed them


;;;; SETUP AUTOLOADS FOR FUNCTIONS THAT NEED IT

;;; autoloads tell Emacs what files to lazily load when you run certain functions
;;; this makes Emacs load faster than calling require for dozens of files that we may not use right away
(cam/setup-autoloads
  ("ace-jump-buffer" #'ace-jump-same-mode-buffers)
  ("bm" #'bm-show-all)
  ("bytecomp" #'byte-recompile-file)
  ("find-things-fast" #'ftf-find-file #'ftf-grepsource)
  ("loccur" #'loccur #'loccur-current #'loccur-previous-match)
  ("highlight-error-keywords" #'highlight-error-keywords-mode)
  ("multiple-cursors" #'mc/mark-all-like-this #'mc/edit-lines #'mc/mark-previous-like-this #'mc/mark-next-like-this)
  ("s" #'s-replace #'s-split #'s-starts-with-p)
  ("vkill" #'vkill))


;;;; GLOBALLY DISABLED MINOR MODES

(cam/disable-minor-modes
  blink-cursor-mode                               ; disable blinking cursor - TODO this seem to work unless done after theme loads (?)
  indent-tabs-mode                                ; disable indentation w/ tabs
  line-number-mode                                ; line numbers on the modeline
  set-fringe-mode                                 ; disable fringes
  )


;;;; GLOBALLY ENABLED MINOR MODES

(cam/suppress-messages
 (cam/enable-minor-modes
   delete-selection-mode                          ; typing will delete selected text
   electric-pair-mode                             ; automatic parens pairing
   global-ace-isearch-mode                        ; C-s switches to ace-jump or helm-swoop after a delay depending on length of search str
   global-auto-revert-mode                        ; automatically reload files when they change on disk
   global-diff-hl-mode                            ; Shows lines that have changed since last VC commit in the fringe
   global-hl-line-mode                            ; highlight the current line
   global-linum-mode                              ; show line numbers on the left margin
   global-undo-tree-mode                          ; make undo work in a tree instead of linearly
   (guide-key-mode . nil)                         ; show completions for prefix keybindings
   ido-mode                                       ; additional setup for ido in ido-config.el
   (rainbow-mode . nil)                           ; colorize strings that represent colors e.g. #00FFFF
   projectile-global-mode
   recentf-mode                                   ; enable the recent files menu
   savehist-mode                                  ; save minibuffer history periodically !!! DEPRECATED this seems to make things really SLOWWWWWW
   show-paren-mode                                ; highlight matching parens
   (undo-tree-mode . nil)                         ; already on because of global-undo-tree-mode but we can't diminish that so diminish this one instead
   winner-mode))                                  ; C-c <left> / C-c <right> to restore window configurations


;;;; GLOBAL HOOKS

(add-hook 'emacs-startup-hook
  (lambda ()
    (kill-buffer "*scratch*")))

(add-hook 'before-save-hook
  (lambda ()
    (delete-trailing-whitespace)
    (set-buffer-file-coding-system 'utf-8-auto-unix)))

(add-hook 'after-save-hook
  (lambda ()
    (executable-make-buffer-file-executable-if-script-p) ; if we're saving a script, give it permissions to execute

    ;; show a AngryPoliceCaptain.com quote
    (unless (or (active-minibuffer-window)
                (minibufferp (current-buffer))
                (eq major-mode 'package-menu-mode))
      (with-timeout (0.25 nil)
        (angry-police-captain)))))

;; Enable paredit + company when evaluating elisp expressions in minibuffer
(add-hook 'eval-expression-minibuffer-setup-hook
  (lambda ()
    (cam/enable-minor-modes
      company-mode
      paredit-mode)
    (setq-local company-echo-delay 10)))

(add-hook 'after-change-major-mode-hook
  (lambda ()
    (cam/enable-minor-modes
      highlight-error-keywords-mode
      rainbow-delimiters-mode
      (rainbow-mode . nil)))
  t)


;;;; GENERAL SETTINGS

(prefer-coding-system 'utf-8-auto-unix)

(ansi-color-for-comint-mode-on)                   ; Works better in Terminal or something like that
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

(setq
    apropos-do-all t                              ; apropos commands will search more extensively
    auto-revert-verbose nil
    auto-window-vscroll nil                       ; don't 'automatically adjust window to view tall lines'
    backup-directory-alist                        ; Write backups to  ~/.emacs.d/backups
    `(("." . ,(expand-file-name
               (concat user-emacs-directory
                       "backups"))))
    bm-cycle-all-buffers t                        ; visual bookmarks bm-next and bm-previous should cycle all buffers
    clean-buffer-list-delay-special 30
    echo-keystrokes 0.1                           ; shorter delay before showing keystrokes in progress
    global-auto-revert-non-file-buffers t         ; also refresh dired but be quiet about it
    inhibit-splash-screen t
    inhibit-startup-screen t
    locale-coding-system 'utf-8-auto-unix
    gc-cons-threshold (* 1024 1024 32)            ; number of bytes of consing before garbage collection. Default is ~800k, use 32MB instead
    guide-key/idle-delay 1.0                      ; delay before showing the guide-key popup
    guide-key/recursive-key-sequence-flag t       ; e.g. specifying C-x below means to also show guides for things like C-x r
    guide-key/guide-key-sequence '("<f12>" "<f1>" ; prefixes to show guides for
                                   "<help>" "A-'"
                                   "A-*" "A-,"
                                   "A-/" "A-1"
                                   "A-3" "A-\""
                                   "A-^" "A-_"
                                   "A-`" "A-r"
                                   "A-~" "C-c"
                                   "C-h" "C-x"
                                   "M-g" "M-o")
    mouse-wheel-scroll-amount '(1 ((shift) . 1 ))
    mouse-yank-at-point t                         ; mouse yank commands yank at point instead of at click.
    nav-width 30                                  ; nav should be 30 chars wide (default is 18)
    ns-right-command-modifier 'hyper
    ns-right-control-modifier 'hyper
    ns-right-option-modifier 'alt
    recentf-max-menu-items 50
    redisplay-dont-pause t                        ; don't pause screen drawing whenever input is detected - causes screen tearning, unneccessary
    require-final-newline t                       ; add final newline on save
    revert-without-query '(".*")                  ; disable revert-buffer confirmation prompts
    save-interprogram-paste-before-kill t         ; save clipboard strings (from other programs besides Emacs) into kill ring before replacing them in Emacs
    scroll-margin 1
    select-enable-clipboard t                     ; Use the clipboard in addition to emacs kill ring
    select-enable-primary t                       ; cutting and pasting uses the primary selection (?)
    w32-apps-modifier 'hyper
    w32-lwindow-modifier 'super
    w32-pass-lwindow-to-system nil
    w32-rwindow-modifier 'alt
    whitespace-line-column 200                    ; don't highlight lines in whitespace mode unless they're REALLY giant. (default is 80)
    vc-make-backup-files t                        ; make backups even if files are under VC
    visible-bell t)

(setq-default
    truncate-lines t                              ; don't word-wrap long lines
    save-place t                                  ; save current position of point when killing a buffer; restore when file is opened
    )


;;;; FUNCTION OVERRIDES W/ FSET

(fset #'yes-or-no-p #'y-or-n-p)                   ; prompt for y/n instead of yes/no
(fset #'proced #'vkill)                           ; use vkill instead of proced since it doesn't work on OS X


;;;; COMMANDS TO ENABLE

(mapc (lambda (fn)
        (put fn 'disabled nil))
      '(downcase-region
        upcase-region
        narrow-to-region))


;;;; COMMANDS TO ALWAYS RUN FULLSCREEN

(mapc (lambda (args) (eval `(cam/run-fullscreen ,@args)))
      '(("package" list-packages package-list-packages package-list-packages-no-fetch)
        ("vkill" vkill)))


;;;; GLOBAL KEY-BINDINGS

(cam/define-keys nil
  "<A-return>" #'repeat
  "<C-s-M-down>" #'windmove-down
  "<C-s-M-left>" #'cam/windmove-left-or-other-frame
  "<C-s-M-return>" #'other-frame
  "<C-s-M-right>" #'cam/windmove-right-or-other-frame
  "<C-s-M-up>" #'windmove-up
  "<C-s-left>" #'next-buffer
  "<C-s-right>" #'previous-buffer
  "<H-S-left>" #'previous-buffer
  "<H-S-right>" #'next-buffer
  "<H-SPC>" #'other-frame
  "<H-down>" #'windmove-down
  "<H-left>" #'cam/windmove-left-or-other-frame
  "<H-return>" #'mc/mark-next-lines
  "<H-right>" #'cam/windmove-right-or-other-frame
  "<H-up>" #'windmove-up
  "<end>" #'ace-jump-buffer
  "<escape>" #'ace-jump-mode
  "<f10>" #'cam/switch-to-nav-buffer-other-window ; Jump to a nav buffer. F10 replaces menu-bar-open, which lets you browse menu from a buffer
  "<f11>" #'paredit-mode                          ; F11 is now global key for paredit-mode
  "<f12> b" #'cam/bing-search
  "<f12> s" #'cam/stackoverflow-search
  "<f13>" #'cam/popup-init-file-menu
  "<f2>" #'helm-swoop                             ; nice Helm search
  "<f9>" #'whitespace-mode
  "<home>" #'ace-jump-mode
  "<insert>" nil                                  ; disable stupid insert key TODO maybe use as a prefix to insert something useful
  "<next>" #'helm-buffers-list
  "<prior>" #'ace-jump-line-mode
  "<scroll>" #'cam/popup-init-file-menu           ; Windows only
  "A-;" #'loccur                                  ; activate loccur-mode (prompt for word/regex)
  "A-<tab>" #'ace-jump-buffer
  "A-H-;" #'loccur-previous-match                 ; jump batch to previous loccur search
  "A-H-b" #'bm-show-all                           ; disable minimize emacs
  "A-b" #'bm-toggle                               ; Toggle visual bookmark on this line
  "A-n" #'bm-next
  "A-p" #'bm-previous
  "A-r h" #'rotate:even-horizontal
  "A-r l" #'rotate-layout
  "A-r t" #'rotate:tiled
  "A-r v" #'rotate:even-vertical
  "A-r w" #'rotate-window
  "C-;" #'cam/add-semicolon-to-eol
  "C-=" #'magit-status
  "C-H-a" #'mc/mark-all-like-this
  "C-H-e" #'mc/edit-lines
  "C-M-:" #'eval-print-last-sexp
  "C-M-;" #'cam/comment-current-line
  "C-M-S-k" #'backward-kill-sexp                  ; Kill sexp before current position
  "C-M-y" #'browse-kill-ring
  "C-S-k" #'cam/backward-kill-line
  "C-c e" #'eval-and-replace                      ; eval previous elisp expression at point, replace with results
  "C-h m" #'discover-my-major                     ; more useful than the default help w/ C-h m
  "C-v" #'yank                                    ; yank instead of whatever it usually does
  "C-x C-b" #'helm-buffers-list                   ; this is (seemingly) better than buffer-menu or ibuffer
  "C-x C-d" #'ido-dired                           ; dired instead of list directory
  "C-x C-g" #'keyboard-quit                       ; Quit commands that I started typing with C-x
  "C-x C-r" #'recentf-open-files                  ; display recent files (overrides open file in read-only mode)
  "C-x C-z" nil                                   ; disable minimize emacs
  "C-x b" #'helm-buffers-list
  "C-x k" #'kill-this-buffer                      ; kill-this-buffer instead of kill-buffer (prompts for which buffer)
  "C-x o" #'ace-window                            ; override default other-buffer; this is much more useful
  "C-x r r" #'register-list                       ; overrides copy-rectangle-to-register, which I don't think I will ever use
  "C-x u" nil                                     ; disable emacs default keybinding for undo, use C-z instead
  "C-x z" nil                                     ; disable minimize emacs
  "C-z" nil                                       ; disable minimize emacs
  "H-;" #'loccur-current                          ; folder current buffer to lines containing the current word
  "H-A" #'mc/mark-previous-like-this
  "H-E" #'mc/mark-next-like-this                  ; Apparently Insert = Hyper on OS X WHEN USED IN COMBINATION WITH OTHER MODIFIER KEYS!
  "H-h" #'highlight-symbol-at-point
  "H-k" #'kill-this-buffer
  "M-RET" #'cam/insert-newline-below
  "M-`" #'cam/projectile-recentf
  "M-j" #'cam/join-next-line
  "M-x" #'helm-M-x
  "S-<f10>" #'nav                                 ; Open nav buffer  ;; "M-x" #'smex                                    ; smex is IDO-mode like M-x behavior
  "s-Z" #'undo-tree-redo
  "s-[" #'cam/force-unindent-region
  "s-]" #'cam/force-indent-region
  "s-b" #'balance-windows
  "s-f" #'ftf-grepsource
  "s-o" #'ftf-find-file
  "s-y" #'undo-tree-redo
  #'dabbrev-expand #'hippie-expand                ; remap dabbrev-expand bindings (M-/) to hippie-expand
  "s-g" #'ace-jump-mode
  "s-l" #'ace-jump-line-mode)


(defvar cam/init-files
  (-filter 'cam/is-init-file-p
           (cons "~/.emacs.d/init.el"
                 (directory-files "~/.emacs.d/lisp"
                                  t ; return file's absolute (full) name
                                  "^[^#.].*.el$")))
  "All the Emacs Lisp init files in my ~/.emacs.d directory.")


;;;; CREATE A MENU TO EASILY JUMP TO AN INIT FILE FOR EDITING

(easy-menu-define cam/init-file-menu global-map "Edit init file.."
  (cons "Edit Init File"
        (mapcar #'cam/menu-edit-init-file
                cam/init-files)))


;;;; RECOMPILE .EL FILES IN .EMACS.D AS NEEDED

(let ((byte-compile-dynamic t))                   ; compile function bodies so they load lazily
  (mapc (lambda (file)
          (byte-recompile-file file
                               nil                ; don't force recompile
                               0))                ; recompile even if there's no .elc file
        cam/init-files))


;;;; LOAD INIT FILES

(defun cam/safe-require (init-file)
  "Attempt to safely require INIT-FILE, or show any errors in a new buffer and jump to INIT-FILE."
  (condition-case err
      (require init-file)
    (error (warn "%s" (error-message-string err))
           (switch-to-buffer "*Warnings*")
           (delete-other-windows)
           (split-window-below)
           (find-file (concat "~/.emacs.d/lisp/" (symbol-name init-file) ".el")))))

(cam/safe-require 'theme-init)
(cam/safe-require 'sandbox)


;;;; LOAD CONFIG FILES FOR MINOR MODES / NON-EDITING MODES

(mapc (lambda (package-name)
        (eval-after-load package-name
          `(cam/safe-require ',(intern (concat package-name "-init")))))
      '("auto-complete"
        "company"
        "dired"
        "editorconfig"
        "find-things-fast"
        "git-timemachine"
        "ido"
        "magit"
        "multiple-cursors"))

;;; ### LOAD CONFIG FOR LANGUAGE-SPECIFIC MAJOR MODES

(add-to-list 'auto-mode-alist '("\.js$" . js3-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . django-mode))
(add-to-list 'auto-mode-alist '("\\.pl\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("python" . django-mode))

(mapc (-lambda ((file . requirement))
        (eval-after-load file `(cam/safe-require ,requirement)))
      '(("cc-mode" . 'cpp-init)
        ("cc-mode" . 'objc-init)
        ("clojure-mode" . 'clojure-init)
        ("cperl-mode" . 'perl-init)
        ("django-mode" . 'python-init)
        ("elisp-mode" . 'elisp-init)
        ("erlang-mode" . 'erlang-init)
        ("ielm" . 'elisp-init)
        ("js3-mode" . 'js-init)
        ("json-mode" . 'json-init)
        ("lisp-mode" . 'common-lisp-init)
        ("markdown-mode" . 'markdown-init)
        ("nxml-mode" . 'html-init)
        ("org" . 'org-init)
        ("ruby-mode" . 'ruby-init)))


;;; HACKs

(blink-cursor-mode -1) ; doesn't seem to work if we try to do it before loading theme-init (?)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote
    ((eval progn
           (define-clojure-indent
             (api-let 2)
             (auto-parse 1)
             (catch-api-exceptions 0)
             (context 2)
             (expect 1)
             (expect-eval-actual-first 1)
             (expect-let 1)
             (ins 1)
             (let-400 1)
             (let-404 1)
             (match 1)
             (match-$ 1)
             (macrolet 1)
             (org-perms-case 1)
             (upd 2)
             (with-credentials 1)))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
