;; -*- comment-column: 50; -*-

;;; init -- General Setup
;;; Commentary:
;;; Code:

;;;; DISABLE MENU/SCROLLBAR/TOOLBAR ASAP SO THEY DON'T FLASH

(mapc (lambda (mode)
        (when (boundp mode)
          (funcall mode -1)))
      '(menu-bar-mode
        scroll-bar-mode
        tool-bar-mode))
(toggle-frame-maximized)

(setq-default default-frame-alist nil)
;; (setq-default default-frame-alist
;;   '((fullscreen . maximized)))                    ; no fringes, no scroll bar


;;;; LOAD PACKAGES

(setq load-prefer-newer t)                        ; Load from .el files if they are newer than matching .elc files

(nconc load-path '("~/.emacs.d/lisp/"))


;;;; PACKAGES TO ALWAYS REQUIRE ON LAUNCH

(mapc #'require '(cl                              ; YUP
                  package-init                    ; needs to be loaded before we can load ELPA packages like dash or powerline
                  dash                            ; load this next so cam-functions can build on it
                  dash-functional                 ; TODO - should we just load "s" and "f" here too ?
                  noflet
                  cam-macros
                  cam-functions
                  powerline
                  powerline-evil
                  saveplace))


;;;; SETUP AUTOLOADS FOR FUNCTIONS THAT NEED IT


(cam/setup-autoloads
  ("ace-jump-buffer" #'ace-jump-same-mode-buffers)
  ("bm" #'bm-show-all)
  ("bytecomp" #'byte-recompile-file)
  ("find-things-fast" #'ftf-find-file #'ftf-grepsource)
  ("loccur" #'loccur #'loccur-current #'loccur-previous-match)
  ("highlight-error-keywords" #'highlight-error-keywords-mode)
  ("multiple-cursors" #'mc/mark-all-like-this #'mc/edit-lines #'mc/mark-previous-like-this #'mc/mark-next-like-this)
  ("s" #'s-replace))


;;;; GLOBALLY DISABLED MINOR MODES

(cam/disable-minor-modes
  blink-cursor-mode                               ; disable blinking cursor
  indent-tabs-mode                                ; disable indentation w/ tabs
  line-number-mode                                ; line numbers on the modeline
  set-fringe-mode                                 ; disable fringes
  )

;;;; GLOBALLY ENABLED MINOR MODES

(cam/suppress-messages
 (cam/enable-minor-modes
   delete-selection-mode                           ; typing will delete selected text
   electric-pair-mode
   evil-mode
   global-ace-isearch-mode
   global-auto-revert-mode
   global-diff-hl-mode                            ; Shows lines that have changed since last VC commit in the fringe
   global-hl-line-mode
   global-undo-tree-mode
   ido-mode
   ido-ubiquitous-mode
   ido-everywhere
   ido-vertical-mode
   flx-ido-mode                                    ; fuzzy matching for ido
   (rainbow-mode . nil)                            ; colorize strings that represent colors e.g. #00FFFF
   recentf-mode
   savehist-mode                                   ; save minibuffer history periodically
   show-paren-mode                                 ; highlight matching parens
   (undo-tree-mode . nil)
   winner-mode))


;;;; GLOBAL HOOKS

(add-hook 'before-save-hook
  (lambda ()
    (delete-trailing-whitespace)
    (set-buffer-file-coding-system 'utf-8-auto-unix)))

(add-hook 'after-save-hook
  (lambda ()
    (executable-make-buffer-file-executable-if-script-p) ; if we're saving a script, give it permissions to execute

    ;; show a AngryPoliceCaptain.com quote
    (unless (or (active-minibuffer-window)
                (minibufferp (current-buffer)))
      (angry-police-captain))
    ))

(add-hook 'emacs-startup-hook
          (lambda ()
            (kill-buffer "*scratch*")))

(add-hook 'dired-mode-hook
  (lambda ()
    (dired-hide-details-mode 1)))

(add-hook 'after-change-major-mode-hook
          (lambda ()
            (cam/enable-minor-modes
              highlight-error-keywords-mode
              rainbow-delimiters-mode
              (rainbow-mode . nil)))
          t)


;;;; GLOBAL EVAL-AFTER-LOADS

(cam/eval-after-load "dired"
  (require 'dired+)
  (require 'dired-x)                              ; things like C-x C-j for dired-jump
  (cam/suppress-messages
   (toggle-diredp-find-file-reuse-dir t))         ; reuse dired buffers
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'always))

;; Install editorconfig via homebrew if possible
(cam/eval-after-load "editorconfig"
  (unless (string-match "^EditorConfig" (shell-command-to-string "editorconfig --version"))
    (warn "EditorConfig is not installed. This is needed by editorconfig package.")
    (when (= system-type
           "darwin")
        (warn "Attempting to install via 'brew install editorconfig'...")
        (call-process-shell-command "brew install editorconfig" nil nil))))

(eval-after-load "multiple-cursors"
  '(multiple-cursors-mode 1))

(eval-after-load "magit"
  '(progn
     (cam/enable-minor-modes
       ;; (global-magit-wip-save-mode t)          ; TODO - investigate this - automatically create a work-in-progress ref whenever saving a file under VC
       (magit-auto-revert-mode . nil))              ; auto-revert buffers that change on disk as result of magit command
     (defadvice magit-status (after magit-status-show-help activate)
       (magit-key-mode-popup-dispatch)            ; show help when showing magit-status
       (call-interactively #'other-window))))     ; switch back to magit status window

(eval-after-load "find-things-fast"
  '(nconc ftf-filetypes '("*.clj"                 ; extra file types to search for/through when using find-things-fast
                          "*.el"
                          "*.html"
                          "*.js")))

(eval-after-load "company"                        ; shorter autocomplete delay w/ company
  '(setq company-idle-delay 0.01                  ; default is 0.5
         company-minimum-prefix-length 1))        ; default is 3

(eval-after-load "git-timemachine"
  '(defadvice git-timemachine (around git-timemachine-split-fullscreen activate)
     "Run git-timemachine for the current buffer in a special temporary fullscreen session"
     (window-configuration-to-register :git-timemachine-fullscreen-window-config)
     (delete-other-windows)
     (split-window-right)
     (call-interactively #'other-window)
     ad-do-it
     (defadvice git-timemachine-quit (after git-timemachine-fullscreen-quit activate)
       (message "DONE <3")
       (jump-to-register :git-timemachine-fullscreen-window-config)
       (advice-remove #'git-timemachine-quit #'git-timemachine-fullscreen-quit))))


;;;; GENERAL SETTINGS

(prefer-coding-system 'utf-8-auto-unix)

(ansi-color-for-comint-mode-on)                   ; Works better in Terminal or something like that
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(midnight-delay-set 'midnight-delay 10)           ; Have to use this function to set midnight-delay

(setq
    auto-revert-verbose nil
    auto-window-vscroll nil                       ; don't 'automatically adjust window to view tall lines'
    bm-cycle-all-buffers t                        ; visual bookmarks bm-next and bm-previous should cycle all buffers
    clean-buffer-list-delay-special 30
    echo-keystrokes 0.1                           ; shorter delay before showing keystrokes in progress
    global-auto-revert-non-file-buffers t         ; also refresh dired but be quiet about it
    inhibit-splash-screen t
    inhibit-startup-screen t
    locale-coding-system 'utf-8-auto-unix
    gc-cons-threshold (* 1024 1024 1024 4)        ; number of bytes of consing before garbage collection, default is ~800k, use 4GB instead
    mouse-wheel-scroll-amount '(1 ((shift) . 1 ))
    nav-width 30                                  ; nav should be 30 chars wide (default is 18)
    ns-right-command-modifier 'hyper
    ns-right-control-modifier 'hyper
    ns-right-option-modifier 'alt
    recentf-max-menu-items 50
    redisplay-dont-pause t                        ; don't pause screen drawing whenever input is detected - causes screen tearning, unneccessary
    require-final-newline t                       ; add final newline on save
    revert-without-query '(".*")                  ; disable revert-buffer confirmation prompts
    scroll-margin 1
    select-enable-clipboard t                     ; Use the clipboard in addition to emacs kill ring
    w32-apps-modifier 'hyper
    w32-lwindow-modifier 'super
    w32-pass-lwindow-to-system nil
    w32-rwindow-modifier 'alt
    whitespace-line-column 200                    ; don't highlight lines in whitespace mode unless they're REALLY giant. (default is 80)
    visible-bell t
 )

(setq-default
    save-place t                                  ; save current position of point when killing a buffer; restore when file is opened
    )

(fset 'yes-or-no-p 'y-or-n-p)                     ; prompt for y/n instead of yes/no


;;;; COMMANDS TO ENABLE

(mapc (lambda (fn)
        (put fn 'disabled nil))
      '(downcase-region
        upcase-region))


;;;; COMMANDS TO ALWAYS RUN FULLSCREEN

(mapc (lambda (args) (eval `(cam/run-fullscreen ,@args)))
      '(("magit" magit-status)
        ("package" list-packages package-list-packages package-list-packages-no-fetch)))


;;;; EVIL CONFIG (CURSOR COLORS IN THEME-INIT.EL)

(cam/define-keys evil-normal-state-map
  "<escape>" 'evil-emacs-state
  "C-z" 'evil-emacs-state)                        ; instead of minimizing Emacs

(setq evil-default-state 'emacs)
(global-evil-matchit-mode 1) ; WTF does this do? https://github.com/redguardtoo/evil-matchit

;; Swoop on calls to any Evil states besides normal and emacs and redirect them to emacs
(mapc (-rpartial 'fset 'evil-emacs-state)
      '(evil-insert-state
        evil-motion-state
        ;; evil-operator-state
        evil-replace-state
        evil-visual-state))

(defadvice evil-normal-state (after cam/enable-rel-line-nums-for-normal-mode activate)
  "Switch to relative-line-numbers mode after entering evil normal mode."
  (interactive)
  (relative-line-numbers-mode 1))

(defadvice evil-emacs-state (after cam/disable-rel-line-nums-for-emacs-mode activate)
  "Switch relative-line-numbers-mode off when entering Emacs mode."
  (interactive)
  (relative-line-numbers-mode -1)
  (linum-mode 1))


;;;; GLOBAL KEY-BINDINGS

(cam/define-keys nil
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
  "<escape>" #'evil-normal-state
  "<f10>" #'cam/switch-to-nav-buffer-other-window  ; Jump to a nav buffer. F10 replaces menu-bar-open, which lets you browse menu from a buffer
  "<f11>" #'paredit-mode                           ; F11 is now global key for paredit-mode
  "<f12> b" #'cam/bing-search
  "<f12> s" #'cam/stackoverflow-search
  "<f13>" #'cam/popup-cam-menu
  "<f2>" #'helm-swoop                             ; (lambda () (interactive) (call-interactively #'helm-swoop))
  "<f9>" #'whitespace-mode
  "<home>" #'ace-jump-mode
  "<insert>" nil                                    ; disable stupid insert key TODO maybe use as a prefix to insert something useful
  "<next>" #'helm-buffers-list
  "<prior>" #'ace-jump-line-mode
  "<scroll>" #'cam/popup-cam-menu                 ; windows only
  "A-;" #'loccur                                   ; activate loccur-mode (prompt for word/regex)
  "A-<tab>" #'ace-jump-buffer
  "A-H-;" #'loccur-previous-match                  ; jump batch to previous loccur search
  "A-H-b" #'bm-show-all                            ; disable minimize emacs
  "A-b" #'bm-toggle                                ; Toggle visual bookmark on this line
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
  "C-c e" #'eval-and-replace                         ; eval previous elisp expression at point, replace with results
  "C-h m" #'discover-my-major                     ; more useful than the default help w/ C-h m
  "C-v" #'yank                                     ; C-v -> yank instead of whatever it usually does
  "C-x C-b" #'buffer-menu                          ;  C-x C-b shows buffer menu
  "C-x C-d" #'ido-dired                            ; C-x C-d -> dired instead of list directory
  "C-x C-g" #'keyboard-quit                        ; Quit commands that I started typing with C-x
  "C-x C-r" #'recentf-open-files                   ; C-x C-r -> display recent files (overrides open file in read-only mode)
  "C-x C-z" nil                                     ; disable minimize emacs
  "C-x k" #'kill-this-buffer                       ; kill-this-buffer instead of kill-buffer (prompts for which buffer)
  "C-x o" #'ace-window                            ; override default other-buffer; this is much more useful
  "C-x r r" #'register-list                       ; overrides copy-rectangle-to-register, which I don't think I will ever use
  "C-x u" nil                                       ; disable emacs default keybinding for undo, use C-z instead
  "C-x z" nil                                       ; disable minimize emacs
  "H-;" #'loccur-current                           ; folder current buffer to lines containing the current word
  "H-A" #'mc/mark-previous-like-this
  "H-E" #'mc/mark-next-like-this                   ; Apparently Insert = Hyper on OS X WHEN USED IN COMBINATION WITH OTHER MODIFIER KEYS!
  "H-h" #'highlight-symbol-at-point
  "H-k" #'kill-this-buffer
  "M-RET" #'cam/insert-newline-below
  "M-`" #'cam/projectile-recentf
  "M-j" #'cam/join-next-line
  "M-x" #'smex                                     ; smex is IDO-mode like M-x behavior
  "S-<f10>" #'nav                                  ; Open nav buffer
  "s-Z" #'undo-tree-redo
  "s-[" #'cam/force-unindent-region
  "s-]" #'cam/force-indent-region
  "s-b" #'balance-windows
  "s-f" #'ftf-grepsource
  "s-o" #'ftf-find-file
  "s-y" #'undo-tree-redo
  )

(defvar cam/init-files
  (-filter 'cam/is-init-file-p
           (cons "~/.emacs.d/init.el"
                 (directory-files "~/.emacs.d/lisp"
                                  t ; return file's absolute (full) name
                                  "^[^#.].*.el$")))
  "All the Emacs Lisp init files in my ~/.emacs.d directory.")


;;;; CAM MENU

(easy-menu-define cam-menu global-map "Edit init file.."
  (cons "Edit Init File"
        (mapcar #'cam/menu-edit-init-file
                cam/init-files)))


;;;; RECOMPILE .EL FILES IN .EMACS.D AS NEEDED

(let ((byte-compile-dynamic t))
  (mapc (lambda (file)
          (byte-recompile-file file
                               nil                ; don't force recompile
                               0))                ; recompile even if there's no .elc file
        cam/init-files))


;;;; LOAD INIT FILES

(mapc (lambda (init-file)
        (condition-case err
            (require init-file)
            (error (warn "%s" (error-message-string err))
                   (switch-to-buffer "*Warnings*")
                   (delete-other-windows)
                   (split-window-below)
                   (find-file (concat "~/.emacs.d/lisp/" (symbol-name init-file))))))
      '(;; load elisp stuff first so we can at least fix errors in other files more easily
        lisp-init
        elisp-init
        common-lisp-init
        clojure-init
        cpp-init
        erlang-init
        html-init
        js-init
        json-init
        markdown-init
        objc-init
        org-init
        perl-init
        python-init
        ruby-init
        theme-init
        sandbox))

(provide 'init)
;;; init.el ends here
