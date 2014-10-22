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

;;;; LOAD PACKAGES

(setq load-prefer-newer t)                        ; Load from .el files if they are newer than matching .elc files

(nconc load-path '("~/.emacs.d/lisp/"))


;;;; PACKAGES TO ALWAYS REQUIRE ON LAUNCH

(mapc 'require '(cl                               ; YUP
                 package-init                     ; needs to be loaded before we can load ELPA packages like dash or powerline
                 dash                             ; load this next so cam-functions can build on it
                 cam-functions
                 powerline
                 powerline-evil))


;;;; SETUP AUTOLOADS FOR FUNCTIONS THAT NEED IT

(cam-setup-autoloads
  ("ace-jump-buffer" ace-jump-same-mode-buffers)
  ("bytecomp" byte-recompile-file)
  ("find-things-fast" ftf-find-file ftf-grepsource)
  ("loccur" loccur loccur-current loccur-previous-match)
  ("highlight-error-keywords" highlight-error-keywords-mode)
  ("multiple-cursors" mc/mark-all-like-this mc/edit-lines mc/mark-previous-like-this mc/mark-next-like-this))


;;;; GLOBALLY DISABLED MINOR MODES

(cam-disable-minor-modes
  indent-tabs-mode                                ; disable indentation w/ tabs
  line-number-mode                                ; line numbers on the modeline
  set-fringe-mode                                 ; disable fringes
  )

;;;; GLOBALLY ENABLED MINOR MODES

(cam-enable-minor-modes
  delete-selection-mode                           ; typing will delete selected text
  electric-pair-mode
  evil-mode
  flx-ido-mode                                    ; fuzzy matching for ido
  global-ace-isearch-mode
  global-auto-revert-mode
  global-hl-line-mode
  global-undo-tree-mode
  ido-everywhere
  ido-mode
  ido-ubiquitous-mode
  (rainbow-mode . nil)                            ; colorize strings that represent colors e.g. #00FFFF
  recentf-mode
  show-paren-mode                                 ; highlight matching parens
  (undo-tree-mode . nil)
  winner-mode)


;;;; GLOBAL HOOKS

(add-hook 'before-save-hook
  (lambda ()
    (delete-trailing-whitespace)
    (set-buffer-file-coding-system 'utf-8-auto-unix)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (kill-buffer "*scratch*")))


(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode 1)))

(add-hook 'after-change-major-mode-hook
          (lambda ()
            (cam-enable-minor-modes
              highlight-error-keywords-mode       ; TODO ! Not working
              rainbow-delimiters-mode
              (rainbow-mode . nil)))
          t)


;;;; GLOBAL EVAL-AFTER-LOADS

(cam/eval-after-load "dired"
  '(unless (featurep 'dired+)
     (require 'dired+))
  (toggle-diredp-find-file-reuse-dir t)) ; reuse dired buffers

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
  '(cam-enable-minor-modes
     ;; (global-magit-wip-save-mode t)            ; TODO - investigate this - automatically create a work-in-progress ref whenever saving a file under VC
     (magit-auto-revert-mode . nil)))             ; auto-revert buffers that change on disk as result of magit command

(eval-after-load "find-things-fast"
  '(nconc ftf-filetypes '("*.clj"
                          "*.el"
                          "*.js")))


;;;; GENERAL SETTINGS

(prefer-coding-system 'utf-8-auto-unix)

(set-terminal-coding-system 'utf-8)               ; work better from Terminal
(set-keyboard-coding-system 'utf-8)
(ansi-color-for-comint-mode-on)
(midnight-delay-set 'midnight-delay 10)           ; Have to use this function to set midnight-delay

(setq
 auto-revert-verbose nil
 auto-window-vscroll nil                          ; don't 'automatically adjust window to view tall lines'
 bm-cycle-all-buffers t                           ; visual bookmarks bm-next and bm-previous should cycle all buffers
 clean-buffer-list-delay-special 30
 global-auto-revert-non-file-buffers t            ; also refresh dired but be quiet about it
 inhibit-splash-screen t
 inhibit-startup-screen t
 initial-frame-alist (quote ((fullscreen . maximized)))
 mouse-wheel-scroll-amount '(1 ((shift) . 1 ))
 nav-width 30                                     ; nav should be 30 chars wide (default is 18)
 ns-right-command-modifier 'hyper
 ns-right-control-modifier 'hyper
 ns-right-option-modifier 'alt
 recentf-max-menu-items 20
 redisplay-dont-pause t                           ; don't pause screen drawing whenever input is detected - causes screen tearning, unneccessary
 require-final-newline t                          ; add final newline on save
 revert-without-query '(".*")                     ; disable revert-buffer confirmation prompts
 scroll-margin 1
 w32-apps-modifier 'hyper
 w32-lwindow-modifier 'super
 w32-pass-lwindow-to-system nil
 w32-rwindow-modifier 'alt
 whitespace-line-column 200                       ; don't highlight lines in whitespace mode unless they're REALLY giant. (default is 80)
 x-select-enable-clipboard t                      ; Use the clipboard in addition to emacs kill ring
 )

(fset 'yes-or-no-p 'y-or-n-p)                     ; prompt for y/n instead of yes/no

;; Commands to enable
(mapc (lambda (fn)
        (put fn 'disabled nil))
      '(downcase-region
        upcase-region))

;; Commands to always run fullscreen
(mapc (lambda (args) (eval `(cam/run-fullscreen ,@args)))
      '(("magit" magit-status)
        ("package" package-list-packages package-list-packages-no-fetch)))


;;;; EVIL CONFIG (CURSOR COLORS IN THEME-INIT.EL)

(cam/define-keys evil-normal-state-map
  "<escape>" 'evil-emacs-state)

(setq evil-default-state 'emacs)
(global-evil-matchit-mode 1) ; WTF does this do? https://github.com/redguardtoo/evil-matchit

(mapc (lambda (fn)
        (eval `(defadvice ,fn (around cam/intercept-evil-state activate)
                 "Intercept any switch to this evil state, and switch to Emacs state instead."
                 (interactive)
                 (evil-emacs-state))))
      '(evil-insert-state
        evil-motion-state
        evil-operator-state
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
  "<C-s-M-down>" 'windmove-down
  "<C-s-M-left>" 'windmove-left-or-other-frame
  "<C-s-M-return>" 'other-frame
  "<end>" 'ace-jump-buffer
  "<escape>" 'evil-normal-state
  "<f10>" 'switch-to-nav-buffer-other-window      ; Jump to a nav buffer. F10 replaces menu-bar-open, which lets you browse menu from a buffer
  "<f11>" 'paredit-mode                           ; F11 is now global key for paredit-mode
  "<f12> b" 'bing-search
  "<f12> s" 'stackoverflow-search
  "<f13>" 'popup-cam-menu
  "<home>" 'ace-jump-mode
  "<prior>" 'ace-jump-line-mode
  "<scroll>" 'popup-cam-menu                      ; windows only
  "<f9>" 'whitespace-mode
  "<insert>" nil                                  ; disable stupid insert key TODO maybe use as a prefix to insert something useful
  "C-H-a" 'mc/mark-all-like-this
  "C-H-e" 'mc/edit-lines
  "C-M-:" 'eval-print-last-sexp
  "C-M-S-k" 'backward-kill-sexp                   ; C-M-S-k is backward-kill-sexp (kill-sexp is (C-M-k))
  "C-M-y" 'popup-yank-menu
  "C-S-k" 'backward-kill-line
  "C-c e" 'eval-and-replace                       ; eval previous elisp expression at point, replace with results
  "C-v" 'yank                                     ;      ; C-v -> yank instead of whatever it usually does
  "C-x C-b" 'buffer-menu                          ;  C-x C-b shows buffer menu
  "C-x C-d" 'ido-dired                            ;  ; C-x C-d -> dired instead of list directory
  "C-x C-r" 'recentf-open-files                   ; C-x C-r -> display recent files (overrides open file in read-only mode)
  "C-x k" 'kill-this-buffer                       ; kill-this-buffer instead of kill-buffer (prompts for which buffer)
  "C-x u" nil                                     ; disable emacs default keybinding for undo, use C-z instead
  "C-x z" nil                                     ; disable minimize emacs
  "C-x C-z" nil                                   ; disable minimize emacs
  "H-A" 'mc/mark-previous-like-this
  "H-E" 'mc/mark-next-like-this                   ; Apparently Insert = Hyper on OS X WHEN USED IN COMBINATION WITH OTHER MODIFIER KEYS!
  "H-h" 'highlight-symbol-at-point
  "M-j" 'join-next-line
  "S-<f10>" 'nav                                  ; Open nav buffer
  "s-[" 'force-unindent-region
  "s-]" 'force-indent-region
  "s-b" 'balance-windows
  "s-f" 'ftf-grepsource
  "s-o" 'ftf-find-file
  "<C-s-M-right>" 'windmove-right-or-other-frame
  "<C-s-M-up>" 'windmove-up
  "<C-s-left>" 'next-buffer
  "<C-s-right>" 'previous-buffer
  "<H-S-left>" 'previous-buffer
  "<H-S-right>" 'next-buffer
  "<H-SPC>" 'other-frame
  "<H-down>" 'windmove-down
  "<H-left>" 'windmove-left-or-other-frame
  "<H-return>" 'mc/mark-next-lines
  "<H-right>" 'windmove-right-or-other-frame
  "<H-up>" 'windmove-up
  "A-;" 'loccur                                   ; activate loccur-mode (prompt for word/regex)
  "A-<tab>" 'ace-jump-buffer
  "A-H-;" 'loccur-previous-match                  ; jump batch to previous loccur search
  "A-H-b" 'bm-show-all                            ; disable minimize emacs
  "A-b" 'bm-toggle                                ; Toggle visual bookmark on this line
  "A-n" 'bm-next
  "A-p" 'bm-previous
  "C-x C-g" 'keyboard-quit                        ; Quit commands that I started typing with C-x
  "H-;" 'loccur-current                           ; folder current buffer to lines containing the current word
  "H-k" 'kill-this-buffer
  "M-x" 'smex                                     ; smex is IDO-mode like M-x behavior
  )

(defvar init-files
  (-filter 'cam/is-init-file-p
           (cons "~/.emacs.d/init.el"
                 (directory-files "~/.emacs.d/lisp"
                                  t ; return file's absolute (full) name
                                  "^[^#.].*.el$")))
  "All the Emacs Lisp init files in my ~/.emacs.d directory.")


;;;; CAM MENU

(easy-menu-define cam-menu global-map "Edit init file.."
  (cons "Edit Init File"
        (mapcar 'menu-edit-init-file
                init-files)))


;;;; RECOMPILE .EL FILES IN .EMACS.D AS NEEDED

(let ((byte-compile-dynamic t))
  (mapc (lambda (file)
          (byte-recompile-file file
                               nil                ; don't force recompile
                               0))                ; recompile even if there's no .elc file
        init-files))


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
        clojure-init
        cpp-init
        erlang-init
        html-init
        js-init
        json-init
        markdown-init
        objc-init
        org-init
        python-init
        ruby-init
        theme-init))


(provide 'init)
;;; init.el ends here
