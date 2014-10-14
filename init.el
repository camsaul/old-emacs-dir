;; -*- comment-column: 50; -*-

; Disable menu/scrollbar/toolbar first so they don't flash
(mapc (lambda (mode)
        (funcall mode -1))
      '(menu-bar-mode
        scroll-bar-mode
        tool-bar-mode))

(nconc load-path '("~/.emacs.d/"
                   "~/.emacs.d/auto-complete-clang"))

(require 'package-init)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'emacs-startup-hook
          (lambda ()
            (kill-buffer "*scratch*")
            (setup-powerline)                     ; needs to be ran as part of startup hook or doesn't work (?)
            ))

;; minor modes to disable
(cam-disable-minor-modes
  line-number-mode                                ; line numbers on the modeline
  set-fringe-mode                                 ; disable fringes
  )

;; minor modes to enable
(cam-enable-minor-modes
  delete-selection-mode                           ; typing will delete selected text
  electric-pair-mode
  evil-mode
  flx-ido-mode                                    ; fuzzy matching for ido
  global-auto-revert-mode
  global-hl-line-mode
  global-undo-tree-mode
  ido-everywhere
  ido-mode
  multiple-cursors-mode
  (rainbow-mode . nil)                            ; colorize strings that represent colors e.g. #00FFFF
  recentf-mode
  show-paren-mode                                 ; highlight matching parens
  winner-mode
  )

;; minor modes to diminish
(cam-diminish-modes
 rainbow-mode
 undo-tree-mode)

;; Enable other minor modes
(toggle-diredp-find-file-reuse-dir 1)             ; reuse dired buffer
(dired-details-install)                           ; enable dired details (hides file size, etc in dired by default)

;; function to call when setting up a major mode
;; TODO - what hook to add this to?
(defun global-mode-setup ()
  "Function that should be called to do some extra customization when setting up any major mode."
  (cam-enable-minor-modes
    rainbow-delimiters-mode
    rainbow-mode)                                 ; colorize strings that represent colors, e.g. "#aabbcc" or "blue"
  ;; highlight these words in bold warning face
  (font-lock-add-keywords
    nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|DEPRECATED\\|NOCOMMIT\\)"
        1 font-lock-warning-face t))))


;; general settings - etc
(prefer-coding-system 'utf-8-auto-unix)
(set-terminal-coding-system 'utf-8)               ; work better from Terminal
(set-keyboard-coding-system 'utf-8)
(ansi-color-for-comint-mode-on)
(midnight-delay-set 'midnight-delay 10)           ; Have to use this function to set midnight-delay
(set-default 'indent-tabs-mode nil)               ; Indentation can insert tabs if this is non-nil

;; general settings w/ setq
(setq
 ac-auto-show-menu t                              ; automatically show menu
 ac-quick-help-delay 0.5                          ; shorter delay before showing quick help. Default is 1.5, 0 makes it crash
 ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers)
 auto-revert-verbose nil
 auto-window-vscroll nil                          ; don't 'automatically adjust window to view tall lines'
 bm-cycle-all-buffers t                           ; visual bookmarks bm-next and bm-previous should cycle all buffers
 clean-buffer-list-delay-special 30
 ;; clean-buffer-list-kill-regexps                        ; Remove all starred buffers not currently in use
 ftf-filetypes (append ftf-filetypes '("*.js"))   ; Additional file types that should be recognized by ftf-grepsource (Super + F)
 global-auto-revert-non-file-buffers t            ; also refresh dired but be quiet about it
 inhibit-splash-screen t
 inhibit-startup-screen t
 initial-frame-alist (quote ((fullscreen . maximized)))
 mouse-wheel-scroll-amount '(1 ((shift) . 1 ))
 nav-width 30                                     ; nav should be 30 chars wide (default is 18)
 ns-right-command-modifier 'hyper
 ns-right-control-modifier 'hyper
 ns-right-option-modifier 'alt
 query-replace-interactive t                      ; Use last incremental seach regexp for query in regexp-query-replace
 recentf-max-menu-items 20
 require-final-newline t                          ; add final newline on save
 scroll-margin 1
 w32-apps-modifier 'hyper
 w32-lwindow-modifier 'super
 w32-pass-lwindow-to-system nil
 w32-rwindow-modifier 'alt
 whitespace-line-column 200                       ; don't highlight lines in whitespace mode unless they're REALLY giant. (default is 80)
 x-select-enable-clipboard t                              ; Use the clipboard in addition to emacs kill ring
 )

;; custom key bindings
(define-keys nil
  '(
    ("<C-s-M-down>" windmove-down)
    ("<C-s-M-left>" windmove-left-or-other-frame)
    ("<C-s-M-return>" other-frame)
    ("<escape>" evil-normal-state)
    ("<f10>" switch-to-nav-buffer-other-window)   ; Jump to a nav buffer. F10 replaces menu-bar-open, which lets you browse menu from a buffer
    ("<f11>" paredit-mode)                        ; F11 is now global key for paredit-mode
    ("<f12> b" bing-search)
    ("<f12> s" stackoverflow-search)
    ("<f13>" popup-cam-menu)
    ("<scroll>" popup-cam-menu)                   ; windows only
    ("<f9>" whitespace-mode)
    ("<insert>" nil)                              ; disable stupid insert key TODO maybe use as a prefix to insert something useful
    ("C-H-a" mc/mark-all-like-this)
    ("C-H-e" mc/edit-lines)
    ("C-M-S-k" backward-kill-sexp)                ; C-M-S-k is backward-kill-sexp (kill-sexp is (C-M-k))
    ("C-M-y" popup-yank-menu)
    ("C-S-k" backward-kill-line)
    ("C-c e" eval-and-replace)                    ; eval previous elisp expression at point, replace with results
    ("C-v" yank)                                  ; C-v -> yank instead of whatever it usually does
    ("C-x C-b" buffer-menu)                       ; C-x C-b shows buffer menu
    ("C-x C-d" ido-dired)                         ; C-x C-d -> dired instead of list directory
    ("C-x C-r" recentf-open-files)                ; C-x C-r -> display recent files (overrides open file in read-only mode)
    ("C-x k" kill-this-buffer)                    ; kill-this-buffer instead of kill-buffer (prompts for which buffer)
    ("C-x u" nil)                                 ; disable emacs default keybinding for undo, use C-z instead
    ("C-x z")                                     ; disable minimize emacs
    ("C-x C-z")                                   ; disable minimize emacs
    ("H-A" mc/mark-previous-like-this)
    ("H-E" mc/mark-next-like-this)                ; Apparently Insert = Hyper on OS X WHEN USED IN COMBINATION WITH OTHER MODIFIER KEYS!
    ("H-h" highlight-symbol-at-point)
    ("M-j" join-next-line)
    ("S-<f10>" nav)                               ; Open nav buffer
    ("s-[" force-unindent-region)
    ("s-]" force-indent-region)
    ("s-b" balance-windows)
    ("s-f" ftf-grepsource)
    ("s-o" ftf-find-file)
    ("<C-s-M-right>" windmove-right-or-other-frame)
    ("<C-s-M-up>" windmove-up)
    ("<C-s-left>" next-buffer)
    ("<C-s-right>" previous-buffer)
    ("<H-S-left>" previous-buffer)
    ("<H-S-right>" next-buffer)
    ("<H-SPC>" other-frame)
    ("<H-down>" windmove-down)
    ("<H-left>" windmove-left-or-other-frame)
    ("<H-return>" mc/mark-next-lines)
    ("<H-right>" windmove-right-or-other-frame)
    ("<H-up>" windmove-up)
    ("A-;" loccur)                                ; activate loccur-mode (prompt for word/regex)
    ("A-<tab>" ace-jump-buffer)
    ("A-H-;" loccur-previous-match)               ; jump batch to previous loccur search
    ("A-H-b" bm-show-all)                         ; Show all visual bookmarks in all files
    ("A-b" bm-toggle)                             ; Toggle visual bookmark on this line
    ("A-n" bm-next)
    ("A-p" bm-previous)
    ("C-x C-g" keyboard-quit)                     ; Quit commands that I started typing with C-x
    ("H-;" loccur-current)                        ; folder current buffer to lines containing the current word
    ("H-k" kill-this-buffer)
    ("M-x" smex)                                  ; smex is IDO-mode like M-x behavior
    ))

(defvar init-files
  (directory-files "~/.emacs.d"
                   nil ; don't return file's full name
                   "^[^#.].*.el$")
  "All the Emacs Lisp init files in my ~/.emacs.d directory.")

;; put my custom stuff in a menu
(easy-menu-define cam-menu global-map "CAM :)"
  (list "CAM :)"
        ["Coffee House" coffee-house]
        ["Insert Lorem Ipsum" lorem-ipsum]
        '("Search"
          ["Bing Search" bing-search]
          ["ClojureDocs Search" clojure-docs-search]
          ["JavaDocs Search" javadocs-search]
          ["StackOverflow Search" stackoverflow-search])
        '("Reference"
          ["Paredit Cheatsheet" paredit-cheatsheet]
          ["Cloure Cheatsheet" clojure-cheatsheet])
        '("Modes"
          ["toggle-paredit-mode" paredit-mode]
          ["whitespace-mode" whitespace-mode])
        (cons "Edit Init File"
              (mapcar 'menu-edit-init-file
                      init-files))))


;; recompile init files as needed
(let ((byte-compile-dynamic t))
  (mapc (lambda (file)
          (byte-recompile-file file
                               nil                ; don't force recompile
                               0))                ; recompile even if there's no .elc file
        init-files))

;; load my various init files
(mapc 'require '(
                 clojure-init
                 cpp-init
                 elisp-init
                 erlang-init
                 html-init
                 js-init
                 markdown-init
                 objc-init
                 org-init
                 python-init
                 ruby-init
                 lisp-init
                 theme-init
                 ))
