(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/auto-complete-clang")

;; MELPA Package Source
(require 'package)
(require 'cl)
(mapc (lambda (l) (add-to-list 'package-archives l))
      '(("melpa" . "http://melpa.milkbox.net/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; install melpa/maramalade packages
(mapc (lambda (package)
	(when (not (package-installed-p package))
	  (package-install package)))
      '(ac-etags
        ace-jump-buffer
        ace-jump-mode
        auto-complete-clang-async
        clojure-mode
        clojure-mode-extra-font-locking
        clojurescript-mode
        company
        diminish
        dired+
        django-mode
        elisp-slime-nav
        elpy
        erlang
        evil
        find-things-fast
        flx-ido
        fuzzy
        highlight-parentheses
        highlight-symbol
        hl-sexp
        ido-ubiquitous
        ipython
        jquery-doc
        js2-mode
        json
        less-css-mode
        loccur
        magit
        markdown-mode
        multiple-cursors
        nav
        organic-green-theme
        outline-magic
        paredit
        projectile
        py-autopep8
        pydoc-info
        pyflakes
        pymacs
        python
        python-magic
        python-pep8
        rainbow-delimiters
        rainbow-mode
        rspec-mode
        ruby-block
        ruby-electric
        slime
        smartparens
        smex
        tabbar
        undo-tree
        xmlgen
        yasnippet))

(mapc 'require '( ;; evil
		 cam-functions
		 dired+
		 midnight
		 nav				  ; nav frame, better than speed bar
		 rainbow-delimiters
		 recentf
		 smex				  ; IDO-like completion for M-x
		 undo-tree
                 ace-jump-buffer
                 ace-jump-mode
                 bm
                 find-things-fast
                 flx-ido
                 highlight-symbol
                 loccur
                 magit
                 multiple-cursors
                 rainbow-mode
		 ))

;; (add-hook 'before-make-frame-hook 'turn-off-tool-bar)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; helper settings to make emacs work better from terminal
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(ansi-color-for-comint-mode-on)

(add-hook 'emacs-startup-hook
	  (lambda ()
            (progn
              (kill-buffer "*scratch*"))))

;; global minor modes
(global-rainbow-delimiters-mode 1)
(winner-mode 1)
(global-linum-mode 1)				  ; linum-mode is for line numbers on left
(line-number-mode 0) 				  ; line-number-mode is for line numbers on mode line
(column-number-mode 1)
(global-auto-revert-mode 1)			  ; auto-revert mode reload buffers when underlying file changes
(global-hl-line-mode 1)				  ; highlights the current line
;; (set-face-background 'hl-line "#222222")
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)                                  ; fuzzy-matching for ido
(recentf-mode 1)
(rainbow-mode 1)				  ; colorize strings that represent colors
(diminish 'rainbow-mode nil)
(global-undo-tree-mode 1)			  ; sane undo in emacs
(diminish 'undo-tree-mode nil)
(tool-bar-mode -1)				  ; disable the toolbar at top of screen
(scroll-bar-mode -1)				  ; disable scrollbar
(delete-selection-mode 1)			  ; Typing will overwrite selections
(toggle-diredp-find-file-reuse-dir 1)		  ; reuse dired buffer
(tabbar-mode 1)
(electric-pair-mode 1)
(multiple-cursors-mode 1)
;; (evil-mode 1)


(defun global-mode-setup ()
  "function to call when setting up any mode, e.g. minor modes that "
  (rainbow-mode 1)				  ; colorize strings that represent colors, e.g. "#aabbcc" or "blue"
  ;; highlight in bold yellow the words FIX. FIXME, TODO, HACK, REFACTOR, NOCOMMIT, DEPRECATED.
  (font-lock-add-keywords
    nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|DEPRECATED\\|NOCOMMIT\\)"
	1 font-lock-warning-face t))))

;; global settings
(setq query-replace-interactive t)		  ; Use last incremental seach regexp for query in regexp-query-replace
(prefer-coding-system 'utf-8-auto-unix)
(setq inhibit-startup-screen t
      inhibit-splash-screen t)
(setq recentf-max-menu-items 20)

(set-frame-font "Menlo-11")
;; (set-frame-font "Menlo Regular-11")
;; (set-frame-font "Source Code Pro-11")

(setq
 scroll-step 1                                    ; prevent Emacs from getting into weird state where it insists on centering the buffer on the cursor
  -conservatively 9999
  -up-aggresively 0.01
 scroll-down-aggresively 0.01
 auto-window-vscroll nil                          ; don't 'automatically adjust window to view tall lines'
 mouse-wheel-scroll-amount '(1 ((shift) . 1 ))
 scroll-margin 1
 whitespace-line-column 200                       ; don't highlight lines in whitespace mode unless they're REALLY giant. (default is 80)
 require-final-newline t                          ; add final newline on save
 initial-frame-alist (quote ((fullscreen . maximized)))
 bm-cycle-all-buffers t                           ; visual bookmarks bm-next and bm-previous should cycle all buffers
 )
(setq global-auto-revert-non-file-buffers t)	  ; also refresh dired but be quiet about it
(setq auto-revert-verbose nil)
(setq ac-delay 0.1)				  ; shorter delay before showing completions. Default is 0.1.
(setq ac-auto-show-menu t)			  ; automatically show menu
(setq ac-quick-help-delay 0.5)			  ; shorter delay before showing quick help. Default is 1.5, 0 makes it crash
(setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
(setq midnight-period 10)			  ; every 10 secs run clean-buffer-list, which kills *Help*, *Buffer List*, *Apropos*, etc buffers that haven't been visited recently
(midnight-delay-set 'midnight-delay 10)		  ; Have to use this function to set midnight-delay
(setq clean-buffer-list-delay-special 30)
(setq clean-buffer-list-delay-general 0.02)	  ; Kill ANY buffer that hasn't been used in the half-hour (ish) - param is in day
(setq clean-buffer-list-kill-regexps		  ; Remove all starred buffers not currently in use
      '("\\*.*\\*"))
(setq nav-width 30)				  ; nav should be 30 chars wide (default is 18)
(nav-disable-overeager-window-splitting)	  ; turn off automatic splitting of frames when opening files in a large frame (?)
(set-default 'indent-tabs-mode nil) 		  ; Indentation can insert tabs if this is non-nil
(setq x-select-enable-clipboard t)		  ; Use the clipboard in addition to emacs kill ring

;; custom key bindings
(define-keys nil
  '(("C-x u" nil) 				  ; disable emacs default keybinding for undo, use C-z instead
    ("C-x C-b" buffer-menu)			  ; C-x C-b shows buffer menu
    ("C-x C-r" recentf-open-files)		  ; C-x C-r -> display recent files (overrides open file in read-only mode)
    ("C-x C-d" ido-dired)			  ; C-x C-d -> dired instead of list directory
    ("C-M-S-k" backward-kill-sexp)		  ; C-M-S-k is backward-kill-sexp (kill-sexp is (C-M-k))
    ("C-S-k" backward-kill-line)
    ("C-M-y" popup-yank-menu)
    ("C-x k" kill-this-buffer)			  ; kill-this-buffer instead of kill-buffer (prompts for which buffer)
    ("<f9>" whitespace-mode)
    ("<f10>" (lambda ()				  ; Jump to a nav buffer. F10 replaces menu-bar-open, which lets you browse menu from a buffer
	       (interactive)
	       (switch-to-buffer-other-window "*nav*")))
    ("S-<f10>" nav)				  ; Open nav buffer
    ("<f11>" paredit-mode)			  ; F11 is now global key for paredit-mode
    ("<f12> s" stackoverflow-search)
    ("<f12> b" bing-search)
    ;; ("C-z" undo)     ; C-z -> undo instead of minimize emacs ; C-z used by evil-mode to switch to emacs state
    ("C-v" yank)				  ; C-v -> yank instead of whatever it usually does
    ;; ("<escape>" keyboard-escape-quit)
    ("<insert>" nil)			          ; disable stupid insert key TODO maybe use as a prefix to insert something useful
    ("H-E" mc/mark-next-like-this)                ; Apparently Insert = Hyper on OS X WHEN USED IN COMBINATION WITH OTHER MODIFIER KEYS!
    ("H-A" mc/mark-previous-like-this)
    ("C-H-a" mc/mark-all-like-this)
    ("C-H-e" mc/edit-lines)
    ("H-h" highlight-symbol-at-point)
    ("C-c e" eval-and-replace)		     	  ; eval previous elisp expression at point, replace with results
    ("M-j" join-next-line)
    ("C-x z")				      	  ; disable minimize emacs
    ("<f13>" popup-cam-menu)
    ("s-]" force-indent-region)
    ("s-[" force-unindent-region)
    ("s-o" ftf-find-file)
    ("s-f" ftf-grepsource)
    ("<escape>" ace-jump-mode)
    ("A-<tab>" ace-jump-buffer)
    ("C-x C-g" keyboard-quit)                     ; Quit commands that I started typing with C-x
    ("M-x" smex)				  ; smex is IDO-mode like M-x behavior
    ;; ("M-ESC" helm-mini)
    ("H-;" loccur-current)                        ; folder current buffer to lines containing the current word
    ("A-;" loccur)                                ; activate loccur-mode (prompt for word/regex)
    ("A-H-;" loccur-previous-match)               ; jump batch to previous loccur search
    ("s-b" balance-windows)
    ("A-b" bm-toggle)                             ; Toggle visual bookmark on this line
    ("A-H-b" bm-show-all)                         ; Show all visual bookmarks in all files
    ("A-n" bm-next)
    ("A-p" bm-previous)
    ))


;; THE HOLY GRAIL <3
(setq ns-right-control-modifier 'hyper
      ns-right-command-modifier 'hyper
      ns-right-option-modifier 'alt)

(setq w32-apps-modifier 'hyper)
(define-keys nil
  '(("<H-up>" windmove-up)
     ("<H-left>" windmove-left)
    ("<H-right>" windmove-right)
    ("<H-down>" windmove-down)
    ("<C-s-M-left>" windmove-left)
    ("<C-s-M-right>" windmove-right)
    ("<C-s-right>" previous-buffer)
    ("<C-s-left>" next-buffer)
    ("<C-s-M-up>" windmove-up)
    ("<C-s-M-down>" windmove-down)
    ("<H-SPC>" other-frame)
    ("<C-s-M-return>" other-frame)
    ("<H-S-left>" previous-buffer)
    ("<H-S-right>" next-buffer)
    ("H-k" kill-this-buffer)))

(defun menu-edit-file (str f)
  (vector str (list 'lambda '() '(interactive) (list 'find-file f))))

(defun menu-edit-init-file (f)
  (menu-edit-file (concat "Edit " f) (concat "~/.emacs.d/" f)))

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
		      '("init.el"
			"cam-functions.el"
			"lisp-init.el"
			"clojure-init.el"
			"elisp-init.el"
			"js-init.el"
			"markdown-init.el"
			"ruby-init.el"
			"objc-init.el"
			"erlang-init.el"
			"python-init.el"
			"html-init.el"
			"cpp-init.el")))))

(mapc 'require '(lisp-init
		 clojure-init
		 elisp-init
		 org-init
		 js-init
		 markdown-init
		 ruby-init
		 objc-init
		 python-init
		 erlang-init
		 html-init
		 cpp-init))

(custom-set-variables
 '(custom-enabled-themes (quote (organic-green)))
 '(custom-safe-themes (quote ("1ef7df153ee59ef210acf0060073cd98e4992c9014b4fc7766243a3cb56cc6e4" default))))
(custom-set-faces)
