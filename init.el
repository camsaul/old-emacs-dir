<<<<<<< HEAD
(add-to-list 'load-path "~/.emacs.d/") ; add this dir to the load path
(add-to-list 'load-path "~/.emacs.d/auto-complete-clang") ; add path for clang autocomplete
=======
;; add this dir to the load path
(add-to-list 'load-path "~/.emacs.d/")
>>>>>>> 9b1d0fc5605e70b1bc80bac48a00c846d2fdcc74

;; MELPA Package Source
(require 'package)
(require 'cl)
(mapc (lambda (l) (add-to-list 'package-archives l))
      '(("melpa" . "http://melpa.milkbox.net/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)
(when (not package-archive-contents)
	(package-refresh-contents))

(mapc (lambda (package)
	(when (not (package-installed-p package))
	  (package-install package)))
      '(clojure-mode clojure-test-mode nrepl ac-nrepl highlight-parentheses paredit markdown-mode
		     less-css-mode diminish rainbow-delimiters rainbow-mode hl-sexp fuzzy
		     json slime erlang python ipython xmlgen rspec-mode ruby-electric ruby-block
		     undo-tree evil nav))

(mapc 'require '(cam-functions 
		 recentf 
		 rainbow-delimiters
                 rainbow-mode
		 clojure-mode-ext ; <- TODO can't these be moded to clojure-init.el?
		 clojure-mode-slime
		 clojuredocs
		 midnight
		 undo-tree
		 evil
		 auto-complete-clang
		 nav ; nav frame, better than speed bar
		 ))

;; global minor modes
(global-rainbow-delimiters-mode 1)
(winner-mode 1)
(global-linum-mode 1) ; linum-mode is for line numbers on left
(line-number-mode 0) ; line-number-mode is for line numbers on mode line
(column-number-mode 1)
(global-auto-revert-mode 1) ; auto-revert mode reload buffers when underlying file changes
(global-hl-line-mode 1) ; highlights the current line
(set-face-background 'hl-line "#F0F0F0")
(ido-mode 1)
(recentf-mode 1)
(rainbow-mode 1)
(global-undo-tree-mode 1) ; sane undo in emacs
(diminish 'undo-tree-mode nil)
(evil-mode 1)
(tool-bar-mode -1) ; disable the toolbar at top of screen
(scroll-bar-mode -1) ; disable scrollbar

(defun global-mode-setup ()
  "function to call when setting up any mode, e.g. minor modes that "
  (rainbow-mode 1) ; colorize strings that represent colors, e.g. "#aabbcc" or "blue"
  ;; highlight in bold red the words FIX. FIXME, TODO, HACK, REFACTOR, NOCOMMIT.
  (font-lock-add-keywords
    nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
	1 font-lock-warning-face t))))

;; global settings
(setq query-replace-interactive t) ; Use last incremental seach regexp for query in regexp-query-replace
(setq make-backup-files nil) ; stop creating those backup~ files
(prefer-coding-system 'utf-8-auto-unix)
(setq inhibit-startup-screen t) ; inhibit startup screen
(setq inhibit-splash-screen t) ; inhibit splash screen
(add-hook 'emacs-startup-hook (lambda () (kill-buffer "*scratch*")))
(setq recentf-max-menu-items 20)
(set-frame-font (if (string-equal window-system "ns")
		    "Menlo Regular-11" ; use the Xcode font on OS X
		  "Source Code Pro-10")) ; Source Code Pro open-source font by Adobe. https://github.com/abobe/Source-Code-Pro
(setq ac-delay 0) ; shorter delay before showing completions. Default is 0.1.
(setq ac-auto-show-menu t) ; automatically show menu
(setq ac-quick-help-delay 0.5) ; shorter delay before showing quick help. Default is 1.5, 0 makes it crash
(setq midnight-period 600) ; every ten minutes run clean-buffer-list, which kills *Help*, *Buffer List*, *Apropos*, etc buffers that haven't been visited in the last hour

;; custom key bindings
(define-keys nil
  '(("C-x u" nil) ; disable emacs default keybinding for undo, use C-z instead
    ("C-x C-b" buffer-menu)		; C-x C-b shows buffer menu
    ("C-x C-r" recentf-open-files) ; C-x C-r -> display recent files (overrides open file in read-only mode)
    ("C-x r" recentf-open-files) ; make C-x r recent files as well in case I hit wrong button
    ("C-x C-d" ido-dired) ; C-x C-d -> dired instead of list directory
    ("C-M-S-k" backward-kill-sexp) ; C-M-S-k is backward-kill-sexp (kill-sexp is (C-M-k))
    ("C-S-k" backward-kill-line)   ; C-S-k will kill line backwards.
    ("C-M-y" popup-yank-menu) 
    ("<f12> s" stackoverflow-search)
    ("<f12> b" bing-search) 
    ;; ("C-z" undo)     ; C-z -> undo instead of minimize emacs ; C-z used by evil-mode to switch to emacs state
    ("C-v" yank)     ; C-v -> yank instead of whatever it usually does
    ("<escape>" keyboard-escape-quit)
    ("<insert>" nil)		    ; disable overwrite key on windows
    ("C-c e" eval-and-replace) ; eval previous elisp expression at point, replace with results
    ("M-j" join-next-line)
    ("C-x z")				; disable minimize emacs
    ))

;; Use the AppButton (Windows) or fn key (Mac) to switch windows, frames, buffers, etc.
(setq ns-function-modifier 'hyper) ; doesn't actually seem to work
(setq w32-apps-modifier 'hyper)
(define-keys nil
  '(("<H-up>" windmove-up)
    ("<H-left>" windmove-left) 
    ("<H-right>" windmove-right) 
    ("<H-down>" windmove-down)
    ("<C-s-M-left>" windmove-left)
    ("<C-s-M-right>" windmove-right)
    ("<C-s-M-up>" windmove-up)
    ("<C-s-M-down>" windmove-down)
    ("<H-SPC>" other-frame)
    ("<C-s-M-return>" other-frame)
    ("<H-S-left>" previous-buffer) 
    ("<H-S-right>" next-buffer)
    ("H-k" kill-this-buffer))) 

;;; todo -> super, alt modifiers?
;;; ns-right-command-modifier
;;; ns-right-option-modifier
;;; (setq w32-pass-lwindow-to-system nil)
;;; (setq w32-apps-lwindow-modifer 'super)
;;; (setq w32-scroll-lock-modifier 'alt)
;;; (setq w32-recognize-altgr t) ; Right Ctrl + Left Alt is <AltGr> ?

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
			"html-init.el")))))

(mapc 'require '(lisp-init
		 ;; clojure-init
		 elisp-init
		 org-init
		 js-init
		 markdown-init
		 ruby-init
		 objc-init
		 python-init
		 erlang-init
		 html-init))
(put 'upcase-region 'disabled nil)
