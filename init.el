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
      '(clojure-mode clojure-test-mode nrepl ac-nrepl highlight-parentheses paredit markdown-mode
		     less-css-mode diminish rainbow-delimiters rainbow-mode hl-sexp fuzzy
		     json slime erlang python ipython xmlgen rspec-mode ruby-electric ruby-block
		     undo-tree evil nav yasnippet dired+ smex elisp-slime-nav tabbar clojurescript-mode
                     elpy pyflakes pymacs outline-magic python-magic multiple-cursors))

;; install el-get if needed
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

;; install el-get packages
(mapc (lambda (package)
	(unless (package-installed-p package)
	  (el-get-install package)))
      '()					  ; nothing right now
      )

(mapc 'require '(cam-functions 
		 recentf 
		 rainbow-delimiters
                 rainbow-mode
		 clojuredocs
		 midnight
		 undo-tree
		 ;; evil
		 nav				  ; nav frame, better than speed bar 
		 dired+
		 smex				  ; IDO-like completion for M-x
                 multiple-cursors
		 ))

(add-hook 'before-make-frame-hook 'turn-off-tool-bar)

;; helper settings to make emacs work better from terminal
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(ansi-color-for-comint-mode-on)

;; start in fullscreen mode
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

(add-hook 'emacs-startup-hook			  
	  (lambda ()
            (progn
              (kill-buffer "*scratch*")
              (nav))))                            ; Start with a nav buffer open

;; global minor modes
(global-rainbow-delimiters-mode 1)
(winner-mode 1)					  
(global-linum-mode 1)				  ; linum-mode is for line numbers on left	      			  
(line-number-mode 0) 				  ; line-number-mode is for line numbers on mode line
(column-number-mode 1)
(global-auto-revert-mode 1)			  ; auto-revert mode reload buffers when underlying file changes 
(global-hl-line-mode 1)				  ; highlights the current line 
(set-face-background 'hl-line "#F0F0F0")
(ido-mode 1)
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
  ;; highlight in bold red the words FIX. FIXME, TODO, HACK, REFACTOR, NOCOMMIT.
  (font-lock-add-keywords
    nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
	1 font-lock-warning-face t))))

;; global settings
(setq query-replace-interactive t)		  ; Use last incremental seach regexp for query in regexp-query-replace 
(prefer-coding-system 'utf-8-auto-unix)
(setq inhibit-startup-screen t
      inhibit-splash-screen t)
(setq recentf-max-menu-items 20)
(set-frame-font (if (string-equal window-system "ns")
		    "Menlo Regular-10"		  ; use the Xcode font on OS X 
		  "Source Code Pro-10"		  ; Source Code Pro open-source font by Adobe. https://github.com/abobe/Source-Code-Pro
		  ))

(setq
 scroll-step 1                                    ; prevent Emacs from getting into weird state where it insists on centering the buffer on the cursor
  -conservatively 9999
  -up-aggresively 0.01
 scroll-down-aggresively 0.01
 auto-window-vscroll nil                          ; don't 'automatically adjust window to view tall lines'
 mouse-wheel-scroll-amount '(1 ((shift) . 1 ))
 scroll-margin 1
 )
(setq global-auto-revert-non-file-buffers t)	  ; also refresh dired but be quiet about it 
(setq auto-revert-verbose nil)
(setq ac-delay 0)				  ; shorter delay before showing completions. Default is 0.1. 
(setq ac-auto-show-menu t)			  ; automatically show menu 
(setq ac-quick-help-delay 0.5)			  ; shorter delay before showing quick help. Default is 1.5, 0 makes it crash 
(setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
(setq midnight-period 10)			  ; every 10 secs run clean-buffer-list, which kills *Help*, *Buffer List*, *Apropos*, etc buffers that haven't been visited recently
(midnight-delay-set 'midnight-delay 10)		  ; Have to use this function to set midnight-delay
(setq clean-buffer-list-delay-special 30)	  ; Remove buffers that haven't been used in last 30 secs
(setq clean-buffer-list-delay-general 0.02)	  ; Kill ANY buffer that hasn't been used in the half-hour (ish) - param is in day
(setq clean-buffer-list-kill-regexps		  ; Remove all starred buffers not currently in use
      '("\\*.*\\*"))
(setq nav-width 30)				  ; nav should be 30 chars wide (default is 18)
(nav-disable-overeager-window-splitting)	  ; turn off automatic splitting of frames when opening files in a large frame (?)
(set-default 'indent-tabs-mode nil) 		  ; Indentation can insert tabs if this is non-nil
(setq x-select-enable-clipboard t)		  ; Use the clipboard in addition to emacs kill ring
;; (setq ns-use-native-fullscreen t)                 ; Native fullscreen on OS X
;; (set-frame-parameter nil 'fullscreen 'fullboth)   ; Always use fullscreen

;; custom key bindings
(define-keys nil
  '(("C-x u" nil) 				  ; disable emacs default keybinding for undo, use C-z instead
    ("C-x C-b" buffer-menu)			  ; C-x C-b shows buffer menu			  		
    ("C-x C-r" recentf-open-files)		  ; C-x C-r -> display recent files (overrides open file in read-only mode)		   
    ("C-x r" recentf-open-files)		  ; make C-x r recent files as well in case I hit wrong button		   
    ("C-x C-d" ido-dired)			  ; C-x C-d -> dired instead of list directory			   
    ("C-M-S-k" backward-kill-sexp)		  ; C-M-S-k is backward-kill-sexp (kill-sexp is (C-M-k))		   
    ("C-S-k" backward-kill-line)		  
    ("C-M-y" popup-yank-menu)
    ("C-x k" kill-this-buffer)			  ; kill-this-buffer instead of kill-buffer (prompts for which buffer)    
    ("<f9>" whitespace-mode)
    ("<f10>" (lambda ()				  ; Jump to a nav buffer. F10 replaces menu-bar-open, which lets you browse menu from a buffer (not very useful)
	       (interactive)
	       (switch-to-buffer-other-window "*nav*")))
    ("S-<f10>" nav)				  ; Open nav buffer
    ("<f11>" paredit-mode)			  ; F11 is now global key for paredit-mode			   
    ("<f12> s" stackoverflow-search)
    ("<f12> b" bing-search) 
    ;; ("C-z" undo)     ; C-z -> undo instead of minimize emacs ; C-z used by evil-mode to switch to emacs state
    ("C-v" yank)				  ; C-v -> yank instead of whatever it usually does				       				  				  
    ("<escape>" keyboard-escape-quit)
    ("<insert>" nil)			          ; disable stupid insert key TODO maybe use as a prefix to insert something useful
    ("H-E" mc/mark-next-like-this)                ; Apparently Insert = Hyper on OS X WHEN USED IN COMBINATION WITH OTHER MODIFIER KEYS!
    ("H-A" mc/mark-previous-like-this)
    ("C-H-a" mc/mark-all-like-this)
    ("C-H-e" mc/edit-lines)
    ("C-c e" eval-and-replace)		     	  ; eval previous elisp expression at point, replace with results
    ("M-j" join-next-line)
    ("C-x z")				      	  ; disable minimize emacs
    ("s-]" force-indent-region)
    ("s-[" force-unindent-region)
    ("M-x" smex)				  ; smex is IDO-mode like M-x behavior    
    ))



(setq ns-function-modifier 'hyper)		  ; doesn't actually seem to work     
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
