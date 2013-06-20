;; add this dir to the load path
(add-to-list 'load-path "~/.emacs.d/")

;; MELPA Package Source
(require 'package)
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
		     less-css-mode diminish rainbow-delimiters rainbow-mode hl-sexp))

(mapc 'require '(cl 
		 cam-functions 
		 recentf 
		 rainbow-delimiters))

;; global minor modes
(global-rainbow-delimiters-mode 1)
(winner-mode 1)
(global-linum-mode 1) ; linum-mode is for line numbers on left
(line-number-mode 0) ; line-number-mode is for line numbers on mode line
(column-number-mode 1)
(global-auto-revert-mode 0)
(global-hl-line-mode 1) ; highlights the current line
(set-face-background 'hl-line "#F0F0F0")
(ido-mode 1)
(recentf-mode 1)
(rainbow-mode 1) ; colorize strings that represent colors, e.g. "#aabbcc" or "blue"  

;; global settings
(setq query-replace-interactive t) ; Use last incremental seach regexp for query in regexp-query-replace
(setq make-backup-files nil) ; stop creating those backup~ files
(prefer-coding-system 'utf-8-auto-unix)
(setq inhibit-startup-screen t) ; inhibit startup screen
(setq inhibit-splash-screen t) ; inhibit splash screen
(add-hook 'emacs-startup-hook (lambda () (kill-buffer "*scratch*")))
(setq recentf-max-menu-items 20)
(setq explicit-bash-args '("-i" "--noediting")) ; arguments sent to bash shell
(set-frame-font "Source Code Pro-9") ; Source Code Pro open-source font by Adobe. https://github.com/abobe/Source-Code-Pro

;; highlight in bold red the words FIX. FIXME, TODO, HACK, REFACTOR, NOCOMMIT.
(font-lock-add-keywords
 nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
	1 font-lock-warning-face t)))

;; custom key bindings
(define-keys nil
  '(("C-x u" nil) ; disable emacs default keybinding for undo, use C-z instead
    ("C-x C-b" buffer-menu)		; C-x C-b shows buffer menu
    ("C-x C-r" recentf-open-files) ; C-x C-r -> display recent files (overrides open file in read-only mode)
    ("C-x C-d" ido-dired) ; C-x C-d -> dired instead of list directory
    ("C-M-y" popup-yank-menu) 
    ("<f12> s" stackoverflow-search)
    ("<f12> b" bing-search) 
    ("C-z" undo)	       ; C-z -> undo instead of minimize emacs
    ("C-v" yank)	       ; C-z -> undo instead of minimize emacs
    ("<escape>" keyboard-escape-quit)
    ("<insert>" nil)			; disable overwrite key
    ("M-j" join-next-line)
    ("C-x z")			; disable minimize emacs
    ("<mouse-4>" speedbar)	; mouse button 4 can open the speedbar
    ))

;; Use the AppButton (Windows) or fn key (Mac) to switch windows, frames, buffers, etc.
(setq ns-function-modifier 'hyper)
(setq w32-apps-modifier 'hyper)
(define-keys nil
  '(("<H-up>" windmove-up) 
    ("<H-left>" windmove-left) 
    ("<H-right>" windmove-right) 
    ("<H-down>" windmove-down) 
    ("<H-SPC>" other-frame) 
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

;;; put my custom stuff in a menu
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
			"objc-init.el")))))

(mapc 'require '(lisp-init
		 clojure-init
		 elisp-init
		 org-init
		 js-init
		 markdown-init
		 objc-init))
