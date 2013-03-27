;; add this dir to the load path
(add-to-list 'load-path "~/.emacs.d/")

;; MELPA Package Source
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(when (not package-archive-contents)
	(package-refresh-contents))
	
(defvar my-packages '(clojure-mode clojure-test-mode nrepl paredit
				   nrepl-ritz ac-nrepl highlight-parentheses))
(dolist (p my-packages)
	(when (not (package-installed-p p))
		(package-install p)))

;;; Auto-Complete for NREPL
(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)
(define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)

;;; Enable eldoc in clojure buffers
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

;;; Autocomplete mode
(add-hook 'nrepl-mode-hook 'auto-complete-mode)
(add-hook 'clojure-mode-hook 'auto-complete-mode)

;;; hide special buffers *nrepl-connection* and *nrepl-server*
(setq nrepl-hide-special-buffers t)

;;; stop error buffer from popping up while working in buffers other than the nrepl
(setq nrepl-popup-stacktraces nil)

;;; Enable error buffer popping also in the REPL:
(setq nrepl-popup-stacktraces-in-repl t)

;;; RainbowDelimiters is a minor mode which highlights parentheses, brackets, and braces according to their depth. Each successive level is highlighted in a different color. This makes it easy to 
;;; spot matching delimiters, orient yourself in the code, and tell which statements are at a given depth. Assuming you've already installed RainbowDelimiters you can enable it in nREPL like this:
(require 'rainbow-delimiters)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)

;;; Enabling CamelCase support for editing commands(like forward-word, backward-word, etc) in nREPL is quite useful since we often have to 
;;; deal with Java class and method names. The built-in Emacs minor mode subword-mode provides such functionality:
(add-hook 'clojure-mode-hook 'subword-mode)
(add-hook 'nrepl-mode-hook 'subword-mode)

;;; The use of paredit when editing Clojure (or any other Lisp) code is highly recommended. You're probably using it already in your clojure-mode 
;;; buffers (if you're not you probably should). You might also want to enable paredit in the nREPL buffer as well:
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;;; enable nrepl pretty printing
(add-hook 'nrepl-mode-hook 'nrepl-toggle-pretty-printing)

;;; enable clojure-test-mode
(add-hook 'clojure-mode-hook 'clojure-test-mode)

;;; enable nrepl-ritz functionality in nrepl connected clojure buffers
(add-hook 'nrepl-interaction-mode-hook 'my-nrepl-mode-setup)
(defun my-nrepl-mode-setup ()
	(require 'nrepl-ritz))
	
;;; enable winner mode (Ctrl-C left to undo changes to window configuration)
(winner-mode 1)

;;; enable shift + arrow-keys to move between windows
;;;(windmove-default-keybindings)

;;; highlight parentheses mode: highlights parenthese that surround the current point
(add-hook 'clojure-mode-hook 'highlight-parentheses-mode)
(add-hook 'nrepl-mode-hook 'highlight-parentheses-mode)

;;; stop creating those backup~ files 
(setq make-backup-files nil)

;;; always show line numbers
(add-hook 'clojure-mode-hook 'linum-mode)

;;; Enable Global Auto-Revert Mode (automatically reloads files that have changed on disc)
(global-auto-revert-mode t)

;;; enable global hi-line mode
(global-hl-line-mode 1)
(set-face-background 'hl-line "#FFF0F0")

;; better indenting for compojure stuff
(require 'clojure-mode)
(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))

;; ClojureScript Files should be edited in Clojure-mode
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))

;; Bind C-x C-b to a buffer menu (instead of list-buffers)
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; Inhibit startup screen & splash screen
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)

;; Display recent files when typing C-x C-r (overrides open file in read-only mode)
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; Popup the Yank menu
(global-set-key (kbd "C-M-y") '(lambda ()
				 (interactive)
				 (popup-menu 'yank-menu)))

;; Set Ctrl-Z to undo (instead of minimize emacs)
(global-set-key (kbd "C-z") 'undo)

;;; CAM STUFF!

(defun nice-ns (namespace)
  (interactive)
  "Returns the path of the src file for the given test namespace."
  (let* ((namespace (clojure-underscores-for-hyphens namespace)))
    (concat (car (last (split-string namespace "\\."))) ".clj")))

(defun toggle-test-file ()
  (interactive)
  (if 
      (string= major-mode "nrepl-mode")
      (progn
	(switch-to-buffer-other-window (nice-ns (nrepl-current-ns)))
	(find-file (funcall clojure-test-for-fn (clojure-find-ns))))
    (progn
      (save-buffer)
      (clojure-jump-between-tests-and-code))))

(defun strip-clj-cljs (namespace-str)
  (interactive)
  "Strips clj. or cljs. from the beginning on a namespace string generated by clojure-mode's clojure-expeceted-ns "
  "function (e.g. when separating Clojure and ClojureScript source in Leiningen)"
  (cond
   ((string= "clj." (substring namespace-str 0 4)) (substring namespace-str 4))
   ((string= "cljs." (substring namespace-str 0 5)) (substring namespace-str 5))
   (else namespace-str)))

;;; get the environment set up
(defun switch-to-nrepl-in-current-ns ()
  (interactive)
  (if (string= major-mode "nrepl-mode")
      (switch-to-buffer-other-window (nice-ns (nrepl-current-ns)))
    (let ((ns (strip-clj-cljs (clojure-expected-ns))))
      (if (or (not (get-buffer "*nrepl*"))
	      (not (get-buffer (nrepl-current-connection-buffer))))
	  (progn
	    (nrepl-jack-in))
	(progn
	  (save-buffer)
	  (nrepl-load-current-buffer)
	  (switch-to-buffer-other-window "*nrepl*")
	  (nrepl-set-ns ns))))))

(defun active-region-or-prompt (prompt)
  (url-hexify-string (if mark-active
			 (buffer-substring (region-beginning) (region-end))
		       (read-string prompt))))

(defun edit-init-file ()
  (interactive)
  (find-file "C:/users/camms_000/AppData/roaming/.emacs.d/init.el"))

(defun coffee-house ()
  "You are working in a coffee house."
  (interactive)
  (browse-url "http://www.coffitivity.com/"))

(defun paredit-cheatsheet ()
  (interactive)
  (browse-url "http://www.emacswiki.org/emacs/PareditCheatsheet"))

(defun clojure-cheatsheet ()
  (interactive)
  (browse-url "http://jafingerhut.github.com/cheatsheet-clj-1.3/cheatsheet-tiptip-no-cdocs-summary.html"))

(defun bing-search ()
  "Bings a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.bing.com/search?setmkt=en-US&q="
    (active-region-or-prompt "Search Bing for: "))))

(defun clojure-docs-search ()
  "Searches clojuredocs.org for a query or selected region if any."
  (interactive)
  (browse-url
   (concat
    "http://clojuredocs.org/search?x=0&y=0&q="
    (active-region-or-prompt "Search clojuredocs.org for: "))))

(defun stackoverflow-search ()
  "Searches StackOverflow for a query or selected region if any."
  (interactive)
  (browse-url
   (concat
    "http://stackoverflow.com/search?q="
    (active-region-or-prompt "Search StackOverflow for: "))))

(defun javadocs-search ()
  "Searches javadocs.org for a query or selected region if any."
  (interactive)
  (browse-url
   (concat
    "http://javadocs.org/"
    (active-region-or-prompt "Search javadocs.org for: "))))
    
;;; put my custom stuff in a menu
(defun cam-menu-setup ()
  (easy-menu-define cam-menu global-map "CAM :)"
    '("CAM :)"
      ["Coffee House" coffee-house]
      ["Bing Search" bing-search]
      ["ClojureDocs Search" clojure-docs-search]
      ["JavaDocs Search" javadocs-search]
      ["StackOverflow Search" stackoverflow-search]
      ["Paredit Cheatsheet" paredit-cheatsheet]
      ["Cloure Cheatsheet" clojure-cheatsheet]
      ["toggle-paredit-mode" paredit-mode]
      ["whitespace-mode" whitespace-mode]
      ["switch-to-nrepl-in-current-ns" switch-to-nrepl-in-current-ns]
      ["toggle tests / code" toggle-test-file]
      ["nrepl-jack-in" nrepl-jack-in]
      ["nrepl-ritz-jack-in" nrepl-ritz-jack-in]
      ["clojure-jump-between-tests-and-code" clojure-jump-between-tests-and-code]
      ["Toggle Line Numbers" linum-mode]
      ["Edit Emacs Init File" edit-init-file])))
      
;;; keyboard shortcuts for stuff i use all the time
(define-minor-mode cam-mode
  "Cam's keyboard shortcuts"
  nil ; initial value
  " CAM" ; indicator for the mode line
  ;; minor mode keymap
  `(
    (, (kbd "<C-M-S-up>") . windmove-up) ; Ctrl + Alt + Shift + arrow keys to move between windows
    (, (kbd "<C-M-S-left>") . windmove-left)
    (, (kbd "<C-M-S-right>") . windmove-right)
    (, (kbd "<C-M-S-down>") . windmove-down)
    (, (kbd "<C-x> w") . whitespace-mode)
    (, (kbd "<f12> b") . bing-search)
    (, (kbd "<f12> c") . clojure-docs-search)
    (, (kbd "<f12> j") . javadocs-search)
    (, (kbd "<f12> s") . stackoverflow-search)
    (, (kbd "<f12> <f12> p") . paredit-cheatsheet)
    (, (kbd "<f12> <f12> c") . clojure-cheatsheet)
    (, (kbd "<C-M-S-return>") . toggle-test-file)
    (, (kbd "S-<f9>") . clojure-test-run-tests)
    (, (kbd "<C-M-return>") . switch-to-nrepl-in-current-ns)
    (, (kbd "<f11>") . paredit-mode)
    ;; Overriding some defaults some i don't go crazy
    (, (kbd "C-z") . undo)
    (, (kbd "C-v") . yank)
    )
  :global 1)
(cam-mode 1)
(cam-menu-setup)
