;; -*- comment-column: 30; -*-
;;; pacakge-init -- Code to load/installs packages on startup

;;; Commentary:
;;; !!!!! IMPORTANT !!!!!!
;;; This is the very first customization file we're loading, so don't try to use anything non-standard function from any library except for cl
;;; (that means no dash and no cam-functions)

;;; Code:

;; Init code relating to loading/setting up packages for use in Emacs

(require 'package)

(setq package--initialized t)            ; fake that we've called package-initialize

;;;; SETTINGS
(unless (assoc "melpa" package-archives) ; make sure we don't add repos more than once
  (nconc package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                            ("marmalade" . "http://marmalade-repo.org/packages/"))))


(defvar cam/packages
  '(ac-etags                  ; etags/ctags completion source for autocomplete (DEPRECATED?)
    ace-jump-buffer           ; quickly jump between buffers
    ace-jump-mode             ; quickly jump anywhere on screen
    ace-isearch               ; C-s switches between ace-jump/isearch/helm-swoop based on length of search term
    ace-window                ; ace-style jumping between all visible windows
    aggressive-indent         ; Minor mode to aggressively keep your code always indented
    anaconda-mode             ; python doc or something like that
    angry-police-captain      ; show quotes from [[http://theangrypolicecaptain.com]]
    async                     ; Asynchronous processing in Emacs
    bm                        ; bookmarks
    browse-kill-ring          ; opens new buffer for browsing kill ring
    cl-lib-highlight          ; full cl-lib font-lock highlighting
    clojure-cheatsheet        ; The Clojure Cheatsheet for Emacs
    clojure-mode              ; Major mode for Clojure code
    clojure-mode-extra-font-locking ; Extra font-locking for Clojure mode
    clojurescript-mode        ; Major mode for ClojureScript code
    company                   ; autocompletion, I prefer this to autocomplete
    company-anaconda          ; Python anaconda source for company (DEPRECATED?)
    cperl-mode                ; perl editing commands for emacs
    dash                      ; Clojure-like functions and macros for Emacs Lisp <3
    dash-functional           ; Collection of combinators for Emacs Lisp
    diff-hl                   ; Highlight uncommited changes
    diminish                  ; Replace or hide minor modes in mode-line
    dired+                    ; extensions to Dired
    discover-my-major         ; Discover key bindings and their meaning for the current Emacs major mode
    django-mode               ; Major mode for Django web framework. (Not sure I need anything here not in python-mode - DEPRECATED?)
    dockerfile-mode           ; major mode for editing Dockerfiles
    editorconfig              ; Read EditorConfig files
    elisp-slime-nav           ; M-. jumps to definition of symbol at point, M-, to jump back
    elpy                      ; Emacs Python Development Environment
    erlang                    ; Erlang major mode
    evil                      ; Extensible Vi layer for Emacs.
    evil-matchit              ; Vim matchit ported into Emacs (requires EVIL)
    evil-paredit              ; Paredit support for evil keybindings
    f                         ; modern API for working with files / dirs
    find-things-fast          ; Find things fast, leveraging the power of git
    flycheck                  ; Modern on-the-fly syntax checking for GNU Emacs
    flymake                   ; version on Marmalade is newer than one bundled w/ Emacs
    flymake-json              ; A flymake handler for json using jsonlint
    flx-ido                   ; flx integration for ido
    fuzzy                     ; Fuzzy Matching
    gitconfig-mode            ; major mode for editing .gitconfig files
    gitignore-mode            ; major mode for editing .gitignore files
    git-timemachine           ; Walk through git revisions of a file
    guide-key                 ; Show completions for prefix keybindings
    helm                      ; Helm is an Emacs incremental and narrowing framework
    highlight-cl              ; font-lock the cl- functions in  ELisp Mode
    highlight-parentheses     ; highlight surrounding parentheses
    highlight-symbol          ; automatic and manual symbol highlighting
    hl-sexp                   ; highlight the current sexp
    ido-ubiquitous            ; Use ido (nearly) everywhere.
    ido-vertical-mode         ; Makes ido-mode display vertically.
    ipython                   ; Adds support for IPython to python-mode.el
    jedi                      ; Python auto-completion for Emacs
    js3-mode                  ; An improved JavaScript editing mode
    json-mode                 ; Major mode for editing JSON files
    less-css-mode             ; Major mode for editing LESS CSS files (lesscss.org)
    loccur                    ; Performs an occur-like folding in current buffer.
    macrostep                 ; interactive macro stepper for Emacs Lisp
    magit                     ; control Git from Emacs
    markdown-mode             ; Emacs Major mode for Markdown-formatted text files
    maxframe                  ; functions like maximize-frame
    moe-theme                 ; A colorful eye-candy theme. Moe, moe, kyun!
    morlock                   ; extra font-lock keywords for elisp editing
    multiple-cursors          ; Multiple cursors for Emacs.
    nav                       ; Emacs mode for filesystem navigation
    noflet                    ; locally override function definitions
    outline-magic             ; outline mode extensions for Emacs
    paredit                   ; minor mode for editing parentheses
    php-mode                  ; Major mode for editing PHP code
    powerline                 ; Rewrite of Powerline
    powerline-evil            ; Utilities for better Evil support for Powerline
    projectile                ; things like projectile-recentf (recently edited files in current Git project)
    py-autopep8               ; Use autopep8 to beautify a Python buffer
    pydoc-info                ; Better Python support for info-lookup-symbol.
    pyflakes                  ; run the python pyflakes checker putting hits in a grep buffer
    pymacs                    ; Interface between Emacs Lisp and Python
    python                    ; Python's flying circus support for Emacs
    python-magic              ; outline mode extension for python mode
    python-pep8               ; minor mode for running `pep8'
    rainbow-delimiters        ; Highlight nested parens, brackets, braces a different color at each depth.
    rainbow-mode              ; Colorize color names in buffers
    register-list             ; dired-like editing of Emacs registers
    relative-line-numbers     ; Display relative line numbers on the margin
    rotate                    ; rotate-window, rotate-layout, etc.
    rspec-mode                ; Enhance ruby-mode for RSpec
    ruby-block                ; highlight matching block
    ruby-electric             ; Minor mode for electrically editing ruby code
    s                         ; "the long lost Emacs string manipulation library"
    saveplace                 ; save the position of point when killing a buffer
    slime                     ; Superior Lisp Interaction Mode for Emacs
    smartparens               ; Automatic insertion, wrapping and paredit-like navigation with user defined pairs.
    smex                      ; M-x interface with Ido-style fuzzy matching.
    undo-tree                 ; Treat undo history as a tree
    web-beautify              ; run js-beautify when saving JS files
    xmlgen                    ; A DSL for generating XML.
    yaml-mode                 ; Major mode for editing YAML files
    yasnippet)                ; Yet another snippet extension for Emacs.
  "List of packages to be installed or activated when Emacs is launched.")


;;;; HELPER FUNCTIONS

(defvar cam/has-refreshed-packages-p nil
  "Have we called package-refresh-contents yet?")

(defun cam/load-archives-if-needed ()
  "Load package archives locally or fetch remotely if needed."
  (unless package-archive-contents
    (package-read-all-archive-contents)
    (package-load-all-descriptors)
    (unless package-archive-contents
      (cam/refresh-package-contents-once))))

(defun cam/refresh-package-contents-once ()
  "Call package-refresh-contents the first time this function is called."
  (unless cam/has-refreshed-packages-p
    (setq cam/has-refreshed-packages-p t)
    (setq package--initialized nil)     ; stop trying to fool package.el so it can do its thing
    (cam/auto-update-packages)))

(defun cam/auto-update-packages ()
  "Fetch packages, and upgrade any packages that can be upgraded."
  (interactive)
  (package-initialize)
  (condition-case err
      (save-window-excursion
        (progn
          (package-list-packages)
          (package-menu--find-upgrades)
          (package-menu-mark-upgrades)
          (package-menu-execute t) ; t = noquery
          (ignore-errors
            (kill-buffer-and-window))
          (message "%s" "Successfully auto-updated packages.")))
    (error (warn "Couldn't auto-update packages: %s" (error-message-string err)))))

;;;; ADVICE / ETC

(defadvice package-install (before package-install-before activate)
  "Make sure archives are loaded first"
  (cam/load-archives-if-needed))

(defadvice package-list-packages-no-fetch (before package-list-packages-no-fetch-before activate)
  "Make sure archives are loaded first"
  (cam/load-archives-if-needed))

(defadvice package-activate (around package-activate-around activate)
  "Catch errors thrown by package-activate."
  (condition-case err
      ad-do-it
    (error (warn (error-message-string err)))))


;;;; ACTIVATE/INSTALL PACKAGES

;;; load locally installed package info
(package-load-all-descriptors)

;;; loop through packages and initialize or install
(mapc (lambda (package)
        (condition-case err
          (if (package-installed-p package) (package-activate package)
            (progn
              (message "Installiing %s..." (symbol-name package))
              (cam/refresh-package-contents-once)
              (package-install package)))
          (error (warn "Failed to install %s: %s" (symbol-name package) (error-message-string err)))))
      cam/packages)

(provide 'package-init)
;;; package-init.el ends here
