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
  '(ac-etags
    ace-jump-buffer
    ace-jump-mode
    ace-isearch
    bm
    browse-kill-ring          ; opens new buffer for browsing kill ring
    clojure-cheatsheet
    clojure-mode
    clojure-mode-extra-font-locking
    clojurescript-mode
    company
    cperl-mode
    dash
    dash-functional
    diff-hl
    diminish
    dired+
    discover-my-major
    django-mode
    dockerfile-mode           ; major mode for editing Dockerfiles
    editorconfig
    elisp-slime-nav
    elpy
    erlang
    evil
    evil-matchit
    evil-paredit
    find-things-fast
    flycheck
    flymake                   ; version on Marmalade is newer than one bundled w/ Emacs
    flymake-json
    flx-ido
    fuzzy
    gitconfig-mode            ; major mode for editing .gitconfig files
    gitignore-mode            ; major mode for editing .gitignore files
    git-timemachine
    highlight-cl              ; font-lock the cl- functions in  ELisp Mode
    highlight-parentheses
    highlight-symbol
    hl-sexp
    ido-ubiquitous
    ido-vertical-mode
    ipython
    jedi
    js3-mode
    json-mode
    less-css-mode
    loccur
    magit
    markdown-mode
    maxframe                  ; functions like maximize-frame
    moe-theme
    morlock                   ; extra font-lock keywords for elisp editing
    multiple-cursors
    nav
    noflet                    ; locally override function definitions
    outline-magic
    paredit
    powerline
    powerline-evil
    py-autopep8
    pydoc-info
    pyflakes
    pymacs
    python
    python-magic
    python-pep8
    rainbow-delimiters
    rainbow-mode
    register-list             ; dired-like editing of Emacs registers
    relative-line-numbers
    rotate                    ; rotate-window, rotate-layout, etc.
    rspec-mode
    ruby-block
    ruby-electric
    slime
    smartparens
    smex
    undo-tree
    web-beautify               ; run js-beautify when saving JS files
    xmlgen
    yaml-mode
    yasnippet)
  "List of packages to be installed or activated when Emacs is lau")
;;;; HELPER FUNCTIONS

(defvar cam-has-refreshed-packages-p nil
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
  (unless cam-has-refreshed-packages-p
    (setq cam-has-refreshed-packages-p t)
    (setq package--initialized nil)     ; stop trying to fool package.el so it can do its thing
    (::auto-update-packages)))

(defun ::auto-update-packages ()
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
