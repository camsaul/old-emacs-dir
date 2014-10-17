;;; pacakge-init -- Code to load/installs packages on startup
;; -*- comment-column: 30; -*-

;;; Commentary:
;;; !!!!! IMPORTANT !!!!!!
;;; This is the very first customization file we're loading, so don't try to use anything non-standard function from any library except for cl
;;; (that means no dash and no cam-functions)

;;; Code:

;; Init code relating to loading/setting up packages for use in Emacs

(require 'package)

(setq package-alist nil                           ; these vars would get replicated in package-initialize, but we're going to replicate the behaviour in a little faster fashion
      package-obsolete-alist nil
      package--initialized t)                     ; fake that we've called package-initialize

;;;; SETTINGS

(nconc package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                          ("marmalade" . "http://marmalade-repo.org/packages/")))

(defvar cam/packages
  '(ac-etags
    ace-jump-buffer
    ace-jump-mode
    ace-isearch
    bm
    clojure-mode
    clojure-mode-extra-font-locking
    clojurescript-mode
    company
    dash
    diminish
    dired+
    django-mode
    editorconfig
    elisp-slime-nav
    elpy
    erlang
    evil
    evil-matchit
    evil-paredit
    find-things-fast
    flycheck
    flymake-json
    flx-ido
    fuzzy
    highlight-parentheses
    highlight-symbol
    hl-sexp
    ido-ubiquitous
    ipython
    jedi
    js3-mode
    ;; json
    json-mode
    less-css-mode
    loccur
    magit
    markdown-mode
    moe-theme
    morlock                   ; extra font-lock keywords for elisp editing
    multiple-cursors
    nav
    outline-magic
    paredit
    powerline
    powerline-evil
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
    relative-line-numbers
    rspec-mode
    ruby-block
    ruby-electric
    slime
    smartparens
    smex
    tommyh-theme
    undo-tree
    xmlgen
    yaml-mode
    yasnippet)
  "List of packages to be installed or activated when Emacs is launched.")


;;;; HELPER FUNCTIONS

(defvar cam-has-refreshed-packages-p nil
  "Have we called package-refresh-contents yet?")

(defun cam/load-archives-if-needed ()
  "Load package archives locally or fetch remotely if needed."
  (unless package-archive-contents
    (package-read-all-archive-contents)
    (package-load-all-descriptors)
    (unless package-archive-contents
      (cam/package-refresh-contents-once))))

(defun cam/package-refresh-contents-once ()
  "Call package-refresh-contents the first time this function is called."
  (unless cam-has-refreshed-packages-p
    (package-refresh-contents)
    (setq cam-has-refreshed-packages-p t)

    ; upgrade any packages that we can upgrade
    (save-window-excursion
      (package-list-packages-no-fetch)
      (package-menu--find-upgrades)
      (package-menu-mark-upgrades)
      (package-menu-execute t) ; t = noquery
      (kill-buffer-and-window))))


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
        (if (package-installed-p package) (package-activate package)
          (progn
            (cam/package-refresh-contents-once)
            (package-install package))))
      cam/packages)

(provide 'package-init)
;;; package-init.el ends here
