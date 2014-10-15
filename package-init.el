;; -*- comment-column: 50; -*-

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
    auto-complete-clang-async
    bm
    clojure-mode
    clojure-mode-extra-font-locking
    clojurescript-mode
    company
    diminish
    dired-details
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
    flx-ido
    fuzzy
    highlight-parentheses
    highlight-symbol
    hl-sexp
    ido-ubiquitous
    ipython
    jquery-doc
    js3-mode
    json
    less-css-mode
    loccur
    magit
    markdown-mode
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
    (unless package-archive-contents
      (cam-refresh-package-contents-if-needed))))

(defun cam-refresh-package-contents-if-needed ()
  "Call package-refresh-contents the first time this function is called."
  (unless cam-has-refreshed-packages-p
    (package-refresh-contents)
    (setq cam-has-refreshed-packages-p t)))

(defun cam/activate-package (package)
  "Activate a locally installed package"
  (let ((package-version (elt (cdr (assoc package package-alist)) 0)))
    (package-activate package package-version)))


;;;; ADVICE / ETC

(defadvice package-install (around package-install-around)
  "Make sure archives are loaded first"
  (cam/load-archives-if-needed)
   ad-do-it)

(defadvice package-list-packages-no-fetch (around package-list-packages-no-fetch-around)
  "Make sure archives are loaded first"
  (cam/load-archives-if-needed)
  ad-do-it)


;;;; ACTIVATE/INSTALL PACKAGES

;;; load locally installed package info
(package-load-all-descriptors)

;;; loop through packages and initialize or install
(mapc (lambda (package)
        (if (package-installed-p package) (cam/activate-package package)
          (progn
            (cam-refresh-package-contents-if-needed)
            (package-install package))))
      cam/packages)

(provide 'package-init)
