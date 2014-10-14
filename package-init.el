;; -*- comment-column: 50; -*-

;; Init code relating to loading/setting up packages for use in Emacs

(require 'package)

(nconc package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                          ("marmalade" . "http://marmalade-repo.org/packages/")))

(defvar cam-has-refreshed-packages-p nil
  "Have we called package-refresh-contents yet?")

(defun cam-refresh-package-contents-if-needed ()
  "Call package-refresh-contents the first time this function is called."
  (when (not cam-has-refreshed-packages-p)
    (setq cam-has-refreshed-packages-p t)
    (package-refresh-contents)))

(package-initialize)
(when (not package-archive-contents)
  (cam-refresh-package-contents-if-needed))

;; install melpa/maramalade packages
(mapc (lambda (package)
        (when (not (package-installed-p package))
          (cam-refresh-package-contents-if-needed)
          (package-install package)))
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
        yasnippet))

;; packages to always require on launch
(mapc 'require '(
                 ace-jump-buffer
                 ace-jump-mode
                 bm
                 cam-functions
                 dired-details
                 dired+
                 evil
                 evil-paredit
                 evil-matchit
                 find-things-fast
                 flx-ido
                 highlight-symbol
                 loccur
                 midnight
                 multiple-cursors
                 nav                              ; nav frame, better than speed bar
                 powerline
                 powerline-evil
                 rainbow-delimiters
                 rainbow-mode
                 recentf
                 relative-line-numbers
                 smex                             ; IDO-like completion for M-x
                 undo-tree
                 ))

(provide 'package-init)
