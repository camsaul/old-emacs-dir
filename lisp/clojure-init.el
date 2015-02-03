;; -*- comment-column: 60; -*-

(defun cam/clojure-mode-setup ()
  "Setup for clojure-mode, cider-mode, and cider-repl-mode."
  (require 'cider)
  (require 'company)
  (require 'clojure-mode-extra-font-locking)
  (require 'clojure-cheatsheet)
  (cam/lisp-mode-setup)
  (cam/enable-minor-modes
    subword-mode ; enable CamelCase support for editor movement
    company-mode
    eldoc-mode)
  (cam/pretty-fn))

(add-hook 'clojure-mode-hook #'cam/clojure-mode-setup)
(add-hook 'cider-mode-hook #'cam/clojure-mode-setup)        ; do we need all three of these ??
(add-hook 'cider-repl-mode-hook #'cam/clojure-mode-setup)

;; custom keyboard shortcuts
(defun cam/define-clojure-keys (mode-map)
  (require 'lisp-init)
  (cam/define-lisp-keys mode-map)
  (cam/define-keys mode-map
    "<f12> c" #'cam/clojure-docs-search
    "<f12> i" #'cam/instant-clojure-cheatsheet-search
    "<f12> j" #'cam/javadocs-search
    "<f12> s" #'cam/stackoverflow-search
    "<f12> <f12> p" #'cam/paredit-cheatsheet
    "<C-M-s-return>" #'cam/save-compile-switch-to-nrepl))

(eval-after-load "clojure"
  '(progn
     (cam/define-clojure-keys clojure-mode-map)))

(eval-after-load "cider"
  '(progn
     (cam/define-clojure-keys cider-mode-map)
     (cam/define-clojure-keys cider-repl-mode-map)
     (cam/define-keys cider-repl-mode-map
       "RET" #'cider-repl-return)
     (setq cider-auto-select-error-buffer nil               ; PLEASE STOP AUTO-JUMPING TO THE ERROR BUFFER !
           cider-repl-use-pretty-printing t                 ; PRETTY PRINTING <3
           )))

(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode)) ; ClojureScript Files should be edited in Clojure-mode

(defun cam/save-compile-switch-to-nrepl ()
  "Save + compile current buffer, set its namespace in the REPL and switch to the REPL."
  (interactive)
  (save-buffer)
  (cider-load-buffer)
  (call-interactively #'cider-repl-set-ns)
  (cider-switch-to-relevant-repl-buffer)
  (cider-repl-clear-buffer))

(defun cam/pretty-fn ()
  "turns fn into a fancy f symbol. credit: emacs-starter-kit on github"
  (font-lock-add-keywords
   nil `(("(\\(\\<fn\\>\\)"
        (0 (progn (compose-region (match-beginning 1)
                                  (match-end 1)
                                  "\u0192"
                                  'decompose-region)))))))

(defun cam/paredit-cheatsheet ()
  (interactive)
  (browse-url "http://www.emacswiki.org/emacs/PareditCheatsheet"))

(defun cam/clojure-docs-search ()
  "Searches clojuredocs.org for a query or selected region if any."
  (interactive)
  (browse-url
   (concat
    "http://clojuredocs.org/search?x=0&y=0&q="
    (active-region-or-prompt "Search clojuredocs.org for: "))))

(defun cam/javadocs-search ()
  "Searches javadocs.org for a query or selected region if any."
  (interactive)
  (browse-url
   (concat
    "http://javadocs.org/"
    (active-region-or-prompt "Search javadocs.org for: "))))

(defun cam/instant-clojure-cheatsheet-search ()
  "Searches Instant Clojure Cheatsheet query or selected region if any."
  (interactive)
  (browse-url
   (concat
    "http://cammsaul.github.io/instant-clojure-cheatsheet/?"
    (active-region-or-prompt "Search Instant Clojure Cheatsheet for: "))))

(provide 'clojure-init)
