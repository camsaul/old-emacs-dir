;; -*- lexical-binding: t; comment-column: 60; -*-
;;; clojure-init - Fns + settings for editing Clojure
;;; Commentary:
;;; Code:

(mapc #'require '(cam-functions
                  cam-macros
                  lisp-init
                  clojure-mode
                  auto-complete
                  ac-cider
                  cider-mode
                  cider-repl
                  clj-refactor
                  clojure-cheatsheet
                  clojure-mode-extra-font-locking))
(eval-when-compile
  (require 'cl))

(defun cam/clojure-mode-setup ()
  "Setup for clojure-mode, cider-mode, and cider-repl-mode."
  (cam/lisp-mode-setup)
  (cam/enable-minor-modes
    clj-refactor-mode
    (subword-mode . nil))       ; enable CamelCase support for editor movement
  (cam/pretty-fn))

(add-hook 'clojure-mode-hook #'cam/clojure-mode-setup)
(add-hook 'cider-mode-hook #'cam/clojure-mode-setup)        ; do we need all three of these ??
(add-hook 'cider-repl-mode-hook #'cam/clojure-mode-setup)

(defun cam/cider-mode-setup ()
  "Setup for cider-mode and cider-repl-mode."
  (ac-cider-setup)
  (cam/enable-minor-modes
    eldoc-mode))

(add-hook 'cider-mode-hook #'cam/cider-mode-setup)
(add-hook 'cider-repl-mode-hook #'cam/cider-mode-setup)

;; custom keyboard shortcuts
(defun cam/define-clojure-keys (mode-map)
  (cam/define-lisp-keys mode-map)
  (cam/define-keys mode-map
    "<C-M-s-return>" #'cam/save-compile-switch-to-nrepl
    "<f10>" #'cam/instant-clojure-cheatsheet-search
    "<f12> <f12> c" #'cam/compojure-docs
    "<f12> <f12> k" #'cam/korma-docs
    "<f12> <f12> p" #'cam/paredit-cheatsheet
    "<f12> c" #'cam/clojure-docs-search
    "<f12> j" #'cam/javadocs-search
    "<f12> s" #'cam/stackoverflow-search
    "C-c C-d" #'cider-doc)
  (cljr-add-keybindings-with-prefix "C-c r"))

(cam/define-clojure-keys clojure-mode-map)
(define-clojure-indent
  (GET 2)
  (api-let 2)
  (auto-parse 1)
  (catch-api-exceptions 0)
  (context 2)
  (expect 1)
  ;; (ins 1)
  (let-400 1)
  (let-404 1)
  (match 1)
  (macrolet 1)
  (org-perms-case 1)
  (with-credentials 1))

(add-to-list 'ac-modes 'cider-mode)
(add-to-list 'ac-modes 'cider-repl-mode)

(cam/define-clojure-keys cider-mode-map)
(cam/define-clojure-keys cider-repl-mode-map)
(cam/define-keys cider-repl-mode-map
  "<C-M-s-return>" #'cider-switch-to-last-clojure-buffer
  "RET" #'cider-repl-return)
(setq cider-auto-select-error-buffer nil          ; PLEASE STOP AUTO-JUMPING TO THE ERROR BUFFER !
      cider-repl-use-clojure-font-lock t          ; REPL uses same (nicer) font locking as Clojure Mode buffers (supposedly!)
      cider-repl-use-pretty-printing t)           ; PRETTY PRINTING <3



(defmacro cam/with-cider (&rest body)
  "Evaluate the contents of BODY with a valid cider connection, starting one if neeed."
  (let ((fn-tag '--with-cider-fn--;; (cl-gensym "cam/with-cider-fn-")
                )
        (buffer-tag '--with-cider-buffer--;; (cl-gensym "cam/with-cider-buffer-")
                    ))
    (message (symbol-name fn-tag))
    `(if (cider-connected-p) (progn ,@body)
       (let ((,buffer-tag (current-buffer)))
         (fset #',fn-tag #'(lambda ()
                             (with-current-buffer ,buffer-tag
                               ,@body
                               (remove-hook 'cider-connected-hook #',fn-tag :local))
                             (fset #',fn-tag nil)))
         (cider-jack-in)
         (add-hook 'cider-connected-hook #',fn-tag :append :local)))))
(put 'cam/with-cider 'lisp-indent-function 0)

(defun cam/save-compile-switch-to-nrepl ()
  "Save + compile current buffer, set its namespace in the REPL and switch to the REPL (starting it if needed)."
  (interactive)
  (save-buffer)
  (cam/with-cider
   (cider-load-buffer)
   (call-interactively #'cider-repl-set-ns)
   (cider-switch-to-relevant-repl-buffer)
   (cider-repl-clear-buffer)))

;; (defmacro fset-test (x &rest body)
;;   (let ((fn-tag (cl-gensym "cam/with-cider-fn-"))
;;      (buffer-tag (cl-gensym "cam/with-cider-buffer-")))
;;     `(if ,x :false
;;        (let ((,buffer-tag (current-buffer)))
;;       (defun ,fn-tag ()
;;         (with-current-buffer ,buffer-tag
;;           (message "DESTROYED: %s" (symbol-name ',fn-tag))
;;           (remove-hook 'test-hook #',fn-tag :local)
;;           '(fset #',fn-tag nil)
;;           ,@body))
;;       (add-hook 'test-hook #',fn-tag :append :local)
;;       (message "CREATED: %s" (symbol-name ',fn-tag))))))
;; (fset-test nil (message (symbol-name (cl-gensym "HERE-"))))
;; test-hook
;; (run-hooks 'test-hook)
;; test-hook

(defadvice message (after cam/message-tail-messagess-buffer activate)
  "When *Messages* buffer is visible, tail it."
  (when-let ((w (cam/buffer-window "*Messages*")))
    (set-window-point w (with-current-buffer "*Messages*" (point-max)))))

(defun cam/pretty-fn ()
  "turns fn into a fancy f symbol. credit: emacs-starter-kit on github"
  (font-lock-add-keywords
   nil `(("(\\(\\<fn\\>\\)"
          (0 (progn (compose-region (match-beginning 1)
                                    (match-end 1)
                                    "\u0192"
                                    'decompose-region)))))))

(defun cam/paredit-cheatsheet ()
  "Open the Paredit Cheatsheet."
  (interactive)
  (browse-url "http://www.emacswiki.org/emacs/PareditCheatsheet"))

(defun cam/korma-docs ()
  "Open the Korma documentation."
  (interactive)
  (browse-url "http://www.sqlkorma.com/docs"))

(defun cam/compojure-docs ()
  "Open the Compojure documentation."
  (interactive)
  (browse-url "https://github.com/weavejester/compojure/"))

(defun cam/clojure-docs-search ()
  "Searches clojuredocs.org for a query or selected region if any."
  (interactive)
  (browse-url
   (concat
    "http://clojuredocs.org/search?x=0&y=0&q="
    (cam/active-region-or-prompt "Search clojuredocs.org for: "))))

(defun cam/javadocs-search ()
  "Searches javadocs.org for a query or selected region if any."
  (interactive)
  (browse-url
   (concat
    "http://javadocs.org/"
    (cam/active-region-or-prompt "Search javadocs.org for: "))))

(defun cam/instant-clojure-cheatsheet-search (search-term)
  "Opens Instant Cheatsheet in a new browser tab and searches for SEARCH-TERM."
  (interactive "sSearch Instant Cheatsheet for: ")
  (browse-url
   (concat "http://localhost:13370/#?q="
           (url-hexify-string search-term))))

(provide 'clojure-init)
;;; clojure-init.el ends here
