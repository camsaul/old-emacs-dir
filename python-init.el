(require 'flymake)
(require 'python)
(require 'django-mode)
;; (require 'ipython)
(require 'auto-complete)
(require 'lisp-init) ; to get my pretty-lambdas function
(require 'py-autopep8)
(require 'python-pep8)
(require 'cam-functions)

(setq pdb-path '/usr/lib/python2.7/pdb.py
      gud-pdb-command-name (symbol-name pdb-path))

(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline pdb-path
	 		    (file-name-nondirectory buffer-file-name)))))

;; tweaks to use ipython as the default python interpreter
(setq python-shell-interpreter "ipython"
      python-command python-shell-interpreter
      py-shell-name python-shell-interpreter
      python-shell-interpreter-args "-i --pylab=tk" ; preload matplotlib and numpy for interactive use
      python-shell-prompt-regexp "In \\[[0-9]+\\]: "
      python-shell-output-regexp "Out\\[[0-9]+\\]: "
      python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
      python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
      python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s''')))\n"
      )

;; (require 'pymacs)
;; (pymacs-load "ropemacs" "rope-") # Requires pip install ropemacs

;; Make info-look work correctly for python (C-h S)
;; (info-lookup-add-help
;;  :mode 'python-mode
;;  :regexp "[[:alnum:]_]+"
;;  :doc-spec
;;  '(("(python)Index" nil "")))

;; (defun cam-python-mode-setup ()
;;   (django-mode))

(defun run-autopep8 ()
  (interactive)
  (when (or (eq major-mode 'django-mode)
            (eq major-mode 'python-mode))
    (py-autopep8)))

(defun cam-django-mode-setup ()
  (require 'info-look)
  (require 'pydoc-info) ; install python info to /usr/share/info https://bitbucket.org/jonwaltman/pydoc-info/
  (require 'yasnippet)
  (require 'elpy)
  (require 'flymake)
  ;; (require 'smartparens)
  (setq python-check-command "pyflakes")
  (global-mode-setup)
  (highlight-parentheses-mode 1) ; highlight parentheses that surround the current sexpr
  (diminish 'highlight-parentheses-mode)
  ;; (paredit-mode 1)
  ;; (diminish 'paredit-mode " π")
  (auto-complete-mode 1)
  (diminish 'auto-complete-mode)
  (set-face-background 'hl-sexp-face "#111111")
  ;; (setq py-python-command-args (cons python-shell-interpreter-args
  ;; 				     py-python-command-args))
  (turn-on-eldoc-mode)
  (diminish 'eldoc-mode)
  ;; (whitespace-mode 1)
  (pretty-lambdas)
  (elpy-mode 1)              ; !!!! EDITED THIS TO WORK IN DJANGO MODE
  ;; (smartparens-mode 1)
  (electric-pair-mode 1)
  (add-hook 'before-save-hook 'untabify-current-buffer nil t)
  (add-hook 'after-save-hook 'run-isort nil t))

(add-hook 'before-save-hook 'run-autopep8)
(setq py-autopep8-options '("--aggressive" "--ignore" "E501,E401" "-j" "0"))
(setq python-pep8-options '("--format=pylint" "--ignore E501,E401"))

(defun run-isort ()
  "Runs isort on the current buffer in place"
  (interactive)
  (call-process "isort" nil t nil (buffer-file-name) "--order-by-type" "--multi_line" "1" "--lines" "120")
  (revert-buffer t t))

(defun insert-lines (lines)
  "Insert a list of strings calling (newline-and-indent) after each."
  (mapc (lambda (line)
          (insert-string line)
          (newline-and-indent))
        lines))

(defun insert-debug-code ()
  "Insert code print a stacktrace and run the Python debugger at location."
  (interactive)
  (insert-lines '("##### DEBUGGING CODE - NOCOMMIT #####"
                  "import pdb"
                  "import pprint"
                  "import traceback"
                  "pp = lambda o: pprint.PrettyPrinter(indent=2).pprint(o)"
                  "ppo = lambda o: pprint.PrettyPrinter(indent=2).pprint(o.__dict__)"
                  "traceback.print_stack()"
                  "pdb.set_trace()"
                  "##### END DEBUGGING CODE - NOCOMMIT #####")))

(defun insert-time-code ()
  "Insert a lambda to use for profiling Python code."
  (interactive)
  (insert-lines '("##### PROFILING CODE - NOCOMMIT #####"
                  "import time"
                  "def time_method(m):"
                  "t1 = time.time()"
                  "res = m()"
                  "print \"TIME = %.0f ms\" % ((time.time() - t1) * 1000)"
                  "return res"
                  "##### END PROFILING CODE - NOCOMMIT #####")))


;; (add-hook 'python-mode-hook 'cam-python-mode-setup)
;; (add-hook 'python-mode-hook 'cam-django-mode-setup)
(defalias 'cam-python-mode-setup 'cam-django-mode-setup)
(add-hook 'inferior-python-mode-hook 'cam-python-mode-setup)
(add-hook 'django-mode-hook 'cam-django-mode-setup)

;; custom keyboard shortcuts
(setq py-mode-map python-mode-map) ; some python modes are looking for keymap under alternate name (?)
(define-keys py-mode-map
  '(("<f5>" flymake-display-err-menu-for-current-line)
    ("<f6>" flymake-goto-next-error)
    ("<f7>" flymake-mode)
    ("<f8>" info-lookup-symbol)
    ("<s-mouse-1>" elpy-goto-definition)
    ("<M-mouse-1>" elpy-doc)
    ("<S-mouse-1>" info-lookup-symbol)
    ))

;; auto-complete-mode
(add-to-list 'auto-mode-alist '("\\.py\\'" . django-mode))
(add-to-list 'interpreter-mode-alist '("python" . django-modee))

(provide 'python-init)
