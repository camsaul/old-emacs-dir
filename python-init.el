;; -*- comment-column: 70; -*-

;;;; FILE PATTERNS

(add-to-list 'auto-mode-alist '("\\.py\\'" . django-mode))
(add-to-list 'interpreter-mode-alist '("python" . django-mode))


;;;; AUTOLOADS

(cam-setup-autoloads
  ("lisp-init" pretty-lambdas))

;; KEY BINDINGS
(defun cam/define-python-keys (mode-map)
  (define-keys mode-map
    '(("<f5>" flymake-display-err-menu-for-current-line)
      ("<f6>" flymake-goto-next-error)
      ("<f7>" flymake-mode)
      ("<f8>" info-lookup-symbol)
      ("<s-mouse-1>" elpy-goto-definition)
      ("<M-mouse-1>" elpy-doc)
      ("<S-mouse-1>" info-lookup-symbol))))


;;;; EVAL-AFTER-LOAD SETTINGS

(eval-after-load "python-mode"
  '(progn
     (setq gud-pdb-command-name (symbol-name pdb-path)
           pdb-path '/usr/lib/python2.7/pdb.py
           py-mode-map python-mode-map
           python-check-command "pyflakes"
           py-autopep8-options '("--aggressive" "--ignore" "E501,E401" "-j" "0")
           py-shell-name python-shell-interpreter
           python-command python-shell-interpreter
           python-pep8-options '("--format=pylint" "--ignore E501,E401")
           python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
           python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
           python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s''')))\n"
           python-shell-interpreter "ipython"
           python-shell-interpreter-args "-i --pylab=tk"              ; preload matplotlib and numpy for interactive use
           python-shell-output-regexp "Out\\[[0-9]+\\]: "
           python-shell-prompt-regexp "In \\[[0-9]+\\]: "             ; some python modes are looking for keymap under alternate name (?)
           )
     (cam/define-python-keys python-mode-map)))

(eval-after-load "django-mode"
  '(cam/define-python-keys django-mode-map))


;;;; MODE SETUP

(defun cam-django-mode-setup ()
  (mapc 'require '(
                   ;; django-mode
                   elpy
                   flymake
                   ; jedi - see http://tkf.github.io/emacs-jedi/latest/#configuration
                   info-look
                   py-autopep8
                   pydoc-info ; install python info to /usr/share/info https://bitbucket.org/jonwaltman/pydoc-info/
                   python-pep8
                   yasnippet))
  (cam-enable-minor-modes
    (company-mode . " ¢")
    electric-pair-mode
    elpy-mode
    (highlight-parentheses-mode . nil))
  (turn-on-eldoc-mode)
  (diminish 'eldoc-mode)
  (pretty-lambdas)
  ;; (jedi:setup)

  ;; HOOKS
  (add-hook 'before-save-hook
            (lambda ()
              (untabify-current-buffer)
              (run-autopep8))
            nil t)
  (add-hook 'after-save-hook 'run-isort nil t))

(defalias 'cam-python-mode-setup 'cam-django-mode-setup)
(add-hook 'inferior-python-mode-hook 'cam-python-mode-setup)
(add-hook 'django-mode-hook 'cam-django-mode-setup)


;;;; ADVICE

(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline pdb-path
                            (file-name-nondirectory buffer-file-name)))))


;;;; FUNCTIONS

(defun run-autopep8 ()
  (interactive)
  (when (or (eq major-mode 'django-mode)
            (eq major-mode 'python-mode))
    (py-autopep8)))


(defun run-isort ()
  "Runs isort on the current buffer in place"
  (interactive)
  (call-process "isort" nil t nil (buffer-file-name) "--order-by-type" "--multi_line" "1" "--lines" "120")
  (revert-buffer t t))

(defun insert-lines (lines)
  "Insert a list of strings calling (newline-and-indent) after each."
  (mapc (lambda (line)
          (insert line)
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
                  "def timed(fn):"
                  "from functools import wraps"
                  "import time"
                  "from colorama import Fore, Style"
                  "@wraps(fn)"
                  "def timed_fn(*args, **kwargs):"
                  "t1 = time.time()"
                  "res = fn(*args, **kwargs)"
                  "print Style.BRIGHT + Fore.MAGENTA + \"%s took %.0f ms\" % (fn.__name__, (time.time() - t1) * 1000) + Style.RESET_ALL"
                  "return res"))
  (python-indent-dedent-line)
  (insert-lines '("return timed_fn"))
  (python-indent-dedent-line)
  (insert-lines '("##### END PROFILING CODE - NOCOMMIT #####")))

(provide 'python-init)
