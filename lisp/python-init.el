;;; python-init -- Setup for editing Python
;; -*- comment-column: 70; -*-

;;; Commentary:

;;; Code:

(require 'cam-functions)


;;;; FILE PATTERNS

(add-to-list 'auto-mode-alist '("\\.py\\'" . django-mode))
(add-to-list 'interpreter-mode-alist '("python" . django-mode))


;;;; AUTOLOADS

(cam-setup-autoloads
  ("lisp-init" pretty-lambdas))

;; KEY BINDINGS
(defun cam/define-python-keys (mode-map)
  "Add python-related key bindings to MODE-MAP."
  (define-keys mode-map
    '(("<f5>" flymake-display-err-menu-for-current-line)
      ("<f6>" flymake-goto-next-error)
      ("<f7>" flymake-mode)
      ("<f8>" info-lookup-symbol)
      ("<s-mouse-1>" elpy-goto-definition)
      ("<M-mouse-1>" elpy-doc)
      ("<S-mouse-1>" info-lookup-symbol))))


;;;; EVAL-AFTER-LOAD SETTINGS

(defun cam/python-install-pip-reqs ()
  "Install pip requirements needed for running elpy, jedi, etc."
  (call-process-shell-command
   "pip install -U pep8 autopep8 flake8 pyflakes rope ropemacs jedi epc"
   nil ; input file
   nil)) ; output. nil = discard, 0 = discard, return immediately (process runs async)

(defvar cam/has-initialized-python-p nil
  "Whether we've done the \"eval-after-load\" stuff for python/django mode yet.")

(defun cam/initialize-python-if-needed ()
  "Initial (one-time) setup for python-mode/django-mode/etc."
  (unless cam/has-initialized-python-p
    (cam/python-install-pip-reqs)
    (mapc 'require '(elpy
                     flymake
                     info-look
                     py-autopep8
                     pydoc-info                                         ; install python info to /usr/share/info https://bitbucket.org/jonwaltman/pydoc-info/
                     python-pep8
                     yasnippet
                     anaconda-mode
                     company-anaconda))

    (setq-default
        cam/has-initialized-python-p t
        py-autopep8-options '("--aggressive" "--ignore" "E501,E401" "-j" "0")
        python-check-command "pyflakes"
        python-pep8-options '("--format=pylint" "--ignore E501,E401")
        python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
        python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s''')))\n"
        python-shell-completion-string-code "';'.join(module_completion('''%s'''))\n"
        python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --pylab=tk"                 ; preload matplotlib and numpy for interactive use
        python-shell-prompt-regexp "In \\[[0-9]+\\]: "                ; some python modes are looking for keymap under alternate name (?)
        )

    ;; TODO we're not even using flycheck w/ python, we're using flymake
    (eval-after-load "flycheck"
      '(setq-default
           flycheck-flake8-maximum-line-length 200))

    (require 'jedi)                                                       ; see http://tkf.github.io/emacs-jedi/latest/#configuration
    (jedi:install-server)))

(cam/eval-after-load "python-mode"
  (cam/initialize-python-if-needed)
  (cam/define-python-keys python-mode-map))

(cam/eval-after-load "django-mode"
  (cam/initialize-python-if-needed)
  (cam/define-python-keys django-mode-map))


;;;; MODE SETUP

(defun cam-django-mode-setup ()
  "Code to execute as part of python/django-mode-hook."
  (require 'anaconda-mode)
  (require 'company)
  (require 'company-anaconda)

  (condition-case nil
      (flymake-mode)
    (error (cam/python-install-pip-reqs)))

  (let ((major-mode 'python-mode)) ; elpy checks major-mode and won't work for django-mode
    (elpy-mode))

  (cam-enable-minor-modes
    (company-mode . " ¢")
    eldoc-mode
    electric-indent-mode
    electric-pair-mode
    (highlight-parentheses-mode . nil)
    anaconda-mode
    pretty-symbols-mode)
  (jedi:setup)

  ;; don't prompt for confirmation when killing elpy process
  (mapc (lambda (process)
          (when (string-match-p "elpy-rpc" (process-name process))
            (set-process-query-on-exit-flag process nil)))
        (process-list))

  ;; seriously, I don't want to use auto-complete-mode
  (when (and (boundp 'auto-complete-mode)
             auto-complete-mode)
    (auto-complete-mode -1))

  ;; delete vars that might be buffer local and overriding our settings
  (mapc #'kill-local-variable
        '(py-autopep8-options
          flycheck-flake8-maximum-line-length
          python-check-command
          python-pep8-options
          python-shell-completion-setup-code
          python-shell-completion-string-code
          python-shell-completion-string-code
          python-shell-interpreter
          python-shell-interpreter-args
          python-shell-prompt-regexp))

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
  "Call py-autopep8 when \"major-mode\" is django-mode or python-mode."
  (interactive)
  (when (or (eq major-mode 'django-mode)
            (eq major-mode 'python-mode))
    (py-autopep8)))


(defun run-isort ()
  "Run isort on the current buffer in place."
  (interactive)
  (call-process "isort" nil t nil (buffer-file-name) "--order-by-type" "--multi_line" "1" "--lines" "120")
  (revert-buffer t t t))

(defun insert-lines (lines)
  "Insert a list of LINES calling (newline-and-indent) after each."
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
                  "_pp = lambda o: pprint.PrettyPrinter(indent=2).pprint(o)"
                  "pp = lambda o: _pp(o.__dict__) if hasattr(o, '__dict__') else _pp(o)"
                  "traceback.print_stack()"
                  "pdb.set_trace()"
                  "##### END DEBUGGING CODE - NOCOMMIT #####")))

(defun insert-pretty-print-code ()
  "Insert code print a stacktrace and run the Python debugger at location."
  (interactive)
  (insert-lines '("##### DEBUGGING CODE - NOCOMMIT #####"
                  "import pprint"
                  "_pp = lambda o: pprint.PrettyPrinter(indent=2).pprint(o)"
                  "pp = lambda o: _pp(o.__dict__) if hasattr(o, '__dict__') else _pp(o)"
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
;;; python-init.el ends here
