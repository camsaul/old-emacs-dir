(require 'python)
;; (require 'ipython)
(require 'info-look)
(require 'auto-complete)
(require 'lisp-init) ; to get my pretty-lambdas function
(require 'elpy)
(require 'flymake)
;; (require 'outline-magic)
;; (require 'python-magic)

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

(defun cam-python-mode-setup ()
  (require 'pydoc-info)
  (global-mode-setup)
  (highlight-parentheses-mode 1) ; highlight parentheses that surround the current sexpr
  (diminish 'highlight-parentheses-mode)
  ;; (paredit-mode 1)
  ;; (diminish 'paredit-mode " π")
  (auto-complete-mode 1)
  (diminish 'auto-complete-mode)  
  ;; (setq py-python-command-args (cons python-shell-interpreter-args
  ;; 				     py-python-command-args))
  (turn-on-eldoc-mode)
  (diminish 'eldoc-mode)
  (electric-pair-mode +1)
  (pretty-lambdas)
  (elpy-mode))

(add-hook 'python-mode-hook 'cam-python-mode-setup)
(add-hook 'inferior-python-mode-hook 'cam-python-mode-setup)

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
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; flymake

(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
	       '("\\.py\\'" flymake-pylint-init)))

(provide 'python-init)
