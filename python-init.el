(require 'python)
;; (require 'ipython)
;; (require 'info-look)
(require 'auto-complete)
(require 'lisp-init) ; to get my pretty-lambdas function

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

(defun cam-python-mode-setup ()
  (global-mode-setup)
  (highlight-parentheses-mode 1) ; highlight parentheses that surround the current sexpr
  (diminish 'highlight-parentheses-mode)
  (paredit-mode 1)
  (diminish 'paredit-mode " π")
  (auto-complete-mode 1)
  (diminish 'auto-complete-mode)  
  ;; (setq py-python-command-args (cons python-shell-interpreter-args
  ;; 				     py-python-command-args))
  ;; (turn-on-eldoc-mode)
  ;; (diminish 'eldoc-mode)
  (pretty-lambdas))
(add-hook 'python-mode-hook 'cam-python-mode-setup)
(add-hook 'inferior-python-mode-hook 'cam-python-mode-setup)

;; custom keyboard shortcuts
(setq py-mode-map python-mode-map) ; some python modes are looking for keymap under alternate napme (?)
(define-keys py-mode-map
  '(("<f5>" flymake-display-err-menu-for-current-line)
    ("<f6>" flymake-goto-next-error)
    ("<f7>" flymake-mode))		; nothing yet
  )


;; Use the correct documentation for info lookup (C-h s) ? 
;; (info-lookup-add-help
;;  :mode 'python-mode
;;  :regexp "[[:alnum:]_]+"
;;  :doc-spec '(("(python)Index" nil "")))
 
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
      (list "epylint" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
	       '("\\.py\\'" flymake-pylint-init)))

(provide 'python-init)
