(require 'python)
(require 'ipython)
;; (require 'info-look)
(require 'auto-complete)
(require 'lisp-init) ; to get my pretty-lambdas function

;; tweaks to use ipython as the default python interpreter
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--pylab" ; preload matplotlib and numpy for interactive use
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
  (setq py-python-command-args (cons "--pylab" py-python-command-args))
  ;; (turn-on-eldoc-mode)
  ;; (diminish 'eldoc-mode)
  (pretty-lambdas))
(add-hook 'python-mode-hook 'cam-python-mode-setup)
(add-hook 'inferior-python-mode-hook 'cam-python-mode-setup)

;; custom keyboard shortcuts
(setq py-mode-map python-mode-map) ; some python modes are looking for keymap under alternate napme (?)
(define-keys py-mode-map
    '() ; nothing yet
    )


;; Use the correct documentation for info lookup (C-h s) ? 
;; (info-lookup-add-help
;;  :mode 'python-mode
;;  :regexp "[[:alnum:]_]+"
;;  :doc-spec '(("(python)Index" nil "")))
 
;; auto-complete-mode
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(provide 'python-init)
