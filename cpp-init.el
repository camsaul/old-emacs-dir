(require 'cc-mode) ; c++-mode
(require 'find-file)
(require 'flymake)

;; Automatically open .h files with #include without .h at the end as c++ instead of c
;; or open header files that have std:: somewhere in them
(add-to-list 'magic-mode-alist
	     `(,(lambda ()
		  (and (string= (file-name-extension buffer-file-name) "h")
		       (or (re-search-forward "#include <\w+>" 
					      magic-mode-regexp-match-limit t)
			   (re-search-forward "std::" 
					      magic-mode-regexp-match-limit t))))
	       . c++-mode))

(add-to-list 'auto-mode-alist '("\.hpp$" . c++-mode))
(add-to-list 'auto-mode-alist '("\.cpp$" . c++-mode))

(defun c++-mode-setup ()
  (global-mode-setup)
  (setq tab-width 4)
  (setq c-basic-indent 4)
  (setq c-basic-offset 4)
  (subword-mode 1)
  (flymake-mode 1)
  (auto-complete-mode 1))
(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'c++-mode))

(add-hook 'c++-mode-hook 'c++-mode-setup)

(define-keys c++-mode-map
  '(("M-;" comment-region)
    ("<f3>" flymake-display-err-menu-for-current-line)
    ("<f4>" flymake-goto-next-error)
    ("C-c f" flymake-mode)
    ("C-c C-k" flymake-compile)
    ("<C-M-up>" c++-jump-to-header)
    ("<C-M-down>" c++-jump-to-implementation)))

(defun c++-jump-to-header ()
  (interactive)
  (find-file (concat (car (split-string (buffer-file-name) "\\."))
		     ".h")))

(defun c++-jump-to-implementation ()
  (interactive)
  (find-file (concat (car (split-string (buffer-file-name) "\\."))
		     ".cpp")))

(provide 'cpp-init)
