(provide 'objc-init)
(require 'cc-mode)
(require 'find-lisp)

;; Automatically open .h files with @interface declarations as obj-c rather than c
(add-to-list 'magic-mode-alist
	     `(,(lambda ()
		  (and (string= (file-name-extension buffer-file-name) "h")
		       (re-search-forward "@\\<interface\\>" 
					  magic-mode-regexp-match-limit t)))
	       . objc-mode))

(defun objc-mode-setup ()
  (global-mode-setup)
  (setq tab-width 4)
  (setq c-basic-indent 4)
  (subword-mode 1)
  (auto-complete-mode 1))
(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'objc-mode))

(add-hook 'objc-mode-hook 'objc-mode-setup)

(define-keys objc-mode-map
	  '(("M-;" comment-region)
	    ("<C-M-up>" objc-jump-to-header)
	    ("<C-M-down>" objc-jump-to-implementation)))

(defun find-h-and-m-files (dir)
  (interactive)
  (let ((files (find-lisp-find-files dir "\\.[h\|m]")))
    (mapcar (lambda (f) (car (last (split-string f (file-truename dir)))))
	    files)))

(easy-menu-define ryde-menu objc-mode-map "Ryde"
  (cons "Ryde"
	(let ((ryde-dir "~/Ryde/MobileMuni"))
	  (mapcar (lambda (f) (menu-edit-file f (concat ryde-dir f)))
		  (find-h-and-m-files ryde-dir)))))

(defun objc-jump-to-header ()
  (interactive)
  (find-file (concat (car (split-string (buffer-file-name) "\\."))
		     ".h")))

(defun objc-jump-to-implementation ()
  (interactive)
  (find-file (concat (car (split-string (buffer-file-name) "\\."))
		     ".m")))
