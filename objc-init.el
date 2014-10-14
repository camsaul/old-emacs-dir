(require 'cc-mode)
(require 'find-lisp)
(require 'find-file)

;; Automatically open .h files with @interface declarations as obj-c rather than c
(add-to-list 'magic-mode-alist
             `(,(lambda ()
                  (and (string= (file-name-extension buffer-file-name) "h")
                       (re-search-forward "@\\<interface\\>"
                                          magic-mode-regexp-match-limit t)))
               . objc-mode))

;; .m and .mm files to cc-other-file-find-alist
(nconc (cadr (assoc "\\.h\\'" cc-other-file-alist)) '(".m" ".mm"))

(defun objc-mode-setup ()
  (global-mode-setup)
  (setq tab-width 4)
  (setq c-basic-indent 4)
  (setq c-basic-offset 4)
  (c-set-offset 'case 4)
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

(defun objc-jump-to-header ()
  (interactive)
  (find-file (concat (car (split-string (buffer-file-name) "\\."))
                     ".h")))

(defun objc-jump-to-implementation ()
  (interactive)
  (find-file (concat (car (split-string (buffer-file-name) "\\."))
                     ".m")))

(provide 'objc-init)
