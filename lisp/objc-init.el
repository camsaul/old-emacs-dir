(cam/setup-autoloads
  ("find-lisp" find-lisp-find-files))

;; Automatically open .h files with @interface declarations as obj-c rather than c
(add-to-list 'magic-mode-alist
             `(,(lambda ()
                  (and (string= (file-name-extension buffer-file-name) "h")
                       (re-search-forward "@\\<interface\\>"
                                          magic-mode-regexp-match-limit t)))
               . objc-mode))

;; add .m and .mm files to objc-mode
(mapc (lambda (item) (add-to-list 'auto-mode-alist item))
      '((".m$" . objc-mode)
        (".mm$" . objc-mode)))

(defun objc-mode-setup ()
  (cam/enable-minor-modes
    subword-mode))

(eval-after-load "objc-mode"
  '(progn
     (require 'cc-mode)
     (setq tab-width 4
           c-basic-indent 4
           c-basic-offset 4)
     (c-set-offset 'case 4)

     ;; tell cc-mode how to jump between .h <-> .m/.mm files
     (nconc (cadr (assoc "\\.h\\'" cc-other-file-alist))
            '(".m" ".mm"))
     (nconc cc-other-file-alist
            '(("\\.m\\'" (".h"))
              ("\\.mm\\'" (".h"))))))

(add-hook 'objc-mode-hook 'objc-mode-setup)

(eval-after-load "cc-mode"
  '(cam/define-keys objc-mode-map
     "M-;" #'comment-region
     "<C-M-up>" #'objc-jump-to-header
     "<C-M-down>" #'objc-jump-to-implementation))

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
