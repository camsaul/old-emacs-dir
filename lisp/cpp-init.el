;; automatically open .h files with #include without .h at the end as c++ instead of c
;; or open header files that have std:: somewhere in them
(add-to-list 'magic-mode-alist
             `(,(lambda ()
                  (and (string= (file-name-extension buffer-file-name) "h")
                       (or (re-search-forward "#include <\\w+>"
                                              magic-mode-regexp-match-limit t)
                           (re-search-forward "std::"
                                              magic-mode-regexp-match-limit t))))
               . c++-mode))

(mapc (lambda (ele) (add-to-list 'auto-mode-alist ele))
      '(("\.hpp$" . c++-mode)
        ("\.cpp$" . c++-mode)))

(defun cam/c++-search ()
  "Lookup C+++ symbol on cplusplus.com"
  (interactive)
  (browse-url
   (concat
    "http://www.cplusplus.com/search.do?q="
    (url-hexify-string (cam/current-token)))))

(defun cam/c++-mode-setup ()
  (require 'find-file)
  (require 'flymake)
  (cam/enable-minor-modes
    electric-pair-mode
    (highlight-parentheses-mode . nil)
    flymake-mode
    subword-mode)
  (setq tab-width 4
        c-basic-indent 4
        c-basic-offset 4))

(add-hook 'c++-mode-hook 'cam/c++-mode-setup)

(defun cam/find-tag-at-mark ()
  (interactive)
  (find-tag (cam/current-token)))

(defun cam/find-tag-at-mouse-mark (event)
  (interactive "e")
  (progn (mouse-set-point event)
         (cam/find-tag-at-mark)))

(eval-after-load "cc-mode"
  '(cam/define-keys c++-mode-map
     "M-;" #'comment-region
     "<f5>" #'flymake-display-err-menu-for-current-line
     "<f6>" #'flymake-goto-next-error
     "<f7>" #'flymake-mode
     "<f8>" #'cam/c++-search
     "<S-mouse-1>" #'cam/c++-search
     "C-c C-k" #'flymake-compile
     "<C-M-up>" #'cam/c++-jump-to-header
     "<C-M-down>" #'cam/c++-jump-to-implementation
     "<s-mouse-1>" #'cam/find-tag-at-mouse-mark
     "<mouse-2>" #'cam/find-tag-at-mouse-mark ; the mouse wheel
     "M-." #'cam/find-tag-at-mark))

(defun cam/c++-jump-to-header ()
  (interactive)
  (find-file (concat (car (split-string (buffer-file-name) "\\."))
                     ".h")))

(defun cam/c++-jump-to-implementation ()
  (interactive)
  (find-file (concat (car (split-string (buffer-file-name) "\\."))
                     ".cpp")))

(provide 'cpp-init)
