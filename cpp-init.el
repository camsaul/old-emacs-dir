(require 'cc-mode) ; c++-mode
;; (require 'auto-complete-config)
;; (add-hook 'auto-complete-mode-hook 'ac-common-setup)

;; (add-to-list 'load-path "~/emacs.d/AC")
;; (add-to-list 'ac-dictionary-directories "~/emacs.d/AC/ac-dict")

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

(add-to-list 'auto-mode-alist '("\.hpp$" . c++-mode))
(add-to-list 'auto-mode-alist '("\.cpp$" . c++-mode))

(defun c++-search ()
  "Lookup C+++ symbol on cplusplus.com"
  (interactive)
  (browse-url
   (concat
    "http://www.cplusplus.com/search.do?q="
    (url-hexify-string (current-token)))))

(defun c++-mode-setup ()
  (require 'auto-complete)
  (require 'find-file)
  (require 'flymake)
  ;; (require 'auto-complete-clang)
  ;; (require 'yasnippet)
  (global-mode-setup)
  (setq tab-width 4)
  (setq c-basic-indent 4)
  (setq c-basic-offset 4)
  (subword-mode 1)
  (flymake-mode 1)
  (electric-pair-mode +1)
  (auto-complete-mode 1)
  (highlight-parentheses-mode 1) ; highlight parentheses that surround the current sexpr
  (diminish 'highlight-parentheses-mode)
  (set-face-background 'hl-sexp-face "#111111")
  (require 'auto-complete-clang-async)
  ;; (require 'ac-etags)
  ;; (ac-etags-setup)
  ;; (ac-etags-ac-setup)
  (setq ac-clang-complete-executable "~/.emacs.d/emacs-clang-complete-async/clang-complete") ; make LLVM_CONFIG=/usr/local/Cellar/llvm/3.3/bin/llvm-config (via homebrew)
  (setq ac-sources '(ac-source-clang-async)) ;  ac-source-yasnippet
  (ac-clang-launch-completion-process))

(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'c++-mode))

(add-hook 'c++-mode-hook 'c++-mode-setup)

(defun find-tag-at-mark ()
  (interactive)
  (find-tag (current-token)))

(defun find-tag-at-mouse-mark (event)
  (interactive "e")
  (progn (mouse-set-point event)
         (find-tag-at-mark)))

(define-keys c++-mode-map
  '(("M-;" comment-region)
    ("<f5>" flymake-display-err-menu-for-current-line)
    ("<f6>" flymake-goto-next-error)
    ("<f7>" flymake-mode)
    ("<f8>" c++-search)
    ("<S-mouse-1>" c++-search)
    ("C-c C-k" flymake-compile)
    ("<C-M-up>" c++-jump-to-header)
    ("<C-M-down>" c++-jump-to-implementation)
    ("<s-mouse-1>" find-tag-at-mouse-mark)
    ("<mouse-2>" find-tag-at-mouse-mark) ; the mouse wheel
    ("M-." find-tag-at-mark)
    ))

(defun c++-jump-to-header ()
  (interactive)
  (find-file (concat (car (split-string (buffer-file-name) "\\."))
		     ".h")))

(defun c++-jump-to-implementation ()
  (interactive)
  (find-file (concat (car (split-string (buffer-file-name) "\\."))
		     ".cpp")))

(provide 'cpp-init)
