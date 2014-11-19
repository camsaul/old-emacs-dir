;; -*- comment-column: 50; -*-

;;; js-init -- Settings for editing JavaScript
;;; Commentary:

;;; Code:

(require 'cam-functions)

(add-to-list 'auto-mode-alist '("\.js$" . js3-mode)) ; use js3-mode instead of js-mode

(add-hook 'js3-mode-hook
  (lambda ()
    (require 'web-beautify)
    (cam/declare-vars highlight-parentheses-mode)
    (cam/enable-minor-modes
      (company-mode . " ¢")
      highlight-parentheses-mode)
    (pretty-function)
    ;; run js-beautify on buffer when saving, requires npm install -g js-beautify
    ;; TODO: web-beautfiy-js-buffer for json-mode, web-beautify-html-buffer for html-mode; web-beautify-css-buffer for css-mode ?
    (add-hook 'before-save-hook 'web-beautify-js-buffer t t)))

(defmacro cam/interactivify (func)
  `(lambda ()
     (interactive)
     (call-interactively ,func)))

(cam/eval-after-load "js3-mode"
  (require 'editorconfig)
     (cam/declare-vars js3-auto-indent-p
                       js3-enter-indents-newline
                       js3-consistent-level-indent-inner-bracket)
     (setq-default
         js3-auto-indent-p t                           ; commas "right themselves" (?)
         js3-enter-indents-newline t
         js3-consistent-level-indent-inner-bracket t)  ; make indentation level inner bracket consitent rather than aligning to beginning bracket position)

     (cam/define-keys js3-mode-map
       "C-j" (cam/interactivify 'js3-insert-and-indent)
       "M-q" 'cam/js-reindent-previous-sexp))

(defun pretty-function ()
  "Turn function into a fancy f symbol."
  (font-lock-add-keywords
   nil `(("\\(\\<function\\>\\)"
          (0 (progn (compose-region (match-beginning 1)
                                    (match-end 1)
                                    "\u0192"
                                    'decompose-region)))))))

(defun cam/js-reindent-previous-sexp ()
  "Reindent sexp before point."
  (interactive)
  (save-excursion
    (let ((end-line-num (line-number-at-pos (point))))
      (backward-sexp)
      (let ((line-nums (number-sequence (line-number-at-pos (point))
                                        end-line-num)))
        (mapc (lambda (line)
                (goto-line line)
                (indent-according-to-mode))
              line-nums)))))


(provide 'js-init)
;;; js-init.el ends here
