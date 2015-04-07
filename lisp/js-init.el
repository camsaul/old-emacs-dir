;; -*- lexical-binding: t -*-
;; -*- comment-column: 50; -*-

;;; js-init -- Settings for editing JavaScript
;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'js3-mode))
(mapc #'require '(cam-functions
                  cam-macros
                  flycheck
                  web-beautify))

(add-hook 'js3-mode-hook
  (lambda ()
    (cam/declare-vars highlight-parentheses-mode)
    (cam/enable-minor-modes
      (company-mode . " ¢")
      (flycheck-mode . " ✔")
      highlight-parentheses-mode
      ;; aggressive-indent-mode
      )
    (setq company-dabbrev-downcase nil)             ; company mode by default downcases completion suggestions, obviously we don't want that
    (cam/pretty-function)
    ;; run js-beautify on buffer when saving, requires npm install -g js-beautify
    ;; TODO: web-beautfiy-js-buffer for json-mode, web-beautify-html-buffer for html-mode; web-beautify-css-buffer for css-mode ?
    (add-hook 'before-save-hook 'web-beautify-js-buffer :append :local)))

(cam/eval-after-load "web-beautify"
  (defadvice web-beautify-js-buffer (around web-beautify-js-buffer-save-window-config activate)
    "Save the current window configuration when running web-beautify-js-buffer"
    (make-local-variable '-original-window-point) ; we'll track the point each JS3-mode buffer was at so we can jump back to it after running web-beautify-js-buffer
    (let* ((current-buffer-windows (-filter (lambda (window)
                                              (eq (window-buffer window) (current-buffer)))
                                            (window-list)))
           (window-points (mapcar #'window-point
                                  current-buffer-windows))
           (window-starts (mapcar #'window-start
                                  current-buffer-windows)))
      ad-do-it
      (mapc (-lambda ((window wpoint wstart))
              (set-window-start window wstart nil) ; noforce=nil -> move even if it would move point offscreen
              (set-window-point window wpoint))
            (->> (-interleave current-buffer-windows window-points window-starts)
                 (-partition 3))))))

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
    "C-M-b" (lambda ()
              (interactive)
              (js3-mode-forward-sexp -1))
    "C-M-f" #'js3-mode-forward-sexp
    "C-j" (cam/interactivify 'js3-insert-and-indent)
    "M-q" #'cam/js-reindent-previous-sexp
    "A-l" #'cam/js-log-var))

(defun cam/pretty-function ()
  "Turn function into a fancy f symbol."
  (font-lock-add-keywords
   nil `(("\\(\\<function\\>\\)"
        (0 (progn (compose-region (match-beginning 1)
                                  (match-end 1)
                                  "\u0192"
                                  'decompose-region)))))))

(defun cam/js-log-var (varnames)
  "Insert a console.log statement for given variable(s)."
  (interactive "sVariable Name(s) (separate by spaces): ")
  (mapc (lambda (varname)
          (insert (format "console.log('%s -------------------->', %s);" varname varname))
          (newline-and-indent))
        (s-split " " varnames)))

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
