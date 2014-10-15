(defun cam-js-mode-setup ()
  (require 'js3-mode)
  (cam-enable-minor-modes
    (company-mode . " ¢")
    highlight-parentheses-mode)
  (pretty-function))

(eval-after-load "js3-mode"
  '(setq
    js3-auto-indent-p t                        ; commas "right themselves" (?)
    js3-enter-indents-newline t
   js3-consistent-level-indent-inner-bracket t ; make indentation level inner bracket consitent rather than aligning to beginning bracket position)
))

(add-to-list 'auto-mode-alist '("\.js$" . js3-mode)) ; use js3-mode instead of js-mode

(add-hook 'js3-mode-hook 'cam-js-mode-setup)

(defun pretty-function ()
  "turns function into a fancy f symbol"
  (font-lock-add-keywords
   nil `(("\\(\\<function\\>\\)"
          (0 (progn (compose-region (match-beginning 1)
                                    (match-end 1)
                                    "\u0192"
                                    'decompose-region)))))))

(provide 'js-init)
