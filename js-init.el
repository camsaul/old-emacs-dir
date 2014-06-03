(require 'js2-mode)

(defun cam-js-mode-setup ()
  (global-mode-setup)
  (pretty-function)
  (electric-layout-mode)
  )

(add-to-list 'auto-mode-alist '("\.js" . js2-mode)) ; use js2-mode instead of js-mode

(add-hook 'js2-mode-hook 'cam-js-mode-setup)

(defun pretty-function ()
  "turns function into a fancy f symbol"
  (font-lock-add-keywords
   nil `(("\\(\\<function\\>\\)"
	  (0 (progn (compose-region (match-beginning 1)
				    (match-end 1)
				    "\u0192"
				    'decompose-region)))))))

(setq js-indent-level 2)
(provide 'js-init)
