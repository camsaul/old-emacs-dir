(provide 'js-init)

(defun cam-js-mode-setup ()
  (global-mode-setup)
  (pretty-function)
  (electric-layout-m))

(add-hook 'js-mode-hook 'cam-js-mode-setup)

;; turns the word "function" into a fancy f symbol
(defun pretty-function ()
  (font-lock-add-keywords
   'js-mode `(("\\(function *\\)("
	       (0 (progn (compose-region (match-beginning 1)
					 (match-end 1) "\u0192")
			 nil))))))

(setq js-indent-level 2)
