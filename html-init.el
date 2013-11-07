(require 'nxml-mode)

(defun html-mode-setup ()
  (global-mode-setup)
  (nxml-mode)) ; fancy xml editing mode
(add-hook 'html-mode-hook 'html-mode-setup)

(provide 'html-init)
