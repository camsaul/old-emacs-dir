;;; common-lisp-init -- setup for developing Common Lisp
;;; Commentary:
;;; Code:

(eval-after-load "slime"
  '(setq inferior-lisp-program "/usr/local/bin/sbcl"))

(provide 'common-lisp-init)
;;; common-lisp-init.el ends here
