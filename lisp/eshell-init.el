;; -*- lexical-binding: t; comment-column: 60; -*-
;;; Code:

(mapc #'require '(eshell
                  cam-macros))

(defun cam/eshell-clear ()
  "Clear previous text in eshell."
  (interactive)
  (comint-kill-region (point-min) (point-max))
  (eshell-send-input))

(defun cam/eshell-setup ()
  (cam/define-keys eshell-mode-map
    "C-c M-o" #'cam/eshell-clear))

(add-hook 'eshell-mode-hook #'cam/eshell-setup)

(setq eshell-ls-initial-args '("-a"))                       ; list of args to pass to ls (default = nil)

(provide 'eshell-init)
;;; eshell-init.el ends here
