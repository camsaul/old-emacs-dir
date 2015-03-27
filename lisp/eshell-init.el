(mapc #'require '(eshell
                  cam-macros))

(defun cam/eshell-clear ()
  "Clear previous text in eshell."
  (interactive)
  (comint-kill-region (point-min) (point-max))
  (eshell-send-input))

(defun cam/eshell-setup ()
  (cam/define-keys eshell-mode-map
    "C-x M-o" #'cam/eshell-clear))

(add-hook 'eshell-mode-hook #'cam/eshell-setup)

(provide 'eshell-init)
