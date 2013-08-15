(provide 'ruby-init)

(defun cam-ruby-mode-setup ()
  ; nothing yet
  )
(add-hook 'ruby-mode-hook 'cam-ruby-mode-setup)

(add-to-list 'auto-mode-alist '("\.podspec$" . ruby-mode)) ; edit Podspec files in ruby mode
