;;; json-init -- Initialization for editing JSON
;;; Commentary:

;;; Code:

(cam/eval-after-load "json-mode"
  (require 'editorconfig))

(add-hook 'json-mode-hook
  (lambda ()
    (cam/enable-minor-modes
      (highlight-parentheses-mode . nil)
      hl-sexp-mode
      (paredit-mode . " π"))
    (condition-case nil
        (flymake-json-load)
      (error (warn "Couldn't launch flymake-json because jsonlint wasn't installed. Attempting to install...")
             (condition-case err
                 (call-process-shell-command "sudo npm install -g jsonlint" nil nil)
               (error (warn (concat "Failed to install jsonlint: "
                                    (error-message-string err)))))))

    (add-hook 'before-save-hook
      (lambda ()
        (cam/untabify-current-buffer))
      nil t)))

(provide 'json-init)
;;; json-init.el ends here
