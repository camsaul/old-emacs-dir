(defvar highlight-error-keywords-pattern '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|DEPRECATED\\|NOCOMMIT\\)"
                                            1 font-lock-warning-face t))
  "The pattern used by highlight-error-keywords-mode for font-lock-add/remove-keywords.")

(defvar highlight-error-keywords-mode-is-enabled-p nil
  "Track whether we've performed actions for enabling/disabling this minor mode yet")
(make-variable-buffer-local 'highlight-error-keywords-mode-is-enabled-p)

(defvar highlight-error-keywords-mode nil)
(define-minor-mode highlight-error-keywords-mode
  "Make various words such as HACK, TODO, NOCOMMIT appear in warning face."
  (let ((enable highlight-error-keywords-mode)
        (enabled highlight-error-keywords-mode-is-enabled-p))
    (unless (equal enable enabled)
      (setq highlight-error-keywords-mode-is-enabled-p enable)
      (funcall (if enable 'font-lock-add-keywords 'font-lock-remove-keywords) major-mode highlight-error-keywords-pattern))))

(highlight-error-keywords-mode 1)

(provide 'highlight-error-keywords)
