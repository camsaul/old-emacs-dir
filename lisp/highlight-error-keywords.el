;;; highlight-error-keywords -- Minor mode to add certain error keywords to major mode's font locking
;;; Commentary:
;;; Code:

(defvar highlight-error-keywords-pattern

  "The pattern used by highlight-error-keywords-mode for font-lock-add/remove-keywords.")

(defvar highlight-error-keywords-mode-is-enabled-p nil
  "Track whether we've performed actions for enabling/disabling this minor mode yet.")
(make-variable-buffer-local 'highlight-error-keywords-mode-is-enabled-p)

(defvar highlight-error-keywords-mode nil)
(define-minor-mode highlight-error-keywords-mode
  "Make various words such as HACK, TODO, NOCOMMIT appear in warning face."
  (let ((enable highlight-error-keywords-mode)
        (enabled highlight-error-keywords-mode-is-enabled-p))
    (unless (equal enable enabled)
      (setq highlight-error-keywords-mode-is-enabled-p enable)
      (funcall (if enable 'font-lock-add-keywords 'font-lock-remove-keywords) major-mode highlight-error-keywords-pattern))))

(defun cam/toggle-highlight-error-keywords ()
  (interactive)
  (cl-letf* ((is-enabled highlight-error-keywords-mode-is-enabled-p)
             (font-lock-fn (if is-enabled 'font-lock-remove-keywords
                             'font-lock-add-keywords)))
    (funcall font-lock-fn major-mode '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|DEPRECATED\\|NOCOMMIT\\)"
                                   1 font-lock-warning-face t)))
    (setq highlight-error-keywords-mode-is-enabled-p (not is-enabled))))

(global-set-key (kbd "<f2>") 'cam/toggle-highlight-error-keywords)

;; (add-hook 'after-change-major-mode-hook
;;   (lambda ()
;;     (font-lock-add-keywords major-mode )))


(provide 'highlight-error-keywords)
;;; highlight-error-keywords.el ends here
