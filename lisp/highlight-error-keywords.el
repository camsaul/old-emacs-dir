;; -*- lexical-binding: t -*-

;;; highlight-error-keywords -- Minor mode to add certain error keywords to major mode's font locking
;;; Commentary:
;;; Code:

(defun highlight-error-keywords-mode (enable)
  "Add the font-lock error keywords (not a real minor-mode."
  (font-lock-add-keywords major-mode '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|DEPRECATED\\|NOCOMMIT\\)"
                                        1 font-lock-warning-face t))))

(provide 'highlight-error-keywords)
;;; highlight-error-keywords.el ends here
