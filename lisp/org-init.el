;; -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'org))
(require 'cam-functions)

;; set global agenda files
(setq org-support-shift-select nil)
(cam/define-keys nil
  "C-c a t" #'org-todo-list
  "C-c a a" #'org-agenda
  "C-c a m" #'org-tags-view
  "C-c l" #'org-insert-link-global)

(cam/define-keys org-mode-map
  "C-c c" #'cam/org-insert-code-block)

(defun cam/org-insert-code-block (language)
  "Insert a new Org code block and start editing it."
  (interactive "sLanguage: ")
  (org-return-indent)
  (insert "#+BEGIN_SRC " language)
  (org-return-indent)
  (insert "#+END_SRC")
  (forward-line -1)
  (org-end-of-line)
  (org-return-indent)
  (org-edit-src-code))

(provide 'org-init)
;;; org-init.el ends here
