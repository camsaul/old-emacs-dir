;;; org-init -- org-mode config -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

(require 'cam-functions)
(require 'cam-macros)
(require 'org)

(setq org-support-shift-select nil)

(cam/define-keys nil
  "C-c a t" #'org-todo-list
  "C-c a a" #'org-agenda
  "C-c a m" #'org-tags-view
  "C-c l" #'org-insert-link-global)

(cam/define-keys org-mode-map
  "C-c c" #'cam/org-insert-code-block)

(defun cam/org-mode-setup ()
  "Setup for org-mode."
  (setq-local truncate-lines nil))
(add-hook 'org-mode-hook #'cam/org-mode-setup)

(defun cam/org-insert-code-block ()
  "Insert a new Org code block and start editing it."
  (interactive)
  (org-return-indent)
  (let ((language (case (read-char-choice "[c]lojure, [e]macs lisp, [o]ther? " '(?c ?e ?o))
                    (?c "clojure")
                    (?e "emacs-lisp")
                    (?o (read-string "Language: ")))))
    (insert "#+BEGIN_SRC " language))
  (org-return-indent)
  (insert "#+END_SRC")
  (forward-line -1)
  (org-end-of-line)
  (org-return-indent)
  (org-edit-src-code))

(provide 'org-init)
;;; org-init.el ends here
