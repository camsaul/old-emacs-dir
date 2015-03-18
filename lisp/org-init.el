;; -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'org))
(require 'cam-functions)

;; set global agenda files
(setq org-support-shift-select nil)
(cam/define-keys nil
                 "C-c l" 'org-insert-link-global
                 "C-c a t" 'org-todo-list
                 "C-c a a" 'org-agenda
                 "C-c a m" 'org-tags-view)

(provide 'org-init)
;;; org-init.el ends here
