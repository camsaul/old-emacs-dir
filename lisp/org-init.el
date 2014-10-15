;; set global agenda files
(setq org-support-shift-select nil)
(global-set-key (kbd "C-c l") 'org-insert-link-global)
(global-set-key (kbd "C-c a t") 'org-todo-list)
(global-set-key (kbd "C-c a a") 'org-agenda)
(global-set-key (kbd "C-c a m") 'org-tags-view)

(provide 'org-init)
