(provide 'org-init)

;; set global agenda files
(setq org-agenda-files (list "C:/Users/camms_000/SkyDrive/bookmarks.org"))
(setq org-support-shift-select nil)
(global-set-key (kbd "C-c l") 'org-insert-link-global)
(global-set-key (kbd "C-c a t") 'org-todo-list)
(global-set-key (kbd "C-c a a") 'org-agenda)
(global-set-key (kbd "C-c a m") 'org-tags-view)
