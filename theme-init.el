(set-frame-font (cond
		 ((string= system-type "windows-nt") "Consolas-10")
		 ((string= system-type "darwin") "Menlo-11"))) ; what about "gnu/linux" ?

(custom-set-variables
 '(custom-enabled-themes (quote (tommyh)))
 '(custom-safe-themes (quote ("353861e69d6510a824905208f7290f90248f0b9354ee034fd4562b962790bdfc" default))))
(custom-set-faces)

;; Evil Mode Configuration

(global-evil-matchit-mode 1) ; WTF does this do? https://github.com/redguardtoo/evil-matchit

(setq
 evil-emacs-state-cursor '("#dd0000" box)
 evil-normal-state-cursor '("#0000dd" box)
 evil-visual-state-cursor '("#CC6633" box)
 evil-insert-state-cursor '("#336600" box)
 evil-replace-state-cursor '("#663399" box)
 evil-operator-state-cursor '("black" box)
 evil-motion-state-cursor '("#FF0099" box))

(defun mode-line-face-background ()
  "Color we should apply to the background of the mode-line (determined by evil state)"
  (cond
   ((evil-emacs-state-p) "#dd0000")
   ((evil-normal-state-p) "#0000dd")
   ((evil-visual-state-p) "#cc6633")
   ((evil-insert-state-p) "#336600")
   ((evil-replace-state-p) "#663399")
   ((evil-operator-state-p) "black")
   ((evil-motion-state-p) "#ff0099")))

(setq powerline-evil-tag-style 'verbose)

(add-hook 'post-command-hook
          (lambda ()
            (set-face-background 'mode-line (mode-line-face-background))))

(add-hook 'evil-normal-state-entry-hook
          (lambda ()
            (relative-line-numbers-mode +1)
            (linum-mode -1)))

(add-hook 'evil-emacs-state-entry-hook
          (lambda ()
            (relative-line-numbers-mode -1)
            (linum-mode +1)))

(defface pl-active-1
  '((t :background "grey20"
       :foreground "white"
       :weight bold))
  "Active face #1 for power line"
  :group 'pl-faces)

(defface pl-active-2
  '((t :background "grey30"
       :foreground "white"
       :weight bold))
  "Active face #3 for power line"
  :group 'pl-faces)

(defface pl-active-3
  '((t :background "grey40"
       :foreground "white"
       :weight bold))
  "Active face #3 for power line"
  :group 'pl-faces)

(defface pl-inactive-color-face
  '((t :background "grey70"
       :foreground "grey10"
       :weight bold))
  "Face to show in place of the state-colored face in power line"
  :group 'pl-faces)

(defface pl-inactive-1
  '((t :background "grey80"
       :foreground "grey20"
       :weight bold))
  "Inactive face #1 for power line"
  :group 'pl-faces)

(defface pl-inactive-2
  '((t :background "grey90"
       :foreground "grey30"
       :weight bold))
  "Inactive face #3 for power line"
  :group 'pl-faces)

(defface pl-inactive-3
  '((t :background "white"
       :foreground "grey40"
       :weight bold))
  "Inactive face #3 for power line"
  :group 'pl-faces)

(defun setup-powerline ()
  (interactive)
  (setq
   mode-line-format
   '("%e"
     (:eval
      (let* ((active (powerline-selected-window-active))
             (color-face (if active nil 'pl-inactive-color-face))
             (face1 (if active 'pl-active-1 'pl-inactive-1))
             (face2 (if active 'pl-active-2 'pl-inactive-2))
             (face3 (if active 'pl-active-3 'pl-inactive-3))
             (separator-left (intern (format "powerline-%s-%s"
                                             powerline-default-separator
                                             (car powerline-default-separator-dir))))
             (separator-right (intern (format "powerline-%s-%s"
                                              powerline-default-separator
                                              (cdr powerline-default-separator-dir))))
             (lhs (list (powerline-raw (powerline-evil-tag) color-face 'l)
                        (powerline-raw " " color-face)
                        (funcall separator-left color-face face1)
                        (powerline-buffer-id face1 'l)
                        (powerline-raw " " face1)
                        (funcall separator-left face1 face2)
                        (powerline-major-mode face2 'l)
                        (powerline-process face2)
                        (powerline-raw " " face2)
                        (funcall separator-left face2 face3)
                        (powerline-minor-modes face3 'l)
                        (powerline-narrow face3 'l)
                        (powerline-raw " " face3)))
             (rhs (list (funcall separator-right face3 face1)
                        (powerline-raw " " face1)
                        (powerline-raw "%l" face1 'r)
                        (funcall separator-right face1 color-face)
                        (when global-mode-string
                          (powerline-raw global-mode-string color-face)
                          (powerline-raw " " color-face))
                        (powerline-vc color-face 'r))))
        (concat (powerline-render lhs)
                (powerline-fill face3 (powerline-width rhs))
                (powerline-render rhs)))))))
(setup-powerline)

(add-hook 'window-configuration-change-hook
          'setup-powerline)

(provide 'theme-init)
