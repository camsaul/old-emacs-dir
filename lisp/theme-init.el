(set-frame-font (cond
                 ((string= system-type "windows-nt") "Consolas-10")
                 ((string= system-type "darwin") "Menlo-11"))) ; what about "gnu/linux" ?

(custom-set-variables
 '(custom-enabled-themes (quote (tommyh)))
 '(custom-safe-themes (quote ("353861e69d6510a824905208f7290f90248f0b9354ee034fd4562b962790bdfc" default))))
(custom-set-faces)

;; Evil Mode Configuration

(global-evil-matchit-mode 1) ; WTF does this do? https://github.com/redguardtoo/evil-matchit

(defvar evil-state-colors
  '(("emacs" . "#dd0000")
    ("normal" . "#0000dd")
    ("visual" . "#cc6633")
    ("insert" . "#336600")
    ("replace" . "#663399")
    ("operator" . "black")
    ("motion" . "#ff0099"))
  "Association list of colors to use for cursor + modeline for each evil state.")


;; set variables like evil-emacs-state-cursor to ("#dd0000" box)
(defmacro evil-set-cursor-colors ()
  `(progn  ,@(mapcar (lambda (pair)
                       (let* ((name (car pair))
                              (color (cdr pair))
                              (cursor-var (intern (format "evil-%s-state-cursor" name)))
                              (cursor-val (list color 'box)))
                         `(setq ,cursor-var ',cursor-val)))
                     (eval 'evil-state-colors)))) ; defer lookup of evil-state-colors, otherwise compiler messes up sometimes
(evil-set-cursor-colors)

(defun mode-line-face-background ()
  "Color we should apply to the background of the mode-line (determined by evil state)"
  (cdr (assoc (symbol-name evil-state) evil-state-colors)))

(setq powerline-evil-tag-style 'verbose)

(add-hook 'post-command-hook
          (lambda ()
            (set-face-background 'mode-line (mode-line-face-background))))

(defmacro cam-toggle-minor-modes (on-mode off-mode &rest body)
  "Enable on-mode and disable off-mode"
  `(lambda ()
    (,on-mode 1)
    (,off-mode -1)
    ,@body))

(set-default 'linum-format "%3d")
(set-default 'relative-line-numbers-format
             (lambda (offset)
               (format "%3d" (abs offset))))

(add-hook 'evil-normal-state-entry-hook
          (cam-toggle-minor-modes relative-line-numbers-mode linum-mode))

(add-hook 'evil-emacs-state-entry-hook
          (cam-toggle-minor-modes linum-mode relative-line-numbers-mode))


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

;;;###autoload
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
