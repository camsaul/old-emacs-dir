;; -*- comment-column: 50; -*-

;;; theme-init -- Setup Emacs theme
;;; Commentary:

;;; Code:

(when (string= system-type "darwin")               ; enable sRGB on OS X (why ?)
  (setq ns-use-srgb-colorspace t))

(set-frame-font
 (cdr (assoc system-type
             '((windows-nt . "Consolas-10") ; what about "gnu/linux" ?
               (darwin . "Menlo-11")))))

(require 'moe-theme)
(moe-light)


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
                     (eval 'evil-state-colors))))
(evil-set-cursor-colors)

(defun mode-line-face-background ()
  "Color we should apply to the background of the mode-line (determined by evil state)"
  (cdr (assoc (symbol-name evil-state) evil-state-colors)))

(setq powerline-evil-tag-style 'verbose)

(add-hook 'post-command-hook
  (lambda ()
    (set-face-background 'mode-line (mode-line-face-background))))

(defmacro cam-toggle-minor-modes (on-mode off-mode &rest body)
  "Enable on-mode and disable off-mode."
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

(defmacro def-pl-faces (name bg-color fg-color &rest rest)
  "Helper to create new face(s) via defface for powerline. Calls set-face-background/foreground as well."
  (let* ((bg-color (eval bg-color))
         (fg-color (eval fg-color))
         (bg (if (symbolp bg-color) (face-background bg-color)
               bg-color))
         (fg (if (symbolp fg-color) (face-foreground fg-color)
               fg-color)))
    `(progn
       (defface ,name
         (quote ((t :background ,bg
                    :foreground ,fg
                    :weight bold)))
         ,(concat (symbol-name name) " face for power line.")
         :group 'pl-faces)
       (set-face-background (quote ,name) ,bg)
       (set-face-foreground (quote ,name) ,fg)
       ,@(when rest
           (cdr (macroexpand `(def-pl-faces ,@rest))))))) ; cdr to skip the initial progn

(def-pl-faces
  pl-active-1 'region 'region
  pl-active-2 'secondary-selection 'secondary-selection
  pl-active-3 'hl-line "black"
  pl-inactive-color-face "gray30" "grey90"
  pl-inactive-1 "gray45" "gray80"
  pl-inactive-2 "gray60" "gray20"
  pl-inactive-3 "gray75" "gray35"
  mode-line 'hl-line "white"
  mode-line-inactive "gray90" "gray50")

(set-face-bold 'mode-line t)
(set-face-bold 'mode-line-inactive nil)

;; #5fafd7
(pl/hex-color
 (face-background 'region))

(setq-default mode-line-format
  '("%e"
    (:eval
     (let* ((active (powerline-selected-window-active))
            (color-face (if active nil 'pl-inactive-color-face))
            (face1 (if active 'pl-active-1 'pl-inactive-1))
            (face2 (if active 'pl-active-2 'pl-inactive-2))
            (face3 (if active 'pl-active-3 'pl-inactive-3))
            (lhs (list
                  (powerline-raw (concat (powerline-evil-tag) " ") color-face 'l)
                  (powerline-slant-left color-face face1)

                  (powerline-raw (concat " " (buffer-name) " ") face1 '1)
                  (powerline-slant-left face1 face2)

                  (powerline-major-mode face2 'l)
                  (powerline-process face2)
                  (powerline-raw " " face2)
                  (powerline-slant-left face2 face3)

                  (powerline-minor-modes face3 'l)
                  (powerline-narrow face3 'l)
                  (powerline-raw " " face3)))

            (rhs (list (powerline-slant-left face3 face1)
                       (powerline-raw " %l" face1 'r)
                       (powerline-slant-left face1 color-face)

                       (when global-mode-string
                         (powerline-raw (concat global-mode-string " ") color-face))
                       (powerline-vc color-face 'r))))
       (concat (powerline-render lhs)
               (powerline-fill face3 (powerline-width rhs))
               (powerline-render rhs))))))

;; remove buffer-local mode-line-formats if they pop up
(add-hook 'window-configuration-change-hook
  (lambda ()
    (mapc (lambda (buffer)
            (when (local-variable-p 'mode-line-format)
              (kill-local-variable 'mode-line-format)))
          (buffer-list))))

(provide 'theme-init)
;;; theme-init.el ends here
