;; -*- lexical-binding: t -*-
;; -*- comment-column: 60 -*-

;;; theme-init -- Setup Emacs theme
;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'moe-theme)
(require 'powerline)

;;;; BASIC THEME CONFIG + TWEAKS

(when (string= system-type "darwin")               ; enable sRGB on OS X (why ?)
  (setq ns-use-srgb-colorspace t))

(defvar cam/frame-font
  "Source Code Pro-12"
  "Font to use for Emacs.")

(setq moe-theme-highlight-buffer-id nil)
(moe-dark)

(set-frame-font cam/frame-font)

;; when opening a new FRAME set the font and bg color
(defadvice make-frame-command (after make-frame-set-font activate)
  (interactive)
  (set-frame-font cam/frame-font))

;; color tweaks
(set-face-attribute 'font-lock-doc-face nil
                    :bold t)

(set-face-attribute 'font-lock-comment-face nil
                    :foreground "#FFFFFF"
                    :bold nil
                    :italic t)


;; make line numbers / relative line numbers use similar padding so switching between the two doesn't resize the fringe/frame
(set-default 'linum-format "%3d")
(set-default 'relative-line-numbers-format
             (lambda (offset)
               (format "%3d" (abs offset))))

(defun cam/-load-faces-if-needed ()
  "Define the faces we'll use in our powerline if they're not already defined."
  (condition-case _
      (face-id 'pl-active-2)
    (error
     (cl-macrolet ((def-pl-face (name bg-color fg-color) (let ((bg (if (symbolp bg-color) (face-background bg-color)
                                                                     bg-color))
                                                               (fg (if (symbolp fg-color) (face-foreground fg-color)
                                                                     fg-color)))
                                                           `(progn (defface ,name
                                                                     (quote ((t :background ,bg
                                                                                :foreground ,fg
                                                                                :weight bold)))
                                                                     ,(concat (symbol-name name) " face for power line.")
                                                                     :group 'pl-faces)
                                                                   (set-face-foreground ',name ,fg)    ; why do I have to do this *and* :foreground above?
                                                                   (set-face-background ',name ,bg))))
                   (def-pl-faces (&rest faces) `(progn
                                                  ,@(mapcar (lambda (face)
                                                              `(def-pl-face ,@face))
                                                            faces))))
       (def-pl-faces
         (pl-active-1 region region)
         (pl-active-2 secondary-selection secondary-selection)
         (pl-active-3 hl-line "#DDDDDD")
         (pl-inactive-color-face "gray30" "grey90")
         (pl-inactive-1 "gray45" "gray80")
         (pl-inactive-2 "gray60" "gray20")
         (pl-inactive-3 "gray75" "gray35")
         (mode-line hl-line "white")
         (mode-line-inactive "gray90" "gray50"))))))

(set-face-bold 'mode-line t)
(set-face-background 'mode-line "#e52d2d")
(set-face-bold 'mode-line-inactive nil)
(set-face-foreground 'mode-line-buffer-id "#000000")
(set-face-background 'mode-line-buffer-id nil)

(setq-default mode-line-format
  '("%e"
    (:eval
     (progn
       (cam/-load-faces-if-needed)
       (let* ((active (powerline-selected-window-active))
              (color-face (if active nil 'pl-inactive-color-face))
              (face1 (if active 'pl-active-1 'pl-inactive-1))
              (face2 (if active 'pl-active-2 'pl-inactive-2))
              (face3 (if active 'pl-active-3 'pl-inactive-3))
              (lhs (list
                    (powerline-raw "EMACS " color-face 'l)
                    (powerline-slant-left color-face face1)
                    (powerline-buffer-id face1 'l)
                    (powerline-raw (concat (if buffer-read-only " (readonly)"
                                             (when (buffer-modified-p) "*"))
                                           " ") face1 '1)
                    (powerline-slant-left face1 face2)

                    (powerline-major-mode face2 'l)
                    (powerline-process face2)
                    (powerline-raw " " face2)
                    (powerline-slant-left face2 face3)

                    (powerline-minor-modes face3 'l)
                    (powerline-narrow face3 'l)
                    (powerline-raw " " face3)))
              (rhs (list
                    (powerline-raw "%n " face3)               ; 'Narrow' when narrowing is in effect
                    (powerline-slant-left face3 face1)

                    (powerline-raw (concat " ♠ %3l ♣  ♥ %3c ♦") face1 'r)
                    (powerline-slant-left face1 color-face)

                    (powerline-raw " %M" color-face)          ; %M -> global-mode-string
                    (powerline-vc color-face 'r))))
         (concat (powerline-render lhs)
                 (powerline-fill face3 (powerline-width rhs))
                 (powerline-render rhs)))))))

;; mode-line-format has a tendency to get set as a buffer-local variable, which overrides our default value for it defined above.
;; whenever the window config changes loop through all buffers and remove any buffer-local values of mode-line-format if found
;; so they don't mask our default val
(add-hook 'window-configuration-change-hook
  (lambda ()
    (mapc (lambda (_)
            (kill-local-variable 'mode-line-format))
          (buffer-list))
    ;; For some reason mode line bg gets stomped on too so fix that while we're at it
    (set-face-background 'mode-line "#dd0000")))

(provide 'theme-init)
;;; theme-init.el ends here
