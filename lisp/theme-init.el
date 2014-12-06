;; -*- comment-column: 60; -*-

;;; theme-init -- Setup Emacs theme
;;; Commentary:

;;; Code:


;;;; BASIC THEME CONFIG + TWEAKS

(when (string= system-type "darwin")               ; enable sRGB on OS X (why ?)
  (setq ns-use-srgb-colorspace t))

(defvar cam/frame-font
  "Source Code Pro-11"
  "Font to use for Emacs.")

(defvar cam/background-color
  "#f4f4f4"
  "Background color for Emacs.")

(require 'moe-theme)
(setq moe-light-pure-white-background-in-terminal t
      moe-theme-highlight-buffer-id nil)
(moe-light)

(set-frame-font cam/frame-font)
(set-background-color cam/background-color)

;; when opening a new FRAME set the font and bg color
(defadvice make-frame-command (after make-frame-set-font activate)
  (interactive)
  (set-background-color cam/background-color)
  (set-frame-font cam/frame-font))

;; color tweaks
(set-face-attribute 'font-lock-doc-face nil
                    :foreground "black"
                    :bold t)

(set-face-attribute 'font-lock-comment-face nil
                    :foreground "#5fafd7"
                    :bold t
                    :italic t)

(set-face-attribute 'font-lock-builtin-face nil
                    :foreground "cc6633"
                    :bold nil)

(set-face-attribute 'font-lock-function-name-face nil
                    :foreground "#008FD7"
                    :bold t)

(set-face-attribute 'font-lock-preprocessor-face nil
                    :bold nil
                    :italic t)



;;;; COMPANY TOOLTIP TWEAKS

;; The company autocomplete popup box doesn't fit in well with moe-light; fix that

(eval-after-load "company"
  '(let ((h-1 (face-background 'hl-line))                   ; #d7ff87
         (h-2 (face-background 'lazy-highlight))            ; #ff1f8b
         (c-1 (face-background 'secondary-selection))       ; #005f87
         (c-2 (face-background 'isearch))                   ; #ff5d17
         (c-3 (face-background 'region))                    ; #5fafd7
         )

     (set-face-attribute 'mode-line nil
                         :foreground "white"
                         :background h-1)
     (set-face-attribute 'minibuffer-prompt nil
                         :background nil
                         :foreground c-1
                         :bold t)
     (set-face-attribute 'company-tooltip nil
                         :background "white")
     (set-face-attribute 'company-tooltip-common nil
                         :background "white"
                         :foreground c-1
                         :bold t)
     (set-face-attribute 'company-tooltip-annotation nil
                         :background "white"
                         :foreground c-2)
     (set-face-attribute 'company-preview-common nil
                         :background "white")
     (set-face-attribute 'company-tooltip-common-selection nil
                         :background h-1
                         :foreground h-2
                         :bold t
                         :italic t)
     (set-face-attribute 'company-tooltip-selection nil
                         :background h-1
                         :foreground c-2)
     (set-face-attribute 'company-scrollbar-bg nil
                         :background h-1)
     (set-face-attribute 'company-scrollbar-fg nil
                         :background c-2)))


;;;; EVIL STATE CURSOR COLOR CONFIG

(defvar evil-state-colors
  '(("emacs" . "#dd0000")
    ("normal" . "#0000dd")
    ("visual" . "#cc6633")
    ("insert" . "#336600")
    ("replace" . "#663399")
    ("operator" . "#000000")
    ("motion" . "#ff0099"))
  "Association list of colors to use for cursor + modeline for each evil state.")

;; Now set the cursor colors for each state.
;; Rhis just loops over evil-state-colors and sets the corresponding variable for that state
;; e.g. it sets evil-emacs-state-cursor to ("#dd0000" . 'box) - a red box
(mapc (-lambda ((name . color))
        (eval `(setq ,(intern (format "evil-%s-state-cursor" name))
                 '(,color . 'box))))
      evil-state-colors)


;;;; POWERLINE CONFIG

(setq powerline-evil-tag-style 'verbose) ; display evil states with verbose names like 'EMACS' / 'NORMAL'

;; make line numbers / relative line numbers use similar padding so switching between the two doesn't resize the fringe/frame
(set-default 'linum-format "%3d")
(set-default 'relative-line-numbers-format
             (lambda (offset)
               (format "%3d" (abs offset))))

(defun mode-line-face-background ()
  "Color we should apply to the background of the mode-line (determined by evil state)"
  (cdr (assoc (symbol-name evil-state) evil-state-colors)))

;; update the mode line background color to corresponding state color whenever a command is executed
(add-hook 'post-command-hook
  (lambda ()
    (set-face-background 'mode-line (mode-line-face-background))))

;; Define the colors we'll use in our powerline
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
(set-face-foreground 'mode-line-buffer-id "white")
(set-face-background 'mode-line-buffer-id nil)

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
               (powerline-render rhs))))))

;; mode-line-format has a tendency to get set as a buffer-local variable, which overrides our default value for it defined above.
;; whenever the window config changes loop through all buffers and remove any buffer-local values of mode-line-format if found
;; so they don't mask our default val
(add-hook 'window-configuration-change-hook
  (lambda ()
    (mapc (lambda (buffer)
            (when (local-variable-p 'mode-line-format)
              (kill-local-variable 'mode-line-format)))
          (buffer-list))))

(provide 'theme-init)
;;; theme-init.el ends here
