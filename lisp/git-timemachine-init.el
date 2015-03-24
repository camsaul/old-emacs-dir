;; -*- lexical-binding: t; comment-column: 50; -*-

;;; git-timemachine-init -- Setup for git-timemachine
;;; Commentary:
;;; Code:

(require 'git-timemachine)

(defadvice git-timemachine (around git-timemachine-split-fullscreen activate)
  "Run git-timemachine for the current buffer in a special temporary fullscreen session"
  (window-configuration-to-register :git-timemachine-fullscreen-window-config)
  (let ((parent-buffer (current-buffer)))
    (delete-other-windows)
    (select-window (split-window-right) :norecord) ; select the newly created window to the right
    ad-do-it
    (while (eq (current-buffer) parent-buffer) ; double-check we're not showing parent buffer
      (other-window 1)))
  (add-hook 'kill-buffer-hook (lambda ()
                                (jump-to-register :git-timemachine-fullscreen-window-config :delete))
            :append :local))

(defadvice git-timemachine-quit (after git-timemachine-fullscreen-quit activate)
  (jump-to-register :git-timemachine-fullscreen-window-config))

(defun calculate-sizes (scheme)
  (let* ((*roundh (-lambda (total factor)
                    (when factor
                      (round (* total factor 0.01)))))
         (*roundw (-lambda (total (factor . buffer))
                    (cons (funcall *roundh total factor) buffer)))
         (*h (apply-partially #'funcall *roundh (frame-height)))
         (*w (apply-partially #'funcall *roundw (frame-width))))
    (mapcar (-lambda ((row . cols))
              (cons (funcall *h row)
                    (mapcar *w cols)))
            scheme)))
(calculate-sizes -window-cols-rows)

(defvar -split-original-buffer nil)

(defun -pick-buffer (buffer)
  (when buffer
    (cond ((eq buffer :*) (-pick-buffer -split-original-buffer))
          ((listp buffer) (or (-pick-buffer (car buffer))
                              (-pick-buffer (cdr buffer))))
          (buffer         (cam/when-buffer (b buffer)
                            (> 0 (buffer-size (switch-to-buffer b :norecord :force-same-window))))))))

(defun split (scheme)
  (let ((-split-original-buffer (current-buffer)))
    (delete-other-windows)
    (mapc (-lambda ((height . widths))
            (let ((next-row (when height
                              (split-window-below height))))
              (mapc (-lambda ((width . buffer))
                      (-pick-buffer buffer)
                      (when width
                        (select-window (split-window-right width) :norecord)))
                    widths)
              (when next-row
                (select-window next-row :norecord))))
          (calculate-sizes scheme))))

(let ((ie "*ielm*")
      (bt '("*Backtrace*" "*Messages*"))
      (pp "*Pp Eval Output*")
      (me "*Pp Macroexpand Output*")
      (cl "*Compile-Log*"))
  (split `((20  . ((25 . ,bt) (25 . ,pp) (25 . ,me) (nil . ,cl)))
           (40  . ((50 . nil) (nil . nil)))
           (nil . ((50 . ,ie) (nil . nil))))))
(window-configuration-to-register :xyz)
(jump-to-register :xyz)

(provide 'git-timemachine-init)
;;; git-timemachine-int.eit ends here
