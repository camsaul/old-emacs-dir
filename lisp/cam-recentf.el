

(defface cam/recentf-dir4
  '((t :background "white"
       :foreground "grey75"))
  "Face to show for the directory portion of the path in the list."
  :group 'cam/recentf-faces)

(define-generic-mode cam/recentf-mode
  nil                                                               ; comment-list
  nil                                                               ; keyword-list
  ;; font-lock-list
  '(("/\\([^/]+\\)[[:space:]]\\[.*$" 1 font-lock-string-face)       ; filename
    ("\\[\\([[:digit:]]+\\)\\]$" 1 font-lock-comment-face)          ; index
    ("\\([^/]+\\)/[^/]+[[:space:]]\\[.*$" 1 font-lock-comment-face) ; parent dir [.]
    ("^\\(.*\\)/[^/]+/[^/]+[[:space:]]\\[.*$" 1 'cam/recentf-dir4)  ; other dirs [..]+
    ("/[^/]+?\\.?\\([^.]*\\)[[:space:]]\\[.*$" 1 font-lock-keyword-face)) ; extension
  '("\\*cam/recentf\\*")                                            ; auto-mode-list
  '((lambda ()                                                           ; [init] function list
      (make-local-variable 'files)
      (setq files (cam/recentf-list))
      (save-excursion
        (mapc (lambda (file)
                (->> file
                  cam/recentf-format-file
                  insert)
                (newline))
              files))
      (setq fill-column (- (window-width) 3))
      (setq fill-nobreak-predicate '((lambda ()
                                       (string= (char-to-string (char-before (point)))
                                                "\n"))))
      (fill-region (buffer-end 1) (buffer-end -1) 'right)
      (setq buffer-read-only t)
      (use-local-map cam/recentf-mode-map)))
  "TODO Docstring"
  )

;;;; Constants

(defvar cam/recentf-buffer-name "*cam-recentf*"
  "Default name to give a new cam/recentf-mode buffer")

;;;; Keymap

(defvar cam/recentf-mode-map (make-keymap)
  "Keymap for cam/recentf-mode")

(cam/define-keys cam/recentf-mode-map
  "<return>" #'cam/recentf-return
  "<mouse-1>" #'cam/recentf-return
  "C-g" #'cam/recentf-kill-buffer)

;;;; Helper functions

(defvar cam/recentf-buffer nil
  "The current cam/recentf-mode buffer, if any.")

(defun cam/recentf-kill-buffer ()
  "Kill *cam-recentf* buffer, if any."
  (interactive)
  (when cam/recentf-buffer
    (kill-buffer cam/recentf-buffer)))

(defun cam/recentf-return ()
  "Go to file for the current-line"
  (interactive)
  (when (string= major-mode (symbol-name 'cam/recentf-mode))
    (cam/recentf-finish (1- (line-number-at-pos)))))      ; RET increments line num

(defun cam/recentf-list ()
  "Return recentf-list as list of (index . filename)"
  (let ((i 0))
    (mapcar (lambda (fname)
              (let ((result (cons i fname)))
                (setq i (1+ i))
                result))
            recentf-list)))

(defun cam/recentf-format-file (f)
  "Take (i . fname) pair and format as string (including truncating if needed) for display."
  (let* ((str (format "%s [%d]\n" (cdr f) (car f)))
            (max-len (- (window-width) 10))
            (str-len (length str)))
    (if (> str-len max-len)
      (substring str (- max-len))
      str)))

;; (defun cam/recentf-format-list (recent-files)
;;   "TODO"
;;   (setq num 0)
;;   (apply 'concat
;;          (mapcar 'cam/recentf-format-file recent-files)))

(defun cam/recentf-finish (i)
  "Finish and go to file on the current line."
  (let ((selected-filename (cdr (nth i files))))
    (find-file selected-filename)
    (cam/recentf-kill-buffer)))

;;;; Public functions

(defun cam/recentf-mode-show ()
  "Open a new cam/recentf-mode buffer in the current window."
  (interactive)
  ;; kill old buffer (if needed) + create new one
  (cam/recentf-kill-buffer)
  (setq cam/recentf-buffer (get-buffer-create cam/recentf-buffer-name))
  (switch-to-buffer cam/recentf-buffer t t)
  (cam/recentf-mode)
  (let* ((buffer-num (read-from-minibuffer "Choose Buffer: " nil))
         (buffer-num (string-to-number buffer-num)))
    (cam/recentf-finish buffer-num)))

(cam/define-keys nil
  "C-x C-M-r" #'cam/recentf-mode-show)

(provide 'cam-recentf)
;;; cam-recentf.el ends here
