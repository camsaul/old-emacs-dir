(defun cygwin-shell ()
  "Run cygwin bash in shell mode."
  (interactive)
  (let ((explicit-shell-file-name "C:/cygwin/bin/bash"))
    (call-interactively 'shell)))

(defun define-keys (map-or-nil keys)
  (cl-flet ((set-key-fn (ks a)
                        (let ((k (eval (list 'kbd ks))))
                          (if map-or-nil
                              (define-key map-or-nil k a)
                            (global-set-key k a)))))
    (mapc (lambda (ka)
            (set-key-fn (car ka) (cadr ka)))
          keys)))

(defun popup-yank-menu ()
  "Show list of recent yanks as a popup menu."
  (interactive)
  (popup-menu 'yank-menu))

(defun popup-cam-menu ()
  "Show the 'CAM :)' menu as a popup menu."
  (interactive)
  (popup-menu cam-menu))

(defun join-next-line ()
  "Basically like C-e C-k then deleting leftover space."
  (interactive)
  (join-line -1))

(defun untabify-current-buffer ()
  "Run untabify over the entire current buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun lorem-ipsum ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

(defun mark-whole-sexp ()
  (progn
    (backward-sexp)
    (mark-sexp)))

(defun current-token ()
  "Returns the entire current sexp"
  (save-excursion
    (mark-whole-sexp)
    (buffer-substring (region-beginning) (region-end))))

(defun active-region-or-prompt (prompt)
  (url-hexify-string (if mark-active
                         (buffer-substring (region-beginning) (region-end))
                       (read-string prompt))))

(defun coffee-house ()
  "You are working in a coffee house."
  (interactive)
  (browse-url "http://www.coffitivity.com/"))

(defun bing-search ()
  "Bings a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.bing.com/search?setmkt=en-US&q="
    (active-region-or-prompt "Search Bing for: "))))

(defun stackoverflow-search ()
  "Searches Stack Overflow for current region, or prompts for query if none exists."
  (interactive)
  (browse-url
   (concat
    "http://stackoverflow.com/search?q="
    (active-region-or-prompt "Search Stack Overflow for: "))))

(defun backward-kill-line ()
  "Calls (kill-line 0), which kills line from current cursor position to beginning of line."
  (interactive)
  (kill-line 0))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value"
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun force-indent-region ()
  "Indent a region, overriding normal indentation behavior."
  (interactive)
  (indent-rigidly (region-beginning) (region-end) 4))

(defun force-unindent-region ()
  "Unindent a region, overriding normal indentation behavior."
  (interactive)
  (indent-rigidly (region-beginning) (region-end) -4))

(defun windmove-left-or-other-frame ()
  "Move one window to the left, or to the frame one position before the current frame."
  (interactive)
  (condition-case nil
      (windmove-left)
    (error (other-frame -1))))

(defun windmove-right-or-other-frame ()
  "Move one window to the right, or to the frame one position before the current frame."
  (interactive)
  (condition-case nil
      (windmove-right)
    (error (other-frame 1))))

;; TODO - put this recentf mode stuff in its own file

(defface cam-recentf-dir4
  '((t :background "white"
       :foreground "grey75"))
  "Face to show for the directory portion of the path in the list."
  :group 'cam-recentf-faces)

;;;###autoload
(define-generic-mode cam-recentf-mode
  nil                                                               ; comment-list
  nil                                                               ; keyword-list
  ;; font-lock-list
  '(("/\\([^/]+\\)[[:space:]]\\[.*$" 1 font-lock-string-face)       ; filename
    ("\\[\\([[:digit:]]+\\)\\]$" 1 font-lock-comment-face)          ; index
    ("\\([^/]+\\)/[^/]+[[:space:]]\\[.*$" 1 font-lock-comment-face) ; parent dir [.]
    ("^\\(.*\\)/[^/]+/[^/]+[[:space:]]\\[.*$" 1 'cam-recentf-dir4)  ; other dirs [..]+
    ("/[^/]+?\\.?\\([^.]*\\)[[:space:]]\\[.*$" 1 font-lock-keyword-face)) ; extension
  '("\\*cam-recentf\\*")                                            ; auto-mode-list
  '((lambda ()                                                           ; [init] function list
      (make-local-variable 'files)
      (setq files (cam-recentf-list))
      (save-excursion
        (insert (cam-recentf-format-list files)))
      (setq fill-column (- (window-width) 3))
      (setq fill-nobreak-predicate '((lambda ()
                                       (string= (char-to-string (char-before (point)))
                                                "\n"))))
                                       ;; (let ((char-is-p (lambda (str)
                                       ;;                      )))
                                       ;;   (or (funcall char-is-p "\n")
                                       ;;       (funcall char-is-p "/"))))))
      (fill-region (buffer-end 1) (buffer-end -1) 'right)
      (setq buffer-read-only t)
      (use-local-map cam-recentf-mode-map)))
  "TODO Docstring"
  )

;;;; Constants

(defvar cam-recentf-buffer-name "*cam-recentf*"
  "Default name to give a new cam-recentf-mode buffer")

;;;; Keymap

(defvar cam-recentf-mode-map (make-keymap)
  "Keymap for cam-recentf-mode")

(define-keys cam-recentf-mode-map
  '(("<return>" cam-recentf-return)
    ("<mouse-1>" cam-recentf-return)
    ("C-g" cam-recentf-kill-buffer)))

;;;; Helper functions

(defvar cam-recentf-buffer nil
  "The current cam-recentf-mode buffer, if any.")

(defun cam-recentf-kill-buffer ()
  "Kill cam-recentf buffer, if any."
  (interactive)
  (when cam-recentf-buffer
    (kill-buffer cam-recentf-buffer)))

(defun cam-recentf-return ()
  "Go to file for the current-line"
  (interactive)
  (when (string= major-mode (symbol-name 'cam-recentf-mode))
    (cam-recentf-finish (1- (line-number-at-pos)))))      ; RET increments line num

(defun cam-recentf-list ()
  "Return recentf-list as list of (index . filename)"
  (let ((i 0))
    (mapcar (lambda (fname)
              (let ((result (cons i fname)))
                (setq i (1+ i))
                result))
            recentf-list)))

(defun cam-recentf-format-file (f)
  "Take (i . fname) pair and format as string (including truncating if needed) for display."
  (let* ((str (format "%s [%d]\n" (cdr f) (car f)))
            (max-len (- (window-width) 10))
            (str-len (length str)))
    (if (> str-len max-len)
      (substring str (- max-len))
      str)))

(defun cam-recentf-format-list (recent-files)
  "TODO"
  (setq num 0)
  (apply 'concat
         (mapcar 'cam-recentf-format-file recent-files)))

(defun cam-recentf-finish (i)
  "Finish and go to file on the current line."
  (let ((selected-filename (cdr (nth i files))))
    (find-file selected-filename)
    (cam-recentf-kill-buffer)))

;;;; Public functions

(defun cam-recentf-mode-show ()
  "Open a new cam-recentf-mode buffer in the current window."
  (interactive)
  ;; kill old buffer (if needed) + create new one
  (cam-recentf-kill-buffer)
  (setq cam-recentf-buffer (get-buffer-create cam-recentf-buffer-name))
  (switch-to-buffer cam-recentf-buffer t t)
  (cam-recentf-mode)
  (let* ((buffer-num (read-from-minibuffer "Choose Buffer: " nil))
         (buffer-num (string-to-number buffer-num)))
    (cam-recentf-finish buffer-num)))

(global-set-key (kbd "C-x C-r") 'cam-recentf-mode-show)

(defun switch-to-nav-buffer-other-window ()
  "Switch to the *nav* buffer"
  (interactive)
  (switch-to-buffer-other-window "*nav*"))

(defun menu-edit-file (str f)
  (vector str (list 'lambda '() '(interactive) (list 'find-file f))))

(defun menu-edit-init-file (f)
  (menu-edit-file (file-name-base f) f))

(defmacro cam-enable-minor-modes (&rest modes)
  "Enable specifed minor modes with symbol or (mode . dimished-string) pair."
  `(mapc (lambda (arg)
           (let ((mode (if (consp arg) (car arg)
                         arg)))
             (unless nil
               (funcall mode 1)                   ; enable mode if needed
               (when (consp arg)                  ; diminish the mode if we were passed a cons cell
                 (diminish mode (cdr arg))))))
         ',modes))
(put 'cam-enable-minor-modes 'lisp-indent-function 0)

(defmacro cam-disable-minor-modes (&rest modes)
  "Disable specified minor modes (if they are bound)"
  `(mapc (lambda (mode)
           (when (fboundp mode)
             (funcall mode nil)))
         ',modes))
(put 'cam-disable-minor-modes 'lisp-indent-function 0)

(defmacro cam-diminish-modes (&rest modes)
  "Diminish a list of modes. Specify individual symbols to completely diminish, or (symbol . diminshed-text) pairs"
  `(mapc (lambda (mode)
           (if (consp mode) (diminish (car mode) (cdr mode))
             (diminish mode nil)))
         ',modes))
(put 'cam-disable-minor-modes 'lisp-indent-function 0)

(defmacro cam-setup-autoloads (&rest autoloads)
  "Setup autoloads with the format (package-name-string symbol1 symbol2 ...)
   e.g. (cam-setup-autoloads (\"bytecomp\" byte-recompile-file))"
  `(progn ,@(cl-reduce 'append
                       (mapcar (lambda (autoload-group)
                                 (let ((file (car autoload-group))
                                       (symbols (cdr autoload-group)))
                                   (mapcar (lambda (symbol)
                                             `(autoload ',symbol ,file nil t)) ; create autoload for each symbol
                                           symbols)))
                               autoloads))))
(put 'cam-setup-autoloads 'lisp-indent-function 0)

(defmacro cam/declare-vars (&rest vars)
  "Declare a series of vairables so flycheck stops complaining about them being unbound."
  `(progn
     ,@(mapcar (lambda (var)
                 `(defvar ,var))
               vars)))

(defmacro cam/eval-after-load (file &rest forms)
  "Execute multiple FORMS after FILE is loaded."
  `(eval-after-load ,file
     (quote (progn ,@forms))))
(put 'cam/eval-after-load 'lisp-indent-function 1)

(provide 'cam-functions)
;;; cam-functions.el ends here