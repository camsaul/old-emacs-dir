;;; cam-macros --- Utility macros
;;; Commentary:
;;; This gets loaded before cam-functions
;;; Code:

(defmacro cam/define-keys (keymap &rest pairs)
  "If KEYMAP is non-nil, map pairs of KEY -> FN for mode map via 'define-key'; otherwise map via 'global-set-key'."
  (let ((def-key (if keymap `(define-key ,keymap)
                   '(global-set-key))))
    `(progn
       ,@(mapcar (-lambda ((key fn))
                   `(,@def-key ,(list 'kbd key) ,fn))
                 (-partition 2 pairs)))))
(put 'cam/define-keys 'lisp-indent-function 1)

(defmacro cam/enable-minor-modes (&rest modes)
  "Enable specifed minor modes with symbol or (mode . dimished-string) pair."
  `(mapc (lambda (arg)
	   (condition-case err
	       (let ((mode (if (consp arg) (car arg)
			     arg)))
		 (unless nil
		   (funcall mode 1)                   ; enable mode if needed
		   (when (consp arg)                  ; diminish the mode if we were passed a cons cell
		     (diminish mode (cdr arg)))))
	     (error (warn (error-message-string err)))))
         ',modes))
(put 'cam/enable-minor-modes 'lisp-indent-function 0)

(defmacro cam/disable-minor-modes (&rest modes)
  "Disable specified minor modes (if they are bound)"
  `(mapc (lambda (mode)
           (when (fboundp mode)
             (funcall mode nil)))
         ',modes))
(put 'cam/disable-minor-modes 'lisp-indent-function 0)

(defmacro cam/diminish-modes (&rest modes)
  "Diminish a list of modes. Specify individual symbols to completely diminish, or (symbol . diminshed-text) pairs"
  `(mapc (lambda (mode)
           (if (consp mode) (diminish (car mode) (cdr mode))
             (diminish mode nil)))
         ',modes))
(put 'cam/disable-minor-modes 'lisp-indent-function 0)

(defmacro cam/setup-autoloads (&rest autoloads)
  "Setup autoloads with the format (package-name-string symbol1 symbol2 ...)
   e.g. (cam/setup-autoloads (\"bytecomp\" byte-recompile-file))"
  `(progn ,@(cl-reduce 'append
                       (mapcar (lambda (autoload-group)
                                 (let ((file (car autoload-group))
                                       (symbols (cdr autoload-group)))
                                   (mapcar (lambda (symbol)
                                             `(autoload ,symbol ,file nil t)) ; create autoload for each symbol
                                           symbols)))
                               autoloads))))
(put 'cam/setup-autoloads 'lisp-indent-function 0)

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

(defmacro cam/run-fullscreen (package &rest fns)
  "Add advice to FNS to always run fullscreen and restore window configuration after buffer is killed.  Advice is added via 'eval-after-load' after PACKAGE is loaded."
  `(eval-after-load ,package
     (quote (progn
              ,@(mapcar (lambda (fn)
                          (let ((window-config (intern (concat ":" (symbol-name fn) "-fullscreen"))))
                            `(defadvice ,fn (around run-fullscreen activate)
                               (window-configuration-to-register ,window-config)
                               ad-do-it
                               (delete-other-windows)
                               (add-hook 'kill-buffer-hook
                                 (lambda ()
                                   (jump-to-register ,window-config))
                                 t t))))
                        fns)))))

(defmacro cam/suppress-messages (&rest body)
  "Suppress messages inside BODY"
  `(noflet ((message (&rest args) nil))
     ,@body))


(provide 'cam-macros)
;;; cam-macros.el ends here
