;;; cam-macros --- Utility macros -*- lexical-binding: t -*-
;;; Commentary:
;;; This gets loaded before cam-functions
;;; Code:

(require 'helm-command)

(defmacro cam/define-keys (keymap-or-mode &rest pairs)
  "Map pairs of KEY -> FN.
   If KEYMAP-OR-MODE is a key map or major mode, map keys for the mode map via 'define-key';
   otherwise map globally via 'global-set-key'.

   KEY may be a string such as 's-b' or another function, in which case the binding(s) for that function will be remapped
   with the ['remap #'other-fn] form

   ex.
   (cam/define-keys nil
      \"s-b\" #'balance-windows     ; bind s-b to balance-windows
      #'discover-my-major #'my-func ; replace keybindings to discover-my-major with ones to my-func
  "
  (let* ((resolved-mode-map (cl-gensym "mode-map-"))
         (def-key (if keymap-or-mode `(define-key ,resolved-mode-map)
                    '(global-set-key))))
    `(let ((,resolved-mode-map ,(when keymap-or-mode
                                  `(if (keymapp ,keymap-or-mode) ,keymap-or-mode
                                     (eval (helm-get-mode-map-from-mode ,keymap-or-mode))))))
       ,@(mapcar (-lambda ((key fn))
                   `(,@def-key ,(if (stringp key) (list 'kbd key)
                                  (list 'vector ''remap key))
                      ,fn))
                 (-partition 2 pairs)))))
(put 'cam/define-keys 'lisp-indent-function 1)

(defmacro cam/enable-minor-modes (&rest modes)
  "Enable specifed minor MODES with symbol or (mode . dimished-string) pair."
  `(progn
     ,@(mapcar (lambda (arg)
                 (let ((mode (if (consp arg) (car arg)
                               arg)))
                   `(condition-case err
                        ,(if (consp arg)
                             `(progn
                                (,mode 1)
                                (diminish ',mode ,(cdr arg)))
                           `(,mode 1))
                      (error (warn (error-message-string err))))))
               modes)))
(put 'cam/enable-minor-modes 'lisp-indent-function 0)

(defmacro cam/disable-minor-modes (&rest modes)
  "Disable specified minor MODES (if they are bound)."
  `(mapc (lambda (mode)
           (when (fboundp mode)
             (funcall mode nil)))
         ',modes))
(put 'cam/disable-minor-modes 'lisp-indent-function 0)

(defmacro cam/diminish-modes (&rest modes)
  "Diminish a list of MODES.  Specify individual symbols to completely diminish, or (symbol . diminshed-text) pairs."
  `(mapc (lambda (mode)
           (if (consp mode) (diminish (car mode) (cdr mode))
             (diminish mode nil)))
         ',modes))
(put 'cam/disable-minor-modes 'lisp-indent-function 0)

(defmacro cam/setup-autoloads (&rest autoloads)
  "Setup AUTOLOADS with the format (package-name-string symbol1 symbol2 ...)
   e.g. (cam/setup-autoloads (\"bytecomp\" #'byte-recompile-file))."
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
  "Declare a series of VARS so flycheck stops complaining about them being unbound."
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
                                 :append :local))))
                        fns)))))

(defmacro cam/suppress-messages (&rest body)
  "Suppress messages inside BODY."
  `(noflet ((message (&rest _) nil))
     ,@body))

(defmacro cam/some-> (form &rest sexps)
  "Similar to Clojure ->."
  (if (not sexps) form
    (let ((sexp (car sexps))
          (rest-sexps (cdr sexps))
          (form-tag (cl-gensym "form-")))
      `(-when-let ((,form-tag ,form))
         (cam/some-> ,(cond ((symbolp sexp) `(,sexp ,form-tag))
                            (:else          (cons (car sexp) (cons form-tag (cdr sexp)))))
                     ,@rest-sexps)))))

(defmacro cam/when-buffer (buffer-name-or-binding &rest body)
  "Execute BODY if a named buffer exists. BUFFER-NAME-OR-BINDING can be either a string or a list like (binding buffer-name)"
  (if (not (listp buffer-name-or-binding))
      `(cam/when-buffer (_ ,buffer-name-or-binding) ,@body)
    (cl-destructuring-bind (binding buffer-name) buffer-name-or-binding
      (let ((buffer-name-tag (cl-gensym "buffer-name-")))
        `(-when-let ((,buffer-name-tag ,buffer-name))
           (-when-let ((,binding (get-buffer ,buffer-name-tag)))
             ,@body))))))
(put 'cam/when-buffer 'lisp-indent-function 1)

(defmacro cam/unless-buffer (buffer-or-buffer-name &rest body)
  "Execute BODY if BUFFER-OR-BUFFER-NAME doesn't exist."
  `(unless (get-buffer ,buffer-or-buffer-name)
     ,@body))
(put 'cam/unless-buffer 'lisp-indent-function 1)

(defmacro cam/edebug-this (&rest body)
  "Execute BODY inside of a edebug-instrumented function."
  (let ((fn-tag (cl-gensym "fn-")))
    `(progn
       (defun ,fn-tag ()
         ,@(mapcar #'macroexpand-all body))
       (,fn-tag))))

(defmacro cam/wrap-fn (f wrapper)
  "Return a function that wraps F inside WRAPPER.
   e.g. (cam/wrap-fn #'windmove-up #'ignore-errors)"
  (-let* ((f (eval f))
          (wrapper (eval wrapper))         ; #'windmove-up comes in like (function windmove-up). Convert to windmove-up
          (argslist (cam/discover-args f))
          ((regular-args rest-arg) (cam/split-argslist argslist)))
    `(lambda ,(help-function-arglist f :preserve-names-if-possible)
       ,(interactive-form f)
       (,wrapper
        ,(if rest-arg
             `(apply #',f ,@regular-args ,rest-arg)
           `(,f ,@regular-args))))))

(defmacro cam/wrap-ignore-errors (f)
  "Call function F inside of an ignore-errors form."
  `(cam/wrap-fn ,f #'ignore-errors))

(provide 'cam-macros)
;;; cam-macros.el ends here
