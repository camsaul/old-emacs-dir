;;; elisp-init -- Setup for editing Emacs Lisp -*- lexical-binding: t; comment-column: 50; -*-
;;; Commentary:

(require 'cam-functions)
(require 'cam-macros)

(require 'lisp-mode)
(require 'ielm)

(require 'auto-complete)
(require 'cl-lib-highlight)
(require 'edebug)
(require 'highlight-cl)
(require 'lisp-init)
(require 'morlock)

;;; Code:

(cam/setup-autoloads
  ("flycheck" #'flycheck-mode)
  ("lisp-init" #'cam/define-lisp-keys #'cam/lisp-mode-setup)
  ("lisp-mode" #'emacs-lisp-mode #'lisp-mode))


;;;; GENERAL

(defvar cam/last-elisp-buffer nil)

(defun cam/byte-recompile-this-file ()
  "Recompile the current Emacs Lisp file if it is an init file."
  (interactive)
  (when (and (buffer-file-name)
             (cam/is-init-file-p (buffer-file-name)))
    (condition-case err
        (byte-recompile-file (buffer-file-name)
                             t  ; force recompile
                             0  ; 0 = compile even if .elc does not exist
                             t) ; load after compiling
      (error (error (error-message-string err))
             (edebug err)))))

(defun cam/elisp-mode-setup ()
  "Code to be ran on \"emacs-lisp-mode-hook\" and \"ielm-mode-hook\"."
  (cam/declare-vars cam/define-elisp-keys
                    flycheck-emacs-lisp-load-path)
  (cam/lisp-mode-setup)

  (cam/enable-minor-modes
    (elisp-slime-nav-mode . nil)
    (flycheck-mode . " ✔")
    (aggressive-indent-mode . nil)
    (auto-complete-mode . "AC"))
  (ac-emacs-lisp-mode-setup)
  (edebug-Trace-fast-mode)

  ;; additional font-locking
  (turn-on-morlock-mode-if-desired)
  (highlight-cl-add-font-lock-keywords) ; <- WTF do these two do the same thing ???
  (cl-lib-highlight-initialize)
  (dash-enable-font-lock)

  (setq flycheck-emacs-lisp-load-path load-path
        lexical-binding t)
  (setq-local debug-on-error t)
  (setq-local edebug-on-error t)
  (setq-local edebug-all-forms t)
  (setq-local edebug-trace t)     ; display a trace of function entry and exit (?)

  (add-hook 'before-save-hook #'cam/untabify-current-buffer nil :local)
  (add-hook 'after-save-hook  #'cam/byte-recompile-this-file :append :local)
  (add-hook 'after-save-hook
    (lambda ()
      (auto-complete-mode -1)
      (auto-complete-mode 1)
      (ac-emacs-lisp-mode-setup)                                ; run setup again or auto-complete doesn't pickup changes
      (setq ac-sources (-distinct ac-sources))) :append :local) ; clear out the the duplicate sources that have been added))

  ;; use byte-compile-dynamic when compiling files in .emacs.d
  (when (string= default-directory                ; default-directory is buffer-local dir of the current buffer
                 (expand-file-name "~/.emacs.d/"))
    (setq-local byte-compile-dynamic t))

  (cam/define-elisp-keys major-mode))

(add-hook 'emacs-lisp-mode-hook #'cam/elisp-mode-setup)


;;;; SETTINGS

;; nicer indentation
(mapc (lambda (symb)
        (put symb 'lisp-indent-function 1))
      '(-lambda
           add-hook
         cl-lambda
         setq
         setq-default
         with-current-buffer-window))

;;;; FUNCTIONS

(defun cam/wrapping-flycheck-next-error ()
  "Call flycheck-next-error, wrap around if we've reached end of buffer."
  (interactive)
  (condition-case nil
      (flycheck-next-error)
    (error (goto-char (point-min)))))

(defvar cam/last-elisp-buffer nil)

(defun cam/elisp-tab-command ()
  "Indent, autocomplete, show dox, etc when hitting tab."
  (interactive)
  (indent-for-tab-command)
  (ac-complete))

(defun cam/ielm-clear-buffer ()
  "Clear the ielm buffer."
  (interactive)
  (comint-kill-region (point-min) (point-max))
  (ielm-return-for-effect))

(defun cam/pp-macroexpand-last-sexp ()
  "Like pp-macroexpand-last-sexp, does macro expansion inline in the macroexpansion output buffer,
   and turns macrostep-mode on."
  (interactive)
  (if (string= (buffer-name) "*Pp Macroexpand Output*")
      (progn
        (pp-macroexpand-last-sexp :eval-into-current-buffer)
        (paredit-backward)
        (backward-kill-sexp))
    (progn
      (call-interactively #'pp-macroexpand-last-sexp)
      (cam/when-buffer (macroexpand-buffer "*Pp Macroexpand Output*")
        (with-current-buffer macroexpand-buffer
          (macrostep-mode 1))))))

(defun cam/ielm-switch-to-last-elisp-buffer ()
  "Switch to the most recent emacs lisp buffer."
  (interactive)
  (when cam/last-elisp-buffer
    (switch-to-buffer-other-window cam/last-elisp-buffer)))

(defun cam/elisp-save-compile-switch-to-ielm ()
  "Save and compile current buffer, then switch to *ielm* (starting it if needed)."
  (interactive)
  (setq cam/last-elisp-buffer (current-buffer))
  (cl-symbol-macrolet ((cl-buffer (get-buffer "*Compile-Log*"))
                       (cl-buffer-tick (cam/when-buffer (b "*Compile-Log*") (with-current-buffer b
                                                                              (buffer-modified-tick)))))
    (let ((cl-buffer-original-tick cl-buffer-tick))
      (save-window-excursion
        (save-buffer))
      ;; disable edebug if it got started (?)
      (when edebug-mode
        (call-interactively #'edebug-mode))
      (when (and cl-buffer-original-tick cl-buffer (<= (- cl-buffer-tick cl-buffer-original-tick) 2))
        (cam/kill-buffer-and-window cl-buffer)))
    (let ((compile-log-window (cam/some-> cl-buffer
                                          (display-buffer '(() (inhibit-same-window . t))))))
      (cam/unless-buffer "*ielm*"
        (save-window-excursion
          (ielm)))
      (if (and compile-log-window (<= (length (window-list-1)) 2))
          (progn
            (select-window (split-window-sensibly compile-log-window) :norecord)
            (switch-to-buffer "*ielm*" nil :force-same-window))
        (progn
          (when compile-log-window
            (set-window-dedicated-p compile-log-window t))
          (switch-to-buffer-other-window "*ielm*")
          (set-window-dedicated-p (selected-window) t)
          (goto-char (point-max))
          (cam/ielm-clear-buffer)
          (when compile-log-window
            (set-window-dedicated-p compile-log-window nil)))))))

;;;; KEY MAPS

(defun cam/define-elisp-keys (mode-map)
  "Add elisp-related key bindings to MODE-MAP."
  (cam/define-lisp-keys mode-map)
  (cam/define-keys mode-map
    "<f5>" #'flycheck-display-errors
    "<f6>" #'cam/wrapping-flycheck-next-error
    "<f7>" #'flycheck-mode
    "<s-mouse-1>" #'elisp-slime-nav-find-elisp-thing-at-point
    "<tab>" #'cam/elisp-tab-command
    "C-c <return>" #'cam/pp-macroexpand-last-sexp
    "C-c <C-return>" #'macrostep-mode
    "C-c RET" #'pp-macroexpand-last-sexp
    "C-x C-e" #'pp-eval-last-sexp
    "<C-M-s-return>" #'cam/elisp-save-compile-switch-to-ielm))

;;;; IELM SPECIFIC

(defun cam/ielm-mode-setup ()
  "Setup to perform when starting ielm-mode."
  (cam/elisp-mode-setup)
  (cam/define-keys inferior-emacs-lisp-mode-map
    "<C-M-s-return>" #'cam/ielm-switch-to-last-elisp-buffer
    "RET" #'ielm-return
    "C-c M-o" #'cam/ielm-clear-buffer)
  (ac-clear-variables-every-minute))

(add-hook 'ielm-mode-hook #'cam/ielm-mode-setup)

(provide 'elisp-init)
;;; elisp-init ends here
