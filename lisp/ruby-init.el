;; -*- lexical-binding: t -*-

;;; Code:

(defun cam/ruby-mode-setup ()
  (require 'rspec-mode)
  (require 'ruby-electric)
  (require 'ruby-block)
  (cam/enable-minor-modes
    (paredit-mode . " π")
    ruby-block-mode
    ruby-electric-mode
    )
  (ruby-electric-mode t)
  (ruby-block-mode t) ; highlight corresponding openings when cursor is on a closing block statement
  (paredit-mode t))
(add-hook 'ruby-mode-hook 'cam/ruby-mode-setup)

(mapc (lambda (str)
        (add-to-list 'auto-mode-alist (cons str 'ruby-mode)))
      '("\.podspec$"
        "\.Gemfile$"
        "\.Rakefile$"
        "\.Podfile$"))

(setq ruby-block-highlight-toggle 'overlay) ; highlight ruby block on screen instead of minibuffer
(setq ruby-block-delay 0.01) ; delay before showing matching block; default is 0.5

(eval-after-load "ruby-mode"
  '(cam/define-keys ruby-mode-map
     "C-c C-f" #'ruby-insert-end))

;; Apparently this function is missing from the version of ruby-electric on MELPA, although it attemps to call it;
;; work around http://stackoverflow.com/questions/10326255/emacs-ruby-electric-does-not-insert-end
(defun ruby-insert-end ()
  "Insert \"end\" at point and reindent current line."
  (interactive)
  ;; Insert a newline if current line isn't empty
  (beginning-of-line)
  (indent-for-tab-command)
  (when (not (eolp))
    (end-of-line)
    (paredit-newline))

  ;; Insert + indent 'end'
  (insert "end")
  (ruby-indent-line t)

  ;; If the block is empty then insert + goto new line inside block
  (let ((block-is-empty (save-excursion
                          (forward-line -1)
                          (end-of-line)
                          (string= (buffer-substring-no-properties (point) (- (point) 2))
                                   "do"))))
    (if block-is-empty
        (progn
          (forward-line -1)
          (end-of-line)
          (paredit-newline))

      ;; If block isn't empty move to end of line after 'end'
      (end-of-line))))

(provide 'ruby-init)
;;; ruby-init.el ends here
