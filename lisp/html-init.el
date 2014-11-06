(cam-setup-autoloads
  ("nxml-mode" nxml-backward-element nxml-backward-up-element nxml-finish-element nxml-forward-element))

(defun html-mode-setup ()
  (require 'nxml-mode)
  (nxml-mode)) ; fancy xml editing mode
(add-hook 'html-mode-hook 'html-mode-setup)

(eval-after-load "nxml"
  '(::define-keys nxml-mode-map
     "C-M-b" #'cam-nxml-backward-element-or-sexp
     "C-M-f" #'cam-nxml-forward-element-or-sexp
     "C-M-k" #'cam-nxml-kill-sexp
     "C-c C-f" #'cam-nxml-finish-element
     "C-j" #'cam-nxml-newline-and-indent
     "RET" #'cam-nxml-newline-and-indent
     "M-b" #'backward-sexp
     "M-f" #'forward-sexp))

(defun cam-nxml-newline-and-indent ()
  "Actaully indent when I type RET / C-j"
  (interactive)
  (call-interactively #'electric-newline-and-maybe-indent)
  (call-interactively #'indent-for-tab-command))


(defun cam-nxml-finish-element ()
  "Call nxml-finish-element, but move point to inside the newly closed element if it is empty"
  (interactive)
  ;; Get the point where the opening tag starts
  (let ((opening-tag-start (save-excursion
                             (nxml-backward-up-element)
                             (point))))

    ;; close the tag; point will now be after end tag
    (nxml-finish-element)

    ;; if opening-tag-start is two-sexps (start tag, closing tag) from the current point then the newly closed element is empty
    (let ((element-inside-is-empty (= (save-excursion
                                        (backward-sexp 2)
                                        (point))
                                      opening-tag-start)))
      ;; if element is empty move point to new line in between opening and closing tags
      (when element-inside-is-empty
        (backward-sexp)
        (newline-and-indent)
        (forward-line -1)
        (indent-for-tab-command)))))

(defun cam-nxml-kill-sexp ()
  "Provide more paredit-like behavior for killing XML sexps"
  (interactive)
  (let ((end-point (save-excursion
                           (nxml-forward-element)
                           (point))))
          (kill-region (point) end-point)))

(defun cam-nxml-backward-element-or-sexp ()
  "Move backward one XML element, or a sexp if not possible"
  (interactive)
  (condition-case nil
      (nxml-backward-element)
    (error (backward-sexp))))

(defun cam-nxml-forward-element-or-sexp ()
  "Move forward one XML element, or a sexp if not possible"
  (interactive)
  (condition-case nil
      (nxml-forward-element)
    (error (forward-sexp))))

(provide 'html-init)
