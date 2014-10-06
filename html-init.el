(require 'nxml-mode)

(defun html-mode-setup ()
  (global-mode-setup)
  (nxml-mode)) ; fancy xml editing mode
(add-hook 'html-mode-hook 'html-mode-setup)

(define-keys nxml-mode-map
  '(("C-c C-f" cam-nxml-finish-element)))

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

(provide 'html-init)
