(provide 'cam-functions)
(require 'speedbar)

(defun cygwin-shell () 
  "Run cygwin bash in shell mode."
  (interactive)
  (let ((explicit-shell-file-name "C:/cygwin/bin/bash"))
    (call-interactively 'shell)))

(defun define-keys (map-or-nil keys)
  (flet ((set-key-fn (ks a)
		     (let ((k (eval (list 'kbd ks))))		       
		       (if map-or-nil
			   (define-key map-or-nil k a)
			 (global-set-key k a)))))
    (mapc (lambda (ka)
	    (set-key-fn (car ka) (cadr ka)))
	  keys)))

(defun popup-yank-menu ()
  (interactive)
  (popup-menu 'yank-menu))

(defun join-next-line ()
  (interactive)
  (join-line -1))

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

(defun backward-kill-line ()
  "Calls (kill-line 0), which kills line from current cursor position to beginning of line."
  (interactive)
  (kill-line 0))
