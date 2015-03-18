;; -*- lexical-binding: t -*-
;; -*- comment-column: 50; -*-

;;; find-things-fast-init -- Setup for find-things-fast
;;; Commentary:
;;; Code:

(require 'find-things-fast)

(nconc ftf-filetypes '("*.clj"                    ; extra file types to search for/through when using find-things-fast
                       "*.css"
                       "*.el"
                       "*.html"
                       "*.js"
                       "*.java"
                       "*.md"
                       "*.yml"))

(provide 'find-things-fast-init)
;;; find-things-fast-init.el ends here
