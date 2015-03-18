;; -*- lexical-binding: t -*-
;; -*- comment-column: 50; -*-

;;; company-init -- Setup for company-mode
;;; Commentary:
;;; Code:

(require 'company)

(setq company-idle-delay 0.01                     ; default is 0.5
      company-minimum-prefix-length 1)            ; default is 3


(provide 'company-init)
;;; company-init.el ends here
