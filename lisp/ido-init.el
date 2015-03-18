;; -*- lexical-binding: t -*-
;; -*- comment-column: 50; -*-

;;; ido-init -- Setup for ido
;;; Commentary:
;;; Code:

(require 'ido)
(eval-when-compile
  (require 'flx-ido)
  (require 'ido-ubiquitous)
  (require 'ido-vertical-mode))

;; global minor modes
(cam/enable-minor-modes
  flx-ido-mode
  ido-everywhere
  ido-ubiquitous-mode
  ido-vertical-mode)

(nconc ido-ignore-directories '("node_modules"
                                "bower_components"
                                ".git"))

(provide 'ido-init)
;;; ido-init ends here
