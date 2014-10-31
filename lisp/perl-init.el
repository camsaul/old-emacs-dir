;;; perl-init --- Settings for editing Perl
;;; Commentary:
;;; Code:

(defalias 'perl-mode 'cperl-mode)       ; load cperl-mode instead of perl-mode
(add-hook 'cperl-mode-hook
  (lambda ()
    (flycheck-mode)
    (company-mode)
    (setq cperl-hairy t)                ; enable electric sutff and font locking for CPerl mode
    ))

(provide 'perl-init)
;;; perl-init.el ends here
