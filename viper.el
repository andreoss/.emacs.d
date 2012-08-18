;;; viper --- Viper Configuration
;;; Commentary:
;; Enables Viper

;;; Code:
(setq-default viper-expert-level '5)
(setq-default viper-inhibit-startup-message 't)
(setq-default viper-mode +1)

(require 'viper)

(viper-mode)

(define-key viper-vi-basic-map      (kbd "v") 'nil)
(define-key viper-vi-basic-map      (kbd "V") 'nil)
(define-key viper-vi-local-user-map (kbd "g f") 'find-file-at-point)
(define-key viper-vi-local-user-map (kbd "g d") 'xref-find-definitions)
(define-key viper-vi-local-user-map (kbd "Z Z") 'save-buffers-kill-emacs)
(define-key viper-vi-local-user-map (kbd ";")   'viper-ex)

(provide 'viper-fallback)
;;; viper.el ends here
