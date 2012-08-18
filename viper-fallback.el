;;; viper --- Viper Configuration

;;; Commentary:
;; Enables Viper before Evil is loaded

;;; Code:
(setq-default viper-expert-level '5)
(setq-default viper-inhibit-startup-message 't)
(setq-default viper-mode +1)

(require 'viper)
(viper-mode)

(provide 'viper-fallback)
;;; viper-fallback.el ends here
