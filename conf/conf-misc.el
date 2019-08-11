;;; conf-misc -- Miscellaneous
;;; Commentary:
;;; Code:
(require 'saveplace)
(require 's)
(setq-default
 save-place-file "~/.emacs.d/saveplace"
 save-place t)

(setq-default initial-buffer-choice      t
              visible-bell               t
              echo-keystrokes        0.001)

;; y/n
(defalias 'yes-or-no-p 'y-or-n-p)
(setq kill-buffer-query-functions nil)
(setq kill-emacs-query-functions nil)

(setq-default inhibit-startup-screen  t)
(setq-default initial-major-mode 'lisp-mode)
(defvar emacs-init-time-in-seconds
    (float-time
     (time-subtract after-init-time before-init-time)))
;; Scratch buffer
(setq-default initial-major-mode 'org-mode)
(setq-default initial-scratch-message nil)
(use-package unkillable-scratch
  :init
  (unkillable-scratch +1))
(setq-default unkillable-scratch-behavior 'bury)
(setq-default unkillable-scratch-do-not-reset-scratch-buffer t)
(use-package persistent-scratch
  :init
  (persistent-scratch-setup-default)
  (persistent-scratch-autosave-mode +1))

(add-hook 'after-init-hook
          (lambda ()
            (message
             (format "* %.1f s\n"
                     emacs-init-time-in-seconds))))


;; Docker
(use-package dockerfile-mode
  :mode "Dockerfile")

;; Other text/config file modes
;; Should these be here? Maybe move to separate init file?
(use-package markdown-mode)

(use-package yaml-mode
  :mode "\\.yaml$")

(add-to-list
 'auto-mode-alist '("cron\\(tab\\)?\\." . crontab-mode))
(provide 'conf-misc)
