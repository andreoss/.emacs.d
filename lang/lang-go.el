;;; lang-go --- Go
;;; Commentary:
;;; Code:
(use-package go-mode)
(use-package go-guru)
(use-package golint)
(use-package go-autocomplete)
(use-package go-eldoc)

(defun my/go-mode-hook ()
  "Go mode hook."
  (go-eldoc-setup)
  (setq-local gofmt-command "goimports")
  (local-set-key (kbd "M-.") 'godef-jump)
  (add-hook 'before-save-hook 'gofmt-before-save nil 'local))

(hook! go-mode-hook (go-eldoc-setup))
(hook! go-mode-hook
     (setq-local gofmt-command "goimports")
     (local-set-key (kbd "M-.") 'godef-jump)
     )

(hook! go-mode-hook
     (add-hook 'before-save-hook 'gofmt-before-save nil 'local))

(hook! go-mode-hook auto-complete-mode)

(if-bound evil-mode
  (evil-define-key 'normal go-mode-map
    (kbd "M-.") 'godef-jump
    (kbd "g d") 'godef-jump
    (kbd "g D") 'godef-jump-other-window))

(provide 'lang-go)
;;; lang-go.el ends here
