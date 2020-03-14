;;; lang-rust --- Rust
;;; Commentary:
;;; Code:
;; company
(use-package company)
(use-package rust-mode
  :init
  (setq rust-format-on-save nil)
  )
(use-package cargo)
(use-package flycheck)
(use-package flycheck-rust)

(push 'company-lsp company-backends)

(setq lsp-rust-server 'rust-analyzer)

(add-hook 'rust-mode-hook '(lambda () (setq tab-width 4)))
(add-hook 'rust-mode-hook 'lsp)
(add-hook 'rust-mode-hook 'company-mode)
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
(evil-define-key     'normal rust-mode-map
  (kbd "C-c C-c")    'cargo-process-run
  (kbd "C-c C-t")    'cargo-process-test
  (kbd "C-c C-f")    'lsp-format-buffer
  )

(provide 'lang-rust)
;;; lang-rust.el ends here
