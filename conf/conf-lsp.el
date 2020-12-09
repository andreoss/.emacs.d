;;; conf-lsp --- LSP configuration
;;; Commentary:
;;; Code:
(use-package ag)
(use-package projectile
  :config
  (global-set-key (kbd "C-S-t")
                  'projectile-toggle-between-implementation-and-test))

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  :config
  (setq lsp-java-code-generation-use-blocks t)
  (setq lsp-java-implementations-code-lens-enabled t)
  (setq lsp-server-trace "verbose")
  (setq lsp-prefer-flymake nil)
  (setq lsp-inhibit-message t
        lsp-eldoc-render-all nil
        lsp-enable-file-watchers nil
        lsp-highlight-symbol-at-point nil)
  :hook
  (java-mode . lsp-deferred)
  (xml-mode . lsp-deferred)
  (scala-mode . lsp)
  (rust-mode . lsp)
  (c++-mode . lsp)
  (c-mode . lsp)
  :commands (lsp lsp-deferred))

(use-package lsp-python-ms
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp-deferred))))
(setq lsp-python-ms-executable "pyls")

(use-package lsp-ui
  :config
  (setq lsp-prefer-flymake nil
        lsp-ui-doc-delay 5.0
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-symbol nil))

(use-package lsp-ivy)
(use-package lsp-treemacs)
(use-package treemacs
  :config
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  ;; enables mouse hover support
  (dap-tooltip-mode 1)
  ;; use tooltips for mouse hover
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  (tooltip-mode 1))
(hook! lsp-mode-hook (lsp-lens-mode +1))

(evil-leader/set-key "l R" 'lsp-workspace-restart)
(evil-leader/set-key "l f" 'lsp-format-buffer)
(evil-leader/set-key "r" 'lsp-rename)
(define-key lsp-ui-mode-map
  [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map
  [remap xref-find-references] #'lsp-ui-peek-find-references)
(provide 'conf-lsp)
;;; conf-lsp.el ends here
