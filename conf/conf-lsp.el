(use-package projectile
  :config
  (global-set-key (kbd "C-S-t")
                  'projectile-toggle-between-implementation-and-test))
;; Helm:
(use-package helm
  :init
  ;; Fuzzy matches everywhere!
  (setq helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t
        helm-M-x-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-buffers-fuzzy-matching t)

  ;; Adopted from sachac's config
  ;; http://pages.sachachua.com/.emacs.d/Sacha.html
  (setq helm-ff-skip-boring-files t)

  ;; Use the same buffer as where invoked
  (setq helm-split-window-default-side 'same
        helm-split-window-inside-p nil
        helm-reuse-last-window-split-state nil)

  )
(use-package lsp-mode
  :config
  (setq lsp-java-code-generation-use-blocks t)
  (setq lsp-java-implementations-code-lens-enabled t)
  (setq lsp-server-trace "verbose")
  (setq lsp-inhibit-message t
        lsp-eldoc-render-all nil
        lsp-enable-file-watchers nil
        lsp-highlight-symbol-at-point nil)
  )

(use-package lsp-ui
  :config
  (setq lsp-prefer-flymake nil
        lsp-ui-doc-delay 5.0
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-symbol nil))

(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs)
(use-package treemacs
  :config
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))
(hook! java-mode-hook lsp)
(hook! xml-mode-hook lsp)
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
(require 'evil)

(global-set-key (kbd "M-2") 'lsp-treemacs-symbols)
(provide 'conf-lsp)
