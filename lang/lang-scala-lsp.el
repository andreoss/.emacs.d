;;; lang-scala-lsp --- Metals
;;; Commentary:
;;; Code:

;; Enable scala-mode and sbt-mode
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

(setq-default sbt:program-name "~/.opt/sbt/bin/sbt")
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  :hook (scala-mode . lsp)
  :config (setq lsp-prefer-flymake nil))

(use-package lsp-ui)

;; Add company-lsp backend for metals
(use-package company-lsp)
(hook! scala-mode-hook
       (hs-minor-mode +1)
       (hs-hide-initial-comment-block)
       )

(provide 'lang-scala-lsp)
;;; lang-scala-lsp.el ends here
