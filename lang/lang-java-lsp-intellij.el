(package! lsp-mode)
(package! lsp-ui)
(package! company-lsp)
(package! lsp-intellij)
(package! kotlin-mode)

(require 'lsp-mode)

(with-eval-after-load 'lsp-mode
  (require 'lsp-intellij)
  (add-hook 'java-mode-hook #'lsp-intellij-enable))


(setq lsp-intellij-server-port 8080)

(require 'lsp-ui)
(add-hook 'lsp-after-open-hook #'lsp-ui-mode)

(require 'company-lsp)
(setq company-lsp-enable-snippet t
      company-lsp-cache-candidates t)
;; (push 'company-lsp company-backends)
;; (push 'java-mode company-global-modes)
;; (push 'kotlin-mode company-global-modes) ;; if using Kotlin
