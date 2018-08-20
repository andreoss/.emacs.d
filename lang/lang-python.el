(setq py-python-command "python3")
(setq python-shell-interpreter "python3")

(use-package lsp-python-ms
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))  ; or lsp-deferred
(setq lsp-python-ms-executable "pyls")
(use-package pyvenv)
(provide 'lang-python)
