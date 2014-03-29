;;; lang-c --- C & C++
;;; Commentary:
;;; Code:
(require 'lsp)
(add-hook 'c++-mode-hook    'lsp)
(add-hook 'c-mode-hook      'lsp)
(use-package c-eldoc)
(use-package company-c-headers)
(require 'elide-head)
(add-hook 'c-mode-hook (lambda () (font-lock-mode -1)))
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
(add-hook 'c-mode-common-hook 'elide-head)
(provide 'lang-c)
;;; lang-c.el ends here
