;;; lang-c --- C & C++
;;; Commentary:
;;; Code:
(require 'elide-head)
(use-package c-eldoc)
(use-package company-c-headers)
(use-package ccls)
(add-hook 'c-mode-hook        'c-turn-on-eldoc-mode)
(add-hook 'c-mode-common-hook 'elide-head)
(provide 'lang-c)
;;; lang-c.el ends here
