;;; lang-c --- C & C++
;;; Commentary:
;;; Code:

(safe-wrap
 (lambda () (require 'irony))
 (lambda () (use-package irony)))

(safe-wrap
 (lambda ()  (require 'irony-eldoc))
 (lambda ()  (use-package irony-eldoc))
 )

(add-hook 'c++-mode-hook    'irony-mode)
(add-hook 'c-mode-hook      'irony-mode)
(add-hook 'irony-mode-hook  'irony-cdb-autosetup-compile-options)
(add-hook 'irony-mode-hook  'irony-eldoc)


;;(use-package c-eldoc)
;; (use-package company-c-headers)

;; (require 'elide-head)

;; (add-hook 'c-mode-hook (lambda () (font-lock-mode -1)))
;; (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
;; (add-hook 'c-mode-common-hook 'elide-head)

(provide 'lang-c)
;;; lang-c.el ends here
