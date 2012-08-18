;;; Code:
(use-package haskell-mode
  :config
  (setq haskell-font-lock-symbols t)
  :hook ((haskell-mode . turn-on-haskell-doc-mode)
         (haskell-mode . turn-on-haskell-indent)
         (haskell-mode . interactive-haskell-mode)))

(provide 'lang-haskell)
