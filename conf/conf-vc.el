;;; conf-vc --- Git Configuration
;;; Commentary:
;;; Code:
(use-package magit)
(use-package magit-gitflow
  :after magit)
(use-package evil-magit
  :after magit
  :init (evil-magit-init))
(use-package forge
  :after magit)
(global-key-binding (kbd "C-x g") 'magit)
(use-package git-gutter
  :diminish
  :init
  (global-git-gutter-mode +1))
(use-package git-commit
  :hook ((git-commit-mode . flyspell-mode)
         (git-commit-mode . git-commit-save-message)
         (git-commit-mode . turn-on-auto-fill)))
(use-package gitconfig-mode
  :mode (("/\\.gitconfig\\'"      . gitconfig-mode)
         ("/\\.git/config\\'"     . gitconfig-mode)
         ("/modules/.*/config\\'" . gitconfig-mode)
         ("/git/config\\'"        . gitconfig-mode)
         ("/\\.gitmodules\\'"     . gitconfig-mode)
         ("/etc/gitconfig\\'"     . gitconfig-mode)))
(use-package gitattributes-mode
  :mode (("/\\.gitattributes\\'"  . gitattributes-mode)
         ("/info/attributes\\'"   . gitattributes-mode)
         ("/git/attributes\\'"    . gitattributes-mode)))
(use-package gitignore-mode
  :mode (("/\\.gitignore\\'"      . gitignore-mode)
         ("/info/exclude\\'"      . gitignore-mode)
         ("/git/ignore\\'"        . gitignore-mode)))
(use-package fullframe
  :config
  (fullframe magit-status magit-mode-quit-window nil))
(provide 'conf-vc)
;;; conf-vc.el ends here
