;;; conf-vc --- Git Configuration
;;; Commentary:
;;; Code:
(use-package magit)
(use-package magit-gitflow
  :after magit)
(use-package evil-magit
  :after magit
  :init (evil-magit-init))
(ignore-errors
  (use-package forge
    :after magit))
(global-set-key (kbd "C-x g") 'magit-status)
(use-package git-gutter
  :diminish
  :init
  (global-git-gutter-mode +1))
(use-package git-commit
  :hook ((git-commit-mode . flyspell-mode)
         (git-commit-mode . git-commit-save-message)
         (git-commit-mode . turn-on-auto-fill)))
(custom-set-faces
 '(git-gutter:added ((t (:inherit default :foreground "medium sea green" :weight bold))))
 '(git-gutter:deleted ((t (:inherit default :foreground "sienna" :weight bold))))
 '(git-gutter:modified ((t (:inherit default :foreground "dark orchid" :weight bold))))
 '(git-gutter:unchanged ((t (:inherit default :background "LemonChiffon4"))))
 )
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
