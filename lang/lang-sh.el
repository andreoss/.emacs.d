;;; lang-sh --- shell configuration
;;; Commentary:
;; shellcheck
;;; Code:
(use-package flymake-shell
  :config
  (hook! sh-mode-hook flymake-shell-load)
  )
(hook! sh-mode-hook (font-lock-mode -1))
(evil-define-key 'normal sh-mode-map (kbd "g d")  'man-follow)
(evil-define-key 'normal sh-mode-map (kbd "RET")  'sh-execute-region)
(evil-define-key 'visual sh-mode-map (kbd "RET")  'sh-execute-region)
(provide 'lang-sh)
;;; lang-sh.el ends here
