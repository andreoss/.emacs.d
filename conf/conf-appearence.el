;;; conf-appearance --- Look and feel
;;; Commentary:
;;; Code:
(scroll-bar-mode +1)
(use-package doom-themes)
(load-theme 'doom-opera-light t)
(defun ai:setup-frame (frame)
  "Setup a FRAME."
  (setq frame (or frame (selected-frame)))
  (with-selected-frame frame
    (set-background-color "#ffffea")
    (set-foreground-color "#000000")
    )
  (set-frame-font "Source Code Pro 10" t (list frame))
  (set-frame-width frame 80)
  (set-frame-height frame 40)
  (fringe-mode '(14 . 7))
  (custom-set-faces
   '(fringe ((t (:background "#f7f7da"))))))
(ai:setup-frame nil)
(add-to-list 'after-make-frame-functions #'ai:setup-frame)
(minibuffer-electric-default-mode +1)
(add-hook 'whitespace-mode-hook (lambda () (font-lock-mode +1)))
(hook! prog-mode-hook (font-lock-mode -1))
(global-set-key (kbd "M-1") 'treemacs)
(use-package feebleline
  :custom
  (feebleline-show-git-branch        t)
  (feebleline-show-dir               t)
  (feebleline-show-time              nil)
  (feebleline-show-previous-buffer   nil)
  :hook
  (after-init . feebleline-mode))
(use-package fringe-current-line
  :config
  (setq-default fcl-fringe-bitmap 'vertical-bar)
  (global-fringe-current-line-mode +1))
(use-package digit-groups)
;; Pop-up windows when display-buffer
(setq pop-up-windows t)
;; Indicate buffer boundaries
(setq-default indicate-buffer-boundaries '((top . left) (t . right)))
(provide 'conf-appearence)
;;; conf-appearence ends here
