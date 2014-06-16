;;; conf-appearance --- Look and feel
;;; Commentary:
;;; Code:
(use-package dashboard
  :ensure t
  :config
    (dashboard-setup-startup-hook)
    (setq dashboard-items '((recents  . 20)
                            (projects . 20)))
    (setq dashboard-banner-logo-title ""))
(use-package better-defaults)
(scroll-bar-mode +1)
(defun ai:setup-frame (frame)
  "Setup a FRAME."
  (setq frame (or frame (selected-frame)))
  (if-any-window-system
   (load-theme 'tango t)
   (with-selected-frame frame
     (set-background-color "#ffffea")
     (set-foreground-color "#000000")
     )
   (set-frame-parameter frame 'internal-border-width 16)
   (set-frame-width frame 80)
   (set-frame-height frame 40)
   (fringe-mode '(14 . 7))
   (custom-set-faces
    '(fringe ((t (:background "#f7f7da"))))))
  )
(add-hook 'after-init-hook
          (lambda ()
            (ai:setup-frame nil)) t)
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
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'default))
(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code ?↩ 'default))
(custom-set-faces
 '(header-line ((t (:background "#f7f7da")))))
(use-package fringe-current-line
  :config
  (setq-default fcl-fringe-bitmap 'vertical-bar)
  (global-fringe-current-line-mode +1))
(use-package digit-groups)
;; Pop-up windows when display-buffer
(setq pop-up-windows t)
;; Indicate buffer boundaries
(setq-default indicate-buffer-boundaries '((top . left) (t . right)))
(set-scroll-bar-mode 'right)
(provide 'conf-appearence)
;;; conf-appearence ends here
