;;; conf-evil --- Evil configuration
;;; Commentary:
;;; Penance.
;;; Code:
(use-package evil
  :preface
  (viper-go-away)
  (setq-default evil-want-keybinding nil)
  :init
  (evil-mode +1)
  (evil-global-set-key 'normal (kbd ";") 'evil-ex)
  (evil-global-set-key 'insert (kbd "C-a") 'beginning-of-line)
  (evil-global-set-key 'insert (kbd "C-e") 'end-of-line)
  (evil-global-set-key 'insert (kbd "C-k") 'kill-line)
  (loop for (mode . state) in
        '((exwm-mode                . emacs)
          (sbt-mode                 . insert)
          (shell-mode               . insert)
          (eshell-mode              . insert))
        do (evil-set-initial-state mode state))
  :config
  (setq evil-insert-state-cursor     '("#952111" (bar  . 3))
        evil-normal-state-cursor     '("#33A050" (hbar . 4))
        evil-operator-state-cursor   '(box)
        evil-emacs-state-cursor      '(bar)
        evil-motion-state-cursor     '(bar)
        evil-visual-state-cursor     '("#11312F" hbar . hollow))
  ;; Disable mode switching
  (define-key evil-normal-state-map   (kbd "C-z") 'evil-normal-state)
  (define-key evil-emacs-state-map    (kbd "C-z") 'evil-emacs-state)
  (define-key evil-insert-state-map   (kbd "C-z") 'evil-normal-state)

  ;; Evil exchange, easily swap two things
  (use-package evil-exchange
    :commands (evil-exchange
               evil-exchange-cancel)
    :init
    (define-key evil-normal-state-map "gx" #'evil-exchange)
    (define-key evil-visual-state-map "gx" #'evil-exchange)
    (define-key evil-normal-state-map "gX" #'evil-exchange-cancel)
    (define-key evil-visual-state-map "gX" #'evil-exchange-cancel))
  ;; Esc quits from everything
  (define-key evil-normal-state-map [escape] #'keyboard-quit)
  (define-key evil-emacs-state-map [escape] #'evil-normal-state)
  (define-key evil-visual-state-map [escape] #'keyboard-quit)
  (define-key evil-motion-state-map [escape] #'evil-normal-state)
  (define-key evil-operator-state-map [escape] #'evil-normal-state)
  (define-key minibuffer-local-map [escape] #'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] #'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] #'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] #'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] #'minibuffer-keyboard-quit)
  (use-package evil-matchit
    :commands (evilmi-inner-text-object
               evilmi-outer-text-object)
    :init
    ;; evil matchit, jump between matching tags and keywords
    (define-key evil-normal-state-map "%" #'evilmi-jump-items)
    (define-key evil-visual-state-map "%" #'evilmi-jump-items)
    (define-key evil-inner-text-objects-map "%" #'evilmi-inner-text-object)
    (define-key evil-outer-text-objects-map "%" #'evilmi-outer-text-object)

    :config
    (evilmi-init-plugins))

  (use-package evil-collection :init (evil-collection-init))
  (use-package evil-goggles
    :init
    (evil-goggles-mode +1)
    (setq-default evil-goggles-duration 0.5)
    (custom-set-faces
     '(evil-goggles-delete-face ((t (:inherit magit-diff-removed))))
     '(evil-goggles-yank-face   ((t (:inherit magit-diff-base-highlight))))
     '(evil-goggles-paste-face  ((t (:inherit magit-diff-added))))
     '(evil-goggles-paste-face  ((t (:inherit magit-diff-added))))
     '(evil-goggles-commentary-face ((t (:inherit magit-diff-context-highlight))))
     '(evil-goggles-indent-face ((t (:inherit magit-diff-added-highlight))))
     ))
  (use-package evil-leader
    :config
    (defun e-top ()
      (interactive)
      (eshell-command "top")
      )
    (global-evil-leader-mode +1)
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "<SPC>" 'save-buffer
      "s"  'better-shell-shell
      "x"  'eshell-here
      "g"  'magit
      "d" 'dired
      "f"  'projectile-find-file-dwim
      "b"  'switch-to-buffer
      "k"  'kill-buffer
      "i"  'indent-buffer
      "&"  'async-shell-command
      "/"  'swiper
      "S g" 'guix
      "S t" 'e-top
      "j r" 'nodejs-repl-switch-to-repl
      "j j" 'nodejs-repl-send-buffer
      )
    (evil-leader/set-key "v m" 'menu-bar-mode)
    (evil-leader/set-key "v w" 'whitespace-mode)
    (evil-leader/set-key "v c" 'font-lock-mode)
    (evil-leader/set-key "v n" 'linum-mode))
  )

(hook! server-after-make-frame-hook
       (if-x-window-system
        (setq-default evil-emacs-state-tag    nil
                      evil-motion-state-tag   nil
                      evil-normal-state-tag   nil
                      evil-operator-state-tag nil
                      evil-replace-state-tag  nil
                      evil-visual-state-tag   nil
                      evil-insert-state-tag   nil)))

(use-package undo-tree)
(use-package evil-commentary
  :after evil
  :init (evil-commentary-mode +1))

(use-package avy
  :after evil
  :init
  (global-set-key (kbd "M-t") 'avy-goto-word-1)
  (setq avy-style 'words)
  (evil-global-set-key 'normal (kbd "g h") 'avy-goto-char)
  (evil-global-set-key 'normal (kbd "g b") 'avy-goto-word-1)
  (evil-global-set-key 'normal (kbd "g t") 'avy-goto-line)
  (evil-global-set-key 'normal (kbd "g :") 'avy-goto-line)
  )

(evil-global-set-key 'normal (kbd "M-i") 'company-complete)
(evil-global-set-key 'insert (kbd "M-i") 'company-complete)

(add-function
 :after (symbol-function 'recenter-top-bottom) #'evil-show-file-info)

(use-package evil-snipe
  :after evil
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)
  )

(provide 'conf-evil)
;;; conf-evil.el ends here
