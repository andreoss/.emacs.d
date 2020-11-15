;;; conf-editor --- basic editor functions
;;; Commentary:
;;; Code:
(use-package company)
(add-hook 'after-init-hook 'global-company-mode)
(use-package midnight)
(use-package projectile)
;; Use Emacs session management
(use-package session
  :config
  (setq session-use-package t)
  (session-initialize)
  (add-to-list 'session-globals-exclude 'org-mark-ring))
(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min)
                 (point-max)))
(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning)
                         (region-end))
          (message "Indented selected region."))
      (progn
        (indent-buffer)
        (message "Indented buffer.")))))
(global-set-key (kbd "<f5>") 'revert-buffer)
(use-package yasnippet
  :diminish
  :init
  (yas-global-mode +1))
(use-package editorconfig
  :diminish
  :init
  (editorconfig-mode +1))
(use-package browse-kill-ring)
(use-package counsel
  :diminish
  :init
  (counsel-mode +1))
(use-package rainbow-mode)
(hook! prog-mode-hook rainbow-mode)
(hook! prog-mode-hook hs-minor-mode)
(use-package ivy)
(use-package prescient)
(use-package ivy-prescient)
(use-package company-prescient)
(ivy-prescient-mode +1)
(use-package swiper :after ivy)
(use-package flx)
(use-package flx-ido)
(flx-ido-mode +1)
(setq ivy-re-builders-alist
      '((ivy-switch-buffer . ivy--regex-plus)
        (t . ivy--regex-or-literal)))
(setq ivy-initial-inputs-alist nil)
(setq ido-ignore-files '("\\`#"
                         "\\`.#"
                         "\\`\\.\\./"
                         "\\`\\./"
                         "\\`00"
                         "\\`.*\\.tsk"
                         "\\`ported\\..*"))

(setq ido-ignore-buffers '("\\` "
                           "\\*Buffer List\\*"
                           "\\*Help\\*"
                           "\\*Messages\\*"
                           "\\*Completions\\*"))
(setq ido-enable-flex-matching t)
(setq make-backup-files nil)
(setq create-lockfiles nil)

(hook! prog-mode-hook
       (setq-local show-trailing-whitespace t))

(setq-default indent-tabs-mode            nil
              select-active-regions       t
              mouse-drag-copy-region      nil
              indicate-empty-lines        t
              indicate-buffer-boundaries  t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))
(global-visual-line-mode -1)
(delete-selection-mode   -1)
(global-prettify-symbols-mode +1)
(use-package flycheck
  :init
  (define-fringe-bitmap 'flycheck-fringe-indicator
    (vector #b0000000000000000
            #b0000000000000000
            #b0000000000000000
            #b0000000000000000
            #b0000000000000000
            #b1111111111111111
            #b1111111111111111
            #b1111111111111111
            #b1111111111111111
            #b1111111111111111
            #b1111111111111111
            #b0000000000000000
            #b0000000000000000
            #b0000000000000000
            #b0000000000000000
            #b0000000000000000
            #b0000000000000000) nil 16)
  :custom (flycheck-indication-mode 'right-fringe)
  :hook (prog-mode . global-flycheck-mode)
  :config
  (flycheck-define-error-level 'error
    :severity 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
    :severity 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
    :severity 0
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-info))

(setq select-active-regions t)
(setq select-enable-clipboard t) ; as above
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
(setq default-input-method 'russian-computer)
(use-package reverse-im
  :ensure t
  :custom
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode t))
(defun untabify-buffer ()
  "Replace tabs by spaces."
  (interactive)
  (untabify (point-min) (point-max)))
(defun indent-buffer ()
  "Indent buffer."
  (interactive)
  (indent-region (point-min) (point-max)))
(defun cleanup-buffer (&optional indent)
  "Perform a bunch of operations on the whitespace content of a buffer.  Also indent buffer if INDENT is non-nil."
  (interactive)
  (if indent (indent-buffer))
  (if (and
       (not (eq major-mode 'fundamental-mode))
       (not (eq major-mode 'makefile-gmake-mode)))
      (untabify-buffer))
  (delete-trailing-whitespace))
(define-key isearch-mode-map (kbd "<up>")    'isearch-ring-retreat)
(define-key isearch-mode-map (kbd "<down>")  'isearch-ring-advance)
(define-key isearch-mode-map (kbd "<left>")  'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward)
(global-unset-key (kbd "<M-drag-mouse-1>"))
(global-unset-key (kbd "<M-mouse-1>"))
(global-unset-key (kbd "<M-mouse-2>"))
(global-unset-key (kbd "<M-mouse-3>"))
(global-unset-key (kbd "C-x f"))
(electric-pair-mode +1)
(electric-indent-mode +1)
(setq-default electric-pair-pairs
              '(pairs
                ?\" ?\"
                ?\{ ?\}
                ?«  ?»
                ?‘  ?’
                ?｢  ?｣
                ?“  ?”
                )
              )
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)
(global-set-key (kbd "C-M-S-l") 'indent-region-or-buffer)
(global-set-key [mouse-2]    'mouse-yank-primary)
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "<f5>")  'revert-buffer)
(hook! prog-mode-hook
       (unless (major-mode? org-mode makefile-mode)
         (hook! before-save-hook cleanup-buffer)))
(use-package company
  :diminish
  :custom
  (company-show-numbers t)
  :init
  (global-company-mode +1)
  )
(defun *company-active-return ()
  (interactive)
  (if (company-explicit-action-p)
      (company-complete)
    (call-interactively
     (or (key-binding (this-command-keys))
         (key-binding (kbd "RET"))))))
(define-key company-active-map (kbd "<return>") #'*company-active-return)
(define-key company-active-map (kbd "RET") #'*company-active-return)
(use-package expand-region)
(global-auto-revert-mode +1)
(require 'recentf)
(recentf-mode +1)
(use-package centered-cursor-mode
  :init
  (centered-cursor-mode +1))
(global-eldoc-mode +1)
(use-package undo-tree)
(global-undo-tree-mode +1)
(global-reveal-mode +1)
(use-package hl-todo
  :init
  (global-hl-todo-mode))
;; Borrowed from
;; http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html
(define-key ctl-x-map "\C-i"
  #'endless/ispell-word-then-abbrev)
(defun endless/simple-get-word ()
  "Get word from Ispell."
  (car-safe (save-excursion (ispell-get-word nil))))
(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (endless/simple-get-word))
                 ;; Word was corrected or used quit.
                 (if (ispell-word nil 'quiet)
                     nil ; End the loop.
                   ;; Also end if we reach `bob'.
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word)
        (backward-char))
      (setq aft (endless/simple-get-word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))
(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)
(setq visual-line-fringe-indicators
      '(left-curly-arrow right-curly-arrow))
(hook! text-mode-hook        turn-on-visual-line-mode)
(hook! fundamental-mode-hook turn-on-visual-line-mode)
(hook! org-mode-hook         turn-on-visual-line-mode)
(hook! prog-mode-hook        (font-lock-mode -1))
(use-package backup-each-save
  :init
  (add-hook
   (make-local-variable 'after-save-hook)
   'backup-each-save))
(use-package keyfreq
  :init
  (keyfreq-mode +1))
(setq vc-follow-symlinks nil)
(add-hook
 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(use-package paredit
  :diminish
  :hook
  (lisp-mode-hook .paredit-mode))
(use-package paren
  :init
  (setq-default show-paren-style 'parenthesis)
  (show-paren-mode +1))
(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy))
(global-set-key (kbd "C-c i") 'helm-imenu)
(use-package beacon :config (beacon-mode +1))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(defun undo-tree-split-side-by-side (original-function &rest args)
  "Split undo-tree side-by-side"
  (let ((split-height-threshold nil)
        (split-width-threshold 0))
    (apply original-function args)))
(advice-add 'undo-tree-visualize :around #'undo-tree-split-side-by-side)
(setq projectile-globally-ignored-directories
      '(
        ".bloop"
        ".bzr"
        "_darcs"
        ".ensime_cache"
        ".eunit"
        "_FOSSIL_"
        ".fslckout"
        ".git"
        ".hg"
        ".idea"
        ".metals"
        ".stack-work"
        ".svn"
        )
      )
(use-package vlf
  :config
  (require 'vlf-setup))
(require 'ispell)
(setq auto-revert-verbose nil)
(setq-default ispell-program-name "aspell")
(mouse-avoidance-mode 'exile)
(provide 'conf-editor)
;;; conf-editor.el ends here
