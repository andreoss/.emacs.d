;;; conf-wm --- Buffer management
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'cl)
  (require 'evil))
(require 'centered-cursor-mode)
(require 'uniquify)
;; C-w handles windows in all states
(global-set-key (key C-w) 'evil-window-map)
(evil-global-set-key 'insert (key C-w) 'evil-window-map)
(evil-global-set-key 'emacs  (key C-w) 'evil-window-map)
(evil-global-set-key 'normal (key C-w) 'evil-window-map)
(use-package winum
  :config (require 'evil-winner))
(setq initial-frame-alist
      '((top . 0) (left . 0) (width . 80) (height . 20)))
;; Prefer vertical splits
;; https://www.emacswiki.org/emacs/HorizontalSplitting
(setq-default split-width-threshold 160)
(setq-default use-dialog-box nil)
(setq frame-title-format '(
                           (:eval (or (buffer-file-name) (buffer-name))) vc-mode)
      )
(use-package ace-window)
(global-set-key (kbd "C-c p") #'ace-window)
;;(require 'transpose-frame)

(defun switch-to-previous-buffer ()
  "Switch to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "C-c e") 'eshell)
(global-set-key (kbd "C-c m") 'man)
(global-set-key (kbd "M-`")   'menu-bar-open)

(hook! shell-mode-hook
     (local-set-key
      (kbd "C-c s") 'delete-window)
     (local-set-key
      (kbd "C-c C-s") 'delete-window)
     (local-set-key
      (kbd "C-l") 'comint-clear-buffer)
     (ansi-color-for-comint-mode-on))

;; (defalias 'window-at-side-p 'window-at-side-p)
(defmacro on-the-side (side &optional size )
  "Buffer placed on SIDE.  SIZE is either width or height."
  (or size (setq size  0.3))
  (list 'quote (list
                (list 'display-buffer-in-side-window)
                (cons 'side  side)
                (if (or (eq side 'right) (eq side 'left))
                    (cons 'window-width  size)
                  (cons 'window-height size)))))
;; Decrease font size in side buffers
(lexical-let
    ((text-dec (lambda () (if (eq window-system 'x) (text-scale-decrease 1)))))
  (loop for mode in
        '(Man
          Info
          help
          shell
          eshell
          xref--xref-buffer
          magit-status
          ielm
          ibuffer
          ensime-inf
          completion-list
          pdf-outline-buffer
          sbt)
        do
        (add-hook (symbol-concat mode '-mode-hook) text-dec)))

(defun kill-or-bury-buffer ()
  "Kill saved or bury unsaved buffer."
  (interactive)
  (if (and (buffer-file-name) (buffer-modified-p))
      (progn
        (message "buffer burried: %s" (current-buffer))
        (bury-buffer))
    (progn
      (message "buffer killed: %s" (current-buffer))
      (kill-buffer))))

(evil-global-set-key 'normal (kbd "q")   'kill-or-bury-buffer)
(evil-global-set-key 'normal (kbd "C-s") 'save-buffer)
(evil-global-set-key 'normal (kbd "C-s") 'save-buffer)

(global-set-key (kbd "C-x k")   'kill-or-bury-buffer)
(global-set-key (kbd "C-x C-k") 'kill-buffer)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-c C-f") 'find-file-other-window)

(use-package follow-mouse)
(turn-on-follow-mouse)

(use-package nav-flash)
(defun nav-flash-show* (&rest xs) (nav-flash-show))
(add-function :after (symbol-function 'other-window) #'nav-flash-show*)
(add-function :after (symbol-function 'select-window) #'nav-flash-show*)
(add-hook 'imenu-after-jump-hook 'nav-flash-show nil t)
;;; Zoom
(use-package default-text-scale)
(define-key global-map [(control +)] (function default-text-scale-increase))
(define-key global-map [(control -)] (function default-text-scale-decrease))
(define-key global-map [(control mouse-4)] (function default-text-scale-increase))
(define-key global-map [(control mouse-5)] (function default-text-scale-decrease))
(defun linum-cycle ()
  (interactive)
  (cond ((not display-line-numbers)
         (setq display-line-numbers 'relative))
        ((equal display-line-numbers 'relative)
         (setq display-line-numbers t))
        ((equal display-line-numbers t)
         (setq display-line-numbers nil))))

(global-set-key (kbd "C-c L") #'linum-cycle)
(global-set-key (kbd "C-c C-l") #'linum-cycle)

(use-package which-key
  :init (which-key-mode +1)
  :diminish which-key-mode
  :config
  (setq which-key-sort-order nil
        which-key-side-window-max-height 0.33)

  (add-to-list 'which-key-description-replacement-alist
               `(,(rx "evil-"
                      (or "a" "an" "inner")
                      "-"
                      (group (zero-or-more not-newline)))
                 . "\\1")))

(provide 'conf-wm)
;;; conf-wm.el ends here
