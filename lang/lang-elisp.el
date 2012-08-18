;;; lang-elisp --- Emacs Lisp
;;; Commentary:
;;; Code:
(use-package helpful
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)
         ("C-h ." . helpful-at-point)))
(use-package emr
  :init
  (autoload 'emr-show-refactor-menu "emr")
  (define-key prog-mode-map (kbd "M-RET")
    'emr-show-refactor-menu)
  (eval-after-load "emr" '(emr-initialize))
  )
(use-package elisp-slime-nav :diminish)
(use-package elisp-lint)
;; Overlays
(advice-add 'eval-region :around
            (lambda (f beg end &rest r)
              (eros--eval-overlay
               (apply f beg end r)
               end)))
(advice-add 'eval-last-sexp :filter-return
            (lambda (r) (eros--eval-overlay r (point))))
(advice-add 'eval-defun :filter-return
            (lambda (r) (eros--eval-overlay
                         r
                         (save-excursion
                           (end-of-defun)
                           (point)))))
(evil-leader/set-key-for-mode 'emacs-lisp-mode "e" 'eval-buffer)

(use-package eros)
(hook! emacs-lisp-mode-hook eros-mode)

(defun eval-dwim (arg)
  "Call eval command you want (Do What I Mean).
If the region is active and `transient-mark-mode' is on, call
`eval-region'. Else, call `eval-last-sexp'."
  (interactive "P")
  (if (and transient-mark-mode mark-active)
      (eval-region (region-beginning) (region-end))
    (eval-defun arg)))
(setq-default flycheck-emacs-lisp-load-path 'inherit)
(hook! emacs-lisp-mode-hook paredit-mode)
(hook! emacs-lisp-mode-hook elisp-slime-nav-mode)
(hook! emacs-lisp-mode-hook eldoc-mode)
(hook! emacs-lisp-mode-hook
       (setq-local prettify-symbols-alist
                   (pairs "<="          ?≤
                          ">="          ?≥
                          "funcall"     ?φ
                          "lambda"      ?λ
                          "eval"        ?ε
                          "&rest"       ?…
                          "&optional"   ?¿
                          )))
(use-package emr)
(evil-define-key   'normal emacs-lisp-mode-map
  (key g s)        'elisp-slime-nav-find-elisp-thing-at-point
  (key M-.)        'elisp-slime-nav-find-elisp-thing-at-point
  (key g l)        'elisp-slime-nav-describe-elisp-thing-at-point
  (key g RET)      'elisp-slime-nav-describe-elisp-thing-at-point
  (key <return>)   'eval-dwim
  (key C-c C-c)    'eval-dwim
  (key C-c C-b)    'eval-buffer
  (key C-c r f)    'emr-el-inline-function
  (key C-c r v)    'emr-el-inline-variable
  (key C-<return>) 'emr-el-eval-and-replace
  )
;; Autocompile Emacs Lisp code
(use-package auto-compile
  :init
  (setq load-prefer-newer t)
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter t))

(define-key emacs-lisp-mode-map [(meta .)] 'find-function)

;;; Display page breaks with an horizontal line instead of ^L.
;;; Note: To insert a page break: C-q C-l
;;;       To jump to the previous/next page break: C-x [ and C-x ]
(use-package page-break-lines)
(add-hook 'emacs-lisp-mode-hook #'turn-on-page-break-lines-mode)

;;; Animation when evaluating a defun or a region:
(use-package highlight)
(use-package eval-sexp-fu)

(provide 'lang-elisp)
;;; lang-elisp.el ends here
