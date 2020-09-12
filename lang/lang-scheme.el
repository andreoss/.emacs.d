(require 'geiser)
(setq geiser-active-implementations '(guile))

(hook! scheme-mode-hook
   (paredit-mode)
   (auto-complete-mode)
   (geiser-mode)
   )

(advice-add 'geiser-eval-region :around
            (lambda (f beg end &rest r)
              (eros--eval-overlay
               (apply f beg end r)
               end)))

(advice-add 'geiser-eval-last-sexp :filter-return
            (lambda (r)
              (eros--eval-overlay r (point))))

(advice-add 'geiser-eval-defun :filter-return
            (lambda (r)
              (eeros--eval-overlay
               r
               (save-excursion
                 (end-of-defun)
                 (point)))))

(defun geiser-eval-dwim (arg)
  "Call eval command you want (Do What I Mean).
If the region is active and `transient-mark-mode' is on, call
`eval-region'. Else, call `eval-last-sexp'."
  (interactive "P")
  (if (and transient-mark-mode mark-active)
      (geiser-eval-region (region-beginning) (region-end))
    (geiser-eval-definition arg)))

(evil-define-key 'normal scheme-mode-map
  (key <return>)   'geiser-eval-dwim
  (key C-c C-c)    'geiser-eval-dwim
  )

(provide 'lang-scheme)
;;; lang-scheme.el ends here
