;;; lang-common-lisp --- Common Lisp
;;; Commentary:
;; See https://github.com/quicklisp/quicklisp-slime-helper
;;; Code:

(use-package slime-company)
(use-package slime
  :mode (("\\.lisp'"    . lisp-mode)
         ("\\.lsp'"     . lisp-mode)
         ("\\.cl'"      . lisp-mode)
         ("\\.asd'"     . lisp-mode)
         ("\\.fasl'"    . lisp-mode))
  :config (slime-setup '(slime-company))
  :init (setq slime-contribs '(slime-fancy)))

(setq-default inferior-lisp-program "sbcl")
(let ((slime-helper "~/.quicklisp/slime-helper.el"))
  (if (file-exists-p slime-helper)
      (load (expand-file-name slime-helper))))
(setq slime-contribs '(slime-fancy))

(defun slime-eval-last-expression-eros ()
  ""
  (interactive)
  (destructuring-bind (output value)
      (slime-eval `(swank:eval-and-grab-output ,(slime-last-expression)))
    (eros--make-result-overlay (concat output value)
      :where (point)
      :duration eros-eval-result-duration)))

(provide 'lang-common-lisp)
;;; lang-common-lisp.el ends here
