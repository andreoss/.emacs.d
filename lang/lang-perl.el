;;; lang-perl --- Perl
;;; Commentary:
;;; Code:
(use-package perl6-mode)

;; https://raw.github.com/illusori/emacs-flymake-perlcritic/master/flymake-perlcritic.el
(setq flymake-perlcritic-severity 5)
(use-package flymake-cursor)

(defmacro save-current-point (body)
  "Save current point; execute BODY; go back to the point."
  `(let ((p (point)))
     (progn ,body (goto-char p))))

(defmacro shell-command-on-buffer (&rest args)
  "Mark the whole buffer; pass ARGS to `shell-command-on-region'."
  `(shell-command-on-region (point-min) (point-max) ,@args))

(defun perltidy-buffer ()
  "Run perltidy on the current buffer."
  (interactive)
  (save-current-point
   (shell-command-on-buffer
    "perltidy -q"
    (not :output-buffer)
    :replace)))

(setq-default cperl-indent-level 4)
(setq-default cperl-continued-statement-offset 0)
(setq-default cperl-extra-newline-before-brace t)

(defun my/perl-mode-hook ()
  (add-hook 'before-save-hook 'perltidy-buffer
            :append :local)

  (local-set-key (kbd "C-c C-c")
                 'cperl-perldoc-at-point)
  (local-set-key (kbd "M-.") 'ffap)

  (font-lock-mode -1)
  (flymake-mode   +1))

(add-hook 'perl-mode-hook  'my/perl-mode-hook)

(use-package ffap-perl-module)
(eval-after-load 'ffap
  '(require 'ffap-perl-module))

(hook! perl-mode (font-lock-mode -1))

(evil-define-key 'normal perl-mode-map
  (kbd "g d")        'cperl-perldoc-at-point)

(provide 'lang-perl)
;;; lang-perl.el ends here
