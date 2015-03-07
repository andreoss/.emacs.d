(let ((debug-on-error (>= emacs-major-version 26))
      (package-check-signature nil)
      (url-show-status nil)
      (early-init-file (locate-user-emacs-file "early-init.el"))
      (user-init-file (locate-user-emacs-file "init.el")))
  (when (>= emacs-major-version 27)
    (if (file-exists-p early-init-file)
        (load early-init-file)))
  (load user-init-file)
  (run-hooks (quote after-init-hook))
  (run-hooks (quote emacs-startup-hook))
  (run-hooks (quote window-setup-hook)))