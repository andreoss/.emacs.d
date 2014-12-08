;;; init --- init ; -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; Code:
;; Enable readable backtraces when error occurs
(org-babel-load-file
 (expand-file-name (concat user-emacs-directory "setup.org")))
(provide 'init)
;;; init.el ends here
