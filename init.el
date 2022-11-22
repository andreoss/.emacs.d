
;;; init --- init ; -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; Code:
(require 'org)
(defun x/org-babel-load-file (directory file &optional compile)
  "Load Emacs Lisp source code blocks in the Org FILE.
This function exports the source code using `org-babel-tangle'
and then loads the resulting file using `load-file'.  With
optional prefix argument COMPILE, the tangled Emacs Lisp file is
byte-compiled before it is loaded."
  (interactive "fFile to load: \nP")
  (let ((tangled-file (concat directory (file-name-base file) ".el")))
    ;; Tangle only if the Org file is newer than the Elisp file.
    (unless (org-file-newer-than-p
	     tangled-file
	     (file-attribute-modification-time
	      (file-attributes (file-truename file))))
      (org-babel-tangle-file file
                             tangled-file
                             (rx string-start
                                 (or "emacs-lisp" "elisp")
                                 string-end)))
    (if compile
	(progn
	  (byte-compile-file tangled-file)
	  (load-file (byte-compile-dest-file tangled-file))
	  (message "Compiled and loaded %s" tangled-file))
      (load-file tangled-file)
      (message "Loaded %s" tangled-file)))
  )
(x/org-babel-load-file
 user-emacs-directory
 (expand-file-name
  (concat
   (file-name-directory
    (or load-file-name user-emacs-directory))
   "README.org")))
(provide 'init)
;;; init.el ends here
