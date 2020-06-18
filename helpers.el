;;; helpers --- helpers
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'cl))
(defun add-to-loadpath (base &optional add-subdirs)
  "Add the BASE directory to the load path.
If ADD-SUBDIRS is non-nil, the subdirectories are also added to the path"
  (add-to-list 'load-path base)
  (message base)
  (if add-subdirs
      (dolist (f (directory-files base))
        (let ((name (concat base "/" f)))
          (when (and (file-directory-p name)
                     (not (string= "." (substring f 0 1))))
            (add-to-list 'load-path name))))))

(defun update-env-var (var new-path)
  "Add to environment variable VAR a path NEW-PATH.
E.g: (update-env-var \"PATH\" \"/usr/local/bin\")"
  (setenv var (concat new-path ":" (getenv var))))

(defmacro safe-wrap (fn &rest clean-up)
  "Try to eval FN; if unsuccessful run CLEAN-UP."
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval (progn ,fn))
           ('error
            (message (format "Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)
     ,@clean-up))
(defalias 'try 'safe-wrap)

(defun save-all-buffers ()
  "Save all the buffers."
  (interactive)
  (save-excursion
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (if (and (buffer-file-name)
               (buffer-modified-p))
          (basic-save-buffer)))))

(defun un-require-namespace-symbols (ns)
  "Symbols in pseudo-namespace NS."
  (loop for s being the symbols if
        (string-match-p
         (concat "^" (symbol-name ns) "-")
         (symbol-name s))
        collect s))

(defun un-require (ns)
  "Undefine all symbols inside NS."
  (loop for s in (un-require-namespace-symbols ns)
        do
        (if (functionp s)
            (fmakunbound s)
          (makunbound s))))

(if (not (fboundp 'make-thread))
    (defun make-thread (function &optional name)
      (funcall function))
  )
(defun set-exec-path ()
  "Set variable `exec-path' and PATH environment according to the user's shell."
  (interactive)
  (let* ((command "/bin/bash -ic 'echo $PATH' 2>/dev/null")
         (output  (shell-command-to-string command))
         (path    (replace-regexp-in-string "[\t \n]*$" "" output)))
    (setenv "PATH" path)
    (setq exec-path (split-string path path-separator))))
(defun </> (&rest args)
  "Concatinate directories ARGS into a path."
  (mapconcat 'identity args "/"))

(provide 'helpers)
;;; helpers.el ends here
