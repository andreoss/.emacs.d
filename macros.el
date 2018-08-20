;;; macros --- Macros
;;; Commentary:
;;; Code:
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

(defmacro alias (x y)
  "Alias X to Y."
  `(defalias ',x ',y))

(defmacro comment (&rest body)
  "Ignore BODY, yield nil."  nil)

(defmacro ==> (params &rest body)
  "Lambda with PARAMS and BODY."
  (if (list? params)
      `#'(lambda (,@params) ,body)
    `#'(lambda (,params) ,body)))

(defmacro --> (&rest body)
  "Turn BODY into a parameter-less lambda."
  (eval `(==> nil ,@body)))

(defmacro theme (theme) "Enable THEME."
          `(progn (require (symbol-concat ',theme '-theme))
                  (enable-theme ',theme)))

(defmacro req (&rest args)
  "Require ARGS."
  `(progn
     ,@ (loop for a in args
              collect
              `(progn (require ',a)))))

(defmacro use (package)
  "Use PACKAGE."
  `(use-package ,package :ensure))

(defmacro for-each (f &rest args)
  "Run F for each of ARGS."
  `(progn ,@(loop for a in args collect `(,f ,a))))

(defmacro async (&rest body)
  "Run BODY asynchronously."
  (let
      ((h (first body)))
    (if (symbol? h)
        `(make-thread (lambda () (progn ,@(rest body)) ',h))
      `(make-thread (lambda () (progn ,@body))))))

(defalias '& 'async)

(defun symbol-concat (&rest args)
  "Concatenates symbolic ARGS."
  (intern (apply 'concat (mapcar (lambda (x) (symbol-name x)) args))))

(defun symbol-assert-bound (s)
  "Assert that symbol S is bound."
  (if (boundp s)
      s
    (error (concat "Symbol " (symbol-name s) " is unbound"))))

(defmacro mode (lang)
  "Mode for LANG."
  `(symbol-concat ',lang '-mode)
  )

(defmacro hook (mode)
  "Hook for MODE."
  `(symbol-concat (mode ,mode) '-hook)
  )

(alias string? stringp)
(alias symbol? symbolp)
(alias list? listp)
(alias array? arrayp)
(alias map? mapp)

(alias empty-string? string-empty-p)
(alias empty-list?   list-empty-p)
(alias empty-array?  array-empty-p)
(alias empty-map?    map-empty-p)

(defmacro array-to-list (a)
  "Turn A into list."
  `(loop for e across ,a collect e))

(defmacro init (mode &rest body)
  "Init hook for MODE; BODY deferred and executed only once."
  (let ((init-hook (cl-gensym)))
    `(progn
       (defun ,init-hook ()
         (message
          (concat "Initializing for " (symbol-name ',mode)))
         ,@body
         (remove-hook (hook ,mode) ',init-hook)
         (funcall (mode ,mode)))
       (add-hook (hook ,mode) ',init-hook))))

(defmacro any? (v fun &rest xs)
  "Check if V passed to FUN with any of XS is `t`."
  `(-any? (==> x ,fun x ,v) ',xs))

(defmacro any-equal? (v &rest xs)
  "V equals any of XS."
  `(any? ,v equal ,@xs))

(defmacro major-mode? (&rest xs)
  "Is Major mode among XS."
  (let
      ((x   (first xs))
       (xxs (rest  xs)))
    (cond ((eq x :not)
           `(not (any-equal? major-mode ,@xxs)))

          (t `(any-equal? major-mode ,@xs)))))

(comment (major-mode? emacs-lisp-mode))

(defmacro if-any-window-system (&rest body)
  "If Emacs running in graphical enviroment execute BODY."
  `(if (not (eq (window-system) 'nil))
       (progn ,@body)))

(defmacro if-x-window-system (&rest body)
  "If Emacs running in X, execute BODY."
  `(if (eq (window-system) 'x) (progn ,@body)))

(defmacro if-bound (sym &rest body)
  "If SYM is bound, execute BODY."
  `(if (fboundp ',sym) (progn ,@body)))

(defmacro key (&rest keys)
  "Turn literal KEYS to key sequence."
  `(kbd (mapconcat 'symbol-name ',keys " ")))

(defmacro pairs (&rest elems)
  "Create list of pairs from ELEMS."
  (if (not (eq (% (length elems) 2) 0))
      (error "Not even number of argumets"))
  `(loop for i from 0 below (length ',elems)
         by 2
         collect (cons (nth i ',elems) (nth (+ i 1) ',elems))))

(defmacro hook! (hook &rest body)
  "Extend HOOK with BODY (wrapped in lambda if necessary)."
  (cond
   ((and (eq (length body) 1) (symbol? (first body)))
    (let ((s (first body)))
      `(add-hook ',hook ',s)))
   (t
    `(add-hook ',hook (lambda () ,@body)))))

(provide 'macros)
;;; macros.el ends here
