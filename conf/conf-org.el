;;; conf-org --- Org
;;; Commentary:
;;; Code:

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

 ;; by convention: "C-c a" opens agenda
 ;; by convention: "C-c c" captures stuff
 ;; by convention: "C-c l" stores a link to this heading
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)
(setq-default org-log-done t)

(use-package org-bullets)
(use-package ob-restclient)

(require 'org-indent)
(hook! org-mode-hook org-indent-mode)

(org-babel-do-load-languages 'org-babel-load-languages
                             '((perl          . t)
                               (java          . t)
                               (sql           . t)
                               (python        . t)
                               (js            . t)
                               (scheme        . t)
                               (restclient    . t)
                               (clojure       . t)))

(setq org-confirm-babel-evaluate nil)

(setq org-src-tab-acts-natively t)

(defvar org-babel-eval-verbose t
  "A non-nil value makes `org-babel-eval' display.")

(defun org-babel-eval (cmd body)
  "Run CMD on BODY.
If CMD succeeds then return its results, otherwise display
STDERR with `org-babel-eval-error-notify'."
  (let ((err-buff (get-buffer-create " *Org-Babel Error*")) exit-code)
    (with-current-buffer err-buff (erase-buffer))
    (with-temp-buffer
      (insert body)
      (setq exit-code
            (org-babel--shell-command-on-region
             (point-min) (point-max) cmd err-buff))
      (if (or (not (numberp exit-code)) (> exit-code 0)
              (and org-babel-eval-verbose (> (buffer-size err-buff) 0))) ; new condition
          (progn
            (with-current-buffer err-buff
              (org-babel-eval-error-notify exit-code (buffer-string)))
            nil)
        (buffer-string)))))

(with-eval-after-load 'org
  (defvar-local rasmus/org-at-src-begin -1
    "Variable that holds whether last position was a ")

  (defvar rasmus/ob-header-symbol ?☰
    "Symbol used for babel headers")

  (defun rasmus/org-prettify-src--update ()
    (let ((case-fold-search t)
          (re "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*")
          found)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (goto-char (match-end 0))
          (let ((args (org-trim
                       (buffer-substring-no-properties (point)
                                                       (line-end-position)))))
            (when (org-string-nw-p args)
              (let ((new-cell (cons args rasmus/ob-header-symbol)))
                (cl-pushnew new-cell prettify-symbols-alist :test #'equal)
                (cl-pushnew new-cell found :test #'equal)))))
        (setq prettify-symbols-alist
              (cl-set-difference prettify-symbols-alist
                                 (cl-set-difference
                                  (cl-remove-if-not
                                   (lambda (elm)
                                     (eq (cdr elm) rasmus/ob-header-symbol))
                                   prettify-symbols-alist)
                                  found :test #'equal)))
        ;; Clean up old font-lock-keywords.
        (font-lock-remove-keywords nil prettify-symbols--keywords)
        (setq prettify-symbols--keywords (prettify-symbols--make-keywords))
        (font-lock-add-keywords nil prettify-symbols--keywords)
        (while (re-search-forward re nil t)
          (font-lock-flush (line-beginning-position) (line-end-position))))))

  (defun rasmus/org-prettify-src ()
    "Hide src options via `prettify-symbols-mode'.

  `prettify-symbols-mode' is used because it has uncollpasing. It's
  may not be efficient."
    (let* ((case-fold-search t)
           (at-src-block (save-excursion
                           (beginning-of-line)
                           (looking-at "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*"))))
      ;; Test if we moved out of a block.
      (when (or (and rasmus/org-at-src-begin
                     (not at-src-block))
                ;; File was just opened.
                (eq rasmus/org-at-src-begin -1))
        (rasmus/org-prettify-src--update))
      ;; Remove composition if at line; doesn't work properly.
      ;; (when at-src-block
      ;;   (with-silent-modifications
      ;;     (remove-text-properties (match-end 0)
      ;;                             (1+ (line-end-position))
      ;;                             '(composition))))
      (setq rasmus/org-at-src-begin at-src-block)))

  (defun rasmus/org-prettify-symbols ()
    (mapc (apply-partially 'add-to-list 'prettify-symbols-alist)
          (cl-reduce 'append
                     (mapcar (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                             `(("#+begin_src" . ?↠) ;; ➤ 🖝 ➟ ➤ ✎
                               ("#+end_src"   . ?↞) ;; ⏹
                               ("#+results:"  . ?⤵) ;; ⏹
                               ("#+header:" . ,rasmus/ob-header-symbol)
                               ;; ("#+name:" . ?)
                               ("#+begin_quote" . ?»)
                               ("CLOCK:" . ?⏲)
                               ("#+end_quote" . ?«)))))
    (turn-on-prettify-symbols-mode)
    ;; (add-hook 'post-command-hook 'rasmus/org-prettify-src t t)
    )
  (hook! org-mode-hook rasmus/org-prettify-symbols))
;; Switch to using enchant as our spell-checking backend (fallback to ispell)
(setq ispell-program-name
      (or (executable-find "enchant")
          (executable-find "ispell")
          "ispell"))

;; Use langtool for grammar checking; ensure languagetool exists in
;; system
(use-package langtool
  :config
  (setq langtool-bin
        (or (executable-find "languagetool")
            "languagetool")))
(use-package org-jira
  :config
  (setq jiralib-url (getenv "JIRA_URL")
        org-jira-download-dir "~/tmp"
        org-jira-working-dir (concat "~/.jira/" (format-time-string "%Y")) )
  )
(provide 'conf-org)
;;; conf-org.el ends here
