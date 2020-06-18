;;; lang-java-lsp -- Java LSP
;;; Commentary:
;;; Code:
(require 'cc-mode)
(use-package treemacs)
(use-package yasnippet)
(use-package hydra)
(use-package groovy-mode)
(use-package kotlin-mode)
(use-package lsp-java
  :after lsp-mode
  :bind (("C-M-b" . lsp-find-implementation))
  :config (add-hook 'java-mode-hook 'lsp))
(hook! prog-mode-hook
       (hs-minor-mode +1)
       (hs-hide-initial-comment-block)
       )
;; eclipse-java-style is the same as the "java" style (copied from
;; cc-styles.el) with the addition of (arglist-cont-nonempty . ++) to
;; c-offsets-alist to make it more like default Eclipse formatting -- function
;; arguments starting on a new line are indented by 8 characters
;; (++ = 2 x normal offset) rather than lined up with the arguments on the
;; previous line
(defconst eclipse-java-style
  '((c-basic-offset . 4)
    (c-comment-only-line-offset . (0 . 0))
    ;; the following preserves Javadoc starter lines
    (c-offsets-alist . ((inline-open . 0)
                        (topmost-intro-cont    . +)
                        (statement-block-intro . +)
                        (knr-argdecl-intro     . 5)
                        (substatement-open     . +)
                        (substatement-label    . +)
                        (label                 . +)
                        (statement-case-open   . +)
                        (statement-cont        . +)
                        (arglist-intro  . c-lineup-arglist-intro-after-paren)
                        (arglist-close  . c-lineup-arglist)
                        (access-label   . 0)
                        (inher-cont     . c-lineup-java-inher)
                        (func-decl-cont . c-lineup-java-throws)
                        (arglist-cont-nonempty . ++)
                        )))
  "Eclipse Java Programming Style")
(c-add-style "Eclipse" eclipse-java-style)
(customize-set-variable 'c-default-style
                        (quote ((java-mode . "eclipse") (awk-mode . "awk") (other . "gnu"))))
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t)
  (require 'dap-java)
  (evil-define-key 'normal lsp-mode-map (kbd "<f7>") 'dap-step-in)
  (evil-define-key 'normal lsp-mode-map (kbd "<f8>") 'dap-next)
  (evil-define-key 'normal lsp-mode-map (kbd "<f9>") 'dap-continue)
  )
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(use-package lsp-ui
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))
(require 'lsp-ui)
(setq lsp-ui-doc-use-childframe nil)
(setq lsp-ui-doc-use-webkit nil)
(evil-define-key   'normal lsp-mode-map
  (kbd "g d")        'lsp-find-definition
  (kbd "C-M-b")      'lsp-find-definition
  (kbd "g r")        'lsp-find-references
 )
(evil-define-key   'normal lsp-mode-map
  (kbd "g d")        'lsp-find-definition
  (kbd "C-M-b")      'lsp-find-definition
  (kbd "g r")        'lsp-find-references
  )

(evil-define-key     'normal java-mode-map
  (kbd "C-c c")      'dap-java-run-test-method
  (kbd "C-c C-c")    'dap-java-run-test-class
  )
(setq lsp-java-content-provider-preferred "fernflower")
(use-package lsp-treemacs)
(setq lsp-ui-sideline-update-mode 'point)
(setq lsp-ui-doc-enable nil)
(setq ai:lombok-jar (expand-file-name (concat user-emacs-directory "lombok.jar")))
(setq ai:java-format-settings-file
      (expand-file-name
       (concat user-emacs-directory "java.xml")
       )
      )
(setq lsp-java-vmargs
        (list "-noverify"
              "-Xmx2G"
              "-XX:+UseG1GC"
              "-XX:+UseStringDeduplication"
              (concat "-javaagent:" ai:lombok-jar)
              (concat "-Xbootclasspath/a:" ai:lombok-jar))
        lsp-file-watch-ignored
        '(".idea" ".ensime_cache" ".eunit" "node_modules" ".git" ".hg" ".fslckout" "_FOSSIL_"
          ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "build")

        lsp-java-import-order '["" "java" "javax" "#"]
        ;; Don't organize imports on save
        lsp-java-save-action-organize-imports nil

        ;; Formatter profile
        lsp-java-format-settings-url (concat "file://" ai:java-format-settings-file)
        lsp-enable-on-type-formatting t
        lsp-enable-indentation t)
(defun ai:lsp-thing-at-point ()
  "Return symbol at point."
  (interactive)
  (let ((contents (-some->>
                   (lsp--text-document-position-params)
                   (lsp--make-request "textDocument/hover")
                   (lsp--send-request)
                   (gethash "contents")
                  )
        ))
    (message (format "%s %s" (type-of contents) contents))
    (cond
     ((hash-table-p contents) (gethash "value" contents))
     ((vectorp contents)
      (let ((mt (aref contents 0)))
        (gethash "value" mt)
        ))
     (t nil))))
(provide 'lang-java-lsp)
;;; lang-java-lsp.el ends here
