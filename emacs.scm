(use-modules
 (guix)
 (guix build-system gnu)
 (gnu packages emacs)
 (gnu packages emacs-xyz)
 (gnu packages fonts)
 (guix licenses)
 )
(define-public emacs-asdf
  (let ((configure-opts '(list "--without-toolkit-scroll-bars" "--with-x-toolkit=no")))
    (package
     (inherit emacs)
     (name "emacs-asdf")
     (synopsis "The extensible, customizable, self-documenting text
editor (with extras)" )
     (propagated-inputs
      (list
        ;; Evil
        `("emacs-evil"               ,emacs-evil)
        `("emacs-evil-collection"    ,emacs-evil-collection)
        `("emacs-evil-commentary"    ,emacs-evil-commentary)
        `("emacs-evil-org"           ,emacs-evil-org)
        `("emacs-evil-ediff"         ,emacs-evil-ediff)
        ;; Magit
        `("emacs-magit"              ,emacs-magit)
        `("emacs-magit-org-todos-el" ,emacs-magit-org-todos-el)
        `("emacs-magit-popup"        ,emacs-magit-popup)
        `("emacs-evil-magit"         ,emacs-evil-magit)
        `("emacs-git-gutter"        ,emacs-git-gutter)
        ;; Lisp
        `("emacs-cider" ,emacs-cider)
        `("emacs-eros"               ,emacs-eros)
        `("emacs-geiser"    ,emacs-geiser)
        `("emacs-paredit"   ,emacs-paredit)
        `("emacs-slime"                ,emacs-slime)
        `("emacs-slime-repl-ansi-color"      ,emacs-slime-repl-ansi-color)
        ;; Emacs etc.
        `("emacs-ag"   ,emacs-ag)
        `("emacs-bash-completion" ,emacs-bash-completion)
        `("emacs-better-defaults" ,emacs-better-defaults)
        `("emacs-company"            ,emacs-company)
        `("emacs-dashboard" ,emacs-dashboard)
        `("emacs-diminish"               ,emacs-diminish)
        `("emacs-dumb-jump"          ,emacs-dumb-jump)
        `("emacs-flx"                ,emacs-flx)
        `("emacs-hl-todo"      ,emacs-hl-todo)
        `("emacs-ivy"                ,emacs-ivy)
        `("emacs-pdf-tools"          ,emacs-pdf-tools)
        `("emacs-projectile"         ,emacs-projectile)
        `("emacs-restart-emacs"      ,emacs-restart-emacs)
        `("emacs-undo-tree"      ,emacs-undo-tree)
        `("emacs-which-key"               ,emacs-which-key)
        ;; LSP
        `("emacs-company-lsp"        ,emacs-company-lsp)
        `("emacs-lsp-ivy"        ,emacs-lsp-ivy)
        `("emacs-lsp-mode"           ,emacs-lsp-mode)
        `("emacs-lsp-ui"             ,emacs-lsp-ui)
        ;; Guix & Nix & etc
        ;; `("emacs-guix"      ,emacs-guix)
        `("emacs-nix-mode"      ,emacs-nix-mode)
        `("emacs-docker"      ,emacs-docker)
        ;; Modes
        `("emacs-markdown-mode" ,emacs-markdown-mode)
        `("emacs-markdown-preview-mode" ,emacs-markdown-preview-mode)
        `("emacs-adoc-mode"     ,emacs-adoc-mode)
        ;; Fonts
        `("font-adobe-source-code-pro" ,font-adobe-source-code-pro)
        `("font-adobe-source-sans-pro" ,font-adobe-source-sans-pro)
        `("font-dejavu"                ,font-dejavu)
        `("font-iosevka"               ,font-iosevka)
        )
      )
     (arguments
      (substitute-keyword-arguments
       (package-arguments emacs)
       ((#:configure-flags flags) configure-opts)
       )
      )
     )
    )
  )
emacs-asdf
