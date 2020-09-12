;;; conf-system -- Integration with OS
;;; Commentary:
;;; Code:
(use-package guix)
(use-package pdf-tools
    :ensure t
    :config
    (pdf-tools-install)
    (setq-default pdf-view-display-size 'fit-page)
    (bind-keys :map pdf-view-mode-map
        ("<s-spc>" .  pdf-view-scroll-down-or-next-page)
        ("g"  . pdf-view-first-page)
        ("G"  . pdf-view-last-page)
        ("l"  . image-forward-hscroll)
        ("h"  . image-backward-hscroll)
        ("j"  . pdf-view-next-page)
        ("k"  . pdf-view-previous-page)
        ("e"  . pdf-view-goto-page)
        ("u"  . pdf-view-revert-buffer)
        ("al" . pdf-annot-list-annotations)
        ("ad" . pdf-annot-delete)
        ("aa" . pdf-annot-attachment-dired)
        ("am" . pdf-annot-add-markup-annotation)
        ("at" . pdf-annot-add-text-annotation)
        ("y"  . pdf-view-kill-ring-save)
        ("i"  . pdf-misc-display-metadata)
        ("s"  . pdf-occur)
        ("b"  . pdf-view-set-slice-from-bounding-box)
        ("r"  . pdf-view-reset-slice))
     (use-package org-pdfview :ensure t))
(add-hook 'pdf-view-mode-hook (lambda () (blink-cursor-mode -1)))
(use-package restart-emacs)
(use-package notmuch)
(defun notmuch-inbox ()
  (notmuch-tree "is:inbox")
  )
(use-package org)
(use-package org-caldav)
(use-package ytdl)
(use-package telega)
(telega-notifications-mode 1)
(evil-leader/set-key "m i"
  '(lambda () (interactive) (notmuch-tree "is:inbox")))
(evil-leader/set-key "m m p"
  '(lambda () (interactive) (notmuch-tree "is:inbox and is:private")))
(evil-leader/set-key "m m g"
  '(lambda () (interactive) (notmuch-tree "is:inbox and is:github")))
(evil-leader/set-key "m s"
  '(lambda () (interactive) (notmuch-tree)))

(update-env-var "PATH" "~/.emacs.d/bin")
(provide 'conf-system)
;;; conf-system.el ends here
