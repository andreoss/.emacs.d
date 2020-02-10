;;; Code:
(require 'eshell)
(require 'shell)
(require 'ansi-color)
(setq-default eshell-where-to-jump 'begin)
(setq-default eshell-review-quick-commands nil)
(setq-default eshell-smart-space-goes-to-end t)
(setq-default
 comint-input-sender-no-newline t
 comint-prompt-read-only t
 eshell-where-to-jump 'begin
 eshell-review-quick-commands nil
 )
(require 'em-smart)
(defun eshell-here ()
  "Go to eshell and set current directory to the buffer's directory."
  (interactive)
  (let ((dir (file-name-directory (or (buffer-file-name)
                                      default-directory))))
    (eshell)
    (eshell/pushd ".")
    (cd dir)
    (goto-char (point-max))
    (eshell-kill-input)
    (eshell-send-input)))
(setq-default eshell-banner-message "")
(use-package better-shell
  :init
  (global-set-key (kbd "C-c s") 'better-shell-shell)
  (global-set-key (kbd "C-c C-s") 'better-shell-for-projectile-root)
  )
(eval-after-load 'em-ls
  '(progn
     (defun ted-eshell-ls-find-file-at-point (point)
       "RET on Eshell's `ls' output to open files."
       (interactive "d")
       (find-file (buffer-substring-no-properties
                   (previous-single-property-change point 'help-echo)
                   (next-single-property-change point 'help-echo))))

     (defun pat-eshell-ls-find-file-at-mouse-click (event)
       "Middle click on Eshell's `ls' output to open files.
 From Patrick Anderson via the wiki."
       (interactive "e")
       (ted-eshell-ls-find-file-at-point (posn-point (event-end event))))

     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "<return>") 'ted-eshell-ls-find-file-at-point)
       (define-key map (kbd "<mouse-1>") 'pat-eshell-ls-find-file-at-mouse-click)
       (defvar ted-eshell-ls-keymap map))

     (defadvice eshell-ls-decorated-name (after ted-electrify-ls activate)
       "Eshell's `ls' now lets you click or RET on file names to open them."
       (add-text-properties 0 (length ad-return-value)
                            (list 'help-echo  "RET, mouse-1: visit this file"
                                  'mouse-face 'highlight
                                  'keymap ted-eshell-ls-keymap)
                            ad-return-value)
       ad-return-value)))
(setq-default shell-font-lock-keywords
 '(
   ("[ \t]\\([+-][^ \t\n]+\\)" . font-lock-comment-face)
   ("^[a-zA-Z]+:"              . font-lock-doc-face)
   ("^\\[[^\\]]+\\]:"          . font-lock-doc-face)
   ("\\[INFO\\]"               . font-lock-doc-face)
   ("\\[WARNING\\]"            . font-lock-warning-face)
   ("\\[ERROR\\]"              . compilation-error-face)
   ("^\\[[1-9][0-9]*\\]"       . font-lock-string-face)))
(custom-set-variables
 '(ansi-color-names-vector
   [
    "black"   "red4"  "green4"
    "yellow4" "blue4" "magenta4"
    "cyan4"   "gray40"
    ]
   )
 )
(add-hook 'shell-mode-hook  'ansi-color-for-comint-mode-on)
(add-hook 'eshell-mode-hook 'ansi-color-for-comint-mode-on)
(use-package bash-completion)
(bash-completion-setup)
(require 'em-tramp)
(setq eshell-prefer-lisp-functions t)
(setq eshell-prefer-lisp-variables t)
(add-to-list 'eshell-modules-list 'eshell-tramp)
(setq password-cache t)
(setq password-cache-expiry 3600)
(provide 'conf-eshell)
;;; conf-eshell.el ends here
