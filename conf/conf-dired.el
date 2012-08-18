;;; conf-dired --- dired configuration
;;; Commentary:
;;; Code:
(defun kill-all-dired-buffers ()
  "Kill all dired buffers."
  (interactive)
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (equal major-mode 'dired-mode)
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i dired buffer(s)." count))))
(eval-when-compile
  (require 'evil))
(require' dired-x)
(setq dired-omit-files "^.$\\|^#\\|~$\\|^.#")
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'dired-omit-mode)
(evil-define-key 'normal dired-mode-map
  (kbd "g h")   'dired-hide-details-mode
  (kbd "g o")   'dired-omit-mode
  (kbd "C-<return>") 'dired-subtree-insert
  (kbd "M-<return>")     'dired-insert-subdir
  (kbd ",")     'dired-insert-subdir
  (kbd "C-o")     'dired-up-directory
  (kbd ".")     'dired-up-directory
  )
(evil-define-key 'insert wdired-mode-map
  (kbd "<return>")     'wdired-finish-edit
  )
(evil-define-key 'normal wdired-mode-map
  (kbd "<return>")     'wdired-exit
  )
(evil-leader/set-key-for-mode 'dired-mode
  "SPC" 'dired-subtree-subdir
  "SPC" 'dired-insert-subdir
  )
(eval-after-load "dired"
  '(progn
    (define-key dired-mode-map "v" 'dired-x-find-file)
    (define-key dired-mode-map "V" 'dired-view-file)
    (define-key dired-mode-map "j" 'dired-next-line)
    (define-key dired-mode-map "J" 'dired-goto-file)
    (define-key dired-mode-map "k" 'dired-previous-line)
    (define-key dired-mode-map "K" 'dired-do-kill-lines)))
(use-package openwith)
(openwith-mode +1)
(setq-default openwith-associations nil)
(let (
      (pdf-files (rx "." (| "epub")))
      (image-files
       (rx "."
           (| "jpeg" "jpg" "gif" "png")))
      (chm-files (rx ".chm"))
      (epub-files
       (rx "."
           (| "mobi" "fb2")
           (? ".part")))
      (media-files
       (rx "."
           (| "mp4" "flac" "mkv"
              "flv" "webm" "avi"
              "ogg" "mp3" "wmv"
              "wav"
              )
           (? ".part"))))
  (add-to-list 'openwith-associations
               (list image-files "sxiv" '(file)))
  (add-to-list 'openwith-associations
               (list chm-files "xchm" '(file)))
  (add-to-list 'openwith-associations
               (list pdf-files "mupdf" '(file)))
  (add-to-list 'openwith-associations
               (list epub-files "FBReader" '(file)))
  (add-to-list 'openwith-associations
               (list media-files "mpv" '(file))))
(setq dired-dwim-target t)
(use-package dired-narrow
  :after dired
  :config
  (bind-key "C-c C-n" #'dired-narrow)
  (bind-key "C-c C-f" #'dired-narrow-fuzzy)
  (bind-key "C-x C-N" #'dired-narrow-regexp)
)
(use-package dired-subtree
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))
(define-key global-map "\C-x\C-d" 'dired-jump)
(define-key global-map "\C-x\C-j" 'dired-jump-other-window)
(require 'wdired)
(add-hook 'dired-load-hook
          (lambda ()
            ;; Set dired-x global variables here.  For example:
            (setq wdired-allow-to-change-permissions t)
            (setq dired-x-hands-off-my-keys nil)
            (load "dired-x")
            ))
(defun dired-sort* ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))
(defadvice dired-readin
  (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding marks."
  (dired-sort*))
(provide 'conf-dired)
;;; conf-dired ends here
