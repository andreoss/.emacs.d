;;; conf-mouse --- ...
;;; Commentary:

;;; Code:
(defun find-file-at-mouse (event &optional promote-to-region)
  (interactive "e\np")
  (save-excursion
    (mouse-set-point event)
    (let ((f (thing-at-point 'filename)))
      (if (file-exists-p f)
          (progn
            (message "File found <%s>" f) (find-file-other-window f))
        (message "File not found <%s>" f)))))

(defun find-symbol-at-mouse (event &optional promote-to-region)
  (interactive "e\np")
  (save-excursion
    (mouse-set-point event)
    (let ((f (thing-at-point 'symbol)))
      (xref-find-definitions f))))

(local-set-key (kbd "<down-mouse-3>") 'my-select-region)

(add-hook 'help-mode-hook
          (lambda () (local-set-key (kbd "<mouse-3>") 'find-file-at-mouse)))
(add-hook 'shell-mode-hook
          (lambda () (local-set-key (kbd "<mouse-3>") 'find-file-at-mouse)))
(add-hook 'eshell-mode-hook
          (lambda () (local-set-key (kbd "<mouse-3>") 'find-file-at-mouse)))
(add-hook 'prog-mode-hook
          (lambda () (local-set-key (kbd "<mouse-3>") 'find-symbol-at-mouse)))

(defun header-line-active-p ()
  "Is header line active."
  (not (null header-line-format)))

(defun move-mouse-to-point ()
  "Move the mouse pointer to point in the current window."
  (let* ((coords (posn-col-row (posn-at-point)))
         (window-coords (window-inside-edges))
         (x (+ (car coords) (car window-coords) 0)) ;the fringe is 0
         (y (+ (cdr coords) (cadr window-coords)
               (if (header-line-active-p)
                   -1
                 0))))
    (set-mouse-position (selected-frame) x y)))

(provide 'conf-mouse)
;;; conf-mouse.el ends here
