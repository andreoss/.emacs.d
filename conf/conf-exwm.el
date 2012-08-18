(setq-default package-archives
              (loop for m in
                    '("gnu" "org" "melpa" "stable-melpa")
                    collect
                    (cons m (concat "~/.elpa/" m)))
              package-archive-priorities (pairs "stable-melpa" 1))

(require 'exwm)
(require 'exwm-config)
(exwm-config-ido)

(setq exwm-workspace-number 4)

(hook! exwm-mode-hook
     (local-set-key (key C-w) 'evil-window-map))

(define-key 'evil-window-map (kbd "b") 'exwm-workspace-switch-to-buffer)

(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))

(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

(exwm-input-set-key (kbd "s-r") #'exwm-reset)
(exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
(exwm-input-set-key (kbd "s-m") #'exwm-workspace-move-window)

(setq-default exwm-input-prefix-keys
  '(?\C-c ?\C-x ?\C-u ?\C-h ?\M-x ?\M-` ?\M-& ?\M-: ?\C-w))

(loop for i from 1 to 10 do
      (exwm-input-set-key
       (kbd (format "s-%d" (% i 10)))
       `(lambda () (interactive)
          (exwm-workspace-switch-create (- ,i 1)))))

(loop for i from 1 to 10 do
      (exwm-input-set-key
       (kbd (format "s-s %d" (% i 10)))
       `(lambda () (interactive)
          (exwm-workspace-move-window (- ,i 1) ))))

(exwm-input-set-key (kbd "s-&")
                    (lambda (command)
                      (interactive (list (read-shell-command "$ ")))
                      (start-process-shell-command command nil command)))

(exwm-input-set-key (key s-l)   (lambda () (interactive) (start-process "" nil "slock")))
(exwm-input-set-key (key s-<tab>) 'exwm-workspace-swap)

(define-key exwm-mode-map (key C-q) #'exwm-input-send-next-key)
(define-key exwm-mode-map (key C-w) 'evil-window-map)

(defun run-or-focus (cmd)
  (interactive "%b")
  (exwm-workspace-switch-to-buffer cmd))

(exwm-enable)

;;(exwm-input-set-key
;; (key <XF86AudioLowerVolume>)
;; (--> start-process-shell-command "" nil "amixer sset Master 1%-"))

;;(exwm-input-set-key
;; (key <XF86AudioRaiseVolume>)
;; (--> start-process-shell-command "" nil "amixer sset Master 5%+"))

(provide 'conf-exwm)
