;; https://www.goldsborough.me/emacs,/java/2016/02/24/22-54-16-setting_up_emacs_for_java_development/


(use-package eclim)

(require 'eclim)
(add-hook 'java-mode-hook 'eclim-mode)

(require 'eclimd)
(require 'gradle-mode)
(add-hook 'java-mode-hook '(lambda() (gradle-mode 1)))

(setq-default eclim-executable  "$HOME/.p2/pool/plugins/org.eclim_2.8.0/bin/eclim")
(setq-default eclimd-executable "/home/a/eclipse/java-2018-12/eclipse/eclimd")
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(define-key eclim-mode-map (kbd "C-c C-c") 'eclim-problems-correct)

(global-eclim-mode +1)
