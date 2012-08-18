(use autodisass-java-bytecode)
(use google-c-style)

(use gradle-mode)
(add-hook 'java-mode-hook '(lambda() (gradle-mode 1)))

(setq-default gradle-executable-path "/usr/bin/gradle")
(require 'company)
(global-company-mode t)


(require 'eclim)
(add-hook 'java-mode-hook 'eclim-mode)
(require 'eclimd)
(setq-default eclim-executable "/home/a/eclipse/eclim")
(setq-default eclimd-executable "/home/a/eclipse/eclimd")
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(define-key eclim-mode-map (kbd "C-c C-c") 'eclim-problems-correct)
(provide 'lang-java)
