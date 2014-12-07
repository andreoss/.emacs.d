;;; init --- init ; -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; Code:
(defvar exwm-enable? nil)
;; Enable readable backtraces when error occurs
(setq debug-on-error t)
(set-language-environment "UTF-8")
;; Disable GC for about 1s speed up
;; See https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(setq gc-cons-threshold  most-positive-fixnum
      gc-cons-percentage 0.6)
;; Enable GC back after init
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 80 (expt 2 20)))
            (setq gc-cons-percentage 0.1)
            (garbage-collect)))
;; Start server unless it is already running
(require 'server)
(add-hook 'after-init-hook
          (lambda ()
            (if (not (server-running-p))
                (progn
                  (message "Starting server")
                  (server-start)))))
;;;
;; Load extra files from ~/.emacs.d
(defconst dotfiles-directory-name
  (file-name-directory (or (buffer-file-name) load-file-name))
  "Location of main Emacs configuration files.")
(require 'viper-fallback (concat dotfiles-directory-name "viper-fallback.el"))
(require 'macros         (concat dotfiles-directory-name "macros.el"))
(require 'helpers        (concat dotfiles-directory-name "helpers.el"))
;;;
;; Load system-wide packages
(ignore-errors
   (require 'guix)
   (guix-prettify-global-mode +1))
(ignore-errors
  (require 'pdf-tools)
  (pdf-tools-install))
;; Setup load-path
(defvar vendor-dir (</> dotfiles-directory-name "vendor") "Non-ELPA packages.")
(defvar conf-dir   (</> dotfiles-directory-name "conf")
  "Configuration of separate modules.")
(defvar lang-dir   (</> dotfiles-directory-name "lang")   "Per language files.")
(add-to-loadpath conf-dir)
(add-to-loadpath lang-dir)
(add-to-loadpath vendor-dir)
;;;
;; Enable packages
(require 'conf-straight)
(require 'conf-evil)
(require 'conf-editor)
(require 'conf-system)
(require 'conf-appearence)
(require 'conf-lsp)
(require 'conf-dired)
(require 'conf-misc)
(require 'conf-mouse)
(require 'conf-org)
(require 'conf-eshell)
(require 'conf-wm)
(require 'lang-lisp)
(require 'lang-elisp)
(require 'lang-common-lisp)
(require 'lang-c)
(require 'lang-java-lsp)
(require 'lang-scala-lsp)
(require 'lang-clojure)
(require 'lang-javascript)
(require 'lang-haskell)
(require 'lang-go)
(require 'lang-perl)
(require 'lang-python)
(require 'lang-scheme)
(require 'lang-sh)
(require 'lang-rust)
(require 'conf-vc)
(provide 'init)
;;; init.el ends here
