;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; Code:
(setq package-enable-at-startup nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq use-package-ensure-function 'straight-use-package)
(setq straight-use-package-by-default t)
(setq use-package-always-defer nil)
(setq use-package-always-ensure t)
(setq use-package-always-demand nil)
(setq straight-vc-git-default-clone-depth 1)
(setq package-enable-at-startup nil)

;;; LSP Performance
(setq read-process-output-max (expt 2 20))
(setenv "LSP_USE_PLISTS" "true")
