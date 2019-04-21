;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur
(setq debug-on-error t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold 80000000)

;; Prevent Emacs package auto startup
(setq package-enable-at-startup nil)

;; cleaner ui
(unless (eq window-system 'ns)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;;----------------------------------------------------------------------------
;; Bootstrap straight.el
;;----------------------------------------------------------------------------
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

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(straight-use-package 'benchmark-init)
(require 'benchmark-init)
;; To disable collection of benchmark data after init is done.
(add-hook 'after-init-hook 'benchmark-init/deactivate)
(require 'init-utils)

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
(straight-use-package 'scratch)

(require 'init-themes)
(require 'init-ivy)
(require 'init-edit)
(require 'init-highlight)
(require 'init-company)
(require 'init-yasnippet)
(require 'init-ibuffer)
(require 'init-kill-ring)
(require 'init-dired)
(require 'init-treemacs)
(require 'init-dashboard)

(require 'init-eshell)
(require 'init-shell)

(require 'init-markdown)
(require 'init-org)

;; Programming
(require 'init-prog)
(require 'init-projectile)
