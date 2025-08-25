;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur
;; (server-start)

;; debug
(setq debug-on-error nil)
(setq warning-minimum-level :emergency)
(defvar native-comp-deferred-compilation-deny-list nil)

(pixel-scroll-precision-mode 1)
(pixel-scroll-mode 1)

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
      (bootstrap-version 6))
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
(require 'init-utils)

;;keep cursor at same position when scrolling
(setq scroll-preserve-screen-position 1)

;;scroll window up/down by three lines
(global-set-key (kbd "M-]") (kbd "C-u 3 C-v"))
(global-set-key (kbd "M-[") (kbd "C-u 3 M-v"))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
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
(require 'init-frame-hooks)
(require 'init-themes)
;; (require 'init-emms)

;; Shell
(require 'init-eshell)
(require 'init-shell)

;; Text
(require 'init-org)
(require 'init-tex)
(require 'init-markdown)
(require 'init-mail)
(require 'init-csv)

;; Programming
(require 'init-git)
(require 'init-flycheck)
(require 'init-lsp)
(require 'init-projectile)

(require 'init-emacs-lisp)
(require 'init-c)
(require 'init-rust)
(require 'init-prolog)
(require 'init-julia)
(require 'init-go)
(require 'init-python)
(require 'init-web)
(require 'init-lua)
(require 'init-r)
(require 'init-scheme)
(require 'init-lean4)
(require 'init-prog)
(require 'init-ai)

;; Load custom files at last
(when (file-exists-p custom-file)
  (load custom-file))
(require 'init-keybind)

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init.el ends here
