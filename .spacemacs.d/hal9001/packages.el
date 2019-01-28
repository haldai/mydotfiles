;;; packages.el --- hal9001 layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author:  <daiwz@HAL9001>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `hal9001-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `hal9001/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `hal9001/pre-init-PACKAGE' and/or
;;   `hal9001/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst hal9001-packages
  '(
    multiple-cursors
    pdf-tools
    markdown-mode
    markdown-preview-mode
    (prolog-mode :location built-in)
    ediprolog
    ob-prolog
    pinentry
    )
  "The list of Lisp packages required by the hal9001 layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun hal9001/init-multiple-cursors ()
  (use-package multiple-cursors
    :defer t
    :init
    (progn
      (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
      (global-set-key (kbd "C->") 'mc/mark-next-like-this)
      (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
      (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))
    )
  )

(defun hal9001/init-pdf-tools ()
  (use-package pdf-tools
    :defer t
    :init
    (pdf-tools-install)
    :config
    (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
    )
  )

(defun hal9001/init-prolog-mode ()
  (autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
  (autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
  (autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
  (setq prolog-system 'swi)
  (setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                  ("\\.m$" . mercury-mode))
                                auto-mode-alist))
  (add-hook 'prolog-mode-hook '(lambda ()
                                 (local-set-key (kbd "RET") 'newline-and-indent)
                                 (auto-complete-mode t)
                                 (local-set-key [f10] 'ediprolog-dwim)
                                 (local-set-key (kbd "C-c <f10>")
                                                'ediprolog-consult)
                                 (local-set-key (kbd "C-c q") 'insert-prolog-query-mark)
                                 ))
  )

(defun hal9001/init-ediprolog ()
  (use-package ediprolog
	  :defer t)
  )

(defun hal9001/init-ob-prolog ()
  (use-package ob-prolog
	  :defer t)
  )

(defun hal9001/init-pinentry ()
  (use-package pinentry
	       :defer t)
  )

(defun hal9001/init-markdown-mode ()
  (use-package markdown-mode
    :defer t
    :init
    (setq markdown-command "pandoc -f markdown -t html")
    :config
    (progn
      (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
      (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
      (autoload 'gfm-mode "markdown-mode"
        "Major mode for editing GitHub Flavored Markdown files" t)
      (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
      (setq markdown-enable-math t))
    )
  )

(defun hal9001/init-markdown-preview-mode ()
  (use-package markdown-preview-mode
	  :defer t)
  )

;;; packages.el ends here
