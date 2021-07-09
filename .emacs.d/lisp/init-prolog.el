;;; Commentary:
;;
;; Prolog configuration.
;;

;;; Code:

;; Prolog Mode
(use-package prolog
  :load-path "~/.emacs.d/lisp/prolog.el"
  :mode (("\\.pl\\'" . prolog-mode)
         ("\\.m\\'" . mercury-mode))
  :bind (("C-c %" . prolog-insert-comment-block)
         ("C-c C-c l" . prolog-insert-library))
  :hook ((prolog-mode . display-line-numbers-mode)
         (prolog-mode . hl-todo-mode))
  :config
  ;; no auto-indentation for comments
  (setq prolog-align-comments-flag nil)
  (setq prolog-indent-mline-comments-flag nil)
  (setq prolog-system 'swi
        prolog-program-switches '((swi ("-G128M" "-T128M" "-L128M" "-O"))
                                  (t nil))
        prolog-electric-if-then-else-flag t)

  (defun prolog-insert-comment-block ()
    "Insert a PceEmacs-style comment block like /* - - ... - - */ "
    (interactive)
    (let ((dashes "-"))
      (dotimes (_ 36) (setq dashes (concat "- " dashes)))
      (insert (format "/* %s\n\n%s */" dashes dashes))
      (forward-line -1)
      (indent-for-tab-command))
    )

  (defun prolog-insert-library ()
    "Insert a module"
    (interactive)
    (insert ":- use_module(library()).")
    (forward-char -3)))

(use-package etrace :load-path "~/.emacs.d/lisp/")

(use-package ediprolog
  :straight t
  :init
  (defun insert-prolog-query-mark ()
    "Insert an epiprolog query mark in prolog mode."
    (interactive)
    (insert "%%?- "))
  :bind (("<f10>" . ediprolog-dwim)
         ("C-c <f10>" . ediprolog-consult)
         ("C-c q" . insert-prolog-query-mark))
  :config
  (setq ediprolog-prefix "%%@"))

;; ob-prolog
(use-package ob-prolog :straight t)

;; Potassco Answer Set Program
(use-package pasp-mode
  :straight (pasp-pmode :type git :host github :repo "llaisdy/pasp-mode")
  :mode ("\\.lp\\'" . pasp-mode))

(provide 'init-prolog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prolog.el ends here
