;;; Commentary:
;;
;; Prolog configuration.
;;

;;; Code:

;; Prolog Mode
(use-package prolog-mode
  :mode (("\\.pl$" . prolog-mode)
         ("\\.m$" . mercury-mode)))

(use-package ediprolog
  :straight t
  :init
  (defun insert-prolog-query-mark ()
    "Insert an epiprolog query mark in prolog mode."
    (interactive)
    (insert "%?- "))
  :bind (("<f10>" . ediprolog-dwim)
         ("C-c <f10>" . ediprolog-consult)
         ("C-c q" . insert-prolog-query-mark)))

;; ob-prolog
(use-package ob-prolog :straight t)

(provide 'init-prolog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prolog.el ends here
