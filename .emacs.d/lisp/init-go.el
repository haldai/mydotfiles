;;; Commentary:
;;
;;Go configuration.
;;

;;; Code:

;; Go Mode
(use-package go-mode
  :straight t
  :mode ("\\.go\\'" . go-mode)
  :hook (go-mode . lsp-mode))

(provide 'init-go)
