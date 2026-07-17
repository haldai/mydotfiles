;;; Commentary:
;;
;; Go configuration.
;; install 'go-tools' before using it

;;; Code:

;; Go Mode
(use-package go-mode
  :straight t
  :mode ("\\.go\\'" . go-mode))

(provide 'init-go)
