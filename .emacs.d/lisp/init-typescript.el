;;; Commentary:
;;
;; Typescript configuration.
;;

;;; Code:

;; Typescript Mode
(use-package typescript-mode
  :straight t
  :mode (("\\.ts\\'" . typescript-ts-mode)))

(provide 'init-typescript)
