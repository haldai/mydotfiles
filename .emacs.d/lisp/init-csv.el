;;; Commentary:
;;
;; CSV configuration.
;;

;;; Code:

;; CSV Mode
(use-package csv-mode
  :straight t
  :defer nil
  :mode (("\\.csv\\'" . csv-mode)))

(provide 'init-csv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-csv.el ends here
