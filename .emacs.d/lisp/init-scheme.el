;;; Commentary:
;;
;; Scheme configurations.
;;

;;; Code:

;; Scheme mode
(use-package geiser
  :straight t
  :init
  (add-hook 'scheme-mode-hook 'company-mode))

(provide 'init-scheme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-scheme.el ends here
