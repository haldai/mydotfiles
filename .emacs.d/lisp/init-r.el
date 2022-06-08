;;; Commentary:
;;
;; R configuration.
;;

;;; Code:

;; R language mode
(use-package ess
  :straight t
  :mode ("\\.r\\'" . ess-r-mode)
  :interpreter ("R" . ess-r-mode))

(provide 'init-r)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-r.el ends here
