;;; Commentary:
;;
;; Yasnippet configurations.
;;

;;; Code:

(use-package yasnippet
  :straight t
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config (straight-use-package 'yasnippet-snippets))

(provide 'init-yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-yasnippet.el ends here
