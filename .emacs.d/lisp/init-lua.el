;;; Commentary:
;;
;; Lua configuration.
;;

;;; Code:

;; Lua Mode
(use-package lua-mode
  :straight t
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode))

(provide 'init-lua)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lua.el ends here
