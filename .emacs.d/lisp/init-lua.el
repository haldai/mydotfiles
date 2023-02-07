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

(use-package pico8-mode
  :straight (pico8-mode :type git :host github :repo "Kaali/pico8-mode"))

(provide 'init-lua)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lua.el ends here
