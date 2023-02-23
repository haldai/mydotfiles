;;; Commentary:
;;
;; Lua configuration.
;;

;;; Code:

;; Lua Mode
(use-package lua-mode
  :straight t
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode)
  :hook (lua-modeq . (lambda () (aggressive-indent-mode -1)))
  :init
  (setq lua-indent-level 2))

(use-package pico8-mode
  :straight (pico8-mode :type git :host github :repo "Kaali/pico8-mode")
  :mode (("\\.p8\\'" . lua-mode)
         ("\\.p8.png\\'" . lua-mode))
  :init
  (setq lua-indent-level 2))

(provide 'init-lua)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lua.el ends here
