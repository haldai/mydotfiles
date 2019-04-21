;;; Commentary:
;;
;;Julia configuration.
;;

;;; Code:

;; Julia Mode
(use-package julia-mode
  :straight (julia-emacs :type git :host github :repo "JuliaEditorSupport/julia-emacs")
  :mode ("\\.jl$" . julia-mode))

(provide 'init-julia)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-julia.el ends here
