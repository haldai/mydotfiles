;;; Commentary:
;;
;;Julia configuration.
;;

;;; Code:

;; Julia Mode
(use-package julia-mode
  :straight (julia-emacs :type git :host github :repo "JuliaEditorSupport/julia-emacs")
  :mode ("\\.jl$" . julia-mode))

(use-package jupyter :straight t)

(setq org-babel-default-header-args:jupyter-julia '((:async . "yes")
                                                    (:session . "jl")
                                                    (:kernel . "julia-1.1")))

(provide 'init-julia)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-julia.el ends here
