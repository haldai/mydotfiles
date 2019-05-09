;;; Commentary:
;;
;;Julia configuration.
;;

;;; Code:

;; Julia Mode
(use-package julia-mode
  :straight (julia-emacs :type git :host github :repo "JuliaEditorSupport/julia-emacs")
  :mode ("\\.jl$" . julia-mode))


;; jupyter mode
(use-package jupyter
  :straight t
  :bind (("C-c C-x r" . jupyter-repl-restart-kernel)
         ("C-c C-x h" . jupyter-org-restart-and-execute-to-point)))


(provide 'init-julia)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-julia.el ends here
