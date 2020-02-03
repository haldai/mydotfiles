;;; Commentary:
;;
;;Julia configuration.
;;

;;; Code:

;; Julia Mode
(use-package julia-mode
  :straight (julia-emacs :type git :host github :repo "JuliaEditorSupport/julia-emacs")
  :requires lsp-julia
  :mode "\\.jl\\'"
  :interpreter "julia"
  ;; lsp-julia is incompatible with julia-v1.3
  ;; :hook ((ess-julia-mode . lsp-mode)
  ;; (julia-mode . lsp-mode))
  )


;; jupyter mode
(use-package jupyter
  :straight t
  :bind (("C-c C-x r" . jupyter-repl-restart-kernel)
         ("C-c C-x h" . jupyter-org-restart-and-execute-to-point)))


(provide 'init-julia)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-julia.el ends here
