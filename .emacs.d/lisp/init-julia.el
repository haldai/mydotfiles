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
  :hook ((ess-julia-mode . lsp-mode)
         (julia-mode . lsp-mode))
  :config
  ;; lsp
  (use-package lsp-julia
    ;; Make sure to install "pkg> dev LanguageServer"
    :straight (lsp-julia :type git :host github :repo "non-Jedi/lsp-julia")
    :init (setq lsp-julia-default-environment "/home/daiwz/.julia/environments/v1.7/")))

(provide 'init-julia)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-julia.el ends here
