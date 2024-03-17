;;; Commentary:
;;
;; Lean4 theorem proving
;;

;;; Code:

(use-package lean4-mode
  :straight (lean4-mode
         :type git
         :host github
         :repo "leanprover/lean4-mode"
         :files ("*.el" "data"))
  ;; to defer loading the package until required
  :commands (lean4-mode))

(provide 'init-lean4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lean4.el ends here
