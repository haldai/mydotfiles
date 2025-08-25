;;; Commentary:
;;
;; Run AI in Emacs
;;

;;; Code:

;; ollama
(use-package ollama-buddy
  :straight t
  :bind
  ("C-c o" . ollama-buddy-menu)
  ("C-c O" . ollama-buddy-transient-menu-wrapper))

(provide 'init-ai)
