;;; Commentary:
;;
;; Python configurations.
;;

;;; Code:

;; Python Mode
;; Install:
;;   pip install pyflakes
;;   pip install autopep8
(use-package python
  :straight t
  :defines gud-pdb-command-name pdb-path
  :init
  (setq python-indent-guess-indent-offset t)
  (setq python-indent-guess-indent-offset-verbose nil)
  :config

  ;; language server
  (use-package lsp-pyright
    :straight t
    :hook (python-mode . (lambda () (require 'lsp-pyright))))

  ;; Live Coding in Python
  (use-package live-py-mode :straight t))

(provide 'init-python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
