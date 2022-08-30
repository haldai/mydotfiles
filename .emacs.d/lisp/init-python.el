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
  :ensure nil
  :defines gud-pdb-command-name pdb-path
  :init
  (setq python-indent-guess-indent-offset t)
  (setq python-indent-guess-indent-offset-verbose nil)
  :config

  ;; language server
  (use-package lsp-pyright
    :ensure t
    :straight t
    :hook (python-mode . (lambda ()
                           (require 'lsp-pyright)
                           (lsp-deferred))))  ; or lsp-deferred

  ;; Live Coding in Python
  (use-package live-py-mode :straight t)
  (use-package py-autopep8
    :straight t
    :hook (python-mode . py-autopep8-enable-on-save)))

(provide 'init-python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
