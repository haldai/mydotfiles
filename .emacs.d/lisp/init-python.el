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
  :hook (python-mode . lsp-mode)
  :init
  (setq python-indent-guess-indent-offset t)
  (setq python-indent-guess-indent-offset-verbose nil)
  :config

  ;; Live Coding in Python
  (use-package live-py-mode :straight t))

(provide 'init-python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
