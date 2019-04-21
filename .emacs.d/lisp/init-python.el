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
  :config
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)

  (add-hook 'inferior-python-mode-hook
            (lambda ()
              ;; (bind-key "C-c C-z" #'kill-buffer-and-window inferior-python-mode-map)
              (process-query-on-exit-flag (get-process "Python"))))

  ;; Live Coding in Python
  (use-package live-py-mode :straight t)

  ;; Format using YAPF
  ;; Install: pip install yapf
  (use-package yapfify
    :straight t
    :diminish yapf-mode
    :hook (python-mode . yapf-mode)))

;; Emacs IPython Notebook
(use-package ein
  :straight t
  :diminish ein:notebook-mode
  :defines ein:completion-backend
  :init (setq ein:completion-backend 'ein:use-company-backend))

(provide 'init-python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
