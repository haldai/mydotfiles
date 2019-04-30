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
  :init (add-hook 'python-mode-hook
                  (lambda ()
                    (setq indent-tabs-mode t)
                    (setq tab-width 4)
                    (setq python-indent-offset 4)))
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

  (provide 'init-python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
