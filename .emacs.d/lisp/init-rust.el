;;; Commentary:
;;
;; Rust configuration.
;;

;;; Code:

(use-package rustic
  :straight t
  :ensure t
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  (custom-set-faces
   '(rustic-compilation-column ((t (:inherit compilation-column-number))))
   '(rustic-compilation-line ((t (:foreground "LimeGreen")))))

  (setq rustic-lsp-server 'rls)

  (setq lsp-rust-analyzer-server-command '("/usr/bin/rust-analyzer"))

  (with-eval-after-load "lsp-rust"
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection
                       (lambda ()
                         `(,(or (executable-find
                                 (cl-first lsp-rust-analyzer-server-command))
                                (lsp-package-path 'rust-analyzer)
                                "rust-analyzer")
                           ,@(cl-rest lsp-rust-analyzer-server-args))))
      :remote? t
      :major-modes '(rust-mode rustic-mode)
      :initialization-options 'lsp-rust-analyzer--make-init-options
      :notification-handlers (ht<-alist lsp-rust-notification-handlers)
      :action-handlers (ht ("rust-analyzer.runSingle" #'lsp-rust--analyzer-run-single))
      :library-folders-fn (lambda (_workspace) lsp-rust-library-directories)
      :after-open-fn (lambda ()
                       (when lsp-rust-analyzer-server-display-inlay-hints
                         (lsp-rust-analyzer-inlay-hints-mode)))
      :ignore-messages nil
      :server-id 'rust-analyzer-remote)))

  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm
  (setq-local buffer-save-without-query t))

(use-package cargo
  :straight t
  :init (add-hook 'rust-mode-hook 'cargo-minor-mode))

(provide 'init-rust)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-c.el ends here
