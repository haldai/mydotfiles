;;; Commentary:
;;
;; Language Server Protocol (LSP) configurations.
;;

;;; Code:

(use-package lsp-mode
  :straight t
  :diminish lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((c-mode c++-mode objc-mode cuda-mode go-mode python-mode
          julia-mode ess-julia-mode js2-mode js2-jsx-mode web-mode
          lua-mode typescript-mode)
         . lsp-deferred)
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point))
  :init
  (setq lsp-copilot-enabled nil)     ; disable copilot LSP
  (setq lsp-keymap-prefix "C-c l")   ; set prefix for lsp-command-keymap
  (setq lsp-auto-guess-root t)       ; Detect project root
  :config
  ;; Configure LSP clients
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0.500)

  (dolist (client '(emmet-ls html-ls))
    (add-to-list 'lsp-disabled-clients client))

  )

;; if you are ivy user
(use-package lsp-ivy
  :straight t
  :commands lsp-ivy-workspace-symbol)

(defun my-lsp-julia-disable-document-color ()
  "Disable document-color requests unsupported by Julia LanguageServer."
  (setq-local lsp-enable-text-document-color nil))

(use-package lsp-julia
  ;; Make sure to install "pkg> dev LanguageServer"
  :straight (lsp-julia :type git :host github :repo "non-Jedi/lsp-julia")
  :init
  (dolist (hook '(julia-mode-hook ess-julia-mode-hook julia-ts-mode-hook))
    (add-hook hook #'my-lsp-julia-disable-document-color))
  (setq lsp-julia-package-dir "/home/daiwz/.julia/environments/v1.12/"
        lsp-julia-default-environment "/home/daiwz/.julia/environments/v1.12/"))

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background unspecified))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :init (setq lsp-ui-doc-enable t
              lsp-ui-doc-header t
              lsp-ui-doc-include-signature t
              lsp-ui-doc-position 'top
              lsp-ui-doc-use-webkit t
              lsp-ui-doc-border (face-foreground 'default)

              lsp-ui-sideline-enable nil
              lsp-ui-sideline-ignore-duplicate t)
  :config
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defun my-hide-lsp-ui-imenu-mode-line (&rest _)
    "Hide the mode line in the LSP UI Imenu buffer."
    (setq mode-line-format nil))
  (advice-add #'lsp-ui-imenu :after
              #'my-hide-lsp-ui-imenu-mode-line))


(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
