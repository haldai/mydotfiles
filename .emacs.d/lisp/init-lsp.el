;;; Commentary:
;;
;; Language Server Protocol (LSP) configurations.
;;

;;; Code:

(use-package lsp-mode
  :straight t
  :diminish lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((prog-mode . lsp)
         (go-mode . lsp-deferred))
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point))
  :init
  (setq lsp-copilot-enabled nil)     ; disable copilot LSP
  (setq lsp-keymap-prefix "C-c l")   ; set prefix for lsp-command-keymap
  (setq lsp-auto-guess-root t)       ; Detect project root
  (setq lsp-prefer-flymake nil)      ; Use lsp-ui and flycheck
  (setq flymake-fringe-indicator-position 'right-fringe)
  :commands lsp
  :config
  ;; Configure LSP clients
  (setq lsp-eslint-server-command
        '("node"
          "/home/daiwz/.vscode/extensions/dbaeumer.vscode-eslint-2.2.2/server/out/eslintServer.js"
          "--stdio"))
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0.500)

  (add-to-list 'lsp-disabled-clients '((emmet-ls . t)
                                       (html-ls . t)))

  (use-package lsp-ui :commands lsp-ui-mode)
  (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
  (use-package lsp-treemacs :commands lsp-treemacs-errors-list))

;; if you are ivy user
(use-package lsp-ivy
  :straight t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-julia
  ;; Make sure to install "pkg> dev LanguageServer"
  :straight (lsp-julia :type git :host github :repo "non-Jedi/lsp-julia")
  :init (setq lsp-julia-default-environment "/home/daiwz/.julia/environments/v1.10/"))

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
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
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))

(use-package company-lsp
  :straight t
  :init (setq company-lsp-cache-candidates 'auto)
  :config
  ;; lua
  (add-to-list 'company-lsp-filter-candidates '(lsp-emmy-lua . t))
  (setq lsp-lua-hint-enable t))

(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
