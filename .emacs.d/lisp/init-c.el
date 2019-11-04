;;; Commentary:
;;
;; C/C++ configuration.
;;

;;; Code:

;; C/C++ Mode
(use-package cc-mode
  :straight t
  :bind (:map c-mode-base-map
              ("C-c c" . compile))
  :hook ((c-mode-common . (lambda ()
                            (c-set-style "bsd")
                            (setq tab-width 2)
                            (setq c-basic-offset 2)))
         (c-mode-common . display-line-numbers-mode)
         (c-mode-common . hl-todo-mode))

  :config
  (use-package modern-cpp-font-lock
    :straight t
    :diminish
    :init (modern-c++-font-lock-global-mode t)))

;; C/C++/Objective-C support
(use-package ccls
  :straight t
  :defines projectile-project-root-files-top-down-recurring
  :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda ()
                                                   (require 'ccls)
                                                   (lsp)))
  :config
  (with-eval-after-load 'projectile
    (setq projectile-project-root-files-top-down-recurring
          (append '("compile_commands.json"
                    ".ccls")
                  projectile-project-root-files-top-down-recurring))))

(provide 'init-c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-c.el ends here
