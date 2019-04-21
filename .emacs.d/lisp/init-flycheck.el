;;; Commentary:
;;
;; Flycheck configurations.
;;

;;; Code:

(use-package flycheck
  :straight t
  :diminish flycheck-mode
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-indication-mode 'right-fringe)
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

  ;; Only check while saving and opening files
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  ;; Display Flycheck errors in GUI tooltips
  (if (display-graphic-p)
      (use-package flycheck-posframe
        :straight t
        :hook (flycheck-mode . flycheck-posframe-mode)
        :config (add-to-list 'flycheck-posframe-inhibit-functions
                             #'(lambda () (bound-and-true-p company-backend))))
    (use-package flycheck-popup-tip
      :straight t
      :hook (flycheck-mode . flycheck-popup-tip-mode))))

(provide 'init-flycheck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flycheck.el ends here
