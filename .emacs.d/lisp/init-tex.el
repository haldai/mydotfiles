;;; Commentary:
;;
;; LaTex configuration.
;;

;;; Code:
(use-package tex
  :straight auctex
  :mode ("\\.tex\\'" . latex-mode)
  :init
  (progn
    (setq TeX-auto-save t
          TeX-parse-self t
          TeX-syntactic-comment t
          ;; Synctex support
          TeX-source-correlate-mode t
          TeX-source-correlate-method 'synctex
          TeX-source-correlate-start-server nil
          ;; Don't insert line-break at inline math
          LaTeX-fill-break-at-separators nil)
    (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    (add-hook 'LaTex-mode-hook 'company-mode)
    (add-hook 'LaTex-mode-hook 'prog-mode))
  :config
  (use-package auctex-latexmk
    :straight t
    :config
    (auctex-latexmk-setup))

  (use-package company-auctex :straight t)

  ;; Reftex
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
  (add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

  ;; Viewer
  (cond
   ((eq system-type 'windows-nt)
    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (setq TeX-view-program-selection '((output-pdf "Acrobat")
                                                   (output-dvi "Yap"))))))
   ((eq system-type 'gnu/linux)
    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (setq TeX-view-program-selection '((output-pdf "PDF Tools")
                                                   (output-dvi "gv"))))))))
;; PDF-view mode
(use-package pdf-tools
  :straight t
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :init (pdf-tools-install)
  :config
  (setq pdf-view-midnight-colors '("#d4d4d4" . "#1e1e1e" ))
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode))


(provide 'init-tex)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-tex.el ends here
