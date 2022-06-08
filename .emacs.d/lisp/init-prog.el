;;; Commentary:
;;
;; General programming configurations.
;;

;;; Code:

;; Prettify Symbols
;; e.g. display “lambda” as “λ”
(use-package prog-mode
  :hook ((after-init . global-prettify-symbols-mode)
         (prog-mode . (lambda ()
                        (setq prettify-symbols-alist
                              '(("lambda" . ?λ)
                                ("->" . ?→)
                                ("->>" . ?↠)
                                ("=>" . ?⇒)
                                ("map" . ?↦)
                                ("/=" . ?≠)
                                ("!=" . ?≠)
                                ("==" . ?≡)
                                ("<=" . ?≤)
                                (">=" . ?≥)
                                ("=<<" . (?= (Br . Bl) ?≪))
                                (">>=" . (?≫ (Br . Bl) ?=))
                                ("<=<" . ?↢)
                                (">=>" . ?↣)
                                ("&&" . ?∧)
                                ("||" . ?∨)
                                ("not" . ?¬))))))
  :init (setq prettify-symbols-unprettify-at-point 'right-edge)
  :config
  ;; for debug
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output))

;; Compilation Mode
(use-package compile
  :straight t
  :preface
  ;; ANSI Coloring
  ;; @see https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
  (defun my-colorize-compilation-buffer ()
    "ANSI coloring in compilation buffers."
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . my-colorize-compilation-buffer))

;; Jump to definition via `ag'/`rg'/`grep'
(use-package dumb-jump
  :straight t
  :functions dumb-jump-hydra/body
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :hook (after-init . dumb-jump-mode)
  :config
  (setq dumb-jump-prefer-searcher 'rg)
  (with-eval-after-load 'ivy
    (setq dumb-jump-selector 'ivy))

  (defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back")
    ("q" nil "quit"))
  (bind-key "C-c C-j" #'dumb-jump-hydra/body dumb-jump-mode-map))

;; Run commands quickly
(use-package quickrun
  :straight t
  :bind (("C-<f5>" . quickrun)
         ("C-c x" . quickrun)))

(use-package editorconfig
  :straight t
  :diminish editorconfig-mode
  :hook (after-init . editorconfig-mode))

(use-package yaml-mode :straight t)

(provide 'init-prog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
