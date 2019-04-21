;;; Commentary:
;;
;; Kill ring configurations.
;;

;;; Code:

(setq kill-ring-max 200)

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

;; Kill & Mark things easily
(use-package easy-kill-extras
  :straight t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark-sexp)
         ([remap mark-word] . easy-mark-word)

         ;; Integrate `zap-to-char'
         ([remap zap-to-char] . easy-mark-to-char)
         ([remap zap-up-to-char] . easy-mark-up-to-char)

         ;; Integrate `expand-region'
         :map easy-kill-base-map
         ("o" . easy-kill-er-expand)
         ("i" . easy-kill-er-unexpand))
  :init
  (setq easy-kill-alist '((?w word           " ")
                          (?s sexp           "\n")
                          (?l list           "\n")
                          (?f filename       "\n")
                          (?d defun          "\n\n")
                          (?D defun-name     " ")
                          (?e line           "\n")
                          (?b buffer-file-name)

                          (?^ backward-line-edge "")
                          (?$ forward-line-edge "")
                          (?h buffer "")
                          (?< buffer-before-point "")
                          (?> buffer-after-point "")
                          (?f string-to-char-forward "")
                          (?F string-up-to-char-forward "")
                          (?t string-to-char-backward "")
                          (?T string-up-to-char-backward ""))))

(provide 'init-kill-ring)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-kill-ring.el ends here
