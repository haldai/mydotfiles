;; -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Run AI in Emacs
;;

;;; Code:

;; gptel
(use-package gptel
  :straight t
  :config
  (setq
   gptel-model 'lm-studio
   gptel-backend (gptel-make-openai "LM Studio"
                   :host "localhost:1234"
                   :protocol "http"
                   :key "not-needed"
                   :stream t)))

(provide 'init-ai)
