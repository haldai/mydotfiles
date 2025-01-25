;;; Commentary:
;;
;; Run Ollama in Emacs
;;

;;; Code:
(use-package ellama
  :straight t
  :bind ("C-c o" . ellama-transient-main-menu)
  :init
  ;; setup key bindings
  ;; (setopt ellama-keymap-prefix "C-c e")
  ;; language you want ellama to translate to
  (setopt ellama-language "English")
  ;; could be llm-openai for example
  (use-package llm :straight t)
  (require 'llm-ollama)
  (setopt ellama-provider
          (make-llm-ollama
           ;; this model should be pulled to use it
           ;; value should be the same as you print in terminal during pull
           :chat-model "phi4:latest"
           :embedding-model "phi4:latest"
           :default-chat-non-standard-params '(("num_ctx" . 8192))))
  (setopt ellama-summarization-provider
          (make-llm-ollama
           :chat-model "qwen2.5:14b"
           :embedding-model "qwen2.5:14b"
           :default-chat-non-standard-params '(("num_ctx" . 32768))))
  (setopt ellama-coding-provider
          (make-llm-ollama
           :chat-model "deepseek-r1:8b"
           :embedding-model "deepseek-r1:8b"
           :default-chat-non-standard-params '(("num_ctx" . 32768))))
  ;; Predefined llm providers for interactive switching.
  ;; You shouldn't add ollama providers here - it can be selected interactively
  ;; without it. It is just example.
  (setopt ellama-providers
          '(("gemmad" . (make-llm-ollama
                         :chat-model "gemma2:9b"
                         :embedding-model "gemma2:9b"))
            ("deepseek-r1" . (make-llm-ollama
                              :chat-model "deepseek-r1:8b"
                              :embedding-model "deepseek-r1:8b"))
            ("qwen2.5" . (make-llm-ollama
                          :chat-model "qwen2.5:14b"
                          :embedding-model "qw2.5:14b"))))
  ;; Naming new sessions with llm
  (setopt ellama-naming-provider
          (make-llm-ollama
           :chat-model "gemma2:9b"
           :embedding-model "gemma2:9b"
           :default-chat-non-standard-params '(("stop" . ("\n")))))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  ;; Translation llm provider
  (setopt ellama-translation-provider
          (make-llm-ollama
           :chat-model "qwen2.5:14b"
           :embedding-model "qwen2.5:14b"
           :default-chat-non-standard-params
           '(("num_ctx" . 32768))))
  ;; customize display buffer behaviour
  ;; see ~(info "(elisp) Buffer Display Action Functions")~
  (setopt ellama-chat-display-action-function #'display-buffer-full-frame)
  (setopt ellama-instant-display-action-function #'display-buffer-at-bottom)
  :config
  ;; send last message in chat buffer with C-c C-c
  (add-hook 'org-ctrl-c-ctrl-c-hook #'ellama-chat-send-last-message))
(provide 'init-ellama)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ellama.el ends here
