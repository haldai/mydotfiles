;;; Commentary:
;;
;; Ellama configuration.
;;

;;; Code:


;; ellama
(use-package ellama
  :straight t
  :init
  ;; setup key bindings
  (setopt ellama-keymap-prefix "C-c e")
  ;; language you want ellama to translate to
  (setopt ellama-language "中文")
  ;; could be llm-openai for example
  (require 'llm-ollama)
  (setopt ellama-provider
      (make-llm-ollama
       ;; this model should be pulled to use it
       ;; value should be the same as you print in terminal during pull
       :chat-model "llama3:8b-instruct-q5_K_M"
       :embedding-model "nomic-embed-text"
       :default-chat-non-standard-params '(("num_ctx" . 8192))))
  ;; Predefined llm providers for interactive switching.
  ;; You shouldn't add ollama providers here - it can be selected interactively
  ;; without it. It is just example.
  (setopt ellama-providers
            '(("qwen2" . (make-llm-ollama
                   :chat-model "qwen2:7b-instruct-q5_0"
                   :embedding-model "qwen2:7b-instruct-q5_0"))
              ("mistral" . (make-llm-ollama
                    :chat-model "mistral:7b-instruct-v0.2-q6_K"
                    :embedding-model "mistral:7b-instruct-v0.2-q6_K"))))
  ;; Naming new sessions with llm
  (setopt ellama-naming-provider
      (make-llm-ollama
       :chat-model "llama3:8b-instruct-q5_K_M"
       :embedding-model "nomic-embed-text"
       :default-chat-non-standard-params '(("stop" . ("\n")))))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  ;; Translation llm provider
  (setopt ellama-translation-provider (make-llm-ollama
                       :chat-model "phi3:3.8b-mini-128k-instruct-q8_0"
                       :embedding-model "nomic-embed-text")))

(provide 'init-ellama)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ellama.el ends here
