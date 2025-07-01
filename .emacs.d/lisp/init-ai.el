;;; Commentary:
;;
;; Run AI in Emacs
;;

;;; Code:

;; ollama
(use-package ellama
  :straight t
  :init
  ;; setup key bindings
  (setopt ellama-keymap-prefix "C-c e")
  ;; language you want ellama to translate to
  (setopt ellama-language "English")
  ;; could be llm-openai for example
  (require 'llm-ollama)
  (setopt ellama-provider
      (make-llm-ollama
       ;; this model should be pulled to use it
       ;; value should be the same as you print in terminal during pull
       :chat-model "llama3.1:70b-instruct-q6_K"
       :embedding-model "nomic-embed-text"
       :default-chat-non-standard-params '(("num_ctx" . 8192))))
  ;; Predefined llm providers for interactive switching.
  ;; You shouldn't add ollama providers here - it can be selected interactively
  ;; without it. It is just example.
  ;; (setopt ellama-providers
  ;;           '(("zephyr" . (make-llm-ollama
  ;;                  :chat-model "zephyr:7b-beta-q6_K"
  ;;                  :embedding-model "zephyr:7b-beta-q6_K"))
  ;;             ("mistral" . (make-llm-ollama
  ;;                   :chat-model "mistral:7b-instruct-v0.2-q6_K"
  ;;                   :embedding-model "mistral:7b-instruct-v0.2-q6_K"))
  ;;             ("mixtral" . (make-llm-ollama
  ;;                   :chat-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"
  ;;                   :embedding-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"))))
  ;; Naming new sessions with llm
  (setopt ellama-naming-provider
      (make-llm-ollama
       :chat-model "llama3.1:70b-instruct-q6_K"
       :embedding-model "nomic-embed-text"
       :default-chat-non-standard-params '(("stop" . ("\n")))))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  ;; Translation llm provider
  (setopt ellama-translation-provider (make-llm-ollama
                       :chat-model "llama3.1:70b-instruct-q6_K"
                       :embedding-model "nomic-embed-text")))

;; aidermacs
;; install aider-install in AUR
(use-package aidermacs
  :straight t
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  ; Set API_KEY in .bashrc, that will automatically picked up by aider or in elisp
  (setenv "ANTHROPIC_API_KEY" "sk-...")
  ; defun my-get-openrouter-api-key yourself elsewhere for security reasons
  (setenv "OPENROUTER_API_KEY" (my-get-openrouter-api-key))
  :custom
  ; See the Configuration section below
  (aidermacs-default-chat-mode 'architect)
  (aidermacs-default-model "gemini"))

(provide 'init-ai)
