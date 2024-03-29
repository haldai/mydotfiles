;;; Commentary:
;;
;; Emacs Lisp configurations.
;;

;;; Code:

;; Emacs lisp mode
(use-package elisp-mode
  :bind (:map emacs-lisp-mode-map
              ("C-c C-x" . ielm)
              ("C-c C-c" . eval-defun)
              ("C-c C-b" . eval-buffer))
  :config
  (if (boundp 'elisp-flymake-byte-compile-load-path)
      (add-to-list 'elisp-flymake-byte-compile-load-path load-path))

  ;; Add remove buttons for advices
  (add-hook 'help-mode-hook 'cursor-sensor-mode)

  (defun function-advices (function)
    "Return FUNCTION's advices."
    (let ((function-def (advice--symbol-function function))
          (ad-functions '()))
      (while (advice--p function-def)
        (setq ad-functions (append `(,(advice--car function-def)) ad-functions))
        (setq function-def (advice--cdr function-def)))
      ad-functions))

  (define-advice describe-function-1 (:after (function) advice-remove-button)
    "Add a button to remove advice."
    (when (get-buffer "*Help*")
      (with-current-buffer "*Help*"
        (save-excursion
          (goto-char (point-min))
          (let ((ad-index 0)
                (ad-list (reverse (function-advices function))))
            (while (re-search-forward "^:[-a-z]+ advice: \\(.+\\)$" nil t)
              (let* ((name (string-trim (match-string 1) "'" "'"))
                     (advice (or (intern-soft name) (nth ad-index ad-list))))
                (when (and advice (functionp advice))
                  (let ((inhibit-read-only t))
                    (insert "\t")
                    (insert-text-button
                     "[Remove]"
                     'cursor-sensor-functions `((lambda (&rest _) (message "%s" ',advice)))
                     'help-echo (format "%s" advice)
                     'action
                     ;; In case lexical-binding is off
                     `(lambda (_)
                        (when (yes-or-no-p (format "Remove %s ? " ',advice))
                          (message "Removing %s of advice from %s" ',function ',advice)
                          (advice-remove ',function ',advice)
                          (revert-buffer nil t)))
                     'follow-link t))))
              (setq ad-index (1+ ad-index)))))))))

;; Show function arglist or variable docstring
;; `global-eldoc-mode' is enabled by default.
(use-package eldoc
  :straight t
  :ensure nil
  :diminish)

;; Interactive macro expander
(use-package macrostep
  :straight t
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)
              :map lisp-interaction-mode-map
              ("C-c e" . macrostep-expand)))

;; Semantic code search for emacs lisp
(use-package elisp-refs :straight t)

;; A better *Help* buffer
(use-package helpful
  :straight t
  :defines ivy-initial-inputs-alist
  :bind (("C-c C-d" . helpful-at-point))
  :config
  (with-eval-after-load 'ivy
    (dolist (cmd '(helpful-callable
                   helpful-variable
                   helpful-function
                   helpful-macro
                   helpful-command))
      (cl-pushnew `(,cmd . "^") ivy-initial-inputs-alist))))

(provide 'init-emacs-lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-emacs-lisp.el ends here
