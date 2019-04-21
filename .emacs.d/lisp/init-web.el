;;; Commentary:
;;
;; Web configurations.
;;

;;; Code:

(use-package css-mode
  :straight t
  :ensure nil
  :init (setq css-indent-offset 2))

;; SCSS mode
(use-package scss-mode
  :straight t
  :init
  ;; Disable complilation on save
  (setq scss-compile-at-save nil))

;; New `less-cs-mde' in Emacs 26
(unless (fboundp 'less-css-mode)
  (use-package less-css-mode))

;; CSS eldoc
(use-package css-eldoc
  :straight t
  :commands turn-on-css-eldoc
  :hook ((css-mode scss-mode less-css-mode) . turn-on-css-eldoc))

;; JSON mode
(use-package json-mode :straight t)

;; Improved JavaScript editing mode
(use-package js2-mode
  :straight t
  :defines flycheck-javascript-eslint-executable
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter (("node" . js2-mode)
                ("node" . js2-jsx-mode))
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-highlight-unused-variables-mode))
  :config
  ;; Use default keybindings for lsp
  (unbind-key "M-." js2-mode-map)

  (with-eval-after-load 'flycheck
    (if (or (executable-find "eslint_d")
            (executable-find "eslint")
            (executable-find "jshint"))
        (setq js2-mode-show-strict-warnings nil))
    (if (executable-find "eslint_d")
        ;; https://github.com/mantoni/eslint_d.js
        ;; npm -i -g eslint_d
        (setq flycheck-javascript-eslint-executable "eslint_d")))

  (use-package js2-refactor
    :straight t
    :diminish js2-refactor-mode
    :hook (js2-mode . js2-refactor-mode)
    :config (js2r-add-keybindings-with-prefix "C-c C-m")))

;; Run Mocha or Jasmine tests
(use-package mocha
  :straight t
  :config (use-package mocha-snippets :straight t))

;; Major mode for CoffeeScript code
(use-package coffee-mode
  :straight t
  :config (setq coffee-tab-width 2))

;; Major mode for editing web templates
(use-package web-mode
  :straight t
  :defines company-backends
  :mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

;; Live browser JavaScript, CSS, and HTML interaction
(use-package skewer-mode
  :straight t
  :diminish skewer-mode
  :hook ((js2-mode . skewer-mode)
         (css-mode . skewer-css-mode)
         (web-mode . skewer-html-mode)
         (html-mode . skewer-html-mode))
  :init
  ;; diminish
  (with-eval-after-load 'skewer-css
    (diminish 'skewer-css-mode))
  (with-eval-after-load 'skewer-html
    (diminish 'skewer-html-mode)))

;; Format HTML, CSS and JavaScript/JSON by js-beautify
;; Insta;; npm -g install js-beautify
(use-package web-beautify
  :straight t
  :init
  (with-eval-after-load 'js-mode
    (bind-key "C-c b" #'web-beautify-js js-mode-map))
  (with-eval-after-load 'js2-mode
    (bind-key "C-c b" #'web-beautify-js js2-mode-map))
  (with-eval-after-load 'json-mode
    (bind-key "C-c b" #'web-beautify-js json-mode-map))
  (with-eval-after-load 'web-mode
    (bind-key "C-c b" #'web-beautify-html web-mode-map))
  (with-eval-after-load 'sgml-mode
    (bind-key "C-c b" #'web-beautify-html html-mode-map))
  (with-eval-after-load 'css-mode
    (bind-key "C-c b" #'web-beautify-css css-mode-map))
  :config
  ;; Set indent size to 2
  (setq web-beautify-args '("-s" "2" "-f" "-")))

(use-package haml-mode :straight t)
(use-package php-mode :straight t)

(provide 'init-web)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
