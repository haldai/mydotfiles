;;---------------------
;; Customize AucTex
;;---------------------
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-source-correlate-mode t)

;; Enable synctex correlation
(setq TeX-source-correlate-method 'synctex)

(if (string-equal system-type "windows-nt")
    (require 'tex-mik))

;; hook mode
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (setq TeX-auto-untabify t     ; remove all tabs before saving
                  TeX-engine 'default)       ; use xlatex default
                  ;TeX-show-compilation t) ; display compilation windows
            (TeX-PDF-mode t)       ; PDF mode enable, not plain
            (setq TeX-save-query nil)
            (imenu-add-menubar-index)
	          (setq TeX-auto-save t)
	          (setq TeX-parse-self t)
	          (setq-default TeX-master nil)
            (define-key LaTeX-mode-map (kbd "TAB") 'TeX-complete-symbol)))

(defadvice TeX-LaTeX-sentinel
    (around mg-TeX-LaTeX-sentinel-open-output activate)
  "Open output when there are errors."
  ;; Run `TeX-LaTeX-sentinel' as usual.
  ad-do-it
  ;; Check for the presence of errors.
  (when
      (with-current-buffer TeX-command-buffer
        (plist-get TeX-error-report-switches (intern (TeX-master-file))))
    ;; If there are errors, open the output buffer.
    (TeX-recenter-output-buffer nil)))

;; do not check comments
(setq ispell-list-command "--list")
(setq ispell-extra-args '("--dont-tex-check-comments"))

;; Reftex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

;; text viewer
(setq TeX-view-program-list
      '(("Acrobat" "Acrobat.exe %o")
        ("Gsview" "gsview32.exe %o")
        ("gv" "gv %o")
        ("qpdfview" "qpdfview --unique %o")
        ("MasterPDF" "masterpdfeditor4 %o")
        ("Evince" "evince %o")
        ("Firefox" "firefox %o")
        ("Yap" "yap.exe %o")))
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
                                                 (output-dvi "gv")))))))
